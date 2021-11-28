import LeanInk.Commands.Analyze.Configuration
import LeanInk.Commands.Analyze.InfoTreeUtil
import LeanInk.Commands.Analyze.Analysis
import LeanInk.Commands.Analyze.ListUtil

import LeanInk.Output.Alectryon

import Lean.MetavarContext
import Lean.Elab.InfoTree

namespace LeanInk.Commands.Analyze

open Output
open Lean
open Lean.Elab

namespace TacticFragment
  private def resolveGoalsAux (ctx : ContextInfo) (mctx : MetavarContext) : List MVarId -> IO (List Format)
    | [] => []
    | goals => do
      let ctx := { ctx with mctx := mctx }
      return [← ctx.runMetaM {} (return Std.Format.prefixJoin "\n" (← goals.mapM (Meta.ppGoal .)))]

  def resolveGoals (self : TacticFragment) : IO (List Alectryon.Goal) := do
    let goalsBefore ← resolveGoalsAux self.ctx self.info.mctxBefore self.info.goalsBefore
    return ← goalsBefore.map (λ g => { name := "", conclusion := s!"{g}", hypotheses := #[] } )
end TacticFragment

namespace MessageFragment
  def toAlectryonMessage (self : MessageFragment) : IO Alectryon.Message := do
    let message ← self.msg.toString
    return { contents := message }
end MessageFragment

structure CompoundFragment where
  headPos: String.Pos
  enumFragments: List (Nat × AnalysisFragment)

namespace CompoundFragment
  def tailPos (self : CompoundFragment) : Option String.Pos := (self.enumFragments.map (λ f => f.2.tailPos)).maximum?

  def fragments (self : CompoundFragment) : List AnalysisFragment := self.enumFragments.map (λ f => f.2)

  private def mapIOContext (f : α -> IO β) : List α -> IO (List β)
    | [] => []
    | t::ts => do (← f t)::(← mapIOContext f ts)

  def toAlectryonFragment (self : CompoundFragment) (contents : String) : IO Alectryon.Fragment := do
    if self.enumFragments.isEmpty then
      return Alectryon.Fragment.text { contents := contents }
    else
      let tactics : List TacticFragment := self.fragments.filterMap (λ f => f.asTactic)
      let tacticGoals ← mapIOContext (λ t => t.resolveGoals) tactics
      let messages : List MessageFragment := self.fragments.filterMap (λ f => f.asMessage)
      let stringMessages ← mapIOContext (λ m => m.toAlectryonMessage) messages
      return Alectryon.Fragment.sentence { contents := contents, goals := tacticGoals.join.toArray, messages := stringMessages.toArray }
end CompoundFragment

instance : ToString CompoundFragment where
  toString (self : CompoundFragment) : String := s!"<COMPOUND head:{self.headPos} fragments:{self.enumFragments.map (λ x => x.1)}"

inductive FragmentEvent where
  | head (pos: String.Pos) (fragment: AnalysisFragment) (idx: Nat)
  | tail (pos: String.Pos) (fragment: AnalysisFragment) (idx: Nat)
  deriving Inhabited

namespace FragmentEvent
  def position: FragmentEvent -> String.Pos
    | head p _ _ => p
    | tail p _ _ => p

  def idx: FragmentEvent -> Nat
    | head _ _ idx => idx
    | tail _ _ idx => idx

  def fragment: FragmentEvent -> AnalysisFragment
    | head _ f _ => f
    | tail _ f _ => f

  def enumerateFragment (self : FragmentEvent) : (Nat × AnalysisFragment) := (self.idx, self.fragment)

  def isHead: FragmentEvent -> Bool
    | head _ _ _ => true
    | tail _ _ _ => false

  def isTail: FragmentEvent -> Bool
    | head _ _ _ => false
    | tail _ _ _ => true
end FragmentEvent

instance : ToString FragmentEvent where
  toString (self : FragmentEvent) : String := s!"¬<FRAGMENT isHead: {self.isHead}, pos: {self.position}, idx: {self.idx}>"

def generateFragmentEventQueue (analysis : List AnalysisFragment) : List FragmentEvent := do
  let filteredAnalysis := analysis.filter (λ f => f.headPos < f.tailPos)
  let enumerateAnalysis := filteredAnalysis.enum
  let headQueue := enumerateAnalysis.map (λ (idx, f) => FragmentEvent.head f.headPos f idx)
  let sortedTailList := List.sort (λ x y => x.2.tailPos < y.2.tailPos) enumerateAnalysis
  let tailQueue := sortedTailList.map (λ (idx, f) => FragmentEvent.tail f.tailPos f idx)
  return List.mergeSort (λ x y => x.position < y.position) headQueue tailQueue

def generateCompoundFragments (l : List CompoundFragment) : List FragmentEvent -> IO (List CompoundFragment)
  | [] => l -- No events left, so we just return!
  | e::es => do
    match l.getLast? with
    | none => do
      if e.isHead then
        let newCompound : CompoundFragment := { headPos := e.position, enumFragments := [e.enumerateFragment]}
        IO.println s!"NO COMPOUND\n-> GENERATING NEW FROM HEAD AT {e.position}\n-> {newCompound}"
        return (← generateCompoundFragments [newCompound] es)
      else
        IO.println s!"NO COMPOUND\n-> UNEXPECTED TAIL"
        return [] -- TODO: handle error
    | some c => do
      if e.isHead then
        if c.headPos == e.position then
          let updatedCompound := { c with enumFragments := c.enumFragments.append [e.enumerateFragment] }
          IO.println s!"FOUND COMPOUND {c} \n-> UPDATING CURRENT WITH HEAD {e.idx}\n-> {updatedCompound}"
          return (← generateCompoundFragments (l.dropLast.append [updatedCompound]) es)
        else
          let newCompound := { c with headPos := e.position, enumFragments := c.enumFragments.append [e.enumerateFragment] }
          IO.println s!"FOUND COMPOUND {c} \n-> CREATING NEW COMPOUND WITH HEAD {e.idx}\n-> {newCompound}"
          return (← generateCompoundFragments (l.append [newCompound]) es)
      else
        let newFragments := c.enumFragments.filter (λ x => x.1 != e.idx) -- Remove all fragments with the same idx
        /-
          It may be the case that the newFragments list isEmpty. This is totally fine as we need to
          insert text spacers later for the text. No we can simply generate a text fragment whenever a compound is empty.
        -/
        let newCompound : CompoundFragment := { headPos := e.position, enumFragments := newFragments }
        IO.println s!"FOUND COMPOUND {c} ¬-> CREATING NEW COMPOUND WITH TAIL {e.idx}\n-> {newCompound}"
        return (← generateCompoundFragments (l.append [newCompound]) es)

/-
Expects a list of sorted CompoundFragments (sorted by headPos).
Generates AlectryonFragments for the given CompoundFragments and input file content.
-/
def annotateFileWithCompounds (l : List Alectryon.Fragment) (contents : String) : List CompoundFragment -> IO (List Alectryon.Fragment)
  | [] => l
  | x::[] => do
    let fragment ← x.toAlectryonFragment (contents.extract x.headPos contents.length)
    return l.append [fragment]
  | x::y::ys => do
    let fragment ← x.toAlectryonFragment (contents.extract x.headPos y.headPos)
    return (← annotateFileWithCompounds (l.append [fragment]) contents (y::ys))

def annotateFile (config : Configuration) (analysis : List AnalysisFragment) : IO (List Alectryon.Fragment) := do
  let events := generateFragmentEventQueue analysis
  IO.println f!"Events: {events}"
  /-
    We generate the compounds and provide an initial compound beginning at the source root index (0) with no fragments.
  -/
  let compounds ← generateCompoundFragments [{ headPos := 0, enumFragments := [] }] events
  IO.println f!"Compounds: {compounds}"
  return (← annotateFileWithCompounds [] config.inputFileContents compounds)