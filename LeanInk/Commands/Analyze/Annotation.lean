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

structure CompoundFragment where
  headPos: String.Pos
  fragments: List (Nat × AnalysisFragment)

instance : ToString CompoundFragment where
  toString (self : CompoundFragment) : String := s!"<COMPOUND head:{self.headPos} fragments:{self.fragments.map (λ x => x.1)}"

namespace CompoundFragment
 -- TODO: Implement to AlectryonFragment
end CompoundFragment

namespace TacticFragment
  private def resolveGoalsAux (ctx: ContextInfo) (mctx : MetavarContext) : List MVarId -> IO (List Format)
    | [] => []
    | goals => do
      let ctx := { ctx with mctx := mctx }
      return [← ctx.runMetaM {} (return Std.Format.prefixJoin "\n" (← goals.mapM (Meta.ppGoal .)))]

  def resolveGoals (self : TacticFragment) : IO (List Alectryon.Goal) := do
    let goalsBefore ← resolveGoalsAux self.ctx self.info.mctxBefore self.info.goalsBefore
    return ← goalsBefore.map (λ g => { name := "", conclusion := s!"{g}", hypotheses := #[] } )
end TacticFragment

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
  toString (self : FragmentEvent) : String := s!"\n<FRAGMENT isHead: {self.isHead}, pos: {self.position}, idx: {self.idx}>"

/-
def _annotateFileAux (l : List Fragment) (contents : String) (pos : String.Pos) (f : List TacticFragment) : IO (List Fragment) := do
  IO.println s!"Running Annotation (l: {l.length}) (pos: {pos}) (f: {f.length})"

  if contents.atEnd pos then
    return l
  else
    match f with
    -- We don't have any further tactics to annotate so we just return the rest of the contents as a text fragment.
    | [] => l.append [Fragment.text { contents := contents.extract pos contents.length }]
    | t::ts =>
      let fragment := Fragment.sentence { contents := contents.extract t.headPos t.tailPos, messages := #[], goals := (← t.resolveGoals).toArray }
      if t.headPos > pos then
        let textFragment := Fragment.text { contents := contents.extract pos t.headPos }
        return ← _annotateFileAux (l.append [textFragment, fragment]) contents t.tailPos ts
      else
        return ← _annotateFileAux (l.append [fragment]) contents t.tailPos ts

def _annotateFile (config: Configuration) (annotations: List TacticFragment) : IO (List Fragment) := _annotateFileAux [] config.inputFileContents 0 annotations
-/

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
      match e.isHead with
      | true => do
        let newCompound : CompoundFragment := { headPos := e.position, fragments := [e.enumerateFragment]}
        IO.println s!"NO COMPOUND\n-> GENERATING NEW FROM HEAD AT {e.position}\n-> {newCompound}"
        return (← generateCompoundFragments [newCompound] es)
      | _ => do
        IO.println s!"NO COMPOUND\n-> UNEXPECTED TAIL"
        return [] -- TODO: handle error
    | some c => do
      match e.isHead with
      | true => do
        if c.headPos == e.position then
          let updatedCompound := { c with fragments := c.fragments.append [e.enumerateFragment] }
          IO.println s!"FOUND COMPOUND {c} \n-> UPDATING CURRENT WITH HEAD {e.idx}\n-> {updatedCompound}"
          return (← generateCompoundFragments (l.dropLast.append [updatedCompound]) es)
        else
          let newCompound := { c with headPos := e.position, fragments := c.fragments.append [e.enumerateFragment] }
          IO.println s!"FOUND COMPOUND {c} \n-> CREATING NEW COMPOUND WITH HEAD {e.idx}\n-> {newCompound}"
          return (← generateCompoundFragments (l.append [newCompound]) es)
      | _ => do
        let newFragments := c.fragments.filter (λ x => x.1 == e.enumerateFragment.1) -- Remove all fragments with the same idx
        if newFragments.isEmpty then
          IO.println s!"FOUND COMPOUND {c} \n-> NO FRAGMENTS LEFT AFTER REMOVAL OF {e.idx}"
          return (← generateCompoundFragments l es)
        else
          let newCompound : CompoundFragment := { headPos := e.position, fragments := newFragments }
          IO.println s!"FOUND COMPOUND {c} \n-> CREATING NEW COMPOUND WITH TAIL {e.idx}\n-> {newCompound}"
          return (← generateCompoundFragments (l.append [newCompound]) es)

def annotateFile (config : Configuration) (analysis : List AnalysisFragment) : IO (List Alectryon.Fragment) := do
  let events := generateFragmentEventQueue analysis
  IO.println s!"Events: {events}"
  let compounds ← generateCompoundFragments [] events
  IO.println s!"Compounds: {compounds}"
  return compounds.map (λ _ => Alectryon.Fragment.text { contents := "Test" })