import LeanInk.Commands.Analyze.Configuration
import LeanInk.Commands.Analyze.InfoTreeUtil
import LeanInk.Commands.Analyze.Analysis
import LeanInk.Commands.Analyze.ListUtil
import LeanInk.Commands.Analyze.Logger

import LeanInk.Output.Alectryon

import Lean.MetavarContext
import Lean.Elab.InfoTree

namespace LeanInk.Commands.Analyze

open Output
open Lean
open Lean.Elab

/-
  CompoundFragment
-/
structure CompoundFragment where
  headPos: String.Pos
  enumFragments: List (Nat × AnalysisFragment)

namespace CompoundFragment
  def tailPos (self : CompoundFragment) : Option String.Pos := (self.enumFragments.map (λ f => f.2.tailPos)).minimum?

  def fragments (self : CompoundFragment) : List AnalysisFragment := self.enumFragments.map (λ f => f.2)

  def toAlectryonFragment (self : CompoundFragment) (contents : String) : IO Alectryon.Fragment := do
    if self.enumFragments.isEmpty then
      return Alectryon.Fragment.text { contents := contents }
    else
      let tactics : List TacticFragment := self.fragments.filterMap (λ f => f.asTactic)
      let tacticGoals ← tactics.mapM (λ t => t.resolveGoals)
      let messages : List MessageFragment := self.fragments.filterMap (λ f => f.asMessage)
      let stringMessages ← messages.mapM (λ m => m.toAlectryonMessage)
      return Alectryon.Fragment.sentence { contents := contents, goals := tacticGoals.join.toArray, messages := stringMessages.toArray }
end CompoundFragment

instance : ToString CompoundFragment where
  toString (self : CompoundFragment) : String := s!"<COMPOUND head:{self.headPos} fragments:{self.enumFragments.map (λ x => x.1)}>"

/-
  FragmentEvent
-/
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
  toString (self : FragmentEvent) : String := s!"<FRAGMENT isHead: {self.isHead}, pos: {self.position}, idx: {self.idx}>"

/-
  Annotation
-/
def generateFragmentEventQueue (analysis : List AnalysisFragment) : List FragmentEvent := do
  let enumerateAnalysis := analysis.enum
  let headQueue := enumerateAnalysis.map (λ (idx, f) => FragmentEvent.head f.headPos f idx)
  let sortedTailList := List.sort (λ x y => x.2.tailPos < y.2.tailPos) enumerateAnalysis
  let tailQueue := sortedTailList.map (λ (idx, f) => FragmentEvent.tail f.tailPos f idx)
  return List.mergeSort (λ x y => x.position < y.position) headQueue tailQueue

def generateCompoundFragments (l : List CompoundFragment) : List FragmentEvent -> AnalysisM (List CompoundFragment)
  | [] => l -- No events left, so we just return!
  | e::es => do
    match l.getLast? with
    | none => do
      if e.isHead then
        let newCompound : CompoundFragment := { headPos := e.position, enumFragments := [e.enumerateFragment]}
        Logger.logInfo s!"NO COMPOUND -> GENERATING NEW FROM HEAD AT {e.position} -> {newCompound}"
        return (← generateCompoundFragments [newCompound] es)
      else
        Logger.logInfo s!"NO COMPOUND -> UNEXPECTED TAIL"
        return [] -- TODO: handle error
    | some c => do
      if e.isHead then
        if c.headPos == e.position then
          let updatedCompound := { c with enumFragments := c.enumFragments.append [e.enumerateFragment] }
          Logger.logInfo s!"FOUND COMPOUND {c} -> UPDATING CURRENT WITH HEAD {e.idx} -> {updatedCompound}"
          return (← generateCompoundFragments (l.dropLast.append [updatedCompound]) es)
        else
          let newCompound := { c with headPos := e.position, enumFragments := c.enumFragments.append [e.enumerateFragment] }
          Logger.logInfo s!"FOUND COMPOUND {c} -> CREATING NEW COMPOUND WITH HEAD {e.idx} -> {newCompound}"
          return (← generateCompoundFragments (l.append [newCompound]) es)
      else
        if c.headPos == e.position then
          let updatedCompound := { c with enumFragments := c.enumFragments.filter (λ x => x.1 != e.idx)}
          Logger.logInfo s!"FOUND COMPOUND {c} -> UPDATING CURRENT WITH TAIL AT {e.position} -> {updatedCompound}"
          return (← generateCompoundFragments (l.dropLast.append [updatedCompound]) es)
        else 
          let newFragments := c.enumFragments.filter (λ x => x.1 != e.idx) -- Remove all fragments with the same idx
          /-
            It may be the case that the newFragments list isEmpty. This is totally fine as we need to
            insert text spacers later for the text. No we can simply generate a text fragment whenever a compound is empty.
          -/
          let newCompound : CompoundFragment := { headPos := e.position, enumFragments := newFragments }
          Logger.logInfo s!"FOUND COMPOUND {c} -> CREATING NEW COMPOUND WITH TAIL {e.idx} -> {newCompound}"
          return (← generateCompoundFragments (l.append [newCompound]) es)

/-
Expects a list of sorted CompoundFragments (sorted by headPos).
Generates AlectryonFragments for the given CompoundFragments and input file content.
-/
def annotateFileWithCompounds (l : List Alectryon.Fragment) (contents : String) : List CompoundFragment -> AnalysisM (List Alectryon.Fragment)
  | [] => l
  | x::[] => do
    let fragment ← x.toAlectryonFragment (contents.extract x.headPos contents.length)
    return l.append [fragment]
  | x::y::ys => do
    let fragment ← x.toAlectryonFragment (contents.extract x.headPos y.headPos)
    return (← annotateFileWithCompounds (l.append [fragment]) contents (y::ys))

def annotateFile (config : Configuration) (analysis : List AnalysisFragment) : AnalysisM (List Alectryon.Fragment) := do
  Logger.logInfo f!"Annotation-Input: {analysis}"
  let events := generateFragmentEventQueue analysis
  Logger.logInfo f!"Events: {events}"
  -- We generate the compounds and provide an initial compound beginning at the source root index (0) with no fragments.
  let compounds ← generateCompoundFragments [{ headPos := 0, enumFragments := [] }] events
  Logger.logInfo f!"Compounds: {compounds}"
  return (← annotateFileWithCompounds [] config.inputFileContents compounds)