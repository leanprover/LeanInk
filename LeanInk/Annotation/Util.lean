import LeanInk.Configuration
import LeanInk.Logger

import LeanInk.Analysis.Analysis
import LeanInk.Analysis.InfoTreeUtil

namespace LeanInk.Annotation

open LeanInk.Analysis

universe u

/- POSITIONAL -/
class Positional (α : Type u) where
  headPos : α -> String.Pos
  tailPos : α -> String.Pos

instance : Positional AnalysisFragment where
  headPos := AnalysisFragment.headPos
  tailPos := AnalysisFragment.tailPos

instance : Positional Token where
  headPos := Token.headPos
  tailPos := Token.tailPos

/- COMPOUND -/
structure Compound (β : Type u) where
  headPos : String.Pos
  fragments : List (Nat × β)

namespace Compound
  def getFragments (self : Compound b) : List (b) := self.fragments.map (λ f => f.2)
  def tailPos { x : Type u } [Positional x] (self : Compound x) : Option String.Pos := (self.getFragments.map (λ f => Positional.tailPos f)).minimum?
end Compound

instance {a : Type u} [ToString a] : ToString (Compound a) where
  toString (self : Compound a) : String := "<COMPOUND head:" ++ toString self.headPos ++ " fragments := " ++ toString self.getFragments ++ ">"

/- FRAGMENT INTERVAL -/
inductive FragmentInterval (a : Type u) where
  | head (pos: String.Pos) (fragment: a) (idx: Nat)
  | tail (pos: String.Pos) (fragment: a) (idx: Nat)
  deriving Inhabited

namespace FragmentInterval
  def position : FragmentInterval a -> String.Pos
    | head p _ _ => p
    | tail p _ _ => p

  def idx : FragmentInterval a -> Nat
    | head _ _ idx => idx
    | tail _ _ idx => idx

  def fragment : FragmentInterval a -> a
    | head _ f _ => f
    | tail _ f _ => f

  def enumerateFragment (self : FragmentInterval a) : (Nat × a) := (self.idx, self.fragment)

  def isHead : FragmentInterval a -> Bool
    | head _ _ _ => true
    | tail _ _ _ => false

  def isTail : FragmentInterval a -> Bool
    | head _ _ _ => false
    | tail _ _ _ => true
end FragmentInterval

instance [ToString a] : ToString (FragmentInterval a) where
  toString (self : FragmentInterval a) : String := s!"<FRAGMENT isHead: {self.isHead}, pos: {self.position}, idx: {self.idx} | '{self.fragment}' >"

/- FUNCTIONS -/
def toFragmentIntervals { x : Type } [Positional x] [Inhabited x] [Inhabited x] (positionals : List x) : List (FragmentInterval x) :=
  let sortedHeadList := List.sort (λ x y => (Positional.headPos x) < (Positional.headPos y)) positionals
  let sortedTailList := List.sort (λ x y => (Positional.tailPos x) < (Positional.tailPos y)) positionals
  let headQueue := sortedHeadList.enum.map (λ (idx, f) => FragmentInterval.head (Positional.headPos f) f idx)
  let tailQueue := sortedTailList.enum.map (λ (idx, f) => FragmentInterval.tail (Positional.tailPos f) f idx)
  List.mergeSortedLists (λ x y => (FragmentInterval.position x) < (FragmentInterval.position y)) headQueue tailQueue

def matchCompounds [Positional a] [ToString a] (l : List (Compound a)) : List (FragmentInterval a) -> AnalysisM (List (Compound a))
  | [] => l -- No events left, so we just return!
  | e::es => do
    match l.getLast? with
    | none => do
      if e.isHead then
        let newCompound : Compound a := { headPos := e.position, fragments := [e.enumerateFragment] }
        logInfo s!"NO COMPOUND -> GENERATING NEW FROM HEAD AT {e.position} -> {newCompound}"
        return (← matchCompounds [newCompound] es)
      else
        logInfo s!"FAILURE: Unexpected tail!"
        return []
    | some c => do
      if e.isHead then
        if c.headPos == e.position then
          let updatedCompound := { c with fragments := c.fragments.append [e.enumerateFragment] }
          logInfo s!"FOUND COMPOUND {c} -> UPDATING CURRENT WITH HEAD {e.idx} -> {updatedCompound}"
          return (← matchCompounds (l.dropLast.append [updatedCompound]) es)
        else
          let newCompound := { c with headPos := e.position, fragments := c.fragments.append [e.enumerateFragment] }
          logInfo s!"FOUND COMPOUND {c} -> CREATING NEW COMPOUND WITH HEAD {e.idx} -> {newCompound}"
          return (← matchCompounds (l.append [newCompound]) es)
      else
        if c.headPos == e.position then
          let updatedCompound := { c with fragments := c.fragments.filter (λ x => x.1 != e.idx)}
          logInfo s!"FOUND COMPOUND {c} -> UPDATING CURRENT WITH TAIL AT {e.position} -> {updatedCompound}"
          return (← matchCompounds (l.dropLast.append [updatedCompound]) es)
        else 
          let newFragments := c.fragments.filter (λ x => x.1 != e.idx) -- Remove all fragments with the same idx
          /-
            It may be the case that the newFragments list isEmpty. This is totally fine as we need to
            insert text spacers later for the text. No we can simply generate a text fragment whenever a compound is empty.
          -/
          let newCompound : Compound a := { headPos := e.position, fragments := newFragments }
          logInfo s!"FOUND COMPOUND {c} -> CREATING NEW COMPOUND WITH TAIL {e.idx} -> {newCompound}"
          return (← matchCompounds (l.append [newCompound]) es)