import LeanInk.Configuration
import LeanInk.Logger

import LeanInk.Analysis.Analysis
import LeanInk.Analysis.DataTypes

namespace LeanInk.Annotation

open LeanInk.Analysis

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
  let indexedPositionals := positionals.enum.map (λ (idx, f) => [FragmentInterval.head (Positional.headPos f) f idx, FragmentInterval.tail (Positional.tailPos f) f idx])
  let mergedPositionals := indexedPositionals.join
  List.sort (λ x y =>  x.position < y.position) mergedPositionals

def matchCompounds [Positional a] [ToString a] (l : List (Compound a)) : List (FragmentInterval a) -> AnalysisM (List (Compound a))
  | [] => pure l -- No events left, so we just return!
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