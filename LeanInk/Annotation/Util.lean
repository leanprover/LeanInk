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

  def headPos [Positional a] (f : FragmentInterval a) : String.Pos := Positional.headPos (fragment f)
  def tailPos [Positional a] (f : FragmentInterval a) : String.Pos := Positional.tailPos (fragment f)

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

def maxTailPos (y : String.Pos) : Option String.Pos -> String.Pos
  | none => y
  | some x => if x < y then x else y

@[inline]
def _insertCompound [Positional a] [ToString a] (e : FragmentInterval a) (compounds : List (Compound a)) : AnalysisM (List (Compound a)) := do
  match compounds with
    | [] => do
      if e.isHead then
        let newCompound : Compound a := { headPos := e.position, tailPos := none, fragments := [e.enumerateFragment] }
        logInfo s!"NO COMPOUND -> GENERATING NEW FROM HEAD AT {e.position} -> {newCompound}"
        return [newCompound]
      else
        logInfo s!"FAILURE: Unexpected tail!"
        return []
    | c::cs => do
      if e.isHead then
        if c.headPos == e.position then
          let updatedCompound := { c with tailPos := none, fragments := c.fragments.append [e.enumerateFragment] }
          logInfo s!"FOUND COMPOUND {c} -> UPDATING CURRENT WITH HEAD {e.idx} -> {updatedCompound}"
          return updatedCompound::cs
        else
          let oldCompound := { c with tailPos := e.position }
          let newCompound := { c with headPos := e.position, tailPos := none, fragments := c.fragments.append [e.enumerateFragment] }
          logInfo s!"FOUND COMPOUND {c} -> CREATING NEW COMPOUND WITH HEAD {e.idx} -> {newCompound}"
          return newCompound::oldCompound::cs
      else
        let newFragments := c.fragments.filter (λ x => x.1 != e.idx) -- Remove all fragments with the same idx
        let mut newTailPos := c.tailPos
        if newFragments.isEmpty then
          newTailPos := none
        if c.headPos == e.position then
          let updatedCompound := { c with tailPos := none, fragments := newFragments}
          logInfo s!"FOUND COMPOUND {c} -> UPDATING CURRENT WITH TAIL AT {e.position} -> {updatedCompound}"
          return updatedCompound::cs
        else 
          /-
            It may be the case that the newFragments list isEmpty. This is totally fine as we need to
            insert text spacers later for the text. No we can simply generate a text fragment whenever a compound is empty.
          -/
          let oldCompound := { c with tailPos := e.position }
          let newCompound := { headPos := e.position, tailPos := none, fragments := newFragments }
          logInfo s!"FOUND COMPOUND {c} -> CREATING NEW COMPOUND WITH TAIL {e.idx} -> {newCompound}"
          return newCompound::oldCompound::cs

def matchCompounds [Positional a] [ToString a] (events : List (FragmentInterval a)) : AnalysisM (List (Compound a)) := do
  let mut compounds : List (Compound a) := [{ headPos := 0, tailPos := none, fragments := [] }]
  for e in events do
    compounds ← _insertCompound e compounds
  return compounds.reverse