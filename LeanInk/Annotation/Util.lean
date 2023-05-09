import LeanInk.Logger

import LeanInk.Analysis.DataTypes
import LeanInk.Annotation.DataTypes

namespace LeanInk.Annotation

open LeanInk.Analysis

/- FRAGMENT INTERVAL -/
structure FragmentInterval (α : Type _) where
  isHead   : Bool
  position : String.Pos
  fragment : α
  idx      : Nat
deriving Inhabited

instance [ToString a] : ToString (FragmentInterval a) where
  toString (self : FragmentInterval a) : String := s!"<FRAGMENT isHead: {self.isHead}, pos: {self.position}, idx: {self.idx} | '{self.fragment}' >"

/- FUNCTIONS -/
def toFragmentIntervals { x : Type } [Positional x] [Inhabited x] [Inhabited x] (positionals : List x) : List (FragmentInterval x) :=
  let indexedPositionals := positionals.enum.map <| fun (idx, f) => 
    [{ isHead := true, position := Positional.headPos f, fragment := f, idx := idx }, 
     { isHead := false, position := Positional.tailPos f, fragment := f, idx := idx }]
  let mergedPositionals := indexedPositionals.join
  List.sort (λ x y =>  x.position < y.position) mergedPositionals

@[inline]
def _insertCompound [Positional a] [ToString a] (e : FragmentInterval a) : List (Compound a) → IO (List (Compound a))
| [] => do
  if e.isHead then
    let newCompound : Compound a := { headPos := e.position, tailPos := none, fragments := [(e.idx, e.fragment)] }
    logInfo s!"NO COMPOUND -> GENERATING NEW FROM HEAD AT {e.position} -> {newCompound}"
    return [newCompound]
  else
    logInfo s!"FAILURE: Unexpected tail!"
    return []
| c::cs => do
  if e.isHead then
    if c.headPos == e.position then
      let updatedCompound := { c with tailPos := none, fragments := c.fragments.append [(e.idx, e.fragment)] }
      logInfo s!"FOUND COMPOUND {c} -> UPDATING CURRENT WITH HEAD {e.idx} -> {updatedCompound}"
      return updatedCompound::cs
    else
      let oldCompound := { c with tailPos := e.position }
      let newCompound := { c with headPos := e.position, tailPos := none, fragments := c.fragments.append [(e.idx, e.fragment)] }
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

def matchCompounds [Positional a] [ToString a] (events : List (FragmentInterval a)) : IO (List (Compound a)) := do
  let mut compounds : List (Compound a) := [{ headPos := 0, tailPos := none, fragments := [] }]
  for e in events do
    compounds ← _insertCompound e compounds
  return compounds.reverse