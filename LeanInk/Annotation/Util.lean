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

def matchCompounds [Positional a] [ToString a] (events : List (FragmentInterval a)) : AnalysisM (List (Compound a)) := do
  pure [{ headPos := 0, tailPos := none, fragments := [] }]