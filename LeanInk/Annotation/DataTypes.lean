import LeanInk.Analysis.DataTypes

namespace LeanInk.Annotation

open LeanInk.Analysis

universe u

/- COMPOUND -/
structure Compound (β : Type u) where
  headPos : String.Pos
  fragments : List (Nat × β)
  deriving Inhabited

structure Annotation where
  sentence : Compound Sentence
  tokens : List (Compound Token)

namespace Compound
  def getFragments (self : Compound b) : List b := self.fragments.map (λ f => f.2)

  def tailPos { x : Type u } [Positional x] (self : Compound x) : Option String.Pos := (self.getFragments.map (λ f => Positional.tailPos f)).maximum?

  def empty { x : Type u } (headPos : String.Pos) : Compound x := { headPos := headPos, fragments := [] }
end Compound

instance {a : Type u} [ToString a] : ToString (Compound a) where
  toString (self : Compound a) : String := "<COMPOUND head:" ++ toString self.headPos ++ " fragments := " ++ toString self.getFragments ++ ">"