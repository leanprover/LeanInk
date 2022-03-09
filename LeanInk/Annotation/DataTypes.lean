import LeanInk.Analysis.DataTypes

namespace LeanInk.Annotation

open LeanInk.Analysis

universe u

/- COMPOUND -/
structure Compound (β : Type u) where
  headPos : String.Pos
  tailPos : Option String.Pos
  fragments : List (Nat × β)
  deriving Inhabited

structure Annotation where
  sentence : Compound Sentence
  tokens : List (Compound Token)

namespace Compound
  def getFragments (self : Compound b) : List b := self.fragments.map (λ f => f.2)

  def empty { x : Type u } (headPos : String.Pos) : Compound x := { headPos := headPos, tailPos := none, fragments := [] }

  -- def empty { x : Type u } (headPos : String.Pos) : Compound x := { headPos := headPos, tailPos := headPos, fragments := [] }
end Compound

instance {a : Type u} [ToString a] : ToString (Compound a) where
  toString (self : Compound a) : String := "<COMPOUND head:" ++ toString self.headPos ++ " fragments := " ++ toString self.getFragments ++ ">"