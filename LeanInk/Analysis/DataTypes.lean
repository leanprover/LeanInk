import LeanInk.ListUtil

import Lean.Elab
import Lean.Data.Lsp
import Lean.Syntax
import Lean.Server

namespace LeanInk.Analysis
open Lean Elab Meta

/- Positional -/
class Positional (α : Type u) where
  headPos : α -> String.Pos
  tailPos : α -> String.Pos

namespace Positional
  def length { α : Type u } [Positional α] (self : α) : String.Pos := (Positional.tailPos self) - (Positional.headPos self)

  def smallest? { α : Type u } [Positional α] (list : List α) : Option α := list.foldl (λ a (y : α) =>
    match a with 
    | none => y
    | some x => if Positional.length x ≤ Positional.length y then x else y
  ) none
end Positional

/- Fragment -/
/--
  A `Fragment` is a simple structure that describes an interval within the source text.
  This is similar to the `Positional` type class. However the structure is used as a parent for other structures
  in the Analysis. As a result every `Fragment` automatically conforms to `Positional`
-/
structure Fragment where
  headPos : String.Pos
  tailPos : String.Pos
  deriving Inhabited

instance : Positional Fragment where
  headPos := Fragment.headPos
  tailPos := Fragment.tailPos

/- Tactics -/

structure Tactic extends Fragment where
  goalsBefore : List String
  goalsAfter : List String
  deriving Inhabited

instance : Positional Tactic where
  headPos := (·.toFragment.headPos)
  tailPos := (·.toFragment.tailPos)

instance : ToString Tactic where
  toString t := s!"Tactic: {t.headPos}-{t.tailPos}"

/- InfoTree -/
def Info.isExpanded (self : Info) : Bool :=
  let stx := Info.stx self
  match stx.getHeadInfo, stx.getTailInfo with
  | SourceInfo.original .., SourceInfo.original .. => false
  | _, _ => true