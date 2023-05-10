import Lean.Elab
import Lean.Data.Lsp
import Lean.Syntax
import Lean.Server

namespace LeanInk.Analysis
open Lean Elab Meta

instance : ToJson String.Pos := ⟨fun ⟨n⟩ => toJson n⟩

/- Fragment -/
/--
  A `Fragment` is a simple structure that describes an interval within the source text.
  This is similar to the `Positional` type class. However the structure is used as a parent for other structures
  in the Analysis. As a result every `Fragment` automatically conforms to `Positional`
-/
structure Fragment where
  headPos : String.Pos
  tailPos : String.Pos
  deriving Inhabited, ToJson

/- Tactics -/

structure TacticFragment extends Fragment where
  goalsBefore : List String
  goalsAfter : List String
  deriving Inhabited, ToJson

structure TacticFragmentWithContent extends TacticFragment where
  content : String
  deriving Inhabited, ToJson

def extractSuggestion (msg : Message) : IO (Option String) := do
  let raw ← msg.data.toString
  return do
    guard <| raw.startsWith "Try this: "
    return raw.drop "Try this: ".length

def TacticFragment.withContent (contents : String) (messages? : Option <| List Message) (fragment : TacticFragment) : IO TacticFragmentWithContent := do
  -- let msg? : Option Message := sorry
  let replacement? := none
  let content : String := replacement?.getD <| contents.extract fragment.headPos fragment.tailPos
  return ⟨fragment, content⟩

/- InfoTree -/
def Info.isExpanded (self : Info) : Bool :=
  let stx := Info.stx self
  match stx.getHeadInfo, stx.getTailInfo with
  | SourceInfo.original .., SourceInfo.original .. => false
  | _, _ => true