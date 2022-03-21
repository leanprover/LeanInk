import LeanInk.ListUtil

import Lean.Elab
import Lean.Data.Lsp
import Lean.Syntax
import Lean.Server

namespace LeanInk.Analysis

open Lean
open Lean.Elab
open Lean.Meta

/- Positional -/
class Positional (α : Type u) where
  headPos : α -> String.Pos
  tailPos : α -> String.Pos

namespace Positional
  def length { α : Type u } [Positional α] (self : α) : String.Pos := (Positional.tailPos self) - (Positional.headPos self)

  def smallest? { α : Type u } [Positional α] (list : List α) : Option α := List.foldl (λ a y => 
    let y : α := y -- We need to help the compiler a bit here otherwise it thinks `y : Option α`
    match a with 
    | none => y
    | some x => if (Positional.length x) <= (Positional.length y) then x else y
  ) none list
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

/- Token -/
/--
  `SemanticTokenInfo` describe the semantic info of the token, which can and should be used for semantic syntax highlighting.
-/
inductive SemanticTokenType where
| property
| «variable»
| keyword

structure SemanticTokenInfo extends Fragment where
  semanticType: Option SemanticTokenType := none
  deriving Inhabited

instance : Positional SemanticTokenInfo where
  headPos := (λ x => x.toFragment.headPos)
  tailPos := (λ x => x.toFragment.tailPos)

/--
  The `TypeTokenInfo` describes the metadata of a source text token that conforms
  to a specific type within our type system. It may also describe just a docstring associated with the token.
  We combine both these informatio together, as the docstring is naturally attached to the type.
  E.g.: A variable token `p` might conform to type `Nat` and has the docstring for `Nat`
-/
structure TypeTokenInfo extends Fragment where
  type: Option String
  docString: Option String
  deriving Inhabited

instance : Positional TypeTokenInfo where
  headPos := (λ x => x.toFragment.headPos)
  tailPos := (λ x => x.toFragment.tailPos)

/-- 
  A `Token` describes the metadata of a specific range of source text.
  E.g.: a `Token.type` describes for some variable `p` that it conforms to type `Nat`.
  For every category of metadata there should be an additonal constructor with specified `TokenInfo` to make sure the
  individual tokens can be analyzed independently and only based on one aspect of metadata.
  This also prevents from having tokens with a huge amount of `Option` fields.
-/
inductive Token where
| type (info: TypeTokenInfo)
| semantic (info: SemanticTokenInfo)
deriving Inhabited

instance : ToString Token where
  toString : Token -> String
  | Token.type _ => "Type"
  | Token.semantic _ => "Semantic"

namespace Token
  def toFragment : Token -> Fragment
  | type info => info.toFragment
  | semantic info => info.toFragment

  def toTypeTokenInfo? : Token -> Option TypeTokenInfo
  | type info => info
  | _ => none

  def toSemanticTokenInfo? : Token -> Option SemanticTokenInfo
  | semantic info => info
  | _ => none
end Token

instance : Positional Token where
  headPos := (λ x => x.toFragment.headPos)
  tailPos := (λ x => x.toFragment.tailPos)

/- Tactics -/
structure Hypothesis where
  names : List String
  type : String
  body : String

structure Goal where
  name : String
  conclusion : String
  hypotheses : List Hypothesis

structure Tactic extends Fragment where
  goalsBefore : List Goal
  goalsAfter : List Goal
  deriving Inhabited

instance : Positional Tactic where
  headPos := (λ x => x.toFragment.headPos)
  tailPos := (λ x => x.toFragment.tailPos)

structure Message extends Fragment where
  msg: String
  deriving Inhabited

/- Sentence -/
inductive Sentence where
| tactic (info: Tactic)
| message (info: Message)
deriving Inhabited

instance : ToString Sentence where -- TODO: Improve this
  toString : Sentence -> String
  | Sentence.tactic t => s!"Tactic {t.headPos}-{t.tailPos}"
  | Sentence.message _ => "Message"

namespace Sentence
  def toFragment : Sentence -> Fragment
  | tactic info => info.toFragment
  | message info => info.toFragment

  def asTactic? : Sentence -> Option Tactic
  | tactic info => info
  | _ => none

  def asMessage? : Sentence -> Option Message
  | message info => info
  | _ => none
end Sentence

instance : Positional Sentence where
  headPos := (λ x => x.toFragment.headPos)
  tailPos := (λ x => x.toFragment.tailPos)

/- InfoTree -/
def Info.isExpanded (self : Info) : Bool :=
  let stx := Info.stx self
  match stx.getHeadInfo, stx.getTailInfo with
  | SourceInfo.original .., SourceInfo.original .. => false
  | _, _ => true