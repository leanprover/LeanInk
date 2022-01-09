import LeanInk.ListUtil
import LeanInk.Annotation.Util

import Lean.Elab
import Lean.Data.Lsp
import Lean.Syntax

namespace LeanInk

open Lean
open Lean.Elab
open Lean.Meta
open LeanInk.Annotation

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
  `TokenInfo` is the base structure of all `Token`.
  Inherently it describe the semantic info of the token, which can and should be used for semantic syntax
  highlighting.
-/
structure TokenInfo extends Fragment where
  semanticType: Option String := none
  deriving Inhabited

/--
  The `TypeTokenInfo` describes the metadata of a source text token that conforms
  to a specific type within our type system.
  E.g.: A variable token `p` might conform to type `Nat`
-/
structure TypeTokenInfo extends TokenInfo where
  type: String
  deriving Inhabited
  
/--
  The `DocStringTokenInfo` describes the metadata of an available docstring of a source text token.
  In this case a source text token may be a function or structure with a docstring.
-/
structure DocStringTokenInfo extends TokenInfo where
  docString: String
  deriving Inhabited

/-- 
  A `Token` describes the metadata of a specific range of source text.
  E.g.: a `Token.type` describes for some variable `p` that it conforms to type `Nat`.
  For every category of metadata there should be an additonal constructor with specified `TokenInfo` to make sure the
  individual tokens can be analyzed independently and only based on one aspect of metadata.
  This also prevents from having tokens with a huge amount of `Option` fields.
-/
inductive Token where
| type (info: TypeTokenInfo)
| docString (info: DocStringTokenInfo)
deriving Inhabited

namespace Token
  def toFragment : Token -> Fragment
  | type info => info.toFragment
  | docString info => info.toFragment
end Token

instance : Positional Token where
  headPos := (λ x => x.toFragment.headPos)
  tailPos := (λ x => x.toFragment.tailPos)

/- Tactics -/
structure Hypothesis where
  name: String
  type: String
  body: String

structure Tactic extends Fragment where
  name: String
  hypotheses : List Hypothesis
  deriving Inhabited

structure Message extends Fragment where
  msg: String
  deriving Inhabited

/- Sentence -/
inductive Sentence where
| tactic (info: Tactic)
| message (info: Message)
deriving Inhabited

namespace Sentence
  def toFragment : Sentence -> Fragment
  | tactic info => info.toFragment
  | message info => info.toFragment
end Sentence

instance : Positional Sentence where
  headPos := (λ x => x.toFragment.headPos)
  tailPos := (λ x => x.toFragment.tailPos)

/- Traversal -/
structure TraversalResult where
  tokens : List Token
  sentences : List Sentence
  deriving Inhabited

namespace TraversalResult
  def empty : TraversalResult := { tokens := [], sentences := [] }

  def merge (x y : TraversalResult) : TraversalResult := {
    tokens := List.mergeSortedLists (λ x y => x.toFragment.headPos < y.toFragment.headPos) x.tokens y.tokens
    sentences := List.mergeSortedLists (λ x y => x.toFragment.headPos < y.toFragment.headPos) x.sentences y.sentences
  }
end TraversalResult

/- InfoTree -/
structure ContextBasedInfo (β : Type) where
  ctx : ContextInfo
  info : β

inductive TraversalFragment where
| tactic (info: ContextBasedInfo TacticInfo)
| term (info: ContextBasedInfo TermInfo)
| field (info: ContextBasedInfo FieldInfo)
| unknown (info: ContextBasedInfo ElabInfo)

namespace TraversalFragment
  private def Info.stx : Info -> Syntax
  | Info.ofTacticInfo i => i.stx
  | Info.ofTermInfo i => i.stx
  | Info.ofCommandInfo i => i.stx
  | Info.ofMacroExpansionInfo i => i.stx
  | Info.ofFieldInfo i => i.stx
  | Info.ofCompletionInfo i => i.stx

  def headPos : TraversalFragment -> String.Pos
  | term fragment => (fragment.info.toElabInfo.stx.getPos? false).getD 0
  | field fragment => (fragment.info.stx.getPos? false).getD 0
  | tactic fragment => (fragment.info.toElabInfo.stx.getPos? false).getD 0
  | unknown fragment => (fragment.info.stx.getPos? false).getD 0

  def tailPos : TraversalFragment -> String.Pos
  | term fragment => (fragment.info.toElabInfo.stx.getTailPos? false).getD 0
  | field fragment => (fragment.info.stx.getTailPos? false).getD 0
  | tactic fragment => (fragment.info.toElabInfo.stx.getTailPos? false).getD 0
  | unknown fragment => (fragment.info.stx.getTailPos? false).getD 0

  private def Info.isExpanded (self : Info) : Bool :=
    let stx := Info.stx self
     match stx.getHeadInfo, stx.getTailInfo with
    | SourceInfo.original .., SourceInfo.original .. => false
    | _, _ => true

  def create (ctx : ContextInfo) (info : Info) : Option TraversalFragment :=
    if Info.isExpanded info then
      none
    else
      match info with 
      | Info.ofTacticInfo info => tactic { info := info, ctx := ctx }
      | Info.ofTermInfo info => term { info := info, ctx := ctx }
      | Info.ofFieldInfo info => field { info := info, ctx := ctx }
      | _ => none

/- 
  Token Generation
-/
  def genDocString? (self : TraversalFragment) : MetaM (Option String) := do
    let env ← getEnv
    match self with
    | term fragment =>
      if let some name := fragment.info.expr.constName? then
        findDocString? env name
      else
        none
    | field fragment => findDocString? env fragment.info.projName
    | tactic fragment =>
      let elabInfo := fragment.info.toElabInfo
      return ← findDocString? env elabInfo.elaborator <||> findDocString? env elabInfo.stx.getKind
    | unknown fragment =>
      let elabInfo := fragment.info
      return ← findDocString? env elabInfo.elaborator <||> findDocString? env elabInfo.stx.getKind

  def runMetaM { α : Type } (func : TraversalFragment -> MetaM α) : TraversalFragment -> AnalysisM α
    | term fragment => fragment.ctx.runMetaM fragment.info.lctx (func (term fragment))
    | field fragment => fragment.ctx.runMetaM fragment.info.lctx (func (field fragment))
    | tactic fragment => fragment.ctx.runMetaM {} (func (tactic fragment))
    | unknown fragment => fragment.ctx.runMetaM {} (func (unknown fragment))

  def genDocStringToken? (self : TraversalFragment) : AnalysisM (Option DocStringTokenInfo) := do
    match ← runMetaM (genDocString?) self with
    | some string => some { headPos := self.headPos, tailPos := self.tailPos, docString := string }
    | none => none
end TraversalFragment

partial def _resolveTacticList (ctx?: Option ContextInfo := none) : InfoTree -> AnalysisM TraversalResult
  | InfoTree.context ctx tree => _resolveTacticList ctx tree
  | InfoTree.node info children =>
    match ctx? with
    | none => TraversalResult.empty
    | some ctx =>
      let ctx? := info.updateContext? ctx
      let resolvedChildrenLeafs := children.toList.map  (_resolveTacticList ctx?)
      let sortedChildrenLeafs := resolvedChildrenLeafs.foldl TraversalResult.merge TraversalResult.empty
      match TraversalFragment.create ctx info with
      | some fragment => 
        -- if sortedChildrenLeafs.tactics.isEmpty then 
        --   { sortedChildrenLeafs with tactics := [f] }
        -- else 
          sortedChildrenLeafs
      | none => sortedChildrenLeafs
  | _ => TraversalResult.empty