import LeanInk.Analysis.DataTypes
import LeanInk.Analysis.SemanticToken

import LeanInk.ListUtil
import LeanInk.Configuration

import Lean.Elab
import Lean.Data.Lsp
import Lean.Syntax
import Lean.Server

namespace LeanInk.Analysis

open Lean
open Lean.Elab
open Lean.Meta

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

  def create (ctx : ContextInfo) (info : Info) : ((Option TraversalFragment) × (Option SemanticTraversalInfo)) :=
    if Info.isExpanded info then
      (none, none)
    else
      let semantic : SemanticTraversalInfo := { node := info, stx := info.stx }
      match info with 
      | Info.ofTacticInfo info => (tactic { info := info, ctx := ctx }, semantic)
      | Info.ofTermInfo info => (term { info := info, ctx := ctx }, semantic)
      | Info.ofFieldInfo info => (field { info := info, ctx := ctx }, semantic)
      | _ => (none, semantic)

  def runMetaM { α : Type } (func : TraversalFragment -> MetaM α) : TraversalFragment -> AnalysisM α
  | term fragment => fragment.ctx.runMetaM fragment.info.lctx (func (term fragment))
  | field fragment => fragment.ctx.runMetaM fragment.info.lctx (func (field fragment))
  | tactic fragment => fragment.ctx.runMetaM {} (func (tactic fragment))
  | unknown fragment => fragment.ctx.runMetaM {} (func (unknown fragment))

  /- 
    Token Generation
  -/
  def inferType? : TraversalFragment -> MetaM (Option String)
    | term termFragment => do
      let format ← try Meta.ppExpr (← Meta.inferType termFragment.info.expr) catch e => e.toMessageData.toString
      return s!"{format}"
    | _ => pure none

  def genDocString? (self : TraversalFragment) : MetaM (Option String) := do
    let env ← getEnv
    match self with
    | term fragment =>
      if let some name := fragment.info.expr.constName? then
        findDocString? env name
      else
        pure none
    | field fragment => findDocString? env fragment.info.projName
    | tactic fragment =>
      let elabInfo := fragment.info.toElabInfo
      return ← findDocString? env elabInfo.elaborator <||> findDocString? env elabInfo.stx.getKind
    | unknown fragment =>
      let elabInfo := fragment.info
      return ← findDocString? env elabInfo.elaborator <||> findDocString? env elabInfo.stx.getKind

  def genTypeTokenInfo? (self : TraversalFragment) : AnalysisM (Option TypeTokenInfo) := do
    let mut docString : Option String := none
    let mut type : Option String := none
    let config ← read
    if config.experimentalDocString then
      docString ← runMetaM (genDocString?) self
    if config.experimentalTypeInfo then
      type ← runMetaM (inferType?) self
    if type == none ∧ docString == none then
      return none
    else
      return some { headPos := self.headPos, tailPos := self.tailPos, type := type, docString := docString }

  def genTokens (self : TraversalFragment) : AnalysisM (List Token) := do
    let mut tokens : List Token := []
    if let some typeToken ← self.genTypeTokenInfo? then
      tokens := tokens.append [Token.type typeToken]
    return tokens

  /- Sentence Generation -/
  private def genGoal (goalType : Format) (hypotheses : List Hypothesis): Name -> MetaM (Goal)
    | Name.anonymous => do
      return { 
        name := ""
        conclusion := toString goalType
        hypotheses := hypotheses 
      }
    | name => do
      let goalFormatName := format name.eraseMacroScopes
      return { 
        name := toString goalFormatName
        conclusion := toString goalType
        hypotheses := hypotheses 
      }

  /-- 
  This method is a adjusted version of the Meta.ppGoal function. As we do need to extract the goal informations into seperate properties instead
  of a single formatted string to support the Alectryon.Goal datatype.
  -/
  private def evalGoal (mvarId : MVarId)  : MetaM (Option Goal) := do
    match (← getMCtx).findDecl? mvarId with
    | none => return none
    | some decl => do
      let auxDecl := pp.auxDecls.get (<- getOptions)
      let lctx := decl.lctx.sanitizeNames.run' { options := (← getOptions) }
      withLCtx lctx decl.localInstances do
        let (hidden, hiddenProp) ← ToHide.collect decl.type
        let pushPending (list : List Hypothesis) (type? : Option Expr) : List Name -> MetaM (List Hypothesis)
        | [] => pure list
        | ids => do
          match type? with
            | none      => pure list
            | some type => do
              let typeFmt ← ppExpr type
              let names := ids.reverse.map (λ n => n.toString)
              return list.append [{ names := names, body := "", type := s!"{typeFmt}" }]
        let evalVar (varNames : List Name) (prevType? : Option Expr) (hypotheses : List Hypothesis) (localDecl : LocalDecl) : MetaM (List Name × Option Expr × (List Hypothesis)) := do
          if hiddenProp.contains localDecl.fvarId then
            let newHypotheses ← pushPending [] prevType? varNames
            let type ← instantiateMVars localDecl.type
            let typeFmt ← ppExpr type
            let newHypotheses := newHypotheses.map (λ h => { h with type := h.type})
            pure ([], none, hypotheses.append newHypotheses)
          else
            match localDecl with
            | LocalDecl.cdecl _ _ varName type _ =>
              let varName := varName.simpMacroScopes
              let type ← instantiateMVars type
              if prevType? == none || prevType? == some type then
                pure (varName::varNames, some type, hypotheses)
              else do
                let hypotheses ← pushPending hypotheses prevType? varNames
                pure ([varName], some type, hypotheses)
            | LocalDecl.ldecl _ _ varName type val _ => do
              let varName := varName.simpMacroScopes
              let hypotheses ← pushPending hypotheses prevType? varNames
              let type ← instantiateMVars type
              let val  ← instantiateMVars val
              let typeFmt ← ppExpr type
              let valFmt ← ppExpr val
              pure ([], none, hypotheses.append [{ names := [varName.toString], body := s!"{valFmt}", type := s!"{typeFmt}" }])
        let (varNames, type?, hypotheses) ← lctx.foldlM (init := ([], none, [])) λ (varNames, prevType?, hypotheses) (localDecl : LocalDecl) =>
          if !auxDecl && localDecl.isAuxDecl || hidden.contains localDecl.fvarId then
            pure (varNames, prevType?, hypotheses)
          else
            evalVar varNames prevType? hypotheses localDecl
        let hypotheses ← pushPending hypotheses type? varNames 
        let typeFmt ← ppExpr (← instantiateMVars decl.type)
        return (← genGoal typeFmt hypotheses decl.userName)

  private def _genGoals (contextInfo : ContextBasedInfo TacticInfo) (goals: List MVarId) (metaCtx: MetavarContext) : AnalysisM (List Goal) := 
    let ctx := { contextInfo.ctx with mctx := metaCtx }
    return (← ctx.runMetaM {} (goals.mapM (evalGoal .))).filterMap (λ x => x)

  private def genGoals (contextInfo : ContextBasedInfo TacticInfo) (beforeNode: Bool): AnalysisM (List Goal) :=
    if beforeNode then
      _genGoals contextInfo contextInfo.info.goalsBefore contextInfo.info.mctxBefore
    else
      _genGoals contextInfo contextInfo.info.goalsAfter contextInfo.info.mctxAfter

  def genTactic? (self : TraversalFragment) : AnalysisM (Option Tactic) := do
    match self with
    | tactic fragment => do 
      let goalsBefore ← genGoals fragment true
      let goalsAfter ← genGoals fragment false
      if goalsAfter.isEmpty then
        return some { headPos := self.headPos, tailPos := self.tailPos, goalsBefore := goalsBefore, goalsAfter := [{ name := "", conclusion := "Goals accomplished! 🐙", hypotheses := [] }] }
      else
        return some { headPos := self.headPos, tailPos := self.tailPos, goalsBefore := goalsBefore, goalsAfter := goalsAfter }
    | _ => pure none

  def genSentences (self : TraversalFragment) : AnalysisM (List Sentence) := do
    if let some t ← self.genTactic? then
      return [Sentence.tactic t]
    else
      return []
end TraversalFragment

/- Traversal -/
structure AnalysisResult where
  tokens : List Token
  sentences : List Sentence
  deriving Inhabited

namespace AnalysisResult
  def empty : AnalysisResult := { tokens := [], sentences := [] }

  def merge (x y : AnalysisResult) : AnalysisResult := {
    tokens := List.mergeSortedLists (λ x y => x.toFragment.headPos < y.toFragment.headPos) x.tokens y.tokens
    sentences := List.mergeSortedLists (λ x y => x.toFragment.headPos < y.toFragment.headPos) x.sentences y.sentences
  }

  def insertTokens (self : AnalysisResult) (tokens : List Token) :  AnalysisResult := merge self { tokens := tokens, sentences := [] }

  def insertFragment (self : AnalysisResult) (fragment : TraversalFragment) : AnalysisM AnalysisResult := do
    let newTokens : List Token := ← fragment.genTokens
    let newSentences ← fragment.genSentences
    pure { self with tokens := self.tokens.append newTokens, sentences := self.sentences.append newSentences }

  def insertSemanticInfo (self : AnalysisResult) (info: SemanticTraversalInfo) : AnalysisM AnalysisResult := do
    if (← read).experimentalSemanticType then
      pure { self with tokens := self.tokens ++ (← info._resolveSemanticTokens []) }
    else
      return self

  def Position.toStringPos (fileMap: FileMap) (pos: Lean.Position) : String.Pos :=
    FileMap.lspPosToUtf8Pos fileMap (fileMap.leanPosToLspPos pos)

  private def genMessage (fileMap : FileMap) (message : Lean.Message) : AnalysisM Message := do
    let headPos := Position.toStringPos fileMap message.pos
    let tailPos := Position.toStringPos fileMap (message.endPos.getD message.pos)
    let mut string ← message.data.toString
    if message.caption != "" then
      string := message.caption ++ ":¬" ++ string
    if message.severity == MessageSeverity.warning then
      string := "Warning: " ++ string
    else if message.severity == MessageSeverity.error then
      string := "Error: " ++ string
    return { headPos := headPos, tailPos := tailPos, msg := string }

  def insertMessages (self : AnalysisResult) (messages : List Lean.Message) (fileMap : FileMap): AnalysisM AnalysisResult := do
    let messages ← messages.mapM (genMessage fileMap)
    let sortedMessages := List.sort (λ x y => x.headPos < y.headPos) messages
    let newSentences := sortedMessages.map (λ x => Sentence.message x)
    let mergedSentences := List.mergeSortedLists (λ x y => (Positional.headPos x) < (Positional.headPos y)) newSentences self.sentences
    return { self with sentences := mergedSentences }
end AnalysisResult

structure TraversalAux where
  allowsNewField : Bool := true
  allowsNewTerm : Bool := true
  allowsNewSemantic : Bool := true
  result : AnalysisResult := AnalysisResult.empty

namespace TraversalAux
  def merge (x y : TraversalAux) : TraversalAux := {
    allowsNewField := x.allowsNewField ∧ y.allowsNewField
    allowsNewTerm := x.allowsNewTerm ∧ y.allowsNewTerm
    result := AnalysisResult.merge x.result y.result
  }

  def insertFragment (self : TraversalAux) (fragment : TraversalFragment) : AnalysisM TraversalAux :=
    match fragment with
    | TraversalFragment.term _ => do
      if self.allowsNewTerm then
        let newResult ← self.result.insertFragment fragment
        return { self with allowsNewTerm := false, result := newResult }
      else 
        return self
    | TraversalFragment.field _ => do
      if self.allowsNewField then
        let newResult ← self.result.insertFragment fragment
        return { self with allowsNewField := false, result := newResult }
      else 
        return self
    | TraversalFragment.tactic contextInfo => do
      let tacticChildren := self.result.sentences.filterMap (λ f => f.asTactic?)
      if tacticChildren.any (λ t => t.headPos == fragment.headPos && t.tailPos == fragment.tailPos) then
        return self
      else
        let newResult ← self.result.insertFragment fragment
        return { self with result := newResult }
    | _ => pure self

    def insertSemanticInfo (self : TraversalAux) (info : SemanticTraversalInfo) : AnalysisM TraversalAux := do
      if self.allowsNewSemantic then
        let newResult ← self.result.insertSemanticInfo info
        return { self with allowsNewSemantic := false,  result := newResult }
      else
        return self
end TraversalAux

partial def _resolveTacticList (ctx?: Option ContextInfo := none) (aux : TraversalAux := {}) (tree : InfoTree) : AnalysisM TraversalAux :=
  match tree with
  | InfoTree.context ctx tree => _resolveTacticList ctx aux tree
  | InfoTree.node info children =>
    match ctx? with
    | some ctx => do
      let ctx? := info.updateContext? ctx
      let resolvedChildrenLeafs ← children.toList.mapM (_resolveTacticList ctx? aux)
      let sortedChildrenLeafs := resolvedChildrenLeafs.foldl TraversalAux.merge {}
      let fragment := TraversalFragment.create ctx info
      match fragment with
      | (some fragment, some semantic) => do
        let sortedChildrenLeafs ← sortedChildrenLeafs.insertSemanticInfo semantic
        sortedChildrenLeafs.insertFragment fragment          
      | (some fragment, none) => sortedChildrenLeafs.insertFragment fragment          
      | (none, some semantic) => sortedChildrenLeafs.insertSemanticInfo semantic
      | (_, _) => pure sortedChildrenLeafs
    | none => pure aux
  | _ => pure aux

def resolveTacticList (trees: List InfoTree) : AnalysisM AnalysisResult := do
  let auxResults ← (trees.map _resolveTacticList).mapM (λ x => x)
  let results := auxResults.map (λ x => x.result)
  return results.foldl AnalysisResult.merge AnalysisResult.empty
