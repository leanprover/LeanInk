import Init.System.IO
import Init.Control.Except

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
open IO

structure ContextBasedInfo (β : Type) where
  ctx : ContextInfo
  info : β

inductive TraversalFragment where
| tactic (info: ContextBasedInfo TacticInfo)
| term (info: ContextBasedInfo TermInfo)
| field (info: ContextBasedInfo FieldInfo)
| unknown (info: ContextBasedInfo ElabInfo)

namespace TraversalFragment
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

  def create (ctx : ContextInfo) (info : Info) : AnalysisM ((Option TraversalFragment) × (Option SemanticTraversalInfo)) := do
    if Info.isExpanded info then
      pure (none, none)
    else
      let mut semantic : Option SemanticTraversalInfo := none 
      if (← read).experimentalSemanticType then
        semantic := some { node := info, stx := info.stx }
      match info with 
      | Info.ofTacticInfo info => pure (tactic { info := info, ctx := ctx }, semantic)
      | Info.ofTermInfo info => pure (term { info := info, ctx := ctx }, semantic)
      | Info.ofFieldInfo info => pure (field { info := info, ctx := ctx }, semantic)
      | _ => pure (none, semantic)

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
      -- This call requires almost half of the runtime of the tree traversal.
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
  private def evalGoal (mvarId : MVarId) (hasSorry: Bool) : MetaM (Option Goal) := do
    match (← getMCtx).findDecl? mvarId with
    | none => return none
    | some decl => do
      if hasSorry then 
        return none
      else
        let ppAuxDecls := pp.auxDecls.get (← getOptions)
        let ppImplDetailHyps := pp.implementationDetailHyps.get (← getOptions)
        let lctx := decl.lctx.sanitizeNames.run' { options := (← getOptions) }
        withLCtx lctx decl.localInstances do
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
              match localDecl with
              | LocalDecl.cdecl _ _ varName type _ _ =>
                let varName := varName.simpMacroScopes
                let type ← instantiateMVars type
                if prevType? == none || prevType? == some type then
                  pure (varName::varNames, some type, hypotheses)
                else do
                  let hypotheses ← pushPending hypotheses prevType? varNames
                  pure ([varName], some type, hypotheses)
              | LocalDecl.ldecl _ _ varName type val _ _ => do
                let varName := varName.simpMacroScopes
                let hypotheses ← pushPending hypotheses prevType? varNames
                let type ← instantiateMVars type
                let val  ← instantiateMVars val
                let typeFmt ← ppExpr type
                let valFmt ← ppExpr val
                pure ([], none, hypotheses.append [{ names := [varName.toString], body := s!"{valFmt}", type := s!"{typeFmt}" }])
          let (varNames, type?, hypotheses) ← lctx.foldlM (init := ([], none, [])) λ (varNames, prevType?, hypotheses) (localDecl : LocalDecl) =>
          if !ppAuxDecls && localDecl.isAuxDecl || !ppImplDetailHyps && localDecl.isImplementationDetail then
              pure (varNames, prevType?, hypotheses)
            else
              evalVar varNames prevType? hypotheses localDecl
          let hypotheses ← pushPending hypotheses type? varNames 
          let typeFmt ← ppExpr (← instantiateMVars decl.type)
          return (← genGoal typeFmt hypotheses decl.userName)

  private def _genGoals (contextInfo : ContextBasedInfo TacticInfo) (goals: List MVarId) (metaCtx: MetavarContext) (hasSorry: Bool) : AnalysisM (List Goal) := 
    let ctx := { contextInfo.ctx with mctx := metaCtx }
    return (← ctx.runMetaM {} (goals.mapM (fun x => evalGoal x hasSorry))).filterMap id

  private def genGoals (contextInfo : ContextBasedInfo TacticInfo) (beforeNode: Bool) (hasSorry : Bool): AnalysisM (List Goal) :=
    if beforeNode then
      _genGoals contextInfo contextInfo.info.goalsBefore contextInfo.info.mctxBefore hasSorry
    else
      _genGoals contextInfo contextInfo.info.goalsAfter contextInfo.info.mctxAfter hasSorry

  def genTactic? (self : TraversalFragment) (hasSorry : Bool) : AnalysisM (Option Tactic) := do
    match self with
    | tactic fragment => do 
      let goalsBefore ← genGoals fragment true hasSorry
      let goalsAfter ← genGoals fragment false hasSorry
      if hasSorry then do 
        return some { headPos := self.headPos, tailPos := self.tailPos, goalsBefore := [], goalsAfter := [] }
      if goalsAfter.isEmpty then  
        return some { headPos := self.headPos, tailPos := self.tailPos, goalsBefore := goalsBefore, goalsAfter := [{ name := "", conclusion := "Goals accomplished! 🐙", hypotheses := [] }] }
      else
        return some { headPos := self.headPos, tailPos := self.tailPos, goalsBefore := goalsBefore, goalsAfter := goalsAfter }
    | _ => pure none

  def genSentences (self : TraversalFragment) (hasSorry : Bool) : AnalysisM (List Sentence) := do
    if let some t ← self.genTactic? hasSorry then
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

  def insertFragment (self : AnalysisResult) (fragment : TraversalFragment) (hasSorry : Bool) : AnalysisM AnalysisResult := do
    let newTokens : List Token := ← fragment.genTokens
    let newSentences ← fragment.genSentences hasSorry
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

  def insertMessages (self : AnalysisResult) (messages : List Lean.Message) (fileMap : FileMap) : AnalysisM AnalysisResult := do
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

  def insertFragment (self : TraversalAux) (fragment : TraversalFragment) (hasSorry : Bool) : AnalysisM TraversalAux := do
    match fragment with
    | TraversalFragment.term _ => do
      if self.allowsNewTerm then
        let newResult ← self.result.insertFragment fragment hasSorry
        return { self with allowsNewTerm := false, result := newResult }
      else 
        return self
    | TraversalFragment.field _ => do
      if self.allowsNewField then
        let newResult ← self.result.insertFragment fragment hasSorry
        return { self with allowsNewField := false, result := newResult }
      else 
        return self
    | TraversalFragment.tactic contextInfo => do
      let tacticChildren := self.result.sentences.filterMap (λ f => f.asTactic?)
      if tacticChildren.any (λ t => t.headPos == fragment.headPos && t.tailPos == fragment.tailPos) then
        return self
      else
        let newResult ← self.result.insertFragment fragment hasSorry
        return { self with result := newResult }
    | _ => pure self

    def insertSemanticInfo (self : TraversalAux) (info : SemanticTraversalInfo) : AnalysisM TraversalAux := do
      if self.allowsNewSemantic then
        let newResult ← self.result.insertSemanticInfo info
        return { self with allowsNewSemantic := false,  result := newResult }
      else
        return self
end TraversalAux

partial def _resolveTacticList (ctx?: Option ContextInfo := none) (aux : TraversalAux := {}) (tree : InfoTree) (hasSorry : Bool): AnalysisM TraversalAux :=
  match tree with
  | InfoTree.context ctx tree => _resolveTacticList ctx aux tree hasSorry
  | InfoTree.node info children =>
    match ctx? with
    | some ctx => do
      let ctx? := info.updateContext? ctx
      let resolvedChildrenLeafs ← children.toList.mapM (fun x => _resolveTacticList ctx? aux x hasSorry) 
      let sortedChildrenLeafs := resolvedChildrenLeafs.foldl TraversalAux.merge {}
      match (← TraversalFragment.create ctx info) with
      | (some fragment, some semantic) => do
        let sortedChildrenLeafs ← sortedChildrenLeafs.insertSemanticInfo semantic
        sortedChildrenLeafs.insertFragment fragment hasSorry         
      | (some fragment, none) => sortedChildrenLeafs.insertFragment fragment hasSorry         
      | (none, some semantic) => sortedChildrenLeafs.insertSemanticInfo semantic
      | (_, _) => pure sortedChildrenLeafs
    | none => pure aux
  | _ => pure aux

inductive TraversalEvent
| result (r : TraversalAux)
| error (e : IO.Error)

partial def _hasSorry (t : InfoTree) : Bool := 
  let rec go (ci? : Option ContextInfo) (t : InfoTree) : Bool :=
    match t with
    | InfoTree.context ci t => go ci t
    | InfoTree.node i cs =>
      if let (some _, .ofTermInfo ti) := (ci?, i) then 
        -- let expr := ti.runMetaM ci (instantiateMVars ti.expr)
        ti.expr.hasSorry
        -- we assume that `cs` are subterms of `ti.expr` and
        -- thus do not have to be checked as well
      else 
        cs.any (go ci?)
    | _ => false
  go none t

def _resolveTask (tree : InfoTree) (hasSorry : Bool) : AnalysisM (Task TraversalEvent) := do
  let taskBody : AnalysisM TraversalEvent := do
    let res ← _resolveTacticList none {} tree hasSorry
    return TraversalEvent.result res
  let task ← IO.asTask (taskBody $ ← read)
  return task.map fun
    | Except.ok ev => ev
    | Except.error e => TraversalEvent.error e

def _resolve (trees: List InfoTree) : AnalysisM AnalysisResult := do
  let config ← read
  let auxResults ← (trees.map (λ t => 
  if config.experimentalSorryConfig 
    then _resolveTacticList none {} t (_hasSorry t)
    else _resolveTacticList none {} t false)).mapM (λ x => x)
  let results := auxResults.map (λ x => x.result)
  return results.foldl AnalysisResult.merge AnalysisResult.empty

def resolveTasks (tasks : Array (Task TraversalEvent)) : AnalysisM (Option (List TraversalAux)) := do
  let mut results : List TraversalAux := []
  for task in tasks do
    let result ← BaseIO.toIO (IO.wait task)
    match result with
    | TraversalEvent.result r => results := r::results
    | _ => return none
  return results

def resolveTacticList (trees: List InfoTree) : AnalysisM AnalysisResult := do
  let config ← read
  let tasks ← trees.toArray.mapM (λ t => if config.experimentalSorryConfig 
    then _resolveTask t (_hasSorry t)
    else _resolveTask t false)
  match (← resolveTasks tasks) with
  | some auxResults => do
    let results := auxResults.map (λ x => x.result)
    return results.foldl AnalysisResult.merge AnalysisResult.empty
  | _ => return { tokens := [], sentences := []}
