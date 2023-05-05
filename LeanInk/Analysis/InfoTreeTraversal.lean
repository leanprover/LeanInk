import Init.System.IO
import Init.Control.Except

import LeanInk.Analysis.DataTypes

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

set_option autoImplicit false

inductive TraversalFragment where
| tactic (ctx : ContextInfo) (info: TacticInfo)
| term (ctx : ContextInfo) (info: TermInfo)
| field (ctx : ContextInfo) (info: FieldInfo)
| unknown (ctx : ContextInfo) (info: ElabInfo)

namespace TraversalFragment
  def headPos : TraversalFragment -> String.Pos
  | term _ info => (info.toElabInfo.stx.getPos? false).getD 0
  | field _ info => (info.stx.getPos? false).getD 0
  | tactic _ info => (info.toElabInfo.stx.getPos? false).getD 0
  | unknown _ info => (info.stx.getPos? false).getD 0

  def tailPos : TraversalFragment -> String.Pos
  | term _ info => (info.toElabInfo.stx.getTailPos? false).getD 0
  | field _ info => (info.stx.getTailPos? false).getD 0
  | tactic _ info => (info.toElabInfo.stx.getTailPos? false).getD 0
  | unknown _ info => (info.stx.getTailPos? false).getD 0

  def create (ctx : ContextInfo) (info : Info) : AnalysisM <| (Option TraversalFragment) := do
    if Info.isExpanded info then
      pure none
    else
      match info with 
      | Info.ofTacticInfo info => pure <| tactic ctx info
      | Info.ofTermInfo info => pure <| term ctx info
      | Info.ofFieldInfo info => pure <| field ctx info
      | _ => pure none

  def runMetaM { α : Type } (func : TraversalFragment -> MetaM α) (fragment : TraversalFragment) : AnalysisM α :=
    match fragment with
    | term ctx info => ctx.runMetaM info.lctx (func fragment)
    | field ctx info => ctx.runMetaM info.lctx (func fragment)
    | tactic ctx _ => ctx.runMetaM {} (func fragment)
    | unknown ctx _ => ctx.runMetaM {} (func fragment)

  def inferType? : TraversalFragment -> MetaM (Option String)
    | term _ info => do
      -- This call requires almost half of the runtime of the tree traversal.
      let format ← try Meta.ppExpr (← Meta.inferType info.expr) catch e => e.toMessageData.toString
      return s!"{format}"
    | _ => pure none

  def genDocString? (self : TraversalFragment) : MetaM (Option String) := do
    let env ← getEnv
    match self with
    | term _ info =>
      if let some name := info.expr.constName? then
        findDocString? env name
      else
        pure none
    | field _ info => findDocString? env info.projName
    | tactic _ info =>
      let elabInfo := info.toElabInfo
      return ← findDocString? env elabInfo.elaborator <||> findDocString? env elabInfo.stx.getKind
    | unknown _ info =>
      let elabInfo := info
      return ← findDocString? env elabInfo.elaborator <||> findDocString? env elabInfo.stx.getKind

  /- Sentence Generation -/
  private def genGoal (goalState : Format) : Name -> MetaM Goal
    | Name.anonymous => do
      return { 
        name := ""
        goalState := toString goalState
      }
    | name => do
      let goalFormatName := format name.eraseMacroScopes
      return { 
        name := toString goalFormatName
        goalState := toString goalState
      }

  private def evalGoal (mvarId : MVarId) : MetaM (Option Goal) := do
    match (← getMCtx).findDecl? mvarId with
      | none => return none
      | some decl => return ← genGoal (← ppGoal mvarId) decl.userName

  private def genGoals (ctx : ContextInfo) (info : TacticInfo) (beforeNode: Bool) : AnalysisM (List Goal) :=
    if beforeNode then
      _genGoals ctx info.goalsBefore info.mctxBefore
    else
      _genGoals ctx info.goalsAfter info.mctxAfter
  where
    _genGoals (ctx : ContextInfo) (goals: List MVarId) (metaCtx: MetavarContext) : AnalysisM (List Goal) := 
      let ctx := { ctx with mctx := metaCtx }
      return (← ctx.runMetaM {} (goals.mapM (fun x => evalGoal x))).filterMap id

  def genTactic? (self : TraversalFragment) : AnalysisM (Option Tactic) := do
    match self with
    | tactic ctx info => do 
      let goalsBefore ← genGoals ctx info true
      let goalsAfter ← genGoals ctx info false
      if goalsAfter.isEmpty then  
        return some { headPos := self.headPos, tailPos := self.tailPos, goalsBefore := goalsBefore, goalsAfter := [{ name := "", goalState := "Goals accomplished! 🐙" }] }
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
  sentences : List Sentence
  deriving Inhabited

namespace AnalysisResult
  def empty : AnalysisResult := { sentences := [] }

  def merge (x y : AnalysisResult) : AnalysisResult := {
    sentences := List.mergeSortedLists (λ x y => x.toFragment.headPos < y.toFragment.headPos) x.sentences y.sentences
  }

  def insertFragment (self : AnalysisResult) (fragment : TraversalFragment) : AnalysisM AnalysisResult := do
    let newSentences ← fragment.genSentences
    pure { self with sentences := self.sentences.append newSentences }

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

  def insertFragment (self : TraversalAux) (fragment : TraversalFragment) : AnalysisM TraversalAux := do
    match fragment with
    | .term _ _ => do
      if self.allowsNewTerm then
        let newResult ← self.result.insertFragment fragment
        return { self with allowsNewTerm := false, result := newResult }
      else 
        return self
    | .field _ _ => do
      if self.allowsNewField then
        let newResult ← self.result.insertFragment fragment
        return { self with allowsNewField := false, result := newResult }
      else 
        return self
    | .tactic _ _ => do
      let tacticChildren := self.result.sentences.filterMap (λ f => f.asTactic?)
      if tacticChildren.any (λ t => t.headPos == fragment.headPos && t.tailPos == fragment.tailPos) then
        return self
      else
        let newResult ← self.result.insertFragment fragment
        return { self with result := newResult }
    | _ => pure self

end TraversalAux

partial def _resolveTacticList (ctx?: Option ContextInfo := none) (aux : TraversalAux := {}) (tree : InfoTree) : AnalysisM TraversalAux := do
  match tree with
  | InfoTree.context ctx tree => _resolveTacticList ctx aux tree
  | InfoTree.node info children =>
    match ctx? with
    | some ctx => do
      let ctx? := info.updateContext? ctx
      let resolvedChildrenLeafs ← children.toList.mapM (fun x => _resolveTacticList ctx? aux x) 
      let sortedChildrenLeafs := resolvedChildrenLeafs.foldl TraversalAux.merge {}
      match (← TraversalFragment.create ctx info) with
      | some fragment => sortedChildrenLeafs.insertFragment fragment
      | none => pure sortedChildrenLeafs
    | none => pure aux
  | _ => pure aux

inductive TraversalEvent
| result (r : TraversalAux)
| error (e : IO.Error)

def _resolveTask (tree : InfoTree) : AnalysisM (Task TraversalEvent) := do
  let taskBody : AnalysisM TraversalEvent := do
    let res ← _resolveTacticList none {} tree
    return TraversalEvent.result res
  let task ← IO.asTask (taskBody $ ← read)
  return task.map fun
    | Except.ok ev => ev
    | Except.error e => TraversalEvent.error e

def _resolve (trees: List InfoTree) : AnalysisM AnalysisResult := do
  let auxResults ← (trees.map (λ t => 
    _resolveTacticList none {} t)).mapM (λ x => x)
  let results := auxResults.map (λ x => x.result)
  return results.foldl AnalysisResult.merge AnalysisResult.empty

def resolveTasks (tasks : Array (Task TraversalEvent)) : AnalysisM (Option (List TraversalAux)) := do
  let mut results : List TraversalAux := []
  for task in tasks do
    let result ← BaseIO.toIO <| IO.wait task
    match result with
    | TraversalEvent.result r => results := r::results
    | _ => return none
  return results

def resolveTacticList (trees: List InfoTree) : AnalysisM AnalysisResult := do
  let tasks ← trees.toArray.mapM (λ t => _resolveTask t)
  match (← resolveTasks tasks) with
  | some auxResults => do
    let results := auxResults.map (λ x => x.result)
    return results.foldl AnalysisResult.merge AnalysisResult.empty
  | _ => return { sentences := [] }
