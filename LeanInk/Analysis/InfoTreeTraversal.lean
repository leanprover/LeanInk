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

open Lean Elab Meta IO

set_option autoImplicit false

namespace TraversalFragment

  /-! Sentence Generation -/

  private def genGoals (ctx : ContextInfo) (info : TacticInfo) (beforeNode: Bool) : AnalysisM (List Goal) :=
    if beforeNode then
      _genGoals ctx info.goalsBefore info.mctxBefore
    else
      _genGoals ctx info.goalsAfter info.mctxAfter
  where
    _genGoals (ctx : ContextInfo) (goals: List MVarId) (metaCtx: MetavarContext) : AnalysisM (List Goal) := 
      let ctx := { ctx with mctx := metaCtx }
      return (← ctx.runMetaM {} <| goals.mapM evalGoal).filterMap id

    evalGoal (mvarId : MVarId) : MetaM (Option Goal) := do
      match (← getMCtx).findDecl? mvarId with
      | none => return none
      | some decl => return ← genGoal (← ppGoal mvarId) decl.userName

    genGoal (goalState : Format) : Name -> MetaM Goal
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

  def genTactic (ctx : ContextInfo) (info : TacticInfo) : AnalysisM Tactic := do
    let goalsBefore ← genGoals ctx info true
    let goalsAfter ← genGoals ctx info false
    if goalsAfter.isEmpty then  
      return { headPos := info.stx.getPos?.getD 0, tailPos := info.stx.getTailPos?.getD 0, goalsBefore := goalsBefore, goalsAfter := [{ name := "", goalState := "Goals accomplished! 🐙" }] }
    else
      return { headPos := info.stx.getPos?.getD 0, tailPos := info.stx.getTailPos?.getD 0, goalsBefore := goalsBefore, goalsAfter := goalsAfter }

  def genSentences (ctx : ContextInfo) (info : TacticInfo) : AnalysisM (List Sentence) := do
    let t ← genTactic ctx info
    return [t]

end TraversalFragment

/- Traversal -/
abbrev AnalysisResult := List Sentence

namespace AnalysisResult
  def empty : AnalysisResult := []

  def merge : AnalysisResult → AnalysisResult → AnalysisResult :=
    List.mergeSortedLists (λ x y => x.toFragment.headPos < y.toFragment.headPos)

  def insertFragment (sentences : AnalysisResult) (ctx : ContextInfo) (info : TacticInfo) : AnalysisM AnalysisResult := do
    let newSentences ← TraversalFragment.genSentences ctx info
    return sentences ++ newSentences

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

  def insertFragment (self : TraversalAux) (ctx : ContextInfo) (info : TacticInfo) : AnalysisM TraversalAux := do
    let tacticChildren := self.result
    if tacticChildren.any (λ t => t.headPos == info.stx.getPos? && t.tailPos == info.stx.getPos?) then
      return self
    else
      let newResult ← self.result.insertFragment ctx info
      return { self with result := newResult }

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
      if Info.isExpanded info then
        pure sortedChildrenLeafs
      else
        match info with
          | .ofTacticInfo tacticInfo => sortedChildrenLeafs.insertFragment ctx tacticInfo
          | _ => pure sortedChildrenLeafs
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
  | _ => return []
