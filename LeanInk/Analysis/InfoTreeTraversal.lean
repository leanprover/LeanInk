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

  private def genGoals (ctx : ContextInfo) (info : TacticInfo) (beforeNode: Bool) : AnalysisM (List String) :=
    if beforeNode then
      _genGoals ctx info.goalsBefore info.mctxBefore
    else
      _genGoals ctx info.goalsAfter info.mctxAfter
  where
    _genGoals (ctx : ContextInfo) (goals: List MVarId) (metaCtx: MetavarContext) : AnalysisM (List String) := 
      let ctx := { ctx with mctx := metaCtx }
      return (← ctx.runMetaM {} <| goals.mapM evalGoal).filterMap id

    evalGoal (mvarId : MVarId) : MetaM (Option String) := do
      return toString (← ppGoal mvarId)

  def genTactic (ctx : ContextInfo) (info : TacticInfo) : AnalysisM Tactic := do
    let goalsBefore ← genGoals ctx info true
    let goalsAfter ← genGoals ctx info false
    if goalsAfter.isEmpty then  
      return { headPos := info.stx.getPos?.getD 0, tailPos := info.stx.getTailPos?.getD 0, goalsBefore := goalsBefore, goalsAfter := ["Goals accomplished! 🐙"] }
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
    List.mergeSortedLists (λ x y => x.headPos < y.headPos)

  def insertFragment (sentences : AnalysisResult) (ctx : ContextInfo) (info : TacticInfo) : AnalysisM AnalysisResult := do
    let newSentences ← TraversalFragment.genSentences ctx info
    return sentences ++ newSentences

end AnalysisResult

abbrev TraversalAux := List Sentence

namespace TraversalAux
  def merge := AnalysisResult.merge

  def insertFragment (self : TraversalAux) (ctx : ContextInfo) (info : TacticInfo) : AnalysisM TraversalAux := do
    let tacticChildren := self
    if tacticChildren.any (λ t => t.headPos == info.stx.getPos? && t.tailPos == info.stx.getPos?) then
      return self
    else
      AnalysisResult.insertFragment self ctx info

end TraversalAux

partial def _resolveTacticList (ctx?: Option ContextInfo := none) (aux : TraversalAux := {}) : InfoTree → AnalysisM TraversalAux
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
  let auxResults ← trees.mapM <| _resolveTacticList none {}
  return auxResults.foldl AnalysisResult.merge AnalysisResult.empty

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
    return auxResults.foldl AnalysisResult.merge AnalysisResult.empty
  | _ => return []
