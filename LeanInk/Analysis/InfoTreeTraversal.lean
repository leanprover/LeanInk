import Init
import Lean
import LeanInk.Configuration

namespace LeanInk.Analysis
open Lean Elab Meta IO

def genSentences (ctx : ContextInfo) (info : TacticInfo) : AnalysisM (List Sentence) := do
  let goalsBefore ← genGoals ctx info true
  let goalsAfter ← genGoals ctx info false
  return [ { headPos := info.stx.getPos?.getD 0, tailPos := info.stx.getTailPos?.getD 0, goalsBefore := goalsBefore, goalsAfter := goalsAfter } ]
where
  genGoals (ctx : ContextInfo) (info : TacticInfo) (beforeNode : Bool) : AnalysisM (List String) :=
    if beforeNode then _genGoals ctx info.goalsBefore info.mctxBefore
    else _genGoals ctx info.goalsAfter info.mctxAfter
  _genGoals (ctx : ContextInfo) (goals: List MVarId) (metaCtx: MetavarContext) : AnalysisM (List String) := 
    { ctx with mctx := metaCtx }.runMetaM {} <| goals.mapM evalGoal >>= List.filterMapM pure
  evalGoal (mvarId : MVarId) : MetaM (Option String) := (some ∘ toString) <$> ppGoal mvarId

def merge := List.mergeSortedLists (λ (x y : Sentence) => x.headPos < y.headPos)
def insertFragment (sentences : List Sentence) (ctx : ContextInfo) (info : TacticInfo) := (sentences ++ ·) <$> 
  if sentences.any (λ t => t.headPos == info.stx.getPos? && t.tailPos == info.stx.getTailPos?) then pure [] else genSentences ctx info

partial def _resolveTacticList (ctx?: Option ContextInfo := none) (aux : List Sentence := []) : InfoTree → AnalysisM (List Sentence)
  | InfoTree.context ctx tree => _resolveTacticList ctx aux tree
  | InfoTree.node info children =>
    match ctx? with
    | some ctx => do
      let ctx? := info.updateContext? ctx
      let resolvedChildrenLeafs ← children.toList.mapM <| _resolveTacticList ctx? aux 
      let sortedChildrenLeafs := resolvedChildrenLeafs.foldl merge []
      if Info.isExpanded info then
        pure sortedChildrenLeafs
      else
        match info with
        | .ofTacticInfo tacticInfo => insertFragment sortedChildrenLeafs ctx tacticInfo
        | _ => pure sortedChildrenLeafs
    | none => pure aux
  | _ => pure aux

inductive TraversalEvent
| result (r : List Sentence)
| error (e : IO.Error)

def _resolveTask (tree : InfoTree) : AnalysisM (Task TraversalEvent) := do
  let taskBody : AnalysisM TraversalEvent := .result <$> _resolveTacticList none [] tree
  let task ← IO.asTask (taskBody $ ← read)
  return task.map fun
    | .ok ev => ev
    | .error e => .error e

def resolveTasks (tasks : Array (Task TraversalEvent)) : AnalysisM <| Option <| List (List Sentence) := do
  let mut results : List (List Sentence) := []
  for task in tasks do
    let result ← BaseIO.toIO <| IO.wait task
    match result with
    | .result r => results := r :: results
    | _ => return none
  return results

def resolveTacticList (trees: List InfoTree) : AnalysisM (List Sentence) := do
  let tasks ← trees.toArray.mapM _resolveTask
  match (← resolveTasks tasks) with
  | some auxResults => return auxResults.foldl merge []
  | _ => return []