import Init
import Lean
import LeanInk.DataTypes

namespace LeanInk.Analysis
open Lean Elab Meta IO

def genSentences (ctx : ContextInfo) (info : TacticInfo) : IO (List TacticFragment) := do
  let goalsBefore ← genGoals ctx info true
  let goalsAfter ← genGoals ctx info false
  return [ { headPos := info.stx.getPos?.getD 0, tailPos := info.stx.getTailPos?.getD 0, goalsBefore := goalsBefore, goalsAfter := goalsAfter } ]
where
  genGoals (ctx : ContextInfo) (info : TacticInfo) (beforeNode : Bool) : IO (List String) :=
    if beforeNode then _genGoals ctx info.goalsBefore info.mctxBefore
    else _genGoals ctx info.goalsAfter info.mctxAfter
  _genGoals (ctx : ContextInfo) (goals : List MVarId) (metaCtx : MetavarContext) : IO (List String) := 
    { ctx with mctx := metaCtx }.runMetaM {} <| goals.mapM evalGoal >>= List.filterMapM pure
  evalGoal (mvarId : MVarId) : MetaM (Option String) := (some ∘ toString) <$> ppGoal mvarId

def insertFragment (sentences : List TacticFragment) (ctx : ContextInfo) (info : TacticInfo) := (sentences ++ ·) <$> 
  if sentences.any (λ t => t.headPos == info.stx.getPos? && t.tailPos == info.stx.getTailPos?) then 
    pure [] 
  else 
    genSentences ctx info

partial def _resolveTacticList (ctx? : Option ContextInfo := none) (aux : List TacticFragment := []) : InfoTree → IO (List TacticFragment)
  | InfoTree.context ctx tree => _resolveTacticList ctx aux tree
  | InfoTree.node info children =>
    match ctx? with
    | some ctx => do
      let resolvedChildrenLeafs ← children.toList.mapM <| _resolveTacticList (info.updateContext? ctx) aux 
      let sortedChildrenLeafs := resolvedChildrenLeafs.join
      if Info.isExpanded info then
        pure sortedChildrenLeafs
      else match info with
      | .ofTacticInfo tacticInfo => insertFragment sortedChildrenLeafs ctx tacticInfo
      | _ => pure sortedChildrenLeafs
    | none => pure aux
  | _ => pure aux

inductive TraversalEvent
| result (r : List TacticFragment)
| error (e : IO.Error)

def _resolveTask (tree : InfoTree) : IO (Task TraversalEvent) := do
  let taskBody : IO TraversalEvent := .result <$> _resolveTacticList none [] tree
  let task ← IO.asTask taskBody
  return task.map fun
    | .ok ev => ev
    | .error e => .error e

def resolveTasks (tasks : Array (Task TraversalEvent)) : IO <| Option <| List (List TacticFragment) := do
  let mut results : List (List TacticFragment) := []
  for task in tasks do
    let result ← BaseIO.toIO <| IO.wait task
    match result with
    | .result r => results := r :: results
    | _ => return none
  return results

def resolveTacticList (trees : List InfoTree) : IO (List TacticFragment) := do
  let tasks ← trees.toArray.mapM _resolveTask
  match (← resolveTasks tasks) with
  | some auxResults => return auxResults.join
  | _ => return []