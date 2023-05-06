import Init
import Lean
import LeanInk.Configuration

namespace LeanInk.Analysis
open Lean Elab Meta IO

namespace TraversalFragment
  /-! Sentence Generation -/
  private def genGoals (ctx : ContextInfo) (info : TacticInfo) (beforeNode: Bool) : AnalysisM (List String) :=
    if beforeNode then
      _genGoals ctx info.goalsBefore info.mctxBefore
    else
      _genGoals ctx info.goalsAfter info.mctxAfter
    where
    _genGoals (ctx : ContextInfo) (goals: List MVarId) (metaCtx: MetavarContext) : AnalysisM (List String) := 
      { ctx with mctx := metaCtx }.runMetaM {} <| goals.mapM evalGoal >>= List.filterMapM pure
    evalGoal (mvarId : MVarId) : MetaM (Option String) := (some ∘ toString) <$> ppGoal mvarId

  def genTactic (ctx : ContextInfo) (info : TacticInfo) : AnalysisM Tactic := do
    let goalsBefore ← genGoals ctx info true
    let goalsAfter ← genGoals ctx info false
    if goalsAfter.isEmpty then  
      return { headPos := info.stx.getPos?.getD 0, tailPos := info.stx.getTailPos?.getD 0, goalsBefore := goalsBefore, goalsAfter := ["Goals accomplished! 🐙"] }
    else
      return { headPos := info.stx.getPos?.getD 0, tailPos := info.stx.getTailPos?.getD 0, goalsBefore := goalsBefore, goalsAfter := goalsAfter }

  def genSentences (ctx : ContextInfo) (info : TacticInfo) : AnalysisM (List Sentence) := genTactic ctx info >>= pure ∘ ([·])

end TraversalFragment

/- Traversal -/
abbrev AnalysisResult := List Sentence

def AnalysisResult.merge : AnalysisResult → AnalysisResult → AnalysisResult := List.mergeSortedLists (λ x y => x.headPos < y.headPos)

def AnalysisResult.insertFragment (sentences : AnalysisResult) (ctx : ContextInfo) (info : TacticInfo) : AnalysisM AnalysisResult :=
  (sentences ++ ·) <$> TraversalFragment.genSentences ctx info

abbrev TraversalAux := AnalysisResult

def TraversalAux.insertFragment (self : TraversalAux) (ctx : ContextInfo) (info : TacticInfo) : AnalysisM TraversalAux := do
  if self.any (λ t => t.headPos == info.stx.getPos? && t.tailPos == info.stx.getTailPos?) then return self
  else AnalysisResult.insertFragment self ctx info

partial def _resolveTacticList (ctx?: Option ContextInfo := none) (aux : TraversalAux := .nil) : InfoTree → AnalysisM TraversalAux
  | InfoTree.context ctx tree => _resolveTacticList ctx aux tree
  | InfoTree.node info children =>
    match ctx? with
    | some ctx => do
      let ctx? := info.updateContext? ctx
      let resolvedChildrenLeafs ← children.toList.mapM <| _resolveTacticList ctx? aux 
      let sortedChildrenLeafs := resolvedChildrenLeafs.foldl .merge .nil
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
  let taskBody : AnalysisM TraversalEvent := .result <$> _resolveTacticList none [] tree
  let task ← IO.asTask (taskBody $ ← read)
  return task.map fun
    | .ok ev => ev
    | .error e => .error e

def resolveTasks (tasks : Array (Task TraversalEvent)) : AnalysisM <| Option (List TraversalAux) := do
  let mut results : List TraversalAux := []
  for task in tasks do
    let result ← BaseIO.toIO <| IO.wait task
    match result with
    | .result r => results := r::results
    | _ => return none
  return results

def resolveTacticList (trees: List InfoTree) : AnalysisM AnalysisResult := do
  let tasks ← trees.toArray.mapM _resolveTask
  match (← resolveTasks tasks) with
  | some auxResults => do
    return auxResults.foldl .merge .nil
  | _ => return []
