import LeanInk.Commands.Analyze.Configuration
import LeanInk.Commands.Analyze.InfoTreeUtil
import LeanInk.Commands.Analyze.Analysis

import LeanInk.Output.AlectryonFragment

import Lean.MetavarContext
import Lean.Elab.InfoTree

namespace LeanInk.Commands.Analyze

open Output.AlectryonFragment
open Lean
open Lean.Elab

structure CompoundFragment where
  headPos: String.Pos
  tailPos: String.Pos
  tactic: List TacticFragment
  messages: List MessageFragment

namespace TacticFragment
  private def resolveGoalsAux (ctx: ContextInfo) (mctx : MetavarContext) : List MVarId -> IO (List Format)
    | [] => []
    | goals => do
      let ctx := { ctx with mctx := mctx }
      return [← ctx.runMetaM {} (return Std.Format.prefixJoin "\n" (← goals.mapM (Meta.ppGoal .)))]

  def resolveGoals (self : TacticFragment) : IO (List Goal) := do
    let goalsBefore ← resolveGoalsAux self.ctx self.info.mctxBefore self.info.goalsBefore
    return ← goalsBefore.map (λ g => { name := "", conclusion := s!"{g}", hypotheses := #[] } )

  def toCompoundFragment (self : TacticFragment) : CompoundFragment :=
    { headPos := self.headPos, tailPos := self.tailPos, tactic := [self], messages := [] }
end TacticFragment

namespace MessageFragment
  def toCompoundFragment (self : MessageFragment) : CompoundFragment :=
    { headPos := self.headPos, tailPos := self.tailPos, tactic := [], messages := [self] }
end MessageFragment

def _annotateFileAux (l : List Fragment) (contents : String) (pos : String.Pos) (f : List TacticFragment) : IO (List Fragment) := do
  IO.println s!"Running Annotation (l: {l.length}) (pos: {pos}) (f: {f.length})"

  if contents.atEnd pos then
    return l
  else
    match f with
    -- We don't have any further tactics to annotate so we just return the rest of the contents as a text fragment.
    | [] => l.append [Fragment.text { contents := contents.extract pos contents.length }]
    | t::ts =>
      let fragment := Fragment.sentence { contents := contents.extract t.headPos t.tailPos, messages := #[], goals := (← t.resolveGoals).toArray }
      if t.headPos > pos then
        let textFragment := Fragment.text { contents := contents.extract pos t.headPos }
        return ← _annotateFileAux (l.append [textFragment, fragment]) contents t.tailPos ts
      else
        return ← _annotateFileAux (l.append [fragment]) contents t.tailPos ts

def _annotateFile (config: Configuration) (annotations: List TacticFragment) : IO (List Fragment) := _annotateFileAux [] config.inputFileContents 0 annotations

def annotateFile (config : Configuration) (analysis : AnalysisResult) : IO (List Fragment) := do
  let initialCompounds := analysis.tactics.map (λ t => t.toCompoundFragment)
  return []