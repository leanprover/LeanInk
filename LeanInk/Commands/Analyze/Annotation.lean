import LeanInk.Commands.Analyze.Configuration
import LeanInk.Commands.Analyze.InfoTreeUtil

import LeanInk.Output.AlectryonFragment

import Lean.MetavarContext
import Lean.Elab.InfoTree

namespace LeanInk.Commands.Analyze

open Output.AlectryonFragment
open Lean
open Lean.Elab

namespace TacticFragment
  private def resolveGoalsAux (ctx: ContextInfo) (mctx : MetavarContext) : List MVarId -> IO (List Format)
    | [] => []
    | goals => do
      let ctx := { ctx with mctx := mctx }
      return [← ctx.runMetaM {} (return Std.Format.prefixJoin "\n" (← goals.mapM (Meta.ppGoal .)))]

  def resolveGoals (self : TacticFragment) : IO (List Goal) := do
    let goalsBefore ← resolveGoalsAux self.ctx self.info.mctxBefore self.info.goalsBefore
    return ← goalsBefore.map (λ g => { name := "", conclusion := s!"{g}", hypotheses := #[] } )

end TacticFragment

def annotateFileAux (l : List Fragment) (contents : String) (pos : String.Pos) (f : List TacticFragment) : IO (List Fragment) := do
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
        return ← annotateFileAux (l.append [textFragment, fragment]) contents t.tailPos ts
      else
        return ← annotateFileAux (l.append [fragment]) contents t.tailPos ts

def annotateFile (config: Configuration) (annotations: List TacticFragment) : IO (List Fragment) := annotateFileAux [] config.inputFileContents 0 annotations