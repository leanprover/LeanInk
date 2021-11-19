import LeanInk.Commands.Analyze.Configuration
import LeanInk.Output.AlectryonFragment

import Lean.Elab.Frontend
import Lean.Elab.Import
import Lean.Elab.Command
import Lean.Parser

import Lean.Util.Trace

namespace LeanInk.Commands.Analyze

open Lean
open Lean.Elab
open Output.AlectryonFragment

inductive AnalysisFragment where
  | tactic (i: TacticInfo) (ctx: ContextInfo)
  | term (i: TermInfo) (ctx: ContextInfo)
  | field (i: FieldInfo) (ctx: ContextInfo)

namespace AnalysisFragment
  def toFormat : AnalysisFragment -> IO Format
  | tactic i ctx => i.format ctx
  | term i ctx => i.format ctx
  | field i ctx => i.format ctx

  def toAlectryonFragment : AnalysisFragment -> Fragment
  | tactic _ _ => Fragment.text { contents := "tactic" }
  | term _ _ => Fragment.text { contents := "term" }
  | field _ _ => Fragment.text { contents := "field" }

end AnalysisFragment

-- INFO TREE analysis
def Info.toAnalysisFragment (info: Info) (ctx: ContextInfo) : Option AnalysisFragment := do
  match info with
  | Info.ofTacticInfo i => AnalysisFragment.tactic i ctx
  --| Info.ofTermInfo i => AnalysisFragment.term i ctx
  --| Info.ofFieldInfo i => AnalysisFragment.field i ctx
  | _ => none

partial def resolveLeafList (ctx?: Option ContextInfo := none) (tree: InfoTree) : List AnalysisFragment := do
  match tree with
  | InfoTree.context ctx tree => resolveLeafList ctx tree
  | InfoTree.node info children =>
    match ctx? with
    | none => return [] -- Add error handling
    | some ctx =>
      if children.isEmpty then
        match Info.toAnalysisFragment info ctx with
        | some f => [f]
        | none => []
      else
        let ctx := info.updateContext? ctx
        let resolvedChildren := children.toList.map (resolveLeafList ctx)
        return resolvedChildren.join
  | _ => []

def configureCommandState (env : Environment) (msg : MessageLog) : Command.State := do 
  return { Command.mkState env msg with infoState := { enabled := true }}

-- atm we just return a bool, but we should probably return a useful data structure that contains all analysis info
def analyzeInput (config: Configuration) : IO (Array Fragment) := do
  let context := Parser.mkInputContext config.inputFileContents config.inputFileName
  let (header, state, messages) ← Parser.parseHeader context
  let options := Options.empty.setBool `trace.Elab.info true
  let (environment, messages) ← processHeader header options messages context 0
  let commandState := configureCommandState environment messages
  let s <- IO.processCommands context state commandState
  let trees := s.commandState.infoState.trees

  IO.println s!"INFO! Trees enabled: {s.commandState.infoState.enabled}"
  IO.println s!"INFO! Gathered trees: {s.commandState.infoState.trees.size}"

  let fragments ← (trees.toList.map (resolveLeafList)).join

  for fragment in fragments do
    let format ← fragment.toFormat
    IO.println format

  return (fragments.map (λ f => f.toAlectryonFragment)).toArray