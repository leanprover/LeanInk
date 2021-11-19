import LeanInk.Commands.Analyze.Configuration
import LeanInk.Commands.Analyze.Utility

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
  deriving Inhabited -- We need this so we can use qSort on an Array AnalysisFragment.

namespace AnalysisFragment
  def toFormat : AnalysisFragment -> IO Format
  | tactic i ctx => i.format ctx
  | term i ctx => i.format ctx
  | field i ctx => i.format ctx

  def headPos : AnalysisFragment -> String.Pos
  | tactic i _ => (i.toElabInfo.stx.getPos? true).getD 0
  | term i _ => (i.toElabInfo.stx.getPos? true).getD 0
  | field i _ => (i.stx.getPos? true).getD 0

  def toAlectryonFragment (fragment: AnalysisFragment) : Fragment := Fragment.text { contents := s!"{fragment.headPos}" }
end AnalysisFragment

def mergeSortLists [Inhabited α] (f: α -> α -> Bool) : List α -> List α -> List α
  | [], x => (x.toArray.qsort f).toList -- this should already be a sorted list ideally but somehow it didn't work, so that's a workaround atm
  | x, [] => (x.toArray.qsort f).toList -- the size of x should be negligible anyway
  | x::xs, y::ys => 
    if f x y then
      return x::y::mergeSortLists f xs ys
    else
      return y::x::mergeSortLists f xs ys

def mergeSortedAF : List AnalysisFragment -> List AnalysisFragment -> List AnalysisFragment := mergeSortLists (λ x y => x.headPos < y.headPos)
def joinSortedAF : List (List AnalysisFragment) -> List AnalysisFragment := List.foldl mergeSortedAF []

-- INFO TREE analysis
def Info.toAnalysisFragment (info: Info) (ctx: ContextInfo) : Option AnalysisFragment := do
  match info with
  | Info.ofTacticInfo i => AnalysisFragment.tactic i ctx
  | Info.ofTermInfo i => AnalysisFragment.term i ctx
  | Info.ofFieldInfo i => AnalysisFragment.field i ctx
  | _ => none

partial def resolveLeafList (ctx?: Option ContextInfo := none) (tree: InfoTree) : List AnalysisFragment := do
  match tree with
  | InfoTree.context ctx tree => resolveLeafList ctx tree
  | InfoTree.node info children =>
    match ctx? with
    | none => return [] -- Add error handling
    | some ctx =>
        let updatedCtx? := info.updateContext? ctx
        let resolvedChildren := joinSortedAF (children.toList.map (resolveLeafList updatedCtx?))
        match updatedCtx? with
        | none => return [] -- Add error handling
        | some ctx =>
          match Info.toAnalysisFragment info ctx with
          | some f => mergeSortedAF [f] resolvedChildren
          | none => resolvedChildren
  | _ => []

def configureCommandState (env : Environment) (msg : MessageLog) : Command.State := do 
  return { Command.mkState env msg with infoState := { enabled := true }}

def analyzeInput (config: Configuration) : IO (List Fragment) := do
  let context := Parser.mkInputContext config.inputFileContents config.inputFileName
  let (header, state, messages) ← Parser.parseHeader context
  let options := Options.empty.setBool `trace.Elab.info true
  let (environment, messages) ← processHeader header options messages context 0
  let commandState := configureCommandState environment messages
  let s <- IO.processCommands context state commandState
  let trees := s.commandState.infoState.trees

  IO.println s!"INFO! Trees enabled: {s.commandState.infoState.enabled}"
  IO.println s!"INFO! Gathered trees: {s.commandState.infoState.trees.size}"

  let fragments ← joinSortedAF (trees.toList.map (resolveLeafList))
  return fragments.map (λ x => x.toAlectryonFragment)