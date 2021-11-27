import LeanInk.Commands.Analyze.Configuration
import LeanInk.Commands.Analyze.InfoTreeUtil
import LeanInk.Commands.Analyze.ListUtil

import Lean.Elab.Frontend
import Lean.Elab.Import
import Lean.Elab.Command
import Lean.Parser

import Lean.Util.Trace

namespace LeanInk.Commands.Analyze

open Lean
open Lean.Elab

inductive AnalysisFragment where
  | tactic (fragment: TacticFragment)
  | message (fragment: MessageFragment)
  deriving Inhabited

namespace AnalysisFragment
  def headPos : AnalysisFragment -> String.Pos
    | tactic f => f.headPos
    | message f => f.headPos

  def tailPos : AnalysisFragment -> String.Pos
    | tactic f => f.tailPos
    | message f => f.tailPos
end AnalysisFragment

def configureCommandState (env : Environment) (msg : MessageLog) : Command.State := do 
  return { Command.mkState env msg with infoState := { enabled := true }}

def analyzeInput (config: Configuration) : IO (List AnalysisFragment) := do
  let context := Parser.mkInputContext config.inputFileContents config.inputFileName
  let (header, state, messages) ← Parser.parseHeader context
  let options := Options.empty.setBool `trace.Elab.info true
  let (environment, messages) ← processHeader header options messages context 0
  let commandState := configureCommandState environment messages
  let s ← IO.processCommands context state commandState

  let trees := s.commandState.infoState.trees.toList
  let tactics := (resolveTacticList trees).map (λ f => AnalysisFragment.tactic f)
  let messages := s.commandState.messages.msgs.toList.map (λ m => AnalysisFragment.message (MessageFragment.mkFragment context.fileMap m))

  return List.mergeSort (λ x y => x.headPos < y.headPos) tactics messages