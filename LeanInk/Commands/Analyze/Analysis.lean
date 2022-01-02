import LeanInk.Commands.Analyze.Configuration
import LeanInk.Commands.Analyze.InfoTreeUtil
import LeanInk.Commands.Analyze.ListUtil
import LeanInk.Commands.Analyze.LeanContext
import LeanInk.Commands.Analyze.Logger

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
  | type (fragment: TermFragment)
  deriving Inhabited

namespace AnalysisFragment
  def headPos : AnalysisFragment -> String.Pos
    | tactic f => f.headPos
    | message f => f.headPos
    | type f => f.headPos

  def tailPos : AnalysisFragment -> String.Pos
    | tactic f => f.tailPos
    | message f => f.tailPos
    | type f => f.tailPos

  def asTactic : AnalysisFragment -> Option TacticFragment
    | tactic f => f
    | _ => none

  def asMessage : AnalysisFragment -> Option MessageFragment
    | message f => f
    | _ => none

  def asType : AnalysisFragment -> Option TermFragment
    | type f => f
    | _ => none

end AnalysisFragment

instance : ToFormat AnalysisFragment where
  format (self : AnalysisFragment) : Format :=
    match self with
    | AnalysisFragment.tactic _ => f!"TACTIC  [{self.headPos}]->[{self.tailPos}]"
    | AnalysisFragment.message _ => f!"MESSAGE [{self.headPos}]->[{self.tailPos}]"
    | AnalysisFragment.type _ => f!"TYPE [{self.headPos}]->[{self.tailPos}]"

structure AnalysisResult where
  sentenceFragments: List AnalysisFragment
  hoverFragments: List AnalysisFragment

namespace AnalysisResult

def create (traversal: TraversalResult) (messages: List Message) (fileMap: FileMap) : AnalysisM AnalysisResult := do
  let tactics := traversal.tactics.map (λ f => AnalysisFragment.tactic f)
  let messages := messages.map (λ m => AnalysisFragment.message (MessageFragment.mkFragment fileMap m))
  let filteredMessages := messages.filter (λ f => f.headPos < f.tailPos)
  let sortedMessages := List.sort (λ x y => x.headPos < y.headPos) filteredMessages
  Logger.logInfo f!"MESSAGES:\n {sortedMessages}"
  let sentenceFragments := List.mergeSortedLists (λ x y => x.headPos < y.headPos) tactics sortedMessages
  Logger.logInfo f!"RESULT:\n {sentenceFragments}"
  if (← read).experimentalTokens then
    let terms := traversal.terms.map (λ f => AnalysisFragment.type f)
    return { sentenceFragments := sentenceFragments, hoverFragments := terms }
  else
    return { sentenceFragments := sentenceFragments, hoverFragments := [] }

end AnalysisResult

def configureCommandState (env : Environment) (msg : MessageLog) : Command.State :=
  { Command.mkState env msg with infoState := { enabled := true }}

def analyzeInput : AnalysisM AnalysisResult := do
  let config := ← read
  let context := Parser.mkInputContext config.inputFileContents config.inputFileName
  let (header, state, messages) ← Parser.parseHeader context
  initializeSearchPaths header config
  let options := Options.empty.setBool `trace.Elab.info true
  let (environment, messages) ← processHeader header options messages context 0
  Logger.logInfo s!"Header: {environment.header.mainModule}"
  Logger.logInfo s!"Header: {environment.header.moduleNames}"
  let commandState := configureCommandState environment messages
  let s ← IO.processCommands context state commandState
  let trees := s.commandState.infoState.trees.toList
  let traversalResult := resolveTacticList trees
  let messages := s.commandState.messages.msgs.toList
  return ← AnalysisResult.create traversalResult messages context.fileMap