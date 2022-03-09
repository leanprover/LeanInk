import LeanInk.Analysis.DataTypes
import LeanInk.Analysis.LeanContext
import LeanInk.Analysis.SemanticToken
import LeanInk.Analysis.InfoTreeTraversal

import LeanInk.Configuration
import LeanInk.Logger

import Lean.Elab.Frontend
import Lean.Elab.Import
import Lean.Elab.Command
import Lean.Parser

import Lean.Util.Trace

namespace LeanInk.Analysis

open Lean
open Lean.Elab

def configureCommandState (env : Environment) (msg : MessageLog) : Command.State :=
  { Command.mkState env msg with infoState := { enabled := true }}

def analyzeInput : AnalysisM AnalysisResult := do
  let config := ← read
  let context := Parser.mkInputContext config.inputFileContents config.inputFileName
  let (header, state, messages) ← Parser.parseHeader context
  initializeSearchPaths header config
    let options := Options.empty.setBool `trace.Elab.info true
  let (environment, messages) ← processHeader header options messages context 0
  logInfo s!"Header: {environment.header.mainModule}"
  logInfo s!"Header: {environment.header.moduleNames}"
  let commandState := configureCommandState environment messages
  let s ← IO.processCommands context state commandState
  let result ← resolveTacticList s.commandState.infoState.trees.toList
  let messages := s.commandState.messages.msgs.toList.filter (λ m => m.endPos.isSome )
  return ← result.insertMessages messages context.fileMap 