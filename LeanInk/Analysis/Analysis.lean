import LeanInk.Analysis.DataTypes
import LeanInk.Analysis.LeanContext
import LeanInk.Analysis.InfoTreeTraversal

import LeanInk.Configuration
import LeanInk.Logger

import Lean.Elab.Frontend
import Lean.Elab.Import
import Lean.Elab.Command
import Lean.Parser

import Lean.Util.Trace

open Lean Elab

def LeanInk.Analysis.analyzeInput (file : System.FilePath) : IO (List Tactic) := do
  let fileContents ← IO.FS.readFile file
  let context := Parser.mkInputContext fileContents file.toString
  let (header, state, messages) ← Parser.parseHeader context
  initializeLakeContext lakeFile header
  let options := Options.empty |>.setBool `trace.Elab.info true |>.setBool `tactic.simp.trace true
  let (environment, messages) ← processHeader header options messages context 0
  logInfo s!"Header: {environment.header.mainModule}"
  logInfo s!"Header: {environment.header.moduleNames}"
  if messages.hasErrors then
    for msg in messages.toList do
      if msg.severity == .error then
        let _ ← logError <$> msg.toString
    throw <| IO.userError "Errors during import; aborting"
  let commandState := { Command.mkState environment messages with infoState := { enabled := true } }
  let s ← IO.processCommands context state commandState
  let result ← resolveTacticList s.commandState.infoState.trees.toList
  let messages := s.commandState.messages.msgs.toList.filter (·.endPos.isSome)
  return result