import LeanInk.Commands.Analyze.Configuration

import Lean.Elab.Frontend
import Lean.Elab.Import
import Lean.Elab.Command
import Lean.Parser

import Lean.Util.Trace

namespace LeanInk.Commands.Analyze

open Lean
open Lean.Elab

def configureCommandState (env : Environment) (msg : MessageLog) : Command.State := do 
  return { Command.mkState env msg with infoState := { enabled := true }}

-- atm we just return a bool, but we should probably return a useful data structure that contains all analysis info
def analyzeInput (config: Configuration) : IO Bool := do
  let context := Parser.mkInputContext config.inputFileContents config.inputFileName
  let (header, state, messages) ← Parser.parseHeader context
  let options := Options.empty.setBool `trace.Elab.info true
  let (environment, messages) ← processHeader header options messages context 0
  let commandState := configureCommandState environment messages
  let s <- IO.processCommands context state commandState
  let trees := s.commandState.infoState.trees

  IO.println s!"INFO! Trees enabled: {s.commandState.infoState.enabled}"

  for cmd in s.commands do
    IO.println s!"{cmd}"

  for tree in trees do
    let format ← InfoTree.format tree

  return true