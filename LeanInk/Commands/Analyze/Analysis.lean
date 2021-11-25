import LeanInk.Commands.Analyze.Configuration
import LeanInk.Commands.Analyze.InfoTreeUtil

import Lean.Elab.Frontend
import Lean.Elab.Import
import Lean.Elab.Command
import Lean.Parser

import Lean.Util.Trace

namespace LeanInk.Commands.Analyze

open Lean
open Lean.Elab

structure AnalysisResult where
  tactics : List TacticFragment
  messages : List MessageFragment

def configureCommandState (env : Environment) (msg : MessageLog) : Command.State := do 
  return { Command.mkState env msg with infoState := { enabled := true }}

def analyzeInput (config: Configuration) : IO AnalysisResult := do
  let context := Parser.mkInputContext config.inputFileContents config.inputFileName
  let (header, state, messages) ← Parser.parseHeader context
  let options := Options.empty.setBool `trace.Elab.info true
  let (environment, messages) ← processHeader header options messages context 0
  let commandState := configureCommandState environment messages
  let s ← IO.processCommands context state commandState
  let trees := s.commandState.infoState.trees.toList
  let messages := s.commandState.messages.msgs.toList.map (λ m => return MessageFragment.mkFragment context.fileMap m)

  for msg in s.commandState.messages.toList do
    IO.print (← msg.toString (includeEndPos := getPrintMessageEndPos options))

  return { tactics := resolveTacticList trees, messages := messages }