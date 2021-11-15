import LeanInk.Commands.Analyze.Configuration

import Lean.Elab.Frontend
import Lean.Elab.Import
import Lean.Elab.Command
import Lean.Parser

namespace LeanInk.Commands.Analyze

open Lean

-- atm we just return a bool, but we should probably return a useful data structure that contains all analysis info
def analyzeInput (config: Configuration) : IO Bool := do
  let context := Parser.mkInputContext config.inputFileContents config.inputFileName
  let (header, state, messages) ← Parser.parseHeader context
  let (environment, messages) ← Elab.processHeader header Options.empty messages context 0
  let commandState := Elab.Command.mkState environment messages Options.empty
  let s <- Elab.IO.processCommands context state commandState

  IO.println s!"{s.commands}"

  return true