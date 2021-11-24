import LeanInk.Commands.Analyze.Configuration
import LeanInk.Commands.Analyze.InfoTreeUtil

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

-- INFO TREE analysis
def analyzeInput (config: Configuration) : IO (List TacticFragment) := do
  let context := Parser.mkInputContext config.inputFileContents config.inputFileName
  let (header, state, messages) ← Parser.parseHeader context
  let options := Options.empty.setBool `trace.Elab.info true
  let (environment, messages) ← processHeader header options messages context 0
  let commandState := configureCommandState environment messages
  let s ← IO.processCommands context state commandState
  let trees := s.commandState.infoState.trees.toList

  IO.println s!"INFO! Trees enabled: {s.commandState.infoState.enabled}"
  IO.println s!"INFO! Gathered trees: {s.commandState.infoState.trees.size}"

  let tacticFragments := trees.map _resolveTacticList

  for fragmentList in tacticFragments do
    IO.println s!"___NEW FRAGMENT_LIST___"
    for fragment in fragmentList do
      let format ← fragment.toFormat
      IO.println s!"{format}"

  return tacticFragments.foldl mergeSortFragments []