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

def generateFragments (trees: List InfoTree) : IO (List Fragment) := do
  match trees with
  | [] => []
  | tree::trees => do
    let format ← InfoTree.format tree
    let fragment := Fragment.text { contents := s!"{format}" }
    let rest ← generateFragments trees
    return fragment::rest

def analyzeInfoTree (config: Configuration) (tree: InfoTree) : Array Fragment := do
  match tree with
  | InfoTree.context info tree => #[]
  | InfoTree.node info children => #[]
  | InfoTree.ofJson json => #[]
  | InfoTree.hole metavar => #[]

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
  IO.println s!"INFO! Gathered commands: {s.commands.size}"
  IO.println s!"INFO! Gathered trees: {s.commandState.infoState.trees.size}"

  let fragments ← generateFragments trees.toList
  return fragments.toArray