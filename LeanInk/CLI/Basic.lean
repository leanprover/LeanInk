import LeanInk.CLI.ParsableEnvironment
import LeanInk.Commands

namespace LeanInk.CLI

open NEW_CLI

def analyzeCommand : Command := {
  identifiers := ["analyze", "a"]
  help := ""
  arguments := []
  run := λ _ args => do
    IO.println s!"EXECUTING ANALYSIS"
    let a := (<- Commands.Analyze.exec [] args)
    return 0
}

def versionCommand : Command := {
  identifiers := ["version", "-v"]
  help := ""
  arguments := []
  run := λ _ _ => Commands.Version.printVersion
}

def leanVersionCommand : Command := {
  identifiers := ["leanVersion", "-lV"]
  help := ""
  arguments := []
  run := λ _ _ => Commands.Version.printLeanVersion
}

def rootCommands := [
  analyzeCommand, 
  versionCommand, 
  leanVersionCommand
]

-- runCLI is the main entry point for the CLI argument parsing and command execution.
def runCLI (args: List String) : IO UInt32 := NEW_CLI.runCLI rootCommands args
