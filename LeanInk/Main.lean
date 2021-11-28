import LeanInk.CLI
import LeanInk.Commands

open LeanInk

def analyzeCommand : CLI.Command := {
  identifiers := ["analyze", "a"]
  help := ""
  arguments := []
  run := Commands.Analyze.exec
}

def versionCommand : CLI.Command := {
  identifiers := ["version", "-v"]
  help := ""
  arguments := []
  run := λ _ _ => Commands.Version.printVersion
}

def leanVersionCommand : CLI.Command := {
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
def main := CLI.runCLI rootCommands