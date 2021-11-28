import LeanInk.CLI
import LeanInk.Commands

open LeanInk

def app : CLI.AppInfo := {
  name := "LeanInk"
  version := { major := 0, minor := 1, patch := 0, suffix := "" }
  description := "LeanInk is a code analysis tool for Lean 4 that extracts proof tactic information. It's main goal is to ease the support for Lean 4 in Alectryon."
}

def analyzeCommand : CLI.Command := {
  identifiers := ["analyze", "a"]
  help := ""
  arguments := []
  run := Commands.Analyze.exec
}

def leanVersionCommand : CLI.Command := {
  identifiers := ["leanVersion", "-lV"],
  help := "",
  arguments := [],
  run := λ _ _ => Commands.Version.printLeanVersion
}

def main : List String -> IO UInt32 := CLI.runCLI app [analyzeCommand, leanVersionCommand]