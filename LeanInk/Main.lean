import LeanInk.CLI
import LeanInk.Commands

open LeanInk
open LeanInk.CLI
open LeanInk.CLI.Argument

def app : AppInfo := {
  name := "LeanInk"
  base := "leanInk"
  version := { major := 0, minor := 1, patch := 0, suffix := "" }
  description := "LeanInk is a code analysis tool for Lean 4 that extracts proof tactic information. It's main goal is to ease the support for Lean 4 in Alectryon."
}

def analyzeCommand : Command := {
  identifiers := ["analyze", "a"]
  help := ""
  arguments := [
    environment {
      identifiers := ["--lake"]
      help := "Specify path to lakefile.lean, so dependencies can be resolved during analysis."
    }
  ]
  run := Commands.Analyze.exec
}

def leanVersionCommand : Command := {
  identifiers := ["leanVersion", "-lV"],
  help := "",
  arguments := [],
  run := λ _ _ => Commands.Version.printLeanVersion
}

def main : List String -> IO UInt32 := runCLI app [analyzeCommand, leanVersionCommand]