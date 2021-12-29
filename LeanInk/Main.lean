import LeanInk.CLI
import LeanInk.Commands

open LeanInk
open LeanInk.CLI
open LeanInk.CLI.Argument

def app : AppInfo := {
  name := "LeanInk"
  base := "leanInk"
  version := { major := 1, minor := 0, patch := 0, suffix := "-pre" }
  description := "LeanInk is a code analysis tool for Lean 4 that extracts proof tactic information. It's main goal is to ease the support for Lean 4 in Alectryon."
}

def analyzeCommand : Command := {
  identifiers := ["analyze", "a"]
  help := "Analyzes the given input file and outputs the results in Alectryons fragment json format."
  additionalUsageInfo := "<INPUT_FILE>"
  arguments := [
    environment {
      identifiers := ["--lake"]
      help := "Specify path to lakefile.lean, so dependencies can be resolved during analysis. Default: none"
    },
    flag {
      identifiers := ["--verbose"]
      help := "Enables verbose output."
    },
    flag {
      identifiers := ["--experimental-type-tokens"]
      help := "Enables output of experimental tokens in Alectryon with additonal type information"
    }
  ]
  run := Commands.Analyze.exec
}

def leanVersionCommand : Command := {
  identifiers := ["leanVersion", "lV"],
  help := "Returns the lean version supported by this leanInk instance.",
  arguments := [],
  run := λ _ _ => Commands.Version.printLeanVersion
}

def main : List String -> IO UInt32 := runCLI app [analyzeCommand, leanVersionCommand]