import LeanInk.CLI
import LeanInk.Analysis
import LeanInk.Version

open LeanInk
open LeanInk.CLI
open LeanInk.CLI.Argument

def app : AppInfo := {
  name := "LeanInk"
  base := "leanInk"
  version := { major := 1, minor := 0, patch := 0 }
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
      identifiers := ["--x-enable-type-info"]
      help := "Enables output of experimental type info support for Alectryon. Alectryon will show a hover popup with type information for certain tokens."
    },
    flag {
      identifiers := ["--x-enable-docStrings"]
      help := "Enables output of experimental docStrings support for Alectryon. Alectryon will show a hover popup with the docString for tactics and terms if available."
    },
    flag {
      identifiers := ["--x-enable-semantic-token"],
      help := "Enables output of experimental semantic token support for Alectryon. Alectryon uses this information to implement semantic syntax highlighting."
    }
  ]
  run := Analysis.exec
}

def leanVersionCommand : Command := {
  identifiers := ["leanVersion", "lV"],
  help := "Returns the lean version supported by this leanInk instance.",
  arguments := [],
  run := λ _ _ => Version.printLeanVersion
}

def main : List String -> IO UInt32 := runCLI app [analyzeCommand, leanVersionCommand]