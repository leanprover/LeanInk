import LeanInk.CLI
import LeanInk.Analysis
import LeanInk.Commands

open LeanInk
open LeanInk.CLI
open LeanInk.CLI.Argument
open LeanInk.Commands

def app : AppInfo := {
  name := "LeanInk"
  base := "leanInk"
  version := { major := 1, minor := 0, patch := 0 }
  description := "LeanInk is a code analysis tool for Lean 4 that extracts proof tactic information. It's main goal is to ease the support for Lean 4 in Alectryon."
}

def main : List String → IO UInt32 := runCLI app [analyzeCommand]