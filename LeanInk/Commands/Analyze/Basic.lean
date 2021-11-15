import LeanInk.CLI.GlobalArgument
import LeanInk.CLI.ParsableArgument
import LeanInk.CLI.Logger

import LeanInk.Commands.Analyze.Argument
import LeanInk.Commands.Analyze.LeanContext
import LeanInk.Commands.Analyze.Configuration
import LeanInk.Commands.Analyze.FileHelper
import LeanInk.Commands.Analyze.Analysis

import Lean.Util.Path

namespace LeanInk.Commands.Analyze

open LeanInk.CLI
open Lean
open System

private def _analyze (configuration: Configuration) : IO Unit := do
  let successfull <- analyzeInput configuration
  return

private def _validateInputFile (file : FilePath) : Bool := do
  return isLeanFile file

private def _buildConfiguration (arguments: List Argument) (file: FilePath) : IO Configuration := do
  let contents ← IO.FS.readFile file

  return { 
    inputFilePath := file,
    inputFileContents := contents,
    outputType := OutputType.alectryonFragments 
  }

-- EXECUTION
def exec (globalArgs: List GlobalArgument) (args: List String) : IO UInt32 := do
  let (arguments, files) : List Argument × List String := parseArgumentList args
  match files with
  | a::as =>
    if not (_validateInputFile a) then do
      Logger.logError s!"Provided file \"{a}\" is not lean file."
    else
      let configuration ← _buildConfiguration arguments a
      Logger.logInfo "Loading Lean Context..."
      initializeLeanContext
      Logger.logInfo "Analyzing..."
      _analyze configuration
      return 0
  | _ => Logger.logError s!"No input files provided"