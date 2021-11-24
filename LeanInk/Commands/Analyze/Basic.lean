import LeanInk.CLI.GlobalArgument
import LeanInk.CLI.ParsableArgument
import LeanInk.CLI.Logger

import LeanInk.Commands.Analyze.Argument
import LeanInk.Commands.Analyze.LeanContext
import LeanInk.Commands.Analyze.Configuration
import LeanInk.Commands.Analyze.FileHelper
import LeanInk.Commands.Analyze.Analysis
import LeanInk.Commands.Analyze.Annotation

import Lean.Util.Path
import Lean.Data.Json
import Lean.Data.Json.Printer

namespace LeanInk.Commands.Analyze

open LeanInk.CLI
open Lean
open System

private def _validateInputFile (file : FilePath) : Bool := do
  return isLeanFile file

private def _buildConfiguration (arguments: List Argument) (file: FilePath) : IO Configuration := do
  let contents ← IO.FS.readFile file

  return { 
    inputFilePath := file,
    inputFileContents := contents,
    outputType := OutputType.alectryonFragments 
  }

-- OUTPUT
open IO.FS

def createOutputFile (folderPath : FilePath) (fileName : String) (content : String) : IO Unit := do
  let dirEntry : DirEntry := { 
    root := folderPath,
    fileName := fileName ++ ".leanInk"
  }
  let path ← dirEntry.path
  IO.FS.writeFile path content
  IO.println s!"Results written to file: {path}!"
  IO.println content

open LeanInk.Output.AlectryonFragment in
def generateOutput (fragments : Array Fragment) : String := s!"{toJson fragments}"

-- EXECUTION
def exec (globalArgs: List GlobalArgument) (args: List String) : IO UInt32 := do
  let (arguments, files) : List Argument × List String := parseArgumentList args
  match files with
  | a::as =>
    if not (_validateInputFile a) then do
      Logger.logError s!"Provided file \"{a}\" is not lean file."
    else
      Logger.logInfo s!"Starting process with lean file: {a}"
      let config ← _buildConfiguration arguments a

      Logger.logInfo "Loading Lean Context..."
      initializeLeanContext

      Logger.logInfo "Analyzing ..."
      let annotations ← analyzeInput config

      Logger.logInfo "Annotating..."
      let outputFragments ← annotateFile config annotations

      IO.println s!"FragmentSize: {outputFragments.length}"

      Logger.logInfo "Outputting..."
      createOutputFile (← IO.currentDir) config.inputFileName (generateOutput outputFragments.toArray)
      return 0
  | _ => Logger.logError s!"No input files provided"