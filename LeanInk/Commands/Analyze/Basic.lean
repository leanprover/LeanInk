import LeanInk.Commands.Analyze.Configuration
import LeanInk.Commands.Analyze.FileHelper
import LeanInk.Commands.Analyze.Analysis
import LeanInk.Commands.Analyze.Annotation
import LeanInk.Commands.Analyze.Logger

import LeanInk.CLI

import Lean.Util.Path
import Lean.Data.Json
import Lean.Data.Json.Printer

namespace LeanInk.Commands.Analyze

open LeanInk.CLI
open Lean
open System

private def _validateInputFile (file : FilePath) : Bool := isLeanFile file

private def _buildConfiguration (arguments: List ResolvedArgument) (file: FilePath) : IO Configuration := do
  let contents ← IO.FS.readFile file
  return { 
    inputFilePath := file
    inputFileContents := contents
    outputType := OutputType.alectryonFragments
    lakeFile := getLakeFile? arguments
    verbose := containsFlag arguments "--verbose"
    experimentalTokens := containsFlag arguments "--experimental-type-tokens"
  }
where
  getLakeFile? (arguments : List ResolvedArgument) : Option FilePath :=
    match environmentValue arguments "--lake" with
    | none => none
    | some string => some (FilePath.mk string)

-- OUTPUT
open IO.FS
def createOutputFile (folderPath : FilePath) (fileName : String) (content : String) : AnalysisM Unit := do
  let dirEntry : DirEntry := { 
    root := folderPath,
    fileName := fileName ++ ".leanInk"
  }
  let path ← dirEntry.path
  IO.FS.writeFile path content
  Logger.logInfo s!"Results written to file: {path}!"

def generateOutput (fragments : Array Output.Alectryon.Fragment) : String := s!"{toJson fragments}"

def runAnalysis : AnalysisM UInt32 := do
  let config ← read
  Logger.logInfo s!"Starting process with lean file: {config.inputFileName}"
  Logger.logInfo "Analyzing..."
  let result ← analyzeInput config

  Logger.logInfo "Annotating..."
  let outputFragments ← annotateFile result

  Logger.logInfo "Outputting..."
  createOutputFile (← IO.currentDir) config.inputFileName (generateOutput outputFragments.toArray)
  return 0

-- EXECUTION
def exec (args: List ResolvedArgument) (files: List String) : IO UInt32 := do
  match files with
  | a::as =>
    if not (_validateInputFile a) then do
      Logger.logError s!"Provided file \"{a}\" is not lean file."
    else
      let config ← _buildConfiguration args a
      return ← (runAnalysis.run config)
  | _ => Logger.logError s!"No input files provided"