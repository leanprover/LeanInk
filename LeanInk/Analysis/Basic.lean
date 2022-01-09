import LeanInk.Configuration
import LeanInk.FileHelper
import LeanInk.Annotation
import LeanInk.Logger
import LeanInk.CLI

import LeanInk.Analysis.Analysis

import Lean.Util.Path
import Lean.Data.Json
import Lean.Data.Json.Printer

namespace LeanInk.Analysis

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
  logInfo s!"Results written to file: {path}!"

def generateOutput (fragments : Array Annotation.Alectryon.Fragment) : String := s!"{toJson fragments}"

def runAnalysis : AnalysisM UInt32 := do
  let config ← read
  logInfo s!"Starting process with lean file: {config.inputFileName}"
  logInfo "Analyzing..."
  let result ← analyzeInput config
  logInfo "Annotating..."
  let outputFragments ← Annotation.annotateFile result
  logInfo "Outputting..."
  createOutputFile (← IO.currentDir) config.inputFileName (generateOutput outputFragments.toArray)
  return 0

-- EXECUTION
def execAux (args: List ResolvedArgument) (file: String) : IO UInt32 := do
  if not (_validateInputFile file) then do
    Logger.logError s!"Provided file \"{file}\" is not lean file."
  else
    IO.println s!"Starting Analysis for: \"{file}\""
    let config ← _buildConfiguration args file
    return ← (runAnalysis.run config)

def exec (args: List ResolvedArgument) : List String -> IO UInt32
  | [] => do Logger.logError s!"No input files provided"
  | files => do
    -- Span task for every file?
    for file in files do
      if (← execAux args file) != 0 then
        return ← Logger.logError s!"Analysis for \"{file}\" failed!"
    return 0