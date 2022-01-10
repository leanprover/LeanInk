import LeanInk.Configuration
import LeanInk.FileHelper
import LeanInk.Annotation
import LeanInk.Logger
import LeanInk.CLI

import LeanInk.Analysis.Analysis

import Lean.Util.Path

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
    experimentalTypeInfo := containsFlag arguments "--x-enable-type-info"
    experimentalDocString := containsFlag arguments "--x-enable-docStrings"
  }
where
  getLakeFile? (arguments : List ResolvedArgument) : Option FilePath :=
    match environmentValue arguments "--lake" with
    | none => none
    | some string => some (FilePath.mk string)

def runAnalysis : AnalysisM UInt32 := do
  let config ← read
  logInfo s!"Starting process with lean file: {config.inputFileName}"
  logInfo "Analyzing..."
  let result ← analyzeInput config
  logInfo "Annotating..."
  let annotation ← Annotation.annotateFile result
  logInfo "Outputting..."
  return ← Annotation.Alectryon.genOutput annotation

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