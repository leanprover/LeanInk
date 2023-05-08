import LeanInk.Configuration
import LeanInk.Annotation.DataTypes
import LeanInk.Annotation.Util
import LeanInk.Annotation.Alectryon
import LeanInk.Logger

import LeanInk.Analysis.Analysis

import Lean.Util.Path

namespace LeanInk.Analysis

open LeanInk.Annotation Lean System

def annotateFile (analysis : List Tactic) : IO (List Annotation) := matchCompounds <| toFragmentIntervals analysis

def runAnalysis (file : System.FilePath) (output : Output) : IO UInt32 := do
  -- logInfo s!"Starting process with lean file: {config.inputFileName}"
  logInfo "Analyzing..."
  let result ← analyzeInput file
  logInfo "Annotating..."
  let annotation ← annotateFile result
  logInfo "Outputting..."
  output.genOutput annotation

-- EXECUTION

def execAux (file : String) : IO UInt32 := do
  if ! (file : System.FilePath).extension == "lean" then do
    Logger.logError s!"Provided file \"{file}\" is not lean file."
  else
    IO.println s!"Starting Analysis for: \"{file}\""
    let contents ← IO.FS.readFile file
    runAnalysis file {
    name := "Alectryon"
    genOutput := Alectryon.genOutput file contents
  }

/-
`enableInitializersExecution` is usually only run from the C part of the
frontend and needs to be used with care but it is required in order
to work with custom user extensions correctly.
-/
@[implemented_by enableInitializersExecution]
private def enableInitializersExecutionWrapper : IO Unit := pure ()

def exec : List String -> IO UInt32
  | [] => do Logger.logError s!"No input files provided"
  | files => do
    enableInitializersExecutionWrapper
    -- Span task for every file?
    for file in files do
      if (← execAux file) != 0 then
        return ← Logger.logError s!"Analysis for \"{file}\" failed!"
    return 0
