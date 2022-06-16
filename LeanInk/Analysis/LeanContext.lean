import Lean.Util.Path
import Lean.Parser.Module
import Lean.Elab
import Lean.Util.Paths

import LeanInk.Logger
import LeanInk.Configuration

namespace LeanInk.Analysis

open System
open Lean

-- LEAN
def initializeLeanContext : IO Unit := do
  let leanPath ← Lean.findSysroot
  Lean.initSearchPath leanPath

-- LAKE
def lakefileName := "lakefile.lean"
def lakeEnvName := "LAKE"
def lakeCmdName := "lake"
def lakePrintPathsCmd := "print-paths"

def getLakePath : IO String := do
  match (← IO.getEnv lakeEnvName) with
  | some path => return path
  | none => return lakeCmdName

open IO
def initializeLakeContext (lakeFile : FilePath) (header : Syntax) : AnalysisM Unit := do
  if !(← lakeFile.pathExists) then
    throw <| IO.userError s!"lakefile does not exist: {lakeFile}"
  else if lakeFile.fileName != some "lakefile.lean" then
    match lakeFile.fileName with
    | none => throw <| IO.userError s!"lakefile is not a valid file!"
    | some fileName => throw <| IO.userError s!"lakefile [{fileName}] not called: lakefile.lean"
  else
    logInfo s!"Loading Lake Context with lakefile ({lakeFile})..."
    let imports := Lean.Elab.headerToImports header
    let arguments := #[lakePrintPathsCmd] ++ imports.map (toString ·.module)
    let lakeProcess ← Process.spawn {
      stdin := Process.Stdio.null
      stdout := Process.Stdio.piped
      stderr := Process.Stdio.piped
      cmd := ← getLakePath
      args := arguments
    }
    let stdout := String.trim (← lakeProcess.stdout.readToEnd)
    match (← lakeProcess.wait) with
    | 0 => do
      let stdout := stdout.split (· == '\n') |>.getLast!
      match Json.parse stdout with
      | Except.error _ => throw <| IO.userError s!"Failed to parse lake output: {stdout}"
      | Except.ok val => match fromJson? val with
        | Except.error _ => throw <| IO.userError s!"Failed to decode lake output: {stdout}"
        | Except.ok paths => do
          let paths : LeanPaths := paths 
          initializeLeanContext
          initSearchPath (← findSysroot) paths.oleanPath
          logInfo s!"{paths.oleanPath}"
          logInfo s!"Successfully loaded lake search paths"
    | 2 => logInfo s!"No search paths required!"
    | _ => throw <| IO.userError s!"Using lake failed! Make sure that lake is installed!\n{← lakeProcess.stderr.readToEnd}"

def configureLake : AnalysisM Unit := do
  let lakeProcess ← Process.spawn {
      stdin := Process.Stdio.null
      stdout := Process.Stdio.piped
      stderr := Process.Stdio.piped
      cmd := ← getLakePath
      args := #["configure"]
    }
    let stdout := String.trim (← lakeProcess.stdout.readToEnd)
    match (← lakeProcess.wait) with
    | _ => logInfo s!"Lake configured dependencies!"

def initializeSearchPaths (header : Syntax) (config : Configuration) : AnalysisM Unit := do
  match config.lakeFile with
  | some lakeFile => do 
    configureLake
    initializeLakeContext lakeFile header
  | none => initializeLeanContext
