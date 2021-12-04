import Lean.Util.Path
import Lean.Parser.Module
import Lean.Elab
import Lean.Util.Paths

import LeanInk.CLI
import LeanInk.Commands.Analyze.Configuration

namespace LeanInk.Commands.Analyze

open System
open Lean
open LeanInk.CLI

-- LEAN
def initializeLeanContext : IO Unit := do
  let leanPath ← Lean.findSysroot?
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
def initializeLakeContext (lakeFile : FilePath) (header : Syntax) : IO Unit := do
  if !(← lakeFile.pathExists) then
    Logger.logInfo s!"lakefile does not exist: {lakeFile}"
  else if lakeFile.fileName != some "lakefile.lean" then
    match lakeFile.fileName with
    | none => Logger.logInfo s!"lakefile is not a valid file!"
    | some fileName => Logger.logInfo s!"lakefile [{fileName}] not called: lakefile.lean"
  else
    Logger.logInfo s!"Loading Lake Context with lakefile ({lakeFile})..."
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
      | Except.error _ => Logger.logInfo s!"Failed to parse lake output: {stdout}"
      | Except.ok val => match fromJson? val with
        | Except.error _ => Logger.logInfo s!"Failed to decode lake output: {stdout}"
        | Except.ok paths => do
          let paths : LeanPaths := paths 
          initializeLeanContext
          initSearchPath (← findSysroot?) paths.oleanPath
          Logger.logInfo s!"Successfully loaded lake search paths"
    | 2 => Logger.logInfo s!"No search paths required!"
    | _ => Logger.logInfo s!"Using lake failed! Make sure that lake is installed!"

def buildLakeDep : IO Unit := do
  let lakeProcess ← Process.spawn {
      stdin := Process.Stdio.null
      stdout := Process.Stdio.piped
      stderr := Process.Stdio.piped
      cmd := ← getLakePath
      args := #["build"]
    }
    let stdout := String.trim (← lakeProcess.stdout.readToEnd)
    match (← lakeProcess.wait) with
    | _ => return 

def initializeSearchPaths (header : Syntax) (config : Configuration) : IO Unit := do
  match config.lakeFile with
  | some lakeFile => do 
    initializeLakeContext lakeFile header
    buildLakeDep
  | none => initializeLeanContext
  