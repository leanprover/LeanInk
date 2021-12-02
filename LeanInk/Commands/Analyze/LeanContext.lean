import Lean.Util.Path

import LeanInk.CLI

namespace LeanInk.Commands.Analyze

open System
open LeanInk.CLI

def initializeLeanContext : IO Unit := do
  let leanPath ← Lean.findSysroot?
  Lean.initSearchPath leanPath

def initializeLakeContext (lakeFile : FilePath) : IO Unit := do
  IO.println "TODO: load lakefile"

def initializeSearchPaths : Option String -> IO Unit
  | some lakeFile => do
    Logger.logInfo s!"Loading Lean Context with lakefile ({lakeFile})..."
    initializeLeanContext
    initializeLakeContext lakeFile
  | none => do
    Logger.logInfo "Loading Lean Context..."
    initializeLeanContext
