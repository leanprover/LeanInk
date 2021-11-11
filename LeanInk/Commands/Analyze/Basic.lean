import LeanInk.CLI.GlobalArgument
import LeanInk.CLI.ParsableArgument
import LeanInk.CLI.Logger

import LeanInk.Commands.Analyze.Argument

import Lean.Elab.Frontend
import Lean.Util.Path

import Lake

namespace LeanInk.Commands.Analyze

open LeanInk.CLI
open Lean
open System

def _debugMessage (file content : String) := s!" {file}:
{content}
"

def leanFileExtension := s!"lean"

def isLeanFile (path : FilePath) : Bool := do
  return path.extension == leanFileExtension

def initializeLeanContext : IO UInt32 := do
  let lean <- Lake.findLeanInstall?
  let lake <- Lake.findLakeInstall?
  let res <- Lake.setupLeanSearchPath lean lake
  return 0

def loadInput (content : String) (name : FilePath) : IO UInt32 := do
  Logger.logInfo s!"Running frontend:"
  let i <- initializeLeanContext 
  let (env, ok) <- Elab.runFrontend content Options.empty name.toString `Init
  if ok then return 0 else return 1

def exec (globalArgs: List GlobalArgument) (args: List String) : IO UInt32 := do
  let (arguments, files) : List Argument × List String := parseArgumentList args
  match files with
  | a::as =>
    let path : FilePath := a
    if isLeanFile path then
      let input <- IO.FS.readFile a
      Logger.logInfo (_debugMessage a input)
      loadInput input path
    else
      Logger.logError s!"Provided file \"{a}\" is not lean file."
  | _ => Logger.logError s!"No input files provided"