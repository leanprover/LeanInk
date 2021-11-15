import LeanInk.CLI.GlobalArgument
import LeanInk.CLI.ParsableArgument
import LeanInk.CLI.Logger

import LeanInk.Commands.Analyze.Argument
import LeanInk.Commands.Analyze.LeanContext
import LeanInk.Commands.Analyze.Configuration
import LeanInk.Commands.Analyze.FileHelper

import Lean.Elab.Frontend
import Lean.Util.Path

namespace LeanInk.Commands.Analyze

open LeanInk.CLI
open Lean
open System

def loadInput (file : FilePath) : IO Unit := do
  let content <- IO.FS.readFile file
  let i <- initializeLeanContext 
  let (env, ok) <- Elab.runFrontend content Options.empty file.toString `Init
  return

def _validateInputFile (file : FilePath) : Bool := do
  return isLeanFile file

def buildConfiguration (arguments: List Argument) (file: FilePath) : Configuration := do
  return { 
    inputFile := file,
    outputType := OutputType.alectryonFragments 
  }

def exec (globalArgs: List GlobalArgument) (args: List String) : IO UInt32 := do
  let (arguments, files) : List Argument × List String := parseArgumentList args
  match files with
  | a::as =>
    if not (_validateInputFile a) then do
      Logger.logError s!"Provided file \"{a}\" is not lean file."
    else
      let configuration := buildConfiguration arguments a
      Logger.logInfo "Analyzing..."
      loadInput configuration.inputFile
      return 0
  | _ => Logger.logError s!"No input files provided"