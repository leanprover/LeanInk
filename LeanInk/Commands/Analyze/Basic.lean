import LeanInk.CLI.GlobalArgument
import LeanInk.CLI.ParsableArgument
import LeanInk.CLI.Logger

import LeanInk.Commands.Analyze.Argument

namespace LeanInk.Commands.Analyze

open LeanInk.CLI
open System

def _debugMessage (file content : String) := s!"
FILE IMPORTED ({file}):
{content}
"

def leanFileExtension := s!"lean"

def isLeanFile (path : FilePath) : Bool := do
  return path.extension == leanFileExtension

def exec (globalArgs: List GlobalArgument) (args: List String) : IO UInt32 := do
  let (arguments, files) : List Argument × List String := parseArgumentList args
  match files with
  | a::as =>
    let path : FilePath := a
    if isLeanFile path then
      let input <- IO.FS.readFile a
      Logger.logInfo (_debugMessage a input)
      return 0
    else
      Logger.logError s!"Provided file \"{a}\" is not lean file."
  | _ => Logger.logError s!"No input files provided"