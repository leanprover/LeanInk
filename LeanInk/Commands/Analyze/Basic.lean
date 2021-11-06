import LeanInk.CLI.GlobalArgument
import LeanInk.CLI.ParsableArgument
import LeanInk.CLI.Logger

import LeanInk.Commands.Analyze.Argument

namespace LeanInk.Commands.Analyze

open LeanInk.CLI

def _debugMessage (file content : String) := s!"
FILE IMPORTED ({file}):
{content}
"

def exec (globalArgs: List GlobalArgument) (args: List String) : IO UInt32 := do
  let (arguments, files) : List Argument × List String := parseArgumentList args
  match files with
  | a::as =>
    let input <- IO.FS.readFile a
    Logger.logInfo (_debugMessage a input)
    return 0
  | _ => Logger.logError s!"No input files provided"