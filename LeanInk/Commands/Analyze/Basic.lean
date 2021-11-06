import LeanInk.CLI.GlobalArgument
import LeanInk.CLI.ParsableArgument
import LeanInk.CLI.Logger

import LeanInk.Commands.Analyze.Argument

namespace LeanInk.Commands.Analyze

open LeanInk.CLI

def exec (globalArgs: List GlobalArgument) (args: List String) : IO UInt32 := do
  let (arguments, _) : List Argument × List String := parseArgumentList args
  Logger.logInfo s!"Executing analysis for {args}"
  return 0