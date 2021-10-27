import LeanInk.GlobalArgument
import LeanInk.Command

namespace LeanInk
namespace CLI

-- error prints an Error message to the terminal and returns with the specified error code.
def error (message: String) (errorCode: UInt32 := 1) : IO UInt32 := do
  IO.println s!"ERROR({errorCode}): {message}"
  return errorCode

-- runCLI is the main entry point for the CLI argument parsing and command execution.
def runCLI (args: List String) : IO UInt32 := do
  match args with
  | [] => error s!"No command provided!"
  | a::as => do
    let (globalArgs, args) : List GlobalArgument × List String := parseArgumentList as
    let command : Option Command := parseArgument a
    match command with
    | some command => Command.execute command globalArgs args
    | none => error s!"Unknown command: '{a}'"
