import LeanInk.CLI.Argument
import LeanInk.CLI.Result

namespace LeanInk.CLI

-- COMMANDS
structure Command where
  identifiers : List String
  help : String
  arguments : List Argument
  run: (List ResolvedArgument) -> (List String) -> IO UInt32

structure ResolvedCommand where
  command: Command
  arguments: List ResolvedArgument

-- VERSION COMMAND
def versionCommand : CLI.Command := {
  identifiers := ["version", "-v"]
  help := ""
  arguments := []
  run := λ _ _ => return 0
}

-- HELP COMMAND
-- The help command is always available
def helpCommand : Command := {
  identifiers := ["help", "-h"]
  help := ""
  arguments := []
  run := λ _ _ => return 0
}