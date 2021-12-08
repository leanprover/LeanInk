import LeanInk.CLI.Argument
import LeanInk.CLI.Result

namespace LeanInk.CLI

-- COMMANDS
structure Command where
  identifiers : List String
  help : String
  additionalUsageInfo : String := ""
  arguments : List Argument
  run: (List ResolvedArgument) -> (List String) -> IO UInt32

structure ResolvedCommand where
  command: Command
  arguments: List ResolvedArgument

-- VERSION COMMAND
def versionCommand : CLI.Command := {
  identifiers := ["version", "-v"]
  help := "Returns the version info of this instance."
  arguments := []
  run := λ _ _ => return 0
}

-- HELP COMMAND
-- The help command is always available
def helpCommand : Command := {
  identifiers := ["help", "-h"]
  help := "Displays a help page."
  additionalUsageInfo := "<COMMAND>"
  arguments := []
  run := λ _ _ => return 0
}