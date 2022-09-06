import LeanInk.CLI.Argument
import LeanInk.CLI.Result
import LeanInk.CLI.App

namespace LeanInk.CLI

structure AppContext where
  app : AppInfo

abbrev CliContextM := ReaderT AppContext IO UInt32

-- COMMAND
structure Command where
  identifiers : List String
  help : String
  additionalUsageInfo : String := ""
  arguments : List Argument
  run: (List ResolvedArgument) → (List String) → CliContextM

structure ResolvedCommand where
  command: Command
  arguments: List ResolvedArgument

-- VERSION COMMAND
def printVersion : CliContextM := do
  let context ← read
  IO.println context.app.versionString
  return 0

def versionCommand : Command := {
  identifiers := ["version", "-v"]
  help := "Returns the version info of this instance."
  arguments := []
  run := λ _ _ => printVersion
}

-- LEAN VERSION COMMAND
def printLeanVersion : IO UInt32 := do
  IO.println Lean.versionString
  return 0

def leanVersionCommand : Command := {
  identifiers := ["leanVersion", "lV"],
  help := "Returns the lean version supported by this leanInk instance.",
  arguments := [],
  run := λ _ _ => printLeanVersion
}


-- HELP COMMAND
def helpCommand : Command := {
  identifiers := ["help", "-h"]
  help := "Displays a help page."
  additionalUsageInfo := "<COMMAND>"
  arguments := []
  run := λ _ _ => return 0 -- implemented by runCLI
}
