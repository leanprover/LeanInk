import LeanInk.CLI.ParsableEnvironment

namespace LeanInk.CLI

open NEW_CLI

def helpCommandConfig : CommandConfig := {
  identifiers := ["help", "-h"]
  help := ""
  arguments := []
  run := (fun _ => return 0)
}

def helpCommand := Command.command helpCommandConfig []

-- runCLI is the main entry point for the CLI argument parsing and command execution.
def runCLI (args: List String) : IO UInt32 := do
  /-
  match args with
  | [] => Logger.logError s!"No command provided!"
  | a::as => do
    let (globalArgs, args) : List GlobalArgument × List String := parseArgumentList as
    let command : Option Command := parseArgument a
    match command with
    | some command => Command.execute command globalArgs args
    | none => Logger.logError s!"Unknown command: '{a}'"-/
  let commands : List Command := [helpCommand]
  let val <- NEW_CLI.runCLI commands args
  return 0
