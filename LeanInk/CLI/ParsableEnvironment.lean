namespace LeanInk.NEW_CLI

-- ARGUMENTS + ENVIRONMENT VARIABLES
structure Argument where
  identifiers : List String
  help: String
  deriving BEq

structure Environment where
  identifiers : List String
  help: String
  deriving BEq

inductive ResolvedArgument where
  | environment (self: Environment) (val: String)
  | argument (self: Argument)

-- COMMANDS
structure Command where
  identifiers : List String
  help: String
  arguments : List Argument
  run: (List ResolvedArgument) -> IO UInt32

structure ResolvedCommand where
  command: Command
  arguments: List ResolvedArgument

-- Result
inductive Result (error : Type) (result: Type) where
  | failure (err: error)
  | success (res: result)

instance [ToString e] : ToString (Result e r) where
  toString
    | Result.failure error => s!"ERROR: {error}"
    | Result.success result => "SUCCESS!"

inductive CLIError where
  | unknownCommand (arg: String): CLIError
  | noArgumentsProvided : CLIError
  | noCommandsProvided : CLIError

namespace CLIError

instance : ToString CLIError where
  toString
    | unknownCommand c => s!"Unknown command: {c}"
    | noArgumentsProvided => s!"Please choose a command for execution!"
    | noCommandsProvided => s!"Implementation error: No root commands available!"

end CLIError

-- METHODS
/-
Resolves a command list given the available commands.

Errors:
- throws CLIError.noCommandsProvided if available commands is empty
- throws CLIError.noArgumentsProvided if the argument list is empty
- throws CLIError.unknownCommand if the first argument cannot be resolved to any of the available commands.
-/
open Result in
private def _resolveCommandList (available: List Command) (args: List String) : Result CLIError (Command × List String) := do
  if available.isEmpty then 
    return failure CLIError.noCommandsProvided -- If no root commands are available we throw an error
  match args with
  | [] => return failure CLIError.noArgumentsProvided -- If no arguments were provided, we cannot resolve anything
  | a::as =>
    match List.find? (fun x => x.identifiers.elem a) available with
    | none => return failure (CLIError.unknownCommand a)
    | some c => return success (c, as)

/-
private def resolveArgumentList (available: List Argument) (args: List String) : List ResolvedArgument × List String := do
  if available.isEmpty then
    return ([], args)
  else
    match args with
    | [] => return ([], []) -- No arguments left
    | a::[] => return ([], []) -- Single argument left
    | a::b::as => -- Maybe environment left
      let argument := List.find? (fun x => x.identifiers.elem a) available
      match argument with
      | none => return resolveArgumentList available as
      | some c =>
        let resolvedArg := ResolvedArgument.mk 
        let (otherArgs, unresolved) := resolveArgumentList (available.erase c) as
        return (c::otherArgs, unresolved)

private def resolveArgumentList : List String -> List ResolvedArgument × List String
  | [] => ([], [])
  | a::as => do
    let (globalArgs, args) := resolveArgumentList as
    let argument := 
    match parseArgument a with
    | some a => (a::globalArgs, args)
    | none => (globalArgs, a::args)

private def resolveArguments (command: Command) (args: List String) : ResolvedCommandTree := do
  match args with
  | [] =>  return ResolvedCommandTree.command (ResolvedCommand.mk command []) none
  | a::as => 
    let arg := command.getConfig.arguments.elem 
-/

-- HELP COMMAND
-- The help command is always available
def helpCommand : Command := {
  identifiers := ["help", "-h"]
  help := ""
  arguments := []
  run := (fun _ => return 0)
}

def runHelp (available: List Command) (arguments : List String) : IO UInt32 := do
  match _resolveCommandList available arguments with
  | Result.failure error => do
    IO.println s!"{error}"
    return 1
  | Result.success (command, unresolvedArgs) => do
    IO.println "PRINTING HELP!"
    return 0

private def resolveCommand (available: List Command) (args: List String) : IO (Option ResolvedCommand) := do
  match _resolveCommandList available args with
  | Result.failure error => do
    IO.println s!"{error}"
    return none
  | Result.success (command, unresolvedArgs) => do
    IO.println "SUCCESS!"
    return some { command := command, arguments := [] }

-- ENTRY
def runCLI (commands: List Command) (args: List String) : IO UInt32 := do
  match (← resolveCommand (helpCommand::commands) args) with -- We automatically add the help command internally for command resolution.
  | none => return 1
  | some result => do 
    if result.command.identifiers == helpCommand.identifiers then
      return (← runHelp commands args) -- We escape the actual command execution and handle the help command ourselves.
    else 
      return (← result.command.run result.arguments)
