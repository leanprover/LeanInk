import LeanInk.CLI.Argument
import LeanInk.CLI.Command
import LeanInk.CLI.Result
import LeanInk.CLI.Help

namespace LeanInk.CLI

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

-- HELPER METHODS
def argument (args : List ResolvedArgument) (identifier : String) : Option ResolvedArgument :=
  List.find? (λ x => x.identifiers.elem identifier) args

def environmentValue (args : List ResolvedArgument) (identifier : String) : Option String :=
  match argument args identifier with
  | ResolvedArgument.env _ a => a
  | _ => none

def containsFlag (args : List ResolvedArgument) (identifier : String) : Bool :=
  match argument args identifier with
  | ResolvedArgument.flag _ => true
  | _ => false

-- METHODS
open Result in
/-- Resolves a command list given the available commands.

Errors:
- throws CLIError.noCommandsProvided if available commands is empty
- throws CLIError.noArgumentsProvided if the argument list is empty
- throws CLIError.unknownCommand if the first argument cannot be resolved to any of the available commands.
-/
private def _resolveCommandList (available: List Command) (args: List String) : Result CLIError (Command × List String) :=
  if available.isEmpty then 
    failure CLIError.noCommandsProvided -- If no root commands are available we throw an error
  else
    match args with
    | [] => failure CLIError.noArgumentsProvided -- If no arguments were provided, we cannot resolve anything
    | a::as =>
      match List.find? (λ x => x.identifiers.elem a) available with
      | none => failure (CLIError.unknownCommand a)
      | some c => success (c, as)

private partial def resolveArgumentList (available: List Argument) (args: List String) : List ResolvedArgument × List String :=
  if available.isEmpty then
    ([], args)
  else
    match args with
    | [] => ([], args) -- No arguments left
    | a::as =>
      let argument := List.find? (λ x => x.identifiers.elem a) available
      match argument with
      | none => 
        let (res, unres) := resolveArgumentList available as
        (res, a::unres)
      | some argument =>
        match argument with
        | Argument.flag i =>
          let resolvedArg := ResolvedArgument.flag i
          let (otherArgs, unresolved) := resolveArgumentList (available.erase argument) as
          (resolvedArg::otherArgs, unresolved)
        | Argument.environment i =>
          match as with 
          | [] => 
            let (res, unres) := resolveArgumentList available as
            (res, a::unres)
          | b::bs => 
            let resolvedArg := ResolvedArgument.env i b
            let (otherArgs, unresolved) := resolveArgumentList (available.erase argument) bs
            (resolvedArg::otherArgs, unresolved)
        

def runHelp (app: AppInfo)  (available: List Command) (arguments : List String) : IO UInt32 := do
  match _resolveCommandList available arguments with
  | Result.failure error => do
    IO.println (generateDefaultHelp app available)
    return 1
  | Result.success (command, unresolvedArgs) => do
      IO.println (generateCommandHelp app command)
    return 0

-- ENTRY
def runCLI (app: AppInfo) (commands: List Command) (args: List String) : IO UInt32 := do
  let commands := helpCommand::versionCommand::commands
  match _resolveCommandList commands args with -- We automatically add the help and version command internally for command resolution.
  | Result.failure error => do
    IO.println (generateDefaultHelp app commands)
    return 1
  | Result.success result => do
    let (command, args) := result
    if command.identifiers == helpCommand.identifiers then
      return (← runHelp app commands args) -- We escape the actual command execution and handle the help command ourselves.
    else if command.identifiers == versionCommand.identifiers then
      IO.println app.versionString
      return 0
    else
      let (resArgs, unresArgs) := resolveArgumentList command.arguments args
      return (← command.run resArgs unresArgs)