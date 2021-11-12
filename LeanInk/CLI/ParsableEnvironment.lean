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
structure CommandConfig where
  identifiers : List String
  help: String
  arguments : List Argument
  run: (List ResolvedArgument) -> IO UInt32

inductive Command where
  | command (self: CommandConfig) (subcommands: List Command)

namespace Command

def getConfig : Command -> CommandConfig
  | command self _ => self

def getSubcommands : Command -> List Command
  | command _ subcommands => subcommands

end Command

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
Resolves a command list given the initial root commands.
Every following command is a subcommand of the previous command.
Is the given list of root commands, then this method will always return none.

Example:

> help command
returns `[help, execute]`. Every command is a subcommand of the `help` command.

> command subcommand subsubcommand
return `[command, subcommand, subsubcommand]` where `command` is a root command, 
`subcommand` a subcommand of `command` and `subsubcommand` a subcommand of `subcommand`

> command unknown
returns: [command], because `command` is known, but `unknown` is not a valid subcommand and therefore considered as an argument.

> unknown command
return: none, because `unknown` is not a known root command and therefore `command` will not be evaluated.

As you can see, command resolution uses a strict rule of order. 
-/
private def _resolveCommandList (available: List Command) (args: List String) : Result CLIError (List Command × List String) := do
  if available.isEmpty then 
    return Result.failure CLIError.noCommandsProvided -- If no root commands are available we return none
  match args with
  | [] => return Result.failure CLIError.noArgumentsProvided -- If no arguments were provided, we cannot resolve anything
  | a::as =>
    let command := List.find? (fun x => x.getConfig.identifiers.elem a) available
    match command with
    | none => return Result.failure (CLIError.unknownCommand a)
    | some c =>
      let subcommands := c.getSubcommands
      if subcommands.isEmpty then 
        return Result.success (c::[], as) 
      match _resolveCommandList subcommands as with
      | Result.failure err => return Result.failure err
      | Result.success (subcommands, unresolved) => return Result.success (c::subcommands, unresolved)

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

private def resolveCommand (available: List Command) (args: List String) : IO (Option ResolvedCommand) := do
  let commandResolution := _resolveCommandList available args
  IO.println s!"{commandResolution}"
  return none

-- ENTRY

def runCLI (commands: List Command) (args: List String) : IO UInt32 := do
  let command <- resolveCommand commands args
  match command with
  | none => return 1
  | some _ => return 0
