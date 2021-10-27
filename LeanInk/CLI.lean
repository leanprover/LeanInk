import LeanInk.Version

namespace LeanInk
namespace CLI

-- The ParsableArgument class describes objects with traits that allow it to be parsed
-- from the List of String arguments in the CLI.
class ParsableArgument (Self: Type) where
  -- toStrings defines a mapping for each constructor of the type to a list of string arguments that
  -- represent the constructor. (In case of structure there is only one constructor).
  -- The list of strings is naturally ordered by priority, so the first possible match is being parsed by
  -- the match and parse helper functions.
  toStrings : Self -> List String
  -- allConstructors gives a list of all possible constructors of the type (in case of a structure there is only one constructor)
  -- this is used to iterate over each possible parsable constructor for an given argument
  allConstructors : List Self

-- checkParsable checks for a given ParsableArgument a if the argument is a representation of one of
-- a's toString template.
--
-- Worst case runtime: 0(n), with n being the count of templates for a.
def checkParsable [ParsableArgument a] (arg: String) (argument: a) : Bool := do
  List.elem arg (ParsableArgument.toStrings argument)

-- parseArgument checks for every possible ParsableArgument constructor if the argument is a representation of one
-- of the constructors toString template.
-- The parsing is prioritized by the first matched constructor and template.
--
-- Worst case runtime: 0(n*m) with n the count of constructors and m the max count of templates of one constructor.
def parseArgument [ParsableArgument a] (arg: String) : Option a := do
  List.find? (checkParsable arg) ParsableArgument.allConstructors

-- parseArgumentList parses a List of String arguments.
-- Returns a tuple of all parsed argumends and all unparsed Strings.
def parseArgumentList [ParsableArgument a] : List String -> List a × List String
  | [] => ([], [])
  | a::as => do
    let (globalArgs, args) := parseArgumentList as
    match parseArgument a with
    | some a => (a::globalArgs, args)
    | none => (globalArgs, a::args)

-- Global Arguments are arguments which are supported by every command.
inductive GlobalArgument where
  | debug : GlobalArgument

namespace GlobalArgument

-- We conform GlobalArgument to ParsableArgument so we get the benefits of it to make
-- it easily parsable for each argument provided by the user. 
instance : ParsableArgument GlobalArgument where
  toStrings
  | debug => ["-d", "--debug"]

  allConstructors := [debug]

end GlobalArgument

-- MARK: Commands
inductive Command where
  | generate : Command
  | analyze : Command
  | help : Command
  | version : Command

namespace Command

-- We conform Command to ParsableArgument so we get the benefits of it to make
-- it easily parsable for each argument provided by the user. 
instance : ParsableArgument Command where
  toStrings
  | Command.generate => ["generate"]
  | Command.analyze => ["analyze"]
  | Command.help => ["help"]
  | Command.version => ["-v", "--version"] -- although this is technically a GlobalArgument we only evaluate it if its the only argument

  allConstructors := [generate, analyze, help, version]

-- Execute defines for each available command the execution context. It propagates all already parsed global arguments
-- and all unspecified arguments to the execution context.
def execute (c: Command) (globalArgs: List GlobalArgument) (args: List String) : IO UInt32 := do
  match c with
  | generate => IO.println s!"Execute generate"
  | analyze => IO.println s!"Execute analyze"
  | help => IO.println s!"Execute help"
  | version => Version.printVersion
  return 0

end Command


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

end CLI