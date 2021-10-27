import LeanInk.Version

namespace LeanInk
namespace CLI

-- MARK: ParsableArgument
class ParsableArgument (Self: Type) where
  toStrings : Self -> List String
  allConstructors : List Self

def matchTemplates (arg: String) (templates: List String) : Bool := do
  match templates with
  | a::as => do
    if a == arg then
      return true
    else 
      matchTemplates arg as
  | _ => false

def checkParsable [ParsableArgument a] (arg: String) (argument: a) : Bool := do
  matchTemplates arg (ParsableArgument.toStrings argument)

def parseArgument [ParsableArgument a] (arg: String) : Option a := do
  List.find? (checkParsable arg) ParsableArgument.allConstructors

def parseArgumentList [ParsableArgument a] : List String -> List a × List String
  | [] => ([], [])
  | a::as => do
    let (globalArgs, args) := parseArgumentList as
    match parseArgument a with
    | some a => (a::globalArgs, args)
    | none => (globalArgs, a::args)


-- MARK: Global Arguments
inductive GlobalArgument where
  | debug : GlobalArgument

namespace GlobalArgument

instance : ParsableArgument GlobalArgument where
  toStrings
  | debug => ["-d", "--debug"]

  allConstructors := [debug]

def parseArguments (args: List String) : List GlobalArgument × List String := do
  parseArgumentList args

end GlobalArgument

-- MARK: Commands
inductive Command where
  | generate : Command
  | analyze : Command
  | help : Command
  | version : Command

namespace Command

instance : ParsableArgument Command where
  toStrings
  | Command.generate => ["generate"]
  | Command.analyze => ["analyze"]
  | Command.help => ["help"]
  | Command.version => ["-v", "--version"]

  allConstructors := [generate, analyze, help, version]

def parseCommand (arg: String) : Option Command := do
  parseArgument arg

def execute (c: Command) (globalArgs: List GlobalArgument) (args: List String) : IO UInt32 := do
  match c with
  | generate => IO.println s!"Execute generate"
  | analyze => IO.println s!"Execute analyze"
  | help => IO.println s!"Execute help"
  | version => Version.printVersion
  return 0

end Command

-- MARK: CLI interface
def run (args: List String) : IO UInt32 := do
  match args with
  | [] => do
        IO.println s!"No command provided!"
        return 1
  | a::as => do
    let (globalArgs, args) := GlobalArgument.parseArguments as
    match Command.parseCommand a with
    | some command => Command.execute command globalArgs args
    | none => do
        IO.println s!"Unknown command {a}"
        return 1

end CLI