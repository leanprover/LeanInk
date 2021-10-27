import LeanInk.ParsableArgument
import LeanInk.GlobalArgument
import LeanInk.Version
import LeanInk.Help

namespace LeanInk

-- MARK: Commands
inductive Command where
  | generate : Command
  | analyze : Command
  | version : Command
  | licenses : Command
  | help : Command

namespace Command

-- We conform Command to ParsableArgument so we get the benefits of it to make
-- it easily parsable for each argument provided by the user. 
instance : ParsableArgument Command where
  toStrings
  | generate => ["g", "generate"]
  | analyze => ["a", "analyze"]
  | version => ["v", "version"]
  | licenses => ["l", "licenses"]
  | help => ["h", "help"]

  allConstructors := [generate, analyze, version, licenses, help]

def helpMessage : Command -> String
  | generate => s!"TODO"
  | analyze => s!"TODO"
  | licenses => s!"TODO"
  | _ => Help.generalHelp

def printHelp : Option Command -> IO UInt32
  | some c => do 
    IO.println (helpMessage c)
    return 0
  | none => do 
    IO.println (helpMessage help)
    return 0

-- Execute defines for each available command the execution context. It propagates all already parsed global arguments
-- and all unspecified arguments to the execution context.
def execute (c: Command) (globalArgs: List GlobalArgument) (args: List String) : IO UInt32 := do
  match c with
  | generate => IO.println s!"Execute generate"; return 0
  | analyze => IO.println s!"Execute analyze"; return 0
  | version => Version.printVersion
  | licenses => IO.println s!"Print licenses"; return 0
  | help => do
    match args with
    | [] => printHelp none
    | a::as => printHelp (parseArgument a)

end Command