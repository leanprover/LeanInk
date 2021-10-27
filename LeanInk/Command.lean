import LeanInk.ParsableArgument
import LeanInk.GlobalArgument
import LeanInk.Version
import LeanInk.Help

namespace LeanInk

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
  | Command.version => ["-V", "--version"] -- although this is technically a GlobalArgument we only evaluate it if its the only argument

  allConstructors := [generate, analyze, help, version]

-- Execute defines for each available command the execution context. It propagates all already parsed global arguments
-- and all unspecified arguments to the execution context.
def execute (c: Command) (globalArgs: List GlobalArgument) (args: List String) : IO UInt32 := do
  match c with
  | generate => IO.println s!"Execute generate"; return 0
  | analyze => IO.println s!"Execute analyze"; return 0
  | help => Help.execute globalArgs args
  | version => Version.printVersion

end Command