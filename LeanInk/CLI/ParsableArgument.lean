namespace LeanInk.CLI

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
