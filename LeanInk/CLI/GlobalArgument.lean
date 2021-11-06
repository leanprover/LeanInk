import LeanInk.CLI.ParsableArgument

namespace LeanInk.CLI

-- Global Arguments are arguments which are supported by every command.
inductive GlobalArgument where
  | verbose : GlobalArgument
  deriving BEq

namespace GlobalArgument

-- We conform GlobalArgument to ParsableArgument so we get the benefits of it to make
-- it easily parsable for each argument provided by the user. 
instance : ParsableArgument GlobalArgument where
  toStrings
  | verbose => ["-v", "--verbose"]

  allConstructors := [verbose]

end GlobalArgument