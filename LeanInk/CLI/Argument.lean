
namespace LeanInk.CLI

-- ARGUMENTS + ENVIRONMENT VARIABLES
structure Flag where
  identifiers : List String
  optional : Bool
  help : String
  deriving BEq

structure Environment where
  identifiers : List String
  optional : Bool
  help : String
  deriving BEq

inductive Argument where
  | flag (i : Flag)
  | env (i : Environment)

inductive ResolvedArgument where
  | flag (self: Argument)
  | env (self: Environment) (val: String)