namespace LeanInk.CLI

-- ARGUMENT INFO
structure ArgumentInfo where
  identifiers : List String
  isOptional : Bool := true
  help : String
  deriving BEq

structure Flag extends ArgumentInfo where
  -- There are no additional fields yet

structure Environment extends ArgumentInfo where
  -- There are no additional fields yet

-- ARGUMENT
inductive Argument where
  | flag (i : Flag)
  | environment (i : Environment)

namespace Argument
  def toArgumentInfo : Argument -> ArgumentInfo
    | flag i => i.toArgumentInfo
    | environment i => i.toArgumentInfo

  def identifiers (self: Argument) : List String := 
    self.toArgumentInfo.identifiers

  def help (self: Argument) : String := 
    self.toArgumentInfo.help

  def isOptional (self: Argument) : Bool := 
    self.toArgumentInfo.isOptional
end Argument

instance : BEq Argument where
  beq (left right : Argument) : Bool := left.identifiers == right.identifiers

-- RESOLVED ARGUMENT
inductive ResolvedArgument where
  | flag (self: Flag)
  | env (self: Environment) (val: String)

namespace ResolvedArgument
  def toArgumentInfo : ResolvedArgument -> ArgumentInfo
    | flag i => i.toArgumentInfo
    | env i _ => i.toArgumentInfo

  def identifiers (self: ResolvedArgument) : List String := 
    self.toArgumentInfo.identifiers

  def help (self: ResolvedArgument) : String := 
    self.toArgumentInfo.help

  def isOptional (self: ResolvedArgument) : Bool := 
    self.toArgumentInfo.isOptional
end ResolvedArgument

instance : ToString ResolvedArgument where
  toString : ResolvedArgument -> String
    | ResolvedArgument.flag i => s!"\n<FLAG>[{i.identifiers}]"
    | ResolvedArgument.env i v => s!"\n<ENV>[{i.identifiers}]={v}"