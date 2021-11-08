import Lean.Data.Json

namespace LeanInk.Output.AlectryonFragment

open Lean

structure Hypothesis where
  names : List String
  body : String
  type : String
  deriving FromJson, ToJson

structure Goal where
  name : String
  conclusion : String
  hyptheses : Array Hypothesis
  deriving FromJson, ToJson

structure Message where
  contents : String
  deriving FromJson, ToJson

structure Sentence where
  contents : String
  messages : Array Message
  goals : Array Goal
  deriving FromJson, ToJson

structure Text where
  contents : String
  deriving FromJson, ToJson

inductive Fragment where
  | text (value : Text) : Fragment
  | sentence (value : Sentence) : Fragment
  deriving FromJson, ToJson
