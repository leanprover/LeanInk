import Lean.Data.Json
import Lean.Data.Json.FromToJson

namespace LeanInk.Output.Alectryon

open Lean

structure TypeInfo where
  _type : String := "typeinfo"
  name : String
  type : String
  docstring : String
  deriving ToJson

structure Token where
  _type : String := "token"
  raw : String
  typeinfo : Option TypeInfo := Option.none
  link : Option String := Option.none
  deriving ToJson

/--
  Support type for experimental --experimental-type-tokens feature
  If flag not set please only use the string case.
-/
inductive Contents where
  | string (value : String)
  | experimentalTokens (value : Array Token)

instance : ToJson Contents where
  toJson
    | Contents.string v => toJson v
    | Contents.experimentalTokens v => toJson v

structure Hypothesis where
  _type : String := "hypothesis"
  names : List String
  body : String
  type : String
  deriving ToJson

structure Goal where
  _type : String := "goal"
  name : String
  conclusion : String
  hypotheses : Array Hypothesis
  deriving ToJson

structure Message where
  _type : String := "message"  
  contents : String
  deriving ToJson

structure Sentence where
  _type : String := "sentence"
  contents : Contents
  messages : Array Message
  goals : Array Goal
  deriving ToJson

structure Text where
  _type : String := "text"
  contents : Contents
  deriving ToJson

/-- 
We need a custom ToJson implementation for Alectryons fragments.

For example we have following fragment:
```
Fragment.text { contents := "Test" }
```

We want to serialize this to:
```
[{"contents": "Test", "_type": "text"}]
```

instead of:
```
[{"text": {"contents": "Test", "_type": "text"}}]
```

This is because of the way Alectryon decodes the json files. It uses the _type field to
determine the namedTuple type with Alectryon.
-/
inductive Fragment where
  | text (value : Text)
  | sentence (value : Sentence)

instance : ToJson Fragment where
  toJson
    | Fragment.text v => toJson v
    | Fragment.sentence v => toJson v