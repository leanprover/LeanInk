import Lean.Data.Json
import Lean.Data.Json.FromToJson

namespace LeanInk.Output.AlectryonFragment

open Lean

structure Hypothesis where
  names : List String
  body : String
  type : String
  deriving FromJson, ToJson

structure Goal where
  _type : String := "goal"
  name : String
  conclusion : String
  hyptheses : Array Hypothesis
  deriving FromJson, ToJson

structure Message where
  _type : String := "message"  
  contents : String
  deriving FromJson, ToJson

structure Sentence where
  _type : String := "sentence"
  contents : String
  messages : Array Message
  goals : Array Goal
  deriving FromJson, ToJson

structure Text where
  _type : String := "text"
  contents : String
  deriving FromJson, ToJson

inductive Fragment where
  | text (value : Text) : Fragment
  | sentence (value : Sentence) : Fragment

namespace Json

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
instance : ToJson Fragment where
  toJson
    | Fragment.text v => toJson v
    | Fragment.sentence v => toJson v
  