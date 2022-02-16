import LeanInk.Annotation.Basic
import LeanInk.Logger
import LeanInk.Analysis.DataTypes
import LeanInk.Annotation.Util
import LeanInk.FileHelper

import Lean.Data.Json
import Lean.Data.Json.FromToJson
import Lean.Data.Lsp

namespace LeanInk.Annotation.Alectryon

open Lean
open LeanInk.Analysis

structure TypeInfo where
  _type : String := "typeinfo"
  name : String
  type : String
  deriving ToJson

structure Token where
  _type : String := "token"
  raw : String
  typeinfo : Option TypeInfo := Option.none
  link : Option String := Option.none
  docstring : Option String := Option.none
  semanticType : Option String :=  Option.none
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

/- 
  Token Generation
-/

def genTypeInfo? (getContents : String.Pos -> String.Pos -> String) (token : Analysis.TypeTokenInfo) : AnalysisM (Option TypeInfo) := do
  match token.type with
  | some type => do
    let headPos := Positional.headPos token
    let tailPos := Positional.tailPos token
    return some { name := (getContents headPos tailPos), type := type }
  | none => pure none

def genSemanticTokenValue : Option SemanticTokenInfo -> AnalysisM (Option String)
  | none => pure none
  | some info =>
    match info.semanticType with
    | SemanticTokenType.property => pure (some "Name.Attribute")
    | SemanticTokenType.keyword => pure (some "Keyword")
    | SemanticTokenType.variable => pure (some "Name.Variable")
    | default => pure none

def genToken (token : Compound Analysis.Token) (contents : String) (getContents : String.Pos -> String.Pos -> String) : AnalysisM Token := do
  let typeTokens := token.getFragments.filterMap (λ x => x.toTypeTokenInfo?)
  let semanticTokens := token.getFragments.filterMap (λ x => x.toSemanticTokenInfo?)
  let semanticToken := Positional.smallest? semanticTokens
  let semanticTokenType ← genSemanticTokenValue semanticToken
  match (Positional.smallest? typeTokens) with
  | none => do 
    return { raw := contents, semanticType := semanticTokenType }
  | some token => do 
    return { raw := contents, typeinfo := ← genTypeInfo? getContents token, link := none, docstring := token.docString, semanticType := semanticTokenType }

def extractContents (offset : String.Pos) (contents : String) (head tail: String.Pos) : String := 
  if head >= tail then
    ""
  else
    contents.extract (head - offset) (tail - offset)

def minPos (x y : String.Pos) := if x < y then x else y
def maxPos (x y : String.Pos) := if x > y then x else y

partial def genTokens (contents : String) (head : String.Pos) (offset : String.Pos) (l : List Token) :  List (Compound Analysis.Token) -> AnalysisM (List Token)
| [] => do
  logInfo s!"TEXT-X: {head} - {contents.utf8ByteSize + offset} > {contents}"
  let text := extractContents offset contents head (contents.utf8ByteSize + offset)
  logInfo s!"Text-X1: {text}"
  return l.append [{ raw := text }]
| x::[] => do
  logInfo s!"TEXT-A: {head} - {contents.utf8ByteSize + offset} > {x.headPos} {x.tailPos.getD x.headPos} {contents}"
  let extract := extractContents offset contents
  if x.headPos <= head then
    let textTail := contents.utf8ByteSize + offset
    let tokenTail := maxPos head (x.tailPos.getD textTail)
    let tail := minPos textTail (x.tailPos.getD textTail)
    let head := maxPos head x.headPos
    let text := extract head tail
    logInfo s!"Text-A1: {text}"
    let fragment ← genToken x text extract
    return ← genTokens contents tail offset (l.append [fragment]) ([])
  else
    let textTail := contents.utf8ByteSize + offset
    let tail := minPos textTail x.headPos
    let text := extract head tail
    logInfo s!"Text-A2: {text}"
    return ← genTokens contents tail offset (l.append [{ raw := text }]) ([x])
| x::y::ys => do
  logInfo s!"TEXT-B: {head} - {contents.utf8ByteSize + offset} > {x.headPos} {x.tailPos.getD x.headPos} {y.headPos} {y.tailPos.getD x.headPos} {contents}"
  let extract := extractContents offset contents
  if x.headPos <= head then
    let textTail := contents.utf8ByteSize + offset
    let tokenTail := maxPos head (x.tailPos.getD y.headPos)
    let tail := minPos (minPos y.headPos tokenTail) textTail
    let head := maxPos head x.headPos
    let text := extract head tail
    logInfo s!"Text-B1: {text}"
    let fragment ← genToken x text extract
    return (← genTokens contents tail offset (l.append [fragment]) (y::ys))
  else
    let textTail := contents.utf8ByteSize + offset
    let tail := minPos textTail x.headPos
    let text := extract head tail
    let nextHead := maxPos head tail
    logInfo s!"Text-B2: {text}"
    return (← genTokens contents tail offset (l.append [{ raw := text }]) (x::y::ys))

/- 
  Fragment Generation
-/

def genHypothesis (hypothesis : Analysis.Hypothesis) : Hypothesis := {
  names := hypothesis.names
  body := hypothesis.body
  type := hypothesis.type
}

def genGoal (goal : Analysis.Goal) : Goal := {
  name := goal.name
  conclusion := goal.conclusion
  hypotheses := (goal.hypotheses.map genHypothesis).toArray
}

def genGoals (beforeNode: Bool) (tactic : Analysis.Tactic) : List Goal := 
  if beforeNode then 
    tactic.goalsBefore.map (λ g => genGoal g)
  else
    tactic.goalsAfter.map (λ g => genGoal g)

def genMessages (message : Analysis.Message) : Message := { contents := message.msg }

def genFragment (annotation : Annotation) (globalTailPos : String.Pos) (contents : String) : AnalysisM Alectryon.Fragment := do
  let config ← read
  if annotation.sentence.fragments.isEmpty then
    if config.experimentalTypeInfo ∨ config.experimentalDocString then
      let headPos := annotation.sentence.headPos
      let tokens ← genTokens contents headPos headPos [] annotation.tokens
      return Fragment.text { contents := Contents.experimentalTokens tokens.toArray }
    else
      return Fragment.text { contents := Contents.string contents }
  else
    let tactics : List Analysis.Tactic := annotation.sentence.getFragments.filterMap (λ f => f.asTactic?)
    let messages : List Analysis.Message := annotation.sentence.getFragments.filterMap (λ f => f.asMessage?)
    let mut goals : List Goal := []
    if let (some tactic) := Positional.smallest? tactics then
      let useBefore : Bool := tactic.tailPos > globalTailPos
      goals := genGoals useBefore tactic
    let mut fragmentContents : Contents := Contents.string contents
    if config.experimentalTypeInfo ∨ config.experimentalDocString then
      let headPos := annotation.sentence.headPos
      let tokens ← genTokens contents headPos headPos [] annotation.tokens
      fragmentContents := Contents.experimentalTokens tokens.toArray
    return Fragment.sentence { 
      contents := fragmentContents
      goals := goals.toArray
      messages := (messages.map genMessages).toArray
    }

/-
Expects a list of sorted CompoundFragments (sorted by headPos).
Generates AlectryonFragments for the given CompoundFragments and input file content.
-/
def annotateFileWithCompounds (l : List Alectryon.Fragment) (contents : String) : List Annotation -> AnalysisM (List Fragment)
| [] => pure l
| x::[] => do
  let fragment ← genFragment x contents.utf8ByteSize (contents.extract x.sentence.headPos contents.utf8ByteSize)
  return l.append [fragment]
| x::y::ys => do
  let fragment ← genFragment x y.sentence.headPos (contents.extract x.sentence.headPos (y.sentence.headPos))
  return (← annotateFileWithCompounds (l.append [fragment]) contents (y::ys))

def genOutput (annotation : List Annotation) : AnalysisM UInt32 := do
  let config := (← read)
  let fragments ← annotateFileWithCompounds [] config.inputFileContents annotation
  createOutputFile (← IO.currentDir) config.inputFileName (generateOutput fragments.toArray)
  return 0
