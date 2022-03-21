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

def genTypeInfo? (getContents : String.Pos -> String.Pos -> Option String) (token : Analysis.TypeTokenInfo) : AnalysisM (Option TypeInfo) := do
  match token.type with
  | some type => do
    let headPos := Positional.headPos token
    let tailPos := Positional.tailPos token
    match getContents headPos tailPos with 
    | none => pure none
    | "" => pure none
    | some x => return some { name := x, type := type }
  | none => pure none

def genSemanticTokenValue : Option SemanticTokenInfo -> AnalysisM (Option String)
  | none => pure none
  | some info =>
    match info.semanticType with
    | SemanticTokenType.property => pure (some "Name.Attribute")
    | SemanticTokenType.keyword => pure (some "Keyword")
    | SemanticTokenType.variable => pure (some "Name.Variable")
    | default => pure none

def genToken (token : Compound Analysis.Token) (contents : Option String) (getContents : String.Pos -> String.Pos -> Option String) : AnalysisM (Option Token) := do
  match contents with
  | none => return none
  | "" => return none
  | some contents => do
    let typeTokens := token.getFragments.filterMap (λ x => x.toTypeTokenInfo?)
    let semanticTokens := token.getFragments.filterMap (λ x => x.toSemanticTokenInfo?)
    let semanticToken := Positional.smallest? semanticTokens
    let semanticTokenType ← genSemanticTokenValue semanticToken
    match (Positional.smallest? typeTokens) with
    | none => do 
      return some { raw := contents, semanticType := semanticTokenType }
    | some token => do 
      return some { raw := contents, typeinfo := ← genTypeInfo? getContents token, link := none, docstring := token.docString, semanticType := semanticTokenType }

def extractContents (offset : String.Pos) (contents : String) (head tail: String.Pos) : Option String := 
  if head >= tail then
    none
  else
    contents.extract (head - offset) (tail - offset)

def minPos (x y : String.Pos) := if x < y then x else y
def maxPos (x y : String.Pos) := if x > y then x else y

partial def genTokens (contents : String) (head : String.Pos) (offset : String.Pos) (l : List Token)  (compounds : List (Compound Analysis.Token)) : AnalysisM (List Token) := do
  let textTail := ⟨contents.utf8ByteSize⟩ + offset
  let mut head : String.Pos := head
  let mut tokens : List Token := []
  for x in compounds do
    let extract := extractContents offset contents
    let tail := x.tailPos.getD textTail
    if x.headPos <= head then
      let text := extract head tail
      head := tail
      logInfo s!"Text-B1: {text}"
      match (← genToken x text extract) with
      | none => logInfo s!"Empty 1 {text} {x.headPos} {tail}"
      | some fragment => tokens := fragment::tokens
    else
      let text := extract head x.headPos
      head := x.headPos
      logInfo s!"Text-B2: {text}"
      match text with
      | none => logInfo s!"Empty 1 {text} {x.headPos} {tail}"
      | some text => tokens := { raw := text }::tokens
  match extractContents offset contents head (⟨contents.utf8ByteSize⟩ + offset) with
  | none => return tokens.reverse
  | some x => return ({ raw := x }::tokens).reverse
  
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
  let fragment ← genFragment x ⟨contents.utf8ByteSize⟩ (contents.extract x.sentence.headPos ⟨contents.utf8ByteSize⟩)
  return l.append [fragment]
| x::y::ys => do
  let fragment ← genFragment x y.sentence.headPos (contents.extract x.sentence.headPos (y.sentence.headPos))
  return (← annotateFileWithCompounds (l.append [fragment]) contents (y::ys))

def genOutput (annotation : List Annotation) : AnalysisM UInt32 := do
  let config := (← read)
  let fragments ← annotateFileWithCompounds [] config.inputFileContents annotation
  let rawContents ← generateOutput fragments.toArray
  createOutputFile (← IO.currentDir) config.inputFileName rawContents
  return 0
