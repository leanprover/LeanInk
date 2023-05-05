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


structure Goal where
  _type : String := "goal"
  name : String
  goalState : String
  deriving ToJson

structure Message where
  _type : String := "message"  
  contents : String
  deriving ToJson

structure Sentence where
  _type : String := "sentence"
  contents : String
  messages : Array Message
  goals : Array Goal
  deriving ToJson

abbrev Fragment := Sentence

def extractContents (offset : String.Pos) (contents : String) (head tail: String.Pos) : Option String := 
  if head >= tail then
    none
  else
    contents.extract (head - offset) (tail - offset)

def minPos (x y : String.Pos) := if x < y then x else y
def maxPos (x y : String.Pos) := if x > y then x else y

  
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
  goalState := goal.goalState
}

def genGoals (beforeNode: Bool) (tactic : Analysis.Tactic) : List Goal := 
  if beforeNode then 
    tactic.goalsBefore.map (λ g => genGoal g)
  else
    tactic.goalsAfter.map (λ g => genGoal g)

def genMessages (message : Analysis.Message) : Message := { contents := message.msg }

def isComment (contents : String) : Bool := 
  let contents := contents.trim
  contents.startsWith "--" || contents.startsWith "/-"

def genFragment (annotation : Annotation) (globalTailPos : String.Pos) (contents : String) : AnalysisM Alectryon.Fragment := do
  let tactics : List Analysis.Tactic := annotation.sentence.getFragments.filterMap (λ f => f.asTactic?)
  let messages : List Analysis.Message := 
    (if isComment contents || annotation.sentence.fragments.isEmpty then [⟨⟨globalTailPos, globalTailPos⟩, "This is text"⟩] else []) ++
    annotation.sentence.getFragments.filterMap (λ f => f.asMessage?)
  let mut goals : List Goal := []
  if let (some tactic) := Positional.smallest? tactics then
    let useBefore : Bool := tactic.tailPos > globalTailPos
    goals := genGoals useBefore tactic
  let mut fragmentContents : String := contents
  return { 
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
