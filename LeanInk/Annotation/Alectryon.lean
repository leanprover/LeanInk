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

set_option autoImplicit false

structure Sentence where
  _type : String := "sentence"
  contents : String
  goals : Array String
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

def genGoals (beforeNode: Bool) (tactic : Analysis.Tactic) : List String := 
  if beforeNode then 
    tactic.goalsBefore
  else
    tactic.goalsAfter

def isComment (contents : String) : Bool := 
  let contents := contents.trim
  contents.startsWith "--" || contents.startsWith "/-"

def genFragment (annotation : Annotation) (globalTailPos : String.Pos) (contents : String) : AnalysisM Alectryon.Fragment := do
  let tactics : List Analysis.Tactic := annotation.sentence.getFragments
  let mut goals : List String := []
  if let (some tactic) := Positional.smallest? tactics then
    let useBefore : Bool := tactic.tailPos > globalTailPos
    goals := genGoals useBefore tactic
  let mut fragmentContents : String := contents
  return { 
    contents := fragmentContents
    goals := goals.toArray
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
