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

structure Fragment where
  contents : String
  goals : Array String
  deriving ToJson
  
/- 
  Fragment Generation
-/

def genGoals (beforeNode: Bool) (tactic : Analysis.Tactic) : List String := 
  if beforeNode then 
    tactic.goalsBefore
  else
    tactic.goalsAfter

def genFragment (annotation : Annotation) (globalTailPos : String.Pos) (contents : String) : AnalysisM Alectryon.Fragment := do
  let tactics : List Analysis.Tactic := annotation.getFragments
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
  let fragment ← genFragment x ⟨contents.utf8ByteSize⟩ (contents.extract x.headPos ⟨contents.utf8ByteSize⟩)
  return l.append [fragment]
| x::y::ys => do
  let fragment ← genFragment x y.headPos (contents.extract x.headPos y.headPos)
  return (← annotateFileWithCompounds (l.append [fragment]) contents (y::ys))

def genOutput (annotation : List Annotation) : AnalysisM UInt32 := do
  let config ← read
  let fragments ← annotateFileWithCompounds [] config.inputFileContents annotation
  let rawContents ← generateOutput fragments.toArray
  createOutputFile (← IO.currentDir) config.inputFileName rawContents
  return 0
