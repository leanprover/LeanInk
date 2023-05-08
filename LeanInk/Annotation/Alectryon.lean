import LeanInk.Logger
import LeanInk.Analysis.DataTypes
import LeanInk.Annotation.DataTypes

import Lean.Data.Json
import Lean.Data.Json.FromToJson
import Lean.Data.Lsp

namespace LeanInk.Annotation.Alectryon

open Lean
open LeanInk.Analysis

set_option autoImplicit false

structure Fragment where
  contents : String
  goalsBefore : List String
  goalsAfter : List String
  deriving ToJson
  
/- 
  Fragment Generation
-/

def genFragment (annotation : Annotation) (contents : String) : Alectryon.Fragment :=
  match Positional.smallest? annotation.getFragments with
    | some tactic => { contents := contents, goalsBefore := tactic.goalsBefore, goalsAfter := tactic.goalsAfter }
    | none => { contents := contents, goalsBefore := [], goalsAfter := [] }

/-
Expects a list of sorted CompoundFragments (sorted by headPos).
Generates AlectryonFragments for the given CompoundFragments and input file content.
-/
def annotateFileWithCompounds (l : List Alectryon.Fragment) (contents : String) : List Annotation -> IO (List Fragment)
| [] => pure l
| [x] => do
  let fragment := genFragment x (contents.extract x.headPos ⟨contents.utf8ByteSize⟩)
  return l.concat fragment
| x :: y :: ys => do
  let fragment := genFragment x (contents.extract x.headPos y.headPos)
  annotateFileWithCompounds (l.concat fragment) contents (y :: ys)

def genOutput (file : System.FilePath) (contents : String) (annotation : List Annotation) : IO UInt32 := do
  let fragments ← annotateFileWithCompounds [] contents annotation
  let rawContents := (toJson fragments.toArray).pretty
  let dirEntry : IO.FS.DirEntry := { 
    root := ← IO.currentDir,
    fileName := file.toString ++ ".json"
  }
  IO.FS.writeFile dirEntry.path rawContents
  logInfo s!"Results written to file: {dirEntry.path}!"
  return 0
