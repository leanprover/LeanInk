import LeanInk.Configuration
import LeanInk.ListUtil
import LeanInk.Logger

import LeanInk.Annotation.Util
import LeanInk.Annotation.DataTypes

import LeanInk.Analysis.DataTypes
import LeanInk.Analysis.Analysis

namespace LeanInk.Annotation

open LeanInk.Analysis

/-
  Annotation
-/

def matchTokenToAnalysis (aux : List Annotation) : List (Compound Sentence) -> List Annotation
  | [] => aux
  | x::y::xs =>
    matchTokenToAnalysis (aux.append [{ sentence := x }]) (y::xs)
  | x::xs =>
    matchTokenToAnalysis (aux.append [{ sentence := x }]) xs

def annotateFile (analysis : AnalysisResult) : AnalysisM (List Annotation) := do
  let compounds ← matchCompounds (toFragmentIntervals analysis)
  return matchTokenToAnalysis [] compounds