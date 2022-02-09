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
def tokensBetween (aux : List (Compound Token)) (head : String.Pos) (tail : Option String.Pos) : List (Compound Token) -> List (Compound Token)
  | [] => aux
  | x::xs =>
    match (tail, x.tailPos) with
    | (_, none) => tokensBetween aux head tail xs
    | (some tail, some tokenTail) =>
      if x.headPos <= tail && tokenTail > head then
        tokensBetween (aux.append [x]) head tail xs
      else
        tokensBetween aux head tail xs
    | (none, some tokenTail) => 
      if tokenTail > head then
        tokensBetween (aux.append [x]) head tail xs
      else
        tokensBetween aux head tail xs

def matchTokenToAnalysis (tokens : List (Compound Token)) (aux : List Annotation) : List (Compound Sentence) -> List Annotation
  | [] => aux
  | x::y::xs => matchTokenToAnalysis tokens (aux.append [{ sentence := x, tokens := tokensBetween [] x.headPos y.headPos tokens }]) (y::xs)
  | x::xs => matchTokenToAnalysis tokens (aux.append [{ sentence := x, tokens := tokensBetween [] x.headPos none tokens }]) xs

def annotateFile (analysis : AnalysisResult) : AnalysisM (List Annotation) := do
  let compounds ← matchCompounds [{ headPos := 0, fragments := [] }] (toFragmentIntervals analysis.sentences)
  let tokens ← matchCompounds [{ headPos := 0, fragments := [] }] (toFragmentIntervals analysis.tokens)
  return matchTokenToAnalysis tokens [] compounds