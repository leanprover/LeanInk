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
def tokensBetween (head : String.Pos) (tail : Option String.Pos) (compounds: List (Compound Token)) : List (Compound Token) := Id.run do
  let mut tokens : Array (Compound Token) := #[]
  for token in compounds do
    match (tail, token.tailPos) with
    | (_, none) => continue
    | (some tail, some tokenTail) =>
      if token.headPos <= tail && tokenTail > head then
        tokens ← tokens.push token
    | (none, some tokenTail) => 
      if tokenTail > head then
        tokens ← tokens.push token
  return tokens.toList

def matchTokenToAnalysis (tokens : List (Compound Token)) (aux : List Annotation) : List (Compound Sentence) -> List Annotation
  | [] => aux
  | x::y::xs => 
    let tokens := (tokens.dropWhile (λ t => x.headPos > t.tailPos.getD t.headPos))
    matchTokenToAnalysis tokens (aux.append [{ sentence := x, tokens := tokensBetween x.headPos y.headPos tokens}]) (y::xs)
  | x::xs => 
    let tokens := (tokens.dropWhile (λ t => x.headPos > t.tailPos.getD t.headPos))
    matchTokenToAnalysis tokens (aux.append [{ sentence := x, tokens := tokensBetween x.headPos none tokens}]) xs

def annotateFile (analysis : AnalysisResult) : AnalysisM (List Annotation) := do
  let compounds ← matchCompounds (toFragmentIntervals analysis.sentences)
  let tokens ← matchCompounds (toFragmentIntervals analysis.tokens)
  return matchTokenToAnalysis tokens [] compounds