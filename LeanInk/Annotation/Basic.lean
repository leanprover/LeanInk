import LeanInk.Configuration
import LeanInk.ListUtil
import LeanInk.Logger

import LeanInk.Annotation.Util

import LeanInk.Analysis.DataTypes
import LeanInk.Analysis.Analysis

namespace LeanInk.Annotation

open LeanInk.Analysis

/-
  Annotation
-/
structure Annotation where
  sentence : Compound Sentence
  tokens : List (Compound Token)

-- partial def generateTokens (contents: String) (head: String.Pos) (offset: String.Pos) (aux: List Alectryon.Token) : List (Compound Token) -> AnalysisM (List Alectryon.Token)
--   | [] => do
--     let text := contents.extract (head - offset) contents.utf8ByteSize
--     logInfo s!"generateTokens last >> '{text}' | {head - offset} - {contents.utf8ByteSize}"
--     if text.isEmpty then return aux
--     let lastToken : Alectryon.Token := { raw := text }
--     return aux.append [lastToken]
--   | tokens::[] => do
--     match tokens.tailPos with
--     | none => generateTokens contents head offset aux []
--     | some tail => do
--       let head := head - offset
--       let tokenHead := tokens.headPos - offset
--       let tokenTail := tail - offset
--       if head >= tokenTail then
--         return ← generateTokens contents (head + offset) offset aux []
--       if head >= tokenHead then
--         let text := contents.extract head tokenTail
--         logInfo s!"generateTokens token - tail >> '{text}' | {head} - {tokenHead}<>{tokenTail}"
--         if text.isEmpty then 
--           return ← generateTokens contents tail offset aux []
--         else
--           let aToken : Alectryon.Token := { raw := text, typeinfo := (← Token.generateTypeInfo tokens text) }
--           return ← generateTokens contents tail offset (aux.append [aToken]) []
--       else
--         let text := contents.extract head tokenHead
--         logInfo s!"generateTokens token - nextHead >> '{text}' | {head} - {tokenHead}<>{tokenTail}"
--         if text.isEmpty then 
--           return ← generateTokens contents tail offset aux (tokens::[])
--         else
--           let aToken : Alectryon.Token := { raw := text }
--           return ← generateTokens contents tokens.headPos offset (aux.append [aToken]) (tokens::[])
--   | tokens::follow::ts => do
--     match tokens.tailPos with
--     | none => generateTokens contents head offset aux ts
--     | some tail => do
--       let head := head - offset
--       let tokenHead := tokens.headPos - offset
--       let tokenTail := tail - offset
--       let followHead := follow.headPos - offset
--       let nextTail := if tokenTail < followHead then tokenTail else followHead
--       if head >= nextTail then
--         return ← generateTokens contents (head + offset) offset aux (follow::ts)
--       if head >= tokenHead then
--         let text := contents.extract head nextTail
--         let fullText :=  contents.extract tokenHead tokenTail
--         logInfo s!"generateTokens token - tail >> '{text}' | {head} - {tokenHead}<>{nextTail}"
--         if text.isEmpty then 
--           return ← generateTokens contents (nextTail + offset) offset aux (follow::ts)
--         else
--           let aToken : Alectryon.Token := { raw := text, typeinfo := (← Token.generateTypeInfo tokens fullText) }
--           return ← generateTokens contents (nextTail + offset) offset (aux.append [aToken]) (follow::ts)
--       else
--         let text := contents.extract head tokenHead
--         logInfo s!"generateTokens token - nextHead >> '{text}' | {head} - {tokenHead}<>{tokenTail}"
--         if text.isEmpty then 
--           return ← generateTokens contents tokens.headPos offset aux (tokens::follow::ts)
--         else
--           let aToken : Alectryon.Token := { raw := text }
--           return ← generateTokens contents tokens.headPos offset (aux.append [aToken]) (tokens::follow::ts)

-- def toAlectryonTokens (self : Annotation) (contents : String) : AnalysisM Alectryon.Contents := do
--   return Alectryon.Contents.experimentalTokens (← generateTokens contents self.sentenceCompound.headPos self.sentenceCompound.headPos [] self.tokenCompound).toArray 

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

/-
Expects a list of sorted CompoundFragments (sorted by headPos).
Generates AlectryonFragments for the given CompoundFragments and input file content.
-/
-- def annotateFileWithCompounds (l : List Alectryon.Fragment) (contents : String) : List Annotation -> AnalysisM (List Alectryon.Fragment)
--   | [] => l
--   | x::[] => do
--     let fragment ← toAlectryonFragment x (contents.extract x.sentenceCompound.headPos contents.utf8ByteSize)
--     return l.append [fragment]
--   | x::y::ys => do
--     let fragment ← toAlectryonFragment x (contents.extract x.sentenceCompound.headPos (y.sentenceCompound.headPos))
--     return (← annotateFileWithCompounds (l.append [fragment]) contents (y::ys))

def annotateFile (analysis : AnalysisResult) : AnalysisM (List Annotation) := do
  -- logInfo f!"Analysis-Input: {analysis.sentenceFragments}"
  -- logInfo f!"Tokens: {analysis.tokens}"
  let compounds ← matchCompounds [{ headPos := 0, fragments := [] }] (toFragmentIntervals analysis.sentences)
  let tokens ← matchCompounds [{ headPos := 0, fragments := [] }] (toFragmentIntervals analysis.tokens)
  return matchTokenToAnalysis tokens [] compounds