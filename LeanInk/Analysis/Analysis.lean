import LeanInk.Analysis.DataTypes
import LeanInk.Analysis.LeanContext

import LeanInk.Configuration
import LeanInk.ListUtil
import LeanInk.Logger

import Lean.Elab.Frontend
import Lean.Elab.Import
import Lean.Elab.Command
import Lean.Parser

import Lean.Util.Trace

namespace LeanInk.Analysis

open Lean
open Lean.Elab

-- def removeTermDuplicatesFromSorted : List TermFragment -> List TermFragment
--   | [] => []
--   | x::[] => [x]
--   | x::y::xs => 
--     if x.headPos == y.headPos then
--       x::removeTermDuplicatesFromSorted xs
--     else
--       x::removeTermDuplicatesFromSorted (y::xs)

-- def create (traversal: TraversalResult) (messages: List Message) (fileMap: FileMap) : AnalysisM AnalysisResult := do
--   let tactics := traversal.tactics.map (λ f => AnalysisFragment.tactic f)
--   let messages := messages.map (λ m => AnalysisFragment.message (MessageFragment.mkFragment fileMap m))
--   let filteredMessages := messages.filter (λ f => f.headPos < f.tailPos)
--   let sortedMessages := List.sort (λ x y => x.headPos < y.headPos) filteredMessages
--   logInfo f!"MESSAGES:\n {sortedMessages}"
--   let sentenceFragments := List.mergeSortedLists (λ x y => x.headPos < y.headPos) tactics sortedMessages
--   logInfo f!"RESULT:\n {sentenceFragments}"
--   if (← read).experimentalTokens then
--     let terms := (removeTermDuplicatesFromSorted traversal.terms).map (λ f => Token.term f)
--     let fields := traversal.fields.map (λ f => Token.field f)
--     let termsAndFields := List.mergeSortedLists (λ x y => x.headPos < y.headPos) terms fields
--     let tactics := traversal.tactics.map (λ f => Token.tactic f)
--     let tokens := List.mergeSortedLists (λ x y => x.headPos < y.headPos) termsAndFields tactics
--     Logger.logInfo f!"Terms:n {terms}"
--     return { sentenceFragments := sentenceFragments, tokens := termsAndFields }
--   else
--     return { sentenceFragments := sentenceFragments, tokens := [] }

def configureCommandState (env : Environment) (msg : MessageLog) : Command.State :=
  { Command.mkState env msg with infoState := { enabled := true }}

def analyzeInput : AnalysisM AnalysisResult := do
  let config := ← read
  let context := Parser.mkInputContext config.inputFileContents config.inputFileName
  let (header, state, messages) ← Parser.parseHeader context
  initializeSearchPaths header config
  let options := Options.empty.setBool `trace.Elab.info true
  let (environment, messages) ← processHeader header options messages context 0
  logInfo s!"Header: {environment.header.mainModule}"
  logInfo s!"Header: {environment.header.moduleNames}"
  let commandState := configureCommandState environment messages
  let s ← IO.processCommands context state commandState
  let trees := s.commandState.infoState.trees.toList
  let result ← resolveTacticList trees
  let messages := s.commandState.messages.msgs.toList
  return result