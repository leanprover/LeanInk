import LeanInk.Commands.Analyze.Configuration

import LeanInk.Output.AlectryonFragment

import Lean.Elab.Frontend
import Lean.Elab.Import
import Lean.Elab.Command
import Lean.Parser

import Lean.Util.Trace

namespace LeanInk.Commands.Analyze

open Lean
open Lean.Elab
open Output.AlectryonFragment

inductive AnalysisFragment where
  | tactic (i: TacticInfo) (ctx: ContextInfo)
  | term (i: TermInfo) (ctx: ContextInfo)
  | field (i: FieldInfo) (ctx: ContextInfo)
  deriving Inhabited -- We need this so we can use qSort on an Array AnalysisFragment.

namespace AnalysisFragment
  def toFormat : AnalysisFragment -> IO Format
  | tactic i ctx => i.format ctx
  | term i ctx => i.format ctx
  | field i ctx => i.format ctx

  def headPos : AnalysisFragment -> String.Pos
  | tactic i _ => (i.toElabInfo.stx.getPos? true).getD 0
  | term i _ => (i.toElabInfo.stx.getPos? true).getD 0
  | field i _ => (i.stx.getPos? true).getD 0

  def tailPos : AnalysisFragment -> String.Pos
  | tactic i _ => (i.toElabInfo.stx.getTailPos? true).getD 0
  | term i _ => (i.toElabInfo.stx.getTailPos? true).getD 0
  | field i _ => (i.stx.getTailPos? true).getD 0

  def size (f: AnalysisFragment) : Nat := 
    return f.tailPos - f.headPos

  def toAlectryonFragment (fragment: AnalysisFragment) : Fragment := Fragment.text { contents := s!"{fragment.headPos}" }
end AnalysisFragment

def mergeSortLists [Inhabited α] (f: α -> α -> Bool) : List α -> List α -> List α
  | [], x => (x.toArray.qsort f).toList -- this should already be a sorted list ideally but somehow it didn't work, so that's a workaround atm
  | x, [] => (x.toArray.qsort f).toList -- the size of x should be negligible anyway
  | x::xs, y::ys => 
    if f x y then
      return x::y::mergeSortLists f xs ys
    else
      return y::x::mergeSortLists f xs ys

def mergeSortedAF : List AnalysisFragment -> List AnalysisFragment -> List AnalysisFragment := mergeSortLists (λ x y => x.headPos < y.headPos)
def joinSortedAF : List (List AnalysisFragment) -> List AnalysisFragment := List.foldl mergeSortedAF []

-- TEXT_FRAGMENTATION
structure AnnotationInfo where
  head: String.Pos
  tail: String.Pos

structure CompoundInfo extends AnnotationInfo where
  fragments: List AnalysisFragment

inductive Annotation where
  | compound (i: CompoundInfo) (next: Option Annotation)
  | text (i: AnnotationInfo) (next: Option Annotation)

structure AnnotationIntervalInfo where
  head: String.Pos
  tail: String.Pos
  maxChildTail: String.Pos
  fragment: AnalysisFragment

inductive AnnotationIntervalTree where
  | node (i: AnnotationIntervalInfo) (left: Option AnnotationIntervalTree) (right: Option AnnotationIntervalTree)

namespace AnnotationIntervalTree

def Fragment.createInfo (f: AnalysisFragment) (maxTail: Option String.Pos := none) : AnnotationIntervalInfo := do
  match maxTail with
  | some maxTail => return { head := f.headPos, tail :=  f.tailPos, maxChildTail := maxTail, fragment := f }
  | none => return { head := f.headPos, tail :=  f.tailPos, maxChildTail := f.tailPos, fragment := f }

def max (a b: Option AnnotationIntervalTree) : Option String.Pos := do
  match a, b with
  | none, none => none
  | some (node ia _ _) , none => some ia.maxChildTail
  | none , some (node ib _ _) => some ib.maxChildTail
  | some (node ia _ _) , some (node ib _ _) =>
    if ia.maxChildTail > ib.maxChildTail then
      return some ia.maxChildTail 
    else 
      return some ib.maxChildTail

-- We expect a sorted list, based on the head position of each fragment.
partial def create : List AnalysisFragment -> Option AnnotationIntervalTree
  | [] => none
  | x::[] => node (Fragment.createInfo x) none none
  | xs => do
    let fragments := xs.toArray
    let centerIndex := fragments.size / 2
    let centerFragment := fragments[centerIndex]
    let left := create (xs.take (centerIndex - 1))
    let right := create (xs.drop centerIndex)
    let maxValue := max left right
    return node (Fragment.createInfo centerFragment maxValue) left right

end AnnotationIntervalTree

-- INFO TREE analysis
def Info.toAnalysisFragment (info: Info) (ctx: ContextInfo) : Option AnalysisFragment := do
  match info with
  | Info.ofTacticInfo i => AnalysisFragment.tactic i ctx
  | Info.ofTermInfo i => AnalysisFragment.term i ctx
  | Info.ofFieldInfo i => AnalysisFragment.field i ctx
  | _ => none

partial def resolveLeafList (ctx?: Option ContextInfo := none) (tree: InfoTree) : List AnalysisFragment := do
  match tree with
  | InfoTree.context ctx tree => resolveLeafList ctx tree
  | InfoTree.node info children =>
    match ctx? with
    | none => return [] -- Add error handling
    | some ctx =>
        let updatedCtx? := info.updateContext? ctx
        let resolvedChildren := joinSortedAF (children.toList.map (resolveLeafList updatedCtx?))
        match updatedCtx? with
        | none => return [] -- Add error handling
        | some ctx =>
          match Info.toAnalysisFragment info ctx with
          | some f => mergeSortedAF [f] resolvedChildren
          | none => resolvedChildren
  | _ => []

def configureCommandState (env : Environment) (msg : MessageLog) : Command.State := do 
  return { Command.mkState env msg with infoState := { enabled := true }}

def analyzeInput (config: Configuration) : IO (Option (AnnotationIntervalTree)) := do
  let context := Parser.mkInputContext config.inputFileContents config.inputFileName
  let (header, state, messages) ← Parser.parseHeader context
  let options := Options.empty.setBool `trace.Elab.info true
  let (environment, messages) ← processHeader header options messages context 0
  let commandState := configureCommandState environment messages
  let s <- IO.processCommands context state commandState
  let trees := s.commandState.infoState.trees

  IO.println s!"INFO! Trees enabled: {s.commandState.infoState.enabled}"
  IO.println s!"INFO! Gathered trees: {s.commandState.infoState.trees.size}"

  let fragments ← joinSortedAF (trees.toList.map (resolveLeafList))
  let filteredFragments := fragments.filter (λ x => x.size > 0)
  return (← AnnotationIntervalTree.create filteredFragments)