import LeanInk.ListUtil

import Lean.Elab.Command
import Lean.Data.Lsp

namespace LeanInk.Analysis

open Lean
open Lean.Elab
open Lean.Meta

/-
  TacticFragment
-/
structure TacticFragment where
  info: TacticInfo
  ctx: ContextInfo
  deriving Inhabited

namespace TacticFragment
  def headPos (f: TacticFragment) : String.Pos := 
    (f.info.toElabInfo.stx.getPos? false).getD 0

  def tailPos (f: TacticFragment) : String.Pos := 
    (f.info.toElabInfo.stx.getTailPos? false).getD 0

  def length (f: TacticFragment) : Nat := 
    tailPos f - headPos f

  def toFormat (f: TacticFragment) : IO Format := 
    TacticInfo.format f.ctx f.info

  def isExpanded (f: TacticFragment) : Bool :=
    match f.info.toElabInfo.stx.getHeadInfo, f.info.toElabInfo.stx.getTailInfo with
    | SourceInfo.original .., SourceInfo.original .. => false
    | _, _ => true
end TacticFragment

/-
  MessageFragment
-/
structure MessageFragment where
  headPos: String.Pos
  tailPos: String.Pos
  msg: Message

def Position.toStringPos (fileMap: FileMap) (pos: Position) : String.Pos :=
  FileMap.lspPosToUtf8Pos fileMap (fileMap.leanPosToLspPos pos)

namespace MessageFragment
  def mkFragment (fileMap: FileMap) (msg: Message) : MessageFragment :=
    let headPos := Position.toStringPos fileMap msg.pos
    let tailPos := Position.toStringPos fileMap (msg.endPos.getD msg.pos)
    { headPos := headPos, tailPos := tailPos, msg := msg }

  def length (f: MessageFragment) : Nat := f.tailPos - f.headPos
end MessageFragment

/-
  Term Fragment
-/
structure TermFragment where
  ctx : ContextInfo
  info : TermInfo
  deriving Inhabited

namespace TermFragment
  def headPos (f: TermFragment) : String.Pos := 
    (f.info.toElabInfo.stx.getPos? false).getD 0

  def tailPos (f: TermFragment) : String.Pos := 
    (f.info.toElabInfo.stx.getTailPos? false).getD 0

  def toFormat (f: TermFragment) : IO Format := 
    TermInfo.format f.ctx f.info

  def isExpanded (f: TermFragment) : Bool :=
    match f.info.toElabInfo.stx.getHeadInfo, f.info.toElabInfo.stx.getTailInfo with
    | SourceInfo.original .., SourceInfo.original .. => false
    | _, _ => true
end TermFragment

inductive Fragment where
  | tactic (f : TacticFragment)
  | term (f : TermFragment)

/-
  Traversal Result
-/
structure TraversalResult where
  tactics : List TacticFragment
  terms : List TermFragment
  deriving Inhabited

namespace TraversalResult
  def empty : TraversalResult := {  tactics := [], terms := [] }
  def isEmpty (x : TraversalResult) : Bool := x.tactics.isEmpty ∧ x.terms.isEmpty
end TraversalResult

/-
  InfoTree traversal
-/
def Info.toFragment (info : Info) (ctx : ContextInfo) : Option Fragment :=
  match info with
  | Info.ofTacticInfo i => 
    let fragment : TacticFragment := { info :=  i, ctx := ctx }
    if fragment.isExpanded then
      none
    else
      Fragment.tactic fragment
  | Info.ofTermInfo i =>
    let fragment : TermFragment := { info :=  i, ctx := ctx }
    if fragment.isExpanded then
      none
    else
      Fragment.term fragment
  | _ => none

def mergeSortFragments (x y : TraversalResult) : TraversalResult := { 
  tactics := List.mergeSortedLists (λ x y => x.headPos < y.headPos) x.tactics y.tactics
  terms := List.mergeSortedLists (λ x y => x.headPos < y.headPos) x.terms y.terms
}

partial def _resolveTacticList (ctx?: Option ContextInfo := none) : InfoTree -> TraversalResult
  | InfoTree.context ctx tree => _resolveTacticList ctx tree
  | InfoTree.node info children =>
    match ctx? with
    | none => TraversalResult.empty
    | some ctx =>
      let ctx? := info.updateContext? ctx
      let resolvedChildrenLeafs := children.toList.map (_resolveTacticList ctx?)
      let sortedChildrenLeafs := resolvedChildrenLeafs.foldl mergeSortFragments TraversalResult.empty
      match Info.toFragment info ctx with
      | some (Fragment.tactic f) => 
        if sortedChildrenLeafs.tactics.isEmpty then 
          { sortedChildrenLeafs with tactics := [f] }
        else 
          sortedChildrenLeafs
      | some (Fragment.term f) => 
        if sortedChildrenLeafs.terms.isEmpty then 
          { sortedChildrenLeafs with terms := [f] }
        else 
          sortedChildrenLeafs
      | none => sortedChildrenLeafs  
  | _ => TraversalResult.empty

def resolveTacticList (trees: List InfoTree) : TraversalResult :=
  (trees.map _resolveTacticList).foldl mergeSortFragments TraversalResult.empty
