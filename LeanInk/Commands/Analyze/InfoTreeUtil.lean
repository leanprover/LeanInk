import Lean.Elab.Command

namespace LeanInk.Commands.Analyze

open Lean
open Lean.Elab

structure TacticFragment where
  info: TacticInfo
  ctx: ContextInfo
  deriving Inhabited

namespace TacticFragment
  def headPos (f: TacticFragment) : String.Pos := 
    (f.info.toElabInfo.stx.getPos? true).getD 0

  def tailPos (f: TacticFragment) : String.Pos := 
    (f.info.toElabInfo.stx.getTailPos? true).getD 0

  def size (f: TacticFragment) : Nat := 
    f.tailPos - f.headPos

  def toFormat (f: TacticFragment) : IO Format := 
    TacticInfo.format f.ctx f.info
end TacticFragment

def mergeSort [Inhabited α] (f: α -> α -> Bool) : List α -> List α -> List α
  | [], x => (x.toArray.qsort f).toList
  | x, [] => (x.toArray.qsort f).toList
  | x::xs, y::ys => 
    if f x y then
      return x::y::mergeSort f xs ys
    else
      return y::x::mergeSort f xs ys

def mergeSortFragments : List TacticFragment -> List TacticFragment -> List TacticFragment := 
  mergeSort (λ x y => x.headPos < y.headPos)

def Info.toFragment (info: Info) (ctx: ContextInfo) : Option TacticFragment := do
  match info with
  | Info.ofTacticInfo i => some { info := i, ctx := ctx }
  | _ => none

partial def _resolveTacticList (ctx?: Option ContextInfo := none) : InfoTree -> List TacticFragment
  | InfoTree.context ctx tree => _resolveTacticList ctx tree
  | InfoTree.node info children =>
    match ctx? with
    | none => return []
    | some ctx =>
      let ctx? := info.updateContext? ctx
      let resolvedChildrenLeafs := children.toList.map (_resolveTacticList ctx?)
      let sortedChildrenLeafs := resolvedChildrenLeafs.foldl mergeSortFragments []
      if sortedChildrenLeafs.isEmpty then
        match Info.toFragment info ctx with
        | some f => [f]
        | none => []
      else
        return sortedChildrenLeafs    
  | _ => []

def resolveTacticList (trees: List InfoTree) : List TacticFragment :=
  return (trees.map _resolveTacticList).foldl mergeSortFragments []

def configureCommandState (env : Environment) (msg : MessageLog) : Command.State := do 
  return { Command.mkState env msg with infoState := { enabled := true }}