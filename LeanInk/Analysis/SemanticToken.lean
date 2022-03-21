import LeanInk.Analysis.DataTypes
import LeanInk.Configuration
import LeanInk.ListUtil

import Lean.Elab
import Lean.Syntax
import Lean.Server

namespace LeanInk.Analysis

open Lean
open Lean.Elab
open Lean.Server

structure SemanticTraversalInfo where
  stx : Syntax
  node : Option Info

namespace SemanticTraversalInfo
  def genSemanticToken (stx : Syntax) (type : SemanticTokenType) : List Token :=
    let headPos := (stx.getPos? false).getD 0
    let tailPos := (stx.getTailPos? false).getD 0
    if headPos >= tailPos then
      []
    else
      [Token.semantic { semanticType := type, headPos := headPos, tailPos := tailPos }]

  def highlightIdentifier (globalTailPos : String.Pos) (info : SemanticTraversalInfo) : AnalysisM (List Token) := do
    match info.node with
    | some node => do
      if Info.isExpanded node then
        return []
      match info.stx.getRange?, info.node with
      | some range, Info.ofTermInfo info => do
        let genToken := genSemanticToken info.stx
        match info.expr with
        | Expr.fvar .. => return genToken SemanticTokenType.variable
        | _ =>
          match info.stx.getPos? with
          | some pos => 
            if pos > globalTailPos then
              return genToken SemanticTokenType.property
            else
              return []
          | _ => pure []  
      | _, _ => pure []
    | _ => pure []

  def highlightKeyword (headPos tailPos : String.Pos) (stx: Syntax) : AnalysisM (List Token) := do
    if let Syntax.atom info val := stx then
      if (val.length > 0 && val[0].isAlpha) || (val.length > 1 && val[0] = '#' && val[⟨1⟩].isAlpha) then
        return genSemanticToken stx SemanticTokenType.keyword
    return []

  partial def _resolveSemanticTokens (aux : List Token) (info : SemanticTraversalInfo) : AnalysisM (List Token) := do
    let stx := info.stx
    let headPos := (stx.getPos? false).getD 0
    let tailPos := (stx.getTailPos? false).getD 0
    if headPos >= tailPos then
      return []
    else
      match stx with
      | `($e.$id:ident) => do
        let newToken := genSemanticToken id SemanticTokenType.property
        _resolveSemanticTokens (aux ++ newToken) { info with stx := e }
      | `($id:ident) => highlightIdentifier tailPos { info with stx := id }
      | _ => do
        if !(FileWorker.noHighlightKinds.contains stx.getKind) then
          let token ← highlightKeyword headPos tailPos stx
          if stx.isOfKind choiceKind then
            _resolveSemanticTokens (aux ++ token) { info with stx := stx[0] }
          else
            let resolvedTokens := (← stx.getArgs.mapM (λ newStx => _resolveSemanticTokens [] { info with stx := newStx })).toList
            return resolvedTokens.foldl (List.mergeSortedLists (λ x y => Positional.headPos x < Positional.headPos y)) (aux ++ token)
        else
          return []
end SemanticTraversalInfo