import LeanInk.Commands.Analyze.Analysis
import LeanInk.Commands.Analyze.Logger
import LeanInk.Commands.Analyze.InfoTreeUtil
import LeanInk.Commands.Analyze.Util

import LeanInk.Output.Alectryon

namespace LeanInk.Commands.Analyze

open Output
open Lean
open Lean.Elab

namespace Token
  def length (self : Token) : Nat := self.tailPos - self.headPos

  def inferType : Token -> MetaM String
    | term termFragment => do
      let format ← Meta.ppExpr (← Meta.inferType termFragment.info.expr)
      return s!"{format}"

  partial def generateDocString? (self : Token) : MetaM (Option String) := do
    let env ← getEnv
    match self with
      | term termFragment =>
        if let some name := termFragment.info.expr.constName? then
          return ← findDocString? env name
        else
          return none

  private def generateTypeInfoAux (self : Token) (name : String) : MetaM (Option Alectryon.TypeInfo) :=
    return some { name :=  name, type := (← inferType self), docstring := (← generateDocString? self) }

  def getSmallestToken? (compound : Compound Token) : Option Token := List.foldl (λ x y => 
    match x, y with -- We make a match over both, otherwise Lean thinks y : Option Token instead of y : Token
    | none, y => y
    | some x, y => if x.length < y.length then x else y
  ) none compound.getFragments

  def generateTypeInfo (self : Compound Token) (name : String) : AnalysisM (Option Alectryon.TypeInfo) :=
    match getSmallestToken? self with -- Select the most useful token
      | none => none
      | some (term fragment) => do
        return ← fragment.ctx.runMetaM fragment.info.lctx (generateTypeInfoAux (Token.term fragment) name)
end Token

