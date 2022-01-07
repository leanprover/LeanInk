import LeanInk.Commands.Analyze.Analysis
import LeanInk.Commands.Analyze.Logger
import LeanInk.Commands.Analyze.InfoTreeUtil

import LeanInk.Output.Alectryon

namespace LeanInk.Commands.Analyze

open Output
open Lean
open Lean.Elab

namespace Token
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

  def generateTypeInfo (self : Token) (name : String) : AnalysisM (Option Alectryon.TypeInfo) :=
    match self with
      | term fragment => do
        return ← fragment.ctx.runMetaM fragment.info.lctx (generateTypeInfoAux self name)
end Token

