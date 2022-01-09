import LeanInk.Annotation.Util

import LeanInk.Analysis.Analysis
import LeanInk.Analysis.InfoTreeUtil

import LeanInk.Logger

import Lean.Data.Json
import Lean.Data.Json.FromToJson
import Lean.Meta
import Lean.Elab

open Lean
open Lean.Elab
open Lean.Meta
open LeanInk.Analysis

namespace LeanInk.Annotation.Alectryon

structure TypeInfo where
  _type : String := "typeinfo"
  name : String
  type : String
  deriving ToJson

structure Token where
  _type : String := "token"
  raw : String
  typeinfo : Option TypeInfo := Option.none
  link : Option String := Option.none
  docstring : Option String := Option.none
  deriving ToJson

/--
  Support type for experimental --experimental-type-tokens feature
  If flag not set please only use the string case.
-/
inductive Contents where
  | string (value : String)
  | experimentalTokens (value : Array Token)

instance : ToJson Contents where
  toJson
    | Contents.string v => toJson v
    | Contents.experimentalTokens v => toJson v

structure Hypothesis where
  _type : String := "hypothesis"
  names : List String
  body : String
  type : String
  deriving ToJson

structure Goal where
  _type : String := "goal"
  name : String
  conclusion : String
  hypotheses : Array Hypothesis
  deriving ToJson

structure Message where
  _type : String := "message"  
  contents : String
  deriving ToJson

structure Sentence where
  _type : String := "sentence"
  contents : Contents
  messages : Array Message
  goals : Array Goal
  deriving ToJson

structure Text where
  _type : String := "text"
  contents : Contents
  deriving ToJson

/-- 
We need a custom ToJson implementation for Alectryons fragments.

For example we have following fragment:
```
Fragment.text { contents := "Test" }
```

We want to serialize this to:
```
[{"contents": "Test", "_type": "text"}]
```

instead of:
```
[{"text": {"contents": "Test", "_type": "text"}}]
```

This is because of the way Alectryon decodes the json files. It uses the _type field to
determine the namedTuple type with Alectryon.
-/
inductive Fragment where
  | text (value : Text)
  | sentence (value : Sentence)

instance : ToJson Fragment where
  toJson
    | Fragment.text v => toJson v
    | Fragment.sentence v => toJson v

end LeanInk.Annotation.Alectryon

/- Output generation -/
open LeanInk.Annotation

namespace LeanInk.Analysis
namespace Token
  def length (self : Token) : Nat := self.tailPos - self.headPos

  def inferType : Token -> MetaM String
    | term termFragment => do
      let format ← Meta.ppExpr (← Meta.inferType termFragment.info.expr)
      return s!"{format}"
    | _ => ""

  partial def auxGenerateDocString? (self : Token) : MetaM (Option String) := do
    let env ← getEnv
    match self with
      | term termFragment =>
        if let some name := termFragment.info.expr.constName? then
          return ← findDocString? env name
        else
          return none
      | field fieldFragment => findDocString? env fieldFragment.info.projName
      | tactic fragment =>
        let elabInfo := fragment.info.toElabInfo
        return ← findDocString? env elabInfo.elaborator <||> findDocString? env elabInfo.stx.getKind

  private def generateTypeInfoAux (self : Token) (name : String) : MetaM (Option Alectryon.TypeInfo) :=
    return some { name :=  name, type := (← inferType self) }

  def getSmallestToken? (compound : Compound Token) : Option Token := List.foldl (λ x y => 
    match x, y with -- We make a match over both, otherwise Lean thinks y : Option Token instead of y : Token
    | none, y => y
    | some x, y => if x.length < y.length then x else y
  ) none compound.getFragments

  def generateDocString? (self : Compound Token) : AnalysisM (Option String) :=
    match getSmallestToken? self with -- Select the most useful token
      | none => none
      | some (term fragment) => do
        return ← fragment.ctx.runMetaM fragment.info.lctx (auxGenerateDocString? (Token.term fragment))
      | some (tactic fragment) => do
        return ← fragment.ctx.runMetaM {} (auxGenerateDocString? (Token.tactic fragment))
      | some (field fragment) => do
        return ← fragment.ctx.runMetaM fragment.info.lctx (auxGenerateDocString? (Token.field fragment)) 

  def generateTypeInfo (self : Compound Token) (name : String) : AnalysisM (Option Alectryon.TypeInfo) :=
    match getSmallestToken? self with -- Select the most useful token
      | none => none
      | some (term fragment) => do
        return ← fragment.ctx.runMetaM fragment.info.lctx (generateTypeInfoAux (Token.term fragment) name)
      | some (tactic fragment) => do
        return ← fragment.ctx.runMetaM {} (generateTypeInfoAux (Token.tactic fragment) name)
      | some (field fragment) => do
        return ← fragment.ctx.runMetaM fragment.info.lctx (generateTypeInfoAux (Token.field fragment) name)
end Token

namespace TacticFragment
private def buildGoal (goalType : Format) (hypotheses : List Alectryon.Hypothesis): Name -> MetaM (Alectryon.Goal)
    | Name.anonymous => do
      return { name := "", conclusion := s!"{goalType}", hypotheses := hypotheses.toArray }
    | name => do
      let goalFormatName := format name.eraseMacroScopes
      let goalName := s!"{goalFormatName}"
      return { name := goalName, conclusion := s!"{goalType}", hypotheses := hypotheses.toArray }

  /-- 
  This method is a adjusted version of the Meta.ppGoal function. As we do need to extract the goal informations into seperate properties instead
  of a single formatted string to support the Alectryon.Goal datatype.
  -/
  private def evalGoal (mvarId : MVarId) : MetaM (Option Alectryon.Goal) := do
    match (← getMCtx).findDecl? mvarId with
    | none => return none
    | some decl => do
      let auxDecl := pp.auxDecls.get (<- getOptions)
      let lctx := decl.lctx.sanitizeNames.run' { options := (← getOptions) }
      withLCtx lctx decl.localInstances do
        let (hidden, hiddenProp) ← ToHide.collect decl.type
        let pushPending (list : List Alectryon.Hypothesis) (type? : Option Expr) : List Name -> MetaM (List Alectryon.Hypothesis)
        | [] => pure list
        | ids => do
          match type? with
            | none      => pure list
            | some type => do
              let typeFmt ← ppExpr type
              let names ← ids.reverse.map (λ n => n.toString)
              return list.append [{ names := names, body := "", type := s!"{typeFmt}" }]
        let evalVar (varNames : List Name) (prevType? : Option Expr) (hypotheses : List Alectryon.Hypothesis) (localDecl : LocalDecl) : MetaM (List Name × Option Expr × (List Alectryon.Hypothesis)) := do
          if hiddenProp.contains localDecl.fvarId then
            let newHypotheses ← pushPending [] prevType? varNames
            let type ← instantiateMVars localDecl.type
            let typeFmt ← ppExpr type
            let newHypotheses := newHypotheses.map (λ h => { h with type := h.type ++ s!" : {typeFmt}"})
            pure ([], none, hypotheses.append newHypotheses)
          else
            match localDecl with
            | LocalDecl.cdecl _ _ varName type _ =>
              let varName := varName.simpMacroScopes
              let type ← instantiateMVars type
              if prevType? == none || prevType? == some type then
                pure (varName::varNames, some type, hypotheses)
              else do
                let hypotheses ← pushPending hypotheses prevType? varNames
                pure ([varName], some type, hypotheses)
            | LocalDecl.ldecl _ _ varName type val _ => do
              let varName := varName.simpMacroScopes
              let hypotheses ← pushPending hypotheses prevType? varNames
              let type ← instantiateMVars type
              let val  ← instantiateMVars val
              let typeFmt ← ppExpr type
              let valFmt ← ppExpr val
              pure ([], none, hypotheses.append [{ names := [varName.toString], body := s!"{valFmt}", type := s!"{typeFmt}" }])
        let (varNames, type?, hypotheses) ← lctx.foldlM (init := ([], none, [])) λ (varNames, prevType?, hypotheses) (localDecl : LocalDecl) =>
          if !auxDecl && localDecl.isAuxDecl || hidden.contains localDecl.fvarId then
            pure (varNames, prevType?, hypotheses)
          else
            evalVar varNames prevType? hypotheses localDecl
        let hypotheses ← pushPending hypotheses type? varNames 
        let typeFmt ← ppExpr (← instantiateMVars decl.type)
        return (← buildGoal typeFmt hypotheses decl.userName)

  private def resolveGoalsAux (ctx : ContextInfo) (mctx : MetavarContext) : List MVarId -> IO (List Alectryon.Goal)
    | [] => []
    | goals => do
      let ctx := { ctx with mctx := mctx }
      return (← ctx.runMetaM {} (goals.mapM (evalGoal .))).filterMap (λ x => x)

  def resolveGoals (self : TacticFragment) : IO (List Alectryon.Goal) := do
    return ← resolveGoalsAux self.ctx self.info.mctxAfter self.info.goalsAfter
end TacticFragment

namespace MessageFragment
  def toAlectryonMessage (self : MessageFragment) : IO Alectryon.Message := do
    let message ← self.msg.toString
    return { contents := message }
end MessageFragment
end LeanInk.Analysis