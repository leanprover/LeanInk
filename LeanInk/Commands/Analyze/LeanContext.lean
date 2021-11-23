import Lean.Util.Path

namespace LeanInk.Commands.Analyze

open System

def initializeLeanContext : IO Unit := do
  let leanPath ← Lean.findSysroot?
  Lean.initSearchPath leanPath