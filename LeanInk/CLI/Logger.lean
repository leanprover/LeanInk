namespace LeanInk.CLI.Logger

open Lean

-- Prints a message if in debug mode, otherwise does nothing
def log [ToString a] (s: a) (isDebug: Bool := true) : IO Unit := do
  match isDebug with
  | true => IO.println s
  | false => IO.print s!""

-- Prints a message if in debug mode, otherwise does nothing
def logInfo [ToString a] (s: a) : IO Unit := do
  log s!"INFO: {s}" true

-- Prints a warning message if in debug mode, otherwise does nothing
def logWarning [ToString a] (s: a) (isDebug: Bool := true) : IO Unit := do
  log s!"WARNING: {s}" isDebug

-- Prints an error message
def logError [ToString a] (s: a) (errorCode: UInt32 := 1) : IO UInt32 := do
  IO.println s!"ERROR({errorCode}): {s}"
  return errorCode