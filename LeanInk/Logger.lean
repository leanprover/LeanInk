open Lean

namespace LeanInk
namespace Logger

-- Prints a message if in debug mode, otherwise does nothing
def log [ToString a] (s : a) (isDebug: Bool := false) : IO Unit := do
  match isDebug with
  | true => IO.println s
  | false => return

-- Prints a message if in debug mode, otherwise does nothing
def logInfo [ToString a] (s : a) (isDebug: Bool := false) : IO Unit := do
  log s!"INFO: {s}" isDebug

-- Prints a warning message if in debug mode, otherwise does nothing
def logWarning [ToString a] (s : a) (isDebug: Bool := false) : IO Unit := do
  log s!"WARNING: {s}" isDebug

-- Prints an error message
def logError [ToString a] (s : a) (errorCode : UInt32 := 1) : IO UInt32 := do
  IO.println s!"ERROR({errorCode}): {s}"
  return errorCode

end Logger

-- Prints a message if in debug mode, otherwise does nothing
def log [ToString a] (s : a) : IO Unit := do
  Logger.log s (isDebug := true)

def logInfo [ToString a] (s : a) (isDebug: Bool := false) : IO Unit := do
  Logger.logInfo s isDebug

-- Prints a warning message if in debug mode, otherwise does nothing
def logWarning [ToString a] (s : a) (isDebug: Bool := false) : IO Unit := do
  Logger.logWarning s isDebug

-- Prints an error message
def logError [ToString a] (s : a) (errorCode : UInt32 := 1) : IO UInt32 := do
  Logger.logError s errorCode
