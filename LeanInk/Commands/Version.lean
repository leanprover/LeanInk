namespace LeanInk.Commands.Version

def version := s!"0.0.1"
def leanVersion := s!"leanprover/lean4:nightly-2021-11-02"
def cliVersionOutput := s!"LeanInk ({version}) for Lean ({leanVersion})"

def printVersion : IO UInt32 := do
  IO.println cliVersionOutput
  return 0

def printLeanVersion : IO UInt32 := do
  IO.println leanVersion
  return 0