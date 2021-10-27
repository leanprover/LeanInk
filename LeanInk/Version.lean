namespace LeanInk

namespace Version

def version := s!"0.0.1-alpha"
def leanVersion := s!"leanprover/lean4:nightly-2021-10-27"
def cliVersionOutput := s!"LeanInk ({version}) for Lean ({leanVersion})"

def printVersion : IO PUnit := do
  IO.println cliVersionOutput

end Version