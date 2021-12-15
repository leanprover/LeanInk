namespace LeanInk.Commands.Version

def leanVersion : String :=
  let versionString := s!"{Lean.version.major}.{Lean.version.minor}.{Lean.version.patch}"
  if Lean.version.isRelease then
    versionString
  else if !String.isEmpty Lean.version.specialDesc then
    versionString ++ s!"-" ++ Lean.version.specialDesc
  else
    versionString ++ s!"-unknown"

def version := s!"0.0.1"
def cliVersionOutput := s!"LeanInk ({version}) for Lean ({leanVersion})"

def printVersion : IO UInt32 := do
  IO.println cliVersionOutput
  return 0

def printLeanVersion : IO UInt32 := do
  IO.println leanVersion
  return 0

