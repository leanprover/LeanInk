namespace LeanInk.Version

def leanVersion : String :=
  let versionString := s!"{Lean.version.major}.{Lean.version.minor}.{Lean.version.patch}"
  if Lean.version.isRelease then
    versionString
  else if !String.isEmpty Lean.version.specialDesc then
    versionString ++ s!"-" ++ Lean.version.specialDesc
  else
    versionString ++ s!"-unknown"

def printLeanVersion : IO UInt32 := do
  IO.println leanVersion
  return 0