import LeanInk
import LeanInk.Version

def printVersion : IO UInt32 := do
  IO.println LeanInk.cliVersionOutput
  return 1

def main (args : List String) : IO UInt32 := do
  match args with
  | ["--version"] => printVersion
  | _ => 
      IO.println s!"Hello from LeanInk!"
      return 1