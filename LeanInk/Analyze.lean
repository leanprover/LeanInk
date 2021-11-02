import LeanInk.GlobalArgument

namespace LeanInk
namespace Analyze

def exec (globalArgs: List GlobalArgument) (args: List String) : IO UInt32 := do
  IO.println s!"Execute analyze"
  return 0