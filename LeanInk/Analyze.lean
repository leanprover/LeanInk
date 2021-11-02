import LeanInk.GlobalArgument
import LeanInk.Logger

namespace LeanInk
namespace Analyze

def exec (globalArgs: List GlobalArgument) (args: List String) : IO UInt32 := do
  Logger.logInfo s!"Executing analysis for {args}"
  return 0