import LeanInk.CLI.Logger

import LeanInk.Commands.Analyze.Configuration

namespace LeanInk.Commands.Analyze.Logger

-- Prints a message if in debug mode, otherwise does nothing
def log [ToString a] (s : a) : AnalysisM Unit := do
  CLI.Logger.log s (← read).verbose

def logInfo [ToString a] (s : a) (isDebug: Bool := false) : AnalysisM Unit := do
  CLI.Logger.logInfo s (← read).verbose

-- Prints a warning message if in debug mode, otherwise does nothing
def logWarning [ToString a] (s : a) (isDebug: Bool := false) : AnalysisM Unit := do
  CLI.Logger.logWarning s (← read).verbose