import LeanInk.CLI.Command
import LeanInk.CLI.App

namespace LeanInk.CLI

def header (app : AppInfo) : String := s!"
  {app.versionString}
  {app.description} 
"

def generateHelp (app : AppInfo) (command : Command) : String := do
  let usageList : List String := command.identifiers.map (λ i => return s!"    {app.base} {i}\n")
  let usages := usageList.foldl (λ x y => x++y) ""
  return s!"{header app}
  USAGE:
{usages}

  COMMANDS:
  TODO:

  ARGUMENTS:
  TODO

  DISCUSSION: {command.help}
"