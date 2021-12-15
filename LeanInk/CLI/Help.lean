import LeanInk.CLI.Command
import LeanInk.CLI.App

namespace LeanInk.CLI

def header (app : AppInfo) : String := s!"
  {app.versionString}
  {app.description}\n\n"

def generateRows (rowEntries : List (String × String)) : List String :=
  let maxKeyLength := (rowEntries.map (λ x => x.1.length)).maximum?
  match (rowEntries.map (λ x => x.1.length)).maximum? with
  | none => []
  | some maxLength => rowEntries.map (λ x =>
      let extendedKey := x.1.pushn ' ' (maxLength - x.1.length)
      s!"{extendedKey}\t{x.2}"
    )

def applyField (string title: String) (rows : List String) : String :=
  if rows.isEmpty then
    string
  else
    let rows := rows.foldl (λ x y => x++"\n    "++y) ""
    s!"{string}  {title}:\n{rows}\n\n"

def applyFieldWithRowPairs (string title: String) (rows : List (String × String)) : String :=
  if rows.isEmpty then
    string
  else
    applyField string title (generateRows rows)

def generateDefaultHelp (app : AppInfo) (commands : List Command) : String :=
  let result := header app
  let result := applyField result "USAGE" [s!"{app.base} <COMMAND>"]
  let result := applyFieldWithRowPairs result "COMMANDS" (commands.map (λ i => (s!"{i.identifiers}", i.help)))
  result

def generateCommandHelp (app : AppInfo) (command : Command) : String :=
  let result := header app
  let result := applyField result "USAGE" (command.identifiers.map (λ i => s!"{app.base} {i} {command.additionalUsageInfo}"))
  let result := applyFieldWithRowPairs result "ARGUMENTS" (command.arguments.map (λ i => (s!"{i.identifiers}", i.help)))
  let result := applyField result "DISCUSSION" [command.help]
  result