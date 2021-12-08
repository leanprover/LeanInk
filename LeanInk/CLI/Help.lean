import LeanInk.CLI.Command
import LeanInk.CLI.App

namespace LeanInk.CLI

def header (app : AppInfo) : String := s!"
  {app.versionString}
  {app.description}\n\n"

def generateRows (rowEntries : List (String × String)) : List String := do
  let maxKeyLength := (rowEntries.map (λ x => x.1.length)).maximum?
  match (rowEntries.map (λ x => x.1.length)).maximum? with
  | none => []
  | some maxLength => return rowEntries.map (λ x => do 
      let extendedKey := x.1.pushn ' ' (maxLength - x.1.length)
      return s!"{extendedKey}\t{x.2}"
    )

def applyField (string title: String) (rows : List String) : String := do
  if rows.isEmpty then
    return string
  let rows := rows.foldl (λ x y => x++"\n    "++y) ""
  return s!"{string}  {title}:\n{rows}\n\n"

def applyFieldWithRowPairs (string title: String) (rows : List (String × String)) : String := do
  if rows.isEmpty then
    return string
  return applyField string title (generateRows rows)

def generateDefaultHelp (app : AppInfo) (commands : List Command) : String := do
  let result := header app
  let result := applyField result "USAGE" [s!"{app.base} <COMMAND>"]
  let result := applyFieldWithRowPairs result "COMMANDS" (commands.map (λ i => return (s!"{i.identifiers}", i.help)))
  return result

def generateCommandHelp (app : AppInfo) (command : Command) : String := do
  let result := header app
  let result := applyField result "USAGE" (command.identifiers.map (λ i => return s!"{app.base} {i} {command.additionalUsageInfo}"))
  let result := applyFieldWithRowPairs result "ARGUMENTS" (command.arguments.map (λ i => return (s!"{i.identifiers}", i.help)))
  let result := applyField result "DISCUSSION" [command.help]
  return result