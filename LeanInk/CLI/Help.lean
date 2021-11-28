import LeanInk.Commands.Version
import LeanInk.CLI.Command

namespace LeanInk.Commands.Help

def header := s!"  {Version.cliVersionOutput}
  LeanInk is a code analysis tool for Lean 4 that extracts proof tactic information. It's main goal is to ease the support for Lean 4 in Alectryon.
"

def generateHelp (command: Command)

def generalHelp := s!"{header}
  USAGE:
      leanInk <COMMAND> [FlAGS]

  FLAGS:
      -v, --verbose        Enable debug/verbose output for most LeanInk commands.
  
  COMMANDS:
      a, analyze           Analyzes the Lean code files and returns the result of its analysis.
      v, version           Prints the version number of LeanInk and it's supported Lean4 version
      h, help <COMMAND>    Shows this message if no command provided or help for a specific command.

  DISCUSSION: TODO
"

def analyzeHelp := s!"{header}
  USAGE:
      leanInk a <LEAN_CODE_FILES> [FlAGS]
      leanInk analyze <LEAN_CODE_FILES> [FlAGS]

  FLAGS:
      -v, --verbose        Enable debug/verbose output for most LeanInk commands.
  
  LEAN_CODE_FILES: TODO
  DISCUSSION: TODO
"

def versionHelp := s!"{header}
  USAGE:
      leanInk v
      leanInk version
      leanInk --version
  
  DISCUSSION: Prints the version number of LeanInk and it's supported Lean4 version.
"

def leanVersionHelp := s!"{header}
  USAGE:
      leanInk lV
      leanInk leanVersion
      leanInk --leanVersion
  
  DISCUSSION: Prints the version of the supported Lean4 version of this LeanInk instance. This is also used for propagating the LeanVersion to Alectryon.
"

def helpHelp := s!"{header}
  USAGE:
      leanInk h [COMMAND]
      leanInk help [COMMAND]

  COMMANDS:
      a, analyze
      v, version
      lV, leanVersion
  
  DISCUSSION: Prints the help message for the specified command or the general help message.
"