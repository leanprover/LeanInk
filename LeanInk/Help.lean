import LeanInk.GlobalArgument
import LeanInk.Version

namespace LeanInk
namespace Help

def generalHelp := s!"  {Version.cliVersionOutput}
  LeanInk is a tool for Lean which generates static code visualization through the integration of Alectryon.

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

def analyzeHelp := s!"  {Version.cliVersionOutput}
  LeanInk is a tool for Lean which generates static code visualization through the integration of Alectryon.

  USAGE:
      leanInk a <LEAN_CODE_FILES> [FlAGS]
      leanInk analyze <LEAN_CODE_FILES> [FlAGS]

  FLAGS:
      -v, --verbose        Enable debug/verbose output for most LeanInk commands.
  
  LEAN_CODE_FILES: TODO
  DISCUSSION: TODO
"

def versionHelp := s!"  {Version.cliVersionOutput}
  LeanInk is a tool for Lean which generates static code visualization through the integration of Alectryon.

  USAGE:
      leanInk v
      leanInk version
  
  DISCUSSION: Prints the version number of LeanInk and it's supported Lean4 version.
"

def helpHelp := s!"  {Version.cliVersionOutput}
  LeanInk is a tool for Lean which generates static code visualization through the integration of Alectryon.

  USAGE:
      leanInk h [COMMAND]
      leanInk help [COMMAND]

  COMMANDS:
      g, generate          Generates LeanInk and Alectryon annotated files. 
      a, analyze           Analyzes the Lean code files and returns the result of its analysis.
      v, version           Prints the version number of LeanInk and it's supported Lean4 version
      l, licenses          Prints all licenses of third-party code used by LeanInk and it's own license.
  
  DISCUSSION: Prints the help message for the specified command or the general help message.
"