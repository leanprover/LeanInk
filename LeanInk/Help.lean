import LeanInk.GlobalArgument
import LeanInk.Version

namespace LeanInk
namespace Help

def defaultHelp := s!"  {Version.cliVersionOutput}
  LeanInk is a tool for Lean which generates static code visualization through the integration of Alectryon.

  USAGE:
      leanInk <COMMAND> [FlAGS]

  FLAGS:
      -v, --verbose                 Enable debug/verbose output for most LeanInk commands.
      -V, --version                 Prints the version number of LeanInk and it's supported Lean4 version
  
  COMMANDS:
      generate <INPUT_FILES...>     Generates LeanInk and Alectryon annotated files, e.g.: HTML website ('.htm', '.html'), 
                                    Restructured Text document ('.rst') or Markdown document ('.md') or just Lean4 code ('.lean'). 
                                    LeanInk attempts to find out the type of the specified file based on its file extension.
      analyze <LEAN_CODE_FILES...>  Analyzes the Lean code files and returns the result of its analysis. (TODO: result type not yet defined)
      licenses                      Prints all licenses of third-party code used by LeanInk and it's own license.
      help <COMMAND>                Shows this message if no command provided or help for a specific command.

  DISCUSSION: TODO
"

def execute (globalArgs: List GlobalArgument) (args: List String) : IO UInt32 := do
  IO.println defaultHelp
  return 0
