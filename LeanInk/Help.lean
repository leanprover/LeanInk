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
      g, generate          Generates LeanInk and Alectryon annotated files. 
      a, analyze           Analyzes the Lean code files and returns the result of its analysis.
      v, version           Prints the version number of LeanInk and it's supported Lean4 version
      l, licenses          Prints all licenses of third-party code used by LeanInk and it's own license.
      h, help <COMMAND>    Shows this message if no command provided or help for a specific command.

  DISCUSSION: TODO
"