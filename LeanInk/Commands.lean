import LeanInk.CLI
import LeanInk.Analysis

open LeanInk
open LeanInk.CLI
open LeanInk.CLI.Argument
open LeanInk.CLI.Command

namespace LeanInk.Commands

-- ANALYZE COMNAND
def analyzeCommand : Command := {
  identifiers := ["analyze", "a"]
  help := "Analyzes the given input file and outputs the results in Alectryons fragment json format."
  additionalUsageInfo := "<INPUT_FILE>"
  arguments := [
    environment {
      identifiers := ["--lake"]
      help := "Specify path to lakefile.lean, so dependencies can be resolved during analysis. Default: none"
    },
    flag {
      identifiers := ["--verbose"]
      help := "Enables verbose output."
    },
    flag {
      identifiers := ["--prettify-output"]
      help := "Prettifies the .leanInk output file"
    },
    flag {
      identifiers := ["--x-enable-type-info"]
      help := "Enables output of experimental type info support for Alectryon. Alectryon will show a hover popup with type information for certain tokens."
    },
    flag {
      identifiers := ["--x-enable-docStrings"]
      help := "Enables output of experimental docStrings support for Alectryon. Alectryon will show a hover popup with the docString for tactics and terms if available."
    },
    flag {
      identifiers := ["--x-enable-semantic-token"],
      help := "Enables output of experimental semantic token support for Alectryon. Alectryon uses this information to implement semantic syntax highlighting."
    }
  ]
  run := Analysis.exec
}

end LeanInk.Commands