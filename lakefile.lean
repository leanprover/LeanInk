import Lake
import Init.System.IO
open System Lake DSL
open System.FilePath IO IO.FS

package leanInk where
  moreLeanArgs := #["-Dtactic.simp.trace=true"]
  moreServerArgs := #["-Dtactic.simp.trace=true"]

lean_lib LeanInk where
  moreLeanArgs := #["-Dtactic.simp.trace=true"]

@[default_target]
lean_exe leanInk where
  root := `LeanInk
  supportInterpreter := true
  moreLeanArgs := #["-Dtactic.simp.trace=true"]