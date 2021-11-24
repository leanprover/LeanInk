import Lake
open System Lake DSL

package leanInk where
  binRoot := `LeanInk.Main
  moreLinkArgs :=
    if Platform.isWindows then
      #["-Wl,--export-all"]
    else
      #["-rdynamic"]