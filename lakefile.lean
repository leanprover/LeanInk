import Lake
open System Lake DSL

package leanInk where
  binRoot := `LeanInk.Main
  dependencies := #[
    {
      name := `lake
      src := Source.git "https://github.com/leanprover/lake.git" "e957827c21e83101c4ce888f1752845dfdd03403"
    }
  ]
  moreLinkArgs :=
    if Platform.isWindows then
      #["-Wl,--export-all"]
    else
      #["-rdynamic"]