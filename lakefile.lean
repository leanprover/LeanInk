import Lake
open System Lake DSL

package leanInk where
  binRoot := `LeanInk.Main
  dependencies := #[
    {
      name := `lake
      src := Source.git "https://github.com/leanprover/lake.git" "ba9005829cf53e77cb241fc6fb86a2d4e8d8219c"
    }
  ]
  moreLinkArgs :=
    if Platform.isWindows then
      #["-Wl,--export-all"]
    else
      #["-rdynamic"]