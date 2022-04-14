import Lake
open System Lake DSL

package leanInk where
  moreLinkArgs :=
    if Platform.isWindows then
      #[]
    else
      #["-rdynamic"]