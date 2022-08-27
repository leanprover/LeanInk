import Lake
open System Lake DSL

package leanInk where
  moreLinkArgs :=
    if Platform.isWindows then
      #[]
    else
      #["-rdynamic"]
      
      
lean_lib LeanInk {
  -- add library configuration options here
}

@[defaultTarget]
lean_exe leanInk {
  root := `Main
}
