import Lake
open System Lake DSL

package leanInk

lean_lib LeanInk

@[defaultTarget]
lean_exe leanInk {
  root := `Main
  supportInterpreter := true
}
