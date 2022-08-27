import Lake
open Lake DSL

package dep

require mathlib from git "https://github.com/leanprover-community/mathlib4.git" @ "362fc2dcb0b9136ac644caa493ed70ab15a5cfa8"

@[defaultTarget]
lean_lib dep
