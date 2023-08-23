import Lake
open Lake DSL

package dep

require mathlib from git "https://github.com/leanprover-community/mathlib4.git" @ "3d5d1404a27f4b285302a1589e1da2672590da34"

@[default_target]
lean_lib dep
