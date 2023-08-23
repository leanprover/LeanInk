import Lake
open Lake DSL

package dep

require mathlib from git "https://github.com/leanprover-community/mathlib4.git" @ "cf479f4ee9e6e5e54f8b3ddea0011c3f816350d4"

@[default_target]
lean_lib dep
