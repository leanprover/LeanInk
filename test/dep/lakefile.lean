import Lake
open Lake DSL

package dep

require mathlib from git "https://github.com/leanprover-community/mathlib4.git" @ "932071475e7e89428d81af9d7b2fab6c0b88bf3c"

@[default_target]
lean_lib dep
