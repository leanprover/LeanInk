import Lake
open Lake DSL

package dep

require mathlib from git "https://github.com/leanprover-community/mathlib4.git" @ "1cd8b316f3e3e2f1e307b4e38e2b304ae5e596bd"

@[default_target]
lean_lib dep
