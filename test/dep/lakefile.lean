import Lake
open Lake DSL

package dep

require mathlib from git "https://github.com/leanprover-community/mathlib4.git" @ "01da6a3c22053b87a02f5a4a7d78c10d6cb25160"

@[default_target]
lean_lib dep
