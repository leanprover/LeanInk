import Lake
open Lake DSL

package dep

require mathlib from git "https://github.com/leanprover-community/mathlib4.git" @ "408fbe22495657efd034398efa252f3f2b33add1"

@[default_target]
lean_lib dep
