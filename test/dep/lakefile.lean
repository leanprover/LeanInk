import Lake
open Lake DSL

package dep

require mathlib from git "https://github.com/leanprover-community/mathlib4.git" @ "a6e1045fe156c34fddec5661e23432c879c43e5c"

@[default_target]
lean_lib dep
