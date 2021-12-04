import Lake
open Lake DSL

package dep {
  defaultFacet := PackageFacet.oleans
  dependencies := #[{
    name := `mathlib
    src := Source.git "https://github.com/leanprover-community/mathlib4.git" "master"
  }]
}
