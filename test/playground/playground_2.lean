/-| 
Hello World!
-/
#print "Hello World!"

/-|
A literate comment!
-/

theorem exampleTheorem (p q : Prop) (hp : p) (hq : q) : p ∧ q ∧ p := by
  apply And.intro
  . exact hp
  . sorry