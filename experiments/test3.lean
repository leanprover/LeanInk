/-!
# Testing LeanInk
-/

/-- A proof of the commutativity of addition of natural numbers. -/
theorem xyz : ∀ m n : Nat, m + n = n + m := by
  intro m
  intro n
  simp
  rw [← Nat.add_zero n]
  simp
  apply Nat.add_comm

-- A text comment
#check xyz -- xyz (m n : Nat) : m + n = n + m