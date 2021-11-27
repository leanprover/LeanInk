namespace LeanInk.Commands.Analyze

namespace List

def sort [Inhabited α] (f: α -> α -> Bool) (xs : List α) : List α := (xs.toArray.qsort f).toList

def mergeSort [Inhabited α] (f: α -> α -> Bool) : List α -> List α -> List α
  | [], xs => sort f xs
  | xs, [] => sort f xs
  | x::xs, y::ys => 
    if f x y then
      return x::y::mergeSort f xs ys
    else
      return y::x::mergeSort f xs ys