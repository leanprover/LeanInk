namespace LeanInk.Commands.Analyze

namespace List

def sort [Inhabited α] (f: α -> α -> Bool) (xs : List α) : List α := (xs.toArray.qsort f).toList

partial def mergeSort [Inhabited α] (f: α -> α -> Bool) : List α -> List α -> List α
  | [], xs => xs
  | xs, [] => xs
  | x::xs, y::ys => 
    if f x y then
      x::mergeSort f xs (y::ys)
    else
      y::mergeSort f (x::xs) ys