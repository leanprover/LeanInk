namespace LeanInk

namespace List

def sort [Inhabited α] (f: α -> α -> Bool) (xs : List α) : List α := (xs.toArray.qsort f).toList

partial def mergeSortedLists [Inhabited α] (f: α -> α -> Bool) : List α -> List α -> List α
  | [], xs => xs
  | xs, [] => xs
  | x::xs, y::ys => 
    if f x y then
      x::mergeSortedLists f xs (y::ys)
    else
      y::mergeSortedLists f (x::xs) ys