namespace LeanInk

namespace List

partial def mergeSortedLists (f: α -> α -> Bool) : List α -> List α -> List α
  | [], xs => xs
  | xs, [] => xs
  | x::xs, y::ys => 
    if f x y then
      x::mergeSortedLists f xs (y::ys)
    else
      y::mergeSortedLists f (x::xs) ys

def mergeSort (f: α -> α -> Bool) : List α -> List α
  | [] => []
  | [x] => [x]
  | [x, y] =>  if f x y then [x, y] else [y, x]
  | xs => mergeSortedLists f (xs.take (split xs)) (xs.drop (split xs))
    where
      split (xs : List α) := (List.length xs) / 2

def sort [Inhabited α] (f: α -> α -> Bool) (xs : List α) : List α := (xs.toArray.qsort f).toList