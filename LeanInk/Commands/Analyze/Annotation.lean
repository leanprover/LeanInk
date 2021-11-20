import LeanInk.Commands.Analyze.InfoTreeUtil

namespace LeanInk.Commands.Analyze

structure AnnotationInfo where
  head: String.Pos

structure CompoundInfo extends AnnotationInfo where
  fragments: List AnalysisFragment

inductive Annotation where
  | compound (i: CompoundInfo)
  | text (i: AnnotationInfo)

namespace Annotation
  def toString : Annotation -> String
  | compound i => s!"{i.head} @ compound <> {i.fragments.toArray.size}"
  | text i => s!"{i.head} @ text"
end Annotation

structure AnnotationIntervalInfo where
  head: String.Pos
  tail: String.Pos
  maxChildTail: String.Pos
  fragment: AnalysisFragment

inductive AnnotationIntervalTree where
  | node (i: AnnotationIntervalInfo) (left: Option AnnotationIntervalTree) (right: Option AnnotationIntervalTree)

namespace AnnotationIntervalTree

def getInfo : AnnotationIntervalTree -> AnnotationIntervalInfo
  | node i _ _ => i

def getLeft? : AnnotationIntervalTree -> Option AnnotationIntervalTree
  | node _ left _ => left

def getRight? : AnnotationIntervalTree -> Option AnnotationIntervalTree
  | node _ _ right => right

def Fragment.createInfo (f: AnalysisFragment) (maxTail: Option String.Pos := none) : AnnotationIntervalInfo := do
  match maxTail with
  | some maxTail => return { head := f.headPos, tail :=  f.tailPos, maxChildTail := maxTail, fragment := f }
  | none => return { head := f.headPos, tail :=  f.tailPos, maxChildTail := f.tailPos, fragment := f }

def max (a b: Option AnnotationIntervalTree) : Option String.Pos := do
  match a, b with
  | none, none => none
  | some (node ia _ _) , none => some ia.maxChildTail
  | none , some (node ib _ _) => some ib.maxChildTail
  | some (node ia _ _) , some (node ib _ _) =>
    if ia.maxChildTail > ib.maxChildTail then
      return some ia.maxChildTail 
    else 
      return some ib.maxChildTail

  def contains (tree: AnnotationIntervalTree) (pos: String.Pos) : Bool := do
    let info := tree.getInfo
    return info.head <= pos && info.tail >= pos

-- We expect a sorted list, based on the head position of each fragment.
partial def create : List AnalysisFragment -> Option AnnotationIntervalTree
  | [] => none
  | x::[] => node (Fragment.createInfo x) none none
  | xs => do
    let fragments := xs.toArray
    let centerIndex := fragments.size / 2
    let centerFragment := fragments[centerIndex]
    let left := create (xs.take (centerIndex - 1))
    let right := create (xs.drop centerIndex)
    let maxValue := max left right
    return node (Fragment.createInfo centerFragment maxValue) left right

partial def queryAt (tree: AnnotationIntervalTree) (pos: String.Pos) : List AnalysisFragment := do
  if pos > tree.getInfo.maxChildTail then -- There are no fragments beyond the roots maxChildTail value
    return []
  let leftFragments := _queryAt? pos tree.getLeft?
  let containsCurrent := [tree].filter (λ t => t.contains pos)
  let result := leftFragments.append (containsCurrent.map (λ t => t.getInfo.fragment))
  if pos < tree.getInfo.head then
    return result
  else
    return result.append (_queryAt? pos tree.getRight?)
  where
    _queryAt? (pos: String.Pos) : (tree?: Option AnnotationIntervalTree) -> List AnalysisFragment
      | none => []
      | some t => queryAt t pos

partial def resolveCompoundAnnotationAux (prevList: List Annotation) (tree: AnnotationIntervalTree) (pos: String.Pos) : List Annotation := do
  if pos > tree.getInfo.maxChildTail then
    return prevList
  match tree.queryAt pos with
  | [] => 
    match prevList.getLast? with
    -- If the previous annotation is already a text annotation we don't have to add a new one.
    | some (Annotation.text _) => resolveCompoundAnnotationAux prevList tree (pos + 1)
    -- If the previous annotation is not a text annotation, then we create a new one with the current pos as its starting position
    | _ => do
      let annotation := Annotation.text { head := pos }
      return resolveCompoundAnnotationAux (prevList.append [annotation]) tree (pos + 1)
  | xs => 
    match prevList.getLast? with
    -- If the previous annotation is a compound annotation, we check for changes. If something changed we add a new compound annotation
    | some (Annotation.compound i) => do 
      -- TODO: Actually check if previous is the same
      let annotation := Annotation.compound { head := pos, fragments := xs}
      return resolveCompoundAnnotationAux (prevList.append [annotation]) tree (pos + 1) 
    | _ => do
    -- If the previous is not a compound annotation, we add a new compound annotation starting at the current position.
      let annotation := Annotation.compound { head := pos, fragments := xs}
      return resolveCompoundAnnotationAux (prevList.append [annotation]) tree (pos + 1) 

def resolveCompoundAnnotation (tree: AnnotationIntervalTree) : List Annotation := do
  return resolveCompoundAnnotationAux [] tree 0

end AnnotationIntervalTree