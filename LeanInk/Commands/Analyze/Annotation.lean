import LeanInk.Commands.Analyze.Configuration
import LeanInk.Commands.Analyze.InfoTreeUtil

import LeanInk.Output.AlectryonFragment

namespace LeanInk.Commands.Analyze

open Output.AlectryonFragment

def annotateFileAux (l : List Fragment) (contents : String) (pos : String.Pos) (f : List TacticFragment) : IO (List Fragment) := do
  IO.println s!"Running Annotation (l: {l.length}) (pos: {pos}) (f: {f.length})"

  if contents.atEnd pos then
    return l
  else
    match f with
    -- We don't have any further tactics to annotate so we just return the rest of the contents as a text fragment.
    | [] => l.append [Fragment.text { contents := contents.extract pos contents.length }]
    | t::ts =>
      let format ← t.toFormat
      let goal : Goal := { name := "", conclusion := s!"{format}", hypotheses := #[] }
      let fragment := Fragment.sentence { contents := contents.extract t.headPos t.tailPos, messages := #[], goals := #[goal] }
      if t.headPos > pos then
        let textFragment := Fragment.text { contents := contents.extract pos t.headPos }
        return ← annotateFileAux (l.append [textFragment, fragment]) contents t.tailPos ts
      else
        return ← annotateFileAux (l.append [fragment]) contents t.tailPos ts

def annotateFile (config: Configuration) (annotations: List TacticFragment) : IO (List Fragment) := do
  for tactic in annotations do
    let format ← tactic.toFormat
    IO.println s!"{format}"

  IO.println s!"{config.inputFileContents}"
  
  return ← annotateFileAux [] config.inputFileContents 0 annotations