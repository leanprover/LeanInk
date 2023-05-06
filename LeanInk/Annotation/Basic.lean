import LeanInk.Configuration
import LeanInk.ListUtil
import LeanInk.Logger

import LeanInk.Annotation.Util
import LeanInk.Annotation.DataTypes

import LeanInk.Analysis.DataTypes
import LeanInk.Analysis.Analysis

namespace LeanInk.Annotation

open LeanInk.Analysis

/-
  Annotation
-/
def annotateFile (analysis : List Sentence) : AnalysisM (List Annotation) := do
  let compounds ← matchCompounds (toFragmentIntervals analysis)
  return compounds