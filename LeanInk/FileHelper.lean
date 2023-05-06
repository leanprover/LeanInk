import LeanInk.Configuration
import LeanInk.Logger

import Lean.Data.Json
import Lean.Data.Json.Printer

namespace LeanInk

open Lean
open System

def leanFileExtension := s!"lean"

def isLeanFile (path : FilePath) : Bool := 
  path.extension == leanFileExtension

-- OUTPUT
open IO.FS
def createOutputFile (folderPath : FilePath) (fileName : String) (content : String) : AnalysisM Unit := do
  let dirEntry : DirEntry := { 
    root := folderPath,
    fileName := fileName ++ ".json"
  }
  let path := dirEntry.path
  IO.FS.writeFile path content
  logInfo s!"Results written to file: {path}!"