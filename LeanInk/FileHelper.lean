namespace LeanInk

open System

def leanFileExtension := s!"lean"

def isLeanFile (path : FilePath) : Bool := 
  path.extension == leanFileExtension