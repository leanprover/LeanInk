namespace LeanInk.Commands.Analyze

open System

-- The `OutputType` specifies in which format the result of
-- leanInks analysis gets returned.
inductive OutputType where
  -- alectryonFragments describes the format used by Alectryon which basically is a list
  -- of fragments. Each fragment is either a `Text` or `Sentence`. TODO: specify further
  | alectryonFragments : OutputType 

-- The analyze `Configuration` describes all input specifications and infos for
-- the LeanInk analysis execution context. It contains the list of input file paths, etc.
structure Configuration where
  inputFilePath : FilePath
  inputFileContents : String
  outputType : OutputType
  lakeFile : Option FilePath

namespace Configuration

def inputFileName (self : Configuration) : String :=
  self.inputFilePath.toString

end Configuration