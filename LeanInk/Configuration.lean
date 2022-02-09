import LeanInk.Annotation.DataTypes

namespace LeanInk

open System
open LeanInk.Annotation

structure Configuration where
  inputFilePath : FilePath
  inputFileContents : String
  lakeFile : Option FilePath
  verbose : Bool
  experimentalTypeInfo : Bool
  experimentalDocString : Bool
  experimentalSemanticType : Bool

namespace Configuration
  def inputFileName (self : Configuration) : String :=
    self.inputFilePath.toString
end Configuration

abbrev AnalysisM := ReaderT Configuration $ IO

structure Output where
  name : String
  genOutput : List Annotation -> AnalysisM UInt32

abbrev ExecM := ReaderT Configuration $ IO