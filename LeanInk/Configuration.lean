import LeanInk.Annotation.DataTypes

namespace LeanInk

open System
open LeanInk.Annotation

structure Configuration where
  inputFilePath : FilePath
  inputFileContents : String
  lakeFile : Option FilePath

namespace Configuration
  def inputFileName (self : Configuration) : String :=
    self.inputFilePath.toString
end Configuration

structure Output where
  name : String
  genOutput : List Annotation -> IO UInt32

abbrev ExecM := ReaderT Configuration IO