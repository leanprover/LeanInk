import LeanInk.Annotation.DataTypes

namespace LeanInk

open System
open LeanInk.Annotation

structure Output where
  name : String
  genOutput : List Annotation -> IO UInt32
