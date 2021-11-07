namespace LeanInk.Output.AlectryonFragment

structure Hypothesis where
  names : List String
  body : String
  type : String

structure Goal where
  name : String
  conclusion : String
  hyptheses : Array Hypothesis

structure Message where
  contents : String

structure Sentence where
  contents : String
  messages : Array Message
  goals : Array Goal

structure Text where
  contents : String

inductive Fragment where
| text (value : Text) : Fragment
| sentence (value : Sentence) : Fragment