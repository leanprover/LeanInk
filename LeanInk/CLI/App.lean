namespace LeanInk.CLI

-- APPLICATION INFO
structure AppVersion where
  major : Nat
  minor : Nat
  patch : Nat
  suffix : String := ""

instance : ToString AppVersion where
  toString (self : AppVersion) : String := s!"{self.major}.{self.minor}.{self.patch}{self.suffix}"

structure AppInfo where
  name : String
  base : String
  version : AppVersion
  description : String

namespace AppInfo
  def versionString (self : AppInfo) : String := s!"{self.name} ({self.version})"
end AppInfo
