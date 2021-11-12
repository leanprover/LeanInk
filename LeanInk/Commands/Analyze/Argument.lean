import LeanInk.CLI.ParsableArgument

namespace LeanInk.Commands.Analyze

-- These define the explicit arguments that can be used when executing the
-- analyze command. This is atm a very naive implementation
-- as it can only check if the given string is included in the list of arguments.
-- 
-- Future improvement: Add more capable environment variables support, e.g:
--
-- analyze --output=alectryonFragments
-- instead of 
-- analyze --alectryonFragmentOuptut
--
-- this would also allow support for additional output formats
inductive Argument where
 | alectryonFragmentOutput : Argument
 | lakeFile : Argument
 deriving BEq

namespace Argument

instance : CLI.ParsableArgument Argument where
  toStrings
  | alectryonFragmentOutput => ["-afo", "--alectryonFragmentOuptut", "--output=alectryonFragments"]
  | lakeFile => ["-l", "--lake"]

  allConstructors := [alectryonFragmentOutput, lakeFile]