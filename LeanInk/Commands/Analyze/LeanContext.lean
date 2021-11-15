import Lake

namespace LeanInk.Commands.Analyze

def initializeLeanContext : IO UInt32 := do
  let lean <- Lake.findLeanInstall?
  let lake <- Lake.findLakeInstall?
  let res <- Lake.setupLeanSearchPath lean lake
  return 0