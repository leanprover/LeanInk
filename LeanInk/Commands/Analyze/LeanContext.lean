import Lake

namespace LeanInk.Commands.Analyze

def initializeLeanContext : IO Unit := do
  let lean <- Lake.findLeanInstall?
  let lake <- Lake.findLakeInstall?
  Lake.setupLeanSearchPath lean lake