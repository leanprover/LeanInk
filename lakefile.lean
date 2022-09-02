import Lake
import Init.System.IO
open System Lake DSL
open System.FilePath

package leanInk

lean_lib LeanInk

@[defaultTarget]
lean_exe leanInk {
  root := `Main
  supportInterpreter := true
}

def getLeanInkExePath : IO (FilePath × FilePath) := do
  let leanInk : FilePath := "build" / "bin" / "leanInk"
  let leanInkExe := leanInk.withExtension FilePath.exeExtension
  let realPath ← IO.FS.realPath leanInkExe
  return (leanInkExe, realPath)

/-! Run the leanInk that is built locally to analyze the given test file.
If there is a lakefile.lean present then pass the additional `--lake` option -/
def runLeanInk (test : FilePath) : IO UInt32 := do
  let (leanInkExe, realPath) ← getLeanInkExePath

  if ! (← leanInkExe.pathExists) then
    IO.println s!"Could not find leanInk executable at {leanInkExe}"
    IO.println s!"Please run `lake build` in the LeanInk directory"
    return 1

  if let some fileName := test.fileName then
    let mut args := #["analyze", fileName, "--x-enable-type-info", "--x-enable-docStrings", "--x-enable-semantic-token", "--prettify-output"]
    if let some dir := test.parent then
      let lakefile := dir / "lakefile.lean"
      if (← lakefile.pathExists) then
        IO.println s!"Running test {test} using lake..."
        args := args ++ #["--lake", "lakefile.lean"]
      else
        IO.println s!"Running test {test}..."

    let out ← IO.Process.output { cmd := realPath.normalize.toString, args := args, cwd := test.parent }
    if out.exitCode = 0 then
      return 0
    else
      IO.println s!"leanInk failed with {out.stdout} {out.stderr}"
      return out.exitCode
  return 1

/-! Compare the text contents of two files -/
def runDiff (actual : FilePath) (expected : FilePath) : IO Bool := do
  let actualStr ← IO.FS.readFile actual
  let expectedStr ← IO.FS.readFile expected
  return actualStr.trim = expectedStr.trim

def copyFile (src : FilePath) (dst : FilePath) : IO Unit := do
  IO.FS.writeBinFile dst (← IO.FS.readBinFile src)

def indexOf? [BEq α] (xs : List α) (s : α) (start := 0): Option Nat :=
  match xs with
  | [] => none
  | a :: tail => if a == s then some start else indexOf? tail s (start+1)

def getTargetLeanInkPath : IO (Option FilePath) := do
  let (leanInkExe, _) ← getLeanInkExePath
  let lakePath ← IO.appPath
  let index := indexOf? lakePath.components ".elan"
  if let some i := index then
    if let some fileName := leanInkExe.fileName then
      let elanPath := lakePath.components.take (i + 1)
      let sep := pathSeparator.toString
      let elanPath : FilePath := sep.intercalate elanPath
      let leanInkPath := elanPath / "bin" / fileName
      return some leanInkPath
  return none

/-! Walk the `test` folder looking for every `.lean` file that's not a `lakefile` or part of an
`lean_package` and run `leanInk` on it. If `capture` is true then update the `.lean.leanInk.expected`
file, otherwise compare the new output to the expected output and return an error if they are
different. -/
def execute (capture : Bool) : IO UInt32 := do
  let root : FilePath := "." / "test"
  let dirs ← walkDir root
  let mut retVal : UInt32 := 0
  for test in dirs do
    if test.extension = "lean" && test.fileName != "lakefile.lean" && !test.components.contains "lean_packages" then
      if let some fileName := test.fileName then
        let actual := test.withFileName (fileName ++ ".leanInk")
        let expected := test.withFileName (fileName ++ ".leanInk.expected")
        if (← expected.pathExists) then
          let rc ← runLeanInk test
          if rc ≠ 0 then
            return 1
          else if (capture) then
            IO.println s!"  UPDATING {expected}"
            copyFile actual expected
          else
            if (← runDiff actual expected) then
              IO.println s!"  SUCCESS"
            else
              IO.println s!"  FAILED: diff {expected} {actual}"
              retVal := retVal + 1
        else
          IO.println s!"  FAILED: expected output file is missing: {expected}"
          retVal := retVal + 1

  if retVal > 0 then
    IO.println s!"FAILED: {retVal} tests failed!"
  return retVal

script tests (args) do
  IO.println "Running diff tests for leanInk"
  execute False

script capture (args) do
  IO.println "Updating .leanInk.expected output files"
  execute True

script install (args) do
  IO.println "Installing leanInk..."
  IO.println "  Building leanInk..."
  if let some targetPath ← getTargetLeanInkPath then
    let (_, realPath) ← getLeanInkExePath
    let out ← IO.Process.output { cmd := "lake", args := #["build"] }
      if out.exitCode = 0 then
        IO.println s!"  Built {realPath}..."
        IO.println s!"  Installing {targetPath}..."
        copyFile realPath targetPath
        IO.println s!"  LeanInk successfully installed!"
        return 0
      else
        IO.println s!"### Failed to build leanInk:\n{out.stdout} {out.stderr}"
        return out.exitCode
  else
    IO.println "### Cannot find .elan target directory"
    return 1
