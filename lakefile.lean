import Lake
import Init.System.IO
open System Lake DSL
open System.FilePath IO IO.FS

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
  let realPath ← realPath leanInkExe
  return (leanInkExe, realPath)

/-! Run the leanInk that is built locally to analyze the given test file.
If there is a lakefile.lean present then pass the additional `--lake` option -/
def runLeanInk (test : FilePath) : IO UInt32 := do
  let (leanInkExe, realPath) ← getLeanInkExePath

  if ! (← leanInkExe.pathExists) then
    println s!"Could not find leanInk executable at {leanInkExe}"
    println s!"Please run `lake build` in the LeanInk directory"
    return 1

  if let some fileName := test.fileName then
    let mut args := #["analyze", fileName, "--x-enable-type-info", "--x-enable-docStrings", "--x-enable-semantic-token", "--prettify-output"]
    if let some dir := test.parent then
      let lakefile := dir / "lakefile.lean"
      if (← lakefile.pathExists) then
        println s!"Running test {test} using lake..."
        args := args ++ #["--lake", "lakefile.lean"]
      else
        println s!"Running test {test}..."

    let out ← Process.output { cmd := realPath.normalize.toString, args := args, cwd := test.parent }
    if out.exitCode = 0 then
      return 0
    else
      println s!"leanInk failed with {out.stdout} {out.stderr}"
      return out.exitCode
  return 1

/-! Compare the text contents of two files -/
def runDiff (actual : FilePath) (expected : FilePath) : IO Bool := do
  let actualStr ← FS.readFile actual
  let expectedStr ← FS.readFile expected
  return actualStr.trim = expectedStr.trim

def copyFile (src : FilePath) (dst : FilePath) : IO Unit := do
  FS.writeBinFile dst (← FS.readBinFile src)

def indexOf? [BEq α] (xs : List α) (s : α) (start := 0): Option Nat :=
  match xs with
  | [] => none
  | a :: tail => if a == s then some start else indexOf? tail s (start+1)

def getTargetLeanInkPath : IO (Option FilePath) := do
  let (leanInkExe, _) ← getLeanInkExePath
  if let some fileName := leanInkExe.fileName then
    let loc := if System.Platform.isWindows then "LOCALAPPDATA" else "HOME"
    let val? ← getEnv loc
    if let some homePath := val? then
      let targetPath : FilePath := homePath / ".leanink" / "bin"
      if !(← targetPath.pathExists) then
        createDirAll targetPath
      return some (targetPath / fileName)
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
            println s!"  UPDATING {expected}"
            copyFile actual expected
          else
            if (← runDiff actual expected) then
              println s!"  SUCCESS"
            else
              println s!"  FAILED: diff {expected} {actual}"
              retVal := retVal + 1
        else
          println s!"  FAILED: expected output file is missing: {expected}"
          retVal := retVal + 1

  if retVal > 0 then
    println s!"FAILED: {retVal} tests failed!"
  return retVal

script tests (args) do
  if args.length > 0 then
    println s!"Unexpected arguments: {args}"
  println "Running diff tests for leanInk"
  execute False

script capture (args) do
  if args.length > 0 then
    println s!"Unexpected arguments: {args}"
  println "Updating .leanInk.expected output files"
  execute True

script install (args) do
  if args.length > 0 then
    println s!"Unexpected arguments: {args}"
  if let some targetPath ← getTargetLeanInkPath then
    println s!"Installing leanInk to {targetPath}"
    println "Please ensure this location is in your PATH"
    let (_, realPath) ← getLeanInkExePath
    println "  Building leanInk..."
    let out ← Process.output { cmd := "lake", args := #["build"] }
      if out.exitCode = 0 then
        println s!"  Built {realPath}..."
        println s!"  Copying to {targetPath}..."
        copyFile realPath targetPath
        println s!"  LeanInk successfully installed!"
        return 0
      else
        println s!"### Failed to build leanInk:\n{out.stdout} {out.stderr}"
        return out.exitCode
  else
    println "### Cannot find .elan target directory"
    return 1
