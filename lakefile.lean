import Lake
import Init.System.IO
open System Lake DSL
open System.FilePath IO IO.FS

package leanInk

lean_lib LeanInk

@[default_target]
lean_exe leanInk {
  root := `Main
  supportInterpreter := true
}

/-! Run the leanInk that is built locally to analyze the given test file.
If there is a lakefile.lean present then pass the additional `--lake` option -/
def runLeanInk (leanInkExe: FilePath) (test : FilePath) : IO UInt32 := do
  let realPath ← realPath leanInkExe
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

/-! Walk the `test` folder looking for every `.lean` file that's not a `lakefile` or part of an
`lean_package` and run `leanInk` on it. If `capture` is true then update the `.lean.leanInk.expected`
file, otherwise compare the new output to the expected output and return an error if they are
different. -/
def execute (leanInkExe: FilePath) (capture : Bool) : IO UInt32 := do
  let root : FilePath := "." / "test"
  let dirs ← walkDir root (enter := fun path => return path.fileName != "lake-packages")
  let mut retVal : UInt32 := 0
  for test in dirs do
    if test.extension = "lean" && test.fileName != "lakefile.lean" then
      if let some fileName := test.fileName then
        let actual := test.withFileName (fileName ++ ".json")
        let expected := test.withFileName (fileName ++ ".json.expected")
        if (← expected.pathExists) then
          let rc ← runLeanInk leanInkExe test
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

def getLeanInkExePath : ScriptM (Option FilePath) := do
  let ws ← Lake.getWorkspace
  if let some exe := ws.findLeanExe? `leanInk then
    return exe.file
  return none

script tests (args) do
  if args.length > 0 then
    println s!"Unexpected arguments: {args}"
  if let some leanInkExe ← getLeanInkExePath then
    println "Running diff tests for leanInk"
    execute leanInkExe False
  else
    println "Cannot find `leanInk` target path"
    return 1

script capture (args) do
  if args.length > 0 then
    println s!"Unexpected arguments: {args}"
  if let some leanInkExe ← getLeanInkExePath then
    println "Updating .leanInk.expected output files"
    execute leanInkExe True
  else
    println "Cannot find `leanInk` target path"
    return 1
