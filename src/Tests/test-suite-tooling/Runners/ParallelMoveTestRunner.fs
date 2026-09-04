// ParallelMoveTestRunner.fs - Executes parallel-move lowering fixtures.
//
// Lowers LIR TailArgMoves and compares the complete symbolic ARM64 sequence.

module TestDSL.ParallelMoveTestRunner

open System.IO
open TestDSL.ParallelMoveFormat
open TestDSL.PassTestRunner

let private context : CodeGen.CodeGenContext = {
    Target = ARM64.targetConfigFor Platform.LinuxARM64
    Options = CodeGen.defaultOptions
    SumShapeRegistry = Map.empty
    RecordRegistry = Map.empty
    ClosurePayloadSizes = Map.empty
    ClosureCaptureTypes = Map.empty
    PlannedListDecHelperLabels = Map.empty
    PlannedGenericDecHelperLabels = Map.empty
    FunctionName = "parallel_move_fixture"
    InstructionSite = "fixture_0"
    StackSize = 0
    UsedCalleeSaved = []
    HeapOverflowLabel = "__heap_oom_parallel_move_fixture"
}

let private render instructions =
    instructions |> List.map prettyPrintARM64Instr |> String.concat "\n"

let runParallelMoveTest (test: ParallelMoveTest) : PassTestResult =
    match CodeGen.convertInstr context (LIR.TailArgMoves test.Moves) with
    | Error msg ->
        { Success = false; Message = $"Parallel-move lowering failed: {msg}"; Expected = None; Actual = None }
    | Ok actual when actual = test.Expected ->
        { Success = true; Message = "Test passed"; Expected = None; Actual = None }
    | Ok actual ->
        { Success = false
          Message = "Parallel-move ARM64 output did not match"
          Expected = Some (render test.Expected)
          Actual = Some (render actual) }

let loadParallelMoveTests path =
    if not (File.Exists path) then Error $"Parallel-move test file not found: {path}"
    else
        try File.ReadAllText path |> parseParallelMoveFileContent path
        with ex -> Error $"Failed to read parallel-move test file {path}: {ex.Message}"

let tests (testFiles: string array) : (string * (unit -> Result<unit, string>)) list =
    let testsForFile path =
        match loadParallelMoveTests path with
        | Error msg -> [ ($"parse {Path.GetFileName path}", fun () -> Error msg) ]
        | Ok cases ->
            cases
            |> List.map (fun test ->
                (test.Name,
                 fun () ->
                    let result = runParallelMoveTest test
                    if result.Success then Ok ()
                    else
                        match result.Expected, result.Actual with
                        | Some expected, Some actual -> Error $"{result.Message}\nExpected:\n{expected}\nActual:\n{actual}"
                        | _ -> Error result.Message))
    testFiles |> Array.sort |> Array.toList |> List.collect testsForFile
