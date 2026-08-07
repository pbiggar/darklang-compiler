// IRFormatSnapshotTestRunner.fs - Executes exact IR formatter snapshot fixtures.
//
// Formats typed ANF, MIR, or LIR inputs and reports stable expected/actual diagnostics.

module TestDSL.IRFormatSnapshotTestRunner

open System.IO
open IRPrinter
open TestDSL.Common
open TestDSL.IRFormatSnapshotFormat
open TestDSL.PassTestRunner

let runIRFormatSnapshotTest (test: IRFormatSnapshotTest) : PassTestResult =
    let actual =
        match test.Input with
        | ANFInput program -> formatANF program
        | MIRInput program -> formatMIR program
        | LIRInput program -> formatLIR program
        |> normalizeLineEndings

    if actual = test.Expected then
        { Success = true; Message = "Test passed"; Expected = None; Actual = None }
    else
        { Success = false
          Message = "Formatted IR did not match"
          Expected = Some test.Expected
          Actual = Some actual }

let loadIRFormatSnapshotTests path =
    if not (File.Exists path) then Error $"IR-format test file not found: {path}"
    else
        try File.ReadAllText path |> parseIRFormatSnapshotFileContent path
        with ex -> Error $"Failed to read IR-format test file {path}: {ex.Message}"

let tests (testFiles: string array) : (string * (unit -> Result<unit, string>)) list =
    let testsForFile path =
        match loadIRFormatSnapshotTests path with
        | Error msg -> [ ($"parse {Path.GetFileName path}", fun () -> Error msg) ]
        | Ok cases ->
            cases
            |> List.map (fun test ->
                (test.Name,
                 fun () ->
                    let result = runIRFormatSnapshotTest test
                    if result.Success then Ok ()
                    else Error $"{result.Message}\nExpected:\n{result.Expected}\nActual:\n{result.Actual}"))
    testFiles |> Array.sort |> Array.toList |> List.collect testsForFile
