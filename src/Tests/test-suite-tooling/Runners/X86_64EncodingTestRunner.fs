// X86_64EncodingTestRunner.fs - Executes x64 encoding and label-resolution fixtures.
//
// Reports final byte streams and deferred fixup labels with stable diagnostics.

module TestDSL.X86_64EncodingTestRunner

open System.IO
open TestDSL.PassTestRunner
open TestDSL.X86_64EncodingFormat

let private bytesToHex (bytes: byte array) : string =
    bytes |> Array.map (fun value -> value.ToString("X2")) |> String.concat " "

let private success : PassTestResult =
    { Success = true; Message = "Test passed"; Expected = None; Actual = None }

let private failure message expected actual : PassTestResult =
    { Success = false; Message = message; Expected = expected; Actual = actual }

let runX64EncodingTest (test: X64EncodingTest) : PassTestResult =
    match X86_64_Resolve.resolveAndEncode test.Instructions, test.Expectation with
    | Error msg, ResolutionErrorContaining expected when msg.Contains expected -> success
    | Error msg, ResolutionErrorContaining expected ->
        failure "Resolution error did not contain expected text" (Some expected) (Some msg)
    | Ok _, ResolutionErrorContaining expected ->
        failure "Expected x64 resolution to fail" (Some expected) (Some "Resolution succeeded")
    | Error msg, ResolvesTo _ -> failure $"x64 encoding/resolution failed: {msg}" None None
    | Ok result, ResolvesTo (expectedBytes, expectedFixups) ->
        let actualFixups = result.DeferredFixups |> List.map (fun fixup -> fixup.TargetLabel)
        match expectedBytes with
        | Some expected when expected <> result.MachineCode ->
            failure "x64 machine code did not match" (Some (bytesToHex expected)) (Some (bytesToHex result.MachineCode))
        | _ when expectedFixups <> actualFixups ->
            failure "x64 deferred fixups did not match" (Some $"{expectedFixups}") (Some $"{actualFixups}")
        | _ -> success

let loadX64EncodingTests path =
    if not (File.Exists path) then Error $"x64 encoding test file not found: {path}"
    else
        try File.ReadAllText path |> parseX64EncodingFileContent path
        with ex -> Error $"Failed to read x64 encoding test file {path}: {ex.Message}"

let tests (testFiles: string array) : (string * (unit -> Result<unit, string>)) list =
    let testsForFile path =
        match loadX64EncodingTests path with
        | Error msg -> [ ($"parse {Path.GetFileName path}", fun () -> Error msg) ]
        | Ok cases ->
            cases
            |> List.map (fun test ->
                (test.Name,
                 fun () ->
                    let result = runX64EncodingTest test
                    if result.Success then Ok ()
                    else
                        match result.Expected, result.Actual with
                        | Some expected, Some actual -> Error $"{result.Message}\nExpected: {expected}\nActual: {actual}"
                        | _ -> Error result.Message))
    testFiles |> Array.sort |> Array.toList |> List.collect testsForFile
