// ParallelUtilsTests.fs - Unit tests for parallel list/result helpers
//
// Verifies parallel mapping helpers preserve order and error semantics.

module ParallelUtilsTests

open ParallelUtils

type TestResult = Result<unit, string>

let testMapResultsParallelPreservesOrder () : TestResult =
    let input = [ 1; 2; 3; 4 ]
    match mapResultsParallel (fun x -> Ok (x * 2)) input with
    | Ok output when output = [ 2; 4; 6; 8 ] -> Ok ()
    | Ok output -> Error $"Expected [2; 4; 6; 8], got {output}"
    | Error err -> Error $"Unexpected error: {err}"

let testMapResultsParallelReturnsFirstError () : TestResult =
    let input = [ 1; 2; 3 ]
    let result =
        mapResultsParallel
            (fun x ->
                if x = 2 then
                    Error "bad 2"
                elif x = 3 then
                    Error "bad 3"
                else
                    Ok x)
            input
    match result with
    | Error "bad 2" -> Ok ()
    | Error err -> Error $"Expected first error 'bad 2', got '{err}'"
    | Ok _ -> Error "Expected error but got Ok"

let runAll () : TestResult =
    let tests = [
        ("order preserved", testMapResultsParallelPreservesOrder)
        ("first error", testMapResultsParallelReturnsFirstError)
    ]
    let rec run remaining =
        match remaining with
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> run rest
            | Error msg -> Error $"{name} test failed: {msg}"
    run tests
