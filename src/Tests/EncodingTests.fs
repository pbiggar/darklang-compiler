// EncodingTests.fs - Unit tests for ARM64 encoding utilities
//
// Tests utility functions like encodeReg that are used by the
// ARM64 instruction encoder.
//
// NOTE: All tests now return Result<> instead of using failwith

module EncodingTests

open ARM64
open ARM64_Encoding

/// Test result type
type TestResult = Result<unit, string>

/// Test that encodeReg produces correct register numbers
let testEncodeReg () : TestResult =
    let tests = [
        (X0, 0u, "X0")
        (X1, 1u, "X1")
        (X15, 15u, "X15")
        (X30, 30u, "X30")
        (SP, 31u, "SP")
    ]

    let rec checkTests = function
        | [] -> Ok ()
        | (reg, expected, name) :: rest ->
            let actual = encodeReg reg
            if actual <> expected then
                Error $"encodeReg {name}: expected {expected}, got {actual}"
            else
                checkTests rest

    checkTests tests

/// Run all encoding unit tests
/// Returns Ok () if all pass, Error with first failure message if any fail
let runAll () : TestResult =
    let tests = [
        ("encodeReg", testEncodeReg)
    ]

    let rec runTests = function
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests tests
