// ANFToMIRTests.fs - Unit tests for ANF to MIR lowering behavior.
//
// Covers pass-local edge cases that are not reachable from public E2E programs.

module ANFToMIRTests

type TestResult = Result<unit, string>

let testRawGetIntrinsicReturnTypeDoesNotDefaultToInt64 () : TestResult =
    match ANF_to_MIR.tryGetIntrinsicReturnType "__raw_get_str" with
    | None -> Ok ()
    | Some actual ->
        Error $"Expected __raw_get_str fallback return type to remain unknown, got {actual}"

let tests : (string * (unit -> TestResult)) list =
    [
        ("raw_get intrinsic fallback does not default to Int64", testRawGetIntrinsicReturnTypeDoesNotDefaultToInt64)
    ]
