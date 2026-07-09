// ANFToMIRTests.fs - Unit tests for ANF to MIR lowering behavior.
//
// Covers pass-local edge cases that are not reachable from public E2E programs.

module ANFToMIRTests

type TestResult = Result<unit, string>

let testRawGetIntrinsicReturnTypeDoesNotDefaultToInt64 () : TestResult =
    try
        let actual = ANF_to_MIR.tryGetIntrinsicReturnType "__raw_get_str"
        Error $"Expected __raw_get_str fallback return type to crash, got {actual}"
    with
    | ex when ex.Message.Contains("monomorphized raw_get return type missing") -> Ok ()
    | ex -> Error $"Expected raw_get fallback crash, got: {ex.Message}"

let tests : (string * (unit -> TestResult)) list =
    [
        ("raw_get intrinsic fallback crashes instead of defaulting to Int64", testRawGetIntrinsicReturnTypeDoesNotDefaultToInt64)
    ]
