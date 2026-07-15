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

let testBuildVariantRegistryRejectsInconsistentTypeParams () : TestResult =
    try
        let variantLookup : AST_to_ANF.VariantLookup =
            Map.empty
            |> Map.add "Some" ("Option", ["a"], 0, Some (AST.TVar "a"))
            |> Map.add "None" ("Option", [], 1, None)

        let actual = ANF_to_MIR.buildVariantRegistry variantLookup
        Error $"Expected inconsistent type parameters to crash, got: {actual}"
    with
    | ex when ex.Message.Contains("inconsistent type parameters") -> Ok ()
    | ex -> Error $"Expected inconsistent type parameter crash, got: {ex.Message}"

let tests : (string * (unit -> TestResult)) list =
    [
        ("raw_get intrinsic fallback crashes instead of defaulting to Int64", testRawGetIntrinsicReturnTypeDoesNotDefaultToInt64)
        ("variant registry rejects inconsistent type parameters", testBuildVariantRegistryRejectsInconsistentTypeParams)
    ]
