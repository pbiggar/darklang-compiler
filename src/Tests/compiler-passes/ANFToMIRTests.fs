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

/// Native record descriptors are compile-time metadata: fields occupy the
/// complete heap payload and lowering must not materialize a descriptor word.
let testRecordAllocationStartsFieldsAtOffsetZero () : TestResult =
    let descriptor : ANF.RecordDescriptor = {
        SourceTypeName = "LayoutRecord"
        RuntimeTypeName = "LayoutRecord"
        TypeArgs = []
        Fields = [("left", AST.TInt64); ("right", AST.TInt64)]
    }
    let program =
        ANF.Program (
            [],
            ANF.Let (
                ANF.TempId 0,
                ANF.RecordAlloc (descriptor, [ANF.IntLiteral (ANF.Int64 10L); ANF.IntLiteral (ANF.Int64 20L)]),
                ANF.Return (ANF.Var (ANF.TempId 0))
            )
        )
    let typeMap : ANF.TypeMap = Map.ofList [(ANF.TempId 0, AST.TRecord ("LayoutRecord", []))]

    match ANF_to_MIR.toMIR program typeMap Map.empty (AST.TRecord ("LayoutRecord", [])) Map.empty Map.empty false Map.empty with
    | Error err ->
        Error $"Unexpected record lowering error: {err}"
    | Ok (MIR.Program (functions, _, _)) ->
        match functions |> List.tryFind (fun func -> func.Name = "_start") with
        | None -> Error "Expected synthetic _start function"
        | Some start ->
            match Map.tryFind start.CFG.Entry start.CFG.Blocks with
            | None -> Error "Expected _start entry block"
            | Some block ->
                match block.Instrs with
                | [ MIR.HeapAlloc (_, 16)
                    MIR.HeapStore (_, 0, MIR.Int64Const 10L, None)
                    MIR.HeapStore (_, 8, MIR.Int64Const 20L, None) ] -> Ok ()
                | actual -> Error $"Expected a 16-byte record with fields at offsets 0 and 8, got {actual}"

let tests : (string * (unit -> TestResult)) list =
    [
        ("raw_get intrinsic fallback crashes instead of defaulting to Int64", testRawGetIntrinsicReturnTypeDoesNotDefaultToInt64)
        ("variant registry rejects inconsistent type parameters", testBuildVariantRegistryRejectsInconsistentTypeParams)
        ("record allocation starts fields at offset zero", testRecordAllocationStartsFieldsAtOffsetZero)
    ]
