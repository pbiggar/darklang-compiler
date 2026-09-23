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
        let variantLookup : LoweringPrimitives.VariantLookup =
            Map.empty
            |> Map.add "Some" ("Option", ["a"], 0, [AST.TVar "a"])
            |> Map.add "None" ("Option", [], 1, [])

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
        ValueType = AST.TRecord ("LayoutRecord", [])
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

/// Nested branches whose alternatives both loop cannot reach an enclosing
/// value continuation. Lowering must not invent an unreachable return block
/// or treat the non-returning subtree as an integer-valued predecessor.
let testNestedTerminalBranchesHaveNoInventedReturn () : TestResult =
    let name = "terminalBranches"
    let first = ANF.TempId 0
    let second = ANF.TempId 1
    let loop id =
        ANF.Let (
            ANF.TempId id,
            ANF.TailCall (TestIds.functionIdForName name, [ANF.BoolLiteral false; ANF.Var first]),
            ANF.Return (ANF.Var (ANF.TempId id)))
    let func : ANF.Function = {
        Id = TestIds.functionIdForName name
        Name = name
        TypedParams = [{ Id = first; Type = AST.TBool }; { Id = second; Type = AST.TBool }]
        ReturnType = AST.TFloat64
        ReturnOwnership = ANF.OwnedReturn
        Body =
            ANF.If (
                ANF.Var first,
                ANF.If (ANF.Var second, loop 2, loop 3),
                ANF.Return (ANF.FloatLiteral 3.5))
    }
    let denseTypes = [|Some AST.TBool; Some AST.TBool; Some AST.TFloat64; Some AST.TFloat64|]
    let types =
        Map.ofList [
            (first, AST.TBool)
            (second, AST.TBool)
            (ANF.TempId 2, AST.TFloat64)
            (ANF.TempId 3, AST.TFloat64)
        ]
    ANF_to_MIR.convertANFFunction
        func
        types
        denseTypes
        Map.empty
        (Map.ofList [(TestIds.functionIdForName name, AST.TFloat64)])
        false
    |> Result.bind (fun lowered ->
        let rec visit seen pending =
            match pending with
            | [] -> Ok seen
            | label :: rest when Set.contains label seen -> visit seen rest
            | label :: rest ->
                match Map.tryFind label lowered.CFG.Blocks with
                | None -> Error $"Missing branch target {label}"
                | Some block ->
                    let successors =
                        match block.Terminator with
                        | MIR.Ret _ -> []
                        | MIR.Jump target -> [target]
                        | MIR.Branch (_, yes, no) -> [yes; no]
                    visit (Set.add label seen) (successors @ rest)
        visit Set.empty [lowered.CFG.Entry]
        |> Result.bind (fun reachable ->
            let all = lowered.CFG.Blocks |> Map.keys |> Set.ofSeq
            if reachable = all then Ok ()
            else Error $"Terminal branches invented unreachable blocks: {Set.difference all reachable}"))

let tests : (string * (unit -> TestResult)) list =
    [
        ("raw_get intrinsic fallback crashes instead of defaulting to Int64", testRawGetIntrinsicReturnTypeDoesNotDefaultToInt64)
        ("variant registry rejects inconsistent type parameters", testBuildVariantRegistryRejectsInconsistentTypeParams)
        ("record allocation starts fields at offset zero", testRecordAllocationStartsFieldsAtOffsetZero)
        ("nested terminal branches have no invented return", testNestedTerminalBranchesHaveNoInventedReturn)
    ]
