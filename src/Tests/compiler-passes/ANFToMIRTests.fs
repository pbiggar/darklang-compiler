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

    match ANF_to_MIR.toMIR program typeMap Map.empty (AST.TRecord ("LayoutRecord", [])) Map.empty Map.empty false Map.empty (Map.ofList [AST.functionId 0UL, "_start"]) with
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
        (Map.ofList [(TestIds.functionIdForName name, name)])
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
        MIR_SSA_Verify.verifyFunction lowered
        |> Result.bind (fun () -> visit Set.empty [lowered.CFG.Entry])
        |> Result.bind (fun reachable ->
            let all = lowered.CFG.Blocks |> Map.keys |> Set.ofSeq
            if reachable = all then Ok ()
            else Error $"Terminal branches invented unreachable blocks: {Set.difference all reachable}"))

let testPhiEdgesPreserveDistinctPredecessors () : TestResult =
    let entry = MIR.Label "entry"
    let yes = MIR.Label "yes"
    let no = MIR.Label "no"
    let join = MIR.Label "join"
    let condition = MIR.VReg 0
    let result = MIR.VReg 1
    let block label instrs terminator : MIR.BasicBlock = {
        Label = label
        Instrs = instrs
        Terminator = terminator
    }
    let func : MIR.Function = {
        Id = TestIds.functionIdForName "invalidPhiEdges"
        Name = "invalidPhiEdges"
        TypedParams = [{ Reg = condition; Type = AST.TBool }]
        ReturnType = AST.TInt64
        CFG = {
            Entry = entry
            Blocks =
                Map.ofList [
                    entry, block entry [] (MIR.Branch (MIR.Register condition, yes, no))
                    yes, block yes [] (MIR.Jump join)
                    no, block no [] (MIR.Jump join)
                    join, block join [MIR.Phi (result, [(MIR.Int64Const 1L, yes); (MIR.Int64Const 2L, yes)], Some AST.TInt64)] (MIR.Ret (MIR.Register result))
                ]
        }
        FloatRegs = Set.empty
    }
    match MIR_SSA_Verify.verifyFunction func with
    | Error message when message.Contains "phi edges disagree" -> Ok ()
    | Error message -> Error $"Expected invalid phi edge error, got {message}"
    | Ok () -> Error "Expected duplicate phi predecessor to be rejected"

/// Branch-local ANF identities can be reused with different value types.
/// The pre-RC SSA builder must type each definition before MIR lowering.
let testPreRcSsaTypesBranchLocalDefinitions () : TestResult =
    let name = "branchLocalTypes"
    let functionId = TestIds.functionIdForName name
    let condition = ANF.TempId 0
    let reused = ANF.TempId 2
    let func : ANF.Function = {
        Id = functionId
        Name = name
        TypedParams = [{ Id = condition; Type = AST.TBool }]
        ReturnType = AST.TInt64
        ReturnOwnership = ANF.OwnedReturn
        Body =
            ANF.If (
                ANF.Var condition,
                ANF.Let (
                    reused,
                    ANF.TypedAtom (ANF.FloatLiteral 1.0, AST.TFloat64),
                    ANF.Return (ANF.IntLiteral (ANF.Int64 0L))),
                ANF.Let (
                    reused,
                    ANF.TypedAtom (ANF.BoolLiteral true, AST.TBool),
                    ANF.Return (ANF.IntLiteral (ANF.Int64 0L))))
    }
    let ctx : RcTypeFacts.TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = Map.ofList [functionId, (name, AST.TFunction ([AST.TBool], AST.TInt64))]
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = RcTypeFacts.createRcTypePlanningContext ()
    }
    SSAANF.convertFunctionBeforeRC 2 ctx func
    |> Result.bind (fun ssaFunc ->
        ANF_to_MIR.convertSSAANFFunction
            ssaFunc
            [|Some AST.TBool|]
            Map.empty
            (Map.ofList [functionId, AST.TInt64])
            (Map.ofList [functionId, name])
            false)
    |> Result.bind MIR_SSA_Verify.verifyFunction

/// A returned join value refers to each predecessor's edge argument. The
/// backedge also requires a fixed point through the loop parameter.
let testSsaReturnFlowAcrossJoinAndLoop () : TestResult =
    let id n = ANF.TempId n
    let label n = SSAANF.Label n
    let block n parameters operations terminator : SSAANF.Block = {
        Label = label n
        Parameters = parameters
        Operations = operations
        Terminator = terminator
    }
    let func : SSAANF.Function = {
        Id = TestIds.functionIdForName "returnFlow"
        Name = "returnFlow"
        TypedParams = [ { Id = id 0; Type = AST.TBool }; { Id = id 1; Type = AST.TString } ]
        ReturnType = AST.TString
        ReturnOwnership = ANF.OwnedReturn
        Entry = label 0
        FreshValueTypes = Map.empty
        Blocks = Map.ofList [
            label 0, block 0 [] [] (SSAANF.Branch (ANF.Var (id 0), label 1, label 2))
            label 1, block 1 [] [] (SSAANF.Jump (label 3, [ANF.Var (id 1)]))
            label 2, block 2 [] [] (SSAANF.Jump (label 3, [ANF.StringLiteral "other"]))
            label 3, block 3 [ { Id = id 3; Type = AST.TString } ]
                [id 4, ANF.Atom (ANF.Var (id 3))]
                (SSAANF.Branch (ANF.Var (id 0), label 4, label 5))
            label 4, block 4 [] [] (SSAANF.Jump (label 3, [ANF.Var (id 4)]))
            label 5, block 5 [] [] (SSAANF.Return (ANF.Var (id 4)))
        ]
    }
    let facts = RcSSAReturnAnalysis.analyze func
    let live = RcSSAValueLiveness.analyze func
    let expected = Map.ofList [
        label 0, Set.singleton (id 1)
        label 1, Set.singleton (id 1)
        label 2, Set.empty
        label 3, Set.singleton (id 3)
        label 4, Set.singleton (id 4)
        label 5, Set.singleton (id 4)
    ]
    if facts.AtEntry <> expected then
        Error $"Unexpected SSA return flow: {facts.AtEntry}"
    elif Map.tryFind (label 3) live.AtEntry <> Some (Set.ofList [id 0; id 3]) then
        Error $"Unexpected liveness at loop header: {live.AtEntry}"
    elif Map.tryFind (id 4) live.AfterDefinition <> Some (Set.ofList [id 0; id 4]) then
        Error $"Unexpected liveness after loop alias: {live.AfterDefinition}"
    else Ok ()

/// Edge arguments are substituted in parallel. Sequential substitution loses
/// one value when a backedge swaps two block parameters.
let testSsaReturnFlowThroughSwappedParameters () : TestResult =
    let id n = ANF.TempId n
    let label n = SSAANF.Label n
    let param n : ANF.TypedParam = { Id = id n; Type = AST.TString }
    let block n parameters terminator : SSAANF.Block = {
        Label = label n
        Parameters = parameters
        Operations = []
        Terminator = terminator
    }
    let func : SSAANF.Function = {
        Id = TestIds.functionIdForName "swappedReturnFlow"
        Name = "swappedReturnFlow"
        TypedParams = [
            { Id = id 0; Type = AST.TBool }
            { Id = id 1; Type = AST.TString }
            { Id = id 2; Type = AST.TString }
        ]
        ReturnType = AST.TString
        ReturnOwnership = ANF.OwnedReturn
        Entry = label 0
        FreshValueTypes = Map.empty
        Blocks = Map.ofList [
            label 0, block 0 [] (SSAANF.Jump (label 1, [ANF.Var (id 1); ANF.Var (id 2)]))
            label 1, block 1 [param 3; param 4]
                (SSAANF.Branch (ANF.Var (id 0), label 2, label 3))
            label 2, block 2 [] (SSAANF.Jump (label 1, [ANF.Var (id 4); ANF.Var (id 3)]))
            label 3, block 3 [] (SSAANF.Return (ANF.Var (id 3)))
        ]
    }
    let returned = RcSSAReturnAnalysis.analyze func
    let live = RcSSAValueLiveness.analyze func
    let both = Set.ofList [id 3; id 4]
    if Map.tryFind (label 1) returned.AtEntry <> Some both then
        Error $"Swapped return parameters were lost: {returned.AtEntry}"
    elif Map.tryFind (label 2) live.AtEntry <> Some (Set.add (id 0) both) then
        Error $"Swapped live parameters were lost: {live.AtEntry}"
    else Ok ()

let tests : (string * (unit -> TestResult)) list =
    [
        ("raw_get intrinsic fallback crashes instead of defaulting to Int64", testRawGetIntrinsicReturnTypeDoesNotDefaultToInt64)
        ("variant registry rejects inconsistent type parameters", testBuildVariantRegistryRejectsInconsistentTypeParams)
        ("record allocation starts fields at offset zero", testRecordAllocationStartsFieldsAtOffsetZero)
        ("nested terminal branches have no invented return", testNestedTerminalBranchesHaveNoInventedReturn)
        ("phi edges preserve distinct predecessors", testPhiEdgesPreserveDistinctPredecessors)
        ("pre-RC SSA types branch-local definitions", testPreRcSsaTypesBranchLocalDefinitions)
        ("SSA return flow across joins and loops", testSsaReturnFlowAcrossJoinAndLoop)
        ("SSA return flow through swapped parameters", testSsaReturnFlowThroughSwappedParameters)
    ]
