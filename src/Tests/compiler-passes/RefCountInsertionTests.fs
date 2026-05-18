// RefCountInsertionTests.fs - Unit tests for RefCountInsertion type inference behavior.
//
// Verifies inferCExprType returns call result types (not function types) so
// downstream RC insertion decisions can use heap/non-heap information correctly.

module RefCountInsertionTests

open ANF
open RefCountInsertion

type TestResult = Result<unit, string>

let testRcShapeConstructionAndEquality () : TestResult =
    let tupleShape =
        FixedBlock (
            16,
            [
                Immediate
                DynamicString
            ]
        )

    let dictShape = DictRoot (DynamicString, TaggedListShape Immediate)
    let closureShape = ClosureShape [tupleShape; dictShape]
    let expected = ClosureShape [FixedBlock (16, [Immediate; DynamicString]); DictRoot (DynamicString, TaggedListShape Immediate)]

    if closureShape = expected then
        Ok ()
    else
        Error $"Expected RcShape equality to use structural representation, got: {closureShape}"

let testRcShapeClassifiesPrimitivesAsImmediate () : TestResult =
    let primitiveTypes = [
        AST.TInt8
        AST.TInt16
        AST.TInt32
        AST.TInt64
        AST.TInt128
        AST.TUInt8
        AST.TUInt16
        AST.TUInt32
        AST.TUInt64
        AST.TUInt128
        AST.TBool
        AST.TFloat64
        AST.TChar
        AST.TUnit
        AST.TRuntimeError
        AST.TVar "a"
    ]

    match primitiveTypes |> List.tryFind (fun typ -> rcShapeOfType Map.empty typ <> Immediate) with
    | None -> Ok ()
    | Some typ -> Error $"Expected primitive type {typ} to classify as Immediate"

let testRcShapeClassifiesTuplesAndRecordsAsFixedBlocks () : TestResult =
    let typeReg =
        Map.ofList [
            ("Pair", [("left", AST.TInt64); ("right", AST.TString)])
        ]

    let tupleShape = rcShapeOfType typeReg (AST.TTuple [AST.TInt64; AST.TString; AST.TBool])
    let recordShape = rcShapeOfType typeReg (AST.TRecord ("Pair", []))

    match tupleShape, recordShape with
    | FixedBlock (24, [Immediate; DynamicString; Immediate]), FixedBlock (16, [Immediate; DynamicString]) ->
        Ok ()
    | _ ->
        Error $"Unexpected fixed-block shapes. tuple={tupleShape}; record={recordShape}"

let testRcShapeClassifiesRemainingRuntimeShapes () : TestResult =
    let samples = [
        (AST.TString, DynamicString)
        (AST.TBytes, DynamicBytes)
        (AST.TRawPtr, RawUnmanaged)
        (AST.TFunction ([AST.TInt64], AST.TString), ClosureShape [])
        (AST.TSum ("Option", [AST.TString]), BoxedSum 16)
        (AST.TList AST.TString, TaggedListShape DynamicString)
        (AST.TDict (AST.TString, AST.TList AST.TInt64), DictRoot (DynamicString, TaggedListShape Immediate))
    ]

    match samples |> List.tryFind (fun (typ, expected) -> rcShapeOfType Map.empty typ <> expected) with
    | None -> Ok ()
    | Some (typ, expected) ->
        Error $"Expected {typ} to classify as {expected}, got {rcShapeOfType Map.empty typ}"

let testRcShapeOwnershipHelpersClassifyManagedRoots () : TestResult =
    let managedShapes = [
        DynamicString
        DynamicBytes
        FixedBlock (16, [Immediate; DynamicString])
        BoxedSum 16
        TaggedListShape DynamicString
        DictRoot (DynamicString, Immediate)
        ClosureShape [DynamicString]
    ]

    let unmanagedShapes = [
        Immediate
        StaticString
        RawUnmanaged
    ]

    match managedShapes |> List.tryFind (fun shape -> not (rcShapeNeedsOwnedScopeRelease shape)) with
    | Some shape ->
        Error $"Expected managed shape {shape} to need owned scope release"
    | None ->
        match unmanagedShapes |> List.tryFind rcShapeNeedsOwnedScopeRelease with
        | Some shape ->
            Error $"Expected unmanaged shape {shape} to skip owned scope release"
        | None ->
            Ok ()

let testRcShapeOwnershipHelpersSelectRootDispatch () : TestResult =
    let samples = [
        (FixedBlock (16, [DynamicString]), Some GenericHeap)
        (BoxedSum 16, Some GenericHeap)
        (TaggedListShape DynamicString, Some TaggedList)
        (TaggedListShape (ClosureShape []), Some GenericHeap)
        (DictRoot (Immediate, DynamicString), Some DictHeap)
        (ClosureShape [DynamicString], Some ClosureHeap)
        (Immediate, None)
        (DynamicString, None)
        (DynamicBytes, None)
        (RawUnmanaged, None)
    ]

    match samples |> List.tryFind (fun (shape, expected) -> rcShapeRootKind shape <> expected) with
    | None ->
        Ok ()
    | Some (shape, expected) ->
        Error $"Expected shape {shape} to use root kind {expected}, got {rcShapeRootKind shape}"

let testRcShapeOwnershipHelpersSelectRetainReleaseOperations () : TestResult =
    let samples = [
        (FixedBlock (16, [DynamicString]), Some (FixedSizeRoot (16, GenericHeap)))
        (BoxedSum 16, Some (FixedSizeRoot (16, GenericHeap)))
        (TaggedListShape DynamicString, Some (FixedSizeRoot (24, TaggedList)))
        (TaggedListShape (ClosureShape []), Some (FixedSizeRoot (24, GenericHeap)))
        (DictRoot (Immediate, DynamicString), Some (FixedSizeRoot (8, DictHeap)))
        (ClosureShape [DynamicString], Some (FixedSizeRoot (0, ClosureHeap)))
        (DynamicString, Some DynamicStringBuffer)
        (DynamicBytes, Some DynamicBytesBuffer)
        (Immediate, None)
        (StaticString, None)
        (RawUnmanaged, None)
    ]

    match samples |> List.tryFind (fun (shape, expected) -> rcShapeRetainOperation shape <> expected) with
    | Some (shape, expected) ->
        Error $"Expected shape {shape} to use retain operation {expected}, got {rcShapeRetainOperation shape}"
    | None ->
        match samples |> List.tryFind (fun (shape, expected) -> rcShapeReleaseOperation shape <> expected) with
        | Some (shape, expected) ->
            Error $"Expected shape {shape} to use release operation {expected}, got {rcShapeReleaseOperation shape}"
        | None ->
            Ok ()

let testRcShapeOwnershipHelpersClassifyRootManagement () : TestResult =
    let managedRootShapes = [
        FixedBlock (16, [Immediate; DynamicString])
        BoxedSum 16
        TaggedListShape DynamicString
        DictRoot (DynamicString, TaggedListShape Immediate)
        ClosureShape [DynamicString]
    ]

    let nonRootShapes = [
        Immediate
        DynamicString
        DynamicBytes
        StaticString
        RawUnmanaged
    ]

    match managedRootShapes |> List.tryFind (fun shape -> not (rcShapeIsRootManaged shape)) with
    | Some shape ->
        Error $"Expected shape {shape} to be a managed RC root"
    | None ->
        match nonRootShapes |> List.tryFind rcShapeIsRootManaged with
        | Some shape ->
            Error $"Expected shape {shape} not to be a managed RC root"
        | None ->
            Ok ()

let testRcShapeOwnershipHelpersClassifyRecursiveRelease () : TestResult =
    let recursiveShapes = [
        FixedBlock (16, [Immediate; DynamicString])
        BoxedSum 16
        TaggedListShape (FixedBlock (8, [DynamicString]))
        DictRoot (DynamicString, TaggedListShape Immediate)
        ClosureShape [DynamicString]
    ]

    let nonRecursiveShapes = [
        Immediate
        DynamicString
        DynamicBytes
        StaticString
        RawUnmanaged
        FixedBlock (8, [Immediate])
        TaggedListShape Immediate
        DictRoot (Immediate, Immediate)
        ClosureShape []
    ]

    match recursiveShapes |> List.tryFind (fun shape -> not (rcShapeNeedsRecursiveRelease shape)) with
    | Some shape ->
        Error $"Expected shape {shape} to need recursive release"
    | None ->
        match nonRecursiveShapes |> List.tryFind rcShapeNeedsRecursiveRelease with
        | Some shape ->
            Error $"Expected shape {shape} not to need recursive release"
        | None ->
            Ok ()

let testInferCallReturnsFunctionReturnType () : TestResult =
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        FuncReg =
            Map.ofList [
                ("mkPair", AST.TFunction ([AST.TInt64], AST.TTuple [AST.TInt64; AST.TInt64]))
            ]
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let cexpr = Call ("mkPair", [IntLiteral (Int64 1L)])

    match inferCExprType ctx cexpr with
    | Some (AST.TTuple [AST.TInt64; AST.TInt64]) ->
        Ok ()
    | Some actual ->
        Error $"Expected inferCExprType Call to return tuple return type, got: {actual}"
    | None ->
        Error "Expected inferCExprType Call to return a concrete type, got None"

let rec private hasDecAfterNonSelfTailCall (funcName: string) (expr: AExpr) : bool =
    match expr with
    | Return _ ->
        false
    | Let (_, TailCall (target, _), Let (_, RefCountDec _, _)) when target <> funcName ->
        true
    | Let (_, TailCall (target, _), Let (_, RefCountDecString _, _)) when target <> funcName ->
        true
    | Let (_, _, body) ->
        hasDecAfterNonSelfTailCall funcName body
    | If (_, thenBranch, elseBranch) ->
        hasDecAfterNonSelfTailCall funcName thenBranch
        || hasDecAfterNonSelfTailCall funcName elseBranch

let rec private hasRefCountIncForTemp (target: TempId) (expr: AExpr) : bool =
    match expr with
    | Return _ ->
        false
    | Let (_, RefCountInc (Var tempId, _, _, _), _) when tempId = target ->
        true
    | Let (_, _, body) ->
        hasRefCountIncForTemp target body
    | If (_, thenBranch, elseBranch) ->
        hasRefCountIncForTemp target thenBranch
        || hasRefCountIncForTemp target elseBranch

let rec private hasRefCountDecForTemp (target: TempId) (expr: AExpr) : bool =
    match expr with
    | Return _ ->
        false
    | Let (_, RefCountDec (Var tempId, _, _, _), _) when tempId = target ->
        true
    | Let (_, _, body) ->
        hasRefCountDecForTemp target body
    | If (_, thenBranch, elseBranch) ->
        hasRefCountDecForTemp target thenBranch
        || hasRefCountDecForTemp target elseBranch

let testNonSelfTailCallDoesNotLeaveDecAfterTailCall () : TestResult =
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            ("callee", AST.TFunction ([AST.TInt64], AST.TInt64))
            ("caller", AST.TFunction ([AST.TInt64], AST.TInt64))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let p0 = TempId 0
    let tupleTmp = TempId 1
    let callTmp = TempId 2
    let func : Function = {
        Name = "caller"
        TypedParams = [{ Id = p0; Type = AST.TInt64 }]
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                tupleTmp,
                TupleAlloc [Var p0; IntLiteral (Int64 1L)],
                Let (
                    callTmp,
                    TailCall ("callee", [Var p0]),
                    Return (Var callTmp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasDecAfterNonSelfTailCall transformed.Name transformed.Body then
        Error "Found RefCountDec after non-self TailCall; dec should execute before tailcall"
    else
        Ok ()

let testAliasReturnMaterializesOwnershipEvenIfFunctionMarkedBorrowed () : TestResult =
    let nodeType = AST.TList AST.TInt64
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            ("Stdlib.__FingerTree.__node2GetChild_i64", AST.TFunction ([nodeType; AST.TInt64], nodeType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let nodeParam = TempId 0
    let indexParam = TempId 1
    let childTemp = TempId 2

    let func : Function = {
        Name = "Stdlib.__FingerTree.__node2GetChild_i64"
        TypedParams = [
            { Id = nodeParam; Type = nodeType }
            { Id = indexParam; Type = AST.TInt64 }
        ]
        ReturnType = nodeType
        ReturnOwnership = BorrowedReturn
        Body =
            Let (
                childTemp,
                RawGet (Var nodeParam, IntLiteral (Int64 0L), Some nodeType),
                Return (Var childTemp)
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp childTemp transformed.Body then
        Ok ()
    else
        Error "Alias return should materialize ownership with RefCountInc even when function is marked BorrowedReturn"

let testBorrowedCallStillGetsAutoDecUnderConservativePolicy () : TestResult =
    let nodeType = AST.TList AST.TInt64
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            ("consumer", AST.TFunction ([nodeType; AST.TInt64], AST.TInt64))
            ("Stdlib.__FingerTree.__node2GetChild_i64", AST.TFunction ([nodeType; AST.TInt64], nodeType))
            ("Stdlib.__FingerTree.__nodeMeasure_i64", AST.TFunction ([nodeType], AST.TInt64))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let nodeParam = TempId 0
    let indexParam = TempId 1
    let childTemp = TempId 2
    let measureTemp = TempId 3

    let func : Function = {
        Name = "consumer"
        TypedParams = [
            { Id = nodeParam; Type = nodeType }
            { Id = indexParam; Type = AST.TInt64 }
        ]
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                childTemp,
                BorrowedCall ("Stdlib.__FingerTree.__node2GetChild_i64", [Var nodeParam; Var indexParam]),
                Let (
                    measureTemp,
                    Call ("Stdlib.__FingerTree.__nodeMeasure_i64", [Var childTemp]),
                    Return (Var measureTemp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountDecForTemp childTemp transformed.Body then
        Ok ()
    else
        Error "BorrowedCall should be treated as owned result under conservative policy and get automatic RefCountDec"

let tests = [
    ("RcShape supports structural construction and equality", testRcShapeConstructionAndEquality)
    ("RcShape classifies primitives as immediate", testRcShapeClassifiesPrimitivesAsImmediate)
    ("RcShape classifies tuples and records as fixed blocks", testRcShapeClassifiesTuplesAndRecordsAsFixedBlocks)
    ("RcShape classifies remaining runtime shapes", testRcShapeClassifiesRemainingRuntimeShapes)
    ("RcShape ownership helpers classify managed roots", testRcShapeOwnershipHelpersClassifyManagedRoots)
    ("RcShape ownership helpers select root dispatch", testRcShapeOwnershipHelpersSelectRootDispatch)
    ("RcShape ownership helpers select retain/release operations", testRcShapeOwnershipHelpersSelectRetainReleaseOperations)
    ("RcShape ownership helpers classify managed RC roots", testRcShapeOwnershipHelpersClassifyRootManagement)
    ("RcShape ownership helpers classify recursive release", testRcShapeOwnershipHelpersClassifyRecursiveRelease)
    ("inferCExprType Call returns function return type", testInferCallReturnsFunctionReturnType)
    ("non-self tailcall does not keep dec after tailcall", testNonSelfTailCallDoesNotLeaveDecAfterTailCall)
    ("alias return materializes ownership even for borrowed-return function", testAliasReturnMaterializesOwnershipEvenIfFunctionMarkedBorrowed)
    ("borrowed call still gets auto-dec under conservative policy", testBorrowedCallStillGetsAutoDecUnderConservativePolicy)
]
