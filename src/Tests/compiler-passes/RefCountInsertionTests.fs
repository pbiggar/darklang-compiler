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
    | Let (_, RefCountInc (Var tempId, _, _), _) when tempId = target ->
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
    | Let (_, RefCountDec (Var tempId, _, _), _) when tempId = target ->
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
    ("inferCExprType Call returns function return type", testInferCallReturnsFunctionReturnType)
    ("non-self tailcall does not keep dec after tailcall", testNonSelfTailCallDoesNotLeaveDecAfterTailCall)
    ("alias return materializes ownership even for borrowed-return function", testAliasReturnMaterializesOwnershipEvenIfFunctionMarkedBorrowed)
    ("borrowed call still gets auto-dec under conservative policy", testBorrowedCallStillGetsAutoDecUnderConservativePolicy)
]
