// TailCallDetectionTests.fs - Unit tests for tailcall conversion and cleanup ordering.
//
// Ensures non-self tailcall conversion does not strand RefCountDec operations
// after TailCall (which would be unreachable).

module TailCallDetectionTests

open MemoryModel
open ReleasePlanFingerprint
open MemoryPlanning

open ANF
open TailCallDetection

type TestResult = Result<unit, string>

let private isTailCallWithUnreachableCleanup (funcName: AST.FunctionId) (cexpr: CExpr) : bool =
    match cexpr with
    | TailCall (target, _) when target <> funcName -> true
    | IndirectTailCall _ -> true
    | ClosureTailCall _ -> true
    | _ -> false

let private isCleanupDec (cexpr: CExpr) : bool =
    match cexpr with
    | RefCountDec _
    | RefCountDecString _
    | RefCountDecBlob _ -> true
    | _ -> false

let rec private hasDecAfterNonSelfTailCall (funcName: AST.FunctionId) (expr: AExpr) : bool =
    match expr with
    | Jump _ | Return _ ->
        false
    | Let (_, cexpr, Let (_, cleanup, _))
        when isTailCallWithUnreachableCleanup funcName cexpr && isCleanupDec cleanup ->
        true
    | Let (_, _, body) ->
        hasDecAfterNonSelfTailCall funcName body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        hasDecAfterNonSelfTailCall funcName thenBranch
        || hasDecAfterNonSelfTailCall funcName elseBranch

let testNonSelfTailCallMovesDecBeforeTailCall () : TestResult =
    let p0 = TempId 0
    let tupleTmp = TempId 1
    let callTmp = TempId 2
    let decTmp = TempId 3
    let tupleType = AST.TTuple [AST.TInt64; AST.TInt64]
    let releasePlan = rcReleasePlanOfType Map.empty tupleType
    let tupleMetadata =
        { ReleasePlanCacheKey = rcReleasePlanCacheKey tupleType releasePlan
          ReleasePlan = Some releasePlan
          SourceType = Some tupleType }

    let caller : Function = {
        Id = TestIds.functionIdForName "caller"
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
                    Call (TestIds.functionIdForName "callee", [Var p0]),
                    Let (decTmp, RefCountDec (Var tupleTmp, 16, GenericHeap, Some tupleMetadata), Return (Var callTmp))
                )
            )
    }

    let transformed = detectTailCallsInFunction caller

    if hasDecAfterNonSelfTailCall transformed.Id transformed.Body then
        Error "Found RefCountDec after non-self TailCall; cleanup should run before tailcall"
    else
        Ok ()

let testIndirectTailCallMovesDecBeforeTailCall () : TestResult =
    let p0 = TempId 0
    let funcTmp = TempId 1
    let tupleTmp = TempId 2
    let callTmp = TempId 3
    let decTmp = TempId 4
    let tupleType = AST.TTuple [AST.TInt64; AST.TInt64]
    let releasePlan = rcReleasePlanOfType Map.empty tupleType
    let tupleMetadata =
        { ReleasePlanCacheKey = rcReleasePlanCacheKey tupleType releasePlan
          ReleasePlan = Some releasePlan
          SourceType = Some tupleType }

    let caller : Function = {
        Id = TestIds.functionIdForName "caller"
        Name = "caller"
        TypedParams = [{ Id = p0; Type = AST.TInt64 }]
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                funcTmp,
                Atom (FuncRef (TestIds.functionIdForName "callee")),
                Let (
                    tupleTmp,
                    TupleAlloc [Var p0; IntLiteral (Int64 1L)],
                    Let (
                        callTmp,
                        IndirectCall (Var funcTmp, [Var p0]),
                        Let (decTmp, RefCountDec (Var tupleTmp, 16, GenericHeap, Some tupleMetadata), Return (Var callTmp))
                    )
                )
            )
    }

    let transformed = detectTailCallsInFunction caller

    if hasDecAfterNonSelfTailCall transformed.Id transformed.Body then
        Error "Found RefCountDec after IndirectTailCall; cleanup should run before tailcall"
    else
        Ok ()

let testClosureCallInTailPositionBecomesClosureTailCall () : TestResult =
    let closure = TempId 0
    let value = TempId 1
    let result = TempId 2
    let caller : Function = {
        Id = TestIds.functionIdForName "closureCaller"
        Name = "closureCaller"
        TypedParams = [
            { Id = closure; Type = AST.TFunction ([AST.TInt64], AST.TInt64) }
            { Id = value; Type = AST.TInt64 }
        ]
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body = Let (result, ClosureCall (Var closure, [Var value]), Return (Var result))
    }
    match (detectTailCallsInFunction caller).Body with
    | Let (bound, ClosureTailCall (Var target, [Var argument]), Return (Var returned))
        when bound = result && target = closure && argument = value && returned = result -> Ok ()
    | body -> Error $"Expected a tail-position closure invocation to form ClosureTailCall, got {body}"

let testOwnedTransferDeclinesMismatchedArity () : TestResult =
    let p0 = TempId 0
    let retainTmp = TempId 1
    let releaseTmp = TempId 2
    let callTmp = TempId 3
    let cleanupTmp = TempId 4
    let tupleType = AST.TTuple [AST.TInt64; AST.TInt64]
    let releasePlan = rcReleasePlanOfType Map.empty tupleType
    let tupleMetadata =
        { ReleasePlanCacheKey = rcReleasePlanCacheKey tupleType releasePlan
          ReleasePlan = Some releasePlan
          SourceType = Some tupleType }
    let refCountDec atom = RefCountDec (atom, 16, GenericHeap, Some tupleMetadata)

    let caller : Function = {
        Id = TestIds.functionIdForName "caller"
        Name = "caller"
        TypedParams = [{ Id = p0; Type = tupleType }]
        ReturnType = tupleType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                retainTmp,
                RefCountInc (Var p0, 16, GenericHeap, Some tupleMetadata),
                Let (
                    releaseTmp,
                    refCountDec (Var p0),
                    Let (
                        callTmp,
                        Call (TestIds.functionIdForName "caller", [Var p0; IntLiteral (Int64 1L); IntLiteral (Int64 2L)]),
                        Let (cleanupTmp, refCountDec (Var p0), Return (Var callTmp))
                    )
                )
            )
    }

    let transformed = detectTailCallsInFunction caller
    match transformed.Body with
    | Let (_, _, Let (_, _, Let (_, Call (name, _), _))) when name = TestIds.functionIdForName "caller" -> Ok ()
    | _ -> Error "Mismatched-arity owned transfer should preserve the ordinary call cleanup path"

let private ownedTransferTestFunction
    (secondArgument: TempId)
    (secondCleanup: (TempId * CExpr) option)
    : Function =
    let p0 = TempId 0
    let p1 = TempId 1
    let replacement0 = TempId 2
    let replacement1 = TempId 3
    let retain0 = TempId 4
    let retain1 = TempId 5
    let release0 = TempId 6
    let release1 = TempId 7
    let callTmp = TempId 8
    let cleanup0 = TempId 9
    let tupleType = AST.TTuple [AST.TInt64; AST.TInt64]
    let releasePlan = rcReleasePlanOfType Map.empty tupleType
    let metadata = {
        ReleasePlanCacheKey = rcReleasePlanCacheKey tupleType releasePlan
        ReleasePlan = Some releasePlan
        SourceType = Some tupleType
    }
    let inc temp = RefCountInc (Var temp, 16, GenericHeap, Some metadata)
    let dec temp = RefCountDec (Var temp, 16, GenericHeap, Some metadata)
    let terminal =
        secondCleanup
        |> Option.map (fun (cleanupTemp, cleanup) ->
            Let (cleanupTemp, cleanup, Return (Var callTmp)))
        |> Option.defaultValue (Return (Var callTmp))
    {
        Id = TestIds.functionIdForName "caller"
        Name = "caller"
        TypedParams = [
            { Id = p0; Type = tupleType }
            { Id = p1; Type = tupleType }
        ]
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                retain0,
                inc p0,
                Let (
                    retain1,
                    inc p1,
                    Let (
                        release0,
                        dec p0,
                        Let (
                            release1,
                            dec p1,
                            Let (
                                callTmp,
                                Call (TestIds.functionIdForName "caller", [Var replacement0; Var secondArgument]),
                                Let (cleanup0, dec replacement0, terminal)
                            )
                        )
                    )
                )
            )
    }

let testOwnedTransferRequiresOneToOneCleanupAccounting () : TestResult =
    let replacement0 = TempId 2
    let transformed =
        ownedTransferTestFunction replacement0 None
        |> detectTailCallsInFunction
    let rec containsOrdinarySelfCall expr =
        match expr with
        | Let (_, Call (id, _), _) when id = TestIds.functionIdForName "caller" -> true
        | Let (_, _, body) -> containsOrdinarySelfCall body
        | Join (_, continuation, entry)
        | If (_, continuation, entry) ->
            containsOrdinarySelfCall continuation || containsOrdinarySelfCall entry
        | Return _ | Jump _ -> false
    if containsOrdinarySelfCall transformed.Body then
        Ok ()
    else
        Error "One replacement edge must not transfer into two owned loop parameters"

let testOwnedTransferAcceptsMultipleExactlyMatchedCleanups () : TestResult =
    let replacement1 = TempId 3
    let cleanup1 = TempId 10
    let tupleType = AST.TTuple [AST.TInt64; AST.TInt64]
    let releasePlan = rcReleasePlanOfType Map.empty tupleType
    let metadata = {
        ReleasePlanCacheKey = rcReleasePlanCacheKey tupleType releasePlan
        ReleasePlan = Some releasePlan
        SourceType = Some tupleType
    }
    let transformed =
        ownedTransferTestFunction
            replacement1
            (Some (cleanup1, RefCountDec (Var replacement1, 16, GenericHeap, Some metadata)))
        |> detectTailCallsInFunction
    let rec containsSelfTailCall expr =
        match expr with
        | Let (_, TailCall (id, _), _) when id = TestIds.functionIdForName "caller" -> true
        | Let (_, _, body) -> containsSelfTailCall body
        | Join (_, continuation, entry)
        | If (_, continuation, entry) ->
            containsSelfTailCall continuation || containsSelfTailCall entry
        | Return _ | Jump _ -> false
    if containsSelfTailCall transformed.Body then
        Ok ()
    else
        Error "Two exactly matched replacement edges should transfer into two owned loop parameters"

let testRetainedProjectionAllowsSelfTailCall () : TestResult =
    let current = TempId 0
    let source = TempId 1
    let projected = TempId 2
    let retain = TempId 3
    let callResult = TempId 4
    let releaseSource = TempId 5
    let listType = AST.TList AST.TInt64
    let tupleType = AST.TTuple [listType]
    let listPlan = rcReleasePlanOfType Map.empty listType
    let tuplePlan = rcReleasePlanOfType Map.empty tupleType
    let metadata typ plan =
        { ReleasePlanCacheKey = rcReleasePlanCacheKey typ plan
          ReleasePlan = Some plan
          SourceType = Some typ }

    let func : Function = {
        Id = TestIds.functionIdForName "loop"
        Name = "loop"
        TypedParams = [{ Id = current; Type = listType }]
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                source,
                TupleAlloc [Var current],
                Let (
                    projected,
                    TupleGet (Var source, 0),
                    Let (
                        retain,
                        RefCountInc (Var projected, 24, TaggedList, Some (metadata listType listPlan)),
                        Let (
                            callResult,
                            Call (TestIds.functionIdForName "loop", [Var projected]),
                            Let (
                                releaseSource,
                                RefCountDec (Var source, 8, GenericHeap, Some (metadata tupleType tuplePlan)),
                                Return (Var callResult)
                            )
                        )
                    )
                )
            )
    }

    let transformed = detectTailCallsInFunction func
    let rec containsSelfTailCall expr =
        match expr with
        | Let (_, TailCall (id, _), _)
            when id = TestIds.functionIdForName "loop" -> true
        | Let (_, _, body) -> containsSelfTailCall body
        | Join (_, continuation, entry)
        | If (_, continuation, entry) ->
            containsSelfTailCall continuation || containsSelfTailCall entry
        | Return _ | Jump _ -> false
    if containsSelfTailCall transformed.Body then
        Ok ()
    else
        Error "An explicitly retained projection should transfer safely into a self tail call"

let tests = [
    ("non-self tailcall moves dec before tailcall", testNonSelfTailCallMovesDecBeforeTailCall)
    ("indirect tailcall moves dec before tailcall", testIndirectTailCallMovesDecBeforeTailCall)
    ("tail-position closure call forms ClosureTailCall", testClosureCallInTailPositionBecomesClosureTailCall)
    ("owned transfer declines mismatched arity", testOwnedTransferDeclinesMismatchedArity)
    ("owned transfer requires one-to-one cleanup accounting", testOwnedTransferRequiresOneToOneCleanupAccounting)
    ("owned transfer accepts multiple exactly matched cleanups", testOwnedTransferAcceptsMultipleExactlyMatchedCleanups)
    ("retained projection allows self tailcall", testRetainedProjectionAllowsSelfTailCall)
]
