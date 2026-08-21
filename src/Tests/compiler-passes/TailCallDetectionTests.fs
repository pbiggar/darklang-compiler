// TailCallDetectionTests.fs - Unit tests for tailcall conversion and cleanup ordering.
//
// Ensures non-self tailcall conversion does not strand RefCountDec operations
// after TailCall (which would be unreachable).

module TailCallDetectionTests

open ANF
open TailCallDetection

type TestResult = Result<unit, string>

let private isTailCallWithUnreachableCleanup (funcName: string) (cexpr: CExpr) : bool =
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

let rec private hasDecAfterNonSelfTailCall (funcName: string) (expr: AExpr) : bool =
    match expr with
    | Return _ ->
        false
    | Let (_, cexpr, Let (_, cleanup, _))
        when isTailCallWithUnreachableCleanup funcName cexpr && isCleanupDec cleanup ->
        true
    | Let (_, _, body) ->
        hasDecAfterNonSelfTailCall funcName body
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
                    Call ("callee", [Var p0]),
                    Let (decTmp, RefCountDec (Var tupleTmp, 16, GenericHeap, Some tupleMetadata), Return (Var callTmp))
                )
            )
    }

    let transformed = detectTailCallsInFunction caller

    if hasDecAfterNonSelfTailCall transformed.Name transformed.Body then
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
        Name = "caller"
        TypedParams = [{ Id = p0; Type = AST.TInt64 }]
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                funcTmp,
                Atom (FuncRef "callee"),
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

    if hasDecAfterNonSelfTailCall transformed.Name transformed.Body then
        Error "Found RefCountDec after IndirectTailCall; cleanup should run before tailcall"
    else
        Ok ()

let testOwnedTransferDeclinesMismatchedArity () : TestResult =
    let p0 = TempId 0
    let retainTmp = TempId 1
    let releaseTmp = TempId 2
    let callTmp = TempId 3
    let cleanupTmp = TempId 4
    let tupleType = AST.TTuple [AST.TInt64; AST.TInt64]
    let tupleMetadata =
        {
            ReleasePlan = Some (rcReleasePlanOfType Map.empty tupleType)
            SourceType = Some tupleType
        }
    let refCountDec atom = RefCountDec (atom, 16, GenericHeap, Some tupleMetadata)

    let caller : Function = {
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
                        Call ("caller", [Var p0; IntLiteral (Int64 1L); IntLiteral (Int64 2L)]),
                        Let (cleanupTmp, refCountDec (Var p0), Return (Var callTmp))
                    )
                )
            )
    }

    let transformed = detectTailCallsInFunction caller
    match transformed.Body with
    | Let (_, _, Let (_, _, Let (_, Call ("caller", _), _))) -> Ok ()
    | _ -> Error "Mismatched-arity owned transfer should preserve the ordinary call cleanup path"

let tests = [
    ("non-self tailcall moves dec before tailcall", testNonSelfTailCallMovesDecBeforeTailCall)
    ("indirect tailcall moves dec before tailcall", testIndirectTailCallMovesDecBeforeTailCall)
    ("owned transfer declines mismatched arity", testOwnedTransferDeclinesMismatchedArity)
]
