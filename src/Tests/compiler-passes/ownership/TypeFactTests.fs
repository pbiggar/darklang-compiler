// TypeFactTests.fs - Verify call-result type facts used by reference counting.

module RcTypeFactTests

open ANF
open RcTypeFacts
open RcCleanup
open MemoryShapeTests

let private fid = AST.functionIdForName

let testInferCallReturnsFunctionReturnType () : TestResult =
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg =
            Map.ofList [
                (fid "mkPair", ("mkPair", AST.TFunction ([AST.TInt64], AST.TTuple [AST.TInt64; AST.TInt64])))
            ]
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let cexpr = Call (fid "mkPair", [IntLiteral (Int64 1L)])

    match inferCExprType ctx cexpr with
    | Some (AST.TTuple [AST.TInt64; AST.TInt64]) ->
        Ok ()
    | Some actual ->
        Error $"Expected inferCExprType Call to return tuple return type, got: {actual}"
    | None ->
        Error "Expected inferCExprType Call to return a concrete type, got None"

let testMalformedRawGetIntrinsicDoesNotInferInt64 () : TestResult =
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = Map.empty
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
        TypePlanning = createRcTypePlanningContext ()
    }

    let cexpr = Call (fid "__raw_get_not_a_mangled_type", [Var (TempId 1); IntLiteral (Int64 0L)])

    match inferCExprType ctx cexpr with
    | None ->
        Ok ()
    | Some actual ->
        Error $"Expected malformed __raw_get_ suffix to remain unknown, got: {actual}"
