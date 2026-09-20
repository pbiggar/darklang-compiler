// ANFOptimizeTests.fs - Unit tests for ANF optimizer ownership-sensitive DCE.
//
// These tests cover optimizer decisions that must agree with the RcShape
// metadata used by ownership insertion and backend helper selection.

module ANFOptimizeTests

open MemoryModel

open ANF

type TestResult = Result<unit, string>

let private dceOnlyOptions : ANFConstants.OptimizeOptions =
    { ANFConstants.defaultOptimizeOptions with
        EnableConstFolding = false
        EnableConstProp = false
        EnableCopyProp = false
        EnableDCE = true
        EnableStrengthReduction = false }

let private optimizeMain (context: ANFConstants.OptimizeContext) (expr: AExpr) : AExpr =
    let program = Program ([], expr)
    let (Program (_, optimizedMain)) =
        ANF_Optimize.optimizeProgramWithOptions context dceOnlyOptions program
    optimizedMain

let private markerContext : ANFConstants.OptimizeContext =
    { TypeReg = Map.empty
      RecordTypeParams = Map.empty
      SumShapeReg =
        Map.ofList [
            ("Marker",
             { TypeParams = ["a"]
               Payloads = [(0, None); (1, None)] })
            ("Box",
             { TypeParams = ["a"]
               Payloads = [(0, Some (AST.TVar "a"))] })
        ] }

let private optimizeExpression (typeEnv: ANFConstants.TypeEnv) (expr: AExpr) : AExpr =
    ANFExpressionOptimization.optimizeAExpr
        markerContext
        ANFConstants.defaultOptimizeOptions
        Map.empty
        typeEnv
        expr
    |> fst

let rec private expressionCalls (expr: AExpr) : Set<AST.FunctionId> =
    match expr with
    | Return _ | Jump _ -> Set.empty
    | Let (_, cexpr, body) ->
        let current =
            match cexpr with
            | Call (name, _) -> Set.singleton name
            | _ -> Set.empty
        Set.union current (expressionCalls body)
    | If (_, thenBranch, elseBranch)
    | Join (_, thenBranch, elseBranch) ->
        Set.union (expressionCalls thenBranch) (expressionCalls elseBranch)

let private stdlibFunction name = AST.functionIdForName $"Darklang.Stdlib.{name}"

let testStringByteIndexConversionFusesWithLookup () : TestResult =
    let fromInt64 = stdlibFunction "Int.fromInt64"
    let getByteAt = stdlibFunction "String.getByteAt"
    let getByteAtInt64 = stdlibFunction "String.__getByteAtInt64"
    let expr =
        Let (
            TempId 2,
            Call (fromInt64, [Var (TempId 0)]),
            Let (
                TempId 3,
                Call (getByteAt, [Var (TempId 1); Var (TempId 2)]),
                Return (Var (TempId 3))
            )
        )
    let optimized =
        optimizeExpression
            (Map.ofList [(TempId 0, AST.TInt64); (TempId 1, AST.TString)])
            expr
    let calls = expressionCalls optimized
    if Set.contains getByteAtInt64 calls
       && not (Set.contains fromInt64 calls)
       && not (Set.contains getByteAt calls) then Ok ()
    else Error $"Expected index conversion and byte lookup to fuse, got {optimized}"

let private byteOptionMatch (usePayload: bool) : AExpr =
    let getByteAtInt64 = stdlibFunction "String.__getByteAtInt64"
    let someBranch =
        if usePayload then
            Let (TempId 5, TupleGet (Var (TempId 2), 1), Return (Var (TempId 5)))
        else
            Return (IntLiteral (Int64 1L))
    Let (
        TempId 2,
        Call (getByteAtInt64, [Var (TempId 0); Var (TempId 1)]),
        Let (
            TempId 3,
            TupleGet (Var (TempId 2), 0),
            Let (
                TempId 4,
                Prim (Eq, Var (TempId 3), IntLiteral (Int64 0L)),
                If (Var (TempId 4), someBranch, Return (IntLiteral (Int64 0L)))
            )
        )
    )

let testDeadStringBytePayloadLowersToBoundsCheck () : TestResult =
    let original = stdlibFunction "String.__getByteAtInt64"
    let byteLength = stdlibFunction "String.__byteLength"
    let optimized =
        byteOptionMatch false
        |> optimizeExpression
            (Map.ofList [(TempId 0, AST.TString); (TempId 1, AST.TInt64)])
    let calls = expressionCalls optimized
    if Set.contains byteLength calls && not (Set.contains original calls) then Ok ()
    else Error $"Expected dead Option payload construction to lower to a bounds check, got {optimized}"

let testLiveStringBytePayloadLowersToUncheckedLoad () : TestResult =
    let original = stdlibFunction "String.__getByteAtInt64"
    let byteLength = stdlibFunction "String.__byteLength"
    let uncheckedLoad = stdlibFunction "String.__byteAtUnchecked"
    let optimized =
        byteOptionMatch true
        |> optimizeExpression
            (Map.ofList [(TempId 0, AST.TString); (TempId 1, AST.TInt64)])
    let calls = expressionCalls optimized
    if Set.contains byteLength calls
       && Set.contains uncheckedLoad calls
       && not (Set.contains original calls) then Ok ()
    else Error $"Expected live Option payload construction to lower to a guarded unchecked load, got {optimized}"

let testLiteralFloatAbsoluteValueFolds () : TestResult =
    let optimized =
        Let (
            TempId 0,
            FloatAbs (FloatLiteral -3.5),
            Return (Var (TempId 0))
        )
        |> optimizeExpression Map.empty
    match optimized with
    | Return (FloatLiteral value) when value = 3.5 -> Ok ()
    | _ -> Error $"Expected literal FloatAbs to fold to 3.5, got {optimized}"

let testLiteralFloatToInt64Folds () : TestResult =
    let optimized =
        Let (
            TempId 0,
            FloatToInt64 (FloatLiteral -3.75),
            Return (Var (TempId 0))
        )
        |> optimizeExpression Map.empty
    match optimized with
    | Return (IntLiteral (Int64 -3L)) -> Ok ()
    | _ -> Error $"Expected literal FloatToInt64 to truncate to -3, got {optimized}"

let testLiteralBitNotFolds () : TestResult =
    let optimized =
        Let (
            TempId 0,
            UnaryPrim (BitNot, IntLiteral (Int64 1L)),
            Return (Var (TempId 0))
        )
        |> optimizeExpression Map.empty
    match optimized with
    | Return (IntLiteral (Int64 -2L)) -> Ok ()
    | _ -> Error $"Expected literal BitNot to fold to -2, got {optimized}"

let testDceDropsUnusedPureGenericSumTypedAtom () : TestResult =
    let markerType = AST.TSum ("Marker", [AST.TString])
    let expr =
        Let (
            TempId 0,
            TypedAtom (IntLiteral (Int64 0L), markerType),
            Return UnitLiteral
        )

    match optimizeMain markerContext expr with
    | Return UnitLiteral -> Ok ()
    | other -> Error $"Expected unused pure generic sum TypedAtom to be removed, got {other}"

let testDcePreservesUnusedHeapGenericSumTypedAtom () : TestResult =
    let boxType = AST.TSum ("Box", [AST.TString])
    let expr =
        Let (
            TempId 0,
            TypedAtom (IntLiteral (Int64 0L), boxType),
            Return UnitLiteral
        )

    match optimizeMain markerContext expr with
    | Let (TempId 0, TypedAtom (_, typ), Return UnitLiteral) when typ = boxType -> Ok ()
    | other -> Error $"Expected heap generic sum TypedAtom to be preserved, got {other}"

let tests = [
    ("DCE drops unused pure generic sum TypedAtom", testDceDropsUnusedPureGenericSumTypedAtom)
    ("DCE preserves unused heap generic sum TypedAtom", testDcePreservesUnusedHeapGenericSumTypedAtom)
    ("String byte lookup fuses Int64 index conversion", testStringByteIndexConversionFusesWithLookup)
    ("String byte lookup with dead payload lowers to bounds checks", testDeadStringBytePayloadLowersToBoundsCheck)
    ("String byte lookup with live payload lowers to unchecked load", testLiveStringBytePayloadLowersToUncheckedLoad)
    ("Literal FloatAbs folds", testLiteralFloatAbsoluteValueFolds)
    ("Literal FloatToInt64 folds", testLiteralFloatToInt64Folds)
    ("Literal BitNot folds", testLiteralBitNotFolds)
]
