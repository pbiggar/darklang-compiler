// ANFOptimizeTests.fs - Unit tests for ANF optimizer ownership-sensitive DCE.
//
// These tests cover optimizer decisions that must agree with the RcShape
// metadata used by ownership insertion and backend helper selection.

module ANFOptimizeTests

open ANF

type TestResult = Result<unit, string>

let private dceOnlyOptions : ANF_Optimize.OptimizeOptions =
    { ANF_Optimize.defaultOptimizeOptions with
        EnableConstFolding = false
        EnableConstProp = false
        EnableCopyProp = false
        EnableDCE = true
        EnableStrengthReduction = false }

let private optimizeMain (context: ANF_Optimize.OptimizeContext) (expr: AExpr) : AExpr =
    let program = Program ([], expr)
    let (Program (_, optimizedMain)) =
        ANF_Optimize.optimizeProgramWithContextAndOptions context dceOnlyOptions program
    optimizedMain

let private markerContext : ANF_Optimize.OptimizeContext =
    { TypeReg = Map.empty
      SumShapeReg =
        Map.ofList [
            ("Marker",
             { TypeParams = ["a"]
               Payloads = [(0, None); (1, None)] })
            ("Box",
             { TypeParams = ["a"]
               Payloads = [(0, Some (AST.TVar "a"))] })
        ] }

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
]
