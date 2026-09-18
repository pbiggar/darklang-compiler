// ASTToANFTests.fs - Unit tests for AST to ANF conversion behavior
//
// Covers targeted AST-to-ANF regression cases that are easier to express
// directly at the pass boundary than through end-to-end language tests.

module ASTToANFTests

open LoweringPrimitives
open TypeRegistries
open SpecializationIdentity
open Monomorphization
open ClosureAnalysis
open ClosureComparisons
open LiftExpressions
open LiftFunctions
open LoweringExpressions
open AST_to_ANF
type TestResult = Result<unit, string>

let private emptyTypeReg : TypeRegistry = Map.empty
let private emptyVariantLookup : VariantLookup = Map.empty
let private emptyFuncReg : FunctionRegistry = Map.empty
let private emptyModuleRegistry : AST.ModuleRegistry = Map.empty

let testMissingVariantPayloadTypeErrors () : TestResult =
    let xId = AST.bindingId 1
    let payloadId = AST.bindingId 2
    let env : VarEnv =
        Map.ofList [(xId, (ANF.TempId 0, AST.TSum ("MissingType", [])))]

    let pattern =
        CheckedAST.PConstructor (AST.constructorId 0 0, [CheckedAST.PVariable payloadId])

    match AST.NonEmptyList.tryFromList [pattern] with
    | None -> Error "NonEmptyList.tryFromList returned None for a non-empty list"
    | Some patterns ->
        let matchCase : CheckedAST.MatchCase = { Patterns = patterns; Guard = None; Body = CheckedAST.Local payloadId }
        let expr = CheckedAST.Match (CheckedAST.Local xId, [matchCase])

        match toANF expr ANF.initialVarGen env emptyTypeReg emptyVariantLookup emptyFuncReg emptyModuleRegistry with
        | Ok _ -> Error "Expected error when constructor payload type is missing from variant lookup"
        | Error msg ->
            if msg.Contains "Constructor tag" then Ok ()
            else Error $"Unexpected error message: {msg}"

let testNeedsLambdaLoweringIgnoresShadowedFunc () : TestResult =
    let knownFuncs = Set.ofList ["f"]
    let fId = AST.bindingId 1
    let expr = CheckedAST.Let (CheckedAST.LPVariable fId, CheckedAST.Int64Literal 1L, CheckedAST.Local fId)
    let program = CheckedAST.Program (CheckedAST.emptySymbols (), [CheckedAST.Expression expr])
    if programNeedsLambdaLowering knownFuncs program then
        Error "Expected shadowed function name to not trigger lambda lowering"
    else
        Ok ()

let testNeedsLambdaLoweringDetectsFuncValue () : TestResult =
    let knownFuncs = Set.ofList ["f"]
    let program = CheckedAST.Program (CheckedAST.emptySymbols (), [CheckedAST.Expression (CheckedAST.NamedValue "f")])
    if programNeedsLambdaLowering knownFuncs program then Ok ()
    else Error "Expected function value usage to trigger lambda lowering"

let testNeedsLambdaLoweringDetectsLambda () : TestResult =
    let knownFuncs = Set.empty
    let xId = AST.bindingId 1
    let expr =
        CheckedAST.Lambda (
            AST.NonEmptyList.singleton
                ({ Pattern = CheckedAST.LPVariable xId; Type = AST.TInt64 }
                    : CheckedAST.LambdaParameter),
            None,
            CheckedAST.Local xId
        )
    let program = CheckedAST.Program (CheckedAST.emptySymbols (), [CheckedAST.Expression expr])
    if programNeedsLambdaLowering knownFuncs program then Ok ()
    else Error "Expected lambda to trigger lambda lowering"

let testMangledTypePreservesFreshenedTypeVariables () : TestResult =
    match tryParseMangledType Map.empty "k$0" with
    | Ok (AST.TVar "k$0") ->
        Ok ()
    | Ok other ->
        Error $"Expected freshened type variable to remain TVar, got {other}"
    | Error err ->
        Error $"Expected freshened type variable to parse, got error: {err}"

let testMangledFunctionTypePreservesSyntheticTypeVariables () : TestResult =
    let typ = AST.TFunction ([AST.TVar "__synthetic_lambda_0_1_y"], AST.TInt64)
    let mangled = typeToMangledName typ
    match tryParseMangledType Map.empty mangled with
    | Ok (AST.TFunction ([AST.TVar "$u$usynthetic$ulambda$u0$u1$uy"], AST.TInt64)) ->
        Ok ()
    | Ok other ->
        Error $"Expected synthetic type variable to parse inside function type, got {other} from {mangled}"
    | Error err ->
        Error $"Expected synthetic type variable function type to parse from {mangled}, got error: {err}"

let rec private findCallArgs (funcName: string) (expr: ANF.AExpr) : ANF.Atom list option =
    match expr with
    | ANF.Let (_, ANF.Call (name, args), rest) when name = funcName ->
        Some args
    | ANF.Let (_, _, rest) ->
        findCallArgs funcName rest
    | ANF.Join (_, thenBranch, elseBranch)
    | ANF.If (_, thenBranch, elseBranch) ->
        match findCallArgs funcName thenBranch with
        | Some args -> Some args
        | None -> findCallArgs funcName elseBranch
    | ANF.Jump _ | ANF.Return _ ->
        None

let rec private containsCExpr (predicate: ANF.CExpr -> bool) (expr: ANF.AExpr) : bool =
    match expr with
    | ANF.Let (_, cexpr, rest) ->
        predicate cexpr || containsCExpr predicate rest
    | ANF.Join (_, thenBranch, elseBranch)
    | ANF.If (_, thenBranch, elseBranch) ->
        containsCExpr predicate thenBranch || containsCExpr predicate elseBranch
    | ANF.Jump _ | ANF.Return _ ->
        false

let private lowerTwoElementListPattern (elementType: AST.Type) : Result<ANF.AExpr, string> =
    let valueId = AST.bindingId 1
    let headId = AST.bindingId 2
    let listType = AST.TList elementType
    let env : VarEnv =
        Map.ofList [(valueId, (ANF.TempId 0, AST.TTuple [listType; AST.TInt64]))]
    let matchCase : CheckedAST.MatchCase = {
        Patterns =
            AST.NonEmptyList.singleton
                (CheckedAST.PTuple [CheckedAST.PList [CheckedAST.PVariable headId; CheckedAST.PWildcard]; CheckedAST.PWildcard])
        Guard = None
        Body = CheckedAST.Local headId
    }
    let expr = CheckedAST.Match (CheckedAST.Local valueId, [matchCase])

    toANF expr ANF.initialVarGen env emptyTypeReg emptyVariantLookup emptyFuncReg emptyModuleRegistry
    |> Result.map fst

let testErasedListHeadPatternLowersToBorrowedCall () : TestResult =
    match lowerTwoElementListPattern (AST.TDict (AST.TString, AST.TString)) with
    | Error err ->
        Error $"Unexpected conversion error: {err}"
    | Ok anfExpr ->
        let hasBorrowedErasedHead =
            anfExpr
            |> containsCExpr (function
                | ANF.BorrowedCall ("Darklang.Stdlib.List.__headUnsafe_i64", _) -> true
                | _ -> false)
        let hasOwnedErasedHead =
            anfExpr
            |> containsCExpr (function
                | ANF.Call ("Darklang.Stdlib.List.__headUnsafe_i64", _) -> true
                | _ -> false)

        if not hasBorrowedErasedHead then
            Error "Managed erased list-head pattern did not lower to BorrowedCall"
        elif hasOwnedErasedHead then
            Error "Managed erased list-head pattern also emitted an owned Call"
        else
            Ok ()

let testTypedListHeadPatternRemainsOwnedCall () : TestResult =
    match lowerTwoElementListPattern AST.TFloat64 with
    | Error err ->
        Error $"Unexpected conversion error: {err}"
    | Ok anfExpr ->
        let hasOwnedTypedHead =
            anfExpr
            |> containsCExpr (function
                | ANF.Call ("Darklang.Stdlib.List.__headUnsafeFloat", _) -> true
                | _ -> false)
        let hasBorrowedTypedHead =
            anfExpr
            |> containsCExpr (function
                | ANF.BorrowedCall ("Darklang.Stdlib.List.__headUnsafeFloat", _) -> true
                | _ -> false)

        if not hasOwnedTypedHead then
            Error "Typed float list-head pattern did not lower to an owned Call"
        elif hasBorrowedTypedHead then
            Error "Typed float list-head pattern incorrectly lowered to BorrowedCall"
        else
            Ok ()

let testSyntheticNullaryCallLowersToZeroArgs () : TestResult =
    let funcName = "Darklang.Stdlib.List.__TAG_SINGLE"
    let expr = CheckedAST.Call (funcName, AST.NonEmptyList.singleton CheckedAST.UnitLiteral)
    let env : VarEnv = Map.empty
    let funcReg : FunctionRegistry =
        Map.ofList [ (funcName, AST.TFunction ([], AST.TInt64)) ]

    match toANF expr ANF.initialVarGen env emptyTypeReg emptyVariantLookup funcReg emptyModuleRegistry with
    | Error err ->
        Error $"Unexpected conversion error: {err}"
    | Ok (anfExpr, _) ->
        match findCallArgs funcName anfExpr with
        | None ->
            Error "Expected to find lowered direct call in ANF output"
        | Some [] ->
            Ok ()
        | Some args ->
            Error $"Expected synthetic nullary call to lower to zero args, got {List.length args}"

let testSyntheticUnitParamLowersFunctionToZeroParams () : TestResult =
    let unitId, symbols = CheckedAST.allocateBinding "$unit0" (CheckedAST.emptySymbols ())
    let funcDef : CheckedAST.FunctionDef = {
        Name = "syntheticNullary"
        TypeParams = []
        Params = AST.NonEmptyList.singleton (unitId, AST.TUnit)
        ReturnType = AST.TInt64
        Body = CheckedAST.Int64Literal 1L
        Recursion = None
    }
    let funcReg : FunctionRegistry =
        Map.ofList [ ("syntheticNullary", AST.TFunction ([], AST.TInt64)) ]

    match convertFunction symbols funcDef ANF.initialVarGen emptyTypeReg emptyVariantLookup funcReg emptyModuleRegistry with
    | Error err ->
        Error $"Unexpected conversion error: {err}"
    | Ok (anfFunc, _) ->
        match anfFunc.TypedParams with
        | [] -> Ok ()
        | typedParams ->
            Error $"Expected 0 lowered params, got {List.length typedParams}"

let testTypedParamAllocationPreservesOrder () : TestResult =
    let loweredParams =
        [(AST.bindingId 1, AST.TInt64); (AST.bindingId 2, AST.TBool); (AST.bindingId 3, AST.TString)]

    let (typedParams, nextVarGen) =
        allocateTypedParams loweredParams ANF.initialVarGen

    match typedParams, nextVarGen with
    | [ { ANF.TypedParam.Id = ANF.TempId 0; Type = AST.TInt64 }
        { ANF.TypedParam.Id = ANF.TempId 1; Type = AST.TBool }
        { ANF.TypedParam.Id = ANF.TempId 2; Type = AST.TString } ],
      ANF.VarGen 3 ->
        Ok ()
    | _ ->
        Error $"Expected ordered typed params t0, t1, t2 and next VarGen 3, got {typedParams} and {nextVarGen}"

let tests = [
    ("Missing constructor payload type errors", testMissingVariantPayloadTypeErrors)
    ("Lambda lowering ignores shadowed functions", testNeedsLambdaLoweringIgnoresShadowedFunc)
    ("Lambda lowering detects function value", testNeedsLambdaLoweringDetectsFuncValue)
    ("Lambda lowering detects lambda", testNeedsLambdaLoweringDetectsLambda)
    ("Mangled type preserves freshened type variables", testMangledTypePreservesFreshenedTypeVariables)
    ("Mangled function type preserves synthetic type variables", testMangledFunctionTypePreservesSyntheticTypeVariables)
    ("Synthetic nullary call lowers to zero args", testSyntheticNullaryCallLowersToZeroArgs)
    ("Synthetic unit param lowers function to zero params", testSyntheticUnitParamLowersFunctionToZeroParams)
    ("Erased list-head pattern lowers to borrowed call", testErasedListHeadPatternLowersToBorrowedCall)
    ("Typed list-head pattern remains owned call", testTypedListHeadPatternRemainsOwnedCall)

    ("Typed parameter allocation preserves order", testTypedParamAllocationPreservesOrder)
]
