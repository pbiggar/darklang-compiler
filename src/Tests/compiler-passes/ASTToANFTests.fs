// ASTToANFTests.fs - Unit tests for AST to ANF conversion behavior
//
// Covers targeted AST-to-ANF regression cases that are easier to express
// directly at the pass boundary than through end-to-end language tests.

module ASTToANFTests

open AST_to_ANF

type TestResult = Result<unit, string>

let private emptyTypeReg : TypeRegistry = Map.empty
let private emptyVariantLookup : VariantLookup = Map.empty
let private emptyFuncReg : FunctionRegistry = Map.empty
let private emptyModuleRegistry : AST.ModuleRegistry = Map.empty

let testMissingVariantPayloadTypeErrors () : TestResult =
    let env : VarEnv =
        Map.ofList [("x", (ANF.TempId 0, AST.TSum ("MissingType", [])))]

    let pattern = AST.PConstructor ("MissingCtor", Some (AST.PVar "payload"))

    match AST.NonEmptyList.tryFromList [pattern] with
    | None -> Error "NonEmptyList.tryFromList returned None for a non-empty list"
    | Some patterns ->
        let matchCase : AST.MatchCase = { Patterns = patterns; Guard = None; Body = AST.Var "payload" }
        let expr = AST.Match (AST.Var "x", [matchCase])

        match toANF expr ANF.initialVarGen env emptyTypeReg emptyVariantLookup emptyFuncReg emptyModuleRegistry with
        | Ok _ -> Error "Expected error when constructor payload type is missing from variant lookup"
        | Error msg ->
            if msg.Contains "MissingCtor" then Ok ()
            else Error $"Unexpected error message: {msg}"

let testNeedsLambdaLoweringIgnoresShadowedFunc () : TestResult =
    let knownFuncs = Set.ofList ["f"]
    let expr = AST.Let (AST.LPVariable "f", AST.Int64Literal 1L, AST.Var "f")
    let program = AST.Program [AST.Expression expr]
    if programNeedsLambdaLowering knownFuncs program then
        Error "Expected shadowed function name to not trigger lambda lowering"
    else
        Ok ()

let testNeedsLambdaLoweringDetectsFuncValue () : TestResult =
    let knownFuncs = Set.ofList ["f"]
    let program = AST.Program [AST.Expression (AST.Var "f")]
    if programNeedsLambdaLowering knownFuncs program then Ok ()
    else Error "Expected function value usage to trigger lambda lowering"

let testNeedsLambdaLoweringDetectsLambda () : TestResult =
    let knownFuncs = Set.empty
    let expr =
        AST.Lambda (
            AST.NonEmptyList.singleton (AST.typedLambdaVariable "x" AST.TInt64),
            None,
            AST.Var "x"
        )
    let program = AST.Program [AST.Expression expr]
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

let testMangledFunctionTypePreservesSyntheticInterpreterTypeVariables () : TestResult =
    let typ = AST.TFunction ([AST.TVar "__interp_lambda_0_1_y"], AST.TInt64)
    let mangled = typeToMangledName typ
    match tryParseMangledType Map.empty mangled with
    | Ok (AST.TFunction ([AST.TVar "$u$uinterp$ulambda$u0$u1$uy"], AST.TInt64)) ->
        Ok ()
    | Ok other ->
        Error $"Expected synthetic interpreter type variable to parse inside function type, got {other} from {mangled}"
    | Error err ->
        Error $"Expected synthetic interpreter type variable function type to parse from {mangled}, got error: {err}"

let rec private findCallArgs (funcName: string) (expr: ANF.AExpr) : ANF.Atom list option =
    match expr with
    | ANF.Let (_, ANF.Call (name, args), rest) when name = funcName ->
        Some args
    | ANF.Let (_, _, rest) ->
        findCallArgs funcName rest
    | ANF.If (_, thenBranch, elseBranch) ->
        match findCallArgs funcName thenBranch with
        | Some args -> Some args
        | None -> findCallArgs funcName elseBranch
    | ANF.Return _ ->
        None

let testSyntheticNullaryCallLowersToZeroArgs () : TestResult =
    let funcName = "Stdlib.Internal.SkewList.__TAG_SINGLE"
    let expr = AST.Call (funcName, AST.NonEmptyList.singleton AST.UnitLiteral)
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
    let funcDef : AST.FunctionDef = {
        Name = "syntheticNullary"
        TypeParams = []
        Params = AST.NonEmptyList.singleton ("$unit0", AST.TUnit)
        ReturnType = AST.TInt64
        Body = AST.Int64Literal 1L
        Recursion = None
    }
    let funcReg : FunctionRegistry =
        Map.ofList [ ("syntheticNullary", AST.TFunction ([], AST.TInt64)) ]

    match convertFunction funcDef ANF.initialVarGen emptyTypeReg emptyVariantLookup funcReg emptyModuleRegistry with
    | Error err ->
        Error $"Unexpected conversion error: {err}"
    | Ok (anfFunc, _) ->
        match anfFunc.TypedParams with
        | [] -> Ok ()
        | typedParams ->
            Error $"Expected 0 lowered params, got {List.length typedParams}"

let tests = [
    ("Missing constructor payload type errors", testMissingVariantPayloadTypeErrors)
    ("Lambda lowering ignores shadowed functions", testNeedsLambdaLoweringIgnoresShadowedFunc)
    ("Lambda lowering detects function value", testNeedsLambdaLoweringDetectsFuncValue)
    ("Lambda lowering detects lambda", testNeedsLambdaLoweringDetectsLambda)
    ("Mangled type preserves freshened type variables", testMangledTypePreservesFreshenedTypeVariables)
    ("Mangled function type preserves synthetic interpreter type variables", testMangledFunctionTypePreservesSyntheticInterpreterTypeVariables)
    ("Synthetic nullary call lowers to zero args", testSyntheticNullaryCallLowersToZeroArgs)
    ("Synthetic unit param lowers function to zero params", testSyntheticUnitParamLowersFunctionToZeroParams)
]
