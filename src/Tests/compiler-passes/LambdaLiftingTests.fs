// LambdaLiftingTests.fs - Unit tests for lambda lifting in AST_to_ANF
//
// Ensures lambda return types are preserved when lifting closures with let-bound bodies.

module LambdaLiftingTests

open AST
open ANF
open AST_to_ANF
open Parser
open TypeChecking

type TestResult = Result<unit, string>

let private convertProgramToAnf (typedAst: AST.Program) : Result<ANF.Program, string> =
    let moduleRegistry = Stdlib.buildModuleRegistry ()
    let monomorphized = monomorphize typedAst
    let inlined = inlineLambdasInProgram monomorphized
    liftLambdasInProgram Map.empty Map.empty Map.empty Map.empty inlined
    |> Result.bind (fun lifted ->
        splitTopLevels lifted
        |> Result.bind (fun (typeDefs, functions, expr) ->
            let aliasReg = buildAliasRegistry typeDefs
            let resolvedFunctions = resolveAliasesInFunctions aliasReg functions
            let registries = buildRegistries moduleRegistry typeDefs aliasReg resolvedFunctions
            let varGen = ANF.VarGen 0
            convertFunctions registries varGen resolvedFunctions
            |> Result.bind (fun (anfFuncs, varGen1) ->
                convertExprToAnf registries varGen1 expr
                |> Result.map (fun (anfExpr, _) ->
                    Program (anfFuncs, anfExpr)))))

let testLetBoundTupleReturnType () : TestResult =
    let source =
        "def apply(f: (Int64) -> (Int64, Int64), x: Int64) : (Int64, Int64) = f(x)\n" +
        "apply((fun x -> let t = (x, x + 1) in t), 1)"
    match parseString false source with
    | Error err -> Error $"Parse error: {err}"
    | Ok ast ->
        match checkProgram ast with
        | Error err -> Error $"Type error: {typeErrorToString err}"
        | Ok (_, typedAst) ->
            match convertProgramToAnf typedAst with
            | Error err -> Error $"ANF conversion error: {err}"
            | Ok (Program (functions, _)) ->
                let liftedFunc = functions |> List.tryFind (fun func -> func.Name.StartsWith("__closure_"))
                match liftedFunc with
                | None -> Error "Expected a lifted lambda function named __closure_*"
                | Some func ->
                    let expected = AST.TTuple [AST.TInt64; AST.TInt64]
                    if func.ReturnType = expected then
                        Ok ()
                    else
                        Error $"Expected lifted lambda return type {typeToString expected}, got {typeToString func.ReturnType}"

let tests = [
    ("Let-bound tuple return type", testLetBoundTupleReturnType)
]
