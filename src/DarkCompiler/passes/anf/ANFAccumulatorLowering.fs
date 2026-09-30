// ANFAccumulatorLowering.fs - Generate recursion helpers before SSA construction.

module ANFAccumulatorLowering

open ANF
open ANFConstants
open ANFExpressionOptimization
open ANFAccumulatorOptimization

/// These rewrites create helper functions from structured recursive arms.
/// They remain at the ANF construction boundary until direct SSA lowering.
let lower
    (context: OptimizeContext)
    (eligibleTailRecursionNames: Set<AST.FunctionId>)
    (externalFunctions: Map<string, Function>)
    (program: Program)
    : Program =
    let (Program (functions, _)) = program
    let programFunctionIds = functions |> List.map (fun func -> func.Id) |> Set.ofList
    let activeEligible = Set.intersect eligibleTailRecursionNames programFunctionIds
    if Set.isEmpty activeEligible then program
    else
        let helpers =
            planTailRecursionModuloHelpers context.FunctionNames context.FunctionIds activeEligible
        (program, freshVarGenForProgram program)
        |> fun (current, varGen) ->
            transformTailRecursionModuloAddition helpers varGen current
        |> fun (current, varGen) ->
            transformTailRecursionModuloSubtraction helpers varGen current
        |> fun (current, varGen) ->
            transformTailRecursionModuloMultiplication helpers varGen current
        |> fun (current, varGen) ->
            transformTailRecursionModuloFixedConstructors helpers varGen current
        |> fun (current, varGen) ->
            transformTailRecursionModuloListConstructors
                helpers externalFunctions varGen current
        |> fst
