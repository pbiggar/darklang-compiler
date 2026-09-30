// ANFAccumulatorLowering.fs - Generate recursion helpers before SSA construction.

module ANFAccumulatorLowering

open ANF
open ANFConstants
open ANFExpressionOptimization
open ANFAccumulatorOptimization

/// These rewrites create helper functions from structured recursive arms.
/// They remain at the ANF construction boundary until direct SSA lowering.
let lower
    (nextFunctionOrdinal: uint64)
    (context: OptimizeContext)
    (eligibleTailRecursionNames: Set<AST.FunctionId>)
    (externalFunctions: Map<string, Function>)
    (program: Program)
    : Program =
    let (Program (functions, _)) = program
    // Ownership lowering and synthesized entries can add functions after checking.
    // Include their ordinals while collecting local candidates; the catalog cursor
    // already reserves all external identities, including pruned declarations.
    let activeEligible, nextFunctionOrdinal =
        functions
        |> List.fold (fun (eligible, next) func ->
            let eligible =
                if Set.contains func.Id eligibleTailRecursionNames then Set.add func.Id eligible
                else eligible
            let next = max next (AST.nextFunctionIdOrdinal (AST.functionIdValue func.Id))
            eligible, next) (Set.empty, nextFunctionOrdinal)
    if Set.isEmpty activeEligible then program
    else
        let helpers =
            planTailRecursionModuloHelpers
                nextFunctionOrdinal context.FunctionNames context.FunctionIds activeEligible
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
