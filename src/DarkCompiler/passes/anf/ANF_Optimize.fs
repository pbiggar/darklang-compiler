// ANF_Optimize.fs - Schedule ANF expression and accumulator optimizations.

module ANF_Optimize

open ANF
open ANFConstants
open ANFExpressionOptimization
open ANFAccumulatorOptimization

let rec private rewriteInvertedBoolLiteralBranches (varGen: VarGen) (expr: AExpr) : AExpr * VarGen =
    match expr with
    | Jump _ -> (expr, varGen)
    | Join (parameter, continuation, entry) ->
        let body, next = rewriteInvertedBoolLiteralBranches varGen continuation
        let entry', final = rewriteInvertedBoolLiteralBranches next entry
        (Join (parameter, body, entry'), final)
    | Return _ -> (expr, varGen)
    | Let (tid, cexpr, body) ->
        let (body', varGen') = rewriteInvertedBoolLiteralBranches varGen body
        (Let (tid, cexpr, body'), varGen')
    | If (cond, thenBranch, elseBranch) ->
        let (thenBranch', varGenAfterThen) = rewriteInvertedBoolLiteralBranches varGen thenBranch
        let (elseBranch', varGenAfterElse) = rewriteInvertedBoolLiteralBranches varGenAfterThen elseBranch

        match thenBranch', elseBranch' with
        | Return (BoolLiteral false), Return (BoolLiteral true) ->
            let (resultId, varGen') = freshVar varGenAfterElse
            (Let (resultId, UnaryPrim (Not, cond), Return (Var resultId)), varGen')
        | _ ->
            (If (cond, thenBranch', elseBranch'), varGenAfterElse)

let private rewriteInvertedBoolLiteralBranchesInProgram (program: Program) : Program =
    let (Program (functions, mainExpr)) = program
    let initialFreshVarGen = freshVarGenForProgram program
    let (functionsReversed, varGenAfterFunctions) =
        functions
        |> List.fold
            (fun (rewritten, varGen) func ->
                let (body', varGen') = rewriteInvertedBoolLiteralBranches varGen func.Body
                ({ func with Body = body' } :: rewritten, varGen'))
            ([], initialFreshVarGen)
    let (mainExpr', _) = rewriteInvertedBoolLiteralBranches varGenAfterFunctions mainExpr
    Program (List.rev functionsReversed, mainExpr')

/// Optimize a program with explicit options
let optimizeProgramWithOptionsAndExternalFunctions
    (context: OptimizeContext)
    (options: OptimizeOptions)
    (eligibleTailRecursionNames: Set<string>)
    (externalFunctions: Map<string, Function>)
    (program: Program)
    : Program =
    let program' =
        if options.EnableConstFolding then
            rewriteInvertedBoolLiteralBranchesInProgram program
        else
            program
    let (Program (functions, mainExpr)) = program'

    // Copy propagation first removes administrative aliases introduced by the
    // public `fun` syntax, exposing the exact local closure use set. Running
    // devirtualization afterwards does not need another fixed-point iteration:
    // it only removes a zero-capture allocation and changes known call forms.
    let functions' =
        functions
        |> List.map (fun func ->
            let optimized = optimizeToFixedPoint context options func 10
            { optimized with Body = devirtualizeCaptureFreeClosures optimized.Body })

    // Optimize main expression
    let mainFunc = { Name = "__main__"
                     TypedParams = []
                     ReturnType = AST.TUnit
                     ReturnOwnership = OwnedReturn
                     Body = mainExpr }
    let mainOptimized = optimizeToFixedPoint context options mainFunc 10

    let optimizedProgram =
        Program (functions', devirtualizeCaptureFreeClosures mainOptimized.Body)
    if options.EnableTailRecursionModuloOperation then
        optimizedProgram
        |> transformTailRecursionModuloAddition eligibleTailRecursionNames
        |> transformTailRecursionModuloSubtraction eligibleTailRecursionNames
        |> transformTailRecursionModuloMultiplication eligibleTailRecursionNames
        |> transformTailRecursionModuloFixedConstructors eligibleTailRecursionNames
        |> transformTailRecursionModuloListConstructors eligibleTailRecursionNames externalFunctions
    else
        optimizedProgram

let optimizeProgramWithOptions (context: OptimizeContext) (options: OptimizeOptions) (program: Program) : Program =
    let (Program (functions, _)) = program
    let eligible = functions |> List.map (fun func -> func.Name) |> Set.ofList
    optimizeProgramWithOptionsAndExternalFunctions context options eligible Map.empty program

/// Optimize a program with default options
let optimizeProgram (context: OptimizeContext) (program: Program) : Program =
    optimizeProgramWithOptions context defaultOptimizeOptions program

let optimizeConstFolding (context: OptimizeContext) (program: Program) : Program =
    optimizeProgramWithOptions
        context
        { defaultOptimizeOptions with
            EnableConstFolding = true
            EnableConstProp = false
            EnableCopyProp = false
            EnableDCE = false
            EnableCSE = false
            EnableStrengthReduction = false
            EnableTailRecursionModuloOperation = false }
        program

let optimizeCopyProp (context: OptimizeContext) (program: Program) : Program =
    optimizeProgramWithOptions
        context
        { defaultOptimizeOptions with
            EnableConstFolding = false
            EnableConstProp = false
            EnableCopyProp = true
            EnableDCE = false
            EnableCSE = false
            EnableStrengthReduction = false
            EnableTailRecursionModuloOperation = false }
        program

let optimizeDCE (context: OptimizeContext) (program: Program) : Program =
    optimizeProgramWithOptions
        context
        { defaultOptimizeOptions with
            EnableConstFolding = false
            EnableConstProp = false
            EnableCopyProp = false
            EnableDCE = true
            EnableCSE = false
            EnableStrengthReduction = false
            EnableTailRecursionModuloOperation = false }
        program
