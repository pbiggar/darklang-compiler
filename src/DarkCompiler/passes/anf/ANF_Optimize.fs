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
let optimizeProgramWithOptionsAndExternalFunctionsWithTrace
    (recordTiming: (string -> System.TimeSpan -> unit) option)
    (context: OptimizeContext)
    (options: OptimizeOptions)
    (eligibleTailRecursionNames: Set<AST.FunctionId>)
    (externalFunctions: Map<string, Function>)
    (program: Program)
    : Program =
    let measure name operation =
        match recordTiming with
        | None -> operation ()
        | Some record ->
            let timer = System.Diagnostics.Stopwatch.StartNew()
            let result = operation ()
            record name timer.Elapsed
            result
    let program' =
        if options.EnableConstFolding then
            measure "ANF Optimize detail: Boolean branch rewrite" (fun () ->
                rewriteInvertedBoolLiteralBranchesInProgram program)
        else
            program
    let (Program (functions, mainExpr)) = program'

    // Copy propagation first removes administrative aliases introduced by the
    // public `fun` syntax, exposing the exact local closure use set. Running
    // devirtualization afterwards does not need another fixed-point iteration:
    // it only removes a zero-capture allocation and changes known call forms.
    let functions' =
        measure "ANF Optimize detail: Function fixed points" (fun () ->
            functions
            |> List.map (fun func ->
                let optimized = optimizeToFixedPoint context options func 10
                { optimized with Body = devirtualizeCaptureFreeClosures optimized.Body }))

    // Optimize main expression
    let mainFunc = { Id = AST.functionIdForName "__dark_anf_optimization_main"
                     Name = "__main__"
                     TypedParams = []
                     ReturnType = AST.TUnit
                     ReturnOwnership = OwnedReturn
                     Body = mainExpr }
    let mainOptimized =
        measure "ANF Optimize detail: Main fixed point" (fun () ->
            optimizeToFixedPoint context options mainFunc 10)

    let optimizedProgram =
        Program (functions', devirtualizeCaptureFreeClosures mainOptimized.Body)
    if options.EnableTailRecursionModuloOperation then
        let programFunctionIds = functions' |> List.map (fun func -> func.Id) |> Set.ofList
        let activeEligible = Set.intersect eligibleTailRecursionNames programFunctionIds
        let helpers =
            measure "ANF Optimize detail: Accumulator helper planning" (fun () ->
                planTailRecursionModuloHelpers
                    context.FunctionNames
                    activeEligible)
        optimizedProgram
        |> fun current ->
            measure "ANF Optimize detail: Addition rewrite" (fun () ->
                transformTailRecursionModuloAddition helpers current)
        |> fun current ->
            measure "ANF Optimize detail: Subtraction rewrite" (fun () ->
                transformTailRecursionModuloSubtraction helpers current)
        |> fun current ->
            measure "ANF Optimize detail: Multiplication rewrite" (fun () ->
                transformTailRecursionModuloMultiplication helpers current)
        |> fun current ->
            measure "ANF Optimize detail: Fixed constructor rewrite" (fun () ->
                transformTailRecursionModuloFixedConstructors helpers current)
        |> fun current ->
            measure "ANF Optimize detail: List constructor rewrite" (fun () ->
                transformTailRecursionModuloListConstructors context.FunctionNames helpers externalFunctions current)
    else
        optimizedProgram

let optimizeProgramWithOptionsAndExternalFunctions
    (context: OptimizeContext)
    (options: OptimizeOptions)
    (eligibleTailRecursionNames: Set<AST.FunctionId>)
    (externalFunctions: Map<string, Function>)
    (program: Program)
    : Program =
    optimizeProgramWithOptionsAndExternalFunctionsWithTrace
        None
        context
        options
        eligibleTailRecursionNames
        externalFunctions
        program

let optimizeProgramWithOptions (context: OptimizeContext) (options: OptimizeOptions) (program: Program) : Program =
    let (Program (functions, _)) = program
    let eligible = functions |> List.map (fun func -> func.Id) |> Set.ofList
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
