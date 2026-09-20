// MIR_Optimize.fs - Schedule MIR simplification and optimization to a fixed point.

module MIR_Optimize

open MIR
open SSA_Construction
open MIROptimizationFacts
open MIRLoopTopology
open MIRInduction
open MIRUnrolling
open MIRLoopInvariantMotion
open MIRDeadCode
open MIRCopyPropagation
open MIRControlFlow
open MIRConstants
open MIRSparseConditionalConstants
open MIRCommonExpressions

/// Try to fold a unary operation on a constant
let tryFoldUnaryOp (op: UnaryOp) (src: Operand) : Operand option =
    match op, src with
    | Neg, Int64Const n -> Some (Int64Const (-n))
    | Not, BoolConst b -> Some (BoolConst (not b))
    | _ -> None

/// Apply constant folding to a CFG
let applyConstantFolding (cfg: CFG) : CFG * bool =
    let (blocks', changed) =
        cfg.Blocks
        |> Map.fold (fun (acc, ch) label block ->
            let (instrs', instrChanged) =
                block.Instrs
                |> List.fold (fun (acc', ch') instr ->
                    match instr with
                    | BinOp (dest, op, left, right, opType) ->
                        match tryFoldBinOp op left right opType with
                        | Some result ->
                            (Mov (dest, result, None) :: acc', true)
                        | None ->
                            (instr :: acc', ch')
                    | UnaryOp (dest, op, src) ->
                        match tryFoldUnaryOp op src with
                        | Some result ->
                            (Mov (dest, result, None) :: acc', true)
                        | None ->
                            (instr :: acc', ch')
                    | _ ->
                        (instr :: acc', ch')
                ) ([], false)
            let instrs' = List.rev instrs'

            let block' = { block with Instrs = instrs' }
            (Map.add label block' acc, ch || instrChanged)
        ) (Map.empty, false)

    ({ cfg with Blocks = blocks' }, changed)

/// Run all optimizations in a single pass (returns whether anything changed).
let private optimizeCFGOnceWithEffectFreeCalls
    (effectFreeFunctions: Set<AST.FunctionId>)
    (options: OptimizeOptions)
    (recordTicks: (string -> int64 -> unit) option)
    (existingTopology: DominatorTopology option)
    (cfg: CFG)
    : CFG * bool * DominatorTopology option =
    let measure name operation =
        match recordTicks with
        | None -> operation ()
        | Some record ->
            let started = System.Diagnostics.Stopwatch.GetTimestamp()
            let result = operation ()
            record name (System.Diagnostics.Stopwatch.GetTimestamp() - started)
            result
    let (cfg0, changed0) =
        if options.EnableConstFolding && options.EnableCFGSimplify then
            measure "MIR Sparse Conditional Constant Propagation" (fun () ->
                applySparseConditionalConstantPropagation cfg)
        else
            (cfg, false)
    let topologyForCse = if changed0 then None else existingTopology
    let (cfg1, changed1) =
        if options.EnableConstFolding then
            measure "MIR Constant Folding" (fun () -> applyConstantFolding cfg0)
        else
            (cfg0, false)
    let (cfg2, changed2, cseTopology) =
        if options.EnableCSE then
            measure "MIR Common Subexpression Elimination" (fun () ->
                let (optimized, changed, topology) =
                    applyCSEWithEffectFreeCallsAndTopology
                        topologyForCse
                        effectFreeFunctions
                        cfg1
                (optimized, changed, Some topology))
        else
            (cfg1, false, topologyForCse)
    let (cfg3, changed3) =
        if options.EnableCopyProp then
            measure "MIR Copy Propagation" (fun () ->
                applyCopyPropagationWithTickTrace recordTicks cfg2)
        else
            (cfg2, false)
    // Run constant folding again only when copy propagation changed the CFG.
    // This catches cases like: v1 = -127; v2 = v1 - 2
    // After copy prop: v2 = Int64Const(-127) - Int64Const(2) -> can fold
    let (cfg4, changed4) =
        if options.EnableConstFolding && changed3 then
            measure "MIR Constant Folding" (fun () -> applyConstantFolding cfg3)
        else
            (cfg3, false)
    let (cfg5, changed5, cfg6, changed6, loopTopology) =
        if options.EnableLICM then
            match
                measure "MIR Loop Topology" (fun () ->
                    match cseTopology with
                    | Some topology ->
                        tryBuildLoopTopologyWithDominators cfg4 topology
                    | None ->
                        tryBuildLoopTopology cfg4)
            with
            | None -> (cfg4, false, cfg4, false, None)
            | Some topology ->
                let (cfg5, changed5) =
                    measure "MIR Affine Strength Reduction" (fun () ->
                        applyAffineInductionStrengthReductionWithTopology
                            topology
                            cfg4)
                let (cfg6, changed6, topologyAfterLicm) =
                    measure "MIR Loop Invariant Code Motion" (fun () ->
                        applyLoopInvariantCodeMotionWithEffectFreeCalls
                            effectFreeFunctions
                            topology
                            cfg5)
                (cfg5, changed5, cfg6, changed6, Some topologyAfterLicm)
        else
            (cfg4, false, cfg4, false, None)
    let (cfg7, changed7) =
        match loopTopology with
        | Some topology ->
            measure "MIR Counted Loop Unrolling" (fun () ->
                applyCountedLoopUnrollingWithTopology topology cfg6)
        | None -> (cfg6, false)
    let (cfg8, changed8) =
        if options.EnableDCE then
            measure "MIR Dead Code Elimination" (fun () ->
                eliminateDeadCodeWithTickTrace recordTicks cfg7)
        else
            (cfg7, false)
    let (cfg9, changed9) =
        if options.EnableCFGSimplify then
            measure "MIR Simplify Constant Branches" (fun () -> simplifyConstantBranches cfg8)
        else
            (cfg8, false)
    let (cfg10, changed10) =
        if options.EnableCFGSimplify then
            measure "MIR Simplify Known Branches" (fun () -> simplifyBranchesKnownFromPredecessor cfg9)
        else
            (cfg9, false)
    let (cfg11, changed11) =
        if options.EnableCFGSimplify then
            measure "MIR Eliminate Unreachable Blocks" (fun () -> eliminateUnreachableBlocks cfg10)
        else
            (cfg10, false)
    let (cfg12, changed12) =
        if options.EnableCFGSimplify then
            measure "MIR Simplify Return Phi Joins" (fun () -> simplifyRetPhiJoins cfg11)
        else
            (cfg11, false)
    let (cfg13, changed13) =
        if options.EnableCFGSimplify then
            measure "MIR Simplify Empty Blocks" (fun () -> simplifyEmptyBlocks cfg12)
        else
            (cfg12, false)
    let (cfg14, changed14) =
        if options.EnableCFGSimplify then
            measure "MIR Merge Linear Blocks" (fun () -> mergeLinearBlocks cfg13)
        else
            (cfg13, false)
    let changed = changed0 || changed1 || changed2 || changed3 || changed4 || changed5 || changed6 || changed7 || changed8 || changed9 || changed10 || changed11 || changed12 || changed13 || changed14
    let topologyChanged =
        changed0
        || changed6
        || changed7
        || changed9
        || changed10
        || changed11
        || changed12
        || changed13
        || changed14
    let reusableTopology =
        if topologyChanged then None else cseTopology
    (cfg14, changed, reusableTopology)

let optimizeCFGOnce (options: OptimizeOptions) (cfg: CFG) : CFG * bool =
    let (optimized, changed, _) =
        optimizeCFGOnceWithEffectFreeCalls Set.empty options None None cfg
    (optimized, changed)

/// Run all optimizations until fixed point
let private optimizeCFGWithEffectFreeCalls
    (effectFreeFunctions: Set<AST.FunctionId>)
    (options: OptimizeOptions)
    (recordTicks: (string -> int64 -> unit) option)
    (cfg: CFG)
    : CFG =
    let rec loop current remaining existingTopology =
        if remaining <= 0 then
            current
        else
            let (next, changed, reusableTopology) =
                optimizeCFGOnceWithEffectFreeCalls
                    effectFreeFunctions
                    options
                    recordTicks
                    existingTopology
                    current
            if changed then
                loop next (remaining - 1) reusableTopology
            else
                next
    loop cfg 10 None

let optimizeCFGWithOptions (options: OptimizeOptions) (cfg: CFG) : CFG =
    optimizeCFGWithEffectFreeCalls Set.empty options None cfg

let optimizeCFG (cfg: CFG) : CFG =
    optimizeCFGWithOptions defaultOptimizeOptions cfg

let private explicitFloatRegisters (cfg: CFG) : Set<int> =
    cfg.Blocks
    |> Map.fold (fun registers _ block ->
        block.Instrs
        |> List.fold (fun acc instr ->
            let floatDestination =
                match instr with
                | Mov (destination, _, Some AST.TFloat64)
                | BinOp (destination, _, _, _, AST.TFloat64)
                | Phi (destination, _, Some AST.TFloat64)
                | FloatSqrt (destination, _)
                | FloatAbs (destination, _)
                | FloatNeg (destination, _)
                | Int64ToFloat (destination, _) -> Some destination
                | _ -> None
            match floatDestination with
            | Some (VReg id) -> Set.add id acc
            | None -> acc
        ) registers
    ) Set.empty

let private withOptimizedCFG (func: Function) (cfg: CFG) : Function =
    {
        func with
            CFG = cfg
            FloatRegs = Set.union func.FloatRegs (explicitFloatRegisters cfg)
    }

/// Optimize a function
let optimizeFunctionWithOptions (options: OptimizeOptions) (func: Function) : Function =
    let cfg' = optimizeCFGWithOptions options func.CFG
    withOptimizedCFG func cfg'

let private optimizeFunctionWithEffectFreeCalls
    (effectFreeFunctions: Set<AST.FunctionId>)
    (options: OptimizeOptions)
    (func: Function)
    : Function =
    let cfg' = optimizeCFGWithEffectFreeCalls effectFreeFunctions options None func.CFG
    withOptimizedCFG func cfg'

let optimizeFunctionWithEffectFreeCallsAndTickTrace
    (phaseTickRecorder: (string -> int64 -> unit) option)
    (effectFreeFunctions: Set<AST.FunctionId>)
    (options: OptimizeOptions)
    (func: Function)
    : Function =
    let cfg =
        optimizeCFGWithEffectFreeCalls
            effectFreeFunctions
            options
            phaseTickRecorder
            func.CFG
    withOptimizedCFG func cfg

let optimizeFunction (func: Function) : Function =
    let cfg' = optimizeCFG func.CFG
    withOptimizedCFG func cfg'

let private sameReturnOperand left right =
    match left, right with
    | FloatSymbol leftValue, FloatSymbol rightValue ->
        (System.BitConverter.DoubleToInt64Bits leftValue) = (System.BitConverter.DoubleToInt64Bits rightValue)
    | _ -> left = right

let private constantReturnOperand (func: Function) : Operand option =
    let hasTailCall =
        func.CFG.Blocks
        |> Map.exists (fun _ block ->
            block.Instrs
            |> List.exists (function
                | TailCall _
                | IndirectTailCall _
                | ClosureTailCall _ -> true
                | _ -> false))
    let definitions =
        func.CFG.Blocks
        |> Map.fold (fun constants _ block ->
            block.Instrs
            |> List.fold (fun current instr ->
                match instr with
                | Mov (destination, (Int64Const _ | BoolConst _ | FloatSymbol _ | StringSymbol _ | FuncAddr _ as value), _) ->
                    Map.add destination value current
                | _ -> current) constants) Map.empty
    let resolve operand =
        match operand with
        | Int64Const _
        | BoolConst _
        | FloatSymbol _
        | StringSymbol _
        | FuncAddr _ -> Some operand
        | Register register -> Map.tryFind register definitions
    let returns =
        func.CFG.Blocks
        |> Map.toList
        |> List.choose (fun (_, block) ->
            match block.Terminator with
            | Ret operand -> Some (resolve operand)
            | Jump _
            | Branch _ -> None)
    match hasTailCall, returns with
    | true, _ -> None
    | false, Some first :: rest when rest |> List.forall (function Some value -> sameReturnOperand first value | None -> false) ->
        Some first
    | _ -> None

let private constantCallResults (functions: Function list) : Map<AST.FunctionId, Operand> =
    functions
    |> List.choose (fun func -> constantReturnOperand func |> Option.map (fun value -> (func.Id, value)))
    |> Map.ofList

let private propagateConstantCallResults
    (optimizeAgain: Function -> Function)
    (options: OptimizeOptions)
    (functions: Function list)
    : Function list =
    if not (options.EnableConstFolding && options.EnableCFGSimplify) then
        functions
    else
        let callResults = constantCallResults functions
        if Map.isEmpty callResults then
            functions
        else
            functions
            |> List.map (fun func ->
                let cfg, changed =
                    applySparseConditionalConstantPropagationWithCallResults callResults func.CFG
                if changed then optimizeAgain (withOptimizedCFG func cfg) else func)

/// Optimize a program
let optimizeProgramWithOptions (options: OptimizeOptions) (program: Program) : Program =
    let (Program (functions, variants, records)) = program
    let effectFreeFunctions =
        if options.EnableLICM || options.EnableCSE then
            analyzeEffectFreeFunctions functions
        else
            Set.empty
    let functions' =
        functions
        |> List.map (optimizeFunctionWithEffectFreeCalls effectFreeFunctions options)
    let optimizedFunctions =
        functions'
        |> propagateConstantCallResults
            (optimizeFunctionWithEffectFreeCalls effectFreeFunctions options)
            options
    Program (optimizedFunctions, variants, records)

/// Optimize a program and report aggregate timings for the fixed-point
/// subpasses. Timings are accumulated as timestamp ticks so tracing does not
/// allocate a Stopwatch for every function iteration.
let optimizeProgramWithOptionsAndTrace
    (phaseRecorder: (string -> float -> unit) option)
    (options: OptimizeOptions)
    (program: Program)
    : Program =
    match phaseRecorder with
    | None -> optimizeProgramWithOptions options program
    | Some record ->
        let (Program (functions, variants, records)) = program
        let accumulatedTicks = System.Collections.Generic.Dictionary<string, int64>()
        let addTicks name ticks =
            match accumulatedTicks.TryGetValue name with
            | true, existing -> accumulatedTicks.[name] <- existing + ticks
            | false, _ -> accumulatedTicks.[name] <- ticks
        let effectAnalysisStart = System.Diagnostics.Stopwatch.GetTimestamp()
        let effectFreeFunctions =
            if options.EnableLICM || options.EnableCSE then
                analyzeEffectFreeFunctions functions
            else
                Set.empty
        addTicks
            "MIR Effect Analysis"
            (System.Diagnostics.Stopwatch.GetTimestamp() - effectAnalysisStart)
        let functions' =
            functions
            |> List.map (fun func ->
                let cfg =
                    optimizeCFGWithEffectFreeCalls
                        effectFreeFunctions
                        options
                        (Some addTicks)
                        func.CFG
                withOptimizedCFG func cfg)
        let optimizedFunctions =
            functions'
            |> propagateConstantCallResults
                (fun func ->
                    let cfg =
                        optimizeCFGWithEffectFreeCalls
                            effectFreeFunctions
                            options
                            (Some addTicks)
                            func.CFG
                    withOptimizedCFG func cfg)
                options
        let tickFrequency = float System.Diagnostics.Stopwatch.Frequency
        for KeyValue (name, ticks) in accumulatedTicks do
            record name (float ticks * 1000.0 / tickFrequency)
        Program (optimizedFunctions, variants, records)

let optimizeProgram (program: Program) : Program =
    optimizeProgramWithOptions defaultOptimizeOptions program

let optimizeConstFolding (program: Program) : Program =
    optimizeProgramWithOptions
        { defaultOptimizeOptions with
            EnableConstFolding = true
            EnableCSE = false
            EnableCopyProp = false
            EnableDCE = false
            EnableCFGSimplify = false
            EnableLICM = false }
        program

let optimizeCopyProp (program: Program) : Program =
    optimizeProgramWithOptions
        { defaultOptimizeOptions with
            EnableConstFolding = false
            EnableCSE = false
            EnableCopyProp = true
            EnableDCE = false
            EnableCFGSimplify = false
            EnableLICM = false }
        program

let optimizeDCE (program: Program) : Program =
    optimizeProgramWithOptions
        { defaultOptimizeOptions with
            EnableConstFolding = false
            EnableCSE = false
            EnableCopyProp = false
            EnableDCE = true
            EnableCFGSimplify = false
            EnableLICM = false }
        program
