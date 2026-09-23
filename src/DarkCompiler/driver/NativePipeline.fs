// NativePipeline.fs - Orchestrate MIR optimization, LIR lowering, and register allocation.

module NativePipeline

open ARM64CodeGenTypes
open ARM64Functions
open ARM64PrepareFunctions
open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Collections.Generic
open Output
open CompilerOptions
open CompilationCacheIdentity
open CompilationSession
open PipelineDiagnostics

/// Run SSA + MIR/LIR optimizations, returning an optimized LIR program
let private compileMirToLir
    (arch: Platform.Arch)
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (passTimingRecorder: PassTimingRecorder option)
    (functionCaches: FunctionCompilationCaches option)
    (registries: AST_to_ANF.Registries)
    (stageSuffix: string)
    (mirProgram: MIR.Program)
    : Result<LIR.Function list, string> =

    let suffix = if stageSuffix = "" then "" else $" ({stageSuffix})"

    if verbosity >= 1 then println $"  [mir.ssa] SSA Construction{suffix}..."
    let ssaStart = sw.Elapsed.TotalMilliseconds
    let convertFunction func =
        let convert () =
            match passTimingRecorder with
            | None -> SSA_Construction.convertFunctionToSSA func
            | Some recorder ->
                let (converted, timings) =
                    SSA_Construction.convertFunctionToSSAWithTiming func
                timings
                |> List.iter (fun timing ->
                    recorder {
                        Pass = timing.Phase
                        Elapsed = TimeSpan.FromMilliseconds timing.ElapsedMs
                    })
                converted
        match functionCaches with
        | Some caches -> caches.ConvertSsa func convert
        | None -> convert ()
    let (MIR.Program (mirFunctions, mirVariants, mirRecords)) = mirProgram
    let ssaProgram =
        MIR.Program (
            mirFunctions |> List.map convertFunction,
            mirVariants,
            mirRecords)
    let ssaElapsed = sw.Elapsed.TotalMilliseconds - ssaStart
    recordPassTiming passTimingRecorder "SSA Construction" ssaElapsed
    if verbosity >= 2 then
        let t = System.Math.Round(ssaElapsed, 1)
        println $"        {t}ms"

    let mirOptions = buildMIROptimizeOptions options
    let mirPassLabel =
        formatPassGroup
            "MIR Optimizations"
            [
                ("const_folding", mirOptions.EnableConstFolding)
                ("cse", mirOptions.EnableCSE)
                ("copy_prop", mirOptions.EnableCopyProp)
                ("dce", mirOptions.EnableDCE)
                ("cfg_simplify", mirOptions.EnableCFGSimplify)
                ("licm", mirOptions.EnableLICM)
            ]
    if verbosity >= 1 then println $"  [mir.optimize] {mirPassLabel}{suffix}..."
    let mirOptStart = sw.Elapsed.TotalMilliseconds
    let optimizedProgram =
        if shouldRunMIROptimize mirOptions then
            let accumulatedTicks = Dictionary<string, int64>()
            let addTicks name ticks =
                match accumulatedTicks.TryGetValue name with
                | true, existing -> accumulatedTicks.[name] <- existing + ticks
                | false, _ -> accumulatedTicks.[name] <- ticks
            let phaseTickRecorder =
                passTimingRecorder |> Option.map (fun _ -> addTicks)
            let effectAnalysisStart = Stopwatch.GetTimestamp()
            let effectFreeFunctions =
                if mirOptions.EnableLICM || mirOptions.EnableCSE then
                    MIROptimizationFacts.analyzeEffectFreeFunctions
                        (let (MIR.Program (functions, _, _)) = ssaProgram in functions)
                else
                    Set.empty
            let effectAnalysisTicks =
                Stopwatch.GetTimestamp() - effectAnalysisStart
            addTicks "MIR Effect Analysis" effectAnalysisTicks
            let optimizeFunction func =
                let optimize () =
                    MIR_Optimize.optimizeFunctionWithEffectFreeCallsAndTickTrace
                        phaseTickRecorder
                        effectFreeFunctions
                        mirOptions
                        func
                let key = {
                    Function = func
                    Options = mirOptions
                    EffectFreeCalls =
                        MIROptimizationFacts.effectFreeCallsForFunction effectFreeFunctions func
                }
                match functionCaches with
                | Some caches -> caches.OptimizeMir key optimize
                | None -> optimize ()
            let (MIR.Program (functions, variants, records)) = ssaProgram
            let optimized =
                MIR.Program (functions |> List.map optimizeFunction, variants, records)
            match passTimingRecorder with
            | Some recorder ->
                let tickFrequency = float Stopwatch.Frequency
                for KeyValue (name, ticks) in accumulatedTicks do
                    recorder {
                        Pass = name
                        Elapsed = TimeSpan.FromMilliseconds(float ticks * 1000.0 / tickFrequency)
                    }
            | None -> ()
            optimized
        else
            ssaProgram
    let mirOptElapsed = sw.Elapsed.TotalMilliseconds - mirOptStart
    recordPassTiming passTimingRecorder "MIR Optimizations" mirOptElapsed
    if shouldDumpIR verbosity options.DumpMIR then
        printMIRProgram options "=== MIR (Control Flow Graph) ===" optimizedProgram
    if verbosity >= 2 then
        let t = System.Math.Round(mirOptElapsed, 1)
        println $"        {t}ms"

    if verbosity >= 1 then println $"  [lir.lower] MIR → LIR{suffix}..."
    let lirStart = sw.Elapsed.TotalMilliseconds
    let lirPhaseRecorder =
        passTimingRecorder
        |> Option.map (fun recorder ->
            fun name (elapsedMs: float) ->
                recorder {
                    Pass = name
                    Elapsed = TimeSpan.FromMilliseconds elapsedMs
                })
    let lirResult =
        MIR_to_LIR.toLIRFunctionsForWithTraceAndRcRegistries
            lirPhaseRecorder
            arch
            registries.RecordFieldsReg
            registries.RecordTypeParamsReg
            registries.RcSumShapeReg
            optimizedProgram
    match lirResult with
    | Error err -> Error $"LIR conversion error: {err}"
    | Ok lirFuncs ->
        let lirElapsed = sw.Elapsed.TotalMilliseconds - lirStart
        recordPassTiming passTimingRecorder "MIR -> LIR" lirElapsed
        let (MIR.Program (_, mirVariants, mirRecords)) = optimizedProgram
        let lirProgramForDump =
            lazy (
                let variants : LIR.VariantRegistry =
                    mirVariants
                    |> Map.map (fun _ typeVariants ->
                        {
                            LIR.TypeParams = typeVariants.TypeParams
                            LIR.Variants =
                                typeVariants.Variants
                                |> List.map (fun variant ->
                                    ({ Name = variant.Name
                                       Tag = variant.Tag
                                       Payload = variant.Payload } : LIR.VariantInfo))
                        })
                let records =
                    mirRecords
                    |> Map.map (fun _ fields ->
                        fields |> List.map (fun field -> (field.Name, field.Type)))
                LIR.Program (lirFuncs, variants, records))
        if shouldDumpIR verbosity options.DumpLIR then
            printLIRProgram options "=== LIR (Low-level IR with CFG) ===" lirProgramForDump.Value
        if verbosity >= 2 then
            let t = System.Math.Round(lirElapsed, 1)
            println $"        {t}ms"

        let lirPassLabel =
            formatPassGroup
                "LIR Peephole"
                [("peephole", not options.DisableLIROpt && not options.DisableLIRPeephole)]
        if verbosity >= 1 then println $"  [lir.peephole] {lirPassLabel}{suffix}..."
        let lirOptStart = sw.Elapsed.TotalMilliseconds
        let optimizedFuncs =
            if options.DisableLIROpt || options.DisableLIRPeephole then
                lirFuncs
            else
                lirFuncs
                |> LIR_Peephole.optimizeConstantReturnCallsInFunctions
                |> List.map (LIR_Peephole.optimizeFunctionFor arch)
        let lirOptElapsed = sw.Elapsed.TotalMilliseconds - lirOptStart
        recordPassTiming passTimingRecorder "LIR Peephole" lirOptElapsed
        if verbosity >= 2 then
            let t = System.Math.Round(lirOptElapsed, 1)
            println $"        {t}ms"
        // Summarize finalized symbolic LIR once. The facts remain attached to
        // functions through allocation and tree shaking, so each executable
        // only unions metadata for its reachable compilation unit.
        Ok (optimizedFuncs |> List.map LIR.attachFunctionCodegenFacts)

/// Allocate registers for one symbolic LIR function.
let private allocateRegistersForFunction
    (arch: Platform.Arch)
    (passTimingRecorder: PassTimingRecorder option)
    (func: LIR.Function)
    : LIR.Function =
    let allocatedFunc =
        match passTimingRecorder with
        | None -> RegisterAllocation.allocateRegisters arch func
        | Some recorder ->
            let (allocated, timings) =
                RegisterAllocation.allocateRegistersWithTiming arch func
            timings
            |> List.iter (fun timing ->
                recorder {
                    Pass = timing.Phase
                    Elapsed = TimeSpan.FromMilliseconds timing.ElapsedMs
                })
            allocated
    allocatedFunc |> LIR_Peephole.removeSelfMovesFromFunction

/// Run MIR+LIR passes (including register allocation) from ANF functions
let internal lowerToAllocatedLir
    (target: Platform.Target)
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (passTimingRecorder: PassTimingRecorder option)
    (functionCaches: FunctionCompilationCaches option)
    (releasePlanSummaryCache: ARM64CodeGenTypes.ReleasePlanSummaryCache option)
    (stageSuffix: string)
    (functions: ANF.Function list)
    (typeMap: ANF.TypeMap)
    (registries: AST_to_ANF.Registries)
    (projectedMirRegistries: (MIR.VariantRegistry * MIR.RecordRegistry) option)
    (externalReturnTypes: Map<AST.FunctionId, string * AST.SemanticType>)
    : Result<LIR.Function list, string> =

    let suffix = if stageSuffix = "" then "" else $" ({stageSuffix})"

    let functionOrder = functions |> List.map (fun f -> f.Name)
    // Function-affinity batches still call helpers compiled in sibling batches.
    // Keep the complete AOT return-type plan available while lowering each one.
    let returnTypeReg = ANF_to_MIR.buildReturnTypeReg functions externalReturnTypes
    let compileFunctions (functionsToCompile: ANF.Function list) : Result<LIR.Function list, string> =
        if List.isEmpty functionsToCompile then
            Ok []
        else
            if verbosity >= 1 then println $"  [mir.lower] ANF → MIR{suffix}..."
            let mirStart = sw.Elapsed.TotalMilliseconds
            let anfProgram = ANF.Program (functionsToCompile, ANF.Return ANF.UnitLiteral)
            let mirPhaseRecorder =
                passTimingRecorder
                |> Option.map (fun recorder ->
                    fun name (elapsedMs: float) ->
                        recorder {
                            Pass = name
                            Elapsed = TimeSpan.FromMilliseconds elapsedMs
                        })
            let mirResult =
                ANF_to_MIR.toMIRFunctionsOnlyWithTrace
                    mirPhaseRecorder
                    projectedMirRegistries
                    anfProgram
                    typeMap
                    registries.FuncParams
                    registries.VariantLookup
                    registries.RecordFieldsReg
                    options.EnableCoverage
                    returnTypeReg
            match mirResult with
            | Error err -> Error $"MIR conversion error: {err}"
            | Ok (mirFuncs, variantRegistry, mirRecordRegistry) ->
                let mirProgram = MIR.Program (mirFuncs, variantRegistry, mirRecordRegistry)
                let mirElapsed = sw.Elapsed.TotalMilliseconds - mirStart
                recordPassTiming passTimingRecorder "ANF -> MIR" mirElapsed
                if verbosity >= 2 then
                    let t = System.Math.Round(mirElapsed, 1)
                    println $"        {t}ms"
                compileMirToLir
                    (Platform.archFor target)
                    verbosity
                    options
                    sw
                    passTimingRecorder
                    functionCaches
                    registries
                    stageSuffix
                    mirProgram
                |> Result.bind (fun lirFuncs ->
                    let metadataPlanningStart = sw.Elapsed.TotalMilliseconds
                    let funcsPreparedForAllocation =
                        match Platform.archFor target with
                        | Platform.ARM64 ->
                            ARM64PrepareFunctions.prepareARM64FunctionsForAllocationWithCache
                                releasePlanSummaryCache
                                (passTimingRecorder
                                 |> Option.map (fun recorder ->
                                     fun name elapsedMs ->
                                         recorder {
                                             Pass = name
                                             Elapsed = TimeSpan.FromMilliseconds elapsedMs
                                         }))
                                registries.RecordFieldsReg
                                registries.RcSumShapeReg
                                lirFuncs
                        | Platform.X86_64 ->
                            lirFuncs
                    let metadataPlanningElapsed =
                        sw.Elapsed.TotalMilliseconds - metadataPlanningStart
                    recordPassTiming
                        passTimingRecorder
                        "ARM64 Function Metadata Planning"
                        metadataPlanningElapsed
                    if verbosity >= 1 then println "  [lir.allocate-registers] Register Allocation..."
                    let allocStart = sw.Elapsed.TotalMilliseconds
                    let arch = Platform.archFor target
                    let allocateFunction func =
                        let allocate () =
                            allocateRegistersForFunction
                                arch
                                passTimingRecorder
                                func
                        match functionCaches with
                        | Some caches -> caches.AllocateLir arch func allocate
                        | None -> allocate ()
                    let allocatedFuncs =
                        funcsPreparedForAllocation |> List.map allocateFunction
                    let allocElapsed = sw.Elapsed.TotalMilliseconds - allocStart
                    recordPassTiming passTimingRecorder "Register Allocation" allocElapsed
                    if verbosity >= 2 then
                        let t = System.Math.Round(allocElapsed, 1)
                        println $"        {t}ms"
                    Ok allocatedFuncs)

    let compileFunctionsWithTiming
        (label: string)
        (functionsToCompile: ANF.Function list)
        : Result<LIR.Function list, string> =
        if List.isEmpty functionsToCompile then
            Ok []
        else
            let startTime = sw.Elapsed.TotalMilliseconds
            compileFunctions functionsToCompile
            |> Result.map (fun compiled ->
                let elapsed = sw.Elapsed.TotalMilliseconds - startTime
                recordPassTiming passTimingRecorder label elapsed
                compiled)

    let (startFunctions, otherFunctions) =
        functions |> List.partition (fun func -> func.Name = "_start")

    let compileResult =
        match passTimingRecorder, startFunctions with
        | Some _, _ :: _ ->
            compileFunctionsWithTiming "Start Function Compilation" startFunctions
            |> Result.bind (fun compiledStart ->
                compileFunctions otherFunctions
                |> Result.map (fun compiledOther -> compiledStart @ compiledOther))
        | _ ->
            compileFunctions functions

    compileResult
    |> Result.map (fun compiledFuncs ->
        // Keep per-name queues so duplicate function names (e.g. lifted __closure_N from
        // different compilation units) preserve distinct bodies in original order.
        let compiledQueues : Map<string, LIR.Function list> =
            List.foldBack
                (fun (func: LIR.Function) (acc: Map<string, LIR.Function list>) ->
                    let existing = Map.tryFind func.Name acc |> Option.defaultValue []
                    Map.add func.Name (func :: existing) acc)
                compiledFuncs
                Map.empty

        let rec rebuildOrder
            (remainingNames: string list)
            (queues: Map<string, LIR.Function list>)
            (acc: LIR.Function list)
            : LIR.Function list =
            match remainingNames with
            | [] ->
                List.rev acc
            | name :: rest ->
                match Map.tryFind name queues with
                | Some (nextFunc :: remainingFuncs) ->
                    let queues' =
                        if List.isEmpty remainingFuncs then
                            Map.remove name queues
                        else
                            Map.add name remainingFuncs queues
                    rebuildOrder rest queues' (nextFunc :: acc)
                | _ ->
                    Crash.crash $"lowerToAllocatedLir: missing compiled function for '{name}'"

        rebuildOrder functionOrder compiledQueues [])
