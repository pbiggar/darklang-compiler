// NativePipeline.fs - Compile direct-call components callee-first.

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

/// Run MIR/LIR optimizations on SSA MIR, returning an optimized LIR program.
let private compileMirToLir
    (arch: Platform.Arch)
    (knownEffectFree: Set<AST.FunctionId>)
    (knownRemovable: Set<AST.FunctionId>)
    (knownTypedConstants: Map<AST.FunctionId, AST.SemanticType * MIR.Operand>)
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (passTimingRecorder: PassTimingRecorder option)
    (functionCaches: FunctionCompilationCaches option)
    (registries: AST_to_ANF.Registries)
    (stageSuffix: string)
    (mirProgram: MIR.Program)
    : Result<LIR.Function list * Map<AST.FunctionId, AST.SemanticType * MIR.Operand>, string> =

    let suffix = if stageSuffix = "" then "" else $" ({stageSuffix})"

    let ssaProgram =
        let (MIR.Program (functions, variants, records)) = mirProgram
        let rewriteCall instr =
            match instr with
            | MIR.Call (dest, callee, [], [], returnType) when Set.contains callee knownRemovable ->
                match Map.tryFind callee knownTypedConstants with
                | Some (typ, (MIR.Int64Const _ | MIR.BoolConst _ | MIR.FloatSymbol _ as value))
                    when typ = returnType ->
                    MIR.Mov (dest, value, Some returnType)
                | _ -> instr
            | _ -> instr
        MIR.Program (
            functions
            |> List.map (fun func ->
                let blocks =
                    func.CFG.Blocks
                    |> Map.map (fun _ block ->
                        { block with Instrs = block.Instrs |> List.map rewriteCall })
                { func with CFG = { func.CFG with Blocks = blocks } }),
            variants, records)

    let mirOptions = buildMIROptimizeOptions options
    let mirPassLabel =
        formatPassGroup
            "MIR Optimizations"
            [
                ("sccp", mirOptions.EnableSCCP)
                ("cse", mirOptions.EnableCSE)
                ("dce", mirOptions.EnableDCE)
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
                    knownEffectFree
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
    let optimizedProgram =
        if mirOptions.EnableSCCP && not (Map.isEmpty knownTypedConstants) then
            let (MIR.Program (functions, variants, records)) = optimizedProgram
            let callResults = knownTypedConstants |> Map.map (fun _ (_, value) -> value)
            let functions =
                functions
                |> List.map (fun func ->
                    let cfg, changed =
                        MIRSparseConditionalConstants.applySparseConditionalConstantPropagationWithCallResults
                            callResults func.CFG
                    if changed then { func with CFG = cfg } else func)
            MIR.Program (functions, variants, records)
        else optimizedProgram
    let typedConstants =
        let (MIR.Program (functions, _, _)) = optimizedProgram
        functions
        |> List.choose (fun func ->
            MIR_Optimize.constantReturnOperand func
            |> Option.map (fun value -> func.Id, (func.ReturnType, value)))
        |> Map.ofList
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
                                       Payload = variant.Payload
                                       FieldCount = variant.FieldCount } : LIR.VariantInfo))
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
                lirFuncs |> List.map (LIR_Peephole.optimizeFunctionFor arch)
        let lirOptElapsed = sw.Elapsed.TotalMilliseconds - lirOptStart
        recordPassTiming passTimingRecorder "LIR Peephole" lirOptElapsed
        if verbosity >= 2 then
            let t = System.Math.Round(lirOptElapsed, 1)
            println $"        {t}ms"
        // Summarize finalized symbolic LIR once. The facts remain attached to
        // functions through allocation and tree shaking, so each executable
        // only unions metadata for its reachable compilation unit.
        Ok (optimizedFuncs |> List.map LIR.attachFunctionCodegenFacts, typedConstants)

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

/// Run MIR+LIR passes (including register allocation) from SSA ANF functions.
let internal lowerToAllocatedLirWithKnown
    (externalSummaries: Map<AST.FunctionId, FunctionSummary>)
    (target: Platform.Target)
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (passTimingRecorder: PassTimingRecorder option)
    (functionCaches: FunctionCompilationCaches option)
    (releasePlanSummaryCache: ARM64CodeGenTypes.ReleasePlanSummaryCache option)
    (stageSuffix: string)
    (functions: SSAANF.Function list)
    (typeMap: ANF.TypeMap)
    (registries: AST_to_ANF.Registries)
    (projectedMirRegistries: (MIR.VariantRegistry * MIR.RecordRegistry) option)
    (externalReturnTypes: Map<AST.FunctionId, string * AST.SemanticType>)
    : Result<LIR.Function list * Map<AST.FunctionId, FunctionSummary>, string> =

    let suffix = if stageSuffix = "" then "" else $" ({stageSuffix})"

    let functionOrder = functions |> List.map (fun f -> f.Name)
    // Function-affinity batches still call helpers compiled in sibling batches.
    // Keep the complete AOT return-type plan available while lowering each one.
    let returnTypeReg =
        externalReturnTypes
        |> Map.map (fun _ (_, typ) -> typ)
        |> fun external ->
            functions
            |> List.fold (fun types func -> Map.add func.Id func.ReturnType types) external
    let compileFunctions
        (functionsToCompile: SSAANF.Function list)
        : Result<LIR.Function list * Map<AST.FunctionId, FunctionSummary>, string> =
        if List.isEmpty functionsToCompile then
            Ok ([], Map.empty)
        else
            if verbosity >= 1 then println $"  [mir.lower] ANF → MIR{suffix}..."
            let mirStart = sw.Elapsed.TotalMilliseconds
            let mirPhaseRecorder =
                passTimingRecorder
                |> Option.map (fun recorder ->
                    fun name (elapsedMs: float) ->
                        recorder {
                            Pass = name
                            Elapsed = TimeSpan.FromMilliseconds elapsedMs
                        })
            let mirResult =
                ANF_to_MIR.toMIRSSAFunctionsOnlyWithTrace
                    mirPhaseRecorder
                    projectedMirRegistries
                    registries.RecursiveMembers
                    (not options.DisableTCO)
                    functionsToCompile
                    typeMap
                    registries.FuncParams
                    registries.VariantLookup
                    registries.RecordFieldsReg
                    options.EnableCoverage
                    returnTypeReg
            match mirResult with
            | Error err -> Error $"MIR conversion error: {err}"
            | Ok (mirFuncs, variantRegistry, mirRecordRegistry) ->
                let mirElapsed = sw.Elapsed.TotalMilliseconds - mirStart
                recordPassTiming passTimingRecorder "ANF -> MIR" mirElapsed
                if verbosity >= 2 then
                    let t = System.Math.Round(mirElapsed, 1)
                    println $"        {t}ms"
                let scheduleStart = sw.Elapsed.TotalMilliseconds
                let components = CallGraphSchedule.calleeFirst mirFuncs
                recordPassTiming
                    passTimingRecorder
                    "Call Graph Scheduling"
                    (sw.Elapsed.TotalMilliseconds - scheduleStart)
                let localIds = mirFuncs |> List.map (fun func -> func.Id) |> Set.ofList
                let ambiguousLocalIds =
                    mirFuncs
                    |> List.countBy (fun func -> func.Id)
                    |> List.choose (fun (id, count) -> if count > 1 then Some id else None)
                    |> Set.ofList
                // A current body always supersedes a cached fact with the same
                // canonical ID, including a definition from another unit.
                let externalSummaries =
                    externalSummaries
                    |> Map.filter (fun id _ -> not (Set.contains id localIds))
                if verbosity >= 2 then
                    let directCalls =
                        mirFuncs
                        |> List.collect (fun func ->
                            func.CFG.Blocks
                            |> Map.toList
                            |> List.collect (fun (_, block) ->
                                block.Instrs
                                |> List.choose (function
                                    | MIR.Call (_, id, _, _, _)
                                    | MIR.TailCall (id, _, _, _) -> Some id
                                    | _ -> None)))
                    let ambiguous, known, unresolved =
                        directCalls
                        |> List.fold (fun (ambiguous, known, unresolved) id ->
                            if Set.contains id ambiguousLocalIds then
                                ambiguous + 1, known, unresolved
                            elif Set.contains id localIds
                                 || (Map.tryFind id externalSummaries
                                     |> Option.bind (fun summary -> summary.Version)
                                     |> Option.isSome) then
                                ambiguous, known + 1, unresolved
                            else
                                ambiguous, known, unresolved + 1)
                            (0, 0, 0)
                    println
                        $"  [callgraph] functions={List.length mirFuncs} batches={List.length components} direct={List.length directCalls} known={known} ambiguous={ambiguous} unresolved={unresolved}"
                let compileComponent
                    knownEffectFree
                    knownRemovable
                    knownTypedConstants
                    knownWrites
                    (componentFuncs: MIR.Function list) =
                    let mirProgram =
                        MIR.Program (componentFuncs, variantRegistry, mirRecordRegistry)
                    compileMirToLir
                        (Platform.archFor target)
                        knownEffectFree
                        knownRemovable
                        knownTypedConstants
                        verbosity
                        options
                        sw
                        passTimingRecorder
                        functionCaches
                        registries
                        stageSuffix
                        mirProgram
                    |> Result.bind (fun (lirFuncs, typedConstants) ->
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
                        let callAwareStart = sw.Elapsed.TotalMilliseconds
                        let allocatedFuncs =
                            let allWrites =
                                match arch with
                                | Platform.ARM64 -> ARM64CalleeClobbers.all
                                | Platform.X86_64 -> X64CalleeClobbers.all
                            let callWritesForSaves =
                                match arch with
                                | Platform.ARM64 -> ARM64CalleeClobbers.callWritesForSaves
                                | Platform.X86_64 -> X64CalleeClobbers.callWritesForSaves
                            // Batches contain no callee edge between different
                            // SCCs. Internal recursive calls use the full ABI
                            // envelope below; later MIR/LIR edges absent from
                            // the schedule also remain unknown here.
                            let callees = knownWrites
                            let callEdges =
                                funcsPreparedForAllocation
                                |> List.fold (fun edges (func: LIR.Function) ->
                                    let calls =
                                        func.CFG.Blocks
                                        |> Map.toList
                                        |> List.collect (fun (_, block) ->
                                            block.Instrs
                                            |> List.choose (function
                                                | LIR.Call (_, id, _)
                                                | LIR.TailCall (id, _) -> Some id
                                                | _ -> None))
                                        |> Set.ofList
                                    let existing =
                                        Map.tryFind func.Id edges |> Option.defaultValue Set.empty
                                    Map.add func.Id (Set.union existing calls) edges) Map.empty
                            let rec canReach target seen current =
                                if current = target then true
                                elif Set.contains current seen then false
                                else
                                    let next =
                                        Map.tryFind current callEdges |> Option.defaultValue Set.empty
                                    next
                                    |> Set.exists (canReach target (Set.add current seen))
                            List.map2
                                (fun (prepared: LIR.Function) (allocated: LIR.Function) ->
                                    let directCallees =
                                        prepared.CFG.Blocks
                                        |> Map.toList
                                        |> List.collect (fun (_, block) ->
                                            block.Instrs
                                            |> List.choose (function
                                                | LIR.Call (_, id, _) -> Some id
                                                | _ -> None))
                                        |> Set.ofList
                                    let relevantCallees =
                                        directCallees
                                        |> Seq.map (fun id ->
                                            id,
                                            (if canReach prepared.Id Set.empty id then
                                                 allWrites
                                             else
                                                 Map.tryFind id callees
                                                 |> Option.defaultValue allWrites))
                                        |> Map.ofSeq
                                    let hasPreservedCallerReg =
                                        prepared.CFG.Blocks
                                        |> Map.exists (fun _ block ->
                                            callWritesForSaves relevantCallees block
                                            |> List.exists (fun writes ->
                                                (RegisterPolicy.callerSavedRegs
                                                 |> List.exists (fun reg ->
                                                     not (Set.contains reg writes.Ints)))
                                                    || (FloatAllocation.floatCallerSavedRegsFor arch
                                                        |> List.exists (fun reg ->
                                                            not (Set.contains reg writes.Floats)))))
                                    let hasCallLiveValue =
                                        not (List.isEmpty allocated.UsedCalleeSaved)
                                        || (allocated.CFG.Blocks
                                            |> Map.exists (fun _ block ->
                                                block.Instrs
                                                |> List.exists (function
                                                    | LIR.SaveRegs (ints, floats) ->
                                                        not (List.isEmpty ints && List.isEmpty floats)
                                                    | _ -> false)))
                                    let allocated =
                                        if hasPreservedCallerReg && hasCallLiveValue then
                                            let allocate () =
                                                RegisterAllocation.allocateRegistersWithCallSummaries
                                                    arch relevantCallees prepared
                                                |> LIR_Peephole.removeSelfMovesFromFunction
                                            match functionCaches with
                                            | Some caches ->
                                                caches.AllocateCallAwareLir
                                                    allocated relevantCallees allocate
                                            | None -> allocate ()
                                        else allocated
                                    match arch with
                                    | Platform.ARM64 -> allocated
                                    | Platform.X86_64 ->
                                        X64CalleeClobbers.pruneFunction relevantCallees allocated)
                                funcsPreparedForAllocation
                                allocatedFuncs
                        recordPassTiming
                            passTimingRecorder
                            "Call-aware Allocation and Save Pruning"
                            (sw.Elapsed.TotalMilliseconds - callAwareStart)
                        let allocElapsed = sw.Elapsed.TotalMilliseconds - allocStart
                        recordPassTiming passTimingRecorder "Register Allocation" allocElapsed
                        if verbosity >= 2 then
                            let t = System.Math.Round(allocElapsed, 1)
                            println $"        {t}ms"
                        Ok (allocatedFuncs, typedConstants))
                let rec compile
                    knownPurity
                    knownLocalEffectFree
                    knownTypedConstants
                    knownWrites
                    published
                    (completed: LIR.Function list list)
                    (remaining: CallGraphSchedule.Component list) =
                    match remaining with
                    | [] -> Ok (completed |> List.rev |> List.concat, published)
                    | group :: rest ->
                        let purityStart = sw.Elapsed.TotalMilliseconds
                        let purity =
                            MIROptimizationFacts.analyzePurityWithKnown
                                knownPurity group.Functions
                            |> Map.map (fun id summary ->
                                if Set.contains id ambiguousLocalIds then
                                    MIROptimizationFacts.unknownPurity
                                else summary)
                        recordPassTiming
                            passTimingRecorder
                            "Call Graph Purity Summary"
                            (sw.Elapsed.TotalMilliseconds - purityStart)
                        let externalPure =
                            externalSummaries
                            |> Map.toList
                            |> List.choose (fun (id, summary) ->
                                if MIROptimizationFacts.isPure summary.Purity then Some id else None)
                            |> Set.ofList
                        let knownEffectFree = Set.union knownLocalEffectFree externalPure
                        let effectFree =
                            MIROptimizationFacts.analyzeEffectFreeFunctionsWithKnown
                                knownEffectFree group.Functions
                        let knownRemovable =
                            knownPurity
                            |> Map.toList
                            |> List.choose (fun (id, summary) ->
                                if MIROptimizationFacts.isPure summary then Some id else None)
                            |> Set.ofList
                        compileComponent
                            (Set.union knownEffectFree effectFree)
                            knownRemovable
                            knownTypedConstants knownWrites group.Functions
                        |> Result.bind (fun (allocated, typedConstants) ->
                            let clobberStart = sw.Elapsed.TotalMilliseconds
                            let knownWrites =
                                (match Platform.archFor target with
                                 | Platform.ARM64 ->
                                     ARM64CalleeClobbers.summariesWithKnown knownWrites allocated
                                 | Platform.X86_64 ->
                                     X64CalleeClobbers.summariesWithKnown knownWrites allocated)
                                |> Map.filter (fun id _ -> not (Set.contains id ambiguousLocalIds))
                            recordPassTiming
                                passTimingRecorder
                                "Call Graph Clobber Summary"
                                (sw.Elapsed.TotalMilliseconds - clobberStart)
                            let knownTypedConstants =
                                typedConstants
                                |> Map.fold (fun known id value -> Map.add id value known)
                                    knownTypedConstants
                                |> Map.filter (fun id _ -> not (Set.contains id ambiguousLocalIds))
                            let published =
                                allocated
                                |> List.fold (fun summaries (func: LIR.Function) ->
                                    let summary = {
                                        Version =
                                            Some (FunctionVersion(
                                                stageSuffix, func.Id, target, options, func))
                                        Purity =
                                            Map.tryFind func.Id purity
                                            |> Option.defaultWith (fun () ->
                                                Crash.crash "Compiled function lacks a purity summary")
                                        ConstantReturn = Map.tryFind func.Id typedConstants
                                        Arm64Writes =
                                            match Platform.archFor target with
                                            | Platform.ARM64 -> Map.tryFind func.Id knownWrites
                                            | Platform.X86_64 -> None
                                        X64Writes =
                                            match Platform.archFor target with
                                            | Platform.X86_64 -> Map.tryFind func.Id knownWrites
                                            | Platform.ARM64 -> None
                                    }
                                    CompilationCacheIdentity.mergeFunctionSummaries
                                        summaries (Map.ofList [func.Id, summary])) published
                            compile
                                (purity
                                 |> Map.fold (fun known id value -> Map.add id value known) knownPurity
                                 |> Map.filter (fun id _ -> not (Set.contains id ambiguousLocalIds)))
                                (Set.union knownLocalEffectFree effectFree
                                 |> Set.filter (fun id -> not (Set.contains id ambiguousLocalIds)))
                                knownTypedConstants
                                knownWrites
                                published
                                (allocated :: completed)
                                rest)
                let initialPurity =
                    externalSummaries |> Map.map (fun _ summary -> summary.Purity)
                let initialTypedConstants =
                    externalSummaries
                    |> Map.toList
                    |> List.choose (fun (id, summary) ->
                        summary.ConstantReturn |> Option.map (fun value -> id, value))
                    |> Map.ofList
                let initialWrites =
                    externalSummaries
                    |> Map.toList
                    |> List.choose (fun (id, summary) ->
                        let writes =
                            match Platform.archFor target with
                            | Platform.ARM64 -> summary.Arm64Writes
                            | Platform.X86_64 -> summary.X64Writes
                        writes |> Option.map (fun value -> id, value))
                    |> Map.ofList
                compile
                    initialPurity Set.empty initialTypedConstants initialWrites
                    Map.empty [] components

    let compileFunctionsWithTiming
        (label: string)
        (functionsToCompile: SSAANF.Function list)
        : Result<LIR.Function list * Map<AST.FunctionId, FunctionSummary>, string> =
        if List.isEmpty functionsToCompile then
            Ok ([], Map.empty)
        else
            let startTime = sw.Elapsed.TotalMilliseconds
            compileFunctions functionsToCompile
            |> Result.map (fun compiled ->
                let elapsed = sw.Elapsed.TotalMilliseconds - startTime
                recordPassTiming passTimingRecorder label elapsed
                compiled)

    let compileResult =
        compileFunctionsWithTiming "Call Graph Compilation" functions

    compileResult
    |> Result.map (fun (compiledFuncs, summaries) ->
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

        rebuildOrder functionOrder compiledQueues [], summaries)
