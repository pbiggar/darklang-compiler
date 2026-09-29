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
            let callResult id =
                Map.tryFind id knownTypedConstants |> Option.map snd
            let functions =
                functions
                |> List.map (fun func ->
                    let cfg, changed =
                        MIRSparseConditionalConstants.applySparseConditionalConstantPropagationWithCallResults
                            callResult func.CFG
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
let internal lowerToAllocatedLirWithKnownGroups
    (externalSummaries: Map<AST.FunctionId, FunctionSummary>)
    (target: Platform.Target)
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (passTimingRecorder: PassTimingRecorder option)
    (functionCaches: FunctionCompilationCaches option)
    (releasePlanSummaryCache: ARM64CodeGenTypes.ReleasePlanSummaryCache option)
    (stageSuffix: string)
    (functionGroups: (SSAANF.Function list * ANF.TypeMap) list)
    (registries: AST_to_ANF.Registries)
    (projectedMirRegistries: (MIR.VariantRegistry * MIR.RecordRegistry) option)
    (externalReturnTypes: Map<AST.FunctionId, string * AST.SemanticType>)
    : Result<LIR.Function list * Map<AST.FunctionId, FunctionSummary>, string> =

    let suffix = if stageSuffix = "" then "" else $" ({stageSuffix})"

    let functions = functionGroups |> List.collect fst
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
                functionGroups
                |> List.fold (fun result (groupFunctions, typeMap) ->
                    result
                    |> Result.bind (fun accumulated ->
                        if List.isEmpty groupFunctions then Ok accumulated
                        else
                            ANF_to_MIR.toMIRSSAFunctionsOnlyWithTrace
                                mirPhaseRecorder
                                projectedMirRegistries
                                registries.RecursiveMembers
                                (not options.DisableTCO)
                                groupFunctions
                                typeMap
                                registries.FuncParams
                                registries.VariantLookup
                                registries.RecordFieldsReg
                                options.EnableCoverage
                                returnTypeReg
                                registries.FunctionNames
                            |> Result.map (fun (mir, variants, records) ->
                                let prior, _, _ = accumulated
                                prior @ mir, variants, records)))
                    (Ok ([], Map.empty, Map.empty))
            match mirResult with
            | Error err -> Error $"MIR conversion error: {err}"
            | Ok (mirFuncs, variantRegistry, mirRecordRegistry) ->
                if List.length mirFuncs <> List.length functionsToCompile then
                    Crash.crash "ANF to MIR did not emit exactly one node per function"
                let mirElapsed = sw.Elapsed.TotalMilliseconds - mirStart
                recordPassTiming passTimingRecorder "ANF -> MIR" mirElapsed
                if verbosity >= 2 then
                    let t = System.Math.Round(mirElapsed, 1)
                    println $"        {t}ms"
                let scheduleStart = sw.Elapsed.TotalMilliseconds
                let components = CallGraphSchedule.calleeFirst mirFuncs
                let expectedNodes = Set.ofList [0 .. List.length mirFuncs - 1]
                let scheduledNodes = components |> List.collect (fun group -> group.NodeIndices)
                if List.length scheduledNodes <> List.length mirFuncs
                   || Set.ofList scheduledNodes <> expectedNodes then
                    Crash.crash "Call graph did not schedule each MIR function exactly once"
                let pipelineStages =
                    Set.ofList ["ANF -> MIR"; "Purity"; "MIR Optimization";
                                "MIR -> LIR"; "LIR Peephole";
                                "Register Allocation"; "Clobber Summary"]
                let recordStages stages (group: CallGraphSchedule.Component) visits =
                    group.NodeIndices
                    |> List.fold (fun visits node ->
                        stages
                        |> List.fold (fun visits stage ->
                            let key = node, stage
                            if Set.contains key visits then
                                Crash.crash $"Pipeline stage {stage} ran twice for function node {node}"
                            Set.add key visits) visits) visits
                let initialVisits =
                    components
                    |> List.fold (fun visits group ->
                        recordStages ["ANF -> MIR"] group visits) Set.empty
                recordPassTiming
                    passTimingRecorder
                    "Call Graph Scheduling"
                    (sw.Elapsed.TotalMilliseconds - scheduleStart)
                let graphSetupStart = sw.Elapsed.TotalMilliseconds
                let localIds = mirFuncs |> List.map (fun func -> func.Id) |> Set.ofList
                let highestReservedId =
                    registries.FunctionNames
                    |> Map.fold
                        (fun highest id _ -> max highest id)
                        (Set.fold max (AST.functionId 0UL) localIds)
                let ambiguousLocalIds =
                    mirFuncs
                    |> List.countBy (fun func -> func.Id)
                    |> List.choose (fun (id, count) -> if count > 1 then Some id else None)
                    |> Set.ofList
                let directCalleeIds =
                    mirFuncs
                    |> List.fold (fun callees func ->
                        Set.union callees (CallGraphSchedule.directCallees func)) Set.empty
                // A current body supersedes a cached fact with the same ID.
                // Only direct external callees can affect this unit; their
                // summaries already include transitive facts.
                let externalSummaries =
                    Set.difference directCalleeIds localIds
                    |> Set.fold (fun summaries id ->
                        let summary =
                            Map.tryFind id externalSummaries
                            |> Option.defaultValue CompilationCacheIdentity.unknownSummary
                        Map.add id summary summaries) Map.empty
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
                recordPassTiming
                    passTimingRecorder
                    "Call Graph Setup"
                    (sw.Elapsed.TotalMilliseconds - graphSetupStart)
                let compileComponent
                    knownEffectFree
                    knownRemovable
                    knownTypedConstants
                    knownWrites
                    (catalog: Map<AST.FunctionId, FunctionSummary>)
                    (helperIds: Map<string, AST.FunctionId>)
                    (group: CallGraphSchedule.Component) =
                    let componentFuncs = group.Functions
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
                        let funcsPreparedForAllocation, helperIds =
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
                                    highestReservedId
                                    helperIds
                                    lirFuncs
                            | Platform.X86_64 ->
                                lirFuncs, helperIds
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
                        let callAwareStart = sw.Elapsed.TotalMilliseconds
                        let allocatedFuncs, callWrites =
                            let allWrites =
                                match arch with
                                | Platform.ARM64 -> ARM64CalleeClobbers.all
                                | Platform.X86_64 -> X64CalleeClobbers.all
                            let callWritesForSaves =
                                match arch with
                                | Platform.ARM64 -> ARM64CalleeClobbers.callWritesForSaves
                                | Platform.X86_64 -> X64CalleeClobbers.callWritesForSaves
                            // Calls introduced after MIR receive an explicit
                            // pessimistic clobber result. A unique local callee
                            // must still have been finalized by the scheduler.
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
                            let sccPeers =
                                group.SCCs
                                |> List.collect (fun scc ->
                                    let ids = scc |> List.map (fun func -> func.Id) |> Set.ofList
                                    scc |> List.map (fun func -> func.Id, ids))
                                |> Map.ofList
                            callEdges
                            |> Map.iter (fun caller calls ->
                                let peers = Map.tryFind caller sccPeers |> Option.defaultValue Set.empty
                                calls
                                |> Set.iter (fun callee ->
                                    if not (Set.contains caller ambiguousLocalIds)
                                       && Set.contains callee localIds
                                       && not (Set.contains callee ambiguousLocalIds)
                                       && not (Set.contains callee peers) then
                                        match Map.tryFind callee catalog with
                                        | Some summary ->
                                            let hasTargetWrites =
                                                match arch with
                                                | Platform.ARM64 -> Option.isSome summary.Arm64Writes
                                                | Platform.X86_64 -> Option.isSome summary.X64Writes
                                            match summary.Version with
                                            | Some version when version.Function = callee
                                                                && version.Target = target
                                                                && hasTargetWrites -> ()
                                            | _ ->
                                                Crash.crash
                                                    $"LIR introduced a call from {caller} before local callee {callee} was finalized"
                                        | None ->
                                            Crash.crash
                                                $"LIR introduced a call from {caller} before local callee {callee} was scheduled"))
                            let callees =
                                callEdges
                                |> Map.fold (fun writes _ calls ->
                                    calls
                                    |> Set.fold (fun writes callee ->
                                        if Map.containsKey callee writes then writes
                                        else Map.add callee allWrites writes) writes) knownWrites
                            let rec canReach target seen current =
                                if current = target then true
                                elif Set.contains current seen then false
                                else
                                    let next =
                                        Map.tryFind current callEdges |> Option.defaultValue Set.empty
                                    next
                                    |> Set.exists (canReach target (Set.add current seen))
                            let allocated =
                                funcsPreparedForAllocation
                                |> List.map (fun (prepared: LIR.Function) ->
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
                                                     |> Option.defaultWith (fun () ->
                                                         Crash.crash
                                                             $"Call graph has no LIR clobber summary for {prepared.Name}'s callee {id}")))
                                            |> Map.ofSeq
                                        let hasPreservedCallerReg =
                                            prepared.CFG.Blocks
                                            |> Map.exists (fun _ block ->
                                                callWritesForSaves relevantCallees block
                                                |> List.exists (fun writes ->
                                                    (RegisterPolicy.callerSavedRegs
                                                     |> List.exists (fun reg ->
                                                         not (ARM64CalleeClobbers.containsInt reg writes)))
                                                        || (FloatAllocation.floatCallerSavedRegsFor arch
                                                            |> List.exists (fun reg ->
                                                                not (ARM64CalleeClobbers.containsFloat reg writes)))))
                                        let allocated =
                                            if hasPreservedCallerReg then
                                                let allocate () =
                                                    RegisterAllocation.allocateRegistersWithCallSummaries
                                                        arch relevantCallees prepared
                                                    |> LIR_Peephole.removeSelfMovesFromFunction
                                                match functionCaches with
                                                | Some caches ->
                                                    caches.AllocateCallAwareLir
                                                        prepared relevantCallees allocate
                                                | None -> allocate ()
                                            else allocateFunction prepared
                                        match arch with
                                        | Platform.ARM64 -> allocated
                                        | Platform.X86_64 ->
                                            X64CalleeClobbers.pruneFunction relevantCallees allocated)
                            allocated, callees
                        recordPassTiming
                            passTimingRecorder
                            "Call-aware Allocation and Save Pruning"
                            (sw.Elapsed.TotalMilliseconds - callAwareStart)
                        let allocElapsed = sw.Elapsed.TotalMilliseconds - allocStart
                        recordPassTiming passTimingRecorder "Register Allocation" allocElapsed
                        if verbosity >= 2 then
                            let t = System.Math.Round(allocElapsed, 1)
                            println $"        {t}ms"
                        Ok (allocatedFuncs, typedConstants, callWrites, helperIds))
                let rec compile
                    knownPurity
                    knownEffectFree
                    knownRemovable
                    knownTypedConstants
                    knownWrites
                    (published: Map<AST.FunctionId, FunctionSummary>)
                    (catalog: Map<AST.FunctionId, FunctionSummary>)
                    (helperIds: Map<string, AST.FunctionId>)
                    (completed: LIR.Function list list)
                    visits
                    (remaining: CallGraphSchedule.Component list) =
                    match remaining with
                    | [] ->
                        let expectedVisits =
                            expectedNodes
                            |> Set.fold (fun visits node ->
                                pipelineStages
                                |> Set.fold (fun visits stage ->
                                    Set.add (node, stage) visits) visits) Set.empty
                        if visits <> expectedVisits then
                            Crash.crash "A function skipped a native compiler pipeline stage"
                        Ok (completed |> List.rev |> List.concat, published)
                    | group :: rest ->
                        // Same-SCC calls cannot yet have final allocation facts.
                        // Every other unique local edge must resolve to a
                        // completed function, rather than silently taking the
                        // same fallback as an unavailable external callee.
                        group.SCCs
                        |> List.iter (fun scc ->
                            let sccIds = scc |> List.map (fun func -> func.Id) |> Set.ofList
                            scc
                            |> List.iter (fun func ->
                                CallGraphSchedule.directCallees func
                                |> Set.iter (fun callee ->
                                    if not (Set.contains callee sccIds) then
                                        let summary =
                                            Map.tryFind callee catalog
                                            |> Option.defaultWith (fun () ->
                                                Crash.crash
                                                    $"Call graph has no summary for {func.Name}'s callee {callee}")
                                        match summary.Version with
                                        | Some version when version.Function <> callee
                                                            || version.Target <> target ->
                                            Crash.crash
                                                $"Call graph has a mismatched version for {func.Name}'s callee {callee}"
                                        | _ -> ()
                                        if Set.contains callee localIds
                                           && not (Set.contains callee ambiguousLocalIds) then
                                            let hasTargetWrites =
                                                match Platform.archFor target with
                                                | Platform.ARM64 -> Option.isSome summary.Arm64Writes
                                                | Platform.X86_64 -> Option.isSome summary.X64Writes
                                            match summary.Version with
                                            | Some version when version.Function = callee
                                                                && version.Target = target
                                                                && hasTargetWrites -> ()
                                            | _ ->
                                                Crash.crash
                                                    $"Call graph scheduled {func.Name} before finalized callee {callee}"
                                        if not (Set.contains callee ambiguousLocalIds) then
                                            let targetWrites =
                                                match Platform.archFor target with
                                                | Platform.ARM64 -> summary.Arm64Writes
                                                | Platform.X86_64 -> summary.X64Writes
                                            let purityMatches =
                                                Map.tryFind callee knownPurity = Some summary.Purity
                                            let constantMatches =
                                                Map.tryFind callee knownTypedConstants = summary.ConstantReturn
                                            let fullWrites =
                                                match Platform.archFor target with
                                                | Platform.ARM64 -> ARM64CalleeClobbers.all
                                                | Platform.X86_64 -> X64CalleeClobbers.all
                                            let writesMatch =
                                                let current =
                                                    Map.tryFind callee knownWrites
                                                    |> Option.defaultValue fullWrites
                                                let saved = targetWrites |> Option.defaultValue fullWrites
                                                current = saved
                                            if not (purityMatches && constantMatches && writesMatch) then
                                                Crash.crash
                                                    $"Call graph facts for {func.Name}'s callee {callee} disagree with its saved summary (purity={purityMatches}, constant={constantMatches}, writes={writesMatch})")))
                        let purityStart = sw.Elapsed.TotalMilliseconds
                        let purity =
                            MIROptimizationFacts.analyzePurityWithKnown
                                knownPurity group.Functions
                            |> Map.map (fun id summary ->
                                if Set.contains id ambiguousLocalIds then
                                    MIROptimizationFacts.unknownPurity
                                else summary)
                        let visits = recordStages ["Purity"] group visits
                        recordPassTiming
                            passTimingRecorder
                            "Call Graph Purity Summary"
                            (sw.Elapsed.TotalMilliseconds - purityStart)
                        let effectFree =
                            MIROptimizationFacts.analyzeEffectFreeFunctionsWithKnown
                                knownEffectFree group.Functions
                        let batchEffectFree =
                            effectFree |> Set.fold (fun known id -> Set.add id known) knownEffectFree
                        let newlyRemovable =
                            purity
                            |> Map.toList
                            |> List.choose (fun (id, summary) ->
                                if MIROptimizationFacts.isPure summary then Some id else None)
                            |> Set.ofList
                        compileComponent
                            batchEffectFree
                            knownRemovable
                            knownTypedConstants knownWrites catalog helperIds group
                        |> Result.bind (fun (allocated, typedConstants, callWrites, helperIds) ->
                            let visits =
                                recordStages
                                    ["MIR Optimization"; "MIR -> LIR";
                                     "LIR Peephole"; "Register Allocation"]
                                    group visits
                            let scheduledIds =
                                group.Functions
                                |> List.countBy (fun func -> func.Id)
                                |> Map.ofList
                            let emittedIds =
                                allocated
                                |> List.countBy (fun (func: LIR.Function) -> func.Id)
                                |> Map.ofList
                            if scheduledIds <> emittedIds then
                                Crash.crash "Call graph batch did not emit every scheduled function"
                            let clobberStart = sw.Elapsed.TotalMilliseconds
                            let localWrites =
                                (match Platform.archFor target with
                                 | Platform.ARM64 ->
                                     ARM64CalleeClobbers.summariesWithKnown callWrites allocated
                                 | Platform.X86_64 ->
                                     X64CalleeClobbers.summariesWithKnown callWrites allocated)
                                |> Map.filter (fun id _ -> not (Set.contains id ambiguousLocalIds))
                            let knownWrites =
                                localWrites
                                |> Map.fold (fun writes id value -> Map.add id value writes) knownWrites
                            let visits = recordStages ["Clobber Summary"] group visits
                            recordPassTiming
                                passTimingRecorder
                                "Call Graph Clobber Summary"
                                (sw.Elapsed.TotalMilliseconds - clobberStart)
                            let knownTypedConstants =
                                typedConstants
                                |> Map.fold (fun known id value ->
                                    if Set.contains id ambiguousLocalIds then known
                                    else Map.add id value known)
                                    knownTypedConstants
                            let batchSummaries =
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
                                            | Platform.ARM64 when Set.contains func.Id ambiguousLocalIds -> None
                                            | Platform.ARM64 ->
                                                Map.tryFind func.Id knownWrites
                                                |> Option.defaultWith (fun () ->
                                                    Crash.crash "Final ARM64 function lacks a clobber summary")
                                                |> Some
                                            | Platform.X86_64 -> None
                                        X64Writes =
                                            match Platform.archFor target with
                                            | Platform.X86_64 when Set.contains func.Id ambiguousLocalIds -> None
                                            | Platform.X86_64 ->
                                                Map.tryFind func.Id knownWrites
                                                |> Option.defaultWith (fun () ->
                                                    Crash.crash "Final x64 function lacks a clobber summary")
                                                |> Some
                                            | Platform.ARM64 -> None
                                    }
                                    CompilationCacheIdentity.mergeFunctionSummaries
                                        summaries (Map.ofList [func.Id, summary])) Map.empty
                            let published =
                                CompilationCacheIdentity.mergeFunctionSummaries
                                    published batchSummaries
                            let catalog =
                                CompilationCacheIdentity.mergeFunctionSummaries
                                    catalog batchSummaries
                            compile
                                (purity
                                 |> Map.fold (fun known id value ->
                                     if Set.contains id ambiguousLocalIds then known
                                     else Map.add id value known) knownPurity)
                                (effectFree
                                 |> Set.fold (fun known id ->
                                     if Set.contains id ambiguousLocalIds then known
                                     else Set.add id known) knownEffectFree)
                                (newlyRemovable
                                 |> Set.fold (fun known id ->
                                     if Set.contains id ambiguousLocalIds then known
                                     else Set.add id known) knownRemovable)
                                knownTypedConstants
                                knownWrites
                                published
                                catalog
                                helperIds
                                (allocated :: completed)
                                visits
                                rest)
                let initialFactsStart = sw.Elapsed.TotalMilliseconds
                let initialPurity =
                    externalSummaries |> Map.map (fun _ summary -> summary.Purity)
                let initialRemovable =
                    initialPurity
                    |> Map.toList
                    |> List.choose (fun (id, summary) ->
                        if MIROptimizationFacts.isPure summary then Some id else None)
                    |> Set.ofList
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
                let initialCatalog =
                    ambiguousLocalIds
                    |> Set.fold (fun summaries id ->
                        Map.add id CompilationCacheIdentity.unknownSummary summaries)
                        externalSummaries
                recordPassTiming
                    passTimingRecorder
                    "Call Graph Initial Facts"
                    (sw.Elapsed.TotalMilliseconds - initialFactsStart)
                compile
                    initialPurity initialRemovable initialRemovable
                    initialTypedConstants initialWrites
                    Map.empty initialCatalog Map.empty [] initialVisits components

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
    lowerToAllocatedLirWithKnownGroups
        externalSummaries target verbosity options sw passTimingRecorder
        functionCaches releasePlanSummaryCache stageSuffix
        [functions, typeMap] registries projectedMirRegistries externalReturnTypes
