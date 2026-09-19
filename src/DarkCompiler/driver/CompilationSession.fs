// CompilationSession.fs - Own bounded compilation caches and their explicit session lifetime.

module CompilationSession

open ARM64CodeGenTypes
open CodeGen
open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Collections.Generic
open CompilerOptions
open CompilationCacheIdentity

type CompilationSession(collectCodegenMetrics: bool) =
    let jsonPlanning = new JsonPlanning.PlanningSession()
    let anfDependenciesByContext =
        Dictionary<
            obj,
            Dictionary<
                AnfDependencyKey,
                Result<ANF.Function list * ANF.VarGen * obj, string>>>(ObjectReferenceComparer())
    let compiledDependenciesByIdentity =
        Dictionary<
            obj,
            Dictionary<
                CompiledDependencyConfig,
                Result<LIR.Function list, string>>>(ObjectReferenceComparer())
    let compiledStartFunctions =
        Dictionary<StartCompilationConfig, Result<LIR.Function list, string>>()
    let ssaFunctionsByExactInput =
        Dictionary<MIR.Function, MIR.Function>(MirFunctionNameHashComparer())
    let ssaFunctions =
        Dictionary<MIR.Function, MIR.Function>(MirFunctionNameHashComparer())
    let optimizedMirFunctions =
        Dictionary<MirOptimizationKey, MIR.Function>(MirOptimizationKeyNameHashComparer())
    let allocatedLirFunctions =
        Dictionary<AllocatedLirFunctionKey, LIR.Function>(AllocatedLirFunctionKeyNameHashComparer())
    let reachableStdlibFunctionsByContext =
        Dictionary<
            obj,
            Dictionary<Set<AST.FunctionId>, LIR.Function list>>(ObjectReferenceComparer())
    let stdlibFunctionInventoryByContext =
        Dictionary<
            obj,
            Dictionary<AST.FunctionId, struct (int * LIR.Function)>>(ObjectReferenceComparer())
    let reachableStdlibNamesByRootAndContext =
        Dictionary<
            obj,
            Dictionary<AST.FunctionId, Set<AST.FunctionId>>>(ObjectReferenceComparer())
    let mirRegistriesByContext =
        Dictionary<
            obj,
            MIR.VariantRegistry * MIR.RecordRegistry>(ObjectReferenceComparer())
    let arm64MetadataGroupsByContext =
        Dictionary<
            obj,
            Dictionary<
                Arm64MetadataGroupKey,
                ARM64CodeGenTypes.Arm64ProgramMetadata>>(ObjectReferenceComparer())
    let arm64FunctionGroupsByContext =
        Dictionary<
            obj,
            Dictionary<
                ARM64.TargetConfig * ARM64CodeGenTypes.CodeGenOptions,
                Result<CodeGen.GeneratedChunk list, string>>>(ObjectReferenceComparer())
    let arm64FunctionsByContext =
        Dictionary<
            obj,
            Dictionary<
                LIR.Function * ARM64.TargetConfig * ARM64CodeGenTypes.CodeGenOptions,
                Result<ARM64Symbolic.Instr list, string>>>(ObjectReferenceComparer())
    let arm64HelpersByContext =
        Dictionary<
            obj,
            Dictionary<Arm64HelperCacheKey, ARM64Symbolic.Instr list>>(ObjectReferenceComparer())
    // Prebuilt stdlib and preamble functions retain object identity across a
    // compilation session. Keep an identity-indexed fast lane for functions
    // that populated the structural cache, avoiding repeated deep CFG equality
    // checks without retaining transient structurally equivalent functions.
    let arm64FunctionsByReferenceAndContext =
        Dictionary<
            obj,
            Dictionary<
                LIR.Function,
                Dictionary<
                    ARM64.TargetConfig * ARM64CodeGenTypes.CodeGenOptions,
                    Result<ARM64Symbolic.Instr list, string>>>>(ObjectReferenceComparer())
    let arm64StartContextIdentity = System.Object()
    // Finalized functions carry every input needed by ARM64 conversion except
    // RawSlotInit's nominal-type lookup. Share all other functions across
    // executable registry contexts while retaining target/options segregation.
    let arm64RegistryIndependentFunctionContextIdentity = System.Object()
    let arm64GenericReleaseHelperContextIdentity = System.Object()
    let arm64RegistryIndependentHelperContextIdentity = System.Object()
    let arm64EmissionChunks =
        Dictionary<
            ARM64Symbolic.Instr list,
            ARM64_Encoding.PreparedChunk>(Arm64InstructionChunkReferenceComparer())
    let arm64EmissionChunkGroups =
        Dictionary<
            ARM64Symbolic.Instr list list,
            ARM64_Encoding.PreparedChunk>(Arm64InstructionChunkGroupReferenceComparer())
    let arm64ReleasePlanSummaries =
        Dictionary<
            bool * string,
            (MemoryModel.RcReleasePlan * LIR.Arm64ReleasePlanSummary) list>()
    let arm64CodegenMetrics = ResizeArray<CodegenFunctionMetric>()
    let arm64LirOpMetrics =
        Dictionary<struct (string * string * string), struct (int * int * int64)>()
    let mutable disposed = false
    let mutable arm64CodegenHitCount = 0
    let mutable arm64CodegenMissCount = 0
    let mutable arm64ReleasePlanSummaryHitCount = 0
    let mutable arm64ReleasePlanSummaryMissCount = 0
    let mutable anfDependencyHitCount = 0
    let mutable anfDependencyMissCount = 0
    let mutable compiledDependencyHitCount = 0
    let mutable compiledDependencyMissCount = 0
    let mutable compiledStartHitCount = 0
    let mutable compiledStartMissCount = 0
    let mutable ssaFunctionHitCount = 0
    let mutable ssaFunctionMissCount = 0
    let mutable mirOptimizationHitCount = 0
    let mutable mirOptimizationMissCount = 0
    let mutable allocatedLirFunctionHitCount = 0
    let mutable allocatedLirFunctionMissCount = 0
    let mutable stdlibReachabilityHitCount = 0
    let mutable stdlibReachabilityMissCount = 0
    let mutable mirRegistryProjectionHitCount = 0
    let mutable mirRegistryProjectionMissCount = 0
    let mutable arm64StartCodegenHitCount = 0
    let mutable arm64MetadataGroupHitCount = 0
    let mutable arm64MetadataGroupMissCount = 0
    let mutable arm64FunctionGroupHitCount = 0
    let mutable arm64FunctionGroupMissCount = 0
    let mutable arm64HelperHitCount = 0
    let mutable arm64HelperMissCount = 0

    new() = new CompilationSession(false)

    member _.JsonPlanning = jsonPlanning
    member internal _.Arm64GenericReleaseHelperContextIdentity =
        arm64GenericReleaseHelperContextIdentity

    member internal _.ConvertAnfDependencies
        (contextIdentity: obj)
        (key: AnfDependencyKey)
        (convert: unit -> Result<ANF.Function list * ANF.VarGen, string>)
        : Result<ANF.Function list * ANF.VarGen * obj, string> =
        if disposed then
            convert () |> Result.map (fun (functions, varGen) -> (functions, varGen, System.Object()))
        else
            let contextEntries =
                match anfDependenciesByContext.TryGetValue contextIdentity with
                | true, entries -> entries
                | false, _ ->
                    let entries =
                        Dictionary<
                            AnfDependencyKey,
                            Result<ANF.Function list * ANF.VarGen * obj, string>>(
                                AnfDependencyKeyNameHashComparer())
                    anfDependenciesByContext.[contextIdentity] <- entries
                    entries
            match contextEntries.TryGetValue key with
            | true, result ->
                anfDependencyHitCount <- anfDependencyHitCount + 1
                result
            | false, _ ->
                let result =
                    convert ()
                    |> Result.map (fun (functions, varGen) -> (functions, varGen, System.Object()))
                contextEntries.[key] <- result
                anfDependencyMissCount <- anfDependencyMissCount + 1
                result

    member internal _.CompileDependencies
        (dependencyIdentity: obj)
        (config: CompiledDependencyConfig)
        (compile: unit -> Result<LIR.Function list, string>)
        : Result<LIR.Function list, string> =
        if disposed || config.Options.EnableCoverage then
            compile ()
        else
            let entries =
                match compiledDependenciesByIdentity.TryGetValue dependencyIdentity with
                | true, entries -> entries
                | false, _ ->
                    let entries = Dictionary<CompiledDependencyConfig, Result<LIR.Function list, string>>()
                    compiledDependenciesByIdentity.[dependencyIdentity] <- entries
                    entries
            match entries.TryGetValue config with
            | true, result ->
                compiledDependencyHitCount <- compiledDependencyHitCount + 1
                result
            | false, _ ->
                let result = compile ()
                entries.[config] <- result
                compiledDependencyMissCount <- compiledDependencyMissCount + 1
                result

    member internal _.Arm64LirOpExpansionRecorder
        : ARM64CodeGenTypes.LirOpExpansionRecorder option =
        if disposed || not collectCodegenMetrics then
            None
        else
            Some (fun functionName opcode detail symbolicInstructionCount elapsedTicks ->
                let key = struct (functionName, opcode, detail)
                match arm64LirOpMetrics.TryGetValue key with
                | true, struct (occurrences, symbolicInstructions, ticks) ->
                    arm64LirOpMetrics.[key] <-
                        struct (
                            occurrences + 1,
                            symbolicInstructions + symbolicInstructionCount,
                            ticks + elapsedTicks)
                | false, _ ->
                    arm64LirOpMetrics.[key] <-
                        struct (1, symbolicInstructionCount, elapsedTicks))

    member internal _.CompileStart
        (config: StartCompilationConfig)
        (compile: unit -> Result<LIR.Function list, string>)
        : Result<LIR.Function list, string> =
        if disposed || config.Options.EnableCoverage then
            compile ()
        else
            match compiledStartFunctions.TryGetValue config with
            | true, result ->
                compiledStartHitCount <- compiledStartHitCount + 1
                result
            | false, _ ->
                let result = compile ()
                compiledStartFunctions.[config] <- result
                compiledStartMissCount <- compiledStartMissCount + 1
                result

    member _.ConvertMirFunctionToSsa
        (func: MIR.Function)
        (convert: unit -> MIR.Function)
        : MIR.Function =
        if disposed then
            convert ()
        else
            match ssaFunctionsByExactInput.TryGetValue func with
            | true, converted ->
                ssaFunctionHitCount <- ssaFunctionHitCount + 1
                converted
            | false, _ ->
                let cacheKey = normalizeMirFunctionRegisterOffset func
                match ssaFunctions.TryGetValue cacheKey with
                | true, converted ->
                    ssaFunctionsByExactInput.[func] <- converted
                    ssaFunctionHitCount <- ssaFunctionHitCount + 1
                    converted
                | false, _ ->
                    let converted = convert ()
                    ssaFunctions.[cacheKey] <- converted
                    ssaFunctionsByExactInput.[func] <- converted
                    ssaFunctionMissCount <- ssaFunctionMissCount + 1
                    converted

    member _.OptimizeMirFunction
        (key: MirOptimizationKey)
        (optimize: unit -> MIR.Function)
        : MIR.Function =
        if disposed then
            optimize ()
        else
            match optimizedMirFunctions.TryGetValue key with
            | true, optimized ->
                mirOptimizationHitCount <- mirOptimizationHitCount + 1
                optimized
            | false, _ ->
                let optimized = optimize ()
                optimizedMirFunctions.[key] <- optimized
                mirOptimizationMissCount <- mirOptimizationMissCount + 1
                optimized

    member _.AllocateLirFunction
        (arch: Platform.Arch)
        (func: LIR.Function)
        (allocate: unit -> LIR.Function)
        : LIR.Function =
        let key = { Arch = arch; Function = func }
        if disposed then
            allocate ()
        else
            match allocatedLirFunctions.TryGetValue key with
            | true, allocated ->
                allocatedLirFunctionHitCount <- allocatedLirFunctionHitCount + 1
                allocated
            | false, _ ->
                let allocated = allocate ()
                allocatedLirFunctions.[key] <- allocated
                allocatedLirFunctionMissCount <- allocatedLirFunctionMissCount + 1
                allocated

    member internal _.ReachableStdlibFunctions
        (contextIdentity: obj)
        (userCallGraph: Map<AST.FunctionId, Set<AST.FunctionId>>)
        (userFunctions: LIR.Function list)
        (stdlibCallGraph: Map<AST.FunctionId, Set<AST.FunctionId>>)
        (stdlibFunctions: LIR.Function list)
        : LIR.Function list =
        let directCalls =
            DeadCodeElimination.directCallsFromFunctions userCallGraph userFunctions
            // Calls between user functions cannot lead into the stdlib graph:
            // every direct user-to-stdlib edge is already present in this set.
            // Excluding those user-local names makes equivalent stdlib queries
            // share one session entry instead of fragmenting the cache by each
            // compilation's generated function names.
            |> Set.filter (fun name -> Map.containsKey name stdlibCallGraph)
        if disposed then
            let reachable = DeadCodeElimination.findReachable stdlibCallGraph directCalls
            stdlibFunctions
            |> List.filter (fun func -> Set.contains func.Id reachable)
        else
            let contextEntries =
                match reachableStdlibFunctionsByContext.TryGetValue contextIdentity with
                | true, entries -> entries
                | false, _ ->
                    let entries = Dictionary<Set<AST.FunctionId>, LIR.Function list>()
                    reachableStdlibFunctionsByContext.[contextIdentity] <- entries
                    entries
            match contextEntries.TryGetValue directCalls with
            | true, functions ->
                stdlibReachabilityHitCount <- stdlibReachabilityHitCount + 1
                functions
            | false, _ ->
                let rootEntries =
                    match reachableStdlibNamesByRootAndContext.TryGetValue contextIdentity with
                    | true, entries -> entries
                    | false, _ ->
                        let entries = Dictionary<AST.FunctionId, Set<AST.FunctionId>>()
                        reachableStdlibNamesByRootAndContext.[contextIdentity] <- entries
                        entries
                let reachable =
                    directCalls
                    |> Set.fold (fun reachable root ->
                        let fromRoot =
                            match rootEntries.TryGetValue root with
                            | true, names -> names
                            | false, _ ->
                                let names =
                                    DeadCodeElimination.findReachable
                                        stdlibCallGraph
                                        (Set.singleton root)
                                rootEntries.[root] <- names
                                names
                        Set.union reachable fromRoot) Set.empty
                let functionInventory =
                    match stdlibFunctionInventoryByContext.TryGetValue contextIdentity with
                    | true, inventory -> inventory
                    | false, _ ->
                        let inventory = Dictionary<AST.FunctionId, struct (int * LIR.Function)>()
                        stdlibFunctions
                        |> List.iteri (fun index func ->
                            inventory.[func.Id] <- struct (index, func))
                        stdlibFunctionInventoryByContext.[contextIdentity] <- inventory
                        inventory
                let functions =
                    reachable
                    |> Seq.choose (fun name ->
                        match functionInventory.TryGetValue name with
                        | true, entry -> Some entry
                        | false, _ -> None)
                    |> Seq.sortBy (fun struct (index, _) -> index)
                    |> Seq.map (fun struct (_, func) -> func)
                    |> Seq.toList
                contextEntries.[directCalls] <- functions
                stdlibReachabilityMissCount <- stdlibReachabilityMissCount + 1
                functions

    member internal _.ProjectMirRegistries
        (contextIdentity: obj)
        ((baseVariants, baseRecords): MIR.VariantRegistry * MIR.RecordRegistry)
        (localVariantLookup: LoweringPrimitives.VariantLookup)
        (localRecordFields: Map<string, (string * AST.Type) list>)
        : MIR.VariantRegistry * MIR.RecordRegistry =
        let projectLocalOverlay () =
            projectMirRegistryOverlay
                (baseVariants, baseRecords)
                localVariantLookup
                localRecordFields
        if disposed then
            projectLocalOverlay ()
        else
            match mirRegistriesByContext.TryGetValue contextIdentity with
            | true, registries ->
                mirRegistryProjectionHitCount <- mirRegistryProjectionHitCount + 1
                registries
            | false, _ ->
                let registries = projectLocalOverlay ()
                mirRegistriesByContext.[contextIdentity] <- registries
                mirRegistryProjectionMissCount <- mirRegistryProjectionMissCount + 1
                registries

    member internal _.Arm64MetadataGroup
        (contextIdentity: obj)
        (functions: LIR.Function list)
        (summarize: unit -> ARM64CodeGenTypes.Arm64ProgramMetadata)
        : ARM64CodeGenTypes.Arm64ProgramMetadata =
        if disposed then
            summarize ()
        else
            let contextEntries =
                match arm64MetadataGroupsByContext.TryGetValue contextIdentity with
                | true, entries -> entries
                | false, _ ->
                    let entries =
                        Dictionary<Arm64MetadataGroupKey, ARM64CodeGenTypes.Arm64ProgramMetadata>(
                            Arm64MetadataGroupKeyComparer()
                        )
                    arm64MetadataGroupsByContext.[contextIdentity] <- entries
                    entries
            let key = { Functions = functions }
            match contextEntries.TryGetValue key with
            | true, metadata ->
                arm64MetadataGroupHitCount <- arm64MetadataGroupHitCount + 1
                metadata
            | false, _ ->
                let metadata = summarize ()
                contextEntries.[key] <- metadata
                arm64MetadataGroupMissCount <- arm64MetadataGroupMissCount + 1
                metadata

    member internal _.CodegenFunctionGroup
        (contextIdentity: obj)
        (target: ARM64.TargetConfig)
        (options: ARM64CodeGenTypes.CodeGenOptions)
        (_functions: LIR.Function list)
        (generate: unit -> Result<CodeGen.GeneratedChunk list, string>)
        : Result<CodeGen.GeneratedChunk list, string> =
        if disposed || options.EnableCoverage then
            generate ()
        else
            let contextEntries =
                match arm64FunctionGroupsByContext.TryGetValue contextIdentity with
                | true, entries -> entries
                | false, _ ->
                    let entries =
                        Dictionary<
                            ARM64.TargetConfig * ARM64CodeGenTypes.CodeGenOptions,
                            Result<CodeGen.GeneratedChunk list, string>>()
                    arm64FunctionGroupsByContext.[contextIdentity] <- entries
                    entries
            let key = (target, options)
            match contextEntries.TryGetValue key with
            | true, result ->
                arm64FunctionGroupHitCount <- arm64FunctionGroupHitCount + 1
                result
            | false, _ ->
                let result = generate ()
                contextEntries.[key] <- result
                arm64FunctionGroupMissCount <- arm64FunctionGroupMissCount + 1
                result

    member _.Arm64ReleasePlanSummary
        (includeStaticRootDependencies: bool)
        (releasePlanCacheKey: string)
        (releasePlan: MemoryModel.RcReleasePlan)
        (generate: unit -> LIR.Arm64ReleasePlanSummary)
        : LIR.Arm64ReleasePlanSummary =
        if disposed then
            generate ()
        else
            let key =
                (includeStaticRootDependencies,
                 releasePlanCacheKey)
            match arm64ReleasePlanSummaries.TryGetValue key with
            | true, entries ->
                match
                    entries
                    |> List.tryFind (fun (existingPlan, _) ->
                        System.Object.ReferenceEquals(existingPlan, releasePlan)
                        || existingPlan = releasePlan)
                with
                | Some (_, summary) ->
                    arm64ReleasePlanSummaryHitCount <- arm64ReleasePlanSummaryHitCount + 1
                    summary
                | None ->
                    let summary = generate ()
                    arm64ReleasePlanSummaries.[key] <- (releasePlan, summary) :: entries
                    arm64ReleasePlanSummaryMissCount <- arm64ReleasePlanSummaryMissCount + 1
                    summary
            | false, _ ->
                let summary = generate ()
                arm64ReleasePlanSummaries.[key] <- [releasePlan, summary]
                arm64ReleasePlanSummaryMissCount <- arm64ReleasePlanSummaryMissCount + 1
                summary

    member _.CodegenFunction
        (contextIdentity: obj)
        (target: ARM64.TargetConfig)
        (options: ARM64CodeGenTypes.CodeGenOptions)
        (func: LIR.Function)
        (generate: unit -> Result<ARM64Symbolic.Instr list, string>)
        : Result<ARM64Symbolic.Instr list, string> =
        if disposed || options.EnableCoverage then generate ()
        else
            let contextIdentity =
                if func.Name = "_start" then arm64StartContextIdentity
                elif
                    func.CodegenFacts
                    |> Option.exists (fun facts ->
                        Set.isEmpty facts.RawSlotInitTypes
                        || Option.isSome facts.Arm64RawSlotInitRetainTargets)
                then
                    arm64RegistryIndependentFunctionContextIdentity
                else contextIdentity
            let structuralEntries =
                match arm64FunctionsByContext.TryGetValue contextIdentity with
                | true, entries -> entries
                | false, _ ->
                    let entries =
                        Dictionary<
                            LIR.Function * ARM64.TargetConfig * ARM64CodeGenTypes.CodeGenOptions,
                            Result<ARM64Symbolic.Instr list, string>>()
                    arm64FunctionsByContext.[contextIdentity] <- entries
                    entries
            let referenceEntriesForContext =
                match arm64FunctionsByReferenceAndContext.TryGetValue contextIdentity with
                | true, entries -> entries
                | false, _ ->
                    let entries =
                        Dictionary<
                            LIR.Function,
                            Dictionary<
                                ARM64.TargetConfig * ARM64CodeGenTypes.CodeGenOptions,
                                Result<ARM64Symbolic.Instr list, string>>>(LirFunctionReferenceComparer())
                    arm64FunctionsByReferenceAndContext.[contextIdentity] <- entries
                    entries
            let key = (func, target, options)
            let targetOptions = (target, options)
            let referenceResult =
                match referenceEntriesForContext.TryGetValue func with
                | true, entries ->
                    match entries.TryGetValue targetOptions with
                    | true, result -> Some result
                    | false, _ -> None
                | false, _ -> None
            match referenceResult with
            | Some result ->
                arm64CodegenHitCount <- arm64CodegenHitCount + 1
                if func.Name = "_start" then
                    arm64StartCodegenHitCount <- arm64StartCodegenHitCount + 1
                result
            | None ->
                match structuralEntries.TryGetValue key with
                | true, result ->
                    arm64CodegenHitCount <- arm64CodegenHitCount + 1
                    if func.Name = "_start" then
                        arm64StartCodegenHitCount <- arm64StartCodegenHitCount + 1
                    result
                | false, _ ->
                    let timer =
                        if collectCodegenMetrics then Some (Stopwatch.StartNew())
                        else None
                    let result = generate ()
                    match timer with
                    | Some timer ->
                        timer.Stop()
                        let lirInstructionCount =
                            func.CFG.Blocks
                            |> Map.toSeq
                            |> Seq.sumBy (fun (_, block) -> block.Instrs.Length + 1)
                        let symbolicInstructionCount =
                            match result with
                            | Ok instructions -> instructions.Length
                            | Error _ -> 0
                        arm64CodegenMetrics.Add {
                            FunctionName = func.Name
                            Elapsed = timer.Elapsed
                            LirInstructionCount = lirInstructionCount
                            SymbolicInstructionCount = symbolicInstructionCount
                        }
                    | None -> ()
                    structuralEntries.[key] <- result
                    let referenceEntries =
                        match referenceEntriesForContext.TryGetValue func with
                        | true, entries -> entries
                        | false, _ ->
                            let entries = Dictionary<ARM64.TargetConfig * ARM64CodeGenTypes.CodeGenOptions, Result<ARM64Symbolic.Instr list, string>>()
                            referenceEntriesForContext.[func] <- entries
                            entries
                    referenceEntries.[targetOptions] <- result
                    arm64CodegenMissCount <- arm64CodegenMissCount + 1
                    result

    member _.Arm64Helpers
        (contextIdentity: obj)
        (target: ARM64.TargetConfig)
        (options: ARM64CodeGenTypes.CodeGenOptions)
        (helperKey: CodeGen.HelperCacheKey)
        (generate: unit -> ARM64Symbolic.Instr list)
        : ARM64Symbolic.Instr list =
        if disposed || options.EnableCoverage then
            generate ()
        else
            let contextIdentity =
                if List.isEmpty helperKey.RecursiveReleaseTypes
                   && List.isEmpty helperKey.ClosureCaptureTypes then
                    arm64RegistryIndependentHelperContextIdentity
                else
                    contextIdentity
            let contextEntries =
                match arm64HelpersByContext.TryGetValue contextIdentity with
                | true, entries -> entries
                | false, _ ->
                    let entries = Dictionary<Arm64HelperCacheKey, ARM64Symbolic.Instr list>()
                    arm64HelpersByContext.[contextIdentity] <- entries
                    entries
            let key = { Target = target; Options = options; Helper = helperKey }
            match contextEntries.TryGetValue key with
            | true, instructions ->
                arm64HelperHitCount <- arm64HelperHitCount + 1
                instructions
            | false, _ ->
                let instructions = generate ()
                contextEntries.[key] <- instructions
                arm64HelperMissCount <- arm64HelperMissCount + 1
                instructions

    member _.PrepareArm64EmissionChunk
        (instructions: ARM64Symbolic.Instr list)
        (prepare: unit -> ARM64_Encoding.PreparedChunk)
        : ARM64_Encoding.PreparedChunk =
        if disposed then prepare ()
        else
            match arm64EmissionChunks.TryGetValue instructions with
            | true, prepared -> prepared
            | false, _ ->
                let prepared = prepare ()
                arm64EmissionChunks.[instructions] <- prepared
                prepared

    member _.PrepareArm64EmissionChunkGroup
        (instructionParts: ARM64Symbolic.Instr list list)
        (prepare: unit -> ARM64_Encoding.PreparedChunk)
        : ARM64_Encoding.PreparedChunk =
        if disposed then prepare ()
        else
            match arm64EmissionChunkGroups.TryGetValue instructionParts with
            | true, prepared -> prepared
            | false, _ ->
                let prepared = prepare ()
                arm64EmissionChunkGroups.[instructionParts] <- prepared
                prepared

    member _.CachedArm64FunctionCount =
        if disposed then 0
        else arm64FunctionsByContext.Values |> Seq.sumBy (fun entries -> entries.Count)
    member _.CachedArm64HelperCount =
        if disposed then 0
        else arm64HelpersByContext.Values |> Seq.sumBy (fun entries -> entries.Count)
    member _.CachedAnfDependencyCount =
        if disposed then 0
        else anfDependenciesByContext.Values |> Seq.sumBy (fun entries -> entries.Count)
    member _.CachedCompiledDependencyCount =
        if disposed then 0
        else compiledDependenciesByIdentity.Values |> Seq.sumBy (fun entries -> entries.Count)
    member _.CachedCompiledStartCount = if disposed then 0 else compiledStartFunctions.Count
    member _.CachedSsaFunctionCount = if disposed then 0 else ssaFunctions.Count
    member _.CachedMirOptimizationCount = if disposed then 0 else optimizedMirFunctions.Count
    member _.CachedAllocatedLirFunctionCount = if disposed then 0 else allocatedLirFunctions.Count
    member _.CachedStdlibReachabilityCount =
        if disposed then 0
        else reachableStdlibFunctionsByContext.Values |> Seq.sumBy (fun entries -> entries.Count)
    member _.CachedMirRegistryProjectionCount =
        if disposed then 0 else mirRegistriesByContext.Count
    member _.CachedArm64MetadataGroupCount =
        if disposed then 0
        else arm64MetadataGroupsByContext.Values |> Seq.sumBy (fun entries -> entries.Count)
    member _.CachedArm64FunctionGroupCount =
        if disposed then 0
        else arm64FunctionGroupsByContext.Values |> Seq.sumBy (fun entries -> entries.Count)
    member _.CachedArm64EmissionChunkCount =
        if disposed then 0
        else arm64EmissionChunks.Count + arm64EmissionChunkGroups.Count
    member _.CachedArm64ReleasePlanSummaryCount =
        if disposed then 0
        else arm64ReleasePlanSummaries.Values |> Seq.sumBy List.length
    member _.CachedJsonPlanCount = jsonPlanning.Count
    member _.JsonPlanHitCount = jsonPlanning.HitCount
    member _.JsonPlanMissCount = jsonPlanning.MissCount
    member _.AnfDependencyHitCount = anfDependencyHitCount
    member _.AnfDependencyMissCount = anfDependencyMissCount
    member _.CompiledDependencyHitCount = compiledDependencyHitCount
    member _.CompiledDependencyMissCount = compiledDependencyMissCount
    member _.CompiledStartHitCount = compiledStartHitCount
    member _.CompiledStartMissCount = compiledStartMissCount
    member _.SsaFunctionHitCount = ssaFunctionHitCount
    member _.SsaFunctionMissCount = ssaFunctionMissCount
    member _.MirOptimizationHitCount = mirOptimizationHitCount
    member _.MirOptimizationMissCount = mirOptimizationMissCount
    member _.AllocatedLirFunctionHitCount = allocatedLirFunctionHitCount
    member _.AllocatedLirFunctionMissCount = allocatedLirFunctionMissCount
    member _.StdlibReachabilityHitCount = stdlibReachabilityHitCount
    member _.StdlibReachabilityMissCount = stdlibReachabilityMissCount
    member _.MirRegistryProjectionHitCount = mirRegistryProjectionHitCount
    member _.MirRegistryProjectionMissCount = mirRegistryProjectionMissCount
    member _.Arm64CodegenHitCount = arm64CodegenHitCount
    member _.Arm64CodegenMissCount = arm64CodegenMissCount
    member _.Arm64StartCodegenHitCount = arm64StartCodegenHitCount
    member _.Arm64MetadataGroupHitCount = arm64MetadataGroupHitCount
    member _.Arm64MetadataGroupMissCount = arm64MetadataGroupMissCount
    member _.Arm64FunctionGroupHitCount = arm64FunctionGroupHitCount
    member _.Arm64FunctionGroupMissCount = arm64FunctionGroupMissCount
    member _.Arm64HelperHitCount = arm64HelperHitCount
    member _.Arm64HelperMissCount = arm64HelperMissCount
    member _.Arm64ReleasePlanSummaryHitCount = arm64ReleasePlanSummaryHitCount
    member _.Arm64ReleasePlanSummaryMissCount = arm64ReleasePlanSummaryMissCount
    member _.Arm64CodegenMetrics = arm64CodegenMetrics |> Seq.toList
    member _.Arm64LirOpMetrics =
        let timestampFrequency = float Stopwatch.Frequency
        arm64LirOpMetrics
        |> Seq.map (fun (KeyValue (struct (functionName, opcode, detail), struct (occurrences, symbolicInstructions, ticks))) ->
            {
                FunctionName = functionName
                Opcode = opcode
                Detail = detail
                Occurrences = occurrences
                SymbolicInstructionCount = symbolicInstructions
                Elapsed = TimeSpan.FromSeconds(float ticks / timestampFrequency)
            })
        |> Seq.toList

    interface IDisposable with
        member _.Dispose() =
            (jsonPlanning :> System.IDisposable).Dispose()
            anfDependenciesByContext.Clear()
            compiledDependenciesByIdentity.Clear()
            compiledStartFunctions.Clear()
            ssaFunctionsByExactInput.Clear()
            ssaFunctions.Clear()
            optimizedMirFunctions.Clear()
            allocatedLirFunctions.Clear()
            reachableStdlibFunctionsByContext.Clear()
            stdlibFunctionInventoryByContext.Clear()
            reachableStdlibNamesByRootAndContext.Clear()
            mirRegistriesByContext.Clear()
            arm64MetadataGroupsByContext.Clear()
            arm64FunctionGroupsByContext.Clear()
            arm64FunctionsByContext.Clear()
            arm64HelpersByContext.Clear()
            arm64FunctionsByReferenceAndContext.Clear()
            arm64EmissionChunks.Clear()
            arm64EmissionChunkGroups.Clear()
            arm64ReleasePlanSummaries.Clear()
            arm64CodegenMetrics.Clear()
            arm64LirOpMetrics.Clear()
            disposed <- true
