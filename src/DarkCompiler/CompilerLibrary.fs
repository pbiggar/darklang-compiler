// CompilerLibrary.fs - Library API for the Dark compiler
//
// Exposes the compiler as a library for use in tests and other tools.
// Provides clean functions that can be called without spawning processes.

module CompilerLibrary

open CodeGen
open IRPrinter

open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Collections.Generic
open Output

/// Timing for a single compiler pass
type PassTiming = {
    Pass: string
    Elapsed: TimeSpan
}

/// Recorder for compiler pass timings
type PassTimingRecorder = PassTiming -> unit

/// Result of execution with timing
type ExecutionOutput = {
    ExitCode: int
    Stdout: string
    Stderr: string
    RuntimeTime: TimeSpan
}

/// Finite stdin supplied to a captured native execution.
type ExecutionInput =
    | Closed
    | Bytes of byte array

/// Compilation mode for labeling and test behavior
type CompileMode =
    | FullProgram
    | TestExpression

/// Shared compiler warning settings.
let defaultWarningSettings : AST.WarningSettings = AST.defaultWarningSettings

/// Result of compilation with timing
type CompileReport = {
    Target: Platform.Target
    Result: Result<byte array, string>
    CompileTime: TimeSpan
}

/// Compiler options for controlling optimization behavior
type CompilerOptions = {
    /// Disable free list memory reuse (always bump allocate)
    DisableFreeList: bool
    /// Disable ANF-level optimizations (constant folding, propagation, etc.)
    DisableANFOpt: bool
    /// Disable ANF constant folding (includes algebraic identities and constant branches)
    DisableANFConstFolding: bool
    /// Disable ANF constant propagation
    DisableANFConstProp: bool
    /// Disable ANF copy propagation
    DisableANFCopyProp: bool
    /// Disable ANF dead code elimination
    DisableANFDCE: bool
    /// Disable ANF strength reduction (pow2 mul/div/mod)
    DisableANFStrengthReduction: bool
    /// Disable ANF function inlining
    DisableInlining: bool
    /// Disable tail call optimization
    DisableTCO: bool
    /// Disable MIR-level optimizations (DCE, copy/constant propagation on SSA)
    DisableMIROpt: bool
    /// Disable MIR constant folding
    DisableMIRConstFolding: bool
    /// Disable MIR common subexpression elimination
    DisableMIRCSE: bool
    /// Disable MIR copy propagation
    DisableMIRCopyProp: bool
    /// Disable MIR dead code elimination
    DisableMIRDCE: bool
    /// Disable MIR CFG simplification
    DisableMIRCFGSimplify: bool
    /// Disable MIR loop-invariant code motion
    DisableMIRLICM: bool
    /// Disable LIR-level optimizations (peephole optimizations)
    DisableLIROpt: bool
    /// Disable LIR peephole optimizations
    DisableLIRPeephole: bool
    /// Disable function tree shaking (pruning unused stdlib/user functions)
    DisableFunctionTreeShaking: bool
    /// Enable runtime expression coverage tracking
    EnableCoverage: bool
    /// Enable leak checking (debug only)
    EnableLeakCheck: bool
    /// Warning compatibility settings passed into type checking
    Warnings: AST.WarningSettings
    /// Dump ANF representations to stdout
    DumpANF: bool
    /// Dump MIR representations to stdout
    DumpMIR: bool
    /// Dump LIR representations to stdout (before and after register allocation)
    DumpLIR: bool
}

/// Default compiler options
let defaultOptions : CompilerOptions = {
    DisableFreeList = false
    DisableANFOpt = false
    DisableANFConstFolding = false
    DisableANFConstProp = false
    DisableANFCopyProp = false
    DisableANFDCE = false
    DisableANFStrengthReduction = false
    DisableInlining = false
    DisableTCO = false
    DisableMIROpt = false
    DisableMIRConstFolding = false
    DisableMIRCSE = false
    DisableMIRCopyProp = false
    DisableMIRDCE = false
    DisableMIRCFGSimplify = false
    DisableMIRLICM = false
    DisableLIROpt = false
    DisableLIRPeephole = false
    DisableFunctionTreeShaking = false
    EnableCoverage = false
    EnableLeakCheck = false
    Warnings = AST.defaultWarningSettings
    DumpANF = false
    DumpMIR = false
    DumpLIR = false
}

/// Explicit lifetime for reuse across a bounded group of compilations (the E2E
/// runner owns one per suite). No compiler-global cache is retained.
type CodegenFunctionMetric = {
    FunctionName: string
    Elapsed: TimeSpan
    LirInstructionCount: int
    SymbolicInstructionCount: int
}

type private LirFunctionReferenceComparer() =
    interface IEqualityComparer<LIR.Function> with
        member _.Equals(left, right) = Object.ReferenceEquals(left, right)
        member _.GetHashCode(func) =
            System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(func)

type private ObjectReferenceComparer() =
    interface IEqualityComparer<obj> with
        member _.Equals(left, right) = Object.ReferenceEquals(left, right)
        member _.GetHashCode(value) =
            System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(value)

[<NoComparison>]
type internal AnfDependencyKey = {
    Functions: AST.FunctionDef list
    LocalRegistries: AST_to_ANF.Registries
    NonInlineableFunctionNames: Set<string>
}

[<NoComparison>]
type internal CompiledDependencyConfig = {
    Target: Platform.Target
    Options: CompilerOptions
    NonInlineableFunctionNames: Set<string>
}

[<NoComparison>]
type internal StartCompilationConfig = {
    Target: Platform.Target
    Options: CompilerOptions
    BoundaryProgramType: AST.Type
}

type private Arm64InstructionChunkReferenceComparer() =
    interface IEqualityComparer<ARM64Symbolic.Instr list> with
        member _.Equals(left, right) = Object.ReferenceEquals(left, right)
        member _.GetHashCode(instructions) =
            System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(instructions)

[<NoComparison>]
type private Arm64MetadataGroupKey = {
    Functions: LIR.Function list
}

type private Arm64MetadataGroupKeyComparer() =
    interface IEqualityComparer<Arm64MetadataGroupKey> with
        member _.Equals(left, right) =
            List.length left.Functions = List.length right.Functions
            && List.forall2
                (fun leftFunction rightFunction ->
                    Object.ReferenceEquals(leftFunction, rightFunction))
                left.Functions
                right.Functions
        member _.GetHashCode(key) =
            key.Functions
            |> List.fold
                (fun hash func ->
                    (hash * 397)
                    ^^^ System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(func))
                17

[<NoComparison>]
type private Arm64HelperCacheKey = {
    Target: ARM64.TargetConfig
    Options: CodeGen.CodeGenOptions
    Helper: CodeGen.HelperCacheKey
}

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
    let reachableStdlibFunctionsByContext =
        Dictionary<
            obj,
            Dictionary<Set<string>, LIR.Function list>>(ObjectReferenceComparer())
    let mirRegistriesByContext =
        Dictionary<
            obj,
            MIR.VariantRegistry * MIR.RecordRegistry>(ObjectReferenceComparer())
    let arm64MetadataGroupsByContext =
        Dictionary<
            obj,
            Dictionary<
                Arm64MetadataGroupKey,
                CodeGen.Arm64ProgramMetadata>>(ObjectReferenceComparer())
    let arm64FunctionGroupsByContext =
        Dictionary<
            obj,
            Dictionary<
                ARM64.TargetConfig * CodeGen.CodeGenOptions,
                Result<CodeGen.GeneratedChunk list, string>>>(ObjectReferenceComparer())
    let arm64FunctionsByContext =
        Dictionary<
            obj,
            Dictionary<
                LIR.Function * ARM64.TargetConfig * CodeGen.CodeGenOptions,
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
                    ARM64.TargetConfig * CodeGen.CodeGenOptions,
                    Result<ARM64Symbolic.Instr list, string>>>>(ObjectReferenceComparer())
    let arm64StartContextIdentity = System.Object()
    let arm64EmissionChunks =
        Dictionary<
            ARM64Symbolic.Instr list,
            ARM64_Encoding.PreparedChunk>(Arm64InstructionChunkReferenceComparer())
    let arm64ReleasePlanSummaries =
        Dictionary<
            bool * string,
            (ANF.RcReleasePlan * LIR.Arm64ReleasePlanSummary) list>()
    let arm64CodegenMetrics = ResizeArray<CodegenFunctionMetric>()
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
                    let entries = Dictionary<AnfDependencyKey, Result<ANF.Function list * ANF.VarGen * obj, string>>()
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

    member internal _.ReachableStdlibFunctions
        (contextIdentity: obj)
        (userFunctions: LIR.Function list)
        (stdlibCallGraph: Map<string, Set<string>>)
        (stdlibFunctions: LIR.Function list)
        : LIR.Function list =
        let directCalls =
            userFunctions
            |> List.fold (fun calls func ->
                Set.union calls (DeadCodeElimination.getCalledFunctions func)) Set.empty
        if disposed then
            let reachable = DeadCodeElimination.findReachable stdlibCallGraph directCalls
            stdlibFunctions
            |> List.filter (fun func -> Set.contains func.Name reachable)
        else
            let contextEntries =
                match reachableStdlibFunctionsByContext.TryGetValue contextIdentity with
                | true, entries -> entries
                | false, _ ->
                    let entries = Dictionary<Set<string>, LIR.Function list>()
                    reachableStdlibFunctionsByContext.[contextIdentity] <- entries
                    entries
            match contextEntries.TryGetValue directCalls with
            | true, functions ->
                stdlibReachabilityHitCount <- stdlibReachabilityHitCount + 1
                functions
            | false, _ ->
                let reachable = DeadCodeElimination.findReachable stdlibCallGraph directCalls
                let functions =
                    stdlibFunctions
                    |> List.filter (fun func -> Set.contains func.Name reachable)
                contextEntries.[directCalls] <- functions
                stdlibReachabilityMissCount <- stdlibReachabilityMissCount + 1
                functions

    member internal _.ProjectMirRegistries
        (contextIdentity: obj)
        (variantLookup: AST_to_ANF.VariantLookup)
        (recordFields: Map<string, (string * AST.Type) list>)
        : MIR.VariantRegistry * MIR.RecordRegistry =
        if disposed then
            (ANF_to_MIR.buildVariantRegistry variantLookup,
             ANF_to_MIR.buildRecordRegistry recordFields)
        else
            match mirRegistriesByContext.TryGetValue contextIdentity with
            | true, registries ->
                mirRegistryProjectionHitCount <- mirRegistryProjectionHitCount + 1
                registries
            | false, _ ->
                let registries =
                    (ANF_to_MIR.buildVariantRegistry variantLookup,
                     ANF_to_MIR.buildRecordRegistry recordFields)
                mirRegistriesByContext.[contextIdentity] <- registries
                mirRegistryProjectionMissCount <- mirRegistryProjectionMissCount + 1
                registries

    member internal _.Arm64MetadataGroup
        (contextIdentity: obj)
        (functions: LIR.Function list)
        (summarize: unit -> CodeGen.Arm64ProgramMetadata)
        : CodeGen.Arm64ProgramMetadata =
        if disposed then
            summarize ()
        else
            let contextEntries =
                match arm64MetadataGroupsByContext.TryGetValue contextIdentity with
                | true, entries -> entries
                | false, _ ->
                    let entries =
                        Dictionary<Arm64MetadataGroupKey, CodeGen.Arm64ProgramMetadata>(
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
        (options: CodeGen.CodeGenOptions)
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
                            ARM64.TargetConfig * CodeGen.CodeGenOptions,
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
        (releasePlan: ANF.RcReleasePlan)
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
                match entries |> List.tryFind (fun (existingPlan, _) -> existingPlan = releasePlan) with
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
        (options: CodeGen.CodeGenOptions)
        (func: LIR.Function)
        (generate: unit -> Result<ARM64Symbolic.Instr list, string>)
        : Result<ARM64Symbolic.Instr list, string> =
        if disposed || options.EnableCoverage then generate ()
        else
            let contextIdentity =
                if func.Name = "_start" then arm64StartContextIdentity
                else contextIdentity
            let structuralEntries =
                match arm64FunctionsByContext.TryGetValue contextIdentity with
                | true, entries -> entries
                | false, _ ->
                    let entries =
                        Dictionary<
                            LIR.Function * ARM64.TargetConfig * CodeGen.CodeGenOptions,
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
                                ARM64.TargetConfig * CodeGen.CodeGenOptions,
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
                            let entries = Dictionary<ARM64.TargetConfig * CodeGen.CodeGenOptions, Result<ARM64Symbolic.Instr list, string>>()
                            referenceEntriesForContext.[func] <- entries
                            entries
                    referenceEntries.[targetOptions] <- result
                    arm64CodegenMissCount <- arm64CodegenMissCount + 1
                    result

    member _.Arm64Helpers
        (contextIdentity: obj)
        (target: ARM64.TargetConfig)
        (options: CodeGen.CodeGenOptions)
        (helperKey: CodeGen.HelperCacheKey)
        (generate: unit -> ARM64Symbolic.Instr list)
        : ARM64Symbolic.Instr list =
        if disposed || options.EnableCoverage then
            generate ()
        else
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
        if disposed then 0 else arm64EmissionChunks.Count
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

    interface IDisposable with
        member _.Dispose() =
            (jsonPlanning :> System.IDisposable).Dispose()
            anfDependenciesByContext.Clear()
            compiledDependenciesByIdentity.Clear()
            compiledStartFunctions.Clear()
            reachableStdlibFunctionsByContext.Clear()
            mirRegistriesByContext.Clear()
            arm64MetadataGroupsByContext.Clear()
            arm64FunctionGroupsByContext.Clear()
            arm64FunctionsByContext.Clear()
            arm64HelpersByContext.Clear()
            arm64FunctionsByReferenceAndContext.Clear()
            arm64EmissionChunks.Clear()
            arm64ReleasePlanSummaries.Clear()
            arm64CodegenMetrics.Clear()
            disposed <- true

let private recordPassTiming
    (recorder: PassTimingRecorder option)
    (pass: string)
    (elapsedMs: float)
    : unit =
    match recorder with
    | None -> ()
    | Some record ->
        record { Pass = pass; Elapsed = TimeSpan.FromMilliseconds(elapsedMs) }

/// Determine whether to dump a specific IR, based on verbosity or explicit option
let private shouldDumpIR (verbosity: int) (enabled: bool) : bool =
    verbosity >= 3 || enabled

let private buildANFOptimizeOptions (options: CompilerOptions) : ANF_Optimize.OptimizeOptions =
    let enabled = not options.DisableANFOpt
    {
        EnableConstFolding = enabled && not options.DisableANFConstFolding
        EnableConstProp = enabled && not options.DisableANFConstProp
        EnableCopyProp = enabled && not options.DisableANFCopyProp
        EnableDCE = enabled && not options.DisableANFDCE
        EnableCSE = enabled
        EnableStrengthReduction = enabled && not options.DisableANFStrengthReduction
        EnableTailRecursionModuloOperation = enabled && not options.DisableTCO
    }

let private shouldRunANFOptimize (anfOptions: ANF_Optimize.OptimizeOptions) : bool =
    anfOptions.EnableConstFolding
    || anfOptions.EnableConstProp
    || anfOptions.EnableCopyProp
    || anfOptions.EnableDCE
    || anfOptions.EnableCSE
    || anfOptions.EnableStrengthReduction
    || anfOptions.EnableTailRecursionModuloOperation

let private buildMIROptimizeOptions (options: CompilerOptions) : MIR_Optimize.OptimizeOptions =
    let enabled = not options.DisableMIROpt
    {
        EnableConstFolding = enabled && not options.DisableMIRConstFolding
        EnableCSE = enabled && not options.DisableMIRCSE
        EnableCopyProp = enabled && not options.DisableMIRCopyProp
        EnableDCE = enabled && not options.DisableMIRDCE
        EnableCFGSimplify = enabled && not options.DisableMIRCFGSimplify
        EnableLICM = enabled && not options.DisableMIRLICM
    }

let private shouldRunMIROptimize (mirOptions: MIR_Optimize.OptimizeOptions) : bool =
    mirOptions.EnableConstFolding
    || mirOptions.EnableCSE
    || mirOptions.EnableCopyProp
    || mirOptions.EnableDCE
    || mirOptions.EnableCFGSimplify
    || mirOptions.EnableLICM

let private formatPassGroup (label: string) (passes: (string * bool) list) : string =
    let enabled =
        passes
        |> List.choose (fun (name, isEnabled) -> if isEnabled then Some name else None)
    let enabledNames = String.concat ", " enabled
    match enabled with
    | [] -> $"{label} (disabled)"
    | _ -> $"{label} ({enabledNames})"

/// Print ANF program in a consistent, human-readable format
let private printANFProgram (title: string) (program: ANF.Program) : unit =
    println title
    println (formatANF program)
    println ""

/// Print MIR program (with CFG) in a consistent format
let private printMIRProgram (title: string) (program: MIR.Program) : unit =
    println title
    println (formatMIR program)
    println ""

/// Print symbolic LIR program (with CFG) in a consistent format
let private printLIRProgram (title: string) (program: LIR.Program) : unit =
    println title
    println (formatLIR program)
    println ""

/// Run SSA + MIR/LIR optimizations, returning an optimized LIR program
let private compileMirToLir
    (arch: Platform.Arch)
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (passTimingRecorder: PassTimingRecorder option)
    (stageSuffix: string)
    (mirProgram: MIR.Program)
    : Result<LIR.Program, string> =

    let suffix = if stageSuffix = "" then "" else $" ({stageSuffix})"

    if verbosity >= 1 then println $"  [3.1/7] SSA Construction{suffix}..."
    let ssaStart = sw.Elapsed.TotalMilliseconds
    let ssaProgram = SSA_Construction.convertToSSA mirProgram
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
    if verbosity >= 1 then println $"  [3.5/7] {mirPassLabel}{suffix}..."
    let mirOptStart = sw.Elapsed.TotalMilliseconds
    let optimizedProgram =
        if shouldRunMIROptimize mirOptions then
            let mirOptPhaseRecorder =
                passTimingRecorder
                |> Option.map (fun recorder ->
                    fun name (elapsedMs: float) ->
                        recorder {
                            Pass = name
                            Elapsed = TimeSpan.FromMilliseconds elapsedMs
                        })
            MIR_Optimize.optimizeProgramWithOptionsAndTrace
                mirOptPhaseRecorder
                mirOptions
                ssaProgram
        else
            ssaProgram
    let mirOptElapsed = sw.Elapsed.TotalMilliseconds - mirOptStart
    recordPassTiming passTimingRecorder "MIR Optimizations" mirOptElapsed
    if shouldDumpIR verbosity options.DumpMIR then
        printMIRProgram "=== MIR (Control Flow Graph) ===" optimizedProgram
    if verbosity >= 2 then
        let t = System.Math.Round(mirOptElapsed, 1)
        println $"        {t}ms"

    if verbosity >= 1 then println $"  [4/7] MIR → LIR{suffix}..."
    let lirStart = sw.Elapsed.TotalMilliseconds
    let lirResult = MIR_to_LIR.toLIRFor arch optimizedProgram
    match lirResult with
    | Error err -> Error $"LIR conversion error: {err}"
    | Ok lirProgram ->
        let lirElapsed = sw.Elapsed.TotalMilliseconds - lirStart
        recordPassTiming passTimingRecorder "MIR -> LIR" lirElapsed
        if shouldDumpIR verbosity options.DumpLIR then
            printLIRProgram "=== LIR (Low-level IR with CFG) ===" lirProgram
        if verbosity >= 2 then
            let t = System.Math.Round(lirElapsed, 1)
            println $"        {t}ms"

        let lirPassLabel =
            formatPassGroup
                "LIR Peephole"
                [("peephole", not options.DisableLIROpt && not options.DisableLIRPeephole)]
        if verbosity >= 1 then println $"  [4.5/7] {lirPassLabel}{suffix}..."
        let lirOptStart = sw.Elapsed.TotalMilliseconds
        let optimizedLir =
            if options.DisableLIROpt || options.DisableLIRPeephole then
                lirProgram
            else
                LIR_Peephole.optimizeProgram lirProgram
        let lirOptElapsed = sw.Elapsed.TotalMilliseconds - lirOptStart
        recordPassTiming passTimingRecorder "LIR Peephole" lirOptElapsed
        if verbosity >= 2 then
            let t = System.Math.Round(lirOptElapsed, 1)
            println $"        {t}ms"
        // Summarize finalized symbolic LIR once. The facts remain attached to
        // functions through allocation and tree shaking, so each executable
        // only unions metadata for its reachable compilation unit.
        Ok (LIR.attachCodegenFacts optimizedLir)

/// Allocate registers for a list of symbolic LIR functions.
let private allocateRegistersForFunctions
    (arch: Platform.Arch)
    (functions: LIR.Function list)
    : LIR.Function list =
    functions
    |> List.map (fun func ->
        func
        |> RegisterAllocation.allocateRegisters arch
        |> LIR_Peephole.removeSelfMovesFromFunction)

/// Run MIR+LIR passes (including register allocation) from ANF functions
let private lowerToAllocatedLir
    (target: Platform.Target)
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (passTimingRecorder: PassTimingRecorder option)
    (releasePlanSummaryCache: CodeGen.ReleasePlanSummaryCache option)
    (stageSuffix: string)
    (functions: ANF.Function list)
    (typeMap: ANF.TypeMap)
    (registries: AST_to_ANF.Registries)
    (projectedMirRegistries: (MIR.VariantRegistry * MIR.RecordRegistry) option)
    (externalReturnTypes: Map<string, AST.Type>)
    : Result<LIR.Function list, string> =

    let suffix = if stageSuffix = "" then "" else $" ({stageSuffix})"

    let functionOrder = functions |> List.map (fun f -> f.Name)
    // Function-affinity batches still call helpers compiled in sibling batches.
    // Keep the complete AOT return-type plan available while lowering each one.
    let allReturnTypes =
        functions
        |> List.fold
            (fun returnTypes func -> Map.add func.Name func.ReturnType returnTypes)
            externalReturnTypes
    let compileFunctions (functionsToCompile: ANF.Function list) : Result<LIR.Function list, string> =
        if List.isEmpty functionsToCompile then
            Ok []
        else
            if verbosity >= 1 then println $"  [3/7] ANF → MIR{suffix}..."
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
                    allReturnTypes
            match mirResult with
            | Error err -> Error $"MIR conversion error: {err}"
            | Ok (mirFuncs, variantRegistry, mirRecordRegistry) ->
                let mirProgram = MIR.Program (mirFuncs, variantRegistry, mirRecordRegistry)
                let mirElapsed = sw.Elapsed.TotalMilliseconds - mirStart
                recordPassTiming passTimingRecorder "ANF -> MIR" mirElapsed
                if verbosity >= 2 then
                    let t = System.Math.Round(mirElapsed, 1)
                    println $"        {t}ms"
                compileMirToLir (Platform.archFor target) verbosity options sw passTimingRecorder stageSuffix mirProgram
                |> Result.bind (fun lirProgram ->
                    if verbosity >= 1 then println "  [5/7] Register Allocation..."
                    let allocStart = sw.Elapsed.TotalMilliseconds
                    let (LIR.Program (lirFuncs, _, _)) = lirProgram
                    let allocatedFuncs =
                        allocateRegistersForFunctions (Platform.archFor target) lirFuncs
                    let allocElapsed = sw.Elapsed.TotalMilliseconds - allocStart
                    recordPassTiming passTimingRecorder "Register Allocation" allocElapsed
                    if verbosity >= 2 then
                        let t = System.Math.Round(allocElapsed, 1)
                        println $"        {t}ms"
                    let metadataPlanningStart = sw.Elapsed.TotalMilliseconds
                    let funcsWithCodegenFacts =
                        match Platform.archFor target with
                        | Platform.ARM64 ->
                            CodeGen.attachARM64CodegenFactsToFunctionsWithCache
                                releasePlanSummaryCache
                                allocatedFuncs
                        | Platform.X86_64 ->
                            allocatedFuncs
                    let metadataPlanningElapsed =
                        sw.Elapsed.TotalMilliseconds - metadataPlanningStart
                    recordPassTiming
                        passTimingRecorder
                        "ARM64 Function Metadata Planning"
                        metadataPlanningElapsed
                    Ok funcsWithCodegenFacts)

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

let private buildConversionResult
    (program: ANF.Program)
    (registries: AST_to_ANF.Registries)
    : AST_to_ANF.ConversionResult =
    {
        Program = program
        RecursiveMembers = registries.RecursiveMembers
        TypeReg = registries.TypeReg
        RecordFieldsReg = registries.RecordFieldsReg
        RecordTypeParamsReg = registries.RecordTypeParamsReg
        VariantLookup = registries.VariantLookup
        RcSumShapeReg = registries.RcSumShapeReg
        FuncReg = registries.FuncReg
        FuncParams = registries.FuncParams
        ModuleRegistry = registries.ModuleRegistry
    }

/// Run ANF optimization + RC insertion, returning a final ANF function list and type map
let private buildAnf
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (registries: AST_to_ANF.Registries)
    (externalInlineCandidates: Map<string, ANF_Inlining.FunctionInfo>)
    (nonInlineableFunctionNames: Set<string>)
    (functions: ANF.Function list)
    (specializeInternalSignatures: bool)
    (passTimingRecorder: PassTimingRecorder option)
    : Result<ANF.Function list * ANF.TypeMap, string> =

    let anfOptions = buildANFOptimizeOptions options
    let anfPassLabel =
        formatPassGroup
            "ANF Optimizations"
            [
                ("const_folding", anfOptions.EnableConstFolding)
                ("const_prop", anfOptions.EnableConstProp)
                ("copy_prop", anfOptions.EnableCopyProp)
                ("dce", anfOptions.EnableDCE)
                ("cse", anfOptions.EnableCSE)
                ("strength_reduction", anfOptions.EnableStrengthReduction)
            ]
    if verbosity >= 1 then println $"  [2.3/7] {anfPassLabel}..."
    let anfProgram = ANF.Program (functions, ANF.Return ANF.UnitLiteral)
    if shouldDumpIR verbosity options.DumpANF then
        printANFProgram "=== ANF (before optimization) ===" anfProgram
    let anfOptStart = sw.Elapsed.TotalMilliseconds
    let anfOptimizeContext : ANF_Optimize.OptimizeContext =
        { TypeReg = registries.RecordFieldsReg
          RecordTypeParams = registries.RecordTypeParamsReg
          SumShapeReg = registries.RcSumShapeReg }
    let anfOptimized =
        if shouldRunANFOptimize anfOptions then
            ANF_Optimize.optimizeProgramWithOptions anfOptimizeContext anfOptions anfProgram
        else
            anfProgram
    let anfOptElapsed = sw.Elapsed.TotalMilliseconds - anfOptStart
    recordPassTiming passTimingRecorder "ANF Optimizations" anfOptElapsed
    if verbosity >= 2 then
        let t = System.Math.Round(anfOptElapsed, 1)
        println $"        {t}ms"
    if shouldDumpIR verbosity options.DumpANF then
        printANFProgram "=== ANF (after optimization) ===" anfOptimized

    if verbosity >= 1 then println "  [2.4/7] ANF Inlining..."
    let inlineStart = sw.Elapsed.TotalMilliseconds
    let anfInlined =
        if options.DisableInlining then
            anfOptimized
        else
            ANF_Inlining.inlineProgramWithExternalCandidatesAndExclusions
                ANF_Inlining.defaultConfig
                externalInlineCandidates
                nonInlineableFunctionNames
                anfOptimized

    if verbosity >= 1 && specializeInternalSignatures then
        println "  [2.4.4/7] ANF Higher-Order Specialization..."
    let higherOrderStart = sw.Elapsed.TotalMilliseconds
    let anfKnownHigherOrder =
        if options.DisableInlining || not specializeInternalSignatures then
            anfInlined
        else
            ANF_HigherOrderSpecialization.specializeProgram anfInlined
    let higherOrderElapsed = sw.Elapsed.TotalMilliseconds - higherOrderStart
    if specializeInternalSignatures then
        recordPassTiming passTimingRecorder "ANF Higher-Order Specialization" higherOrderElapsed
    if verbosity >= 2 && specializeInternalSignatures then
        let t = System.Math.Round(higherOrderElapsed, 1)
        println $"        {t}ms"

    if verbosity >= 1 && specializeInternalSignatures then
        println "  [2.4.5/7] ANF Direct-Call Specialization..."
    let specializationStart = sw.Elapsed.TotalMilliseconds
    let anfSpecialized =
        if options.DisableInlining || not specializeInternalSignatures then
            anfKnownHigherOrder
        else
            ANF_DirectCallSpecialization.specializeProgram anfKnownHigherOrder
    let specializationElapsed = sw.Elapsed.TotalMilliseconds - specializationStart
    if specializeInternalSignatures then
        recordPassTiming passTimingRecorder "ANF Direct-Call Specialization" specializationElapsed
    if verbosity >= 2 && specializeInternalSignatures then
        let t = System.Math.Round(specializationElapsed, 1)
        println $"        {t}ms"
    let inlineElapsed = sw.Elapsed.TotalMilliseconds - inlineStart
    recordPassTiming passTimingRecorder "ANF Inlining" inlineElapsed
    if verbosity >= 2 then
        let t = System.Math.Round(inlineElapsed, 1)
        println $"        {t}ms"

    let convResult = buildConversionResult anfSpecialized registries

    if verbosity >= 1 then println "  [2.5/7] Reference Count Insertion..."
    let rcStart = sw.Elapsed.TotalMilliseconds
    let rcPhaseRecorder =
        passTimingRecorder
        |> Option.map (fun recorder ->
            fun name (elapsedMs: float) ->
                recorder {
                    Pass = name
                    Elapsed = TimeSpan.FromMilliseconds elapsedMs
                })
    let rcResult =
        RefCountInsertion.insertRCInProgramWithTrace rcPhaseRecorder convResult
    match rcResult with
    | Error err -> Error $"Reference count insertion error: {err}"
    | Ok (anfAfterRC, typeMap) ->
        let rcElapsed = sw.Elapsed.TotalMilliseconds - rcStart
        recordPassTiming passTimingRecorder "Reference Count Insertion" rcElapsed
        if verbosity >= 2 then
            let t = System.Math.Round(rcElapsed, 1)
            println $"        {t}ms"
        if shouldDumpIR verbosity options.DumpANF then
            printANFProgram "=== ANF (after RC insertion) ===" anfAfterRC

        let (ANF.Program (finalFunctions, _)) = anfAfterRC
        Ok (finalFunctions, typeMap)

/// Run tail call detection on a function list (for post-print insertion TCO)
let private applyTco
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (recursiveMembers: Map<string, AST.LoweredRecursiveMember>)
    (functions: ANF.Function list)
    (passTimingRecorder: PassTimingRecorder option)
    : ANF.Function list =
    if verbosity >= 1 then println "  [2.7/7] Tail Call Detection..."
    let tcoStart = sw.Elapsed.TotalMilliseconds
    let anfProgram = ANF.Program (functions, ANF.Return ANF.UnitLiteral)
    let anfAfterTCO =
        if options.DisableTCO then
            anfProgram
        else
            TailCallDetection.detectTailCallsInProgramWithRecursion recursiveMembers anfProgram
    let tcoElapsed = sw.Elapsed.TotalMilliseconds - tcoStart
    recordPassTiming passTimingRecorder "Tail Call Detection" tcoElapsed
    if verbosity >= 2 then
        let t = System.Math.Round(tcoElapsed, 1)
        println $"        {t}ms"
    if shouldDumpIR verbosity options.DumpANF then
        printANFProgram "=== ANF (after Tail Call Detection) ===" anfAfterTCO
    let (ANF.Program (tcoFunctions, _)) = anfAfterTCO
    tcoFunctions

/// Run codegen, encoding, and binary generation
let private generateBinary
    (target: Platform.Target)
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (passTimingRecorder: PassTimingRecorder option)
    (codegenLabel: string)
    (emitLabel: string)
    (dumpAsm: bool)
    (dumpMachineCode: bool)
    (session: CompilationSession option)
    (programContextIdentity: obj)
    (functionGroups: CodeGen.FunctionGroup list)
    (metadataGroups: CodeGen.MetadataGroup list)
    (allocatedProgram: LIR.Program)
    : Result<byte array, string> =

    match target with
    | Platform.LinuxX86_64 ->
        // x86-64 backend
        if verbosity >= 1 then println codegenLabel
        let codegenStart = sw.Elapsed.TotalMilliseconds
        let codegenResult = CodeGen_X86_64.translateProgram allocatedProgram options.EnableLeakCheck
        match codegenResult with
        | Error err -> Error $"x86-64 code generation error: {err}"
        | Ok x86Instructions ->
            let codegenElapsed = sw.Elapsed.TotalMilliseconds - codegenStart
            recordPassTiming passTimingRecorder "Code Generation" codegenElapsed
            if verbosity >= 2 then
                let t = System.Math.Round(codegenElapsed, 1)
                println $"        {t}ms"

            if dumpAsm && verbosity >= 3 then
                println "=== x86-64 Assembly Instructions ==="
                for (i, instr) in List.indexed x86Instructions do
                    println $"  {i}: {instr}"
                println ""

            if verbosity >= 1 then println (emitLabel.Replace("{format}", "ELF"))
            let emitStart = sw.Elapsed.TotalMilliseconds
            let x64StaticStringPool = X86_64_Resolve.collectStringPool x86Instructions
            match X86_64_Resolve.resolveAndEncode x86Instructions with
            | Error err -> Error $"x86-64 resolve error: {err}"
            | Ok resolveResult ->
                // Patch data labels (e.g., leak counter) if there are deferred fixups
                let patchedResult =
                    if List.isEmpty resolveResult.DeferredFixups then
                        Ok resolveResult
                    else
                        let elfHeaderSize = 64
                        let programHeaderSize = 56
                        let codeFileOffset = elfHeaderSize + programHeaderSize
                        let codeSize = resolveResult.MachineCode.Length
                        let dataLabels =
                            X86_64_Resolve.dataLabelOffsets codeFileOffset codeSize x64StaticStringPool
                        X86_64_Resolve.patchDataLabels resolveResult dataLabels codeFileOffset
                match patchedResult with
                | Error err -> Error $"x86-64 data label error: {err}"
                | Ok resolveResult ->
                    match X86_64_Resolve.requireLabelPosition "_start" resolveResult.LabelPositions with
                    | Error err -> Error $"x86-64 resolve error: {err}"
                    | Ok entryOffset ->
                        let binary =
                            Binary_Generation_ELF_X86_64.createExecutableWithPools
                                resolveResult.MachineCode x64StaticStringPool LiteralPool.emptyFloatPool
                                options.EnableLeakCheck entryOffset
                        let emitElapsed = sw.Elapsed.TotalMilliseconds - emitStart
                        recordPassTiming passTimingRecorder "x86-64 Emit" emitElapsed
                        if verbosity >= 2 then
                            let t = System.Math.Round(emitElapsed, 1)
                            println $"        {t}ms"
                        Ok binary

    | Platform.ARM64Backend armTarget ->
        // ARM64 backend (original)
        if verbosity >= 1 then println codegenLabel
        let codegenStart = sw.Elapsed.TotalMilliseconds
        let coverageExprCount = if options.EnableCoverage then LIR.countCoverageHits allocatedProgram else 0
        let codegenOptions : CodeGen.CodeGenOptions = {
            DisableFreeList = options.DisableFreeList
            EnableCoverage = options.EnableCoverage
            CoverageExprCount = coverageExprCount
            EnableLeakCheck = options.EnableLeakCheck
        }
        let arm64Target = ARM64.targetConfigFor armTarget
        let functionContexts =
            let contexts = Dictionary<LIR.Function, obj>(LirFunctionReferenceComparer())
            for group in metadataGroups do
                for func in group.Functions do
                    contexts.[func] <- group.ContextIdentity
            contexts
        let functionCache =
            session
            |> Option.filter (fun _ -> not options.EnableCoverage)
            |> Option.map (fun current ->
                fun func generate ->
                    match functionContexts.TryGetValue func with
                    | true, contextIdentity ->
                        current.CodegenFunction
                            contextIdentity
                            arm64Target
                            codegenOptions
                            func
                            generate
                    | false, _ ->
                        current.CodegenFunction
                            programContextIdentity
                            arm64Target
                            codegenOptions
                            func
                            generate)
        let metadataGroupCache =
            session
            |> Option.map (fun current ->
                fun contextIdentity functions summarize ->
                    current.Arm64MetadataGroup
                        contextIdentity
                        functions
                        summarize)
        let functionGroupCache =
            session
            |> Option.filter (fun _ -> not options.EnableCoverage)
            |> Option.map (fun current ->
                fun contextIdentity functions generate ->
                    current.CodegenFunctionGroup
                        contextIdentity
                        arm64Target
                        codegenOptions
                        functions
                        generate)
        let helperCache =
            session
            |> Option.filter (fun _ -> not options.EnableCoverage)
            |> Option.map (fun current ->
                fun helperKey generate ->
                    current.Arm64Helpers
                        programContextIdentity
                        arm64Target
                        codegenOptions
                        helperKey
                        generate)
        let codegenPhaseRecorder =
            passTimingRecorder
            |> Option.map (fun record ->
                fun name (elapsedMs: float) ->
                    record {
                        Pass = name
                        Elapsed = TimeSpan.FromMilliseconds elapsedMs
                    })
        let codegenResult =
            CodeGen.generateARM64WithOptionsAndCaches
                arm64Target
                codegenOptions
                functionCache
                functionGroupCache
                functionGroups
                metadataGroupCache
                helperCache
                metadataGroups
                codegenPhaseRecorder
                allocatedProgram
        match codegenResult with
        | Error err -> Error $"Code generation error: {err}"
        | Ok arm64Program ->
            let codegenElapsed = sw.Elapsed.TotalMilliseconds - codegenStart
            recordPassTiming passTimingRecorder "Code Generation" codegenElapsed
            if verbosity >= 2 then
                let t = System.Math.Round(codegenElapsed, 1)
                println $"        {t}ms"

            if dumpAsm && verbosity >= 3 then
                let arm64Instructions =
                    CodeGen.generatedProgramInstructions arm64Program
                println "=== ARM64 Assembly Instructions ==="
                for (i, instr) in List.indexed arm64Instructions do
                    println $"  {i}: {instr}"
                println ""

            let os = ARM64.targetOS arm64Target
            let formatName = match os with | Platform.MacOS -> "Mach-O" | Platform.Linux -> "ELF"
            if verbosity >= 1 then println (emitLabel.Replace("{format}", formatName))
            let emitStart = sw.Elapsed.TotalMilliseconds
            let prepareCachedChunk =
                session
                |> Option.map (fun current -> current.PrepareArm64EmissionChunk)
            let emit =
                ARM64_Emit.emitBinary
                    arm64Program
                    os
                    options.EnableLeakCheck
                    prepareCachedChunk
                    codegenPhaseRecorder
            let emitElapsed = sw.Elapsed.TotalMilliseconds - emitStart
            recordPassTiming passTimingRecorder "ARM64 Emit" emitElapsed
            if verbosity >= 2 then
                let t = System.Math.Round(emitElapsed, 1)
                println $"        {t}ms"

            if dumpMachineCode && verbosity >= 3 then
                println "=== Machine Code (hex) ==="
                for i in 0 .. 4 .. (emit.MachineCode.Length - 1) do
                    if i + 3 < emit.MachineCode.Length then
                        let bytes = sprintf "%02x %02x %02x %02x" emit.MachineCode.[i] emit.MachineCode.[i+1] emit.MachineCode.[i+2] emit.MachineCode.[i+3]
                        println $"  {i:X4}: {bytes}"
                println $"Total: {emit.MachineCode.Length} bytes\n"

            Ok emit.Binary


let private buildBaseFuncNames
    (registries: AST_to_ANF.Registries)
    : Set<string> =
    registries.FuncParams
    |> Map.fold (fun acc name _ -> Set.add name acc) Set.empty

let private mergeReturnTypes
    (baseReturnTypes: Map<string, AST.Type>)
    (overlayReturnTypes: Map<string, AST.Type>)
    : Map<string, AST.Type> =
    Map.fold (fun acc k v -> Map.add k v acc) baseReturnTypes overlayReturnTypes

/// Shared compilation context used across pipeline steps
type PipelineContext = {
    Target: Platform.Target
    TypeCheckEnv: TypeChecking.TypeCheckEnv
    GenericFuncDefs: AST_to_ANF.GenericFuncDefs
    SpecRegistry: AST_to_ANF.SpecRegistry
    Registries: AST_to_ANF.Registries
    BaseFuncNames: Set<string>
    ReturnTypes: Map<string, AST.Type>
}

let private buildContext
    (target: Platform.Target)
    (typeCheckEnv: TypeChecking.TypeCheckEnv)
    (genericFuncDefs: AST_to_ANF.GenericFuncDefs)
    (specRegistry: AST_to_ANF.SpecRegistry)
    (registries: AST_to_ANF.Registries)
    (returnTypes: Map<string, AST.Type>)
    : PipelineContext =
    let baseFuncNames = buildBaseFuncNames registries
    {
        Target = target
        TypeCheckEnv = typeCheckEnv
        GenericFuncDefs = genericFuncDefs
        SpecRegistry = specRegistry
        Registries = registries
        BaseFuncNames = baseFuncNames
        ReturnTypes = returnTypes
    }

/// Compiled preamble context - extends stdlib for a test file
/// Preamble functions are compiled ONCE per file, then reused for all tests in that file
type PreambleContext = {
    /// Extended compilation context (stdlib + preamble)
    Context: PipelineContext
    /// Preamble's ANF functions (after mono, inline, lift, ANF, RC, TCO)
    ANFFunctions: ANF.Function list
    /// Type map from RC insertion (merged with stdlib's TypeMap)
    TypeMap: ANF.TypeMap
    /// Preamble's symbolic LIR functions after register allocation
    SymbolicFunctions: LIR.Function list
}

/// Parsed and typechecked preamble analysis for suite-level specialization
type PreambleAnalysis = {
    TypedAST: AST.Program
    TypeCheckEnv: TypeChecking.TypeCheckEnv
    GenericFuncDefs: AST_to_ANF.GenericFuncDefs
}

/// Result of compiling stdlib - can be reused across compilations
type StdlibResult = {
    /// Parsed stdlib AST (for merging with user AST)
    AST: AST.Program
    /// Type-checked stdlib with inferred types
    TypedAST: AST.Program
    /// Shared compilation context (typecheck env + registries)
    Context: PipelineContext
    /// Pre-allocated stdlib functions (physical registers assigned, ready for merge)
    AllocatedFunctions: LIR.Function list
    /// Call graph for dead code elimination (which stdlib funcs call which other funcs)
    StdlibCallGraph: Map<string, Set<string>>
    /// Stdlib ANF functions indexed by name (for coverage analysis)
    StdlibANFFunctions: Map<string, ANF.Function>
    /// Pre-reference-count stdlib ANF functions available as user inlining candidates
    StdlibInlineCandidates: Map<string, ANF_Inlining.FunctionInfo>
    /// Call graph at ANF level (for coverage analysis reachability)
    StdlibANFCallGraph: Map<string, Set<string>>
    /// TypeMap from RC insertion (needed for getReachableStdlibFunctions)
    StdlibTypeMap: ANF.TypeMap
}

/// Context for compiling user code
type CompileContext =
    | StdlibOnly of StdlibResult
    | StdlibWithPreamble of StdlibResult * PreambleContext

/// Recursive custom-type identity retained only at the immutable package-value
/// catalog boundary. Runtime type arguments use the same exact nested custom
/// identities as ValueSearch's ValueType query.
type PackageCustomType = {
    Hash: string
    TypeArguments: PackageCustomType list
}

/// A branch-visible package location. Input order is the interpreter package
/// manager's branch-prioritized order and remains observable during selection.
type CatalogPackageLocation = {
    VisibleInBranches: string list
    Owner: string
    Modules: string list
    Name: string
}

/// Evaluation availability is explicit; missing and failed package evaluation
/// both become None at the public primitive, but are distinct catalog states.
type PackageValueEvaluatorState =
    | Available of AST.Expr
    | Unavailable
    | EvaluationFailure

/// The evaluator's concrete result type is checked before its expression can
/// cross into a monomorphized ValueSearch caller.
type TypedPackageValueEvaluator = {
    ResultType: AST.Type
    State: PackageValueEvaluatorState
}

type PackageValueCatalogEntry = {
    ValueHash: string
    RuntimeType: PackageCustomType
    Locations: CatalogPackageLocation list
    Evaluator: TypedPackageValueEvaluator
}

/// Explicit AOT package snapshot. Unlike the interpreter package manager this
/// value is immutable and contains no database or live branch traversal.
type PackageValueCatalog = PackageValueCatalog of PackageValueCatalogEntry list

let emptyPackageValueCatalog : PackageValueCatalog = PackageValueCatalog []

/// One independently parsed source unit. Ordering is caller-owned and is
/// preserved when declaration overlays are composed.
type SourceUnit = {
    Name: string
    Purpose: NameSyntax.SourceUnitPurpose
    Source: string
}

/// Request for compiling source code
type CompileRequest = {
    Context: CompileContext
    Mode: CompileMode
    Sources: AST.NonEmptyList<SourceUnit>
    AllowInternal: bool
    Verbosity: int
    Options: CompilerOptions
    PackageValues: PackageValueCatalog
    PassTimingRecorder: PassTimingRecorder option
    /// Optional caller-owned bounded reuse scope.
    Session: CompilationSession option
}


// Helper functions for exception-to-Result conversion (Darklang compatibility)

/// Extract return types from a FuncReg (FunctionRegistry maps func name -> full type)
/// This is needed because buildReturnTypeReg only includes functions in the current program,
/// but we need return types for all callable functions (including stdlib)
let private extractReturnTypes (funcReg: Map<string, AST.Type>) : Map<string, AST.Type> =
    funcReg
    |> Map.toSeq
    |> Seq.choose (fun (name, typ) ->
        match typ with
        | AST.TFunction (_, retType) -> Some (name, retType)
        | other -> Crash.crash $"extractReturnTypes: Non-function type '{other}' found in FuncReg for '{name}'")
    |> Map.ofSeq

let private emptyRegistries (moduleRegistry: AST.ModuleRegistry) : AST_to_ANF.Registries =
    {
        TypeReg = Map.empty
        RecordFieldsReg = Map.empty
        RecordTypeParamsReg = Map.empty
        VariantLookup = Map.empty
        RcSumShapeReg = Map.empty
        FuncReg = Map.empty
        FuncParams = Map.empty
        ModuleRegistry = moduleRegistry
        RecursiveMembers = Map.empty
    }

let private liftLambdasWithBase
    (baseRegistries: AST_to_ANF.Registries)
    (baseFuncNames: Set<string>)
    (program: AST.Program)
    : Result<AST.Program, string> =
    let baseFuncReturnTypes = extractReturnTypes baseRegistries.FuncReg
    let baseFuncParamsWithReservedNames =
        baseFuncNames
        |> Set.fold (fun acc name ->
            if Map.containsKey name acc then
                acc
            else
                Map.add name [] acc) baseRegistries.FuncParams
    AST_to_ANF.liftLambdasInProgram
        baseRegistries.TypeReg
        baseRegistries.VariantLookup
        baseFuncParamsWithReservedNames
        baseFuncReturnTypes
        program

let private mergeSpecRegistries
    (baseRegistry: AST_to_ANF.SpecRegistry)
    (overlayRegistry: AST_to_ANF.SpecRegistry)
    : AST_to_ANF.SpecRegistry =
    Map.fold (fun acc key value -> Map.add key value acc) baseRegistry overlayRegistry

let private collectLocalSpecs
    (genericDefs: AST_to_ANF.GenericFuncDefs)
    (program: AST.Program)
    : Set<AST_to_ANF.SpecKey> =
    let (AST.Program topLevels) = program
    let allSpecs =
        topLevels
        |> List.map (function
            | AST.FunctionDef f when List.isEmpty f.TypeParams -> AST_to_ANF.collectTypeAppsFromFunc f
            | AST.Expression e -> AST_to_ANF.collectTypeApps e
            | _ -> Set.empty)
        |> List.fold Set.union Set.empty
    allSpecs
    |> Set.filter (fun (funcName, _) -> Map.containsKey funcName genericDefs)

type private MonomorphizationMode =
    | Monomorphize of AST_to_ANF.GenericFuncDefs option
    | ReplaceTypeApps of AST_to_ANF.SpecRegistry
    | SpecializeLocalAndReplace of AST_to_ANF.SpecRegistry

let private prepareProgramForAnf
    (monomorphization: MonomorphizationMode)
    (baseRegistries: AST_to_ANF.Registries)
    (baseFuncNames: Set<string>)
    (program: AST.Program)
    : Result<AST.Program, string> =
    let monomorphizedResult =
        match monomorphization with
        | Monomorphize None ->
            Ok (AST_to_ANF.monomorphize program)
        | Monomorphize (Some defs) ->
            Ok (AST_to_ANF.monomorphizeWithExternalDefs defs program)
        | ReplaceTypeApps specRegistry ->
            AST_to_ANF.replaceTypeAppsInProgramWithRegistry specRegistry program
        | SpecializeLocalAndReplace specRegistry ->
            let localGenericDefs = AST_to_ANF.extractGenericFuncDefs program
            if Map.isEmpty localGenericDefs then
                AST_to_ANF.replaceTypeAppsInProgramWithRegistry specRegistry program
            else
                let localSpecs = collectLocalSpecs localGenericDefs program
                let specialization = AST_to_ANF.specializeFromSpecs localGenericDefs localSpecs
                let combinedSpecRegistry =
                    mergeSpecRegistries specRegistry specialization.SpecRegistry
                let (AST.Program items) = program
                let specializedTopLevels = specialization.SpecializedFuncs |> List.map AST.FunctionDef
                let programWithSpecializations = AST.Program (specializedTopLevels @ items)
                AST_to_ANF.replaceTypeAppsInProgramWithRegistry combinedSpecRegistry programWithSpecializations
    match monomorphizedResult with
    | Error err -> Error err
    | Ok monomorphized ->
        let (AST.Program topLevels) = monomorphized
        let localFuncNames =
            topLevels
            |> List.choose (function AST.FunctionDef f -> Some f.Name | _ -> None)
            |> Set.ofList
        let knownFuncNames = Set.union baseFuncNames localFuncNames
        let needsLowering = AST_to_ANF.programNeedsLambdaLowering knownFuncNames monomorphized
        if needsLowering then
            let inlined = AST_to_ANF.inlineLambdasInProgram monomorphized
            liftLambdasWithBase baseRegistries baseFuncNames inlined
        else
            Ok monomorphized

let private buildRegistriesForProgram
    (moduleRegistry: AST.ModuleRegistry)
    (baseRegistries: AST_to_ANF.Registries)
    (typeDefs: AST.TypeDef list)
    (functions: AST.FunctionDef list)
    : AST_to_ANF.Registries * AST_to_ANF.Registries * AST.FunctionDef list =
    let aliasReg = AST_to_ANF.buildAliasRegistry typeDefs
    let resolvedFunctions = AST_to_ANF.resolveAliasesInFunctions aliasReg functions
    let localRegistries = AST_to_ANF.buildRegistries moduleRegistry typeDefs aliasReg resolvedFunctions
    let mergedRegistries = AST_to_ANF.mergeRegistries baseRegistries localRegistries
    (mergedRegistries, localRegistries, resolvedFunctions)

type private DeclarationConversion = {
    Functions: ANF.Function list
    Registries: AST_to_ANF.Registries
    LocalReturnTypes: Map<string, AST.Type>
}

let private splitDeclarations
    (AST.Program topLevels)
    : Result<AST.TypeDef list * AST.FunctionDef list, string> =
    let expressions =
        topLevels |> List.choose (function AST.Expression expression -> Some expression | _ -> None)
    if not (List.isEmpty expressions) then
        Error $"Declaration-only program must not contain entry expressions; found {expressions.Length}"
    else
        Ok (
            topLevels |> List.choose (function AST.TypeDef definition -> Some definition | _ -> None),
            topLevels |> List.choose (function AST.FunctionDef definition -> Some definition | _ -> None)
        )

let private convertTypedDeclarations
    (baseContext: PipelineContext option)
    (monomorphization: MonomorphizationMode)
    (typedProgram: AST.Program)
    : Result<DeclarationConversion, string> =
    let moduleRegistry =
        baseContext
        |> Option.map (fun context -> context.Registries.ModuleRegistry)
        |> Option.defaultWith Stdlib.buildModuleRegistry
    let baseRegistries =
        baseContext
        |> Option.map (fun context -> context.Registries)
        |> Option.defaultValue (emptyRegistries moduleRegistry)
    let baseFuncNames =
        baseContext
        |> Option.map (fun context -> context.BaseFuncNames)
        |> Option.defaultValue (buildBaseFuncNames baseRegistries)
    prepareProgramForAnf monomorphization baseRegistries baseFuncNames typedProgram
    |> Result.bind (fun liftedProgram ->
        splitDeclarations liftedProgram
        |> Result.bind (fun (typeDefs, functions) ->
            let (registries, localRegistries, resolvedFunctions) =
                buildRegistriesForProgram moduleRegistry baseRegistries typeDefs functions
            AST_to_ANF.convertFunctions registries (ANF.VarGen 0) resolvedFunctions
            |> Result.map (fun (anfFunctions, _) ->
                { Functions = anfFunctions
                  Registries = registries
                  LocalReturnTypes = extractReturnTypes localRegistries.FuncReg })))

let private convertTypedProgramToConversionResult
    (moduleRegistry: AST.ModuleRegistry)
    (typedProgram: AST.Program)
    : Result<AST_to_ANF.ConversionResult, string> =
    let baseRegistries = emptyRegistries moduleRegistry
    let baseFuncNames = buildBaseFuncNames baseRegistries
    prepareProgramForAnf (Monomorphize None) baseRegistries baseFuncNames typedProgram
    |> Result.bind (fun liftedProgram ->
        AST_to_ANF.splitTopLevels liftedProgram
        |> Result.bind (fun (typeDefs, functions, expr) ->
            let (registries, _localRegistries, resolvedFunctions) =
                buildRegistriesForProgram moduleRegistry baseRegistries typeDefs functions
            let varGen = ANF.VarGen 0
            AST_to_ANF.convertFunctions registries varGen resolvedFunctions
            |> Result.bind (fun (anfFuncs, varGen1) ->
                AST_to_ANF.convertExprToAnf registries varGen1 expr
                |> Result.map (fun (anfExpr, _) ->
                    buildConversionResult (ANF.Program (anfFuncs, anfExpr)) registries))))

let private convertTypedProgramToUserOnlyWithMode
    (baseContext: PipelineContext)
    (monomorphization: MonomorphizationMode)
    (typeCheckEnv: TypeChecking.TypeCheckEnv)
    (session: CompilationSession option)
    (passTimingRecorder: PassTimingRecorder option)
    (typedProgram: AST.Program)
    : Result<AST_to_ANF.UserOnlyResult * obj, string> =
    let measure name operation =
        let timer = Stopwatch.StartNew()
        let result = operation ()
        timer.Stop()
        recordPassTiming passTimingRecorder name timer.Elapsed.TotalMilliseconds
        result

    // Late AOT plans (notably Json) may introduce concrete calls to generic
    // stdlib functions after the suite preamble registry was built. Materialize
    // just those missing specializations into the user compilation unit.
    let (typedProgram, monomorphization, nonInlineableFunctionNames) =
        measure "AST -> ANF Dependency Planning" (fun () ->
            let addMissing baseRegistry rebuildMode =
                let requested = collectLocalSpecs baseContext.GenericFuncDefs typedProgram
                let (AST.Program items) = typedProgram
                let knownFunctionNames =
                    items
                    |> List.choose (function
                        | AST.FunctionDef fn -> Some fn.Name
                        | _ -> None)
                    |> Set.ofList
                    |> Set.union baseContext.BaseFuncNames
                let rec materialize
                    (specRegistry: AST_to_ANF.SpecRegistry)
                    (knownFunctionNames: Set<string>)
                    (pendingSpecs: Set<AST_to_ANF.SpecKey>)
                    (accFunctions: AST.FunctionDef list)
                    : AST_to_ANF.SpecRegistry * AST.FunctionDef list =
                    let missingSpecs =
                        pendingSpecs
                        |> Set.filter (fun key -> not (Map.containsKey key specRegistry))
                    if Set.isEmpty missingSpecs then
                        (specRegistry, accFunctions)
                    else
                        let specialization =
                            AST_to_ANF.specializeFromSpecs baseContext.GenericFuncDefs missingSpecs
                        let combinedRegistry =
                            mergeSpecRegistries specRegistry specialization.SpecRegistry
                        let materializedTopLevels =
                            specialization.SpecializedFuncs
                            |> List.filter (fun fn -> not (Set.contains fn.Name knownFunctionNames))
                            |> List.map AST.FunctionDef
                            |> TypeChecking.materializeEqHelpersInTopLevels
                                typeCheckEnv.AliasReg
                                typeCheckEnv.IndexedTypeReg
                                typeCheckEnv.VariantLookup
                        let newFunctions =
                            materializedTopLevels
                            |> List.choose (function
                                | AST.FunctionDef fn when not (Set.contains fn.Name knownFunctionNames) -> Some fn
                                | _ -> None)
                        let nextKnownFunctionNames =
                            newFunctions
                            |> List.fold (fun names fn -> Set.add fn.Name names) knownFunctionNames
                        let nextSpecs =
                            materializedTopLevels
                            |> AST.Program
                            |> collectLocalSpecs baseContext.GenericFuncDefs
                        materialize
                            combinedRegistry
                            nextKnownFunctionNames
                            nextSpecs
                            (accFunctions @ newFunctions)

                let (combinedRegistry, newFunctions) =
                    materialize baseRegistry knownFunctionNames requested []
                let programWithSpecializations =
                    AST.Program ((newFunctions |> List.map AST.FunctionDef) @ items)
                let specializedFunctionNames =
                    newFunctions |> List.map (fun fn -> fn.Name) |> Set.ofList
                (programWithSpecializations, rebuildMode combinedRegistry, specializedFunctionNames)
            match monomorphization with
            | ReplaceTypeApps registry -> addMissing registry ReplaceTypeApps
            | SpecializeLocalAndReplace registry -> addMissing registry SpecializeLocalAndReplace
            | Monomorphize _ -> (typedProgram, monomorphization, Set.empty))
    let baseFuncNames = baseContext.BaseFuncNames
    measure "AST -> ANF Program Preparation" (fun () ->
        prepareProgramForAnf monomorphization baseContext.Registries baseFuncNames typedProgram)
    |> Result.bind (fun liftedProgram ->
        measure "AST -> ANF Registry Construction" (fun () ->
            AST_to_ANF.splitTopLevels liftedProgram
            |> Result.map (fun (typeDefs, functions, expr) ->
                let (registries, localRegistries, resolvedFunctions) =
                    buildRegistriesForProgram baseContext.Registries.ModuleRegistry baseContext.Registries typeDefs functions
                let localReturnTypes = extractReturnTypes localRegistries.FuncReg
                (registries, localRegistries, resolvedFunctions, localReturnTypes, expr)))
        |> Result.bind (fun (registries, localRegistries, resolvedFunctions, localReturnTypes, expr) ->
            let varGen = ANF.VarGen 0
            let conversionKey = {
                Functions = resolvedFunctions
                LocalRegistries = localRegistries
                NonInlineableFunctionNames = nonInlineableFunctionNames
            }
            let convert () =
                measure "AST -> ANF Dependency Conversion" (fun () ->
                    AST_to_ANF.convertFunctions registries varGen resolvedFunctions)
            let convertedDependencies =
                measure "AST -> ANF Dependency Lookup" (fun () ->
                    match session with
                    | Some current ->
                        current.ConvertAnfDependencies
                            (box baseContext)
                            conversionKey
                            convert
                    | None ->
                        convert ()
                        |> Result.map (fun (anfFuncs, varGen1) ->
                            (anfFuncs, varGen1, System.Object())))
            convertedDependencies
            |> Result.bind (fun (anfFuncs, varGen1, dependencyIdentity) ->
                measure "AST -> ANF Expression Conversion" (fun () ->
                    AST_to_ANF.convertExprToAnf registries varGen1 expr)
                |> Result.map (fun (anfExpr, _) ->
                    ({
                        UserFunctions = anfFuncs
                        NonInlineableFunctionNames = nonInlineableFunctionNames
                        MainExpr = anfExpr
                        TypeReg = registries.TypeReg
                        RecordFieldsReg = registries.RecordFieldsReg
                        RecordTypeParamsReg = registries.RecordTypeParamsReg
                        VariantLookup = registries.VariantLookup
                        RcSumShapeReg = registries.RcSumShapeReg
                        FuncReg = registries.FuncReg
                        LocalReturnTypes = localReturnTypes
                        FuncParams = registries.FuncParams
                        ModuleRegistry = registries.ModuleRegistry
                        RecursiveMembers = registries.RecursiveMembers
                     },
                     dependencyIdentity)))))

let private convertTypedProgramToUserOnly
    (baseContext: PipelineContext)
    (typedProgram: AST.Program)
    : Result<AST_to_ANF.UserOnlyResult, string> =
    convertTypedProgramToUserOnlyWithMode
        baseContext
        (Monomorphize (Some baseContext.GenericFuncDefs))
        baseContext.TypeCheckEnv
        None
        None
        typedProgram
    |> Result.map fst

/// Try to delete a file, ignoring any errors
let private tryDeleteFile (path: string) : unit =
    try File.Delete(path) with _ -> ()

/// Try to start a process, returning Result instead of throwing
let private tryStartProcess (info: ProcessStartInfo) : Result<Process, string> =
    try Ok (Process.Start(info))
    with ex -> Error ex.Message

let private checkProgramWithBaseEnv
    (warningSettings: AST.WarningSettings)
    (baseEnv: TypeChecking.TypeCheckEnv)
    (program: AST.Program)
    : Result<AST.Type * AST.Program * TypeChecking.TypeCheckEnv, TypeChecking.TypeError> =
    TypeChecking.checkProgramWithBaseEnvAndSettings baseEnv true warningSettings program

let private checkSyntheticPreambleWithBaseEnv
    (warningSettings: AST.WarningSettings)
    (baseEnv: TypeChecking.TypeCheckEnv)
    (program: AST.Program)
    : Result<AST.Type * AST.Program * TypeChecking.TypeCheckEnv, TypeChecking.TypeError> =
    TypeChecking.checkSyntheticPreambleWithBaseEnvAndSettings
        baseEnv
        true
        warningSettings
        program

/// Parse and typecheck a preamble, returning typed AST + preamble typecheck env
let analyzePreamble
    (allowInternal: bool)
    (stdlib: StdlibResult)
    (preamble: string)
    : Result<PreambleAnalysis, string> =
    InterpreterParser.parseString allowInternal preamble
    |> Result.mapError (fun err -> $"Preamble parse error: {err}")
    |> Result.bind (fun preambleAst ->
        checkSyntheticPreambleWithBaseEnv
            defaultWarningSettings
            stdlib.Context.TypeCheckEnv
            preambleAst
        |> Result.mapError (fun typeErr -> $"Preamble type error: {TypeChecking.typeErrorToString typeErr}")
        |> Result.map (fun (_programType, typedPreambleAst, preambleTypeCheckEnv) ->
            let preambleGenericDefs = AST_to_ANF.extractGenericFuncDefs typedPreambleAst
            {
                TypedAST = typedPreambleAst
                TypeCheckEnv = preambleTypeCheckEnv
                GenericFuncDefs = preambleGenericDefs
            }))

/// Load a .dark file allowing internal identifiers (for stdlib sources)
let private loadDarkFileAllowInternal (filename: string) : Result<AST.Program, string> =
    let exePath = Assembly.GetExecutingAssembly().Location
    let exeDir = Path.GetDirectoryName(exePath)
    let possiblePaths = [
        Path.Combine(exeDir, filename)
        Path.Combine(exeDir, "..", "..", "..", "..", "src", "DarkCompiler", filename)
        Path.Combine(Environment.CurrentDirectory, "src", "DarkCompiler", filename)
    ]
    let filePath = possiblePaths |> List.tryFind File.Exists
    match filePath with
    | None ->
        let pathsStr = String.Join(", ", possiblePaths)
        Error $"Could not find {filename} in any of: {pathsStr}"
    | Some path ->
        let source = File.ReadAllText(path)
        InterpreterParser.parseString true source
        |> Result.mapError (fun err -> $"Error parsing {filename}: {err}")

/// Load the stdlib and unicode_data.dark files
/// Returns the merged stdlib AST or an error message
let private loadStdlib () : Result<AST.Program, string> =
    let stdlibFiles = [
        "stdlib/Types.dark"
        "stdlib/NoModule.dark"
        "stdlib/Int8.dark"
        "stdlib/Int16.dark"
        "stdlib/Int32.dark"
        "stdlib/Int64.dark"
        "stdlib/__Integer.dark"
        "stdlib/Int.dark"
        "stdlib/Int128.dark"
        "stdlib/UInt8.dark"
        "stdlib/UInt16.dark"
        "stdlib/UInt32.dark"
        "stdlib/UInt64.dark"
        "stdlib/UInt128.dark"
        "stdlib/Bool.dark"
        "stdlib/Builtin.dark"
        "stdlib/Tuple2.dark"
        "stdlib/Tuple3.dark"
        "stdlib/Result.dark"
        "stdlib/Option.dark"
        "stdlib/ListSortByComparatorHelpers.dark"
        "stdlib/List.dark"
        "stdlib/Print.dark"
        "stdlib/Fun.dark"
        "stdlib/Float.dark"
        "stdlib/CliPosix.dark"
        "stdlib/Retry.dark"
        "stdlib/CliPosixError.dark"
        "stdlib/CliPath.dark"
        "stdlib/CliFile.dark"
        "stdlib/Path.dark"
        "unicode_data.dark"
        "unicode_data_index/00.dark"
        "unicode_data_index/01.dark"
        "unicode_data_index/02.dark"
        "unicode_data_index/03.dark"
        "unicode_data_index/04.dark"
        "unicode_data_index/05.dark"
        "unicode_data_index/06.dark"
        "unicode_data/00.dark"
        "unicode_data/01.dark"
        "unicode_data/02.dark"
        "unicode_data/03.dark"
        "unicode_data/04.dark"
        "unicode_data/05.dark"
        "unicode_data/06.dark"
        "unicode_data/07.dark"
        "unicode_data/08.dark"
        "unicode_data/09.dark"
        "unicode_data/10.dark"
        "unicode_data/11.dark"
        "unicode_data/12.dark"
        "unicode_data/13.dark"
        "unicode_data/14.dark"
        "unicode_data/15.dark"
        "unicode_data/16.dark"
        "unicode_data/17.dark"
        "unicode_data/18.dark"
        "unicode_data/19.dark"
        "unicode_data/20.dark"
        "unicode_data/21.dark"
        "unicode_data/22.dark"
        "unicode_data/23.dark"
        "unicode_data/24.dark"
        "unicode_data/25.dark"
        "unicode_data/26.dark"
        "unicode_data/27.dark"
        "unicode_data/28.dark"
        "unicode_data/29.dark"
        "unicode_data/30.dark"
        "unicode_data/31.dark"
        "unicode_data/32.dark"
        "unicode_data/33.dark"
        "unicode_data/34.dark"
        "unicode_data/35.dark"
        "unicode_data/36.dark"
        "unicode_data/37.dark"
        "unicode_data/38.dark"
        "unicode_data/39.dark"
        "unicode_data/40.dark"
        "unicode_data/41.dark"
        "unicode_data/42.dark"
        "unicode_data/43.dark"
        "unicode_data/44.dark"
        "unicode_data/45.dark"
        "unicode_data/46.dark"
        "unicode_data/47.dark"
        "unicode_data/48.dark"
        "unicode_data/49.dark"
        "unicode_data/50.dark"
        "unicode_data/51.dark"
        "unicode_data/52.dark"
        "unicode_data/53.dark"
        "unicode_data/54.dark"
        "unicode_data/55.dark"
        "unicode_data/56.dark"
        "unicode_data/57.dark"
        "unicode_data/58.dark"
        "unicode_data/59.dark"
        "unicode_data/60.dark"
        "unicode_data/61.dark"
        "unicode_data/62.dark"
        "unicode_data/63.dark"
        "stdlib/Unicode.dark"
        "stdlib/String.dark"
        "stdlib/__Hash.dark"
        "stdlib/Dict.dark"
        "stdlib/__HAMT.dark"
        "stdlib/Uuid.dark"
        "stdlib/UuidCompatibility.dark"
        "stdlib/Diff.dark"
        "stdlib/ProgramTypes.dark"
        "stdlib/RuntimeTypes.dark"
        "stdlib/RuntimeTypesBase.dark"
        "stdlib/RuntimeFQTypeName.dark"
        "stdlib/RuntimeTypeReference.dark"
        "stdlib/RuntimeValueType.dark"
        "stdlib/RuntimeValueTypeSupport.dark"
        "stdlib/PackageManager.dark"
        "stdlib/ValueSearch.dark"
        "stdlib/DateTime.dark"
        "stdlib/Duration.dark"
        "stdlib/Bytes.dark"
        "stdlib/Blob.dark"
        "stdlib/Stream.dark"
        "stdlib/Html.dark"
        "stdlib/Http.dark"
        "stdlib/HttpRequest.dark"
        "stdlib/Char.dark"
        "stdlib/Regex.dark"
        "stdlib/AWS.dark"
        "stdlib/Twitter.dark"
        "stdlib/Base64.dark"
        "stdlib/X509.dark"
        "stdlib/Crypto.dark"
        "stdlib/Math.dark"
        "stdlib/__SkewList.dark"
        "stdlib/CliColor.dark"
        "stdlib/CliLog.dark"
        "stdlib/CliProgress.dark"
        "stdlib/CliPrompt.dark"
        "stdlib/CliSpinner.dark"
        "stdlib/CliTable.dark"
        "stdlib/CliExecution.dark"
        "stdlib/CliOS.dark"
        "stdlib/CliArchitecture.dark"
        "stdlib/CliShell.dark"
        "stdlib/CliHost.dark"
        "stdlib/CliEnv.dark"
        "stdlib/CliArgs.dark"
        "stdlib/CliProcess.dark"
        "stdlib/CliSys.dark"
        "stdlib/CliStdin.dark"
        "stdlib/CliStdinModifiers.dark"
        "stdlib/CliStdinKeyRead.dark"
        "stdlib/CliStdinRead.dark"
        "stdlib/AltJsonParseError.dark"
        "stdlib/AltJson.dark"
        "stdlib/AltJsonHelpers.dark"
        "stdlib/AltJsonBuilder.dark"
        "stdlib/LanguageTools.dark"
        "stdlib/JsonPathPart.dark"
        "stdlib/JsonPath.dark"
        "stdlib/JsonParseError.dark"
        "stdlib/Json.dark"
    ]
    let mergeFile (acc: AST.TopLevel list) (filename: string) : Result<AST.TopLevel list, string> =
        match loadDarkFileAllowInternal filename with
        | Error err -> Error err
        | Ok (AST.Program items) ->
            Ok (acc @ items)
    stdlibFiles
    |> List.fold (fun acc filename -> Result.bind (fun items -> mergeFile items filename) acc) (Ok [])
    |> Result.bind (fun items -> Ok (AST.Program items))


/// Build stdlib in isolation, returning reusable result
/// This can be called once and the result reused for multiple user program compilations
let buildStdlibWithTrace
    (target: Platform.Target)
    (passTimingRecorder: PassTimingRecorder option)
    : Result<StdlibResult, string> =
    match loadStdlib() with
    | Error e ->
        Error e
    | Ok stdlibAst ->
        match TypeChecking.checkDeclarationProgramWithEnv stdlibAst with
        | Error e ->
            let msg = TypeChecking.typeErrorToString e
            Error msg
        | Ok (_, typedStdlib, typeCheckEnv) ->
            // Extract generic function definitions for on-demand monomorphization
            let genericFuncDefs = AST_to_ANF.extractGenericFuncDefs typedStdlib
            // Build module registry once (reused across all compilations)
            let moduleRegistry = Stdlib.buildModuleRegistry ()
            match
                convertTypedDeclarations
                    None
                    (Monomorphize None)
                    typedStdlib
            with
            | Error e ->
                Error e
            | Ok anfResult ->
                let sw = Stopwatch.StartNew()
                let registries = anfResult.Registries
                let returnTypes = extractReturnTypes registries.FuncReg
                let context = buildContext target typeCheckEnv genericFuncDefs Map.empty registries returnTypes
                let stdlibFunctions = anfResult.Functions
                let stdlibOptions = { defaultOptions with DisableANFOpt = true; DisableInlining = true }
                match buildAnf 0 stdlibOptions sw registries Map.empty Set.empty stdlibFunctions false passTimingRecorder with
                | Error e ->
                    Error e
                | Ok (anfFunctions, typeMap) ->
                    let tcoFunctions = applyTco 0 stdlibOptions sw registries.RecursiveMembers anfFunctions passTimingRecorder
                    let stdlibFuncMap =
                        tcoFunctions
                        |> List.map (fun f -> f.Name, f)
                        |> Map.ofList
                    let stdlibInlineCandidates =
                        stdlibFunctions
                        |> ANF_Inlining.buildExternalCandidateInfoMap ANF_Inlining.defaultConfig
                    let stdlibLiftedFuncNames =
                        tcoFunctions
                        |> List.map (fun f -> f.Name)
                        |> Set.ofList
                    let contextWithLiftedNames = {
                        context with
                            BaseFuncNames = Set.union context.BaseFuncNames stdlibLiftedFuncNames
                    }
                    let stdlibANFCallGraph = ANFDeadCodeElimination.buildCallGraph tcoFunctions

                    let externalReturnTypes = returnTypes
                    match lowerToAllocatedLir
                        target
                        0
                        stdlibOptions
                        sw
                        passTimingRecorder
                        None
                        "stdlib"
                        tcoFunctions
                        typeMap
                        registries
                        None
                        externalReturnTypes with
                    | Error e ->
                        Error e
                    | Ok allocatedFuncs ->
                        let stdlibCallGraph = DeadCodeElimination.buildCallGraph allocatedFuncs
                        Ok {
                            AST = stdlibAst
                            TypedAST = typedStdlib
                            Context = contextWithLiftedNames
                            AllocatedFunctions = allocatedFuncs
                            StdlibCallGraph = stdlibCallGraph
                            StdlibANFFunctions = stdlibFuncMap
                            StdlibInlineCandidates = stdlibInlineCandidates
                            StdlibANFCallGraph = stdlibANFCallGraph
                            StdlibTypeMap = typeMap
                        }

/// Build stdlib in isolation with default settings
let buildStdlib (target: Platform.Target) : Result<StdlibResult, string> =
    buildStdlibWithTrace target None

/// Build stdlib specializations for a spec set and merge them into the stdlib result
let buildStdlibSpecializations
    (stdlib: StdlibResult)
    (specs: Set<AST_to_ANF.SpecKey>)
    (externalTypeReg: AST_to_ANF.TypeRegistry)
    (externalVariantLookup: AST_to_ANF.VariantLookup)
    (passTimingRecorder: PassTimingRecorder option)
    : Result<StdlibResult, string> =
    if Set.isEmpty specs then
        Ok stdlib
    else
        let materializationVariantLookup =
            Map.fold
                (fun acc name variant -> Map.add name variant acc)
                stdlib.Context.TypeCheckEnv.VariantLookup
                externalVariantLookup
        let externalIndexedTypeReg =
            TypeChecking.indexTypeRegistry
                materializationVariantLookup
                (AST_to_ANF.recordTypeParamsRegistry externalTypeReg)
                (AST_to_ANF.recordFieldsRegistry externalTypeReg)
        let materializationTypeReg =
            Map.fold
                (fun acc name typeInfo -> Map.add name typeInfo acc)
                stdlib.Context.TypeCheckEnv.IndexedTypeReg
                externalIndexedTypeReg
        let specialization = AST_to_ANF.specializeFromSpecs stdlib.Context.GenericFuncDefs specs
        let initialCombinedSpecRegistry = mergeSpecRegistries stdlib.Context.SpecRegistry specialization.SpecRegistry
        let existingNames =
            stdlib.StdlibANFFunctions
            |> Map.keys
            |> Set.ofSeq
        let newSpecializedFuncs =
            specialization.SpecializedFuncs
            |> List.filter (fun f -> not (Set.contains f.Name existingNames))

        if List.isEmpty newSpecializedFuncs then
            let updatedContext = { stdlib.Context with SpecRegistry = initialCombinedSpecRegistry }
            Ok {
                stdlib with
                    Context = updatedContext
            }
        else
            AST_to_ANF.splitDeclarations stdlib.TypedAST
            |> Result.bind (fun (typeDefs, _functions) ->
                let initiallyMaterializedFunctions =
                    newSpecializedFuncs
                    |> List.collect (fun funcDef ->
                        [AST.FunctionDef funcDef]
                        |> TypeChecking.materializeEqHelpersInTopLevels
                            stdlib.Context.TypeCheckEnv.AliasReg
                            materializationTypeReg
                            materializationVariantLookup)
                    |> List.choose (function
                        | AST.FunctionDef funcDef -> Some funcDef
                        | _ -> None)
                let helperSpecs =
                    initiallyMaterializedFunctions
                    |> List.map AST_to_ANF.collectTypeAppsFromFunc
                    |> List.fold Set.union Set.empty
                    |> Set.filter (fun (funcName, _) ->
                        Map.containsKey funcName stdlib.Context.GenericFuncDefs)
                let helperSpecialization =
                    AST_to_ANF.specializeFromSpecs stdlib.Context.GenericFuncDefs helperSpecs
                let combinedSpecRegistry =
                    mergeSpecRegistries initialCombinedSpecRegistry helperSpecialization.SpecRegistry
                let materializedFunctions =
                    (helperSpecialization.SpecializedFuncs @ initiallyMaterializedFunctions)
                    |> List.filter (fun f -> not (Set.contains f.Name existingNames))
                    |> List.map AST.FunctionDef
                    |> TypeChecking.materializeEqHelpersInTopLevels
                        stdlib.Context.TypeCheckEnv.AliasReg
                        materializationTypeReg
                        materializationVariantLookup
                    |> List.choose (function
                        | AST.FunctionDef funcDef -> Some funcDef
                        | _ -> None)
                    |> List.distinctBy (fun funcDef -> funcDef.Name)
                let specializationProgram =
                    AST.Program (
                        (typeDefs |> List.map AST.TypeDef)
                        @ (materializedFunctions |> List.map AST.FunctionDef)
                    )
                prepareProgramForAnf
                    (ReplaceTypeApps combinedSpecRegistry)
                    stdlib.Context.Registries
                    stdlib.Context.BaseFuncNames
                    specializationProgram
                |> Result.bind AST_to_ANF.splitDeclarations
                |> Result.bind (fun (preparedTypeDefs, preparedFunctions) ->
                    let (registries, localRegistries, resolvedFunctions) =
                        buildRegistriesForProgram
                            stdlib.Context.Registries.ModuleRegistry
                            stdlib.Context.Registries
                            preparedTypeDefs
                            preparedFunctions
                    let registries = {
                        registries with
                            TypeReg =
                                Map.fold
                                    (fun acc name recordInfo -> Map.add name recordInfo acc)
                                    registries.TypeReg
                                    externalTypeReg
                            VariantLookup =
                                Map.fold (fun acc k v -> Map.add k v acc) registries.VariantLookup externalVariantLookup
                    }
                    let localReturnTypes = extractReturnTypes localRegistries.FuncReg
                    let varGen = ANF.VarGen 0
                    AST_to_ANF.convertFunctions registries varGen resolvedFunctions
                    |> Result.bind (fun (anfFuncs, _varGen1) ->
                        let stdlibOptions = { defaultOptions with DisableANFOpt = true; DisableInlining = true }
                        let sw = Stopwatch.StartNew()
                        buildAnf 0 stdlibOptions sw registries Map.empty Set.empty anfFuncs false passTimingRecorder
                        |> Result.bind (fun (anfFunctions, typeMap) ->
                            let tcoFunctions = applyTco 0 stdlibOptions sw registries.RecursiveMembers anfFunctions passTimingRecorder
                            let newAnfFuncMap =
                                tcoFunctions
                                |> List.map (fun f -> f.Name, f)
                                |> Map.ofList
                            let externalReturnTypes =
                                mergeReturnTypes stdlib.Context.ReturnTypes localReturnTypes
                            lowerToAllocatedLir
                                stdlib.Context.Target
                                0
                                stdlibOptions
                                sw
                                passTimingRecorder
                                None
                                "stdlib_specializations"
                                tcoFunctions
                                typeMap
                                registries
                                None
                                externalReturnTypes
                            |> Result.bind (fun allocatedFuncs ->
                                let allLirFuncs = stdlib.AllocatedFunctions @ allocatedFuncs
                                let mergedStdlibTypeMap =
                                    Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibTypeMap typeMap
                                let mergedStdlibAnfFunctions =
                                    Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibANFFunctions newAnfFuncMap
                                let newInlineCandidateMap =
                                    anfFuncs
                                    |> ANF_Inlining.buildExternalCandidateInfoMap ANF_Inlining.defaultConfig
                                let mergedStdlibInlineCandidates =
                                    Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibInlineCandidates newInlineCandidateMap
                                let allAnfFunctions =
                                    mergedStdlibAnfFunctions
                                    |> Map.toList
                                    |> List.map snd
                                let stdlibCallGraph = DeadCodeElimination.buildCallGraph allLirFuncs
                                let stdlibAnfCallGraph = ANFDeadCodeElimination.buildCallGraph allAnfFunctions
                                let specializedFuncNames =
                                    mergedStdlibAnfFunctions
                                    |> Map.keys
                                    |> Set.ofSeq
                                let baseFuncNames =
                                    Set.union
                                        stdlib.Context.BaseFuncNames
                                        (Set.union (buildBaseFuncNames registries) specializedFuncNames)
                                let updatedContext = {
                                    stdlib.Context with
                                        Registries = registries
                                        SpecRegistry = combinedSpecRegistry
                                        BaseFuncNames = baseFuncNames
                                        ReturnTypes = externalReturnTypes
                                }
                                Ok {
                                    stdlib with
                                        Context = updatedContext
                                        AllocatedFunctions = allLirFuncs
                                        StdlibCallGraph = stdlibCallGraph
                                        StdlibANFFunctions = mergedStdlibAnfFunctions
                                        StdlibInlineCandidates = mergedStdlibInlineCandidates
                                        StdlibANFCallGraph = stdlibAnfCallGraph
                                        StdlibTypeMap = mergedStdlibTypeMap
                                }
                            )
                        )
                    )
                )
            )

type private UserCompileLabels = {
    Parse: string
    TypeCheck: string
    Anf: string
    StageSuffix: string
}

type private UserCompilePlan = {
    AllowInternal: bool
    Mode: CompileMode
    Verbosity: int
    Options: CompilerOptions
    PackageValues: PackageValueCatalog
    PassTimingRecorder: PassTimingRecorder option
    Session: CompilationSession option
    Stdlib: StdlibResult
    BaseContext: PipelineContext
    Monomorphization: MonomorphizationMode
    ExternalInlineCandidates: Map<string, ANF_Inlining.FunctionInfo>
    PrebuiltSymbolicFunctions: LIR.Function list
    SkipFunctionNames: Set<string>
    EmitFunctionEvents: bool
    TreeShakeUserFunctions: bool
    Labels: UserCompileLabels
    Sources: AST.NonEmptyList<SourceUnit>
}

/// Parse source text into AST using the canonical interpreter syntax.
let parseProgram
    (allowInternal: bool)
    (source: string)
    : Result<AST.Program, string> =
    InterpreterParser.parseString allowInternal source

let private parseSourceTree
    (allowInternal: bool)
    (source: string)
    : Result<NameSyntax.ParsedSource, string> =
    InterpreterParser.parseSourceString allowInternal source

let private applyDeclarationOverlays (topLevels: AST.TopLevel list) : AST.TopLevel list =
    let declarationKey topLevel =
        match topLevel with
        | AST.FunctionDef definition -> Some ("function", definition.Name)
        | AST.TypeDef (AST.RecordDef (name, _, _))
        | AST.TypeDef (AST.SumTypeDef (name, _, _))
        | AST.TypeDef (AST.TypeAlias (name, _, _)) -> Some ("type", name)
        | AST.Expression _ -> None
    let winningIndices =
        topLevels
        |> List.indexed
        |> List.choose (fun (index, topLevel) ->
            declarationKey topLevel |> Option.map (fun key -> (key, index)))
        |> Map.ofList
    topLevels
    |> List.indexed
    |> List.choose (fun (index, topLevel) ->
        declarationKey topLevel
        |> Option.map (fun key -> if Map.tryFind key winningIndices = Some index then Some topLevel else None)
        |> Option.defaultValue (Some topLevel))

/// Parse every source unit independently and validate entry ownership before
/// crossing into the expression-oriented lowering AST.
let parseSourceProgram
    (allowInternal: bool)
    (sources: AST.NonEmptyList<SourceUnit>)
    : Result<NameSyntax.ValidatedExecutableProgram * AST.Program, string> =
    let rec parseUnits remaining parsedUnits loweredTopLevels =
        match remaining with
        | [] ->
            let sourceProgram =
                parsedUnits
                |> List.rev
                |> AST.NonEmptyList.fromList
                |> NameSyntax.createSourceProgram
            NameSyntax.validateExecutableProgram sourceProgram
            |> Result.map (fun validated ->
                let composedTopLevels = List.rev loweredTopLevels |> List.collect id
                (validated, AST.Program (applyDeclarationOverlays composedTopLevels)))
        | sourceUnit :: rest ->
            NameSyntax.sourceUnitName sourceUnit.Name
            |> Result.bind (fun name ->
                parseSourceTree allowInternal sourceUnit.Source
                |> Result.bind (fun parsed ->
                    parseProgram allowInternal sourceUnit.Source
                    |> Result.bind (fun (AST.Program topLevels) ->
                        let parsedUnit : NameSyntax.ParsedSourceUnit =
                            { Name = name
                              Purpose = sourceUnit.Purpose
                              Source = parsed }
                        parseUnits rest (parsedUnit :: parsedUnits) (topLevels :: loweredTopLevels))))
    parseUnits (AST.NonEmptyList.toList sources) [] []

let private packageHashType =
    AST.TSum ("Darklang.LanguageTools.ProgramTypes.Hash", [])

let private packageLocationType =
    AST.TRecord ("Darklang.LanguageTools.ProgramTypes.PackageLocation", [])

let private runtimeValueType =
    AST.TSum ("Darklang.LanguageTools.RuntimeTypes.ValueType", [])

let private optionType (innerType: AST.Type) =
    AST.TSum ("Stdlib.Option.Option", [innerType])

let private constructor
    (typeName: string)
    (caseName: string)
    (payload: AST.Expr option)
    : AST.Expr =
    AST.Constructor (AST.UnresolvedConstructor (Some typeName), caseName, payload)

let private packageHashExpr (hash: string) : AST.Expr =
    constructor
        "Darklang.LanguageTools.ProgramTypes.Hash"
        "Hash"
        (Some (AST.StringLiteral hash))

let private optionNoneExpr : AST.Expr =
    constructor "Stdlib.Option.Option" "None" None

let private optionSomeExpr (value: AST.Expr) : AST.Expr =
    constructor "Stdlib.Option.Option" "Some" (Some value)

let private call (name: string) (args: AST.Expr list) : AST.Expr =
    AST.Call (name, AST.NonEmptyList.fromList args)

let private addOrderedGroup
    (key: 'key)
    (value: 'value)
    (groups: ('key * 'value list) list)
    : ('key * 'value list) list
    when 'key: equality =
    let rec add remaining =
        match remaining with
        | [] -> [(key, [value])]
        | (existingKey, values) :: rest when existingKey = key ->
            (existingKey, values @ [value]) :: rest
        | group :: rest -> group :: add rest
    add groups

let private nestedIf
    (cases: (AST.Expr * AST.Expr) list)
    (fallback: AST.Expr)
    : AST.Expr =
    List.foldBack
        (fun (condition, result) remaining -> AST.If (condition, result, remaining))
        cases
        fallback

let private catalogFunction
    (name: string)
    (parameters: (string * AST.Type) list)
    (returnType: AST.Type)
    (body: AST.Expr)
    : AST.FunctionDef =
    {
        Name = name
        TypeParams = []
        Params = AST.NonEmptyList.fromList parameters
        ReturnType = returnType
        Body = body
        Recursion = None
    }

let private collectProgramSpecs (program: AST.Program) : Set<AST_to_ANF.SpecKey> =
    let (AST.Program topLevels) = program
    topLevels
    |> List.map (function
        | AST.FunctionDef func when List.isEmpty func.TypeParams ->
            AST_to_ANF.collectTypeAppsFromFunc func
        | AST.Expression expr -> AST_to_ANF.collectTypeApps expr
        | _ -> Set.empty)
    |> List.fold Set.union Set.empty

let private collectProgramCalls (program: AST.Program) : Set<string> =
    let (AST.Program topLevels) = program
    topLevels
    |> List.map (function
        | AST.FunctionDef func -> AST_to_ANF.collectCalledFunctions func.Body
        | AST.Expression expr -> AST_to_ANF.collectCalledFunctions expr
        | AST.TypeDef _ -> Set.empty)
    |> List.fold Set.union Set.empty

let private validateDistinctCatalogHashes
    (entries: PackageValueCatalogEntry list)
    : Result<unit, string> =
    let folder (state: Result<Set<string>, string>) entry =
        state
        |> Result.bind (fun hashes ->
            if Set.contains entry.ValueHash hashes then
                Error $"Package value catalog contains duplicate value hash '{entry.ValueHash}'"
            else
                Ok (Set.add entry.ValueHash hashes))
    entries
    |> List.fold folder (Ok Set.empty)
    |> Result.map (fun _ -> ())

let private materializePackageValueCatalog
    (baseContext: PipelineContext)
    (warningSettings: AST.WarningSettings)
    (catalog: PackageValueCatalog)
    (typedProgram: AST.Program)
    : Result<AST.Program, string> =
    let (PackageValueCatalog entries) = catalog
    validateDistinctCatalogHashes entries
    |> Result.bind (fun () ->
        let localGenericDefs = AST_to_ANF.extractGenericFuncDefs typedProgram
        let genericDefs =
            Map.fold
                (fun current name definition -> Map.add name definition current)
                baseContext.GenericFuncDefs
                localGenericDefs
        let specialization =
            typedProgram
            |> collectProgramSpecs
            |> AST_to_ANF.specializeFromSpecs genericDefs
        let requestedEvaluatorTypes =
            specialization.ExternalSpecs
            |> Set.toList
            |> List.choose (function
                | "Builtin.pmEvaluateValue", [resultType] -> Some resultType
                | _ -> None)
            |> Set.ofList
        let specializedCalls =
            specialization.SpecializedFuncs
            |> List.map (fun func -> AST_to_ANF.collectCalledFunctions func.Body)
            |> List.fold Set.union Set.empty
        let reachableCalls = Set.union (collectProgramCalls typedProgram) specializedCalls
        let needsFind = Set.contains "Builtin.pmFindValuesByValueType" reachableCalls
        let needsLocations = Set.contains "Builtin.pmGetLocationsByValue" reachableCalls
        let needsEvaluators = not (Set.isEmpty requestedEvaluatorTypes)

        if not needsFind && not needsLocations && not needsEvaluators then
            Ok typedProgram
        else
            let reachableEntries =
                entries
                |> List.filter (fun entry ->
                    Set.contains entry.Evaluator.ResultType requestedEvaluatorTypes)

            let findGroups =
                reachableEntries
                |> List.filter (fun entry -> List.isEmpty entry.RuntimeType.TypeArguments)
                |> List.fold
                    (fun groups entry ->
                        addOrderedGroup entry.RuntimeType entry.ValueHash groups)
                    []
            let findCases =
                findGroups
                |> List.map (fun (catalogType, hashes) ->
                    let condition =
                        call
                            "Darklang.LanguageTools.RuntimeTypes.isCustomTypeWithNoTypeArguments"
                            [AST.Var "valueType"; AST.StringLiteral catalogType.Hash]
                    let result = hashes |> List.map packageHashExpr |> AST.ListLiteral
                    (condition, result))
            let findFunction =
                catalogFunction
                    "Builtin.pmFindValuesByValueType"
                    [("valueType", runtimeValueType)]
                    (AST.TList packageHashType)
                    (nestedIf findCases (AST.ListLiteral []))

            let visibleLocations =
                reachableEntries
                |> List.collect (fun entry ->
                    entry.Locations
                    |> List.collect (fun location ->
                        location.VisibleInBranches
                        |> List.map (fun branchId ->
                            ((branchId, entry.ValueHash), location))))
            let locationGroups =
                visibleLocations
                |> List.fold
                    (fun groups (key, location) -> addOrderedGroup key location groups)
                    []
            let locationExpr (location: CatalogPackageLocation) =
                AST.RecordLiteral (
                    AST.unresolvedRecordReference "Darklang.LanguageTools.ProgramTypes.PackageLocation" [],
                    [
                        ("owner", AST.StringLiteral location.Owner)
                        ("modules", location.Modules |> List.map AST.StringLiteral |> AST.ListLiteral)
                        ("name", AST.StringLiteral location.Name)
                    ]
                )
            let locationCases =
                locationGroups
                |> List.map (fun ((branchId, valueHash), locations) ->
                    let branchMatches =
                        AST.BinOp (AST.Eq, AST.Var "branchId", AST.StringLiteral branchId)
                    let hashMatches =
                        AST.BinOp (AST.Eq, AST.Var "hashText", AST.StringLiteral valueHash)
                    let result = locations |> List.map locationExpr |> AST.ListLiteral
                    (AST.BinOp (AST.And, branchMatches, hashMatches), result))
            let locationsBody =
                AST.Let (
                    AST.LPVariable "hashText",
                    call "Darklang.LanguageTools.ProgramTypes.hashToString" [AST.Var "valueHash"],
                    nestedIf locationCases (AST.ListLiteral [])
                )
            let locationsFunction =
                catalogFunction
                    "Builtin.pmGetLocationsByValue"
                    [("branchId", AST.TString); ("valueHash", packageHashType)]
                    (AST.TList packageLocationType)
                    locationsBody

            let evaluatorFunction (resultType: AST.Type) =
                let name = AST_to_ANF.specName "Builtin.pmEvaluateValue" [resultType]
                let cases =
                    reachableEntries
                    |> List.choose (fun entry ->
                        if entry.Evaluator.ResultType <> resultType then
                            None
                        else
                            match entry.Evaluator.State with
                            | Available value ->
                                let condition =
                                    AST.BinOp (
                                        AST.Eq,
                                        AST.Var "hashText",
                                        AST.StringLiteral entry.ValueHash
                                    )
                                Some (condition, optionSomeExpr value)
                            | Unavailable
                            | EvaluationFailure -> None)
                let body =
                    AST.Let (
                        AST.LPVariable "hashText",
                        call "Darklang.LanguageTools.ProgramTypes.hashToString" [AST.Var "valueHash"],
                        nestedIf cases optionNoneExpr
                    )
                catalogFunction
                    name
                    [("valueHash", packageHashType)]
                    (optionType resultType)
                    body

            let generatedFunctions =
                (if needsFind then [findFunction] else [])
                @ (if needsLocations then [locationsFunction] else [])
                @ (requestedEvaluatorTypes |> Set.toList |> List.map evaluatorFunction)
            let syntheticProgram =
                AST.Program (
                    (generatedFunctions |> List.map AST.FunctionDef)
                )
            TypeChecking.checkDeclarationProgramWithBaseEnvAndSettings
                baseContext.TypeCheckEnv
                false
                warningSettings
                syntheticProgram
            |> Result.mapError (fun error ->
                $"Package value catalog validation failed: {TypeChecking.typeErrorToString error}")
            |> Result.map (fun (_, AST.Program generatedTopLevels, _) ->
                let (AST.Program userTopLevels) = typedProgram
                AST.Program (generatedTopLevels @ userTopLevels)))

/// Compile a user/test program against a prebuilt stdlib/preamble context
let private compileUserWithPlan (plan: UserCompilePlan) : CompileReport =
    let sw = Stopwatch.StartNew()
    let result =
        try
            // Pass 1: Parse user code only
            if plan.Verbosity >= 1 then println plan.Labels.Parse
            let parseResult =
                parseSourceProgram plan.AllowInternal plan.Sources
                |> Result.map snd
            let parseTime = sw.Elapsed.TotalMilliseconds
            recordPassTiming plan.PassTimingRecorder "Parse" parseTime
            if plan.Verbosity >= 2 then
                let t = System.Math.Round(parseTime, 1)
                println $"        {t}ms"

            match parseResult with
            | Error err -> Error $"Parse error: {err}"
            | Ok userAst ->
                // Pass 1.5: Type Checking (user code with base TypeCheckEnv)
                if plan.Verbosity >= 1 then println plan.Labels.TypeCheck
                let typeCheckResult =
                    checkProgramWithBaseEnv
                        plan.Options.Warnings
                        plan.BaseContext.TypeCheckEnv
                        userAst
                let typeCheckTime = sw.Elapsed.TotalMilliseconds - parseTime
                recordPassTiming plan.PassTimingRecorder "Type Checking" typeCheckTime
                if plan.Verbosity >= 2 then
                    let t = System.Math.Round(typeCheckTime, 1)
                    println $"        {t}ms"

                match typeCheckResult with
                | Error typeErr -> Error (TypeChecking.typeErrorToString typeErr)
                | Ok (programType, _, _) when
                    plan.Mode = FullProgram
                    && programType <> AST.TUnit
                    && programType <> AST.TInt64
                    && programType <> AST.TInt ->
                    Error
                        $"File entry expression must return Unit, Int, or Int64; got {TypeChecking.typeToString programType}"
                | Ok (programType, typedUserAst, userEnv) ->
                    let jsonPlanningStart = sw.Elapsed.TotalMilliseconds
                    let plannedUserAst =
                        JsonPlanning.rewriteProgramWithSession
                            (plan.Session |> Option.map (fun session -> session.JsonPlanning))
                            userEnv
                            typedUserAst
                    let jsonPlanningElapsed = sw.Elapsed.TotalMilliseconds - jsonPlanningStart
                    recordPassTiming plan.PassTimingRecorder "JSON Planning" jsonPlanningElapsed
                    let valueRenderingStart = Stopwatch.StartNew()
                    let plannedProgramType = TypeChecking.resolveType userEnv.AliasReg programType
                    let renderedUserAst, boundaryProgramType =
                        if plan.Mode = FullProgram then
                            (plannedUserAst, plannedProgramType)
                        else if plannedProgramType = AST.TUnit then
                            (plannedUserAst, AST.TUnit)
                        else
                            (ValueRendering.rewriteProgram
                                plan.BaseContext.Registries.RecordFieldsReg
                                userEnv.IndexedTypeReg
                                plan.BaseContext.Registries.VariantLookup
                                plan.BaseContext.Registries.FuncReg
                                plannedProgramType
                                plannedUserAst,
                             AST.TString)
                    valueRenderingStart.Stop()
                    recordPassTiming
                        plan.PassTimingRecorder
                        "Value Rendering"
                        valueRenderingStart.Elapsed.TotalMilliseconds
                    if plan.Verbosity >= 3 then
                        println $"Program type: {TypeChecking.typeToString programType}"
                        println ""

                    // Pass 2: AST → ANF (user only)
                    if plan.Verbosity >= 1 then println plan.Labels.Anf
                    let catalogStart = Stopwatch.StartNew()
                    let materializedProgramResult =
                        materializePackageValueCatalog
                            plan.BaseContext
                            plan.Options.Warnings
                            plan.PackageValues
                            renderedUserAst
                    catalogStart.Stop()
                    recordPassTiming
                        plan.PassTimingRecorder
                        "AST -> ANF Package Catalog"
                        catalogStart.Elapsed.TotalMilliseconds
                    let userOnlyResult =
                        materializedProgramResult
                        |> Result.bind (fun materializedProgram ->
                            convertTypedProgramToUserOnlyWithMode
                                plan.BaseContext
                                plan.Monomorphization
                                userEnv
                                plan.Session
                                plan.PassTimingRecorder
                                materializedProgram)
                    let anfTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime
                    recordPassTiming plan.PassTimingRecorder "AST -> ANF" anfTime
                    if plan.Verbosity >= 2 then
                        let t = System.Math.Round(anfTime, 1)
                        println $"        {t}ms"

                    match userOnlyResult with
                    | Error err -> Error $"ANF conversion error: {err}"
                    | Ok (userOnly, dependencyIdentity) ->
                        let functionsToCompile =
                            userOnly.UserFunctions
                            |> List.filter (fun f -> not (Set.contains f.Name plan.SkipFunctionNames))

                        let dependencyRoots =
                            functionsToCompile
                            |> List.choose (fun func ->
                                if func.Name.StartsWith("__dark_json_")
                                   || func.Name.StartsWith("__dark_eq_")
                                   || Set.contains func.Name userOnly.NonInlineableFunctionNames then
                                    Some func.Name
                                else
                                    None)
                            |> Set.ofList
                        let dependencyNames =
                            CallGraphReachability.findReachable
                                (ANFDeadCodeElimination.buildCallGraph functionsToCompile)
                                dependencyRoots
                        let dependencyFunctions, programFunctions =
                            functionsToCompile
                            |> List.partition (fun func -> Set.contains func.Name dependencyNames)

                        if plan.EmitFunctionEvents && plan.Verbosity >= 3 then
                            println $"  [COMPILE] {programFunctions.Length} program functions compiled fresh"
                            for f in functionsToCompile do
                                println $"    - {f.Name}"

                        let programEntryName = "__dark_compiler_program_entry"
                        let hasReservedName =
                            functionsToCompile
                            |> List.exists (fun func -> func.Name = programEntryName)
                        let userRegistries : AST_to_ANF.Registries = {
                            TypeReg = userOnly.TypeReg
                            RecordFieldsReg = userOnly.RecordFieldsReg
                            RecordTypeParamsReg = userOnly.RecordTypeParamsReg
                            VariantLookup = userOnly.VariantLookup
                            RcSumShapeReg = userOnly.RcSumShapeReg
                            FuncReg = userOnly.FuncReg
                            FuncParams = userOnly.FuncParams
                            ModuleRegistry = userOnly.ModuleRegistry
                            RecursiveMembers = userOnly.RecursiveMembers
                        }
                        let externalReturnTypes =
                            mergeReturnTypes plan.BaseContext.ReturnTypes userOnly.LocalReturnTypes
                        let releasePlanSummaryCache =
                            plan.Session
                            |> Option.map (fun current ->
                                fun includeStaticRootDependencies releasePlanCacheKey releasePlan generate ->
                                    current.Arm64ReleasePlanSummary
                                        includeStaticRootDependencies
                                        releasePlanCacheKey
                                        releasePlan
                                        generate)
                        let mirRegistryTimer = Stopwatch.StartNew()
                        let projectedMirRegistries =
                            match plan.Session with
                            | Some current ->
                                current.ProjectMirRegistries
                                    dependencyIdentity
                                    userRegistries.VariantLookup
                                    userRegistries.RecordFieldsReg
                            | None ->
                                (ANF_to_MIR.buildVariantRegistry userRegistries.VariantLookup,
                                 ANF_to_MIR.buildRecordRegistry userRegistries.RecordFieldsReg)
                        mirRegistryTimer.Stop()
                        recordPassTiming
                            plan.PassTimingRecorder
                            "MIR Registry Projection Preparation"
                            mirRegistryTimer.Elapsed.TotalMilliseconds

                        let compileDependencyFunctions () =
                            buildAnf
                                plan.Verbosity
                                plan.Options
                                sw
                                userRegistries
                                plan.ExternalInlineCandidates
                                userOnly.NonInlineableFunctionNames
                                dependencyFunctions
                                true
                                plan.PassTimingRecorder
                            |> Result.bind (fun (anfDependencies, dependencyTypeMap) ->
                                let tcoDependencies =
                                    applyTco
                                        plan.Verbosity
                                        plan.Options
                                        sw
                                        userRegistries.RecursiveMembers
                                        anfDependencies
                                        plan.PassTimingRecorder
                                lowerToAllocatedLir
                                    plan.BaseContext.Target
                                    plan.Verbosity
                                    plan.Options
                                    sw
                                    plan.PassTimingRecorder
                                    releasePlanSummaryCache
                                    plan.Labels.StageSuffix
                                    tcoDependencies
                                    dependencyTypeMap
                                    userRegistries
                                    (Some projectedMirRegistries)
                                    externalReturnTypes)

                        let dependencyLirResult =
                            if List.isEmpty dependencyFunctions then
                                Ok []
                            else
                                match plan.Session with
                                | Some current ->
                                    current.CompileDependencies
                                        dependencyIdentity
                                        {
                                            Target = plan.BaseContext.Target
                                            Options = plan.Options
                                            NonInlineableFunctionNames = userOnly.NonInlineableFunctionNames
                                        }
                                        compileDependencyFunctions
                                | None ->
                                    compileDependencyFunctions ()

                        let programEntry =
                            AST_to_ANF.synthesizeEntryFunction
                                programEntryName
                                boundaryProgramType
                                userOnly.MainExpr
                        let programAnfResult =
                            if hasReservedName then
                                Error $"Function name '{programEntryName}' is reserved"
                            else
                                buildAnf
                                    plan.Verbosity
                                    plan.Options
                                    sw
                                    userRegistries
                                    plan.ExternalInlineCandidates
                                    userOnly.NonInlineableFunctionNames
                                    (programEntry :: programFunctions)
                                    true
                                    plan.PassTimingRecorder
                        match dependencyLirResult, programAnfResult with
                        | Error err, _
                        | _, Error err -> Error err
                        | Ok allocatedDependencyFuncs, Ok (programAnfFunctions, programTypeMap) ->
                            if plan.Verbosity >= 1 then println "  [2.6/7] Print Insertion..."
                            let printStart = sw.Elapsed.TotalMilliseconds
                            let printResult =
                                match plan.Mode with
                                | FullProgram -> Ok programAnfFunctions
                                | TestExpression ->
                                    PrintInsertion.insertPrintInEntry
                                        programEntryName
                                        boundaryProgramType
                                        programAnfFunctions
                            match printResult with
                            | Error err -> Error $"Print insertion error: {err}"
                            | Ok printedFunctions ->
                                let printElapsed = sw.Elapsed.TotalMilliseconds - printStart
                                recordPassTiming plan.PassTimingRecorder "Print Insertion" printElapsed
                                if plan.Verbosity >= 2 then
                                    let t = System.Math.Round(printElapsed, 1)
                                    println $"        {t}ms"
                                if shouldDumpIR plan.Verbosity plan.Options.DumpANF then
                                    let printProgram = ANF.Program (printedFunctions, ANF.Return ANF.UnitLiteral)
                                    printANFProgram "=== ANF (after Print insertion) ===" printProgram

                                let tcoProgramFunctions =
                                    applyTco
                                        plan.Verbosity
                                        plan.Options
                                        sw
                                        userRegistries.RecursiveMembers
                                        printedFunctions
                                        plan.PassTimingRecorder
                                let programLirResult =
                                    lowerToAllocatedLir
                                        plan.BaseContext.Target
                                        plan.Verbosity
                                        plan.Options
                                        sw
                                        plan.PassTimingRecorder
                                        releasePlanSummaryCache
                                        plan.Labels.StageSuffix
                                        tcoProgramFunctions
                                        programTypeMap
                                        userRegistries
                                        (Some projectedMirRegistries)
                                        externalReturnTypes
                                let startResultId = ANF.TempId 0
                                let startFunction =
                                    AST_to_ANF.synthesizeEntryFunction
                                        "_start"
                                        boundaryProgramType
                                        (ANF.Let (
                                            startResultId,
                                            ANF.Call (programEntryName, []),
                                            ANF.Return (ANF.Var startResultId)))
                                let startRegistries = {
                                    userRegistries with
                                        FuncReg =
                                            Map.add
                                                programEntryName
                                                (AST.TFunction ([], boundaryProgramType))
                                                userRegistries.FuncReg
                                        FuncParams =
                                            Map.add programEntryName [] userRegistries.FuncParams
                                }
                                let compileStart () =
                                    buildAnf
                                        plan.Verbosity
                                        plan.Options
                                        sw
                                        startRegistries
                                        Map.empty
                                        Set.empty
                                        [startFunction]
                                        false
                                        plan.PassTimingRecorder
                                    |> Result.bind (fun (startAnf, startTypeMap) ->
                                        let tcoStart =
                                            applyTco
                                                plan.Verbosity
                                                plan.Options
                                                sw
                                                startRegistries.RecursiveMembers
                                                startAnf
                                                plan.PassTimingRecorder
                                        lowerToAllocatedLir
                                            plan.BaseContext.Target
                                            plan.Verbosity
                                            plan.Options
                                            sw
                                            plan.PassTimingRecorder
                                            releasePlanSummaryCache
                                            plan.Labels.StageSuffix
                                            tcoStart
                                            startTypeMap
                                            startRegistries
                                            (Some projectedMirRegistries)
                                            (Map.add programEntryName boundaryProgramType externalReturnTypes))
                                let startLirResult =
                                    match plan.Session with
                                    | Some current ->
                                        current.CompileStart
                                            {
                                                Target = plan.BaseContext.Target
                                                Options = plan.Options
                                                BoundaryProgramType = boundaryProgramType
                                            }
                                            compileStart
                                    | None ->
                                        compileStart ()
                                match programLirResult, startLirResult with
                                | Error err, _
                                | _, Error err -> Error err
                                | Ok allocatedProgramFuncs, Ok allocatedStartFuncs ->
                                    let allocatedUserFuncs =
                                        allocatedStartFuncs
                                        @ allocatedProgramFuncs
                                        @ allocatedDependencyFuncs
                                    let allSymbolicUserFuncs = plan.PrebuiltSymbolicFunctions @ allocatedUserFuncs
                                    let finalUserFuncs =
                                        if plan.TreeShakeUserFunctions then
                                            if plan.Verbosity >= 1 then println "  [5.5/7] Function Tree Shaking..."
                                            let treeShakeStart = sw.Elapsed.TotalMilliseconds
                                            let shakenUserFuncs =
                                                if plan.Options.DisableFunctionTreeShaking then
                                                    allSymbolicUserFuncs
                                                else
                                                    FunctionTreeShaking.filterUserFunctions (Some "_start") allSymbolicUserFuncs
                                            let treeShakeElapsed = sw.Elapsed.TotalMilliseconds - treeShakeStart
                                            recordPassTiming plan.PassTimingRecorder "Function Tree Shaking" treeShakeElapsed
                                            shakenUserFuncs
                                        else
                                            allSymbolicUserFuncs

                                    if plan.EmitFunctionEvents && plan.Verbosity >= 3 then
                                        println $"  [COMBINED] fresh: {allocatedUserFuncs.Length}, total: {allSymbolicUserFuncs.Length}"
                                        for f in allSymbolicUserFuncs do
                                            println $"    - {f.Name}"
                                        println $"  [TreeShaking] user funcs: {finalUserFuncs.Length}"

                                    // Filter stdlib functions to only include reachable ones (dead code elimination)
                                    let reachableStdlib =
                                        if plan.Options.DisableFunctionTreeShaking then plan.Stdlib.AllocatedFunctions
                                        else
                                            let treeShakeStart = sw.Elapsed.TotalMilliseconds
                                            let filtered =
                                                match plan.Session with
                                                | Some current ->
                                                    current.ReachableStdlibFunctions
                                                        (box plan.Stdlib)
                                                        finalUserFuncs
                                                        plan.Stdlib.StdlibCallGraph
                                                        plan.Stdlib.AllocatedFunctions
                                                | None ->
                                                    FunctionTreeShaking.filterStdlibFunctions
                                                        plan.Stdlib.StdlibCallGraph
                                                        finalUserFuncs
                                                        plan.Stdlib.AllocatedFunctions
                                            filtered
                                            |> fun shakenStdlib ->
                                                let treeShakeElapsed = sw.Elapsed.TotalMilliseconds - treeShakeStart
                                                recordPassTiming plan.PassTimingRecorder "Function Tree Shaking" treeShakeElapsed
                                                shakenStdlib

                                    // Combine reachable stdlib functions with user functions
                                    let allFuncs =
                                        reachableStdlib @ finalUserFuncs
                                    let reachableDependencyFuncs, reachableProgramFuncs =
                                        finalUserFuncs
                                        |> List.partition (fun func ->
                                            Set.contains func.Name dependencyNames)
                                    let lirVariantRegistry : LIR.VariantRegistry =
                                        let combinedVariantLookup =
                                            Map.fold
                                                (fun acc variantName variantInfo -> Map.add variantName variantInfo acc)
                                                plan.Stdlib.Context.Registries.VariantLookup
                                                userRegistries.VariantLookup
                                        combinedVariantLookup
                                        |> Map.toList
                                        |> List.choose (fun (lookupName, info) ->
                                            let (typeName, _, _, _) = info
                                            let prefix = $"{typeName}."
                                            if lookupName.StartsWith(prefix) then
                                                Some (lookupName.Substring(prefix.Length), info)
                                            else
                                                None)
                                        |> List.groupBy (fun (_, (typeName, _, _, _)) -> typeName)
                                        |> List.map (fun (typeName, variants) ->
                                            let typeParams =
                                                variants
                                                |> List.tryHead
                                                |> Option.map (fun (_, (_, typeParams, _, _)) -> typeParams)
                                                |> Option.defaultValue []
                                            let lirVariants =
                                                variants
                                                |> List.map (fun (variantName, (_, _, tag, payload)) ->
                                                    ({ Name = variantName
                                                       Tag = tag
                                                       Payload = payload } : LIR.VariantInfo))
                                                |> List.sortBy (fun variant -> variant.Tag)
                                            (typeName, { LIR.TypeParams = typeParams; LIR.Variants = lirVariants }))
                                        |> Map.ofList
                                    let allocatedProgram =
                                        LIR.Program (
                                            allFuncs,
                                            lirVariantRegistry,
                                            userRegistries.RecordFieldsReg
                                        )
                                    let freshProgramContextIdentity = box allocatedProgramFuncs
                                    let startProgramFuncs, otherProgramFuncs =
                                        reachableProgramFuncs
                                        |> List.partition (fun func -> func.Name = "_start")
                                    let functionGroups : CodeGen.FunctionGroup list =
                                        [
                                            {
                                                ContextIdentity = freshProgramContextIdentity
                                                ReusableAcrossCompilations = false
                                                Functions = startProgramFuncs
                                            }
                                            {
                                                ContextIdentity = box reachableStdlib
                                                ReusableAcrossCompilations = true
                                                Functions = reachableStdlib
                                            }
                                            {
                                                ContextIdentity = freshProgramContextIdentity
                                                ReusableAcrossCompilations = false
                                                Functions = otherProgramFuncs
                                            }
                                            {
                                                ContextIdentity = dependencyIdentity
                                                ReusableAcrossCompilations = false
                                                Functions = reachableDependencyFuncs
                                            }
                                        ]
                                        |> List.filter (fun group -> not (List.isEmpty group.Functions))
                                    if shouldDumpIR plan.Verbosity plan.Options.DumpLIR then
                                        printLIRProgram "=== LIR (After Register Allocation) ===" allocatedProgram

                                    let binaryResult =
                                        generateBinary
                                            plan.BaseContext.Target
                                            plan.Verbosity
                                            plan.Options
                                            sw
                                            plan.PassTimingRecorder
                                            "  [6/7] Code Generation..."
                                            "  [7/7] ARM64 Emit ({format})..."
                                            false
                                            false
                                            plan.Session
                                            dependencyIdentity
                                            functionGroups
                                            ([
                                                {
                                                    CodeGen.ContextIdentity = box plan.BaseContext
                                                    Functions = reachableStdlib
                                                }
                                                {
                                                    CodeGen.ContextIdentity = dependencyIdentity
                                                    Functions = reachableProgramFuncs
                                                }
                                                {
                                                    CodeGen.ContextIdentity = dependencyIdentity
                                                    Functions = reachableDependencyFuncs
                                                }
                                             ]
                                             |> List.filter (fun group -> not (List.isEmpty group.Functions)))
                                            allocatedProgram
                                    match binaryResult with
                                    | Error err -> Error err
                                    | Ok binary ->
                                        Ok binary
        with
        | ex ->
            Error $"Compilation failed: {ex.Message}"
    sw.Stop()
    match result with
    | Ok _ when plan.Verbosity >= 1 ->
        println $"  ✓ Compilation complete ({System.Math.Round(sw.Elapsed.TotalMilliseconds, 1)}ms)"
    | _ -> ()
    { Target = plan.BaseContext.Target; Result = result; CompileTime = sw.Elapsed }

/// Build preamble with stdlib as base, returning extended context for test compilation
/// Preamble functions go through the full pipeline (parse → typecheck → mono → inline → lift → ANF → RC → TCO)
/// The result is built once per file and reused for all tests in that file
let buildPreambleContext
    (allowInternal: bool)
    (stdlib: StdlibResult)
    (preamble: string)
    (sourceFile: string)
    (_funcLineMap: Map<string, int>)
    (passTimingRecorder: PassTimingRecorder option)
    : Result<StdlibResult * PreambleContext, string> =
    // Handle empty preamble - return a context that just wraps stdlib
    if String.IsNullOrWhiteSpace(preamble) then
        let emptyContext = {
            Context = stdlib.Context
            ANFFunctions = []
            TypeMap = stdlib.StdlibTypeMap
            SymbolicFunctions = []
        }
        Ok (stdlib, emptyContext)
    else
    match InterpreterParser.parseString allowInternal preamble with
        | Error err ->
            let msg = $"Preamble parse error: {err}"
            Error msg
        | Ok preambleAst ->
            // Type-check preamble with stdlib context
            match TypeChecking.checkDeclarationProgramWithBaseEnv stdlib.Context.TypeCheckEnv preambleAst with
            | Error typeErr ->
                let msg = $"Preamble type error: {TypeChecking.typeErrorToString typeErr}"
                Error msg
            | Ok (_programType, typedPreambleAst, preambleTypeCheckEnv) ->
                // Extract generic function definitions from preamble
                let preambleGenericDefs = AST_to_ANF.extractGenericFuncDefs typedPreambleAst
                // Merge stdlib generics with preamble generics
                let mergedGenericDefs = Map.fold (fun acc k v -> Map.add k v acc) stdlib.Context.GenericFuncDefs preambleGenericDefs

                // Convert preamble to ANF (mono → inline → lift → ANF)
                match
                    convertTypedDeclarations
                        (Some stdlib.Context)
                        (Monomorphize (Some stdlib.Context.GenericFuncDefs))
                        typedPreambleAst
                with
                | Error err ->
                    let msg = $"Preamble ANF conversion error: {err}"
                    Error msg
                | Ok preambleUserOnly ->
                    let preambleRegistries = preambleUserOnly.Registries
                    let preambleOptions = defaultOptions
                    let sw = Stopwatch.StartNew()
                    let preambleReturnTypes =
                        mergeReturnTypes stdlib.Context.ReturnTypes preambleUserOnly.LocalReturnTypes
                    let pipelineContext =
                        buildContext stdlib.Context.Target preambleTypeCheckEnv mergedGenericDefs Map.empty preambleRegistries preambleReturnTypes
                    match buildAnf 0 preambleOptions sw preambleRegistries Map.empty Set.empty preambleUserOnly.Functions false passTimingRecorder with
                    | Error err ->
                        let rcPrefix = "Reference count insertion error: "
                        let msg =
                            if err.StartsWith(rcPrefix) then
                                let suffix = err.Substring(rcPrefix.Length)
                                $"Preamble RC insertion error: {suffix}"
                            else
                                $"Preamble {err}"
                        Error msg
                    | Ok (preambleFunctions, typeMap) ->
                        let tcoFunctions = applyTco 0 preambleOptions sw preambleRegistries.RecursiveMembers preambleFunctions passTimingRecorder
                        let preambleExternalReturnTypes = preambleReturnTypes
                        match lowerToAllocatedLir
                            stdlib.Context.Target
                            0
                            preambleOptions
                            sw
                            passTimingRecorder
                            None
                            "preamble"
                            tcoFunctions
                            typeMap
                            preambleRegistries
                            None
                            preambleExternalReturnTypes with
                        | Error err ->
                            let msg = $"Preamble {err}"
                            Error msg
                        | Ok allocatedFuncs ->
                            let stdlibFuncNames =
                                stdlib.AllocatedFunctions
                                |> List.map (fun func -> func.Name)
                                |> Set.ofList
                            let isStdlibFunction (name: string) : bool =
                                Set.contains name stdlibFuncNames
                            let preambleOnlyFuncs =
                                allocatedFuncs
                                |> List.filter (fun func -> not (isStdlibFunction func.Name))
                            let preambleSymbolicFuncs = preambleOnlyFuncs
                            let preambleLiftedFuncNames =
                                tcoFunctions
                                |> List.map (fun func -> func.Name)
                                |> Set.ofList
                            let pipelineContextWithLiftedNames = {
                                pipelineContext with
                                    BaseFuncNames =
                                        Set.union pipelineContext.BaseFuncNames preambleLiftedFuncNames
                            }

                            // Merge TypeMaps (stdlib + preamble)
                            let mergedTypeMap = Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibTypeMap typeMap

                            let context = {
                                Context = pipelineContextWithLiftedNames
                                ANFFunctions = tcoFunctions
                                TypeMap = mergedTypeMap
                                SymbolicFunctions = preambleSymbolicFuncs
                            }
                            Ok (stdlib, context)

/// Build preamble context from a typed preamble analysis and precomputed specializations
let buildPreambleContextFromAnalysis
    (stdlib: StdlibResult)
    (analysis: PreambleAnalysis)
    (specialization: AST_to_ANF.SpecializationResult)
    (sourceFile: string)
    (_funcLineMap: Map<string, int>)
    (passTimingRecorder: PassTimingRecorder option)
    : Result<StdlibResult * PreambleContext, string> =
    let combinedSpecRegistry = mergeSpecRegistries stdlib.Context.SpecRegistry specialization.SpecRegistry

    let mergedGenericDefs =
        Map.fold (fun acc k v -> Map.add k v acc) stdlib.Context.GenericFuncDefs analysis.GenericFuncDefs

    let (AST.Program items) = analysis.TypedAST
    let specializedTopLevels = specialization.SpecializedFuncs |> List.map AST.FunctionDef
    let specializedAndOriginalTopLevels = specializedTopLevels @ items
    let materializedTopLevels =
        TypeChecking.materializeEqHelpersInTopLevels
            analysis.TypeCheckEnv.AliasReg
            analysis.TypeCheckEnv.IndexedTypeReg
            analysis.TypeCheckEnv.VariantLookup
            specializedAndOriginalTopLevels
    let programWithSpecializations = AST.Program materializedTopLevels

    convertTypedDeclarations
        (Some stdlib.Context)
        (ReplaceTypeApps combinedSpecRegistry)
        programWithSpecializations
    |> Result.bind (fun preambleUserOnly ->
        let preambleRegistries = preambleUserOnly.Registries
        let preambleOptions = defaultOptions
        let sw = Stopwatch.StartNew()
        let preambleReturnTypes =
            mergeReturnTypes stdlib.Context.ReturnTypes preambleUserOnly.LocalReturnTypes
        let pipelineContext =
            buildContext stdlib.Context.Target analysis.TypeCheckEnv mergedGenericDefs combinedSpecRegistry preambleRegistries preambleReturnTypes
        match buildAnf 0 preambleOptions sw preambleRegistries Map.empty Set.empty preambleUserOnly.Functions false passTimingRecorder with
        | Error err ->
            let rcPrefix = "Reference count insertion error: "
            let msg =
                if err.StartsWith(rcPrefix) then
                    let suffix = err.Substring(rcPrefix.Length)
                    $"Preamble RC insertion error: {suffix}"
                else
                    $"Preamble {err}"
            Error msg
        | Ok (preambleFunctions, typeMap) ->
            let tcoFunctions = applyTco 0 preambleOptions sw preambleRegistries.RecursiveMembers preambleFunctions passTimingRecorder
            let preambleExternalReturnTypes = preambleReturnTypes
            match lowerToAllocatedLir
                stdlib.Context.Target
                0
                preambleOptions
                sw
                passTimingRecorder
                None
                "preamble"
                tcoFunctions
                typeMap
                preambleRegistries
                None
                preambleExternalReturnTypes with
            | Error err ->
                let msg = $"Preamble {err}"
                Error msg
            | Ok allocatedFuncs ->
                let stdlibFuncNames =
                    stdlib.AllocatedFunctions
                    |> List.map (fun func -> func.Name)
                    |> Set.ofList
                let isStdlibFunction (name: string) : bool =
                    Set.contains name stdlibFuncNames
                let preambleOnlyFuncs =
                    allocatedFuncs
                    |> List.filter (fun func -> not (isStdlibFunction func.Name))
                let preambleSymbolicFuncs = preambleOnlyFuncs

                let mergedTypeMap = Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibTypeMap typeMap

                Ok (stdlib, {
                    Context = pipelineContext
                    ANFFunctions = tcoFunctions
                    TypeMap = mergedTypeMap
                    SymbolicFunctions = preambleSymbolicFuncs
                }))

let private labelsForMode (mode: CompileMode) : UserCompileLabels =
    match mode with
    | FullProgram ->
        {
            Parse = "  [1/7] Parse..."
            TypeCheck = "  [1.5/7] Type Checking (with stdlib env)..."
            Anf = "  [2/7] AST → ANF (user only)..."
            StageSuffix = "user only"
        }
    | TestExpression ->
        {
            Parse = "  [1/7] Parse (test expr only)..."
            TypeCheck = "  [1.5/7] Type Checking (with preamble env)..."
            Anf = "  [2/7] AST → ANF (test expr only)..."
            StageSuffix = ""
        }

let private buildCompilePlan (request: CompileRequest) : UserCompilePlan =
    let (stdlib, baseContext, prebuiltSymbolic, skipNames) =
        match request.Context with
        | StdlibOnly stdlib ->
            stdlib, stdlib.Context, [], Set.empty
        | StdlibWithPreamble (stdlib, preambleCtx) ->
            let preambleFuncs = preambleCtx.SymbolicFunctions
            let preambleFuncNameSet =
                preambleFuncs |> List.map (fun f -> f.Name) |> Set.ofList
            stdlib, preambleCtx.Context, preambleFuncs, preambleFuncNameSet

    let emitFunctionEvents, treeShakeUserFunctions =
        match request.Mode with
        | FullProgram -> false, false
        | TestExpression -> true, true

    let monomorphization =
        match request.Mode with
        | FullProgram -> Monomorphize (Some baseContext.GenericFuncDefs)
        | TestExpression -> SpecializeLocalAndReplace baseContext.SpecRegistry

    {
        AllowInternal = request.AllowInternal
        Mode = request.Mode
        Verbosity = request.Verbosity
        Options = request.Options
        PackageValues = request.PackageValues
        PassTimingRecorder = request.PassTimingRecorder
        Session = request.Session
        Stdlib = stdlib
        BaseContext = baseContext
        Monomorphization = monomorphization
        ExternalInlineCandidates = stdlib.StdlibInlineCandidates
        PrebuiltSymbolicFunctions = prebuiltSymbolic
        SkipFunctionNames = skipNames
        EmitFunctionEvents = emitFunctionEvents
        TreeShakeUserFunctions = treeShakeUserFunctions
        Labels = labelsForMode request.Mode
        Sources = request.Sources
    }

/// Compile source code to binary (in-memory, no file I/O)
let compile (request: CompileRequest) : CompileReport =
    let plan = buildCompilePlan request
    compileUserWithPlan plan

/// Execute a compiled binary with positional arguments and finite stdin while
/// capturing both output streams.
let executeCapturedWithArguments
    (target: Platform.Target)
    (verbosity: int)
    (arguments: string list)
    (input: ExecutionInput)
    (binary: byte array)
    : ExecutionOutput =
    let sw = Stopwatch.StartNew()
    let finish (exitCode: int) (stdout: string) (stderr: string) : ExecutionOutput =
        sw.Stop()
        { ExitCode = exitCode
          Stdout = stdout
          Stderr = stderr
          RuntimeTime = sw.Elapsed }

    if verbosity >= 1 then println ""
    if verbosity >= 1 then println "  Execution:"

    // Write binary to temp file
    if verbosity >= 1 then println "    • Writing binary to temp file..."
    let tempPath = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))

    // Write and flush to disk to minimize (but not eliminate) "Text file busy" race
    do
        use stream = new IO.FileStream(tempPath, IO.FileMode.Create, IO.FileAccess.Write, IO.FileShare.None)
        stream.Write(binary, 0, binary.Length)
        stream.Flush(true)  // Flush both stream and OS buffers to disk

    let writeTime = sw.Elapsed.TotalMilliseconds
    if verbosity >= 2 then println $"      {System.Math.Round(writeTime, 1)}ms"

    let result =
        try
            // Make executable using Unix file mode
            if verbosity >= 1 then println "    • Setting executable permissions..."
            let permissions = File.GetUnixFileMode(tempPath)
            File.SetUnixFileMode(tempPath, permissions ||| IO.UnixFileMode.UserExecute)
            let chmodTime = sw.Elapsed.TotalMilliseconds - writeTime
            if verbosity >= 2 then println $"      {System.Math.Round(chmodTime, 1)}ms"

            // Code sign with adhoc signature (required for macOS only)
            let codesignResult =
                if Platform.requiresCodeSigning (Platform.osFor target) then
                    if verbosity >= 1 then println "    • Code signing (adhoc)..."
                    let codesignStart = sw.Elapsed.TotalMilliseconds
                    let codesignInfo = ProcessStartInfo("codesign")
                    codesignInfo.Arguments <- $"-s - \"{tempPath}\""
                    codesignInfo.UseShellExecute <- false
                    codesignInfo.RedirectStandardOutput <- true
                    codesignInfo.RedirectStandardError <- true
                    let codesignProc = Process.Start(codesignInfo)
                    codesignProc.WaitForExit()

                    if codesignProc.ExitCode <> 0 then
                        let stderr = codesignProc.StandardError.ReadToEnd()
                        Some $"Code signing failed: {stderr}"
                    else
                        let codesignTime = sw.Elapsed.TotalMilliseconds - codesignStart
                        if verbosity >= 2 then println $"      {System.Math.Round(codesignTime, 1)}ms"
                        None
                else
                    if verbosity >= 1 then println "    • Code signing skipped (not required on Linux)"
                    None

            match codesignResult with
            | Some errorMsg ->
                // Code signing or platform detection failed - return error
                finish -1 "" errorMsg
            | None ->
                // Execute (with retry for "Text file busy" race condition)
                // Even with flush, kernel may not have fully synced file/permissions in fast test runs
                if verbosity >= 1 then println "    • Running binary..."
                let execStart = sw.Elapsed.TotalMilliseconds
                let execInfo = ProcessStartInfo(tempPath)
                execInfo.RedirectStandardOutput <- true
                execInfo.RedirectStandardError <- true
                execInfo.RedirectStandardInput <- true
                execInfo.UseShellExecute <- false
                arguments |> List.iter execInfo.ArgumentList.Add

                // Retry up to 3 times with small delay if we get "Text file busy"
                let rec startWithRetry attempts =
                    match tryStartProcess execInfo with
                    | Ok proc -> Ok proc
                    | Error msg when msg.Contains("Text file busy") && attempts > 0 ->
                        Threading.Thread.Sleep(10)  // Wait 10ms before retry
                        startWithRetry (attempts - 1)
                    | Error msg -> Error msg

                match startWithRetry 3 with
                | Error msg ->
                    finish -1 "" $"Failed to start process: {msg}"
                | Ok execProc ->
                    use proc = execProc
                    match input with
                    | Closed -> proc.StandardInput.Close()
                    | Bytes bytes ->
                        proc.StandardInput.BaseStream.Write(bytes, 0, bytes.Length)
                        proc.StandardInput.Close()
                    // Start async reads immediately to avoid blocking
                    let stdoutTask = proc.StandardOutput.ReadToEndAsync()
                    let stderrTask = proc.StandardError.ReadToEndAsync()

                    // Wait for process to complete
                    proc.WaitForExit()

                    // Now wait for output to be fully read
                    let stdout = stdoutTask.Result
                    let stderr = stderrTask.Result

                    let execTime = sw.Elapsed.TotalMilliseconds - execStart
                    if verbosity >= 2 then println $"      {System.Math.Round(execTime, 1)}ms"

                    if verbosity >= 1 then
                        println $"  ✓ Execution complete ({System.Math.Round(sw.Elapsed.TotalMilliseconds, 1)}ms)"

                    finish proc.ExitCode stdout stderr
        finally
            // Cleanup - ignore deletion errors
            tryDeleteFile tempPath
    result

/// Execute a compiled binary with finite stdin while capturing both output streams.
let executeCaptured
    (target: Platform.Target)
    (verbosity: int)
    (input: ExecutionInput)
    (binary: byte array)
    : ExecutionOutput =
    executeCapturedWithArguments target verbosity [] input binary

/// Backward-compatible captured execution with an already-closed stdin stream.
let execute (target: Platform.Target) (verbosity: int) (binary: byte array) : ExecutionOutput =
    executeCaptured target verbosity Closed binary

/// Execute a compiled binary with stdin/stdout/stderr inherited from this process.
/// This is the interactive run path: presentation bytes are visible immediately
/// and the OS remains responsible for terminal and signal behavior.
let executeAttached
    (target: Platform.Target)
    (verbosity: int)
    (binary: byte array)
    : ExecutionOutput =
    let sw = Stopwatch.StartNew()
    let finish (exitCode: int) (stderr: string) : ExecutionOutput =
        sw.Stop()
        { ExitCode = exitCode
          Stdout = ""
          Stderr = stderr
          RuntimeTime = sw.Elapsed }

    if verbosity >= 1 then println ""
    if verbosity >= 1 then println "  Execution:"
    if verbosity >= 1 then println "    • Writing binary to temp file..."

    let tempPath = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    do
        use stream = new IO.FileStream(tempPath, IO.FileMode.Create, IO.FileAccess.Write, IO.FileShare.None)
        stream.Write(binary, 0, binary.Length)
        stream.Flush(true)

    let writeTime = sw.Elapsed.TotalMilliseconds
    if verbosity >= 2 then println $"      {System.Math.Round(writeTime, 1)}ms"

    let result =
        try
            if verbosity >= 1 then println "    • Setting executable permissions..."
            let permissions = File.GetUnixFileMode(tempPath)
            File.SetUnixFileMode(tempPath, permissions ||| IO.UnixFileMode.UserExecute)
            let chmodTime = sw.Elapsed.TotalMilliseconds - writeTime
            if verbosity >= 2 then println $"      {System.Math.Round(chmodTime, 1)}ms"

            let codesignResult =
                if Platform.requiresCodeSigning (Platform.osFor target) then
                    if verbosity >= 1 then println "    • Code signing (adhoc)..."
                    let codesignStart = sw.Elapsed.TotalMilliseconds
                    let codesignInfo = ProcessStartInfo("codesign")
                    codesignInfo.Arguments <- $"-s - \"{tempPath}\""
                    codesignInfo.UseShellExecute <- false
                    codesignInfo.RedirectStandardOutput <- true
                    codesignInfo.RedirectStandardError <- true
                    let codesignProc = Process.Start(codesignInfo)
                    codesignProc.WaitForExit()
                    if codesignProc.ExitCode <> 0 then
                        Some $"Code signing failed: {codesignProc.StandardError.ReadToEnd()}"
                    else
                        let codesignTime = sw.Elapsed.TotalMilliseconds - codesignStart
                        if verbosity >= 2 then println $"      {System.Math.Round(codesignTime, 1)}ms"
                        None
                else
                    if verbosity >= 1 then println "    • Code signing skipped (not required on Linux)"
                    None

            match codesignResult with
            | Some errorMsg -> finish -1 errorMsg
            | None ->
                if verbosity >= 1 then println "    • Running binary..."
                let execStart = sw.Elapsed.TotalMilliseconds
                let execInfo = ProcessStartInfo(tempPath)
                execInfo.UseShellExecute <- false

                let rec startWithRetry attempts =
                    match tryStartProcess execInfo with
                    | Ok proc -> Ok proc
                    | Error msg when msg.Contains("Text file busy") && attempts > 0 ->
                        Threading.Thread.Sleep(10)
                        startWithRetry (attempts - 1)
                    | Error msg -> Error msg

                match startWithRetry 3 with
                | Error msg -> finish -1 $"Failed to start process: {msg}"
                | Ok execProc ->
                    use proc = execProc
                    proc.WaitForExit()
                    let execTime = sw.Elapsed.TotalMilliseconds - execStart
                    if verbosity >= 2 then println $"      {System.Math.Round(execTime, 1)}ms"
                    if verbosity >= 1 then
                        println $"  ✓ Execution complete ({System.Math.Round(sw.Elapsed.TotalMilliseconds, 1)}ms)"
                    finish proc.ExitCode ""
        finally
            tryDeleteFile tempPath
    result

/// Get all stdlib function names from the prebuilt stdlib
let getAllStdlibFunctionNamesFromStdlib (stdlib: StdlibResult) : Set<string> =
    stdlib.StdlibANFFunctions |> Map.keys |> Set.ofSeq

/// Get the set of stdlib function names reachable from user code (using prebuilt stdlib)
/// Used for coverage analysis without re-compiling stdlib
let getReachableStdlibFunctionsFromStdlib (stdlib: StdlibResult) (source: string) : Result<Set<string>, string> =
    // Parse user code
    match InterpreterParser.parseString false source with
    | Error err -> Error $"Parse error: {err}"
    | Ok userAst ->
        // Type check with stdlib environment
        match TypeChecking.checkPublicProgramWithBaseEnvAndSettings
            stdlib.Context.TypeCheckEnv
            false
            defaultWarningSettings
            userAst with
        | Error typeErr -> Error (TypeChecking.typeErrorToString typeErr)
        | Ok (programType, typedUserAst, userEnv) ->
            let plannedUserAst = JsonPlanning.rewriteProgram userEnv typedUserAst
            let plannedProgramType = TypeChecking.resolveType userEnv.AliasReg programType
            let renderedUserAst, boundaryProgramType =
                if plannedProgramType = AST.TUnit then
                    (plannedUserAst, AST.TUnit)
                else
                    (ValueRendering.rewriteProgram
                        stdlib.Context.Registries.RecordFieldsReg
                        userEnv.IndexedTypeReg
                        stdlib.Context.Registries.VariantLookup
                        stdlib.Context.Registries.FuncReg
                        plannedProgramType
                        plannedUserAst,
                     AST.TString)
            // Convert to ANF
            match convertTypedProgramToUserOnly stdlib.Context renderedUserAst with
            | Error err -> Error $"ANF conversion error: {err}"
            | Ok userOnly ->
                let coverageOptions = { defaultOptions with DisableANFOpt = true; DisableInlining = true }
                let sw = Stopwatch.StartNew()
                let entryFunction =
                    AST_to_ANF.synthesizeEntryFunction "_start" boundaryProgramType userOnly.MainExpr
                let userRegistries : AST_to_ANF.Registries = {
                    TypeReg = userOnly.TypeReg
                    RecordFieldsReg = userOnly.RecordFieldsReg
                    RecordTypeParamsReg = userOnly.RecordTypeParamsReg
                    VariantLookup = userOnly.VariantLookup
                    RcSumShapeReg = userOnly.RcSumShapeReg
                    FuncReg = userOnly.FuncReg
                    FuncParams = userOnly.FuncParams
                    ModuleRegistry = userOnly.ModuleRegistry
                    RecursiveMembers = userOnly.RecursiveMembers
                }
                match buildAnf 0 coverageOptions sw userRegistries Map.empty userOnly.NonInlineableFunctionNames (entryFunction :: userOnly.UserFunctions) false None with
                | Error err -> Error err
                | Ok (userFunctions, _typeMap) ->
                    match PrintInsertion.insertPrintInEntry "_start" boundaryProgramType userFunctions with
                    | Error err -> Error $"Print insertion error: {err}"
                    | Ok printedFunctions ->
                        let tcoFunctions = applyTco 0 coverageOptions sw userRegistries.RecursiveMembers printedFunctions None
                        let reachableStdlibNames =
                            ANFDeadCodeElimination.getReachableStdlib stdlib.StdlibANFCallGraph tcoFunctions
                        Ok reachableStdlibNames
