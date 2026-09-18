// CodeGen.fs - Assemble planned function and runtime-helper instruction chunks.

module CodeGen

open ARM64CodeGenTypes
open ARM64HeapAllocation
open ARM64ListReferenceCounts
open ARM64ClosureReferenceCounts
open ARM64ReleaseSelection
open ARM64DictReferenceCounts
open ARM64GenericReferenceCounts
open ARM64ProcessLifecycle
open ARM64RunProcess
open ARM64ExecuteProcess
open ARM64Functions
open ARM64Peephole
open ARM64ReleasePlanSummary

/// Convert LIR program to ARM64 instructions with options
/// Caller-owned conversion cache. Program-wide helper and layout generation is
/// intentionally outside this hook and is performed for every executable.
type FunctionCodegenCache =
    LIR.Function -> (unit -> Result<ARM64Symbolic.Instr list, string>) -> Result<ARM64Symbolic.Instr list, string>

/// Caller-owned cache for summaries of reusable function groups. Group
/// summaries form a monoid, so an executable can merge cached dependency and
/// stdlib facts with the small fresh program fragment.
type MetadataGroup = {
    ContextIdentity: obj
    Functions: LIR.Function list
}

type FunctionGroup = {
    /// Must uniquely identify this exact ordered function sequence when reusable.
    ContextIdentity: obj
    ReusableAcrossCompilations: bool
    Functions: LIR.Function list
}

type MetadataGroupCache =
    obj
        -> LIR.Function list
        -> (unit -> Arm64ProgramMetadata)
        -> Arm64ProgramMetadata

[<NoComparison>]
type HelperCacheKey = {
    ClosurePayloadSizesFromParams: (string * int) list
    ClosurePayloadSizesFromAllocs: (string * int) list
    ClosureCaptureTypes: (string * AST.Type list) list
    RecursiveReleaseTypes: AST.Type list
    CliArgvHelperLabels: string list
    NeedsCliExecuteHelper: bool
    NeedsCliRunProcessHelper: bool
    NeedsCliProcessLifecycleHelpers: bool
    NeedsRuntimeErrorHelper: bool
    ListDecHelperLabels: string list
    PlannedListDecHelpers: (string * int) list
    PlannedGenericDecHelperLabels: string list
    PlannedDictDecHelperLabels: string list
    DictDecHelperLabels: string list
    NeedsListRcIncHelper: bool
    NeedsDictRcIncHelper: bool
    NeedsClosureRcIncHelper: bool
    NeedsClosureRcDecHelper: bool
    NeedsStreamRcDecHelper: bool
}

type HelperCodegenCache =
    HelperCacheKey
        -> (unit -> ARM64Symbolic.Instr list)
        -> ARM64Symbolic.Instr list

type GeneratedChunk = {
    InstructionParts: ARM64Symbolic.Instr list list
    ReusableAcrossCompilations: bool
}

type FunctionGroupCodegenCache =
    obj
        -> LIR.Function list
        -> (unit -> Result<GeneratedChunk list, string>)
        -> Result<GeneratedChunk list, string>

type GeneratedProgram = private GeneratedProgram of GeneratedChunk list

let generatedProgramChunks (GeneratedProgram chunks) : GeneratedChunk list = chunks

let generatedProgramInstructions (GeneratedProgram chunks) : ARM64Symbolic.Instr list =
    chunks
    |> List.collect (fun chunk ->
        chunk.InstructionParts |> List.collect id)

let private generatePreparedARM64WithOptionsAndCache
    (target: ARM64.TargetConfig)
    (options: CodeGenOptions)
    (preparedSumShapeRegistry: MemoryModel.RcSumShapeRegistry option)
    (functionCache: FunctionCodegenCache option)
    (functionGroupCache: FunctionGroupCodegenCache option)
    (functionGroups: FunctionGroup list)
    (metadataGroupCache: MetadataGroupCache option)
    (helperCache: HelperCodegenCache option)
    (metadataGroups: MetadataGroup list)
    (lirOpExpansionRecorder: LirOpExpansionRecorder option)
    (phaseRecorder: (string -> float -> unit) option)
    (program: LIR.Program)
    : Result<GeneratedProgram, string> =
    let startPhase () =
        phaseRecorder |> Option.map (fun _ -> System.Diagnostics.Stopwatch.StartNew())
    let recordPhase name timer =
        match phaseRecorder, timer with
        | Some record, Some (timer: System.Diagnostics.Stopwatch) ->
            timer.Stop()
            record name timer.Elapsed.TotalMilliseconds
        | _ -> ()
    let metadataTimer = startPhase ()
    let (LIR.Program (functions, variantRegistry, recordRegistry)) = program
    let registrySetupTimer = startPhase ()
    let heapOverflowTrapBody = preparedHeapOverflowTrapBody target
    let sumShapeRegistry =
        preparedSumShapeRegistry
        |> Option.defaultWith (fun () ->
            rcSumShapeRegistryFromVariantRegistry variantRegistry)
    recordPhase "ARM64 Metadata Registry Setup" registrySetupTimer
    let functionInventoryTimer = startPhase ()
    // Ensure _start is first (entry point)
    let sortedFunctions =
        match List.partition (fun (f: LIR.Function) -> f.Name = "_start") functions with
        | startFunc :: _, otherFuncs -> startFunc :: otherFuncs
        | [], _ -> functions  // No _start, keep original order
    recordPhase "ARM64 Metadata Function Inventory" functionInventoryTimer

    let unionLabelSets (sets: Set<string> list) : Set<string> =
        match sets with
        | [] -> Set.empty
        | _ -> Set.unionMany sets

    let listDecHelperDictDependencyLabels (helperLabel: string) : Set<string> =
        if helperLabel = listRefCountDecDictListHelperLabel then
            Set.singleton dictRefCountDecListValueHelperLabel
        else
            Set.empty

    let summarizeReleasePlan = summarizePrecomputedReleasePlan

    let emptyRcHelperRequirements = precomputedEmptyRcHelperRequirements

    let emptyProgramMetadata = {
        Facts = {
            ClosurePayloadSizesFromParams = Map.empty
            ClosurePayloadSizesFromAllocs = Map.empty
            ClosureCaptureTypes = Map.empty
            RecursiveReleaseTypes = Set.empty
            CliArgvHelperLabels = Set.empty
            NeedsCliExecuteHelper = false
            NeedsCliRunProcessHelper = false
            NeedsCliProcessLifecycleHelpers = false
            NeedsRuntimeErrorHelper = false
        }
        RcHelperRequirements = emptyRcHelperRequirements
    }

    let hasRcHelperRequirements (requirements: RcHelperRequirements) =
        not (Set.isEmpty requirements.ListDecHelperLabels)
        || not (Map.isEmpty requirements.PlannedListDecHelpers)
        || not (Map.isEmpty requirements.PlannedGenericDecHelpers)
        || not (Map.isEmpty requirements.PlannedDictDecHelpers)
        || not (Set.isEmpty requirements.DictDecHelperLabels)
        || requirements.NeedsListRcIncHelper
        || requirements.NeedsDictRcIncHelper
        || requirements.NeedsClosureRcIncHelper
        || requirements.NeedsClosureRcDecHelper
        || requirements.NeedsStreamRcDecHelper
        || not (Map.isEmpty requirements.ReleasePlanSummaries)

    let contributesProgramMetadata (func: LIR.Function) =
        let facts =
            match func.CodegenFacts with
            | Some facts -> facts
            | None -> Crash.crash "ARM64 codegen invariant: missing validated function facts"
        let contributesRcHelpers =
            match facts.Arm64RcHelperRequirements with
            | Some requirements -> hasRcHelperRequirements requirements
            | None -> Crash.crash "ARM64 codegen invariant: missing validated RC helper requirements"
        Option.isSome facts.ClosurePayloadSizeFromParams
        || not (List.isEmpty facts.ClosurePayloadSizesFromAllocs)
        || not (Set.isEmpty facts.RecursiveReleaseTypes)
        || not (Set.isEmpty facts.RawSlotInitTypes)
        || facts.NeedsCliArgvHelper
        || facts.NeedsCliExecuteHelper
        || facts.NeedsCliRunProcessHelper
        || facts.NeedsCliProcessLifecycleHelpers
        || facts.NeedsRuntimeErrorHelper
        || contributesRcHelpers

    let releasePlanSummary = precomputedReleasePlanSummary
    let addReleasePlanRequirements = addPrecomputedReleasePlanRequirements

    let collectRawSlotInitRequirement
        (requirements: RcHelperRequirements)
        (retainTarget: LIR.Arm64SlotInitRootRetainTarget option)
        : RcHelperRequirements =
        match retainTarget with
        | Some LIR.SlotInitListRootRetain ->
            { requirements with NeedsListRcIncHelper = true }
        | Some LIR.SlotInitDictRootRetain ->
            { requirements with NeedsDictRcIncHelper = true }
        | Some LIR.SlotInitClosureRootRetain ->
            { requirements with NeedsClosureRcIncHelper = true }
        | Some LIR.SlotInitDynamicBufferRetain
        | Some (LIR.SlotInitGenericRootRetain _)
        | None ->
            requirements

    let collectFunctionMetadata
        (metadata: Arm64ProgramMetadata)
        ((func, facts): LIR.Function * LIR.FunctionCodegenFacts)
        : Arm64ProgramMetadata =
        let withClosureParams =
            match facts.ClosurePayloadSizeFromParams with
            | Some payloadSize ->
                {
                    metadata with
                        Facts = {
                            metadata.Facts with
                                ClosurePayloadSizesFromParams =
                                    Map.add
                                        func.Name
                                        payloadSize
                                        metadata.Facts.ClosurePayloadSizesFromParams
                        }
                }
            | None ->
                metadata

        let withClosureCaptures =
            match facts.ClosureCaptureTypes with
            | Some captures ->
                {
                    withClosureParams with
                        Facts = {
                            withClosureParams.Facts with
                                ClosureCaptureTypes =
                                    Map.add
                                        func.Name
                                        captures
                                        withClosureParams.Facts.ClosureCaptureTypes
                        }
                }
            | None ->
                withClosureParams

        let withAllocSizes =
            facts.ClosurePayloadSizesFromAllocs
            |> List.fold
                (fun metadata (funcName, payloadSize) ->
                    {
                        metadata with
                            Facts = {
                                metadata.Facts with
                                    ClosurePayloadSizesFromAllocs =
                                        Map.add
                                            funcName
                                            payloadSize
                                            metadata.Facts.ClosurePayloadSizesFromAllocs
                            }
                    })
                withClosureCaptures

        let requirements =
            match facts.Arm64RcHelperRequirements with
            | Some plannedRequirements ->
                mergePrecomputedRcHelperRequirements
                    withAllocSizes.RcHelperRequirements
                    plannedRequirements
            | None ->
                Crash.crash "ARM64 codegen invariant: missing validated RC helper requirements"
            |> fun requirements ->
                match facts.Arm64RawSlotInitRetainTargets with
                | Some targets ->
                    targets
                    |> Map.values
                    |> Seq.fold collectRawSlotInitRequirement requirements
                | None ->
                    facts.RawSlotInitTypes
                    |> Set.fold
                        (fun current valueType ->
                            slotInitRootRetainTarget
                                recordRegistry
                                sumShapeRegistry
                                valueType
                            |> collectRawSlotInitRequirement current)
                        requirements

        {
            withAllocSizes with
                Facts = {
                    withAllocSizes.Facts with
                        RecursiveReleaseTypes =
                            Set.union
                                withAllocSizes.Facts.RecursiveReleaseTypes
                                facts.RecursiveReleaseTypes
                        CliArgvHelperLabels =
                            if facts.NeedsCliArgvHelper then
                                Set.add
                                    $"__dark_cli_argv_{func.Name}"
                                    withAllocSizes.Facts.CliArgvHelperLabels
                            else
                                withAllocSizes.Facts.CliArgvHelperLabels
                        NeedsCliExecuteHelper =
                            withAllocSizes.Facts.NeedsCliExecuteHelper
                            || facts.NeedsCliExecuteHelper
                        NeedsCliRunProcessHelper =
                            withAllocSizes.Facts.NeedsCliRunProcessHelper
                            || facts.NeedsCliRunProcessHelper
                        NeedsCliProcessLifecycleHelpers =
                            withAllocSizes.Facts.NeedsCliProcessLifecycleHelpers
                            || facts.NeedsCliProcessLifecycleHelpers
                        NeedsRuntimeErrorHelper =
                            withAllocSizes.Facts.NeedsRuntimeErrorHelper
                            || facts.NeedsRuntimeErrorHelper
                }
                RcHelperRequirements = requirements
        }

    let finishMetadata (instructionMetadata: Arm64ProgramMetadata) =
        let rcHelperRequirements =
            instructionMetadata.Facts.ClosureCaptureTypes
            |> Map.fold
                (fun requirements _ captureTypes ->
                    captureTypes
                    |> List.fold
                        (fun requirements captureType ->
                            match tryRcReleasePlanOfType recordRegistry sumShapeRegistry captureType with
                            | None -> requirements
                            | Some releasePlan ->
                                let summary, requirements =
                                    releasePlanSummary
                                        true
                                        (LIR.StructuralReleasePlan (Some releasePlan))
                                        releasePlan
                                        requirements
                                let withPlanRequirements = addReleasePlanRequirements summary requirements
                                {
                                    withPlanRequirements with
                                        ListDecHelperLabels =
                                            Set.union
                                                withPlanRequirements.ListDecHelperLabels
                                                summary.ListDecHelperLabels
                                        DictDecHelperLabels =
                                            Set.union
                                                withPlanRequirements.DictDecHelperLabels
                                                summary.DictDecHelperLabels
                                })
                        requirements)
                instructionMetadata.RcHelperRequirements
        { instructionMetadata with RcHelperRequirements = rcHelperRequirements }

    let summarizeGroup (group: LIR.Function list) =
        let summaryTimer = startPhase ()
        // Retain source function order: map entries intentionally preserve the
        // original last-writer-wins behavior for compiler-generated labels.
        let summary =
            group
            |> List.map (fun func ->
                let facts =
                    match func.CodegenFacts with
                    | Some facts -> facts
                    | None -> Crash.crash "ARM64 codegen invariant: missing validated function facts"
                (func, facts))
            |> List.fold collectFunctionMetadata emptyProgramMetadata
            |> finishMetadata
        recordPhase "ARM64 Metadata Group Summarization" summaryTimer
        summary

    let mergeMaps left right =
        Map.fold (fun result key value -> Map.add key value result) left right

    let isEmptyMetadata (metadata: Arm64ProgramMetadata) =
        Map.isEmpty metadata.Facts.ClosurePayloadSizesFromParams
        && Map.isEmpty metadata.Facts.ClosurePayloadSizesFromAllocs
        && Map.isEmpty metadata.Facts.ClosureCaptureTypes
        && Set.isEmpty metadata.Facts.RecursiveReleaseTypes
        && Set.isEmpty metadata.Facts.CliArgvHelperLabels
        && not metadata.Facts.NeedsCliExecuteHelper
        && not metadata.Facts.NeedsCliRunProcessHelper
        && not metadata.Facts.NeedsCliProcessLifecycleHelpers
        && not metadata.Facts.NeedsRuntimeErrorHelper
        && not (hasRcHelperRequirements metadata.RcHelperRequirements)

    let mergeMetadata
        (left: Arm64ProgramMetadata)
        (right: Arm64ProgramMetadata)
        : Arm64ProgramMetadata =
        if isEmptyMetadata left then
            right
        elif isEmptyMetadata right then
            left
        else
            {
            Facts = {
                ClosurePayloadSizesFromParams =
                    mergeMaps
                        left.Facts.ClosurePayloadSizesFromParams
                        right.Facts.ClosurePayloadSizesFromParams
                ClosurePayloadSizesFromAllocs =
                    mergeMaps
                        left.Facts.ClosurePayloadSizesFromAllocs
                        right.Facts.ClosurePayloadSizesFromAllocs
                ClosureCaptureTypes =
                    mergeMaps
                        left.Facts.ClosureCaptureTypes
                        right.Facts.ClosureCaptureTypes
                RecursiveReleaseTypes =
                    Set.union
                        left.Facts.RecursiveReleaseTypes
                        right.Facts.RecursiveReleaseTypes
                CliArgvHelperLabels =
                    Set.union
                        left.Facts.CliArgvHelperLabels
                        right.Facts.CliArgvHelperLabels
                NeedsCliExecuteHelper =
                    left.Facts.NeedsCliExecuteHelper
                    || right.Facts.NeedsCliExecuteHelper
                NeedsCliRunProcessHelper =
                    left.Facts.NeedsCliRunProcessHelper
                    || right.Facts.NeedsCliRunProcessHelper
                NeedsCliProcessLifecycleHelpers =
                    left.Facts.NeedsCliProcessLifecycleHelpers
                    || right.Facts.NeedsCliProcessLifecycleHelpers
                NeedsRuntimeErrorHelper =
                    left.Facts.NeedsRuntimeErrorHelper
                    || right.Facts.NeedsRuntimeErrorHelper
            }
            RcHelperRequirements =
                mergePrecomputedRcHelperRequirements
                    left.RcHelperRequirements
                    right.RcHelperRequirements
            }

    let groupCompositionTimer = startPhase ()
    let groups : MetadataGroup list =
        match metadataGroups with
        | [] ->
            let identity = System.Object()
            [{ ContextIdentity = identity
               Functions = functions }]
        | groups -> groups

    let programMetadata =
        groups
        |> List.fold (fun metadata group ->
            // Functions with no contribution are the identity element. Leaving
            // them out makes cache keys reflect metadata semantics rather than
            // incidental reachability, so overlapping stdlib subsets reuse the
            // same immutable summary.
            let contributingFunctions =
                group.Functions |> List.filter contributesProgramMetadata
            let groupMetadata =
                match metadataGroupCache with
                | Some cache ->
                    cache
                        group.ContextIdentity
                        contributingFunctions
                        (fun () -> summarizeGroup contributingFunctions)
                | None ->
                    summarizeGroup contributingFunctions
            mergeMetadata metadata groupMetadata) emptyProgramMetadata
    recordPhase "ARM64 Metadata Group Composition" groupCompositionTimer

    let rcHelperRequirements = programMetadata.RcHelperRequirements
    let needsCliExecuteHelper = programMetadata.Facts.NeedsCliExecuteHelper
    let needsCliRunProcessHelper = programMetadata.Facts.NeedsCliRunProcessHelper
    let needsCliProcessLifecycleHelpers = programMetadata.Facts.NeedsCliProcessLifecycleHelpers

    let closurePayloadSizes =
        Map.fold
            (fun acc funcName payloadSize -> Map.add funcName payloadSize acc)
            programMetadata.Facts.ClosurePayloadSizesFromParams
            programMetadata.Facts.ClosurePayloadSizesFromAllocs

    // StackSize and UsedCalleeSaved are set per-function in convertFunction.
    let ctx = {
        Target = target
        Options = options
        SumShapeRegistry = sumShapeRegistry
        RecordRegistry = recordRegistry
        RawSlotInitRetainTargets = None
        ClosurePayloadSizes = closurePayloadSizes
        ClosureCaptureTypes = programMetadata.Facts.ClosureCaptureTypes
        FunctionName = ""
        InstructionSite = ""
        StackSize = 0
        UsedCalleeSaved = []
        HeapOverflowLabel = ""
        RecordLirOpExpansion = lirOpExpansionRecorder
    }

    let plannedListDecHelpers = rcHelperRequirements.PlannedListDecHelpers

    let plannedGenericDecHelpers =
        rcHelperRequirements.PlannedGenericDecHelpers

    let plannedDictDecHelpers = rcHelperRequirements.PlannedDictDecHelpers

    recordPhase "ARM64 Codegen Metadata" metadataTimer

    let convertCached func =
        // Function chunks are closed by their epilogue (or _start exit), so
        // peephole patterns cannot span into the next function's entry label.
        // The compiler's fixed _start trampoline is reusable too: the changing
        // user expression lives behind its __dark_compiler_program_entry call.
        // Cache each finalized chunk and never rescan it per executable.
        let generate () =
            convertFunction heapOverflowTrapBody ctx func
            |> Result.map peepholeOptimize
        let reusableAcrossCompilations = Option.isSome functionCache
        let converted =
            match functionCache with
            | Some cache when reusableAcrossCompilations -> cache func generate
            | _ -> generate ()
        converted
        |> Result.map (fun instructions -> {
            InstructionParts = [instructions]
            ReusableAcrossCompilations = reusableAcrossCompilations
        })

    let functionTimer = startPhase ()
    // Compilation assembly already knows the exact stdlib/program/dependency
    // boundaries. Consume those ordered groups directly instead of rescanning
    // every function to rediscover them during codegen.
    let functionRuns : (FunctionGroup option * LIR.Function list) list =
        match functionGroups with
        | [] -> [ (None, sortedFunctions) ]
        | groups ->
            let groupedFunctions = groups |> List.collect (fun group -> group.Functions)
            let orderMatches =
                List.length groupedFunctions = List.length sortedFunctions
                && List.forall2
                    (fun left right -> obj.ReferenceEquals(left, right))
                    groupedFunctions
                    sortedFunctions
            if not orderMatches then
                Crash.crash "ARM64 codegen invariant: function groups do not match program order"
            groups
            |> List.map (fun group -> (Some group, group.Functions))

    let convertRun (group: FunctionGroup option, runFunctions) =
        let generate () =
            ResultList.mapResults convertCached runFunctions
            |> Result.map (fun chunks ->
                match group, functionGroupCache, chunks with
                | Some group, Some _, _ :: _ when group.ReusableAcrossCompilations ->
                    // Retain the cached function instruction lists as separate
                    // preparation parts. Emission can compose their already-
                    // encoded templates once per reusable group without
                    // re-encoding every fixed instruction in each group shape.
                    [{
                        InstructionParts =
                            chunks
                            |> List.collect (fun chunk -> chunk.InstructionParts)
                        ReusableAcrossCompilations = true
                    }]
                | _ -> chunks)
        match group, functionGroupCache with
        | Some group, Some cache when group.ReusableAcrossCompilations ->
            cache group.ContextIdentity runFunctions generate
        | _ ->
            generate ()

    let convertedFunctionChunks =
        ResultList.mapResults convertRun functionRuns
        |> Result.map List.concat
    recordPhase "ARM64 Codegen Functions" functionTimer

    convertedFunctionChunks
    |> Result.map (fun functionChunks ->
        let functionChunks =
            if needsCliProcessLifecycleHelpers && ARM64.targetOS target = Platform.Linux then
                functionChunks
                |> List.map (fun chunk ->
                    let containsStartEpilogue =
                        chunk.InstructionParts
                        |> List.exists (List.contains (ARM64Symbolic.Label "_epilogue__start"))
                    if containsStartEpilogue then
                        { chunk with
                            InstructionParts =
                                chunk.InstructionParts
                                |> List.map (List.collect (fun instr ->
                                    if instr = ARM64Symbolic.Label "_epilogue__start" then
                                        [instr; ARM64Symbolic.BL "__dark_cli_cleanup_processes"]
                                    else
                                        [instr]))
                            ReusableAcrossCompilations = false }
                    else
                        chunk)
            else
                functionChunks
        let helperTimer = startPhase ()
        let generateHelperInstructions () =
            // The helper cache key below completely describes helper planning
            // as well as the emitted helper instructions. Keep the dependency
            // closure inside the cache miss path so repeated executables do
            // not rediscover an already-generated plan.
            let helperMetadataTimer = startPhase ()
            let listDecHelperDependencyLabels (helperLabel: string) : Set<string> =
                match Map.tryFind helperLabel plannedListDecHelpers with
                | Some (_, releasePlan) ->
                    (summarizeReleasePlan false releasePlan).ListDecHelperLabels
                    |> Set.remove helperLabel
                | None ->
                    Set.empty

            let rec expandListDecHelperDependencies
                (selectedLabels: Set<string>)
                (pendingLabels: string list)
                : Set<string> =
                match pendingLabels with
                | [] ->
                    selectedLabels
                | helperLabel :: rest ->
                    let dependencyLabels =
                        listDecHelperDependencyLabels helperLabel
                        |> Set.filter (fun dependencyLabel ->
                            not (Set.contains dependencyLabel selectedLabels))

                    expandListDecHelperDependencies
                        (Set.union selectedLabels dependencyLabels)
                        (rest @ Set.toList dependencyLabels)

            let neededListRcDecHelperLabels =
                let calledLabels = rcHelperRequirements.ListDecHelperLabels
                if Set.isEmpty calledLabels then
                    Set.empty
                else
                    let rootLabels = Set.add listRefCountDecHelperLabel calledLabels
                    expandListDecHelperDependencies rootLabels (Set.toList rootLabels)

            let rec rcReleasePlanContains
                (predicate: MemoryModel.RcReleasePlan -> bool)
                (releasePlan: MemoryModel.RcReleasePlan)
                : bool =
                if predicate releasePlan then
                    true
                else
                    match releasePlan with
                    | MemoryModel.RootRelease (_, _, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases))
                    | MemoryModel.RootRelease (_, _, MemoryModel.BoxedSumPayloadRelease (_, fieldReleases, _))
                    | MemoryModel.RootRelease (_, _, MemoryModel.ClosurePayloadRelease fieldReleases) ->
                        fieldReleases
                        |> List.exists (fun (MemoryModel.FieldRelease (_, fieldReleasePlan)) ->
                            rcReleasePlanContains predicate fieldReleasePlan)
                    | MemoryModel.RootRelease (_, _, MemoryModel.DictPayloadRelease (keyRelease, valueRelease)) ->
                        rcReleasePlanContains predicate keyRelease
                        || rcReleasePlanContains predicate valueRelease
                    | MemoryModel.RootRelease (_, _, MemoryModel.TaggedListPayloadRelease elementRelease) ->
                        rcReleasePlanContains predicate elementRelease
                    | _ ->
                        false

            let selectedPlannedListHelpersNeed predicate selectedLabels =
                plannedListDecHelpers
                |> Map.toList
                |> List.exists (fun (helperLabel, (_, releasePlan)) ->
                    Set.contains helperLabel selectedLabels
                    && rcReleasePlanContains predicate releasePlan)

            let selectedStaticListHelpersNeed
                (directSpecNeed: ListRefCountDecHelperSpec -> bool)
                (selectedLabels: Set<string>)
                : bool =
                listRefCountDecHelperSpecs
                |> List.exists (fun spec ->
                    Set.contains spec.Label selectedLabels
                    && directSpecNeed spec)

            let dictDecHelperDependencyLabels (helperLabel: string) : Set<string> =
                match Map.tryFind helperLabel plannedDictDecHelpers with
                | Some (MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, valueRelease))) ->
                    match valueRelease with
                    | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
                        Set.singleton dictRefCountDecListValueHelperLabel
                    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
                        Set.singleton (dictDecHelperForReleasePlan valueRelease)
                    | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, _) ->
                        Set.union
                            (summarizeReleasePlan false valueRelease).ListDecHelperLabels
                            (summarizeReleasePlan true valueRelease).DictDecHelperLabels
                    | _ ->
                        Set.empty
                | Some other ->
                    Crash.crash $"ARM64 planned dict dependency labels require a DictHeap release plan, got {other}"
                | None ->
                    match helperLabel with
                    | label when label = dictRefCountDecListValueHelperLabel ->
                        Set.singleton listRefCountDecHelperLabel
                    | label when label = dictRefCountDecDictValueHelperLabel ->
                        Set.singleton dictRefCountDecHelperLabel
                    | label when label = dictRefCountDecDictListValueHelperLabel ->
                        Set.singleton dictRefCountDecListValueHelperLabel
                    | label when label = dictRefCountDecTupleStringListValueHelperLabel ->
                        Set.singleton listRefCountDecHelperLabel
                    | label when label = dictRefCountDecTupleStringListDictValueHelperLabel ->
                        Set.ofList [ listRefCountDecHelperLabel; dictRefCountDecHelperLabel ]
                    | _ ->
                        Set.empty

            let neededDictRcDecHelperLabels =
                let listHelperDictLabels =
                    neededListRcDecHelperLabels
                    |> Set.toList
                    |> List.map (fun helperLabel ->
                        let staticLabels = listDecHelperDictDependencyLabels helperLabel
                        let plannedLabels =
                            plannedListDecHelpers
                            |> Map.tryFind helperLabel
                            |> Option.map (fun (_, releasePlan) ->
                                (summarizeReleasePlan false releasePlan).DictDecHelperLabels)
                            |> Option.defaultValue Set.empty
                        Set.union staticLabels plannedLabels)
                    |> unionLabelSets

                let directLabels =
                    rcHelperRequirements.DictDecHelperLabels
                    |> Set.union listHelperDictLabels

                let rec expandDependencies selectedLabels pendingLabels =
                    match pendingLabels with
                    | [] -> selectedLabels
                    | helperLabel :: rest ->
                        let dependencies =
                            dictDecHelperDependencyLabels helperLabel
                            |> Set.filter (fun dependency ->
                                not (Set.contains dependency selectedLabels))
                        expandDependencies
                            (Set.union selectedLabels dependencies)
                            (rest @ Set.toList dependencies)

                expandDependencies directLabels (Set.toList directLabels)

            let listRcDecHelperLabelsFromDictHelpers =
                neededDictRcDecHelperLabels
                |> Set.toList
                |> List.map dictDecHelperDependencyLabels
                |> unionLabelSets
                |> Set.filter (fun label -> label = listRefCountDecHelperLabel)

            let selectedListRcDecHelperLabels =
                Set.union neededListRcDecHelperLabels listRcDecHelperLabelsFromDictHelpers

            let needsDictRcDecHelper =
                Set.contains dictRefCountDecHelperLabel neededDictRcDecHelperLabels
            let needsDictRcDecListValueHelper =
                Set.contains dictRefCountDecListValueHelperLabel neededDictRcDecHelperLabels
            let needsDictRcDecDictValueHelper =
                Set.contains dictRefCountDecDictValueHelperLabel neededDictRcDecHelperLabels
            let needsDictRcDecDictListValueHelper =
                Set.contains dictRefCountDecDictListValueHelperLabel neededDictRcDecHelperLabels
            let needsDictRcDecTupleStringListValueHelper =
                Set.contains dictRefCountDecTupleStringListValueHelperLabel neededDictRcDecHelperLabels
            let needsDictRcDecTupleStringListDictValueHelper =
                Set.contains dictRefCountDecTupleStringListDictValueHelperLabel neededDictRcDecHelperLabels
            let needsDictRcDecSumStringValueHelper =
                Set.contains dictRefCountDecSumStringValueHelperLabel neededDictRcDecHelperLabels

            recordPhase "ARM64 Metadata Helper Planning" helperMetadataTimer

            let helperSelectionTimer = startPhase ()
            let selectedListHelpersNeedDictDecHelper =
                selectedStaticListHelpersNeed
                    (fun spec -> spec.ReleaseLeafDictPayload)
                    selectedListRcDecHelperLabels
                || selectedPlannedListHelpersNeed
                    (function MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) -> true | _ -> false)
                    selectedListRcDecHelperLabels

            let selectedListHelpersNeedClosureDecHelper =
                selectedStaticListHelpersNeed
                    (fun spec -> spec.ReleaseLeafClosurePayload)
                    selectedListRcDecHelperLabels
                || selectedPlannedListHelpersNeed
                    (function MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) -> true | _ -> false)
                    selectedListRcDecHelperLabels

            let plannedDictHelpersNeedClosureDecHelper =
                plannedDictDecHelpers
                |> Map.exists (fun _ releasePlan ->
                    rcReleasePlanContains
                        (function MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) -> true | _ -> false)
                        releasePlan)

            let selectedListHelpersNeedStreamDecHelper =
                selectedPlannedListHelpersNeed
                    (function MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) -> true | _ -> false)
                    selectedListRcDecHelperLabels

            let plannedDictHelpersNeedStreamDecHelper =
                plannedDictDecHelpers
                |> Map.exists (fun _ releasePlan ->
                    rcReleasePlanContains
                        (function MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) -> true | _ -> false)
                        releasePlan)

            let needsClosureRcDecHelper = rcHelperRequirements.NeedsClosureRcDecHelper
            let selectedClosureHelpersNeedStreamDecHelper =
                if needsClosureRcDecHelper
                   || selectedListHelpersNeedClosureDecHelper
                   || plannedDictHelpersNeedClosureDecHelper then
                    ctx.ClosureCaptureTypes
                    |> Map.exists (fun _ captureTypes ->
                        captureTypes
                        |> List.exists (fun captureType ->
                            tryRcReleasePlanOfType ctx.RecordRegistry ctx.SumShapeRegistry captureType
                            |> Option.exists (rcReleasePlanContains
                                (function MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) -> true | _ -> false))))
                else
                    false

            let needsStreamRcDecHelper =
                rcHelperRequirements.NeedsStreamRcDecHelper
                || selectedListHelpersNeedStreamDecHelper
                || plannedDictHelpersNeedStreamDecHelper
                || selectedClosureHelpersNeedStreamDecHelper

            let emitClosureRcDecHelper =
                needsClosureRcDecHelper
                || selectedListHelpersNeedClosureDecHelper
                || plannedDictHelpersNeedClosureDecHelper
                || needsStreamRcDecHelper

            recordPhase "ARM64 Helper Selection" helperSelectionTimer
            let listHelperTimer = startPhase ()
            let listRcHelpers =
                (if rcHelperRequirements.NeedsListRcIncHelper then generateListRefCountIncHelper () else [])
                @ generateNeededListRefCountDecHelpers ctx selectedListRcDecHelperLabels plannedListDecHelpers
            recordPhase "ARM64 Helper List Generation" listHelperTimer
            let genericHelperTimer = startPhase ()
            let genericRcHelpers =
                plannedGenericDecHelpers
                |> Map.toList
                |> List.collect (fun (helperLabel, spec) ->
                    let generate () =
                        generatePlannedGenericRefCountDecHelper helperLabel spec ctx
                        |> Ok
                    let generated =
                        match functionCache with
                        | Some cache ->
                            cache
                                (plannedGenericRefCountDecHelperCacheKey helperLabel)
                                generate
                        | None ->
                            generate ()
                    match generated with
                    | Ok instructions -> instructions
                    | Error error ->
                        Crash.crash
                            $"ARM64 cached generic release helper generation failed for {helperLabel}: {error}")
            recordPhase "ARM64 Helper Generic Release Generation" genericHelperTimer
            let dictHelperTimer = startPhase ()
            let dictRcHelpers =
                (if rcHelperRequirements.NeedsDictRcIncHelper then generateDictRefCountIncHelper () else [])
                @ (plannedDictDecHelpers
                   |> Map.toList
                   |> List.collect (fun (helperLabel, releasePlan) ->
                       generatePlannedDictRefCountDecHelper helperLabel releasePlan ctx))
                @ (if needsDictRcDecHelper || selectedListHelpersNeedDictDecHelper || not (Map.isEmpty plannedDictDecHelpers) then generateDictRefCountDecHelper dictRefCountDecHelperLabel MemoryModel.NoReleasePlan false false None false false None false false false ctx else [])
                @ (if needsDictRcDecListValueHelper then generateDictRefCountDecHelper dictRefCountDecListValueHelperLabel MemoryModel.NoReleasePlan false true None false false None false false false ctx else [])
                @ (if needsDictRcDecDictValueHelper then generateDictRefCountDecHelper dictRefCountDecDictValueHelperLabel MemoryModel.NoReleasePlan false false (Some dictRefCountDecHelperLabel) false false None false false false ctx else [])
                @ (if needsDictRcDecDictListValueHelper then generateDictRefCountDecHelper dictRefCountDecDictListValueHelperLabel MemoryModel.NoReleasePlan false false (Some dictRefCountDecListValueHelperLabel) false false None false false false ctx else [])
                @ (if needsDictRcDecTupleStringListValueHelper then generateDictRefCountDecHelper dictRefCountDecTupleStringListValueHelperLabel MemoryModel.NoReleasePlan false false None false false None true false false ctx else [])
                @ (if needsDictRcDecTupleStringListDictValueHelper then generateDictRefCountDecHelper dictRefCountDecTupleStringListDictValueHelperLabel MemoryModel.NoReleasePlan false false None false false None false true false ctx else [])
                @ (if needsDictRcDecSumStringValueHelper then generateDictRefCountDecHelper dictRefCountDecSumStringValueHelperLabel MemoryModel.NoReleasePlan false false None false false None false false true ctx else [])
            recordPhase "ARM64 Helper Dict Generation" dictHelperTimer
            let closureHelperTimer = startPhase ()
            let closureRcHelpers =
                (if rcHelperRequirements.NeedsClosureRcIncHelper then generateClosureRefCountIncHelper ctx else [])
                @ (if emitClosureRcDecHelper then generateClosureRefCountDecHelper dictDecHelperForReleasePlan ctx else [])
            let streamRcHelpers =
                if needsStreamRcDecHelper then generateStreamRefCountDecHelper ctx else []
            recordPhase "ARM64 Helper Closure Stream Generation" closureHelperTimer
            let recursiveHelperTimer = startPhase ()
            let recursiveSumRcHelpers =
                programMetadata.Facts.RecursiveReleaseTypes
                |> Set.toList
                |> List.collect (generateRecursiveSumRefCountDecHelper ctx)
            recordPhase "ARM64 Helper Recursive Sum Generation" recursiveHelperTimer
            let cliHelperTimer = startPhase ()
            let cliArgvHelpers =
                programMetadata.Facts.CliArgvHelperLabels
                |> Set.toList
                |> List.collect (generateCliArgvHelper ctx)
            let cliHelpers =
                cliArgvHelpers
                @ (if needsCliProcessLifecycleHelpers && ARM64.targetOS target = Platform.Linux then generateLinuxCliSpawnProcessHelper () else [])
                @ (if needsCliProcessLifecycleHelpers && ARM64.targetOS target = Platform.Linux then generateLinuxCliProcessLifecycleHelpers ctx else [])
                @ (if needsCliRunProcessHelper && ARM64.targetOS target = Platform.Linux then generateLinuxCliRunProcessHelper () else [])
                @ (if needsCliExecuteHelper && ARM64.targetOS target = Platform.Linux then generateLinuxCliExecuteHelper () else [])
            recordPhase "ARM64 Helper CLI Generation" cliHelperTimer
            let runtimeErrorHelper =
                if programMetadata.Facts.NeedsRuntimeErrorHelper then
                    generateRuntimeErrorHelper target
                else
                    []
            let helperInstructions =
                listRcHelpers
                @ genericRcHelpers
                @ dictRcHelpers
                @ closureRcHelpers
                @ streamRcHelpers
                @ recursiveSumRcHelpers
                @ cliHelpers
                @ runtimeErrorHelper
            let peepholeTimer = startPhase ()
            let optimized = peepholeOptimize helperInstructions
            recordPhase "ARM64 Codegen Peephole" peepholeTimer
            optimized
        let helperCacheKey = {
            ClosurePayloadSizesFromParams =
                programMetadata.Facts.ClosurePayloadSizesFromParams |> Map.toList
            ClosurePayloadSizesFromAllocs =
                programMetadata.Facts.ClosurePayloadSizesFromAllocs |> Map.toList
            ClosureCaptureTypes =
                programMetadata.Facts.ClosureCaptureTypes |> Map.toList
            RecursiveReleaseTypes =
                programMetadata.Facts.RecursiveReleaseTypes |> Set.toList
            CliArgvHelperLabels =
                programMetadata.Facts.CliArgvHelperLabels |> Set.toList
            NeedsCliExecuteHelper = programMetadata.Facts.NeedsCliExecuteHelper
            NeedsCliRunProcessHelper = programMetadata.Facts.NeedsCliRunProcessHelper
            NeedsCliProcessLifecycleHelpers = programMetadata.Facts.NeedsCliProcessLifecycleHelpers
            NeedsRuntimeErrorHelper = programMetadata.Facts.NeedsRuntimeErrorHelper
            ListDecHelperLabels = rcHelperRequirements.ListDecHelperLabels |> Set.toList
            PlannedListDecHelpers =
                rcHelperRequirements.PlannedListDecHelpers
                |> Map.toList
                |> List.map (fun (label, (payloadSize, _releasePlan)) ->
                    (label, payloadSize))
            PlannedGenericDecHelperLabels =
                rcHelperRequirements.PlannedGenericDecHelpers |> Map.keys |> Seq.toList
            PlannedDictDecHelperLabels =
                rcHelperRequirements.PlannedDictDecHelpers |> Map.keys |> Seq.toList
            DictDecHelperLabels = rcHelperRequirements.DictDecHelperLabels |> Set.toList
            NeedsListRcIncHelper = rcHelperRequirements.NeedsListRcIncHelper
            NeedsDictRcIncHelper = rcHelperRequirements.NeedsDictRcIncHelper
            NeedsClosureRcIncHelper = rcHelperRequirements.NeedsClosureRcIncHelper
            NeedsClosureRcDecHelper = rcHelperRequirements.NeedsClosureRcDecHelper
            NeedsStreamRcDecHelper = rcHelperRequirements.NeedsStreamRcDecHelper
        }
        let optimizedHelperInstructions =
            match helperCache with
            | Some cache -> cache helperCacheKey generateHelperInstructions
            | None -> generateHelperInstructions ()
        recordPhase "ARM64 Codegen Helpers" helperTimer
        let assemblyTimer = startPhase ()
        let generated =
            GeneratedProgram (
                functionChunks
                @ [{
                    InstructionParts = [optimizedHelperInstructions]
                    ReusableAcrossCompilations = Option.isSome helperCache
                }])
        recordPhase "ARM64 Codegen Assembly" assemblyTimer
        generated)

let generateARM64WithOptionsAndCaches
    (target: ARM64.TargetConfig)
    (options: CodeGenOptions)
    (preparedSumShapeRegistry: MemoryModel.RcSumShapeRegistry option)
    (functionCache: FunctionCodegenCache option)
    (functionGroupCache: FunctionGroupCodegenCache option)
    (functionGroups: FunctionGroup list)
    (metadataGroupCache: MetadataGroupCache option)
    (helperCache: HelperCodegenCache option)
    (metadataGroups: MetadataGroup list)
    (lirOpExpansionRecorder: LirOpExpansionRecorder option)
    (phaseRecorder: (string -> float -> unit) option)
    (program: LIR.Program)
    : Result<GeneratedProgram, string> =
    let (LIR.Program (functions, _, _)) = program
    let missingFacts =
        functions
        |> List.tryPick (fun func ->
            match func.CodegenFacts with
            | None ->
                Some $"ARM64 codegen requires prepared LIR; function '{func.Name}' has no codegen facts"
            | Some facts when Option.isNone facts.Arm64RcHelperRequirements ->
                Some $"ARM64 codegen requires prepared LIR; function '{func.Name}' has no ARM64 helper plan"
            | Some _ ->
                None)
    match missingFacts with
    | Some error -> Error error
    | None ->
        generatePreparedARM64WithOptionsAndCache
            target
            options
            preparedSumShapeRegistry
            functionCache
            functionGroupCache
            functionGroups
            metadataGroupCache
            helperCache
            metadataGroups
            lirOpExpansionRecorder
            phaseRecorder
            program

let generateARM64WithOptionsAndCache
    (target: ARM64.TargetConfig)
    (options: CodeGenOptions)
    (functionCache: FunctionCodegenCache option)
    (phaseRecorder: (string -> float -> unit) option)
    (program: LIR.Program)
    : Result<GeneratedProgram, string> =
    generateARM64WithOptionsAndCaches
        target
        options
        None
        functionCache
        None
        []
        None
        None
        []
        None
        phaseRecorder
        program

let generateARM64WithOptions (target: ARM64.TargetConfig) (options: CodeGenOptions) (program: LIR.Program) : Result<GeneratedProgram, string> =
    generateARM64WithOptionsAndCache target options None None program

/// Convert LIR program to ARM64 instructions (uses default options)
let generateARM64 (target: ARM64.TargetConfig) (program: LIR.Program) : Result<GeneratedProgram, string> =
    generateARM64WithOptions target defaultOptions program
