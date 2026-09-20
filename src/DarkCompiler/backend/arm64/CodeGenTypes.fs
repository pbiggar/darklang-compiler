// CodeGenTypes.fs - Define target code generation options, contexts, and planning facts.

module ARM64CodeGenTypes

type CodeGenOptions = {
    /// Disable free list memory reuse (always bump allocate)
    DisableFreeList: bool
    /// Enable coverage instrumentation
    EnableCoverage: bool
    /// Number of coverage expressions (determines buffer size)
    CoverageExprCount: int
    /// Enable leak checking instrumentation
    EnableLeakCheck: bool
}

/// Default code generation options
let defaultOptions : CodeGenOptions = {
    DisableFreeList = false
    EnableCoverage = false
    CoverageExprCount = 0
    EnableLeakCheck = false
}

/// Caller-owned reuse for expensive, immutable release-plan summaries.
/// The cache validates complete release-plan shapes before returning a value.
type ReleasePlanSummaryCache =
    bool
        -> string
        -> MemoryModel.RcReleasePlan
        -> (unit -> LIR.Arm64ReleasePlanSummary)
        -> LIR.Arm64ReleasePlanSummary

/// Opt-in attribution for one freshly generated LIR instruction. The elapsed
/// value uses Stopwatch timestamp ticks, and the instruction count is measured
/// before the function-level ARM64 peephole pass.
type LirOpExpansionRecorder = string -> string -> string -> int -> int64 -> unit

/// Code generation context (passed through to instruction conversion)
type CodeGenContext = {
    Target: ARM64.TargetConfig
    Options: CodeGenOptions
    SumShapeRegistry: MemoryModel.RcSumShapeRegistry
    RecordRegistry: LIR.RecordRegistry
    RawSlotInitRetainTargets: Map<AST.Type, LIR.Arm64SlotInitRootRetainTarget option> option
    ClosurePayloadSizes: Map<string, int>
    ClosureCaptureTypes: Map<string, AST.Type list>
    FunctionNames: Map<AST.FunctionId, string>
    FunctionName: string
    /// Deterministic block/instruction identity for labels emitted by an effect.
    /// One source effect can be cloned into multiple CFG locations.
    InstructionSite: string
    // Function context for tail call epilogue generation
    StackSize: int
    UsedCalleeSaved: LIR.PhysReg list
    HeapOverflowLabel: string
    RecordLirOpExpansion: LirOpExpansionRecorder option
}

let internal functionName (ctx: CodeGenContext) (functionId: AST.FunctionId) : string =
    match Map.tryFind functionId ctx.FunctionNames with
    | Some name -> name
    | None -> Crash.crash $"ARM64 code generation: missing function name for identity {AST.functionIdValue functionId}"

let internal rcSumShapeRegistryFromVariantRegistry (variantRegistry: LIR.VariantRegistry) : MemoryModel.RcSumShapeRegistry =
    variantRegistry
    |> Map.map (fun _typeName typeVariants ->
        { TypeParams = typeVariants.TypeParams
          Payloads =
            typeVariants.Variants
            |> List.sortBy (fun variant -> variant.Tag)
            |> List.map (fun variant -> variant.Tag, variant.Payload) })

let leakCounterLabel = ARM64Symbolic.leakCounterLabelName
let heapOutOfMemoryMessage = "Out of heap memory"
let internal heapMmapSizeBytes = 512L * 1024L * 1024L
let internal heapMmapSizeMovzImm16 = 0x2000us  // 512MB == 0x20000000
let internal heapOverflowLabelPrefix = "__heap_oom_"
let internal runtimeErrorHelperLabel = "__dark_runtime_error"
let internal listRefCountIncHelperLabel = "__dark_list_refcount_inc_helper"
let internal listRefCountDecHelperLabel = "__dark_list_refcount_dec_helper"
let private plannedListRefCountDecHelperLabelPrefix = "__dark_list_refcount_dec_plan_"
let internal plannedGenericRefCountDecHelperLabelPrefix = "__dark_generic_refcount_dec_plan_"
let internal listRefCountDecStringHelperLabel = "__dark_list_refcount_dec_string_helper"
let internal listRefCountDecBlobHelperLabel = "__dark_list_refcount_dec_blob_helper"
let internal listRefCountDecListHelperLabel = "__dark_list_refcount_dec_list_helper"
let internal listRefCountDecDictHelperLabel = "__dark_list_refcount_dec_dict_helper"
let internal listRefCountDecDictListHelperLabel = "__dark_list_refcount_dec_dict_list_helper"
let internal listRefCountDecClosureHelperLabel = "__dark_list_refcount_dec_closure_helper"
let internal dictRefCountIncHelperLabel = "__dark_dict_refcount_inc_helper"
let internal dictRefCountDecHelperLabel = "__dark_dict_refcount_dec_helper"
let internal dictRefCountDecListValueHelperLabel = "__dark_dict_refcount_dec_list_value_helper"
let internal dictRefCountDecDictValueHelperLabel = "__dark_dict_refcount_dec_dict_value_helper"
let internal dictRefCountDecDictListValueHelperLabel = "__dark_dict_refcount_dec_dict_list_value_helper"
let internal dictRefCountDecTupleStringListValueHelperLabel = "__dark_dict_refcount_dec_tuple_string_list_value_helper"
let internal dictRefCountDecTupleStringListDictValueHelperLabel = "__dark_dict_refcount_dec_tuple_string_list_dict_value_helper"
let internal dictRefCountDecSumStringValueHelperLabel = "__dark_dict_refcount_dec_sum_string_value_helper"
let private plannedDictRefCountDecHelperLabelPrefix = "__dark_dict_refcount_dec_plan_"
let internal closureRefCountIncHelperLabel = "__dark_closure_refcount_inc_helper"
let internal closureRefCountDecHelperLabel = "__dark_closure_refcount_dec_helper"
let internal streamRefCountDecHelperLabel = "__dark_stream_refcount_dec_helper"

// Small fixed blocks are cheaper to build directly than to name and cache.
// Stop counting as soon as repeated recursive expansion becomes the dominant
// cost and an outlined helper is worthwhile.
let private genericReleaseHelperNodeThreshold = ReleasePlanFingerprint.rcReleasePlanCompactKeyNodeThreshold

let internal genericReleasePlanIsExpensive (releasePlan: MemoryModel.RcReleasePlan) : bool =
    ReleasePlanFingerprint.rcReleasePlanExceedsNodeCount
        genericReleaseHelperNodeThreshold
        releasePlan

let internal callerOwnsSinglePayloadSum (functionName: string) : bool =
    functionName.StartsWith("Darklang.Stdlib.Dict.")
    || not (functionName.StartsWith("Darklang.Stdlib."))

let internal recursiveNominalRefCountDecHelperLabel (sourceType: AST.Type) : string =
    $"__dark_recursive_nominal_rc_dec_{ReleasePlanFingerprint.rcSourceTypeFingerprint sourceType}"

let internal plannedListDecHelperLabelForFingerprint (fingerprint: string) : string =
    $"{plannedListRefCountDecHelperLabelPrefix}{fingerprint}"

let internal plannedListDecHelperLabelForReleasePlan (releasePlan: MemoryModel.RcReleasePlan) : string =
    releasePlan
    |> ReleasePlanFingerprint.rcReleasePlanFingerprint
    |> plannedListDecHelperLabelForFingerprint

let internal plannedGenericDecHelperBaseLabelForFingerprint (fingerprint: string) : string =
    $"{plannedGenericRefCountDecHelperLabelPrefix}{fingerprint}"

let internal specializePlannedGenericDecHelperLabel
    (ownsSinglePayloadSum: bool)
    (baseLabel: string)
    : string =
    let ownership = if ownsSinglePayloadSum then "owned" else "borrowed"
    $"{baseLabel}_{ownership}"

let internal plannedDictDecHelperLabelForFingerprint (fingerprint: string) : string =
    $"{plannedDictRefCountDecHelperLabelPrefix}{fingerprint}"

let internal plannedDictDecHelperLabelForReleasePlan (releasePlan: MemoryModel.RcReleasePlan) : string =
    releasePlan
    |> ReleasePlanFingerprint.rcReleasePlanFingerprint
    |> plannedDictDecHelperLabelForFingerprint

type internal RcReleasePlanSummary = LIR.Arm64ReleasePlanSummary
type internal RcHelperRequirements = LIR.Arm64RcHelperRequirements

type Arm64ProgramFacts = {
    ClosurePayloadSizesFromParams: Map<string, int>
    ClosurePayloadSizesFromAllocs: Map<AST.FunctionId, int>
    ClosureCaptureTypes: Map<string, AST.Type list>
    RecursiveReleaseTypes: Set<AST.Type>
    CliArgvHelperLabels: Set<string>
    NeedsCliExecuteHelper: bool
    NeedsCliRunProcessHelper: bool
    NeedsCliProcessLifecycleHelpers: bool
    NeedsRuntimeErrorHelper: bool
}

/// ARM64-only metadata assembled from the reachable functions' carried facts.
[<Struct>]
type Arm64ProgramMetadata = {
    Facts: Arm64ProgramFacts
    RcHelperRequirements: RcHelperRequirements
}

let internal slotInitRootRetainTarget
    (recordRegistry: LIR.RecordRegistry)
    (sumShapeRegistry: MemoryModel.RcSumShapeRegistry)
    (valueType: AST.Type)
    : LIR.Arm64SlotInitRootRetainTarget option =
    let shapeOfKnownType (typ: AST.Type) : MemoryModel.RcShape option =
        match typ with
        | AST.TRecord (name, _) when not (Map.containsKey name recordRegistry) ->
            None
        | _ ->
            Some (
                MemoryPlanning.rcShapeOfTypeWithSums
                    recordRegistry
                    (MemoryPlanning.inferredRecordTypeParamsRegistry recordRegistry)
                    sumShapeRegistry
                    typ
            )

    shapeOfKnownType valueType
    |> Option.bind (function
            | MemoryModel.TaggedListShape _ ->
                Some LIR.SlotInitListRootRetain
            | MemoryModel.DictRoot _ ->
                Some LIR.SlotInitDictRootRetain
            | MemoryModel.DynamicString
            | MemoryModel.DynamicBlob
            | MemoryModel.DynamicInt ->
                Some LIR.SlotInitDynamicBufferRetain
            | MemoryModel.ClosureShape _ ->
                Some LIR.SlotInitClosureRootRetain
            | MemoryModel.FixedBlock (payloadSize, _) ->
                match valueType with
                | AST.TTuple _
                | AST.TRecord _
                | AST.TInt128
                | AST.TUInt128 -> Some (LIR.SlotInitGenericRootRetain payloadSize)
                | _ -> None
            | MemoryModel.StreamRoot -> Some (LIR.SlotInitGenericRootRetain 24)
            | MemoryModel.BoxedSum (payloadSize, _, _) ->
                match valueType with
                | AST.TSum _ -> Some (LIR.SlotInitGenericRootRetain payloadSize)
                | _ -> None
            | MemoryModel.RecursiveNominalRef _ ->
                Some (LIR.SlotInitGenericRootRetain 16)
            | MemoryModel.Immediate
            | MemoryModel.StaticString
            | MemoryModel.RawUnmanaged ->
                None)
