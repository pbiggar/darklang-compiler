// MemoryModel.fs - ANF-independent memory representation and release contracts.

module MemoryModel

/// Immutable canonical byte-buffer representations whose semantic equality is
/// byte equality. The kind preserves the source-level reason that the
/// representation comparison is valid instead of conflating integers with
/// strings after lowering.
type CanonicalBufferKind =
    | Utf8String
    | GraphemeCluster

/// Reference-count operation kind
type RcKind =
    | GenericHeap
    | StreamHeap
    | TaggedList
    | DictHeap
    | ClosureHeap

/// Runtime representation shape used to decide ownership behavior.
///
/// This is deliberately more specific than source-level heap-ness: values with
/// the same source type category can have different runtime ownership rules
/// depending on whether they are immediate, static, fixed-size, dynamically
/// sized, tagged, or unmanaged.
type RcShape =
    | Immediate
    | FixedBlock of payloadSize:int * fieldShapes:RcShape list
    | StreamRoot
    | BoxedSum of payloadSize:int * fieldShapes:(int * RcShape) list * variants:RcBoxedSumVariantShape list
    | RecursiveNominalRef of sourceType:AST.SemanticType
    | TaggedListShape of elementShape:RcShape
    | DictRoot of keyShape:RcShape * valueShape:RcShape
    | DynamicString
    | DynamicBlob
    | DynamicInt
    | ClosureShape of captureShapes:RcShape list
    | StaticString
    | RawUnmanaged
and RcBoxedSumVariantShape = {
    Tag: int
    FieldShapes: (int * RcShape) list
}

/// Minimal sum metadata needed by RcShape without depending on later IR modules.
type RcSumShapeInfo = {
    TypeParams: string list
    Payloads: (int * AST.SemanticType option) list
}

type RcSumShapeRegistry = Map<string, RcSumShapeInfo>

/// Root-level retain/release operation selected from a runtime shape.
type RcOperation =
    | FixedSizeRoot of payloadSize:int * kind:RcKind
    | DynamicStringBuffer
    | DynamicBlobBuffer
    | DynamicIntBuffer

/// High-level storage management class selected from a runtime shape.
type RcStorageClass =
    | UnmanagedStorage
    | ManagedDynamicBuffer of operation:RcOperation
    | ManagedRcRoot of payloadSize:int * kind:RcKind

/// Structured release plan selected from a runtime shape.
///
/// Backends can consume this instead of rediscovering nested ownership by
/// pattern matching on source types. RootRelease describes the refcounted root;
/// the nested payload plan describes extra work that must happen only when the
/// root refcount reaches zero.
type RcReleasePlan =
    | NoReleasePlan
    | DynamicBufferRelease of operation:RcOperation
    | RecursiveRelease of sourceType:AST.SemanticType
    | RootRelease of payloadSize:int * kind:RcKind * payload:RcPayloadReleasePlan
and RcPayloadReleasePlan =
    | NoPayloadRelease
    | FixedBlockPayloadRelease of payloadSize:int * fieldReleases:RcFieldRelease list
    | BoxedSumPayloadRelease of payloadSize:int * fieldReleases:RcFieldRelease list * variants:RcBoxedSumVariantRelease list
    | TaggedListPayloadRelease of elementRelease:RcReleasePlan
    | DictPayloadRelease of keyRelease:RcReleasePlan * valueRelease:RcReleasePlan
    | ClosurePayloadRelease of captureReleases:RcFieldRelease list
and RcFieldRelease =
    | FieldRelease of offset:int * release:RcReleasePlan
and RcBoxedSumVariantRelease = {
    Tag: int
    FieldReleases: RcFieldRelease list
}

/// Metadata carried by refcount operations after ownership insertion.
///
/// ReleasePlan is the backend-facing source of truth for retain/release helper
/// selection. SourceType is retained as contextual metadata for diagnostics and
/// focused compiler-pass tests; backend cleanup must not reconstruct release
/// behavior from it.
type RcMetadata = {
    /// Stable memo key derived from the canonical source type when available.
    /// Backends use it to avoid comparing expanded release plans. Helper names
    /// remain plan-derived so equivalent shapes continue to share code.
    ReleasePlanCacheKey: string option
    ReleasePlan: RcReleasePlan option
    SourceType: AST.SemanticType option
}
