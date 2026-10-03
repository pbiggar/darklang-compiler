(* ANF-independent memory representation and release contracts. *)
type canonicalBufferKind = Utf8String | NullableUtf8String | GraphemeCluster | NullableGraphemeCluster
type rcKind = GenericHeap | StreamHeap | TaggedList | DictHeap | ClosureHeap
type rcShape = Immediate | FixedBlock of int * rcShape list | StreamRoot
 | BoxedSum of int * (int * rcShape) list * rcBoxedSumVariantShape list
 | RecursiveNominalRef of AST.semanticType | TaggedListShape of rcShape
 | DictRoot of rcShape * rcShape | DynamicString | DynamicBlob | DynamicInt
 | ClosureShape of rcShape list | StaticString | RawUnmanaged
and rcBoxedSumVariantShape = {tag : int; fieldShapes : (int * rcShape) list}
module IntSet : Set.S with type elt = int
type rcSumShapeInfo = {typeParams : string list; payloads : (int * AST.semanticType option) list; unaryPayloadTags : IntSet.t}
type rcSumShapeRegistry = rcSumShapeInfo StringOrder.Map.t
type rcOperation = FixedSizeRoot of int * rcKind | DynamicStringBuffer | DynamicBlobBuffer | DynamicIntBuffer
type rcStorageClass = UnmanagedStorage | ManagedDynamicBuffer of rcOperation | ManagedRcRoot of int * rcKind
type rcReleasePlan = NoReleasePlan | DynamicBufferRelease of rcOperation | RecursiveRelease of AST.semanticType | RootRelease of int * rcKind * rcPayloadReleasePlan
and rcPayloadReleasePlan = NoPayloadRelease | FixedBlockPayloadRelease of int * rcFieldRelease list
 | BoxedSumPayloadRelease of int * rcFieldRelease list * rcBoxedSumVariantRelease list
 | TaggedListPayloadRelease of rcReleasePlan | DictPayloadRelease of rcReleasePlan * rcReleasePlan
 | ClosurePayloadRelease of rcFieldRelease list
and rcFieldRelease = FieldRelease of int * rcReleasePlan
and rcBoxedSumVariantRelease = {tag : int; fieldReleases : rcFieldRelease list}
type rcMetadata = {releasePlanCacheKey : string option; releasePlan : rcReleasePlan option; sourceType : AST.semanticType option}
