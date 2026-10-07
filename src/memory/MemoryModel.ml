(*
   MemoryModel.ml - ANF-independent memory representation and release contracts.
*)
(* ANF-independent memory representation and release contracts. *)
(*
   Immutable canonical byte-buffer representations whose semantic equality is
   byte equality. The kind preserves the source-level reason that the
   representation comparison is valid instead of conflating integers with
   strings after lowering.
*)
type canonicalBufferKind =
  | Utf8String
  | NullableUtf8String
  | GraphemeCluster
  | NullableGraphemeCluster

(*
   Reference-count operation kind
*)
type rcKind = GenericHeap | StreamHeap | TaggedList | DictHeap | ClosureHeap

(*
   Runtime representation shape used to decide ownership behavior.
   This is deliberately more specific than source-level heap-ness: values with
   the same source type category can have different runtime ownership rules
   depending on whether they are immediate, static, fixed-size, dynamically
   sized, tagged, or unmanaged.
*)
type rcShape =
  | Immediate
  | FixedBlock of int * rcShape list
  | StreamRoot
  | BoxedSum of int * (int * rcShape) list * rcBoxedSumVariantShape list
  | RecursiveNominalRef of AST.semanticType
  | TaggedListShape of rcShape
  | DictRoot of rcShape * rcShape
  | DynamicString
  | DynamicBlob
  | DynamicInt
  | ClosureShape of rcShape list
  | StaticString
  | RawUnmanaged

and rcBoxedSumVariantShape = { tag : int; fieldShapes : (int * rcShape) list }

module IntSet = Set.Make (Int)

(*
   Minimal sum metadata needed by RcShape without depending on later IR modules.
*)
type rcSumShapeInfo = {
  typeParams : string list;
  payloads : (int * AST.semanticType option) list;
  unaryPayloadTags : IntSet.t;
}

type rcSumShapeRegistry = rcSumShapeInfo StringOrder.Map.t

(*
   Root-level retain/release operation selected from a runtime shape.
*)
type rcOperation =
  | FixedSizeRoot of int * rcKind
  | DynamicStringBuffer
  | DynamicBlobBuffer
  | DynamicIntBuffer

(*
   High-level storage management class selected from a runtime shape.
*)
type rcStorageClass =
  | UnmanagedStorage
  | ManagedDynamicBuffer of rcOperation
  | ManagedRcRoot of int * rcKind

(*
   Structured release plan selected from a runtime shape.
   Backends can consume this instead of rediscovering nested ownership by
   pattern matching on source types. RootRelease describes the refcounted root;
   the nested payload plan describes extra work that must happen only when the
   root refcount reaches zero.
*)
type rcReleasePlan =
  | NoReleasePlan
  | DynamicBufferRelease of rcOperation
  | RecursiveRelease of AST.semanticType
  | RootRelease of int * rcKind * rcPayloadReleasePlan

and rcPayloadReleasePlan =
  | NoPayloadRelease
  | FixedBlockPayloadRelease of int * rcFieldRelease list
  | BoxedSumPayloadRelease of
      int * rcFieldRelease list * rcBoxedSumVariantRelease list
  | TaggedListPayloadRelease of rcReleasePlan
  | DictPayloadRelease of rcReleasePlan * rcReleasePlan
  | ClosurePayloadRelease of rcFieldRelease list

and rcFieldRelease = FieldRelease of int * rcReleasePlan

and rcBoxedSumVariantRelease = {
  tag : int;
  fieldReleases : rcFieldRelease list;
}

(*
   Metadata carried by refcount operations after ownership insertion.
   ReleasePlan is the backend-facing source of truth for retain/release helper
   selection. SourceType is retained as contextual metadata for diagnostics and
   focused compiler-pass tests; backend cleanup must not reconstruct release
   behavior from it.
   Stable memo key derived from the canonical source type when available.
   Backends use it to avoid comparing expanded release plans. Helper names
   remain plan-derived so equivalent shapes continue to share code.
*)
type rcMetadata = {
  releasePlanCacheKey : string option;
  releasePlan : rcReleasePlan option;
  sourceType : AST.semanticType option;
}
