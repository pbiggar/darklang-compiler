[@@@warning "-4"]
(*
   MemoryPlanning.ml - ANF-independent memory representation and release contracts.
*)
open AST
open! MemoryModel
module M = StringOrder.Map
module S = StringOrder.Set
module SemanticTypeSet = Set.Make (struct type t = AST.semanticType let compare = AST.compareSemanticType end)
type recordRegistry = (string * AST.semanticType) list M.t
(*
   Source types with a constructible, one-word native root. Keep this list
   explicit so a future multiword representation cannot become transparent by
   default when its source type is added.
*)
let canUseTransparentSumPayload = function
 | TNever | TVar _ | TInferenceVar _ -> false
 | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
 | TBool | TFloat64 | TDateTime | TUnit | TString | TChar | TBlob | TTuple _ | TRecord _ | TSum _
 | TList _ | TDict _ | TStream _ | TFunction _ | TInternalRawPtr -> true
let sortedPayloads (info : rcSumShapeInfo) =
 List.stable_sort (fun (_, left) (_, right) -> match left, right with
 | None, None -> 0 | None, Some _ -> -1 | Some _, None -> 1
 | Some left, Some right -> AST.compareSemanticType left right) info.payloads
let nullablePointerSumPayloadType (sumReg : rcSumShapeRegistry) = function
 | TSum (name, args) -> (match M.find_opt name sumReg with
   | Some info when List.length info.typeParams = List.length args ->
     let subst = M.of_list (List.combine info.typeParams args) in
     (match sortedPayloads info with
      | [(_, None); (tag, Some template)] when IntSet.mem tag info.unaryPayloadTags ->
        let payload = match template with
         | TVar name -> (match M.find_opt name subst with Some value -> value | None -> Crash.crash ("Nullable sum payload variable '" ^ name ^ "' is not declared"))
         | value -> value in
        (match payload with TString | TChar | TBlob | TInt128 | TUInt128 | TTuple _ | TRecord _ -> Some payload | _ -> None)
      | _ -> None)
   | _ -> None)
 | _ -> None
let isNullablePointerSumType registry typ = Option.is_some (nullablePointerSumPayloadType registry typ)
let isSpareImmediateSumType (sumReg : rcSumShapeRegistry) = function
 | TSum (name, args) -> (match M.find_opt name sumReg with
   | Some info when List.length info.typeParams = List.length args ->
     let subst = M.of_list (List.combine info.typeParams args) in
     (match sortedPayloads info with
      | [(_, None); (tag, Some template)] when IntSet.mem tag info.unaryPayloadTags ->
        let payload = match template with TVar name -> M.find_opt name subst | value -> Some value in
        (match payload with Some (TUnit | TBool | TInt8 | TUInt8 | TInt16 | TUInt16 | TInt32 | TUInt32) -> true | _ -> false)
      | _ -> false)
   | _ -> false)
 | _ -> false
(*
   Classify a source type into its current runtime RC representation shape.
   The classifier is intentionally pure and side-effect free. Ownership
   insertion and backend helper selection use this as the source of truth for
   runtime retain/release shape decisions.
   Arbitrary Int uses tagged immediates or a limb buffer. Fixed-width 128-bit
   values are immutable two-limb blocks with the refcount after the payload.
*)
let rcShapeOfType registry typ =
 let rec classify expanding = function
 | TInt8 | TInt16 | TInt32 | TInt64 | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TBool | TFloat64 | TDateTime | TUnit | TNever | TVar _ | TInferenceVar _ -> Immediate
 | TInt -> DynamicInt | TInt128 | TUInt128 -> FixedBlock (16, [])
 | TTuple types -> FixedBlock (List.length types * 8, List.map (classify expanding) types)
 | TRecord (name, _) as source ->
   if SemanticTypeSet.mem source expanding then RecursiveNominalRef source else
   (match M.find_opt name registry with
    | Some fields -> let expanding = SemanticTypeSet.add source expanding in FixedBlock (List.length fields * 8, List.map (fun (_, value) -> classify expanding value) fields)
    | None -> Crash.crash ("rcShapeOfType: Record type '" ^ name ^ "' not found in typeReg"))
 | TSum (_, []) -> Immediate
 | TSum (_, [payload]) -> BoxedSum (16, [8, classify expanding payload], [])
 | TSum _ -> BoxedSum (16, [], [])
 | TList value -> TaggedListShape (classify expanding value)
 | TStream _ -> StreamRoot | TDict (key, value) -> DictRoot (classify expanding key, classify expanding value)
 | TString | TChar -> DynamicString | TBlob -> DynamicBlob | TFunction _ -> ClosureShape [] | TInternalRawPtr -> RawUnmanaged in
 classify SemanticTypeSet.empty typ
let rcShapeTypeSubstitution parameters arguments =
 if parameters = [] then M.empty
 else if List.length parameters = List.length arguments then M.of_list (List.combine parameters arguments)
 else Crash.crash (Printf.sprintf "rcShapeOfTypeWithSums: sum type argument mismatch: params=%d, args=%d" (List.length parameters) (List.length arguments))
let collectTypeVarsInOrder typ =
 let rec collect = function
 | TVar name | TInferenceVar (_, name) -> [name]
 | TTuple types | TRecord (_, types) | TSum (_, types) -> List.concat_map collect types
 | TList value | TStream value -> collect value | TDict (key, value) -> collect key @ collect value
 | TFunction (parameters, result) -> List.concat_map collect parameters @ collect result
 | _ -> [] in
 let _, values = List.fold_left (fun (seen, values) value -> if S.mem value seen then seen, values else S.add value seen, value :: values) (S.empty, []) (collect typ) in List.rev values
let inferredRecordTypeParamsRegistry registry =
 M.map (fun fields -> let _, values = List.fold_left (fun (seen, values) value -> if S.mem value seen then seen, values else S.add value seen, value :: values) (S.empty, []) (List.concat_map (fun (_, typ) -> collectTypeVarsInOrder typ) fields) in List.rev values) registry
let rec applyRcShapeTypeSubstitution subst typ = match typ with
 | TVar name | TInferenceVar (_, name) -> Option.value (M.find_opt name subst) ~default:typ
 | TTuple values -> TTuple (List.map (applyRcShapeTypeSubstitution subst) values)
 | TRecord (name, values) -> TRecord (name, List.map (applyRcShapeTypeSubstitution subst) values)
 | TSum (name, values) -> TSum (name, List.map (applyRcShapeTypeSubstitution subst) values)
 | TList value -> TList (applyRcShapeTypeSubstitution subst value) | TStream value -> TStream (applyRcShapeTypeSubstitution subst value)
 | TDict (key, value) -> TDict (applyRcShapeTypeSubstitution subst key, applyRcShapeTypeSubstitution subst value)
 | TFunction (parameters, result) -> TFunction (List.map (applyRcShapeTypeSubstitution subst) parameters, applyRcShapeTypeSubstitution subst result)
 | _ -> typ
(*
   Classify a source type using record metadata and optional named-sum metadata.
   Bare nominal references are parsed before constructor
   metadata is available. Classify the equivalent internal sum
   spelling here so recursive JSON trees retain correctly.
   These canonical buffers share a header; the
   dynamic-int operation also skips the zero word.
*)
let rcShapeOfTypeWithSums registry recordParams (sumReg : rcSumShapeRegistry) typ =
 let rec classify expanding = function
 | TTuple types -> FixedBlock (List.length types * 8, List.map (classify expanding) types)
 | TRecord (name, args) as source ->
   if SemanticTypeSet.mem source expanding then RecursiveNominalRef source else
   (match M.find_opt name registry with
    | Some fields ->
      let parameters = match M.find_opt name recordParams with Some values -> values | None -> Crash.crash ("rcShapeOfTypeWithSums: Record metadata '" ^ name ^ "' not found") in
      let subst = rcShapeTypeSubstitution parameters args in
      let expanding = SemanticTypeSet.add source expanding in
      FixedBlock (List.length fields * 8, List.map (fun (_, value) -> classify expanding (applyRcShapeTypeSubstitution subst value)) fields)
    | None when M.mem name sumReg -> classify expanding (TSum (name, args))
    | None -> Crash.crash ("rcShapeOfTypeWithSums: Record type '" ^ name ^ "' not found in typeReg"))
 | TSum (name, args) as source ->
   if SemanticTypeSet.mem source expanding then RecursiveNominalRef source else
   (match M.find_opt name sumReg with
    | Some info ->
      let subst = rcShapeTypeSubstitution info.typeParams args in
      let expanding = SemanticTypeSet.add source expanding in
      let concrete value = applyRcShapeTypeSubstitution subst value in
      let transparent = match info.payloads with
       | [(tag, Some value)] when IntSet.mem tag info.unaryPayloadTags ->
         let value = concrete value in if canUseTransparentSumPayload value then Some (classify expanding value) else None
       | _ -> None in
      let nullable = if Option.is_some transparent then None else match nullablePointerSumPayloadType sumReg source with
       | Some (TString | TChar | TBlob) -> Some DynamicInt | Some value -> Some (classify expanding (concrete value)) | None -> None in
      let spareList = if Option.is_some transparent || Option.is_some nullable then None else match sortedPayloads info with
       | [(_, None); (tag, Some value)] when IntSet.mem tag info.unaryPayloadTags ->
         (match concrete value with TList _ as value -> Some (classify expanding value) | _ -> None)
       | _ -> None in
      let chosen = match transparent with Some _ -> transparent | None -> (match nullable with Some _ -> nullable | None -> spareList) in
      (match chosen with
       | Some value -> value
       | None when isSpareImmediateSumType sumReg source -> Immediate
       | None when List.exists (fun (_, value) -> Option.is_some value) info.payloads ->
         let variants = List.map (fun (tag, value) -> ({tag; fieldShapes = match value with None -> [] | Some value -> [8, classify expanding (concrete value)]} : rcBoxedSumVariantShape)) info.payloads in
         BoxedSum (16, List.concat_map (fun (value : rcBoxedSumVariantShape) -> value.fieldShapes) variants, variants)
       | None -> Immediate)
    | None when M.mem name registry -> classify expanding (TRecord (name, args))
    | None -> Crash.crash ("rcShapeOfTypeWithSums: Sum type '" ^ name ^ "' not found in sumReg"))
 | TList value -> TaggedListShape (classify expanding value) | TStream _ -> StreamRoot
 | TDict (key, value) -> DictRoot (classify expanding key, classify expanding value) | TFunction _ -> ClosureShape []
 | TString | TChar -> DynamicString | TInt -> DynamicInt | TInt128 | TUInt128 -> FixedBlock (16, [])
 | TBlob -> DynamicBlob | TInternalRawPtr -> RawUnmanaged
 | TInt8 | TInt16 | TInt32 | TInt64 | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TBool | TFloat64 | TDateTime | TUnit | TNever | TVar _ | TInferenceVar _ -> Immediate in
 classify SemanticTypeSet.empty typ
(*
   True when a runtime shape can own managed memory that must be released when
   an owning binding leaves scope.
*)
let rcShapeNeedsOwnedScopeRelease = function Immediate | StaticString | RawUnmanaged -> false | _ -> true
(*
   True when a shape is managed through a fixed-size or tagged RC root rather
   than a dynamic-buffer helper or an unmanaged representation.
*)
let rcShapeIsRootManaged = function FixedBlock _ | StreamRoot | BoxedSum _ | RecursiveNominalRef _ | TaggedListShape _ | DictRoot _ | ClosureShape _ -> true | _ -> false
(*
   True when releasing a value of this shape can require walking owned payload
   fields, captures, list leaves, or dict leaf entries in addition to releasing
   the root allocation itself.
*)
let rcShapeNeedsRecursiveRelease = function
 | FixedBlock (_, fields) | ClosureShape fields -> List.exists rcShapeNeedsOwnedScopeRelease fields
 | StreamRoot | RecursiveNominalRef _ -> true
 | BoxedSum (_, fields, _) -> List.exists (fun (_, value) -> rcShapeNeedsOwnedScopeRelease value) fields
 | TaggedListShape value -> rcShapeNeedsOwnedScopeRelease value
 | DictRoot (key, value) -> rcShapeNeedsOwnedScopeRelease key || rcShapeNeedsOwnedScopeRelease value
 | _ -> false
(*
   Dispatch kind for fixed-size/tagged RC roots. Dynamic buffers use their own
   string/bytes operations, so they intentionally do not have a root kind here.
*)
let rcShapeRootKind = function FixedBlock _ | BoxedSum _ | RecursiveNominalRef _ -> Some GenericHeap | StreamRoot -> Some StreamHeap | TaggedListShape _ -> Some TaggedList | DictRoot _ -> Some DictHeap | ClosureShape _ -> Some ClosureHeap | _ -> None
(*
   Payload size for fixed-size/tagged RC roots.
*)
let rcShapePayloadSize = function FixedBlock (size, _) | BoxedSum (size, _, _) -> Some size | StreamRoot | TaggedListShape _ -> Some 24 | RecursiveNominalRef _ -> Some 16 | DictRoot _ -> Some 8 | ClosureShape _ -> Some 0 | _ -> None
(*
   Storage class for deciding whether a value is unmanaged, managed by a
   dynamic-buffer helper, or managed by a fixed/tagged RC root helper.
*)
let rcShapeStorageClass shape = match shape with
 | DynamicString -> ManagedDynamicBuffer DynamicStringBuffer | DynamicBlob -> ManagedDynamicBuffer DynamicBlobBuffer | DynamicInt -> ManagedDynamicBuffer DynamicIntBuffer
 | _ -> (match rcShapePayloadSize shape, rcShapeRootKind shape with Some size, Some kind -> ManagedRcRoot (size, kind) | _ -> UnmanagedStorage)
(*
   True when a value is represented by an RC root whose ownership can be
   transferred to another aggregate or helper call.
*)
let rcShapeIsOwnershipTransferRoot shape = match rcShapeStorageClass shape with ManagedRcRoot _ -> true | _ -> false
(*
   Retain operation for an owned or borrowed value of the given shape.
*)
let rcShapeRetainOperation shape = match rcShapeStorageClass shape with ManagedDynamicBuffer operation -> Some operation | ManagedRcRoot (size, kind) -> Some (FixedSizeRoot (size, kind)) | UnmanagedStorage -> None
(*
   Release operation for an owned value of the given shape.
*)
let rcShapeReleaseOperation shape = if rcShapeNeedsOwnedScopeRelease shape then rcShapeRetainOperation shape else None
(*
   True when a borrowed value of this shape needs a retain before it can be
   returned or otherwise materialized as a new owned value.
*)
let rcShapeNeedsBorrowedRetain shape = Option.is_some (rcShapeRetainOperation shape)
(*
   True when a normal owning binding of this shape should receive an automatic
   decrement from RC insertion. Closure roots are handled by closure-producing
   expressions so aliases of function-typed values do not double-release.
*)
let rcShapeNeedsAutomaticBindingDec = function ClosureShape _ -> false | shape -> rcShapeNeedsOwnedScopeRelease shape
(*
   True when a borrowed alias of this shape carries a managed root identity
   that should be preserved by type inference. Closure aliases are excluded
   because closure-producing expressions own their lifetime separately.
*)
let rcShapeNeedsManagedAliasRootPreservation shape = match rcShapeStorageClass shape with ManagedRcRoot (_, ClosureHeap) -> false | ManagedRcRoot _ -> true | _ -> false
(*
   Release plan for a value with the given runtime shape.
*)
let rec rcShapeReleasePlan shape =
 let atOffsets fields = List.filter_map (fun (offset, shape) -> match rcShapeReleasePlan shape with NoReleasePlan -> None | plan -> Some (FieldRelease (offset, plan))) fields in
 let fieldPlans fields = atOffsets (List.mapi (fun index value -> index * 8, value) fields) in
 let payload = function
  | FixedBlock (size, fields) -> FixedBlockPayloadRelease (size, fieldPlans fields)
  | StreamRoot -> FixedBlockPayloadRelease (24, fieldPlans [Immediate; ClosureShape []; ClosureShape []])
  | BoxedSum (size, fields, variants) ->
    let variants = List.map (fun (variant : rcBoxedSumVariantShape) -> ({tag = variant.tag; fieldReleases = atOffsets variant.fieldShapes} : rcBoxedSumVariantRelease)) variants in
    BoxedSumPayloadRelease (size, atOffsets fields, variants)
  | TaggedListShape value -> TaggedListPayloadRelease (rcShapeReleasePlan value)
  | DictRoot (key, value) -> DictPayloadRelease (rcShapeReleasePlan key, rcShapeReleasePlan value)
  | ClosureShape fields -> ClosurePayloadRelease (fieldPlans fields) | _ -> NoPayloadRelease in
 match rcShapeStorageClass shape with
 | ManagedRcRoot (size, kind) -> (match shape with RecursiveNominalRef typ -> RecursiveRelease typ | _ -> RootRelease (size, kind, payload shape))
 | UnmanagedStorage -> NoReleasePlan | ManagedDynamicBuffer operation -> DynamicBufferRelease operation
(*
   Release plan for a source type using the current representation registry.
*)
let rcReleasePlanOfType registry typ = rcShapeReleasePlan (rcShapeOfType registry typ)
(*
   Release plan for a source type using record and named-sum metadata.
*)
let rcReleasePlanOfTypeWithSums registry sums typ = rcShapeReleasePlan (rcShapeOfTypeWithSums registry (inferredRecordTypeParamsRegistry registry) sums typ)
(*
   Collect the concrete recursive nominal roots referenced by a finite release plan.
*)
let rec recursiveReleaseTypes plan =
 let fromFields fields = List.fold_left (fun result (FieldRelease (_, plan)) -> SemanticTypeSet.union result (recursiveReleaseTypes plan)) SemanticTypeSet.empty fields in
 match plan with
 | RecursiveRelease typ -> SemanticTypeSet.singleton typ
 | RootRelease (_, _, FixedBlockPayloadRelease (_, fields)) | RootRelease (_, _, BoxedSumPayloadRelease (_, fields, _)) | RootRelease (_, _, ClosurePayloadRelease fields) -> fromFields fields
 | RootRelease (_, _, TaggedListPayloadRelease value) -> recursiveReleaseTypes value
 | RootRelease (_, _, DictPayloadRelease (key, value)) -> SemanticTypeSet.union (recursiveReleaseTypes key) (recursiveReleaseTypes value)
 | RootRelease (_, _, NoPayloadRelease) | DynamicBufferRelease _ | NoReleasePlan -> SemanticTypeSet.empty
