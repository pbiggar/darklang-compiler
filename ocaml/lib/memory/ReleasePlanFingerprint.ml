(* ReleasePlanFingerprint.fs - ANF-independent memory representation and release contracts. *)
open MemoryModel
type rcReleasePlanFingerprintState = {mutable hash : int64}
let newRcReleasePlanFingerprintState () = {hash=0xcbf29ce484222325L}
let addRcReleasePlanFingerprintByte state value = state.hash <- Int64.mul (Int64.logxor state.hash (Int64.of_int value)) 1099511628211L
let addRcReleasePlanFingerprintInt = addRcReleasePlanFingerprintByte
let addRcReleasePlanFingerprintString state value =
 let units = HostText.utf16Units value in
 addRcReleasePlanFingerprintInt state (Array.length units);
 Array.iter (addRcReleasePlanFingerprintInt state) units
let addRcReleasePlanFingerprintKind state kind = addRcReleasePlanFingerprintByte state (match kind with GenericHeap -> 0 | StreamHeap -> 1 | TaggedList -> 2 | DictHeap -> 3 | ClosureHeap -> 4)
let finishRcReleasePlanFingerprint state = Printf.sprintf "%016Lx" state.hash
(*
   Deterministic identity for an RC source type. A compilation has one record
   and sum registry, so a canonical source type uniquely selects its release
   plan without traversing the registry-expanded plan itself.
*)
let rcSourceTypeFingerprint sourceType =
 let state = newRcReleasePlanFingerprintState () in
 let addByte = addRcReleasePlanFingerprintByte state and addInt = addRcReleasePlanFingerprintInt state and addString = addRcReleasePlanFingerprintString state in
 let rec addType = function
 | AST.TInt8 -> addByte 0 | AST.TInt16 -> addByte 1 | AST.TInt32 -> addByte 2 | AST.TInt64 -> addByte 3 | AST.TInt128 -> addByte 4 | AST.TInt -> addByte 5
 | AST.TUInt8 -> addByte 6 | AST.TUInt16 -> addByte 7 | AST.TUInt32 -> addByte 8 | AST.TUInt64 -> addByte 9 | AST.TUInt128 -> addByte 10
 | AST.TBool -> addByte 11 | AST.TFloat64 -> addByte 12 | AST.TString -> addByte 13 | AST.TBlob -> addByte 14 | AST.TChar -> addByte 15 | AST.TDateTime -> addByte 16 | AST.TUnit -> addByte 17 | AST.TNever -> addByte 18
 | AST.TFunction (parameters,result) -> addByte 19;addTypes parameters;addType result
 | AST.TTuple elements -> addByte 20;addTypes elements
 | AST.TRecord (name,args) -> addByte 22;addString name;addTypes args
 | AST.TSum (name,args) -> addByte 23;addString name;addTypes args
 | AST.TList element -> addByte 24;addType element
 | AST.TStream element -> addByte 25;addType element
 | AST.TVar name -> addByte 26;addString name
 | AST.TInferenceVar (name,identity) -> addByte 29;addString name;addString identity
 | AST.TInternalRawPtr -> addByte 27
 | AST.TDict (key,value) -> addByte 28;addType key;addType value
 and addTypes types = addInt (List.length types);List.iter addType types in
 addType sourceType;finishRcReleasePlanFingerprint state
(*
   Build one release-plan fingerprint node from already-fingerprinted direct
   children. Keeping the hash compositional lets consumers that already walk
   a plan compute every nested helper identity in one bottom-up pass.
*)
let rcReleasePlanFingerprintHashFromChildren releasePlan childFingerprints =
 let state = newRcReleasePlanFingerprintState () in
 let addByte = addRcReleasePlanFingerprintByte state and addInt = addRcReleasePlanFingerprintInt state and addString = addRcReleasePlanFingerprintString state and addKind = addRcReleasePlanFingerprintKind state in
 let addFields fields = addInt (List.length fields);List.iter (fun (FieldRelease (offset,_)) -> addInt offset) fields in
 let addPayload = function
 | NoPayloadRelease -> addByte 0
 | FixedBlockPayloadRelease (size,fields) -> addByte 1;addInt size;addFields fields
 | BoxedSumPayloadRelease (size,fields,variants) -> addByte 2;addInt size;addFields fields;addInt (List.length variants);List.iter (fun (variant : rcBoxedSumVariantRelease) -> addInt variant.tag;addFields variant.fieldReleases) variants
 | TaggedListPayloadRelease _ -> addByte 3
 | DictPayloadRelease _ -> addByte 4
 | ClosurePayloadRelease fields -> addByte 5;addFields fields in
 (match releasePlan with
 | NoReleasePlan -> addByte 0
 | DynamicBufferRelease operation -> addByte 1;(match operation with FixedSizeRoot (size,kind) -> addByte 0;addInt size;addKind kind | DynamicStringBuffer -> addByte 1 | DynamicBlobBuffer -> addByte 2 | DynamicIntBuffer -> addByte 3)
 | RecursiveRelease typ -> addByte 2;addString (rcSourceTypeFingerprint typ)
 | RootRelease (size,kind,payload) -> addByte 3;addInt size;addKind kind;addPayload payload);
 addInt (List.length childFingerprints);
 List.fold_left (fun hash child -> Int64.mul (Int64.logxor hash child) 1099511628211L) state.hash childFingerprints
let rcReleasePlanFingerprintString fingerprint = Printf.sprintf "%016Lx" fingerprint
let rec rcReleasePlanFingerprintHash releasePlan =
 let children = match releasePlan with
 | RootRelease (_,_,payload) ->
  let fieldsChildren fields = List.map (fun (FieldRelease (_,plan)) -> rcReleasePlanFingerprintHash plan) fields in
  (match payload with
  | NoPayloadRelease -> []
  | FixedBlockPayloadRelease (_,fields) | ClosurePayloadRelease fields -> fieldsChildren fields
  | BoxedSumPayloadRelease (_,fields,variants) ->
   let fieldChildren = fieldsChildren fields in
   let variantChildren = List.concat_map (fun (variant : rcBoxedSumVariantRelease) -> fieldsChildren variant.fieldReleases) variants in fieldChildren @ variantChildren
  | TaggedListPayloadRelease element -> [rcReleasePlanFingerprintHash element]
  | DictPayloadRelease (key,value) -> let key = rcReleasePlanFingerprintHash key in let value = rcReleasePlanFingerprintHash value in [key;value])
 | NoReleasePlan | DynamicBufferRelease _ | RecursiveRelease _ -> [] in
 rcReleasePlanFingerprintHashFromChildren releasePlan children
(*
   Deterministic allocation-light identity for a release plan.
*)
let rcReleasePlanFingerprint releasePlan = rcReleasePlanFingerprintString (rcReleasePlanFingerprintHash releasePlan)
(*
   True once a release plan contains more than the requested number of nodes.
   Traversal stops at the limit so callers can cheaply choose a compact memo
   key without fully walking an already-large plan.
*)
let rcReleasePlanExceedsNodeCount nodeLimit releasePlan =
 let nodes = ref 0 in
 let visitNode () = nodes := Int32.to_int (Int32.add (Int32.of_int !nodes) 1l);!nodes > nodeLimit in
 let rec visitPlan plan = visitNode () || match plan with RootRelease (_,_,payload) -> visitPayload payload | NoReleasePlan | DynamicBufferRelease _ | RecursiveRelease _ -> false
 and visitFields = function [] -> false | FieldRelease (_,plan)::rest -> visitNode () || visitPlan plan || visitFields rest
 and visitVariants = function [] -> false | variant::rest -> visitNode () || visitFields variant.fieldReleases || visitVariants rest
 and visitPayload payload = visitNode () || match payload with
 | NoPayloadRelease -> false
 | FixedBlockPayloadRelease (_,fields) | ClosurePayloadRelease fields -> visitFields fields
 | BoxedSumPayloadRelease (_,fields,variants) -> visitFields fields || visitVariants variants
 | TaggedListPayloadRelease element -> visitPlan element
 | DictPayloadRelease (key,value) -> visitPlan key || visitPlan value in
 visitPlan releasePlan
let rcReleasePlanCompactKeyNodeThreshold = 24
(*
   Compact key only for plans large enough that structural map comparisons are
   measurably more expensive than hashing their canonical source type.
*)
let rcReleasePlanCacheKey sourceType releasePlan = if rcReleasePlanExceedsNodeCount rcReleasePlanCompactKeyNodeThreshold releasePlan then Some (rcSourceTypeFingerprint sourceType) else None
