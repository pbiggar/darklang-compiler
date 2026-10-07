(*
   ARM64ReleaseSelection.ml - Select backend helpers from explicit representation release plans.
*)
[@@@warning "-4"]

open ARM64CodeGenTypes
open ARM64ClosureReferenceCounts

let releasePlanIsRootKind (kind : MemoryModel.rcKind)
    (releasePlan : MemoryModel.rcReleasePlan) : bool =
  match releasePlan with
  | MemoryModel.RootRelease (_, planKind, _) when planKind = kind -> true
  | _ -> false

let releasePlanIsTaggedListWithElementRelease
    (elementPredicate : MemoryModel.rcReleasePlan -> bool)
    (releasePlan : MemoryModel.rcReleasePlan) : bool =
  match releasePlan with
  | MemoryModel.RootRelease
      ( _,
        MemoryModel.TaggedList,
        MemoryModel.TaggedListPayloadRelease elementRelease ) ->
      elementPredicate elementRelease
  | _ -> false

let releasePlanIsDictWithListValue (releasePlan : MemoryModel.rcReleasePlan) :
    bool =
  match releasePlan with
  | MemoryModel.RootRelease
      ( _,
        MemoryModel.DictHeap,
        MemoryModel.DictPayloadRelease
          (_, MemoryModel.RootRelease (_, MemoryModel.TaggedList, _)) ) ->
      true
  | _ -> false

let releasePlanIsDynamicBufferOperation (operation : MemoryModel.rcOperation)
    (releasePlan : MemoryModel.rcReleasePlan) : bool =
  match releasePlan with
  | MemoryModel.DynamicBufferRelease planOperation
    when planOperation = operation ->
      true
  | _ -> false

let releasePlanFieldHasRelease (fieldOffset : int)
    (predicate : MemoryModel.rcReleasePlan -> bool)
    (fieldReleases : MemoryModel.rcFieldRelease list) : bool =
  fieldReleases
  |> List.exists (function
    | MemoryModel.FieldRelease (offset, releasePlan) when offset = fieldOffset
      ->
        predicate releasePlan
    | _ -> false)

let releasePlanIsFixedBlockWithFieldReleases (payloadSize : int)
    (expectedFieldReleases : (int * (MemoryModel.rcReleasePlan -> bool)) list)
    (releasePlan : MemoryModel.rcReleasePlan) : bool =
  match releasePlan with
  | MemoryModel.RootRelease
      ( _,
        MemoryModel.GenericHeap,
        MemoryModel.FixedBlockPayloadRelease (planPayloadSize, fieldReleases) )
    when planPayloadSize = payloadSize ->
      expectedFieldReleases
      |> List.for_all (fun (fieldOffset, predicate) ->
          releasePlanFieldHasRelease fieldOffset predicate fieldReleases)
  | _ -> false

let releasePlanIsSingleListDictFieldPayload
    (releasePlan : MemoryModel.rcReleasePlan) : bool =
  releasePlan
  |> releasePlanIsFixedBlockWithFieldReleases 8
       [
         ( 0,
           releasePlanIsTaggedListWithElementRelease
             (releasePlanIsRootKind MemoryModel.DictHeap) );
       ]
[@@warning "-32"]

let releasePlanIsThreeManagedFieldPayload
    (releasePlan : MemoryModel.rcReleasePlan) : bool =
  releasePlan
  |> releasePlanIsFixedBlockWithFieldReleases 24
       [
         (0, releasePlanIsDynamicBufferOperation MemoryModel.DynamicStringBuffer);
         (8, releasePlanIsRootKind MemoryModel.TaggedList);
         (16, releasePlanIsRootKind MemoryModel.DictHeap);
       ]
[@@warning "-32"]

let releasePlanIsTaggedListWithTuple2ElementFieldRelease
    (fieldPredicate : MemoryModel.rcReleasePlan -> bool)
    (releasePlan : MemoryModel.rcReleasePlan) : bool =
  releasePlan
  |> releasePlanIsTaggedListWithElementRelease (function
    | MemoryModel.RootRelease
        ( _,
          MemoryModel.GenericHeap,
          MemoryModel.FixedBlockPayloadRelease (16, fieldReleases) ) ->
        fieldReleases
        |> List.exists (function MemoryModel.FieldRelease (_, fieldRelease) ->
            fieldPredicate fieldRelease)
    | _ -> false)
[@@warning "-32"]

let releasePlanFieldReleaseAt (fieldOffset : int)
    (fieldReleases : MemoryModel.rcFieldRelease list) :
    MemoryModel.rcReleasePlan option =
  fieldReleases
  |> List.find_map (function
    | MemoryModel.FieldRelease (offset, releasePlan) when offset = fieldOffset
      ->
        Some releasePlan
    | _ -> None)

let releasePlanRootKindAt (fieldOffset : int) (kind : MemoryModel.rcKind)
    (fieldReleases : MemoryModel.rcFieldRelease list) : bool =
  match releasePlanFieldReleaseAt fieldOffset fieldReleases with
  | Some (MemoryModel.RootRelease (_, planKind, _)) when planKind = kind -> true
  | _ -> false

let releasePlanDynamicOperationAt (fieldOffset : int)
    (operation : MemoryModel.rcOperation)
    (fieldReleases : MemoryModel.rcFieldRelease list) : bool =
  match releasePlanFieldReleaseAt fieldOffset fieldReleases with
  | Some (MemoryModel.DynamicBufferRelease planOperation)
    when planOperation = operation ->
      true
  | _ -> false

let listDecHelperForElementRelease (elementFingerprint : string)
    (elementRelease : MemoryModel.rcReleasePlan) : string =
  match elementRelease with
  | MemoryModel.NoReleasePlan -> listRefCountDecHelperLabel
  | MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer ->
      listRefCountDecStringHelperLabel
  | MemoryModel.DynamicBufferRelease MemoryModel.DynamicBlobBuffer ->
      listRefCountDecBlobHelperLabel
  | MemoryModel.DynamicBufferRelease MemoryModel.DynamicIntBuffer ->
      listRefCountDecBlobHelperLabel
  | MemoryModel.DynamicBufferRelease _ ->
      Crash.crash "list dynamic-buffer release used a fixed-size operation"
  | MemoryModel.RecursiveRelease _ ->
      plannedListDecHelperLabelForFingerprint elementFingerprint
  | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
      plannedListDecHelperLabelForFingerprint elementFingerprint
  | MemoryModel.RootRelease
      ( _,
        MemoryModel.DictHeap,
        MemoryModel.DictPayloadRelease (MemoryModel.DynamicBufferRelease _, _)
      )
  | MemoryModel.RootRelease
      ( _,
        MemoryModel.DictHeap,
        MemoryModel.DictPayloadRelease (_, MemoryModel.DynamicBufferRelease _)
      ) ->
      plannedListDecHelperLabelForFingerprint elementFingerprint
  | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _)
    when releasePlanIsDictWithListValue elementRelease ->
      listRefCountDecDictListHelperLabel
  | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
      listRefCountDecDictHelperLabel
  | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
      listRefCountDecClosureHelperLabel
  | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) ->
      plannedListDecHelperLabelForFingerprint elementFingerprint
  | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, _) ->
      plannedListDecHelperLabelForFingerprint elementFingerprint

let listDecHelperForReleasePlan (releasePlan : MemoryModel.rcReleasePlan) :
    string =
  match releasePlan with
  | MemoryModel.RootRelease
      (_, _, MemoryModel.TaggedListPayloadRelease elementRelease) ->
      listDecHelperForElementRelease
        (ReleasePlanFingerprint.rcReleasePlanFingerprint elementRelease)
        elementRelease
  | _ -> listRefCountDecHelperLabel

let listDecHelperForType (ctx : codeGenContext) (sourceType : AST.semanticType)
    : string =
  match
    tryRcReleasePlanOfType ctx.recordRegistry ctx.sumShapeRegistry sourceType
  with
  | Some releasePlan -> listDecHelperForReleasePlan releasePlan
  | None ->
      Crash.crash
        ("listDecHelperForType: missing RC metadata for list element type "
        ^ StructuralFormat.semanticType sourceType
        ^ "")
[@@warning "-32"]

let dictPayloadReleaseNeedsPlannedHelper
    (keyRelease : MemoryModel.rcReleasePlan)
    (valueRelease : MemoryModel.rcReleasePlan) : bool =
  match (keyRelease, valueRelease) with
  | MemoryModel.NoReleasePlan, MemoryModel.NoReleasePlan -> false
  | _ -> true

let dictDecHelperForReleasePlanWithFingerprint (releasePlanFingerprint : string)
    (releasePlan : MemoryModel.rcReleasePlan) : string =
  match releasePlan with
  | MemoryModel.RootRelease
      ( _,
        MemoryModel.DictHeap,
        MemoryModel.DictPayloadRelease (keyRelease, valueRelease) )
    when dictPayloadReleaseNeedsPlannedHelper keyRelease valueRelease ->
      plannedDictDecHelperLabelForFingerprint releasePlanFingerprint
  | MemoryModel.RootRelease
      (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, valueRelease))
    -> (
      match valueRelease with
      | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
          dictRefCountDecListValueHelperLabel
      | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _)
        when releasePlanIsDictWithListValue valueRelease ->
          dictRefCountDecDictListValueHelperLabel
      | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
          dictRefCountDecDictValueHelperLabel
      | MemoryModel.RootRelease
          ( _,
            MemoryModel.GenericHeap,
            MemoryModel.FixedBlockPayloadRelease (16, fieldReleases) )
        when releasePlanDynamicOperationAt 0 MemoryModel.DynamicStringBuffer
               fieldReleases
             && releasePlanRootKindAt 8 MemoryModel.TaggedList fieldReleases ->
          dictRefCountDecTupleStringListValueHelperLabel
      | MemoryModel.RootRelease
          ( _,
            MemoryModel.GenericHeap,
            MemoryModel.FixedBlockPayloadRelease (24, fieldReleases) )
        when releasePlanDynamicOperationAt 0 MemoryModel.DynamicStringBuffer
               fieldReleases
             && releasePlanRootKindAt 8 MemoryModel.TaggedList fieldReleases
             && releasePlanRootKindAt 16 MemoryModel.DictHeap fieldReleases ->
          dictRefCountDecTupleStringListDictValueHelperLabel
      | MemoryModel.RootRelease
          ( _,
            MemoryModel.GenericHeap,
            MemoryModel.BoxedSumPayloadRelease (_, fieldReleases, _) )
        when releasePlanDynamicOperationAt 8 MemoryModel.DynamicStringBuffer
               fieldReleases ->
          dictRefCountDecSumStringValueHelperLabel
      | _ -> dictRefCountDecHelperLabel)
  | _ -> dictRefCountDecHelperLabel

let dictDecHelperForReleasePlan (releasePlan : MemoryModel.rcReleasePlan) :
    string =
  dictDecHelperForReleasePlanWithFingerprint
    (ReleasePlanFingerprint.rcReleasePlanFingerprint releasePlan)
    releasePlan
