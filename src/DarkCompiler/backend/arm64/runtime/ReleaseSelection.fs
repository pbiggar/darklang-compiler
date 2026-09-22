// ReleaseSelection.fs - Select backend helpers from explicit representation release plans.

module ARM64ReleaseSelection

open ARM64CodeGenTypes
open ARM64ClosureReferenceCounts

let private releasePlanIsRootKind (kind: MemoryModel.RcKind) (releasePlan: MemoryModel.RcReleasePlan) : bool =
    match releasePlan with
    | MemoryModel.RootRelease (_, planKind, _) when planKind = kind ->
        true
    | _ ->
        false

let private releasePlanIsTaggedListWithElementRelease
    (elementPredicate: MemoryModel.RcReleasePlan -> bool)
    (releasePlan: MemoryModel.RcReleasePlan)
    : bool =
    match releasePlan with
    | MemoryModel.RootRelease (_, MemoryModel.TaggedList, MemoryModel.TaggedListPayloadRelease elementRelease) ->
        elementPredicate elementRelease
    | _ ->
        false

let private releasePlanIsDictWithListValue (releasePlan: MemoryModel.RcReleasePlan) : bool =
    match releasePlan with
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, MemoryModel.RootRelease (_, MemoryModel.TaggedList, _))) ->
        true
    | _ ->
        false

let private releasePlanIsDynamicBufferOperation
    (operation: MemoryModel.RcOperation)
    (releasePlan: MemoryModel.RcReleasePlan)
    : bool =
    match releasePlan with
    | MemoryModel.DynamicBufferRelease planOperation when planOperation = operation ->
        true
    | _ ->
        false

let private releasePlanFieldHasRelease
    (fieldOffset: int)
    (predicate: MemoryModel.RcReleasePlan -> bool)
    (fieldReleases: MemoryModel.RcFieldRelease list)
    : bool =
    fieldReleases
    |> List.exists (function
        | MemoryModel.FieldRelease (offset, releasePlan) when offset = fieldOffset ->
            predicate releasePlan
        | _ ->
            false)

let private releasePlanIsFixedBlockWithFieldReleases
    (payloadSize: int)
    (expectedFieldReleases: (int * (MemoryModel.RcReleasePlan -> bool)) list)
    (releasePlan: MemoryModel.RcReleasePlan)
    : bool =
    match releasePlan with
    | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (planPayloadSize, fieldReleases))
        when planPayloadSize = payloadSize ->
        expectedFieldReleases
        |> List.forall (fun (fieldOffset, predicate) ->
            releasePlanFieldHasRelease fieldOffset predicate fieldReleases)
    | _ ->
        false

let private releasePlanIsSingleListDictFieldPayload (releasePlan: MemoryModel.RcReleasePlan) : bool =
    releasePlan
    |> releasePlanIsFixedBlockWithFieldReleases
        8
        [
            0, releasePlanIsTaggedListWithElementRelease (releasePlanIsRootKind MemoryModel.DictHeap)
        ]

let private releasePlanIsThreeManagedFieldPayload (releasePlan: MemoryModel.RcReleasePlan) : bool =
    releasePlan
    |> releasePlanIsFixedBlockWithFieldReleases
        24
        [
            0, releasePlanIsDynamicBufferOperation MemoryModel.DynamicStringBuffer
            8, releasePlanIsRootKind MemoryModel.TaggedList
            16, releasePlanIsRootKind MemoryModel.DictHeap
        ]

let private releasePlanIsTaggedListWithTuple2ElementFieldRelease
    (fieldPredicate: MemoryModel.RcReleasePlan -> bool)
    (releasePlan: MemoryModel.RcReleasePlan)
    : bool =
    releasePlan
    |> releasePlanIsTaggedListWithElementRelease (function
        | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (16, fieldReleases)) ->
            fieldReleases
            |> List.exists (function
                | MemoryModel.FieldRelease (_, fieldRelease) ->
                    fieldPredicate fieldRelease)
        | _ ->
            false)

let private releasePlanFieldReleaseAt
    (fieldOffset: int)
    (fieldReleases: MemoryModel.RcFieldRelease list)
    : MemoryModel.RcReleasePlan option =
    fieldReleases
    |> List.tryPick (function
        | MemoryModel.FieldRelease (offset, releasePlan) when offset = fieldOffset ->
            Some releasePlan
        | _ ->
            None)

let internal releasePlanRootKindAt
    (fieldOffset: int)
    (kind: MemoryModel.RcKind)
    (fieldReleases: MemoryModel.RcFieldRelease list)
    : bool =
    match releasePlanFieldReleaseAt fieldOffset fieldReleases with
    | Some (MemoryModel.RootRelease (_, planKind, _)) when planKind = kind ->
        true
    | _ ->
        false

let internal releasePlanDynamicOperationAt
    (fieldOffset: int)
    (operation: MemoryModel.RcOperation)
    (fieldReleases: MemoryModel.RcFieldRelease list)
    : bool =
    match releasePlanFieldReleaseAt fieldOffset fieldReleases with
    | Some (MemoryModel.DynamicBufferRelease planOperation) when planOperation = operation ->
        true
    | _ ->
        false

let internal listDecHelperForElementRelease
    (elementFingerprint: string)
    (elementRelease: MemoryModel.RcReleasePlan)
    : string =
        match elementRelease with
        | MemoryModel.NoReleasePlan ->
            listRefCountDecHelperLabel
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
            listRefCountDecListHelperLabel
        | MemoryModel.RootRelease (
              _,
              MemoryModel.DictHeap,
              MemoryModel.DictPayloadRelease (MemoryModel.DynamicBufferRelease _, _))
        | MemoryModel.RootRelease (
              _,
              MemoryModel.DictHeap,
              MemoryModel.DictPayloadRelease (_, MemoryModel.DynamicBufferRelease _)) ->
            plannedListDecHelperLabelForFingerprint elementFingerprint
        | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) when releasePlanIsDictWithListValue elementRelease ->
            listRefCountDecDictListHelperLabel
        | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
            listRefCountDecDictHelperLabel
        | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
            listRefCountDecClosureHelperLabel
        | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) ->
            plannedListDecHelperLabelForFingerprint elementFingerprint
        | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, _) ->
            plannedListDecHelperLabelForFingerprint elementFingerprint

let internal listDecHelperForReleasePlan (releasePlan: MemoryModel.RcReleasePlan) : string =
    match releasePlan with
    | MemoryModel.RootRelease (_, _, MemoryModel.TaggedListPayloadRelease elementRelease) ->
        listDecHelperForElementRelease
            (ReleasePlanFingerprint.rcReleasePlanFingerprint elementRelease)
            elementRelease
    | _ ->
        listRefCountDecHelperLabel

let private listDecHelperForType (ctx: CodeGenContext) (sourceType: AST.SemanticType) : string =
    match tryRcReleasePlanOfType ctx.RecordRegistry ctx.SumShapeRegistry sourceType with
    | Some releasePlan -> listDecHelperForReleasePlan releasePlan
    | None -> Crash.crash $"listDecHelperForType: missing RC metadata for list element type {sourceType}"

let internal dictPayloadReleaseNeedsPlannedHelper (keyRelease: MemoryModel.RcReleasePlan) (valueRelease: MemoryModel.RcReleasePlan) : bool =
    match keyRelease, valueRelease with
    | MemoryModel.DynamicBufferRelease _, _
    | _, MemoryModel.DynamicBufferRelease _ ->
        true
    | MemoryModel.NoReleasePlan, MemoryModel.RootRelease (_, MemoryModel.TaggedList, _)
    | MemoryModel.NoReleasePlan, MemoryModel.RootRelease (_, MemoryModel.DictHeap, _)
    | MemoryModel.NoReleasePlan, MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
        true
    | _, MemoryModel.RootRelease (_, MemoryModel.GenericHeap, _) ->
        true
    | _ ->
        false

let internal dictDecHelperForReleasePlanWithFingerprint
    (releasePlanFingerprint: string)
    (releasePlan: MemoryModel.RcReleasePlan)
    : string =
    match releasePlan with
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (keyRelease, valueRelease))
        when dictPayloadReleaseNeedsPlannedHelper keyRelease valueRelease ->
        plannedDictDecHelperLabelForFingerprint releasePlanFingerprint
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, valueRelease)) ->
        match valueRelease with
        | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
            dictRefCountDecListValueHelperLabel
        | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) when releasePlanIsDictWithListValue valueRelease ->
            dictRefCountDecDictListValueHelperLabel
        | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
            dictRefCountDecDictValueHelperLabel
        | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (16, fieldReleases))
            when releasePlanDynamicOperationAt 0 MemoryModel.DynamicStringBuffer fieldReleases
                 && releasePlanRootKindAt 8 MemoryModel.TaggedList fieldReleases ->
            dictRefCountDecTupleStringListValueHelperLabel
        | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (24, fieldReleases))
            when releasePlanDynamicOperationAt 0 MemoryModel.DynamicStringBuffer fieldReleases
                 && releasePlanRootKindAt 8 MemoryModel.TaggedList fieldReleases
                 && releasePlanRootKindAt 16 MemoryModel.DictHeap fieldReleases ->
            dictRefCountDecTupleStringListDictValueHelperLabel
        | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease (_, fieldReleases, _))
            when releasePlanDynamicOperationAt 8 MemoryModel.DynamicStringBuffer fieldReleases ->
            dictRefCountDecSumStringValueHelperLabel
        | _ ->
            dictRefCountDecHelperLabel
    | _ ->
        dictRefCountDecHelperLabel

let internal dictDecHelperForReleasePlan (releasePlan: MemoryModel.RcReleasePlan) : string =
    dictDecHelperForReleasePlanWithFingerprint
        (ReleasePlanFingerprint.rcReleasePlanFingerprint releasePlan)
        releasePlan
