// ReleaseSelection.fs - Select backend helpers from explicit representation release plans.

module X64ReleaseSelection

open X64Operands
open X64CodeGenTypes

// ============================================================================
// Reference Counting Helpers
// ============================================================================

let internal genDynamicBufferFieldRelease
    (ctx: FuncCtx)
    (skipTagged: bool)
    (fieldOffset: int)
    : X86_64.Instr list =
    let doneLabel = freshLabel "rc_dec_field_done"
    let literalLabel = freshLabel "rc_dec_field_lit"
    let noFreeLabel = freshLabel "rc_dec_field_nofree"
    let leakDec = genLeakCounterDec ctx
    let taggedGuard =
        if skipTagged then
            [X86_64.MOV_reg (scratch, X86_64.R8)
             X86_64.AND_imm (scratch, 1)
             X86_64.Jcc (X86_64.NE, doneLabel)]
        else
            []
    [X86_64.MOV_load (X86_64.R8, X86_64.RDX, fieldOffset)
     X86_64.TEST_reg (X86_64.R8, X86_64.R8)
     X86_64.Jcc (X86_64.EQ, doneLabel)]
    @ taggedGuard
    @ [X86_64.MOV_load (X86_64.R9, X86_64.R8, 0)]
    @ loadImm64 scratch 0x7FFFFFFFFFFFFFFFL
    @ [X86_64.CMP_reg (X86_64.R9, scratch)
       X86_64.Jcc (X86_64.EQ, literalLabel)
       X86_64.SUB_imm (X86_64.R9, 1)
       X86_64.MOV_store (X86_64.R8, 0, X86_64.R9)
       X86_64.TEST_reg (X86_64.R9, X86_64.R9)
       X86_64.Jcc (X86_64.NE, noFreeLabel)]
    @ leakDec
    @ [X86_64.Label noFreeLabel
       X86_64.Label literalLabel
       X86_64.Label doneLabel]

let internal listRefCountDecHelperLabel = "__dark_list_rc_dec_helper"
let internal listRefCountDecListHelperLabel = "__dark_list_rc_dec_list_helper"
let internal listRefCountDecClosureHelperLabel = "__dark_list_rc_dec_closure_helper"
let internal listRefCountDecDictHelperLabel = "__dark_list_rc_dec_dict_helper"
let internal listRefCountDecDictListHelperLabel = "__dark_list_rc_dec_dict_list_helper"
let internal listRefCountDecDynamicBufferHelperLabel = "__dark_list_rc_dec_dynamic_buffer_helper"
let internal listRefCountDecDynamicIntHelperLabel = "__dark_list_rc_dec_dynamic_int_helper"
let private plannedListRefCountDecHelperLabelPrefix = "__dark_list_rc_dec_plan_"
let internal dictRefCountIncHelperLabel = "__dark_dict_rc_inc_helper"
let internal dictRefCountDecHelperLabel = "__dark_dict_rc_dec_helper"
let internal dictRefCountDecDynamicKeyHelperLabel = "__dark_dict_rc_dec_dynamic_key_helper"
let internal dictRefCountDecDynamicValueHelperLabel = "__dark_dict_rc_dec_dynamic_value_helper"
let internal dictRefCountDecDynamicKeyValueHelperLabel = "__dark_dict_rc_dec_dynamic_key_value_helper"
let internal dictRefCountDecDynamicKeyListValueHelperLabel = "__dark_dict_rc_dec_dynamic_key_list_value_helper"
let internal dictRefCountDecDynamicKeyDictValueHelperLabel = "__dark_dict_rc_dec_dynamic_key_dict_value_helper"
let internal dictRefCountDecDynamicKeyDictListValueHelperLabel = "__dark_dict_rc_dec_dynamic_key_dict_list_value_helper"
let internal dictRefCountDecListValueHelperLabel = "__dark_dict_rc_dec_list_value_helper"
let internal dictRefCountDecDictValueHelperLabel = "__dark_dict_rc_dec_dict_value_helper"
let internal dictRefCountDecDictListValueHelperLabel = "__dark_dict_rc_dec_dict_list_value_helper"
let internal dictRefCountDecTupleStringListValueHelperLabel = "__dark_dict_rc_dec_tuple_string_list_value_helper"
let internal dictRefCountDecTupleStringListDictValueHelperLabel = "__dark_dict_rc_dec_tuple_string_list_dict_value_helper"
let internal dictRefCountDecDynamicKeyTupleStringListDictValueHelperLabel = "__dark_dict_rc_dec_dynamic_key_tuple_string_list_dict_value_helper"
let internal dictRefCountDecSumStringValueHelperLabel = "__dark_dict_rc_dec_sum_string_value_helper"
let private plannedDictRefCountDecHelperLabelPrefix = "__dark_dict_rc_dec_plan_"
let internal closureRefCountIncHelperLabel = "__dark_closure_rc_inc_helper"
let internal closureRefCountDecHelperLabel = "__dark_closure_rc_dec_helper"
let internal streamRefCountDecHelperLabel = "__dark_stream_rc_dec_helper"

type internal SlotInitRootRetainTarget =
    | SlotInitListRootRetain
    | SlotInitDictRootRetain
    | SlotInitDynamicBufferRetain
    | SlotInitClosureRootRetain
    | SlotInitGenericRootRetain of payloadSize:int

let internal slotInitRootRetainTarget
    (recordRegistry: LIR.RecordRegistry)
    (sumShapeRegistry: MemoryModel.RcSumShapeRegistry)
    (valueType: AST.SemanticType)
    : SlotInitRootRetainTarget option =
    let shapeOfKnownType (typ: AST.SemanticType) : MemoryModel.RcShape option =
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
                Some SlotInitListRootRetain
            | MemoryModel.DictRoot _ ->
                Some SlotInitDictRootRetain
            | MemoryModel.DynamicString
            | MemoryModel.DynamicBlob
            | MemoryModel.DynamicInt ->
                Some SlotInitDynamicBufferRetain
            | MemoryModel.ClosureShape _ ->
                Some SlotInitClosureRootRetain
            | MemoryModel.FixedBlock (payloadSize, _) ->
                match valueType with
                | AST.TTuple _
                | AST.TRecord _
                | AST.TInt128
                | AST.TUInt128 -> Some (SlotInitGenericRootRetain payloadSize)
                | _ -> None
            | MemoryModel.StreamRoot -> Some (SlotInitGenericRootRetain 24)
            | MemoryModel.BoxedSum (payloadSize, _, _) ->
                match valueType with
                | AST.TSum _ -> Some (SlotInitGenericRootRetain payloadSize)
                | _ -> None
            | MemoryModel.RecursiveNominalRef _ ->
                Some (SlotInitGenericRootRetain 16)
            | MemoryModel.Immediate
            | MemoryModel.StaticString
            | MemoryModel.RawUnmanaged ->
                None)

let internal tryRcReleasePlanOfType
    (recordRegistry: LIR.RecordRegistry)
    (sumShapeRegistry: MemoryModel.RcSumShapeRegistry)
    (typ: AST.SemanticType)
    : MemoryModel.RcReleasePlan option =
    match typ with
    | AST.TRecord (name, _) when not (Map.containsKey name recordRegistry) ->
        None
    | _ ->
        Some (MemoryPlanning.rcReleasePlanOfTypeWithSums recordRegistry sumShapeRegistry typ)

let internal rcMetadataReleasePlan (metadata: MemoryModel.RcMetadata option) : MemoryModel.RcReleasePlan option =
    metadata |> Option.bind (fun m -> m.ReleasePlan)

let internal requiredRcMetadataReleasePlan (context: string) (metadata: MemoryModel.RcMetadata option) : MemoryModel.RcReleasePlan =
    match rcMetadataReleasePlan metadata with
    | Some releasePlan -> releasePlan
    | None -> Crash.crash $"{context}: missing RC release plan metadata"

let internal releasePlanIsRootKind (kind: MemoryModel.RcKind) (releasePlan: MemoryModel.RcReleasePlan) : bool =
    match releasePlan with
    | MemoryModel.RootRelease (_, planKind, _) when planKind = kind ->
        true
    | _ ->
        false

let internal releasePlanIsDictWithListValue (releasePlan: MemoryModel.RcReleasePlan) : bool =
    match releasePlan with
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, MemoryModel.RootRelease (_, MemoryModel.TaggedList, _))) ->
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

let private releasePlanIsDynamicBufferRelease (releasePlan: MemoryModel.RcReleasePlan) : bool =
    match releasePlan with
    | MemoryModel.DynamicBufferRelease _ ->
        true
    | _ ->
        false

let private stableRcReleasePlanHash (releasePlan: MemoryModel.RcReleasePlan) : string =
    let fnvOffset = 14695981039346656037UL
    let fnvPrime = 1099511628211UL

    sprintf "%A" releasePlan
    |> Seq.fold (fun hash ch ->
        (hash ^^^ (uint64 (int ch))) * fnvPrime)
        fnvOffset
    |> fun hash -> hash.ToString("x16")

let internal recursiveNominalRefCountDecHelperLabel (sourceType: AST.SemanticType) : string =
    let hash =
        $"{sourceType}"
        |> Seq.fold (fun hash ch ->
            (hash ^^^ (uint64 (int ch))) * 1099511628211UL)
            14695981039346656037UL
        |> fun value -> value.ToString("x16")
    $"__dark_recursive_nominal_rc_dec_{hash}"

let internal plannedListDecHelperLabelForReleasePlan (releasePlan: MemoryModel.RcReleasePlan) : string =
    $"{plannedListRefCountDecHelperLabelPrefix}{stableRcReleasePlanHash releasePlan}"

let private plannedDictDecHelperLabelForReleasePlan (releasePlan: MemoryModel.RcReleasePlan) : string =
    $"{plannedDictRefCountDecHelperLabelPrefix}{stableRcReleasePlanHash releasePlan}"

let rec internal rcReleasePlanContains
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
            |> List.exists (function
                | MemoryModel.FieldRelease (_, fieldRelease) ->
                    rcReleasePlanContains predicate fieldRelease)
        | MemoryModel.RootRelease (_, _, MemoryModel.DictPayloadRelease (keyRelease, valueRelease)) ->
            rcReleasePlanContains predicate keyRelease
            || rcReleasePlanContains predicate valueRelease
        | MemoryModel.RootRelease (_, _, MemoryModel.TaggedListPayloadRelease elementRelease) ->
            rcReleasePlanContains predicate elementRelease
        | _ ->
            false

let private typeReleasePlanContains
    (predicate: MemoryModel.RcReleasePlan -> bool)
    (recordRegistry: LIR.RecordRegistry)
    (sumShapeRegistry: MemoryModel.RcSumShapeRegistry)
    (typ: AST.SemanticType)
    : bool =
    typ
    |> tryRcReleasePlanOfType recordRegistry sumShapeRegistry
    |> Option.exists (rcReleasePlanContains predicate)

let internal genClosureFieldRelease (fieldOffset: int) : X86_64.Instr list =
    [X86_64.PUSH X86_64.RDX
     X86_64.MOV_load (X86_64.RAX, X86_64.RDX, fieldOffset)
     X86_64.CALL closureRefCountDecHelperLabel
     X86_64.POP X86_64.RDX]

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

let private releasePlanIsDynamicBufferAt (fieldOffset: int) (fieldReleases: MemoryModel.RcFieldRelease list) : bool =
    match releasePlanFieldReleaseAt fieldOffset fieldReleases with
    | Some (MemoryModel.DynamicBufferRelease _) -> true
    | _ -> false

let private releasePlanIsRootKindAt
    (fieldOffset: int)
    (kind: MemoryModel.RcKind)
    (fieldReleases: MemoryModel.RcFieldRelease list)
    : bool =
    match releasePlanFieldReleaseAt fieldOffset fieldReleases with
    | Some (MemoryModel.RootRelease (_, planKind, _)) when planKind = kind -> true
    | _ -> false

let private releasePlanIsDictWithValueAt
    (fieldOffset: int)
    (valuePredicate: MemoryModel.RcReleasePlan -> bool)
    (fieldReleases: MemoryModel.RcFieldRelease list)
    : bool =
    match releasePlanFieldReleaseAt fieldOffset fieldReleases with
    | Some (MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, valueRelease))) ->
        valuePredicate valueRelease
    | _ ->
        false

let rec internal listDecHelperForReleasePlan (releasePlan: MemoryModel.RcReleasePlan) : string =
    match releasePlan with
    | MemoryModel.RootRelease (_, _, MemoryModel.TaggedListPayloadRelease elementRelease) ->
        match elementRelease with
        | MemoryModel.NoReleasePlan ->
            listRefCountDecHelperLabel
        | MemoryModel.DynamicBufferRelease MemoryModel.DynamicIntBuffer ->
            listRefCountDecDynamicIntHelperLabel
        | MemoryModel.DynamicBufferRelease _ ->
            listRefCountDecDynamicBufferHelperLabel
        | MemoryModel.RecursiveRelease sourceType ->
            plannedListDecHelperLabelForReleasePlan (MemoryModel.RecursiveRelease sourceType)
        | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
            listRefCountDecListHelperLabel
        | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
            plannedListDecHelperLabelForReleasePlan elementRelease
        | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
            listRefCountDecClosureHelperLabel
        | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) ->
            plannedListDecHelperLabelForReleasePlan elementRelease
        | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, _) ->
            plannedListDecHelperLabelForReleasePlan elementRelease
    | _ ->
        listRefCountDecHelperLabel

let internal listDecHelperForType
    (recordRegistry: LIR.RecordRegistry)
    (sumShapeRegistry: MemoryModel.RcSumShapeRegistry)
    (fieldType: AST.SemanticType)
    : string =
    match tryRcReleasePlanOfType recordRegistry sumShapeRegistry fieldType with
    | Some releasePlan -> listDecHelperForReleasePlan releasePlan
    | None -> Crash.crash $"listDecHelperForType: missing RC metadata for list element type {fieldType}"

let internal dictPayloadReleaseNeedsPlannedHelper (keyRelease: MemoryModel.RcReleasePlan) (valueRelease: MemoryModel.RcReleasePlan) : bool =
    match keyRelease, valueRelease with
    | MemoryModel.NoReleasePlan, MemoryModel.NoReleasePlan ->
        false
    | _ ->
        true

let rec internal dictDecHelperForReleasePlan (releasePlan: MemoryModel.RcReleasePlan) : string =
    match releasePlan with
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (keyRelease, valueRelease))
        when dictPayloadReleaseNeedsPlannedHelper keyRelease valueRelease ->
        plannedDictDecHelperLabelForReleasePlan releasePlan
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (MemoryModel.DynamicBufferRelease _, MemoryModel.DynamicBufferRelease _)) ->
        dictRefCountDecDynamicKeyValueHelperLabel
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (MemoryModel.DynamicBufferRelease _, MemoryModel.RootRelease (_, MemoryModel.TaggedList, _))) ->
        dictRefCountDecDynamicKeyListValueHelperLabel
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (MemoryModel.DynamicBufferRelease _, MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, MemoryModel.RootRelease (_, MemoryModel.TaggedList, _))))) ->
        dictRefCountDecDynamicKeyDictListValueHelperLabel
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (MemoryModel.DynamicBufferRelease _, MemoryModel.RootRelease (_, MemoryModel.DictHeap, _))) ->
        dictRefCountDecDynamicKeyDictValueHelperLabel
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (MemoryModel.DynamicBufferRelease _, MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (24, fieldReleases))))
        when releasePlanIsDynamicBufferAt 0 fieldReleases
             && releasePlanIsRootKindAt 8 MemoryModel.TaggedList fieldReleases
             && releasePlanIsRootKindAt 16 MemoryModel.DictHeap fieldReleases ->
        dictRefCountDecDynamicKeyTupleStringListDictValueHelperLabel
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (MemoryModel.DynamicBufferRelease _, MemoryModel.NoReleasePlan)) ->
        dictRefCountDecDynamicKeyHelperLabel
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, MemoryModel.DynamicBufferRelease _)) ->
        dictRefCountDecDynamicValueHelperLabel
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, MemoryModel.RootRelease (_, MemoryModel.TaggedList, _))) ->
        dictRefCountDecListValueHelperLabel
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, MemoryModel.RootRelease (_, MemoryModel.TaggedList, _))))) ->
        dictRefCountDecDictListValueHelperLabel
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, MemoryModel.RootRelease (_, MemoryModel.DictHeap, _))) ->
        dictRefCountDecDictValueHelperLabel
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (16, fieldReleases))))
        when releasePlanIsDynamicBufferAt 0 fieldReleases
             && releasePlanIsRootKindAt 8 MemoryModel.TaggedList fieldReleases ->
        dictRefCountDecTupleStringListValueHelperLabel
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (24, fieldReleases))))
        when releasePlanIsDynamicBufferAt 0 fieldReleases
             && releasePlanIsRootKindAt 8 MemoryModel.TaggedList fieldReleases
             && releasePlanIsRootKindAt 16 MemoryModel.DictHeap fieldReleases ->
        dictRefCountDecTupleStringListDictValueHelperLabel
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease (_, fieldReleases, _))))
        when releasePlanIsDynamicBufferAt 8 fieldReleases ->
        dictRefCountDecSumStringValueHelperLabel
    | _ ->
        dictRefCountDecHelperLabel

let internal dictTupleStringListValueReleasePlan : MemoryModel.RcReleasePlan =
    MemoryModel.RootRelease
        (16,
         MemoryModel.GenericHeap,
         MemoryModel.FixedBlockPayloadRelease
             (16,
              [MemoryModel.FieldRelease (0, MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer)
               MemoryModel.FieldRelease (8, MemoryModel.RootRelease (0, MemoryModel.TaggedList, MemoryModel.TaggedListPayloadRelease MemoryModel.NoReleasePlan))]))

let internal dictTupleStringListDictValueReleasePlan : MemoryModel.RcReleasePlan =
    MemoryModel.RootRelease
        (24,
         MemoryModel.GenericHeap,
         MemoryModel.FixedBlockPayloadRelease
             (24,
              [MemoryModel.FieldRelease (0, MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer)
               MemoryModel.FieldRelease (8, MemoryModel.RootRelease (0, MemoryModel.TaggedList, MemoryModel.TaggedListPayloadRelease MemoryModel.NoReleasePlan))
               MemoryModel.FieldRelease (16, MemoryModel.RootRelease (0, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (MemoryModel.NoReleasePlan, MemoryModel.NoReleasePlan)))]))

let internal dictSumStringValueReleasePlan : MemoryModel.RcReleasePlan =
    MemoryModel.RootRelease
        (16,
         MemoryModel.GenericHeap,
         MemoryModel.BoxedSumPayloadRelease
             (16,
              [MemoryModel.FieldRelease (8, MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer)],
              []))

let internal genDictFieldRelease (fieldOffset: int) (fieldReleasePlan: MemoryModel.RcReleasePlan) : X86_64.Instr list =
    [X86_64.PUSH X86_64.RDX
     X86_64.MOV_load (X86_64.R8, X86_64.RDX, fieldOffset)
     X86_64.MOV_reg (X86_64.RAX, X86_64.R8)
     X86_64.CALL (dictDecHelperForReleasePlan fieldReleasePlan)
     X86_64.POP X86_64.RDX]

let internal genListFieldRelease (fieldOffset: int) (fieldReleasePlan: MemoryModel.RcReleasePlan) : X86_64.Instr list =
    [X86_64.PUSH X86_64.RDX
     X86_64.MOV_load (X86_64.RAX, X86_64.RDX, fieldOffset)
     X86_64.CALL (listDecHelperForReleasePlan fieldReleasePlan)
     X86_64.POP X86_64.RDX]
