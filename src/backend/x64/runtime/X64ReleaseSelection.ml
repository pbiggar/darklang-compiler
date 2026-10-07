(* X64ReleaseSelection.ml - Select backend helpers from explicit representation release plans. *)
[@@@warning "-4"]
open X64Operands
open X64CodeGenTypes

(* Reference Counting Helpers *)
let genDynamicBufferFieldRelease ctx skipTagged fieldOffset =
 let doneLabel=freshLabel "rc_dec_field_done" in
 let literalLabel=freshLabel "rc_dec_field_lit" in
 let noFreeLabel=freshLabel "rc_dec_field_nofree" in
 let leakDec=genLeakCounterDec ctx in
 let taggedGuard=if skipTagged then
  [X86_64.MOV_reg (scratch,X86_64.R8);X86_64.AND_imm (scratch,1l);X86_64.Jcc (X86_64.NE,doneLabel)] else [] in
 [X86_64.MOV_load (X86_64.R8,X86_64.RDX,Int32.of_int fieldOffset);
  X86_64.TEST_reg (X86_64.R8,X86_64.R8);X86_64.Jcc (X86_64.EQ,doneLabel)]
 @ taggedGuard @ [X86_64.MOV_load (X86_64.R9,X86_64.R8,0l)]
 @ loadImm64 scratch 0x7FFFFFFFFFFFFFFFL
 @ [X86_64.CMP_reg (X86_64.R9,scratch);X86_64.Jcc (X86_64.EQ,literalLabel);
    X86_64.SUB_imm (X86_64.R9,1l);X86_64.MOV_store (X86_64.R8,0l,X86_64.R9);
    X86_64.TEST_reg (X86_64.R9,X86_64.R9);X86_64.Jcc (X86_64.NE,noFreeLabel)]
 @ leakDec @ [X86_64.Label noFreeLabel;X86_64.Label literalLabel;X86_64.Label doneLabel]
let listRefCountDecHelperLabel = "__dark_list_rc_dec_helper"
let listRefCountDecListHelperLabel = "__dark_list_rc_dec_list_helper"
let listRefCountDecClosureHelperLabel = "__dark_list_rc_dec_closure_helper"
let listRefCountDecDictHelperLabel = "__dark_list_rc_dec_dict_helper"
let listRefCountDecDictListHelperLabel = "__dark_list_rc_dec_dict_list_helper"
let listRefCountDecDynamicBufferHelperLabel = "__dark_list_rc_dec_dynamic_buffer_helper"
let listRefCountDecDynamicIntHelperLabel = "__dark_list_rc_dec_dynamic_int_helper"
let plannedListRefCountDecHelperLabelPrefix = "__dark_list_rc_dec_plan_"
let dictRefCountIncHelperLabel = "__dark_dict_rc_inc_helper"
let dictRefCountDecHelperLabel = "__dark_dict_rc_dec_helper"
let dictRefCountDecDynamicKeyHelperLabel = "__dark_dict_rc_dec_dynamic_key_helper"
let dictRefCountDecDynamicValueHelperLabel = "__dark_dict_rc_dec_dynamic_value_helper"
let dictRefCountDecDynamicKeyValueHelperLabel = "__dark_dict_rc_dec_dynamic_key_value_helper"
let dictRefCountDecDynamicKeyListValueHelperLabel = "__dark_dict_rc_dec_dynamic_key_list_value_helper"
let dictRefCountDecDynamicKeyDictValueHelperLabel = "__dark_dict_rc_dec_dynamic_key_dict_value_helper"
let dictRefCountDecDynamicKeyDictListValueHelperLabel = "__dark_dict_rc_dec_dynamic_key_dict_list_value_helper"
let dictRefCountDecListValueHelperLabel = "__dark_dict_rc_dec_list_value_helper"
let dictRefCountDecDictValueHelperLabel = "__dark_dict_rc_dec_dict_value_helper"
let dictRefCountDecDictListValueHelperLabel = "__dark_dict_rc_dec_dict_list_value_helper"
let dictRefCountDecTupleStringListValueHelperLabel = "__dark_dict_rc_dec_tuple_string_list_value_helper"
let dictRefCountDecTupleStringListDictValueHelperLabel = "__dark_dict_rc_dec_tuple_string_list_dict_value_helper"
let dictRefCountDecDynamicKeyTupleStringListDictValueHelperLabel = "__dark_dict_rc_dec_dynamic_key_tuple_string_list_dict_value_helper"
let dictRefCountDecSumStringValueHelperLabel = "__dark_dict_rc_dec_sum_string_value_helper"
let plannedDictRefCountDecHelperLabelPrefix = "__dark_dict_rc_dec_plan_"
let closureRefCountIncHelperLabel = "__dark_closure_rc_inc_helper"
let closureRefCountDecHelperLabel = "__dark_closure_rc_dec_helper"
let streamRefCountDecHelperLabel = "__dark_stream_rc_dec_helper"

type slotInitRootRetainTarget = SlotInitListRootRetain | SlotInitDictRootRetain | SlotInitDynamicBufferRetain | SlotInitClosureRootRetain | SlotInitGenericRootRetain of int

let slotInitRootRetainTarget recordRegistry sumShapeRegistry valueType =
 let shapeOfKnownType typ=match typ with
 | AST.TRecord (name,_) when not (StringOrder.Map.mem name recordRegistry) -> None
 | _ -> Some (MemoryPlanning.rcShapeOfTypeWithSums recordRegistry (MemoryPlanning.inferredRecordTypeParamsRegistry recordRegistry) sumShapeRegistry typ) in
 match shapeOfKnownType valueType with
 | None -> None
 | Some shape -> match shape with
   | MemoryModel.TaggedListShape _ -> Some SlotInitListRootRetain
   | MemoryModel.DictRoot _ -> Some SlotInitDictRootRetain
   | MemoryModel.DynamicString | MemoryModel.DynamicBlob | MemoryModel.DynamicInt -> Some SlotInitDynamicBufferRetain
   | MemoryModel.ClosureShape _ -> Some SlotInitClosureRootRetain
   | MemoryModel.FixedBlock (payloadSize,_) -> (match valueType with
      | AST.TTuple _ | AST.TRecord _ | AST.TInt128 | AST.TUInt128 -> Some (SlotInitGenericRootRetain payloadSize)
      | _ -> None)
   | MemoryModel.StreamRoot -> Some (SlotInitGenericRootRetain 24)
   | MemoryModel.BoxedSum (payloadSize,_,_) -> (match valueType with AST.TSum _ -> Some (SlotInitGenericRootRetain payloadSize) | _ -> None)
   | MemoryModel.RecursiveNominalRef _ -> Some (SlotInitGenericRootRetain 16)
   | MemoryModel.Immediate | MemoryModel.StaticString | MemoryModel.RawUnmanaged -> None

let tryRcReleasePlanOfType recordRegistry sumShapeRegistry typ = match typ with
 | AST.TRecord (name,_) when not (StringOrder.Map.mem name recordRegistry) -> None
 | _ -> Some (MemoryPlanning.rcReleasePlanOfTypeWithSums recordRegistry sumShapeRegistry typ)
let rcMetadataReleasePlan metadata=Option.bind metadata (fun (m:MemoryModel.rcMetadata) -> m.MemoryModel.releasePlan)
let requiredRcMetadataReleasePlan context metadata=match rcMetadataReleasePlan metadata with
 | Some releasePlan -> releasePlan
 | None -> Crash.crash (context ^ ": missing RC release plan metadata")
let releasePlanIsRootKind kind releasePlan=match releasePlan with
 | MemoryModel.RootRelease (_,planKind,_) when planKind=kind -> true | _ -> false
let releasePlanIsDictWithListValue releasePlan=match releasePlan with
 | MemoryModel.RootRelease (_,MemoryModel.DictHeap,MemoryModel.DictPayloadRelease (_,MemoryModel.RootRelease (_,MemoryModel.TaggedList,_))) -> true
 | _ -> false
let releasePlanIsTaggedListWithElementRelease elementPredicate releasePlan=match releasePlan with
 | MemoryModel.RootRelease (_,MemoryModel.TaggedList,MemoryModel.TaggedListPayloadRelease elementRelease) -> elementPredicate elementRelease
 | _ -> false
[@@warning "-32"]
let releasePlanIsDynamicBufferRelease releasePlan=match releasePlan with
 | MemoryModel.DynamicBufferRelease _ -> true | _ -> false
[@@warning "-32"]
let recursiveNominalRefCountDecHelperLabel sourceType="__dark_recursive_nominal_rc_dec_" ^ ReleasePlanFingerprint.rcSourceTypeFingerprint sourceType
let plannedListDecHelperLabelForReleasePlan releasePlan=plannedListRefCountDecHelperLabelPrefix ^ ReleasePlanFingerprint.rcReleasePlanFingerprint releasePlan
let plannedDictDecHelperLabelForReleasePlan releasePlan=plannedDictRefCountDecHelperLabelPrefix ^ ReleasePlanFingerprint.rcReleasePlanFingerprint releasePlan
let rec rcReleasePlanContains predicate releasePlan =
 if predicate releasePlan then true else match releasePlan with
 | MemoryModel.RootRelease (_,_,MemoryModel.FixedBlockPayloadRelease (_,fields))
 | MemoryModel.RootRelease (_,_,MemoryModel.BoxedSumPayloadRelease (_,fields,_))
 | MemoryModel.RootRelease (_,_,MemoryModel.ClosurePayloadRelease fields) ->
   List.exists (fun (MemoryModel.FieldRelease (_,fieldRelease)) -> rcReleasePlanContains predicate fieldRelease) fields
 | MemoryModel.RootRelease (_,_,MemoryModel.DictPayloadRelease (keyRelease,valueRelease)) -> rcReleasePlanContains predicate keyRelease || rcReleasePlanContains predicate valueRelease
 | MemoryModel.RootRelease (_,_,MemoryModel.TaggedListPayloadRelease elementRelease) -> rcReleasePlanContains predicate elementRelease
 | _ -> false
let typeReleasePlanContains predicate recordRegistry sumShapeRegistry typ =
 Option.fold ~none:false ~some:(rcReleasePlanContains predicate) (tryRcReleasePlanOfType recordRegistry sumShapeRegistry typ)
[@@warning "-32"]
let genClosureFieldRelease fieldOffset =
 [X86_64.PUSH X86_64.RDX;X86_64.MOV_load (X86_64.RAX,X86_64.RDX,Int32.of_int fieldOffset);
 X86_64.CALL closureRefCountDecHelperLabel;X86_64.POP X86_64.RDX]
let releasePlanFieldReleaseAt fieldOffset fieldReleases =
 List.find_map (fun (MemoryModel.FieldRelease (offset,plan)) -> if offset=fieldOffset then Some plan else None) fieldReleases
let releasePlanIsDynamicBufferAt fieldOffset fields=match releasePlanFieldReleaseAt fieldOffset fields with
 | Some (MemoryModel.DynamicBufferRelease _) -> true | _ -> false
let releasePlanIsRootKindAt fieldOffset kind fields=match releasePlanFieldReleaseAt fieldOffset fields with
 | Some (MemoryModel.RootRelease (_,planKind,_)) when planKind=kind -> true | _ -> false
let releasePlanIsDictWithValueAt fieldOffset valuePredicate fields=match releasePlanFieldReleaseAt fieldOffset fields with
 | Some (MemoryModel.RootRelease (_,MemoryModel.DictHeap,MemoryModel.DictPayloadRelease (_,valueRelease))) -> valuePredicate valueRelease
 | _ -> false
[@@warning "-32"]
let listDecHelperForReleasePlan (releasePlan: MemoryModel.rcReleasePlan) : string =
    match releasePlan with
    | MemoryModel.RootRelease (_, _, MemoryModel.TaggedListPayloadRelease elementRelease) ->
        (match elementRelease with
        | MemoryModel.NoReleasePlan ->
            listRefCountDecHelperLabel
        | MemoryModel.DynamicBufferRelease MemoryModel.DynamicIntBuffer ->
            listRefCountDecDynamicIntHelperLabel
        | MemoryModel.DynamicBufferRelease _ ->
            listRefCountDecDynamicBufferHelperLabel
        | MemoryModel.RecursiveRelease sourceType ->
            plannedListDecHelperLabelForReleasePlan (MemoryModel.RecursiveRelease sourceType)
        | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
            plannedListDecHelperLabelForReleasePlan elementRelease
        | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
            plannedListDecHelperLabelForReleasePlan elementRelease
        | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
            listRefCountDecClosureHelperLabel
        | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) ->
            plannedListDecHelperLabelForReleasePlan elementRelease
        | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, _) ->
            plannedListDecHelperLabelForReleasePlan elementRelease)
    | _ ->
        listRefCountDecHelperLabel

let listDecHelperForType
    (recordRegistry: LIR.recordRegistry)
    (sumShapeRegistry: MemoryModel.rcSumShapeRegistry)
    (fieldType: AST.semanticType) : string =
    match tryRcReleasePlanOfType recordRegistry sumShapeRegistry fieldType with
    | Some releasePlan -> listDecHelperForReleasePlan releasePlan
    | None -> Crash.crash ("listDecHelperForType: missing RC metadata for list element type " ^ StructuralFormat.semanticType fieldType)

let dictPayloadReleaseNeedsPlannedHelper (keyRelease: MemoryModel.rcReleasePlan) (valueRelease: MemoryModel.rcReleasePlan) : bool =
    match (keyRelease, valueRelease) with
    | MemoryModel.NoReleasePlan, MemoryModel.NoReleasePlan ->
        false
    | _ ->
        true

let dictDecHelperForReleasePlan (releasePlan: MemoryModel.rcReleasePlan) : string =
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

let dictTupleStringListValueReleasePlan =
 MemoryModel.RootRelease (16,MemoryModel.GenericHeap,MemoryModel.FixedBlockPayloadRelease (16,
  [MemoryModel.FieldRelease (0,MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer);
   MemoryModel.FieldRelease (8,MemoryModel.RootRelease (0,MemoryModel.TaggedList,MemoryModel.TaggedListPayloadRelease MemoryModel.NoReleasePlan))]))
let dictTupleStringListDictValueReleasePlan =
 MemoryModel.RootRelease (24,MemoryModel.GenericHeap,MemoryModel.FixedBlockPayloadRelease (24,
  [MemoryModel.FieldRelease (0,MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer);
   MemoryModel.FieldRelease (8,MemoryModel.RootRelease (0,MemoryModel.TaggedList,MemoryModel.TaggedListPayloadRelease MemoryModel.NoReleasePlan));
   MemoryModel.FieldRelease (16,MemoryModel.RootRelease (0,MemoryModel.DictHeap,MemoryModel.DictPayloadRelease (MemoryModel.NoReleasePlan,MemoryModel.NoReleasePlan)))]))
let dictSumStringValueReleasePlan =
 MemoryModel.RootRelease (16,MemoryModel.GenericHeap,MemoryModel.BoxedSumPayloadRelease (16,
  [MemoryModel.FieldRelease (8,MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer)],[]))
let genDictFieldRelease fieldOffset fieldReleasePlan =
 [X86_64.PUSH X86_64.RDX;X86_64.MOV_load (X86_64.R8,X86_64.RDX,Int32.of_int fieldOffset);
  X86_64.MOV_reg (X86_64.RAX,X86_64.R8);X86_64.CALL (dictDecHelperForReleasePlan fieldReleasePlan);X86_64.POP X86_64.RDX]
let genListFieldRelease fieldOffset fieldReleasePlan =
 [X86_64.PUSH X86_64.RDX;X86_64.MOV_load (X86_64.RAX,X86_64.RDX,Int32.of_int fieldOffset);
  X86_64.CALL (listDecHelperForReleasePlan fieldReleasePlan);X86_64.POP X86_64.RDX]
