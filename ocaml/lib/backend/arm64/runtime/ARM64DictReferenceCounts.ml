(*
   DictReferenceCounts.fs - Generate HAMT root and recursive payload lifetime helpers.
*)
[@@@warning "-4"]
open! MemoryModel
let isEmpty xs=xs=[]
let int16 value=let low=value land 65535 in if low>=32768 then low-65536 else low
let uint16 value=value land 65535
module F=HostStructuralFormat
let number n=F.Scalar (string_of_int n)
let kindValue kind=F.Union ((match kind with MemoryModel.GenericHeap->"GenericHeap"|StreamHeap->"StreamHeap"|TaggedList->"TaggedList"|DictHeap->"DictHeap"|ClosureHeap->"ClosureHeap"),[])
let operationValue = function
 | MemoryModel.FixedSizeRoot (size,kind)->F.Union ("FixedSizeRoot",[number size;kindValue kind])
 | DynamicStringBuffer->F.Union ("DynamicStringBuffer",[])
 | DynamicBlobBuffer->F.Union ("DynamicBlobBuffer",[])
 | DynamicIntBuffer->F.Union ("DynamicIntBuffer",[])
let rec planValue = function
 | MemoryModel.NoReleasePlan->F.Union ("NoReleasePlan",[])
 | DynamicBufferRelease operation->F.Union ("DynamicBufferRelease",[operationValue operation])
 | RecursiveRelease typ->F.Union ("RecursiveRelease",[F.semanticValue typ])
 | RootRelease (size,kind,payload)->F.Union ("RootRelease",[number size;kindValue kind;payloadValue payload])
and fieldValue (MemoryModel.FieldRelease (offset,plan))=F.Union ("FieldRelease",[number offset;planValue plan])
and fieldsValue fields=F.Sequence (List.map fieldValue fields)
and payloadValue = function
 | MemoryModel.NoPayloadRelease->F.Union ("NoPayloadRelease",[])
 | FixedBlockPayloadRelease (size,fields)->F.Union ("FixedBlockPayloadRelease",[number size;fieldsValue fields])
 | BoxedSumPayloadRelease (size,fields,variants)->F.Union ("BoxedSumPayloadRelease",[number size;fieldsValue fields;F.Sequence (List.map (fun (variant:MemoryModel.rcBoxedSumVariantRelease)->F.Record ["Tag",number variant.MemoryModel.tag;"FieldReleases",fieldsValue variant.MemoryModel.fieldReleases]) variants)])
 | TaggedListPayloadRelease plan->F.Union ("TaggedListPayloadRelease",[planValue plan])
 | DictPayloadRelease (key,value)->F.Union ("DictPayloadRelease",[planValue key;planValue value])
 | ClosurePayloadRelease fields->F.Union ("ClosurePayloadRelease",[fieldsValue fields])
let planText value=F.format (planValue value)



open ARM64CodeGenTypes
open HeapAllocation
open ARM64ReleaseSelection

(*
   X0 = tagged HAMT root. Tags: 1 internal, 2 leaf, 3 collision.
   This contiguous all-ones mask is an encodable AArch64 logical immediate.
   bitmap
   D16 is reserved scratch: count the bitmap bytes, horizontally add
   them, and zero-extend the resulting byte into the child count.
*)
let generateDictRefCountIncHelper () : Symbolic.instr list =
    let label (name: string) : string = (Printf.sprintf "__dark_dict_rc_inc_%s" (name))
    in
    let internalTag = label "internal"
    in
    let leafTag = label "leaf"
    in
    let collisionTag = label "collision"
    in
    let haveOffset = label "have_offset"
    in
    let helperRet = label "ret"

    in
    [
        Symbolic.Label dictRefCountIncHelperLabel;

        Symbolic.CBZ (Symbolic.X0, helperRet);
        Symbolic.AND_imm (Symbolic.X1, Symbolic.X0, 3L);
        Symbolic.CBZ (Symbolic.X1, helperRet);
        Symbolic.CMP_imm (Symbolic.X1, 3);
        Symbolic.B_cond_label (Symbolic.GT, helperRet);

        Symbolic.AND_imm (Symbolic.X2, Symbolic.X0, 0xFFFFFFFFFFFFFFF8L);
        Symbolic.CMP_reg (Symbolic.X2, Symbolic.X27);
        Symbolic.B_cond_label (Symbolic.LT, helperRet);
        Symbolic.CMP_reg (Symbolic.X2, Symbolic.X28);
        Symbolic.B_cond_label (Symbolic.GE, helperRet);

        Symbolic.CMP_imm (Symbolic.X1, 1);
        Symbolic.B_cond_label (Symbolic.EQ, internalTag);
        Symbolic.CMP_imm (Symbolic.X1, 2);
        Symbolic.B_cond_label (Symbolic.EQ, leafTag);
        Symbolic.B_label collisionTag;

        Symbolic.Label leafTag;
        Symbolic.MOVZ (Symbolic.X3, 16, 0);
        Symbolic.B_label haveOffset;

        Symbolic.Label collisionTag;
        Symbolic.LDR (Symbolic.X3, Symbolic.X2, 0);
        Symbolic.LSL_imm (Symbolic.X3, Symbolic.X3, 4);
        Symbolic.ADD_imm (Symbolic.X3, Symbolic.X3, 8);
        Symbolic.B_label haveOffset;

        Symbolic.Label internalTag;
        Symbolic.LDR (Symbolic.X4, Symbolic.X2, 0);


        Symbolic.FMOV_from_gp (Symbolic.D16, Symbolic.X4);
        Symbolic.CNT_8B (Symbolic.D16, Symbolic.D16);
        Symbolic.ADDV_8B (Symbolic.D16, Symbolic.D16);
        Symbolic.UMOV_byte (Symbolic.X3, Symbolic.D16);
        Symbolic.LSL_imm (Symbolic.X3, Symbolic.X3, 3);
        Symbolic.ADD_imm (Symbolic.X3, Symbolic.X3, 8);

        Symbolic.Label haveOffset;
        Symbolic.ADD_reg (Symbolic.X4, Symbolic.X2, Symbolic.X3);
        Symbolic.LDR (Symbolic.X5, Symbolic.X4, 0);
        Symbolic.ADD_imm (Symbolic.X5, Symbolic.X5, 1);
        Symbolic.STR (Symbolic.X5, Symbolic.X4, 0);

        Symbolic.Label helperRet;
        Symbolic.RET
    ]

(*
   This contiguous all-ones mask is an encodable AArch64 logical immediate.
   X0 = current tagged HAMT root, X1 = pending work stack count.
   bitmap
   D16 is reserved scratch: count the bitmap bytes, horizontally add
   them, and zero-extend the resulting byte into the child count.
   Internal nodes select their first child before reaching freeNode.
   Leaves and collisions have no child, so clear the released root.
*)
let generateDictRefCountDecHelper
    (helperLabel: string)
    (keyReleasePlan: MemoryModel.rcReleasePlan)
    (releaseLeafDynamicValue: bool)
    (releaseLeafListValue: bool)
    (releaseLeafDictValueHelper: string option)
    (releaseLeafClosureValue: bool)
    (releaseLeafStreamValue: bool)
    (leafFixedBlockValueRelease: (int * MemoryModel.rcReleasePlan) option)
    (releaseLeafTupleStringListValue: bool)
    (releaseLeafTupleStringListDictValue: bool)
    (releaseLeafSumStringValue: bool)
    (ctx: codeGenContext)
    : Symbolic.instr list =
    let label (name: string) : string = (Printf.sprintf "%s_%s" (helperLabel) (name))
    in
    let leakDec =
        if ctx.options.enableLeakCheck then
            let labelRef = dataLabel leakCounterLabel
            in
            [
                Symbolic.ADRP (Symbolic.X17, labelRef);
                Symbolic.ADD_label (Symbolic.X17, Symbolic.X17, labelRef);
                Symbolic.LDR (Symbolic.X16, Symbolic.X17, 0);
                Symbolic.SUB_imm (Symbolic.X16, Symbolic.X16, 1);
                Symbolic.STR (Symbolic.X16, Symbolic.X17, 0)
            ]
        else
            []

    in
    let addChild (suffix: string) : Symbolic.instr list =
        let doneLabel = label (Printf.sprintf "child_done_%s" (suffix))
        in
        let pushLabel = label (Printf.sprintf "child_push_%s" (suffix))
        in
        [
            Symbolic.CBZ (Symbolic.X8, doneLabel);
            Symbolic.AND_imm (Symbolic.X9, Symbolic.X8, 3L);
            Symbolic.CBZ (Symbolic.X9, doneLabel);
            Symbolic.CMP_imm (Symbolic.X9, 3);
            Symbolic.B_cond_label (Symbolic.GT, doneLabel);

            Symbolic.AND_imm (Symbolic.X10, Symbolic.X8, 0xFFFFFFFFFFFFFFF8L);
            Symbolic.CMP_reg (Symbolic.X10, Symbolic.X27);
            Symbolic.B_cond_label (Symbolic.LT, doneLabel);
            Symbolic.CMP_reg (Symbolic.X10, Symbolic.X28);
            Symbolic.B_cond_label (Symbolic.GE, doneLabel);
            Symbolic.CBNZ (Symbolic.X0, pushLabel);
            Symbolic.MOV_reg (Symbolic.X0, Symbolic.X8);
            Symbolic.B_label doneLabel;
            Symbolic.Label pushLabel;
            Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 16);
            Symbolic.STR (Symbolic.X8, Symbolic.SP, 0);
            Symbolic.ADD_imm (Symbolic.X1, Symbolic.X1, 1);
            Symbolic.Label doneLabel
        ]

    in
    let loopCheck = label "loop_check"
    in
    let popOrRet = label "pop_or_ret"
    in
    let helperRet = label "ret"
    in
    let internalTag = label "internal"
    in
    let leafTag = label "leaf"
    in
    let collisionTag = label "collision"
    in
    let haveOffset = label "have_offset"
    in
    let collectInternal = label "collect_internal"
    in
    let collectLoop = label "collect_loop"
    in
    let freeNode = label "free_node"
    in
    let nextNode = label "next_node"
    in
    let skipFreeList = label "skip_freelist"
    in
    let skipLeafListValueRelease = label "skip_leaf_list_value_release"
    in
    let skipLeafDictValueRelease = label "skip_leaf_dict_value_release"
    in
    let skipLeafClosureValueRelease = label "skip_leaf_closure_value_release"
    in
    let skipLeafStreamValueRelease = label "skip_leaf_stream_value_release"
    in
    let _skipLeafDynamicKeyRelease = label "skip_leaf_dynamic_key_release"
    in
    let skipLeafDynamicValueRelease = label "skip_leaf_dynamic_value_release"
    in
    let skipCollisionPayloadRelease = label "skip_collision_payload_release"
    in
    let collisionPayloadLoop = label "collision_payload_loop"
    in
    let collisionPayloadDone = label "collision_payload_done"
    in
    let _skipCollisionDynamicKeyRelease = label "skip_collision_dynamic_key_release"
    in
    let skipCollisionDynamicValueRelease = label "skip_collision_dynamic_value_release"
    in
    let skipCollisionRootPayloadRelease = label "skip_collision_root_payload_release"
    in
    let collisionRootPayloadLoop = label "collision_root_payload_loop"
    in
    let collisionRootPayloadDone = label "collision_root_payload_done"
    in
    let skipCollisionListValueRelease = label "skip_collision_list_value_release"
    in
    let skipCollisionDictValueRelease = label "skip_collision_dict_value_release"
    in
    let skipCollisionClosureValueRelease = label "skip_collision_closure_value_release"
    in
    let skipCollisionStreamValueRelease = label "skip_collision_stream_value_release"
    in
    let skipLeafFixedBlockValueRelease = label "skip_leaf_fixed_block_value_release"
    in
    let skipCollisionGenericPayloadRelease = label "skip_collision_generic_payload_release"
    in
    let collisionGenericPayloadLoop = label "collision_generic_payload_loop"
    in
    let collisionGenericPayloadDone = label "collision_generic_payload_done"
    in
    let skipLeafKeyRelease = label "skip_leaf_key_release"
    in
    let skipCollisionKeyRelease = label "skip_collision_key_release"
    in
    let collisionKeyLoop = label "collision_key_loop"
    in
    let collisionKeyDone = label "collision_key_done"
    in
    let skipLeafTupleStringListValueRelease = label "skip_leaf_tuple_string_list_value_release"
    in
    let tupleStringListValueDone = label "tuple_string_list_value_done"
    in
    let tupleStringBufferDone = label "tuple_string_buffer_done"
    in
    let skipLeafSumStringValueRelease = label "skip_leaf_sum_string_value_release"
    in
    let sumStringValueDone = label "sum_string_value_done"
    in
    let sumStringBufferDone = label "sum_string_buffer_done"

    in
    let releaseLeafManagedRootValueInstrs (targetHelperLabel: string) (skipLabel: string) =
        [
            Symbolic.CMP_imm (Symbolic.X2, 2);
            Symbolic.B_cond_label (Symbolic.NE, skipLabel);
            Symbolic.STP_pre (Symbolic.X0, Symbolic.X1, Symbolic.SP, -112);
            Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
            Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
            Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
            Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
            Symbolic.STP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
            Symbolic.STR (Symbolic.X30, Symbolic.SP, 96);
            Symbolic.LDR (Symbolic.X0, Symbolic.X3, 8);
            Symbolic.BL targetHelperLabel;
            Symbolic.LDR (Symbolic.X30, Symbolic.SP, 96);
            Symbolic.LDP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
            Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
            Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
            Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
            Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
            Symbolic.LDP_post (Symbolic.X0, Symbolic.X1, Symbolic.SP, 112);
            Symbolic.Label skipLabel
        ]

    in
    let releaseManagedRootValueAtBaseInstrs
        (baseReg: Symbolic.reg)
        (fieldOffset: int)
        (targetHelperLabel: string)
        (skipLabel: string)
        =
        [
            Symbolic.STP_pre (Symbolic.X0, Symbolic.X1, Symbolic.SP, -112);
            Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
            Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
            Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
            Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
            Symbolic.STP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
            Symbolic.STR (Symbolic.X30, Symbolic.SP, 96);
            Symbolic.LDR (Symbolic.X0, baseReg, fieldOffset);
            Symbolic.BL targetHelperLabel;
            Symbolic.LDR (Symbolic.X30, Symbolic.SP, 96);
            Symbolic.LDP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
            Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
            Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
            Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
            Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
            Symbolic.LDP_post (Symbolic.X0, Symbolic.X1, Symbolic.SP, 112);
            Symbolic.Label skipLabel
        ]

    in
    let releaseLeafDynamicBufferFieldInstrs
        (fieldOffset: int)
        (skipLabel: string)
        : Symbolic.instr list =
        let refcountUpdate =
            if isEmpty leakDec then
                [
                    Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
                    Symbolic.STR (Symbolic.X15, Symbolic.X12, 0)
                ]
            else
                [
                    Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
                    Symbolic.STR (Symbolic.X15, Symbolic.X12, 0);
                    Symbolic.CBNZ (Symbolic.X15, skipLabel)
                ] @ leakDec

        in
        [
            Symbolic.CMP_imm (Symbolic.X2, 2);
            Symbolic.B_cond_label (Symbolic.NE, skipLabel);
            Symbolic.LDR (Symbolic.X12, Symbolic.X3, fieldOffset);
            Symbolic.CBZ (Symbolic.X12, skipLabel);
            Symbolic.CMP_reg (Symbolic.X12, Symbolic.X27);
            Symbolic.B_cond_label (Symbolic.LT, skipLabel);
            Symbolic.CMP_reg (Symbolic.X12, Symbolic.X28);
            Symbolic.B_cond_label (Symbolic.GT, skipLabel);
            Symbolic.LDR (Symbolic.X15, Symbolic.X12, 0);
            Symbolic.MOVZ (Symbolic.X13, 0xFFFF, 0);
            Symbolic.MOVK (Symbolic.X13, 0xFFFF, 16);
            Symbolic.MOVK (Symbolic.X13, 0xFFFF, 32);
            Symbolic.MOVK (Symbolic.X13, 0x7FFF, 48);
            Symbolic.CMP_reg (Symbolic.X15, Symbolic.X13);
            Symbolic.B_cond_label (Symbolic.EQ, skipLabel)
        ]
        @ refcountUpdate
        @ [Symbolic.Label skipLabel]

    in
    let releaseDynamicBufferFieldAtBaseInstrs
        (baseReg: Symbolic.reg)
        (fieldOffset: int)
        (operation: MemoryModel.rcOperation)
        (skipLabel: string)
        : Symbolic.instr list =
        let refcountUpdate =
            if isEmpty leakDec then
                [
                    Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
                    Symbolic.STR (Symbolic.X15, Symbolic.X12, 0)
                ]
            else
                [
                    Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
                    Symbolic.STR (Symbolic.X15, Symbolic.X12, 0);
                    Symbolic.CBNZ (Symbolic.X15, skipLabel)
                ] @ leakDec

        in
        let taggedGuard =
            (match operation with
            | MemoryModel.DynamicIntBuffer ->
                [ Symbolic.AND_imm (Symbolic.X13, Symbolic.X12, 1L);
                  Symbolic.CBNZ (Symbolic.X13, skipLabel) ]
            | _ -> []
            )
        in
        [
            Symbolic.LDR (Symbolic.X12, baseReg, fieldOffset);
            Symbolic.CBZ (Symbolic.X12, skipLabel)
        ]
        @ taggedGuard
        @ [
            Symbolic.CMP_reg (Symbolic.X12, Symbolic.X27);
            Symbolic.B_cond_label (Symbolic.LT, skipLabel);
            Symbolic.CMP_reg (Symbolic.X12, Symbolic.X28);
            Symbolic.B_cond_label (Symbolic.GT, skipLabel);
            Symbolic.LDR (Symbolic.X15, Symbolic.X12, 0);
            Symbolic.MOVZ (Symbolic.X13, 0xFFFF, 0);
            Symbolic.MOVK (Symbolic.X13, 0xFFFF, 16);
            Symbolic.MOVK (Symbolic.X13, 0xFFFF, 32);
            Symbolic.MOVK (Symbolic.X13, 0x7FFF, 48);
            Symbolic.CMP_reg (Symbolic.X15, Symbolic.X13);
            Symbolic.B_cond_label (Symbolic.EQ, skipLabel)
        ]
        @ refcountUpdate
        @ [Symbolic.Label skipLabel]

    in
    let rec releasePlanFieldFrom
        (baseReg: Symbolic.reg)
        (fieldOffset: int)
        (path: string)
        (fieldReleasePlan: MemoryModel.rcReleasePlan)
        : Symbolic.instr list =
        (match fieldReleasePlan with
        | MemoryModel.DynamicBufferRelease operation ->
            releaseDynamicBufferFieldAtBaseInstrs
                baseReg
                (int16 fieldOffset)
                operation
                (label (Printf.sprintf "generic_dynamic_%s_%d_done" (path) (fieldOffset)))
        | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
            releaseManagedRootValueAtBaseInstrs
                baseReg
                (int16 fieldOffset)
                (listDecHelperForReleasePlan fieldReleasePlan)
                (label (Printf.sprintf "generic_list_%s_%d_done" (path) (fieldOffset)))
        | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
            releaseManagedRootValueAtBaseInstrs
                baseReg
                (int16 fieldOffset)
                (dictDecHelperForReleasePlan fieldReleasePlan)
                (label (Printf.sprintf "generic_dict_%s_%d_done" (path) (fieldOffset)))
        | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
            releaseManagedRootValueAtBaseInstrs
                baseReg
                (int16 fieldOffset)
                closureRefCountDecHelperLabel
                (label (Printf.sprintf "generic_closure_%s_%d_done" (path) (fieldOffset)))
        | MemoryModel.RecursiveRelease sourceType ->
            releaseManagedRootValueAtBaseInstrs
                baseReg
                (int16 fieldOffset)
                (recursiveNominalRefCountDecHelperLabel sourceType)
                (label (Printf.sprintf "generic_recursive_%s_%d_done" (path) (fieldOffset)))
        | MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease _)
        | MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease _) ->
            releaseGenericValueAtBaseInstrs
                baseReg
                (int16 fieldOffset)
                payloadSize
                fieldReleasePlan
                (Printf.sprintf "%s_%d" (path) (fieldOffset))
        | _ ->
            []

        )
    and releasePlanBoxedSumVariantFieldsFrom
        (baseReg: Symbolic.reg)
        (path: string)
        (variants: MemoryModel.rcBoxedSumVariantRelease list)
        : Symbolic.instr list =
        let releaseVariant (variant: MemoryModel.rcBoxedSumVariantRelease) =
            let releaseInstrs =
                variant.MemoryModel.fieldReleases
                |> List.concat_map (fun (MemoryModel.FieldRelease (fieldOffset, fieldReleasePlan)) ->
                    releasePlanFieldFrom
                        baseReg
                        fieldOffset
                        (Printf.sprintf "%s_tag_%d" (path) (variant.MemoryModel.tag))
                        fieldReleasePlan)

            in
            if isEmpty releaseInstrs then
                None
            else
                Some (variant.MemoryModel.tag, releaseInstrs)

        in
        let cases = variants |> List.filter_map releaseVariant

        in
        if isEmpty cases then
            []
        else
            let sumDone = label (Printf.sprintf "generic_sum_%s_done" (path))
            in
            [
                Symbolic.LDR (Symbolic.X10, baseReg, 0)
            ]
            @
            (cases
             |> List.mapi (fun index (tag, releaseInstrs) ->
                let nextCase = label (Printf.sprintf "generic_sum_%s_variant_%d_next" (path) (index))
                in
                [
                    Symbolic.CMP_imm (Symbolic.X10, uint16 tag);
                    Symbolic.B_cond_label (Symbolic.NE, nextCase)
                ]
                @ releaseInstrs
                @ [
                    Symbolic.B_label sumDone;
                    Symbolic.Label nextCase
                ])
             |> List.concat)
            @ [Symbolic.Label sumDone]

    and releaseGenericValueAtBaseInstrs
        (baseReg: Symbolic.reg)
        (fieldOffset: int)
        (payloadSize: int)
        (releasePlan: MemoryModel.rcReleasePlan)
        (path: string)
        : Symbolic.instr list =
        let genericDone = label (Printf.sprintf "generic_value_%s_done" (path))
        in
        let childFieldReleases =
            (match releasePlan with
            | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases)) ->
                fieldReleases
                |> List.concat_map (fun (MemoryModel.FieldRelease (childOffset, childReleasePlan)) ->
                    releasePlanFieldFrom
                        Symbolic.X11
                        childOffset
                        (Printf.sprintf "%s_%d" (path) (childOffset))
                        childReleasePlan)
            | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease (_, _, variants)) ->
                releasePlanBoxedSumVariantFieldsFrom Symbolic.X11 path variants
            | _ ->
                []

            )
        in
        [
            Symbolic.STP_pre (Symbolic.X0, Symbolic.X1, Symbolic.SP, -112);
            Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
            Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
            Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
            Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
            Symbolic.STP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
            Symbolic.STR (Symbolic.X30, Symbolic.SP, 96);
            Symbolic.LDR (Symbolic.X12, baseReg, fieldOffset);
            Symbolic.CBZ (Symbolic.X12, genericDone);
            Symbolic.LDR (Symbolic.X15, Symbolic.X12, int16 payloadSize);
            Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
            Symbolic.STR (Symbolic.X15, Symbolic.X12, int16 payloadSize);
            Symbolic.CBNZ (Symbolic.X15, genericDone);
            Symbolic.MOV_reg (Symbolic.X11, Symbolic.X12);
            Symbolic.STR (Symbolic.X12, Symbolic.SP, 104)
        ]
        @ childFieldReleases
        @ [
            Symbolic.LDR (Symbolic.X12, Symbolic.SP, 104)
        ]
        @ (if payloadSize >= 0 && payloadSize < 256 then
            [
                Symbolic.ADD_imm (Symbolic.X13, Symbolic.X27, uint16 payloadSize);
                Symbolic.LDR (Symbolic.X14, Symbolic.X13, 0);
                Symbolic.STR (Symbolic.X14, Symbolic.X12, 0);
                Symbolic.STR (Symbolic.X12, Symbolic.X13, 0)
            ]
           else
            [])
        @ leakDec
        @ [
            Symbolic.Label genericDone;
            Symbolic.LDR (Symbolic.X30, Symbolic.SP, 96);
            Symbolic.LDP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
            Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
            Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
            Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
            Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
            Symbolic.LDP_post (Symbolic.X0, Symbolic.X1, Symbolic.SP, 112)
        ]

    in
    let releaseLeafKeyInstrs =
        (match keyReleasePlan with
        | MemoryModel.NoReleasePlan -> []
        | _ ->
            [
                Symbolic.CMP_imm (Symbolic.X2, 2);
                Symbolic.B_cond_label (Symbolic.NE, skipLeafKeyRelease)
            ]
            @ releasePlanFieldFrom Symbolic.X3 0 "leaf_key" keyReleasePlan
            @ [Symbolic.Label skipLeafKeyRelease]

        )
    in
    let releaseCollisionKeyInstrs =
        (match keyReleasePlan with
        | MemoryModel.NoReleasePlan -> []
        | _ ->
            [
                Symbolic.CMP_imm (Symbolic.X2, 3);
                Symbolic.B_cond_label (Symbolic.NE, skipCollisionKeyRelease);
                Symbolic.LDR (Symbolic.X5, Symbolic.X3, 0);
                Symbolic.MOVZ (Symbolic.X6, 0, 0);
                Symbolic.Label collisionKeyLoop;
                Symbolic.CMP_reg (Symbolic.X6, Symbolic.X5);
                Symbolic.B_cond_label (Symbolic.GE, collisionKeyDone);
                Symbolic.LSL_imm (Symbolic.X11, Symbolic.X6, 4);
                Symbolic.ADD_imm (Symbolic.X11, Symbolic.X11, 8);
                Symbolic.ADD_reg (Symbolic.X11, Symbolic.X3, Symbolic.X11)
            ]
            @ releasePlanFieldFrom Symbolic.X11 0 "collision_key" keyReleasePlan
            @ [
                Symbolic.ADD_imm (Symbolic.X6, Symbolic.X6, 1);
                Symbolic.B_label collisionKeyLoop;
                Symbolic.Label collisionKeyDone;
                Symbolic.Label skipCollisionKeyRelease
            ]

        )
    in
    let releaseLeafDynamicValueInstrs =
        if releaseLeafDynamicValue then
            releaseLeafDynamicBufferFieldInstrs 8 skipLeafDynamicValueRelease
        else
            []

    in
    let releaseCollisionDynamicPayloadInstrs =
        if releaseLeafDynamicValue then
            [
                Symbolic.CMP_imm (Symbolic.X2, 3);
                Symbolic.B_cond_label (Symbolic.NE, skipCollisionPayloadRelease);
                Symbolic.LDR (Symbolic.X5, Symbolic.X3, 0);
                Symbolic.MOVZ (Symbolic.X6, 0, 0);
                Symbolic.Label collisionPayloadLoop;
                Symbolic.CMP_reg (Symbolic.X6, Symbolic.X5);
                Symbolic.B_cond_label (Symbolic.GE, collisionPayloadDone);
                Symbolic.LSL_imm (Symbolic.X11, Symbolic.X6, 4);
                Symbolic.ADD_imm (Symbolic.X11, Symbolic.X11, 8);
                Symbolic.ADD_reg (Symbolic.X11, Symbolic.X3, Symbolic.X11)
            ]
            @ (if releaseLeafDynamicValue then
                   releaseDynamicBufferFieldAtBaseInstrs
                       Symbolic.X11
                       8
                       MemoryModel.DynamicStringBuffer
                       skipCollisionDynamicValueRelease
               else
                   [])
            @ [
                Symbolic.ADD_imm (Symbolic.X6, Symbolic.X6, 1);
                Symbolic.B_label collisionPayloadLoop;
                Symbolic.Label collisionPayloadDone;
                Symbolic.Label skipCollisionPayloadRelease
            ]
        else
            []

    in
    let releaseCollisionManagedRootValueInstrs =
        let releases =
            (if releaseLeafListValue then
                 releaseManagedRootValueAtBaseInstrs
                     Symbolic.X11
                     8
                     listRefCountDecHelperLabel
                     skipCollisionListValueRelease
             else
                 [])
            @ (match releaseLeafDictValueHelper with
               | Some targetHelperLabel ->
                   releaseManagedRootValueAtBaseInstrs
                       Symbolic.X11
                       8
                       targetHelperLabel
                       skipCollisionDictValueRelease
               | None ->
                   [])
            @ (if releaseLeafClosureValue then
                   releaseManagedRootValueAtBaseInstrs
                       Symbolic.X11
                       8
                       closureRefCountDecHelperLabel
                       skipCollisionClosureValueRelease
               else
                   [])
            @ (if releaseLeafStreamValue then
                   releaseManagedRootValueAtBaseInstrs
                       Symbolic.X11
                       8
                       streamRefCountDecHelperLabel
                       skipCollisionStreamValueRelease
               else
                   [])

        in
        if isEmpty releases then
            []
        else
            [
                Symbolic.CMP_imm (Symbolic.X2, 3);
                Symbolic.B_cond_label (Symbolic.NE, skipCollisionRootPayloadRelease);
                Symbolic.LDR (Symbolic.X5, Symbolic.X3, 0);
                Symbolic.MOVZ (Symbolic.X6, 0, 0);
                Symbolic.Label collisionRootPayloadLoop;
                Symbolic.CMP_reg (Symbolic.X6, Symbolic.X5);
                Symbolic.B_cond_label (Symbolic.GE, collisionRootPayloadDone);
                Symbolic.LSL_imm (Symbolic.X11, Symbolic.X6, 4);
                Symbolic.ADD_imm (Symbolic.X11, Symbolic.X11, 8);
                Symbolic.ADD_reg (Symbolic.X11, Symbolic.X3, Symbolic.X11)
            ]
            @ releases
            @ [
                Symbolic.ADD_imm (Symbolic.X6, Symbolic.X6, 1);
                Symbolic.B_label collisionRootPayloadLoop;
                Symbolic.Label collisionRootPayloadDone;
                Symbolic.Label skipCollisionRootPayloadRelease
            ]

    in
    let releaseLeafGenericValueInstrs =
        (match leafFixedBlockValueRelease with
        | Some (_, MemoryModel.RecursiveRelease sourceType) ->
            [
                Symbolic.CMP_imm (Symbolic.X2, 2);
                Symbolic.B_cond_label (Symbolic.NE, skipLeafFixedBlockValueRelease)
            ]
            @ releaseManagedRootValueAtBaseInstrs
                Symbolic.X3
                8
                (recursiveNominalRefCountDecHelperLabel sourceType)
                (label "leaf_recursive_value_done")
            @ [Symbolic.Label skipLeafFixedBlockValueRelease]
        | Some (payloadSize, releasePlan) ->
            [
                Symbolic.CMP_imm (Symbolic.X2, 2);
                Symbolic.B_cond_label (Symbolic.NE, skipLeafFixedBlockValueRelease)
            ]
            @ releaseGenericValueAtBaseInstrs
                Symbolic.X3
                8
                payloadSize
                releasePlan
                "leaf_value"
            @ [
                Symbolic.Label skipLeafFixedBlockValueRelease
            ]
        | None ->
            []

        )
    in
    let releaseCollisionGenericValueInstrs =
        (match leafFixedBlockValueRelease with
        | Some (_, MemoryModel.RecursiveRelease sourceType) ->
            [
                Symbolic.CMP_imm (Symbolic.X2, 3);
                Symbolic.B_cond_label (Symbolic.NE, skipCollisionGenericPayloadRelease);
                Symbolic.LDR (Symbolic.X5, Symbolic.X3, 0);
                Symbolic.MOVZ (Symbolic.X6, 0, 0);
                Symbolic.Label collisionGenericPayloadLoop;
                Symbolic.CMP_reg (Symbolic.X6, Symbolic.X5);
                Symbolic.B_cond_label (Symbolic.GE, collisionGenericPayloadDone);
                Symbolic.LSL_imm (Symbolic.X11, Symbolic.X6, 4);
                Symbolic.ADD_imm (Symbolic.X11, Symbolic.X11, 8);
                Symbolic.ADD_reg (Symbolic.X11, Symbolic.X3, Symbolic.X11)
            ]
            @ releaseManagedRootValueAtBaseInstrs
                Symbolic.X11
                8
                (recursiveNominalRefCountDecHelperLabel sourceType)
                (label "collision_recursive_value_done")
            @ [
                Symbolic.ADD_imm (Symbolic.X6, Symbolic.X6, 1);
                Symbolic.B_label collisionGenericPayloadLoop;
                Symbolic.Label collisionGenericPayloadDone;
                Symbolic.Label skipCollisionGenericPayloadRelease
            ]
        | Some (payloadSize, releasePlan) ->
            [
                Symbolic.CMP_imm (Symbolic.X2, 3);
                Symbolic.B_cond_label (Symbolic.NE, skipCollisionGenericPayloadRelease);
                Symbolic.LDR (Symbolic.X5, Symbolic.X3, 0);
                Symbolic.MOVZ (Symbolic.X6, 0, 0);
                Symbolic.Label collisionGenericPayloadLoop;
                Symbolic.CMP_reg (Symbolic.X6, Symbolic.X5);
                Symbolic.B_cond_label (Symbolic.GE, collisionGenericPayloadDone);
                Symbolic.LSL_imm (Symbolic.X11, Symbolic.X6, 4);
                Symbolic.ADD_imm (Symbolic.X11, Symbolic.X11, 8);
                Symbolic.ADD_reg (Symbolic.X11, Symbolic.X3, Symbolic.X11)
            ]
            @ releaseGenericValueAtBaseInstrs
                Symbolic.X11
                8
                payloadSize
                releasePlan
                "collision_value"
            @ [
                Symbolic.ADD_imm (Symbolic.X6, Symbolic.X6, 1);
                Symbolic.B_label collisionGenericPayloadLoop;
                Symbolic.Label collisionGenericPayloadDone;
                Symbolic.Label skipCollisionGenericPayloadRelease
            ]
        | None ->
            []

        )
    in
    let releaseLeafListValueInstrs =
        if releaseLeafListValue then
            releaseLeafManagedRootValueInstrs listRefCountDecHelperLabel skipLeafListValueRelease
        else
            []

    in
    let releaseLeafDictValueInstrs =
        (match releaseLeafDictValueHelper with
        | Some targetHelperLabel ->
            releaseLeafManagedRootValueInstrs targetHelperLabel skipLeafDictValueRelease
        | None ->
            []

        )
    in
    let releaseLeafClosureValueInstrs =
        if releaseLeafClosureValue then
            releaseLeafManagedRootValueInstrs closureRefCountDecHelperLabel skipLeafClosureValueRelease
        else
            []

    in
    let releaseLeafStreamValueInstrs =
        if releaseLeafStreamValue then
            releaseLeafManagedRootValueInstrs streamRefCountDecHelperLabel skipLeafStreamValueRelease
        else
            []

    in
    let releaseLeafTupleStringListValueInstrs =
        if releaseLeafTupleStringListValue || releaseLeafTupleStringListDictValue then
            let tupleRefcountOffset =
                if releaseLeafTupleStringListDictValue then 24 else 16
            in
            let tuplePayloadSize =
                if releaseLeafTupleStringListDictValue then 24 else 16
            in
            let bufferLeakDec = leakDec
            in
            let bufferRefcountUpdate =
                if isEmpty bufferLeakDec then
                    [
                        Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
                        Symbolic.STR (Symbolic.X15, Symbolic.X12, 0)
                    ]
                else
                    [
                        Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
                        Symbolic.STR (Symbolic.X15, Symbolic.X12, 0);
                        Symbolic.CBNZ_offset (Symbolic.X15, 6)
                    ] @ bufferLeakDec
            in
            let tupleLeakDec = leakDec
            in
            [
                Symbolic.CMP_imm (Symbolic.X2, 2);
                Symbolic.B_cond_label (Symbolic.NE, skipLeafTupleStringListValueRelease);
                Symbolic.STP_pre (Symbolic.X0, Symbolic.X1, Symbolic.SP, -112);
                Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                Symbolic.STP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                Symbolic.STR (Symbolic.X30, Symbolic.SP, 96);
                Symbolic.LDR (Symbolic.X11, Symbolic.X3, 8);
                Symbolic.STR (Symbolic.X11, Symbolic.SP, 104);
                Symbolic.CBZ (Symbolic.X11, tupleStringListValueDone);
                Symbolic.LDR (Symbolic.X12, Symbolic.X11, tupleRefcountOffset);
                Symbolic.SUB_imm (Symbolic.X12, Symbolic.X12, 1);
                Symbolic.STR (Symbolic.X12, Symbolic.X11, tupleRefcountOffset);
                Symbolic.CBNZ (Symbolic.X12, tupleStringListValueDone);

                Symbolic.LDR (Symbolic.X12, Symbolic.X11, 0);
                Symbolic.CBZ (Symbolic.X12, tupleStringBufferDone);
                Symbolic.LDR (Symbolic.X15, Symbolic.X12, 0);
                Symbolic.MOVZ (Symbolic.X13, 0xFFFF, 0);
                Symbolic.MOVK (Symbolic.X13, 0xFFFF, 16);
                Symbolic.MOVK (Symbolic.X13, 0xFFFF, 32);
                Symbolic.MOVK (Symbolic.X13, 0x7FFF, 48);
                Symbolic.CMP_reg (Symbolic.X15, Symbolic.X13);
                Symbolic.B_cond_label (Symbolic.EQ, tupleStringBufferDone)
            ]
            @ bufferRefcountUpdate
            @ [
                Symbolic.Label tupleStringBufferDone;
                Symbolic.LDR (Symbolic.X11, Symbolic.SP, 104);
                Symbolic.LDR (Symbolic.X0, Symbolic.X11, 8);
                Symbolic.BL listRefCountDecHelperLabel
            ]
            @ (if releaseLeafTupleStringListDictValue then
                   [
                       Symbolic.LDR (Symbolic.X11, Symbolic.SP, 104);
                       Symbolic.LDR (Symbolic.X0, Symbolic.X11, 16);
                       Symbolic.BL dictRefCountDecHelperLabel
                   ]
               else
                   [])
            @ [
                Symbolic.LDR (Symbolic.X11, Symbolic.SP, 104);
                Symbolic.ADD_imm (Symbolic.X13, Symbolic.X27, tuplePayloadSize);
                Symbolic.LDR (Symbolic.X14, Symbolic.X13, 0);
                Symbolic.STR (Symbolic.X14, Symbolic.X11, 0);
                Symbolic.STR (Symbolic.X11, Symbolic.X13, 0)
            ]
            @ tupleLeakDec
            @ [
                Symbolic.Label tupleStringListValueDone;
                Symbolic.LDR (Symbolic.X30, Symbolic.SP, 96);
                Symbolic.LDP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                Symbolic.LDP_post (Symbolic.X0, Symbolic.X1, Symbolic.SP, 112);
                Symbolic.Label skipLeafTupleStringListValueRelease
            ]
        else
            []

    in
    let releaseLeafSumStringValueInstrs =
        if releaseLeafSumStringValue then
            let bufferLeakDec = leakDec
            in
            let bufferRefcountUpdate =
                if isEmpty bufferLeakDec then
                    [
                        Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
                        Symbolic.STR (Symbolic.X15, Symbolic.X12, 0)
                    ]
                else
                    [
                        Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
                        Symbolic.STR (Symbolic.X15, Symbolic.X12, 0);
                        Symbolic.CBNZ (Symbolic.X15, sumStringBufferDone)
                    ] @ bufferLeakDec
            in
            [
                Symbolic.CMP_imm (Symbolic.X2, 2);
                Symbolic.B_cond_label (Symbolic.NE, skipLeafSumStringValueRelease);
                Symbolic.STP_pre (Symbolic.X0, Symbolic.X1, Symbolic.SP, -112);
                Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                Symbolic.STP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                Symbolic.STR (Symbolic.X30, Symbolic.SP, 96);
                Symbolic.LDR (Symbolic.X11, Symbolic.X3, 8);
                Symbolic.CBZ (Symbolic.X11, sumStringValueDone);
                Symbolic.LDR (Symbolic.X12, Symbolic.X11, 16);
                Symbolic.SUB_imm (Symbolic.X12, Symbolic.X12, 1);
                Symbolic.STR (Symbolic.X12, Symbolic.X11, 16);
                Symbolic.CBNZ (Symbolic.X12, sumStringValueDone);

                Symbolic.LDR (Symbolic.X12, Symbolic.X11, 8);
                Symbolic.CBZ (Symbolic.X12, sumStringBufferDone);
                Symbolic.LDR (Symbolic.X15, Symbolic.X12, 0);
                Symbolic.MOVZ (Symbolic.X13, 0xFFFF, 0);
                Symbolic.MOVK (Symbolic.X13, 0xFFFF, 16);
                Symbolic.MOVK (Symbolic.X13, 0xFFFF, 32);
                Symbolic.MOVK (Symbolic.X13, 0x7FFF, 48);
                Symbolic.CMP_reg (Symbolic.X15, Symbolic.X13);
                Symbolic.B_cond_label (Symbolic.EQ, sumStringBufferDone)
            ]
            @ bufferRefcountUpdate
            @ [
                Symbolic.Label sumStringBufferDone;
                Symbolic.ADD_imm (Symbolic.X13, Symbolic.X27, 16);
                Symbolic.LDR (Symbolic.X14, Symbolic.X13, 0);
                Symbolic.STR (Symbolic.X14, Symbolic.X11, 0);
                Symbolic.STR (Symbolic.X11, Symbolic.X13, 0)
            ]
            @ leakDec
            @ [
                Symbolic.Label sumStringValueDone;
                Symbolic.LDR (Symbolic.X30, Symbolic.SP, 96);
                Symbolic.LDP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                Symbolic.LDP_post (Symbolic.X0, Symbolic.X1, Symbolic.SP, 112);
                Symbolic.Label skipLeafSumStringValueRelease
            ]
        else
            []

    in
    [
        Symbolic.Label helperLabel;

        Symbolic.MOVZ (Symbolic.X1, 0, 0);
        Symbolic.B_label loopCheck;

        Symbolic.Label loopCheck;
        Symbolic.CBZ (Symbolic.X0, popOrRet);
        Symbolic.AND_imm (Symbolic.X2, Symbolic.X0, 3L);
        Symbolic.CBZ (Symbolic.X2, popOrRet);
        Symbolic.CMP_imm (Symbolic.X2, 3);
        Symbolic.B_cond_label (Symbolic.GT, popOrRet);

        Symbolic.AND_imm (Symbolic.X3, Symbolic.X0, 0xFFFFFFFFFFFFFFF8L);
        Symbolic.CMP_reg (Symbolic.X3, Symbolic.X27);
        Symbolic.B_cond_label (Symbolic.LT, popOrRet);
        Symbolic.CMP_reg (Symbolic.X3, Symbolic.X28);
        Symbolic.B_cond_label (Symbolic.GE, popOrRet);

        Symbolic.CMP_imm (Symbolic.X2, 1);
        Symbolic.B_cond_label (Symbolic.EQ, internalTag);
        Symbolic.CMP_imm (Symbolic.X2, 2);
        Symbolic.B_cond_label (Symbolic.EQ, leafTag);
        Symbolic.B_label collisionTag;

        Symbolic.Label leafTag;
        Symbolic.MOVZ (Symbolic.X4, 16, 0);
        Symbolic.MOVZ (Symbolic.X5, 0, 0);
        Symbolic.B_label haveOffset;

        Symbolic.Label collisionTag;
        Symbolic.LDR (Symbolic.X4, Symbolic.X3, 0);
        Symbolic.LSL_imm (Symbolic.X4, Symbolic.X4, 4);
        Symbolic.ADD_imm (Symbolic.X4, Symbolic.X4, 8);
        Symbolic.MOVZ (Symbolic.X5, 0, 0);
        Symbolic.B_label haveOffset;

        Symbolic.Label internalTag;
        Symbolic.LDR (Symbolic.X6, Symbolic.X3, 0);


        Symbolic.FMOV_from_gp (Symbolic.D16, Symbolic.X6);
        Symbolic.CNT_8B (Symbolic.D16, Symbolic.D16);
        Symbolic.ADDV_8B (Symbolic.D16, Symbolic.D16);
        Symbolic.UMOV_byte (Symbolic.X5, Symbolic.D16);
        Symbolic.LSL_imm (Symbolic.X4, Symbolic.X5, 3);
        Symbolic.ADD_imm (Symbolic.X4, Symbolic.X4, 8);

        Symbolic.Label haveOffset;
        Symbolic.ADD_reg (Symbolic.X6, Symbolic.X3, Symbolic.X4);
        Symbolic.LDR (Symbolic.X7, Symbolic.X6, 0);
        Symbolic.SUB_imm (Symbolic.X7, Symbolic.X7, 1);
        Symbolic.STR (Symbolic.X7, Symbolic.X6, 0);
        Symbolic.CBNZ (Symbolic.X7, popOrRet)
    ]
    @ releaseLeafKeyInstrs
    @ releaseLeafDynamicValueInstrs
    @ releaseCollisionDynamicPayloadInstrs
    @ releaseCollisionKeyInstrs
    @ releaseCollisionManagedRootValueInstrs
    @ releaseCollisionGenericValueInstrs
    @ releaseLeafListValueInstrs
    @ releaseLeafDictValueInstrs
    @ releaseLeafClosureValueInstrs
    @ releaseLeafStreamValueInstrs
    @ releaseLeafGenericValueInstrs
    @ releaseLeafTupleStringListValueInstrs
    @ releaseLeafSumStringValueInstrs
    @ [
        Symbolic.CMP_imm (Symbolic.X2, 1);
        Symbolic.B_cond_label (Symbolic.EQ, collectInternal);
        Symbolic.B_label freeNode;

        Symbolic.Label collectInternal;
        Symbolic.MOVZ (Symbolic.X0, 0, 0);
        Symbolic.MOVZ (Symbolic.X6, 0, 0);
        Symbolic.Label collectLoop;
        Symbolic.CMP_reg (Symbolic.X6, Symbolic.X5);
        Symbolic.B_cond_label (Symbolic.GE, freeNode);
        Symbolic.LSL_imm (Symbolic.X7, Symbolic.X6, 3);
        Symbolic.ADD_imm (Symbolic.X7, Symbolic.X7, 8);
        Symbolic.ADD_reg (Symbolic.X7, Symbolic.X3, Symbolic.X7);
        Symbolic.LDR (Symbolic.X8, Symbolic.X7, 0)
    ]
    @ addChild "internal"
    @ [
        Symbolic.ADD_imm (Symbolic.X6, Symbolic.X6, 1);
        Symbolic.B_label collectLoop;

        Symbolic.Label freeNode;
        Symbolic.CMP_imm (Symbolic.X4, 256);
        Symbolic.B_cond_label (Symbolic.GE, skipFreeList);
        Symbolic.ADD_reg (Symbolic.X6, Symbolic.X27, Symbolic.X4);
        Symbolic.LDR (Symbolic.X7, Symbolic.X6, 0);
        Symbolic.STR (Symbolic.X7, Symbolic.X3, 0);
        Symbolic.STR (Symbolic.X3, Symbolic.X6, 0);
        Symbolic.Label skipFreeList;


        Symbolic.CMP_imm (Symbolic.X2, 1);
        Symbolic.B_cond_label (Symbolic.EQ, nextNode);
        Symbolic.MOVZ (Symbolic.X0, 0, 0);
        Symbolic.Label nextNode
    ]
    @ leakDec
    @ [
        Symbolic.B_label loopCheck;

        Symbolic.Label popOrRet;
        Symbolic.CBZ (Symbolic.X1, helperRet);
        Symbolic.LDR (Symbolic.X0, Symbolic.SP, 0);
        Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 16);
        Symbolic.SUB_imm (Symbolic.X1, Symbolic.X1, 1);
        Symbolic.B_label loopCheck;

        Symbolic.Label helperRet;
        Symbolic.RET
    ]

let generatePlannedDictRefCountDecHelper
    (helperLabel: string)
    (releasePlan: MemoryModel.rcReleasePlan)
    (ctx: codeGenContext)
    : Symbolic.instr list =
    (match releasePlan with
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (keyRelease, valueRelease)) ->
        let (
            releaseLeafDynamicValue,
            releaseLeafListValue,
            releaseLeafDictValueHelper,
            releaseLeafClosureValue,
            releaseLeafStreamValue,
            leafFixedBlockValueRelease,
            releaseLeafTupleStringListValue,
            releaseLeafTupleStringListDictValue,
            releaseLeafSumStringValue
            ) =
            (match valueRelease with
            | MemoryModel.NoReleasePlan ->
                false, false, None, false, false, None, false, false, false
            | MemoryModel.DynamicBufferRelease _ ->
                true, false, None, false, false, None, false, false, false
            | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
                false, true, None, false, false, None, false, false, false
            | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
                false, false, Some (dictDecHelperForReleasePlan valueRelease), false, false, None, false, false, false
            | MemoryModel.RecursiveRelease sourceType ->
                false, false, Some (recursiveNominalRefCountDecHelperLabel sourceType), false, false, None, false, false, false
            | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
                false, false, None, true, false, None, false, false, false
            | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) ->
                false, false, None, false, true, None, false, false, false
            | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (16, fieldReleases))
                when releasePlanDynamicOperationAt 0 MemoryModel.DynamicStringBuffer fieldReleases
                     && releasePlanRootKindAt 8 MemoryModel.TaggedList fieldReleases ->
                false, false, None, false, false, Some (16, valueRelease), false, false, false
            | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (24, fieldReleases))
                when releasePlanDynamicOperationAt 0 MemoryModel.DynamicStringBuffer fieldReleases
                     && releasePlanRootKindAt 8 MemoryModel.TaggedList fieldReleases
                     && releasePlanRootKindAt 16 MemoryModel.DictHeap fieldReleases ->
                false, false, None, false, false, Some (24, valueRelease), false, false, false
            | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease (_, fieldReleases, _))
                when releasePlanDynamicOperationAt 8 MemoryModel.DynamicStringBuffer fieldReleases ->
                false, false, None, false, false, Some (16, valueRelease), false, false, false
            | MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, _) ->
                false, false, None, false, false, Some (payloadSize, valueRelease), false, false, false

            )
        in
        generateDictRefCountDecHelper
            helperLabel
            keyRelease
            releaseLeafDynamicValue
            releaseLeafListValue
            releaseLeafDictValueHelper
            releaseLeafClosureValue
            releaseLeafStreamValue
            leafFixedBlockValueRelease
            releaseLeafTupleStringListValue
            releaseLeafTupleStringListDictValue
            releaseLeafSumStringValue
            ctx
    | other ->
        Crash.crash (Printf.sprintf "ARM64 planned dict RefCountDec helper requires a DictHeap release plan, got %s" (planText other))
    )
