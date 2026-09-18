// DictReferenceCounts.fs - Generate HAMT root and recursive payload lifetime helpers.

module ARM64DictReferenceCounts

open ARM64CodeGenTypes
open ARM64HeapAllocation
open ARM64ReleaseSelection

let internal generateDictRefCountIncHelper () : ARM64Symbolic.Instr list =
    let label (name: string) : string = $"__dark_dict_rc_inc_{name}"
    let internalTag = label "internal"
    let leafTag = label "leaf"
    let collisionTag = label "collision"
    let haveOffset = label "have_offset"
    let helperRet = label "ret"

    [
        ARM64Symbolic.Label dictRefCountIncHelperLabel
        // X0 = tagged HAMT root. Tags: 1 internal, 2 leaf, 3 collision.
        ARM64Symbolic.CBZ (ARM64Symbolic.X0, helperRet)
        ARM64Symbolic.AND_imm (ARM64Symbolic.X1, ARM64Symbolic.X0, 3UL)
        ARM64Symbolic.CBZ (ARM64Symbolic.X1, helperRet)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X1, 3us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, helperRet)
        // This contiguous all-ones mask is an encodable AArch64 logical immediate.
        ARM64Symbolic.AND_imm (ARM64Symbolic.X2, ARM64Symbolic.X0, 0xFFFFFFFFFFFFFFF8UL)
        ARM64Symbolic.CMP_reg (ARM64Symbolic.X2, ARM64Symbolic.X27)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, helperRet)
        ARM64Symbolic.CMP_reg (ARM64Symbolic.X2, ARM64Symbolic.X28)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, helperRet)

        ARM64Symbolic.CMP_imm (ARM64Symbolic.X1, 1us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, internalTag)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X1, 2us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, leafTag)
        ARM64Symbolic.B_label collisionTag

        ARM64Symbolic.Label leafTag
        ARM64Symbolic.MOVZ (ARM64Symbolic.X3, 16us, 0)
        ARM64Symbolic.B_label haveOffset

        ARM64Symbolic.Label collisionTag
        ARM64Symbolic.LDR (ARM64Symbolic.X3, ARM64Symbolic.X2, 0s)
        ARM64Symbolic.LSL_imm (ARM64Symbolic.X3, ARM64Symbolic.X3, 4)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X3, ARM64Symbolic.X3, 8us)
        ARM64Symbolic.B_label haveOffset

        ARM64Symbolic.Label internalTag
        ARM64Symbolic.LDR (ARM64Symbolic.X4, ARM64Symbolic.X2, 0s) // bitmap
        // D16 is reserved scratch: count the bitmap bytes, horizontally add
        // them, and zero-extend the resulting byte into the child count.
        ARM64Symbolic.FMOV_from_gp (ARM64Symbolic.D16, ARM64Symbolic.X4)
        ARM64Symbolic.CNT_8B (ARM64Symbolic.D16, ARM64Symbolic.D16)
        ARM64Symbolic.ADDV_8B (ARM64Symbolic.D16, ARM64Symbolic.D16)
        ARM64Symbolic.UMOV_byte (ARM64Symbolic.X3, ARM64Symbolic.D16)
        ARM64Symbolic.LSL_imm (ARM64Symbolic.X3, ARM64Symbolic.X3, 3)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X3, ARM64Symbolic.X3, 8us)

        ARM64Symbolic.Label haveOffset
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X4, ARM64Symbolic.X2, ARM64Symbolic.X3)
        ARM64Symbolic.LDR (ARM64Symbolic.X5, ARM64Symbolic.X4, 0s)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X5, ARM64Symbolic.X5, 1us)
        ARM64Symbolic.STR (ARM64Symbolic.X5, ARM64Symbolic.X4, 0s)

        ARM64Symbolic.Label helperRet
        ARM64Symbolic.RET
    ]

let internal generateDictRefCountDecHelper
    (helperLabel: string)
    (keyReleasePlan: MemoryModel.RcReleasePlan)
    (releaseLeafDynamicValue: bool)
    (releaseLeafListValue: bool)
    (releaseLeafDictValueHelper: string option)
    (releaseLeafClosureValue: bool)
    (releaseLeafStreamValue: bool)
    (leafFixedBlockValueRelease: (int * MemoryModel.RcReleasePlan) option)
    (releaseLeafTupleStringListValue: bool)
    (releaseLeafTupleStringListDictValue: bool)
    (releaseLeafSumStringValue: bool)
    (ctx: CodeGenContext)
    : ARM64Symbolic.Instr list =
    let label (name: string) : string = $"{helperLabel}_{name}"
    let leakDec =
        if ctx.Options.EnableLeakCheck then
            let labelRef = dataLabel leakCounterLabel
            [
                ARM64Symbolic.ADRP (ARM64Symbolic.X17, labelRef)
                ARM64Symbolic.ADD_label (ARM64Symbolic.X17, ARM64Symbolic.X17, labelRef)
                ARM64Symbolic.LDR (ARM64Symbolic.X16, ARM64Symbolic.X17, 0s)
                ARM64Symbolic.SUB_imm (ARM64Symbolic.X16, ARM64Symbolic.X16, 1us)
                ARM64Symbolic.STR (ARM64Symbolic.X16, ARM64Symbolic.X17, 0s)
            ]
        else
            []

    let addChild (suffix: string) : ARM64Symbolic.Instr list =
        let doneLabel = label $"child_done_{suffix}"
        let pushLabel = label $"child_push_{suffix}"
        [
            ARM64Symbolic.CBZ (ARM64Symbolic.X8, doneLabel)
            ARM64Symbolic.AND_imm (ARM64Symbolic.X9, ARM64Symbolic.X8, 3UL)
            ARM64Symbolic.CBZ (ARM64Symbolic.X9, doneLabel)
            ARM64Symbolic.CMP_imm (ARM64Symbolic.X9, 3us)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, doneLabel)
            // This contiguous all-ones mask is an encodable AArch64 logical immediate.
            ARM64Symbolic.AND_imm (ARM64Symbolic.X10, ARM64Symbolic.X8, 0xFFFFFFFFFFFFFFF8UL)
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X10, ARM64Symbolic.X27)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, doneLabel)
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X10, ARM64Symbolic.X28)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, doneLabel)
            ARM64Symbolic.CBNZ (ARM64Symbolic.X0, pushLabel)
            ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.X8)
            ARM64Symbolic.B_label doneLabel
            ARM64Symbolic.Label pushLabel
            ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
            ARM64Symbolic.STR (ARM64Symbolic.X8, ARM64Symbolic.SP, 0s)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.X1, 1us)
            ARM64Symbolic.Label doneLabel
        ]

    let loopCheck = label "loop_check"
    let popOrRet = label "pop_or_ret"
    let helperRet = label "ret"
    let internalTag = label "internal"
    let leafTag = label "leaf"
    let collisionTag = label "collision"
    let haveOffset = label "have_offset"
    let collectInternal = label "collect_internal"
    let collectLoop = label "collect_loop"
    let freeNode = label "free_node"
    let skipFreeList = label "skip_freelist"
    let skipLeafListValueRelease = label "skip_leaf_list_value_release"
    let skipLeafDictValueRelease = label "skip_leaf_dict_value_release"
    let skipLeafClosureValueRelease = label "skip_leaf_closure_value_release"
    let skipLeafStreamValueRelease = label "skip_leaf_stream_value_release"
    let skipLeafDynamicKeyRelease = label "skip_leaf_dynamic_key_release"
    let skipLeafDynamicValueRelease = label "skip_leaf_dynamic_value_release"
    let skipCollisionPayloadRelease = label "skip_collision_payload_release"
    let collisionPayloadLoop = label "collision_payload_loop"
    let collisionPayloadDone = label "collision_payload_done"
    let skipCollisionDynamicKeyRelease = label "skip_collision_dynamic_key_release"
    let skipCollisionDynamicValueRelease = label "skip_collision_dynamic_value_release"
    let skipCollisionRootPayloadRelease = label "skip_collision_root_payload_release"
    let collisionRootPayloadLoop = label "collision_root_payload_loop"
    let collisionRootPayloadDone = label "collision_root_payload_done"
    let skipCollisionListValueRelease = label "skip_collision_list_value_release"
    let skipCollisionDictValueRelease = label "skip_collision_dict_value_release"
    let skipCollisionClosureValueRelease = label "skip_collision_closure_value_release"
    let skipCollisionStreamValueRelease = label "skip_collision_stream_value_release"
    let skipLeafFixedBlockValueRelease = label "skip_leaf_fixed_block_value_release"
    let skipCollisionGenericPayloadRelease = label "skip_collision_generic_payload_release"
    let collisionGenericPayloadLoop = label "collision_generic_payload_loop"
    let collisionGenericPayloadDone = label "collision_generic_payload_done"
    let skipLeafKeyRelease = label "skip_leaf_key_release"
    let skipCollisionKeyRelease = label "skip_collision_key_release"
    let collisionKeyLoop = label "collision_key_loop"
    let collisionKeyDone = label "collision_key_done"
    let skipLeafTupleStringListValueRelease = label "skip_leaf_tuple_string_list_value_release"
    let tupleStringListValueDone = label "tuple_string_list_value_done"
    let skipLeafSumStringValueRelease = label "skip_leaf_sum_string_value_release"
    let sumStringValueDone = label "sum_string_value_done"
    let sumStringBufferDone = label "sum_string_buffer_done"

    let releaseLeafManagedRootValueInstrs (targetHelperLabel: string) (skipLabel: string) =
        [
            ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 2us)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, skipLabel)
            ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -112s)
            ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
            ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
            ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
            ARM64Symbolic.STP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
            ARM64Symbolic.STR (ARM64Symbolic.X30, ARM64Symbolic.SP, 96s)
            ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X3, 8s)
            ARM64Symbolic.BL targetHelperLabel
            ARM64Symbolic.LDR (ARM64Symbolic.X30, ARM64Symbolic.SP, 96s)
            ARM64Symbolic.LDP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
            ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
            ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
            ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
            ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 112s)
            ARM64Symbolic.Label skipLabel
        ]

    let releaseManagedRootValueAtBaseInstrs
        (baseReg: ARM64.Reg)
        (fieldOffset: int16)
        (targetHelperLabel: string)
        (skipLabel: string)
        =
        [
            ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -112s)
            ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
            ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
            ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
            ARM64Symbolic.STP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
            ARM64Symbolic.STR (ARM64Symbolic.X30, ARM64Symbolic.SP, 96s)
            ARM64Symbolic.LDR (ARM64Symbolic.X0, baseReg, fieldOffset)
            ARM64Symbolic.BL targetHelperLabel
            ARM64Symbolic.LDR (ARM64Symbolic.X30, ARM64Symbolic.SP, 96s)
            ARM64Symbolic.LDP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
            ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
            ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
            ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
            ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 112s)
            ARM64Symbolic.Label skipLabel
        ]

    let releaseLeafDynamicBufferFieldInstrs
        (fieldOffset: int16)
        (skipLabel: string)
        : ARM64Symbolic.Instr list =
        let refcountUpdate =
            if List.isEmpty leakDec then
                [
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
                    ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)
                ]
            else
                [
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
                    ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)
                    ARM64Symbolic.CBNZ (ARM64Symbolic.X15, skipLabel)
                ] @ leakDec

        [
            ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 2us)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, skipLabel)
            ARM64Symbolic.LDR (ARM64Symbolic.X12, ARM64Symbolic.X3, fieldOffset)
            ARM64Symbolic.CBZ (ARM64Symbolic.X12, skipLabel)
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X12, ARM64Symbolic.X27)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, skipLabel)
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X12, ARM64Symbolic.X28)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, skipLabel)
            ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)
            ARM64Symbolic.MOVZ (ARM64Symbolic.X13, 0xFFFFus, 0)
            ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 16)
            ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 32)
            ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0x7FFFus, 48)
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X15, ARM64Symbolic.X13)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, skipLabel)
        ]
        @ refcountUpdate
        @ [ARM64Symbolic.Label skipLabel]

    let releaseDynamicBufferFieldAtBaseInstrs
        (baseReg: ARM64.Reg)
        (fieldOffset: int16)
        (skipLabel: string)
        : ARM64Symbolic.Instr list =
        let refcountUpdate =
            if List.isEmpty leakDec then
                [
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
                    ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)
                ]
            else
                [
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
                    ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)
                    ARM64Symbolic.CBNZ (ARM64Symbolic.X15, skipLabel)
                ] @ leakDec

        [
            ARM64Symbolic.LDR (ARM64Symbolic.X12, baseReg, fieldOffset)
            ARM64Symbolic.CBZ (ARM64Symbolic.X12, skipLabel)
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X12, ARM64Symbolic.X27)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, skipLabel)
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X12, ARM64Symbolic.X28)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, skipLabel)
            ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)
            ARM64Symbolic.MOVZ (ARM64Symbolic.X13, 0xFFFFus, 0)
            ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 16)
            ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 32)
            ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0x7FFFus, 48)
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X15, ARM64Symbolic.X13)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, skipLabel)
        ]
        @ refcountUpdate
        @ [ARM64Symbolic.Label skipLabel]

    let rec releasePlanFieldFrom
        (baseReg: ARM64.Reg)
        (fieldOffset: int)
        (path: string)
        (fieldReleasePlan: MemoryModel.RcReleasePlan)
        : ARM64Symbolic.Instr list =
        match fieldReleasePlan with
        | MemoryModel.DynamicBufferRelease _ ->
            releaseDynamicBufferFieldAtBaseInstrs baseReg (int16 fieldOffset) (label $"generic_dynamic_{path}_{fieldOffset}_done")
        | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
            releaseManagedRootValueAtBaseInstrs
                baseReg
                (int16 fieldOffset)
                (listDecHelperForReleasePlan fieldReleasePlan)
                (label $"generic_list_{path}_{fieldOffset}_done")
        | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
            releaseManagedRootValueAtBaseInstrs
                baseReg
                (int16 fieldOffset)
                (dictDecHelperForReleasePlan fieldReleasePlan)
                (label $"generic_dict_{path}_{fieldOffset}_done")
        | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
            releaseManagedRootValueAtBaseInstrs
                baseReg
                (int16 fieldOffset)
                closureRefCountDecHelperLabel
                (label $"generic_closure_{path}_{fieldOffset}_done")
        | MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease _)
        | MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease _) ->
            releaseGenericValueAtBaseInstrs
                baseReg
                (int16 fieldOffset)
                payloadSize
                fieldReleasePlan
                $"{path}_{fieldOffset}"
        | _ ->
            []

    and releasePlanBoxedSumVariantFieldsFrom
        (baseReg: ARM64.Reg)
        (path: string)
        (variants: MemoryModel.RcBoxedSumVariantRelease list)
        : ARM64Symbolic.Instr list =
        let releaseVariant (variant: MemoryModel.RcBoxedSumVariantRelease) =
            let releaseInstrs =
                variant.FieldReleases
                |> List.collect (fun (MemoryModel.FieldRelease (fieldOffset, fieldReleasePlan)) ->
                    releasePlanFieldFrom
                        baseReg
                        fieldOffset
                        $"{path}_tag_{variant.Tag}"
                        fieldReleasePlan)

            if List.isEmpty releaseInstrs then
                None
            else
                Some (variant.Tag, releaseInstrs)

        let cases = variants |> List.choose releaseVariant

        if List.isEmpty cases then
            []
        else
            let sumDone = label $"generic_sum_{path}_done"
            [
                ARM64Symbolic.LDR (ARM64Symbolic.X10, baseReg, 0s)
            ]
            @
            (cases
             |> List.mapi (fun index (tag, releaseInstrs) ->
                let nextCase = label $"generic_sum_{path}_variant_{index}_next"
                [
                    ARM64Symbolic.CMP_imm (ARM64Symbolic.X10, uint16 tag)
                    ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, nextCase)
                ]
                @ releaseInstrs
                @ [
                    ARM64Symbolic.B_label sumDone
                    ARM64Symbolic.Label nextCase
                ])
             |> List.concat)
            @ [ARM64Symbolic.Label sumDone]

    and releaseGenericValueAtBaseInstrs
        (baseReg: ARM64.Reg)
        (fieldOffset: int16)
        (payloadSize: int)
        (releasePlan: MemoryModel.RcReleasePlan)
        (path: string)
        : ARM64Symbolic.Instr list =
        let genericDone = label $"generic_value_{path}_done"
        let childFieldReleases =
            match releasePlan with
            | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases)) ->
                fieldReleases
                |> List.collect (fun (MemoryModel.FieldRelease (childOffset, childReleasePlan)) ->
                    releasePlanFieldFrom
                        ARM64Symbolic.X11
                        childOffset
                        $"{path}_{childOffset}"
                        childReleasePlan)
            | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease (_, _, variants)) ->
                releasePlanBoxedSumVariantFieldsFrom ARM64Symbolic.X11 path variants
            | _ ->
                []

        [
            ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -112s)
            ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
            ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
            ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
            ARM64Symbolic.STP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
            ARM64Symbolic.STR (ARM64Symbolic.X30, ARM64Symbolic.SP, 96s)
            ARM64Symbolic.LDR (ARM64Symbolic.X12, baseReg, fieldOffset)
            ARM64Symbolic.CBZ (ARM64Symbolic.X12, genericDone)
            ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X12, int16 payloadSize)
            ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
            ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X12, int16 payloadSize)
            ARM64Symbolic.CBNZ (ARM64Symbolic.X15, genericDone)
            ARM64Symbolic.MOV_reg (ARM64Symbolic.X11, ARM64Symbolic.X12)
            ARM64Symbolic.STR (ARM64Symbolic.X12, ARM64Symbolic.SP, 104s)
        ]
        @ childFieldReleases
        @ [
            ARM64Symbolic.LDR (ARM64Symbolic.X12, ARM64Symbolic.SP, 104s)
        ]
        @ (if payloadSize >= 0 && payloadSize < 256 then
            [
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X27, uint16 payloadSize)
                ARM64Symbolic.LDR (ARM64Symbolic.X14, ARM64Symbolic.X13, 0s)
                ARM64Symbolic.STR (ARM64Symbolic.X14, ARM64Symbolic.X12, 0s)
                ARM64Symbolic.STR (ARM64Symbolic.X12, ARM64Symbolic.X13, 0s)
            ]
           else
            [])
        @ leakDec
        @ [
            ARM64Symbolic.Label genericDone
            ARM64Symbolic.LDR (ARM64Symbolic.X30, ARM64Symbolic.SP, 96s)
            ARM64Symbolic.LDP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
            ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
            ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
            ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
            ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 112s)
        ]

    let releaseLeafKeyInstrs =
        match keyReleasePlan with
        | MemoryModel.NoReleasePlan -> []
        | _ ->
            [
                ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 2us)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, skipLeafKeyRelease)
            ]
            @ releasePlanFieldFrom ARM64Symbolic.X3 0 "leaf_key" keyReleasePlan
            @ [ARM64Symbolic.Label skipLeafKeyRelease]

    let releaseCollisionKeyInstrs =
        match keyReleasePlan with
        | MemoryModel.NoReleasePlan -> []
        | _ ->
            [
                ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 3us)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, skipCollisionKeyRelease)
                ARM64Symbolic.LDR (ARM64Symbolic.X5, ARM64Symbolic.X3, 0s)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X6, 0us, 0)
                ARM64Symbolic.Label collisionKeyLoop
                ARM64Symbolic.CMP_reg (ARM64Symbolic.X6, ARM64Symbolic.X5)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, collisionKeyDone)
                ARM64Symbolic.LSL_imm (ARM64Symbolic.X11, ARM64Symbolic.X6, 4)
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X11, ARM64Symbolic.X11, 8us)
                ARM64Symbolic.ADD_reg (ARM64Symbolic.X11, ARM64Symbolic.X3, ARM64Symbolic.X11)
            ]
            @ releasePlanFieldFrom ARM64Symbolic.X11 0 "collision_key" keyReleasePlan
            @ [
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X6, ARM64Symbolic.X6, 1us)
                ARM64Symbolic.B_label collisionKeyLoop
                ARM64Symbolic.Label collisionKeyDone
                ARM64Symbolic.Label skipCollisionKeyRelease
            ]

    let releaseLeafDynamicValueInstrs =
        if releaseLeafDynamicValue then
            releaseLeafDynamicBufferFieldInstrs 8s skipLeafDynamicValueRelease
        else
            []

    let releaseCollisionDynamicPayloadInstrs =
        if releaseLeafDynamicValue then
            [
                ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 3us)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, skipCollisionPayloadRelease)
                ARM64Symbolic.LDR (ARM64Symbolic.X5, ARM64Symbolic.X3, 0s)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X6, 0us, 0)
                ARM64Symbolic.Label collisionPayloadLoop
                ARM64Symbolic.CMP_reg (ARM64Symbolic.X6, ARM64Symbolic.X5)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, collisionPayloadDone)
                ARM64Symbolic.LSL_imm (ARM64Symbolic.X11, ARM64Symbolic.X6, 4)
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X11, ARM64Symbolic.X11, 8us)
                ARM64Symbolic.ADD_reg (ARM64Symbolic.X11, ARM64Symbolic.X3, ARM64Symbolic.X11)
            ]
            @ (if releaseLeafDynamicValue then
                   releaseDynamicBufferFieldAtBaseInstrs ARM64Symbolic.X11 8s skipCollisionDynamicValueRelease
               else
                   [])
            @ [
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X6, ARM64Symbolic.X6, 1us)
                ARM64Symbolic.B_label collisionPayloadLoop
                ARM64Symbolic.Label collisionPayloadDone
                ARM64Symbolic.Label skipCollisionPayloadRelease
            ]
        else
            []

    let releaseCollisionManagedRootValueInstrs =
        let releases =
            (if releaseLeafListValue then
                 releaseManagedRootValueAtBaseInstrs
                     ARM64Symbolic.X11
                     8s
                     listRefCountDecHelperLabel
                     skipCollisionListValueRelease
             else
                 [])
            @ (match releaseLeafDictValueHelper with
               | Some targetHelperLabel ->
                   releaseManagedRootValueAtBaseInstrs
                       ARM64Symbolic.X11
                       8s
                       targetHelperLabel
                       skipCollisionDictValueRelease
               | None ->
                   [])
            @ (if releaseLeafClosureValue then
                   releaseManagedRootValueAtBaseInstrs
                       ARM64Symbolic.X11
                       8s
                       closureRefCountDecHelperLabel
                       skipCollisionClosureValueRelease
               else
                   [])
            @ (if releaseLeafStreamValue then
                   releaseManagedRootValueAtBaseInstrs
                       ARM64Symbolic.X11
                       8s
                       streamRefCountDecHelperLabel
                       skipCollisionStreamValueRelease
               else
                   [])

        if List.isEmpty releases then
            []
        else
            [
                ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 3us)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, skipCollisionRootPayloadRelease)
                ARM64Symbolic.LDR (ARM64Symbolic.X5, ARM64Symbolic.X3, 0s)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X6, 0us, 0)
                ARM64Symbolic.Label collisionRootPayloadLoop
                ARM64Symbolic.CMP_reg (ARM64Symbolic.X6, ARM64Symbolic.X5)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, collisionRootPayloadDone)
                ARM64Symbolic.LSL_imm (ARM64Symbolic.X11, ARM64Symbolic.X6, 4)
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X11, ARM64Symbolic.X11, 8us)
                ARM64Symbolic.ADD_reg (ARM64Symbolic.X11, ARM64Symbolic.X3, ARM64Symbolic.X11)
            ]
            @ releases
            @ [
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X6, ARM64Symbolic.X6, 1us)
                ARM64Symbolic.B_label collisionRootPayloadLoop
                ARM64Symbolic.Label collisionRootPayloadDone
                ARM64Symbolic.Label skipCollisionRootPayloadRelease
            ]

    let releaseLeafGenericValueInstrs =
        match leafFixedBlockValueRelease with
        | Some (payloadSize, releasePlan) ->
            [
                ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 2us)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, skipLeafFixedBlockValueRelease)
            ]
            @ releaseGenericValueAtBaseInstrs
                ARM64Symbolic.X3
                8s
                payloadSize
                releasePlan
                "leaf_value"
            @ [
                ARM64Symbolic.Label skipLeafFixedBlockValueRelease
            ]
        | None ->
            []

    let releaseCollisionGenericValueInstrs =
        match leafFixedBlockValueRelease with
        | Some (payloadSize, releasePlan) ->
            [
                ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 3us)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, skipCollisionGenericPayloadRelease)
                ARM64Symbolic.LDR (ARM64Symbolic.X5, ARM64Symbolic.X3, 0s)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X6, 0us, 0)
                ARM64Symbolic.Label collisionGenericPayloadLoop
                ARM64Symbolic.CMP_reg (ARM64Symbolic.X6, ARM64Symbolic.X5)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, collisionGenericPayloadDone)
                ARM64Symbolic.LSL_imm (ARM64Symbolic.X11, ARM64Symbolic.X6, 4)
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X11, ARM64Symbolic.X11, 8us)
                ARM64Symbolic.ADD_reg (ARM64Symbolic.X11, ARM64Symbolic.X3, ARM64Symbolic.X11)
            ]
            @ releaseGenericValueAtBaseInstrs
                ARM64Symbolic.X11
                8s
                payloadSize
                releasePlan
                "collision_value"
            @ [
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X6, ARM64Symbolic.X6, 1us)
                ARM64Symbolic.B_label collisionGenericPayloadLoop
                ARM64Symbolic.Label collisionGenericPayloadDone
                ARM64Symbolic.Label skipCollisionGenericPayloadRelease
            ]
        | None ->
            []

    let releaseLeafListValueInstrs =
        if releaseLeafListValue then
            releaseLeafManagedRootValueInstrs listRefCountDecHelperLabel skipLeafListValueRelease
        else
            []

    let releaseLeafDictValueInstrs =
        match releaseLeafDictValueHelper with
        | Some targetHelperLabel ->
            releaseLeafManagedRootValueInstrs targetHelperLabel skipLeafDictValueRelease
        | None ->
            []

    let releaseLeafClosureValueInstrs =
        if releaseLeafClosureValue then
            releaseLeafManagedRootValueInstrs closureRefCountDecHelperLabel skipLeafClosureValueRelease
        else
            []

    let releaseLeafStreamValueInstrs =
        if releaseLeafStreamValue then
            releaseLeafManagedRootValueInstrs streamRefCountDecHelperLabel skipLeafStreamValueRelease
        else
            []

    let releaseLeafTupleStringListValueInstrs =
        if releaseLeafTupleStringListValue || releaseLeafTupleStringListDictValue then
            let tupleRefcountOffset =
                if releaseLeafTupleStringListDictValue then 24s else 16s
            let tuplePayloadSize =
                if releaseLeafTupleStringListDictValue then 24us else 16us
            let bufferLeakDec = leakDec
            let bufferRefcountUpdate =
                if List.isEmpty bufferLeakDec then
                    [
                        ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
                        ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)
                    ]
                else
                    [
                        ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
                        ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)
                        ARM64Symbolic.CBNZ_offset (ARM64Symbolic.X15, 6)
                    ] @ bufferLeakDec
            let tupleLeakDec = leakDec
            [
                ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 2us)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, skipLeafTupleStringListValueRelease)
                ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -112s)
                ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                ARM64Symbolic.STP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                ARM64Symbolic.STR (ARM64Symbolic.X30, ARM64Symbolic.SP, 96s)
                ARM64Symbolic.LDR (ARM64Symbolic.X11, ARM64Symbolic.X3, 8s)
                ARM64Symbolic.STR (ARM64Symbolic.X11, ARM64Symbolic.SP, 104s)
                ARM64Symbolic.CBZ (ARM64Symbolic.X11, tupleStringListValueDone)
                ARM64Symbolic.LDR (ARM64Symbolic.X12, ARM64Symbolic.X11, tupleRefcountOffset)
                ARM64Symbolic.SUB_imm (ARM64Symbolic.X12, ARM64Symbolic.X12, 1us)
                ARM64Symbolic.STR (ARM64Symbolic.X12, ARM64Symbolic.X11, tupleRefcountOffset)
                ARM64Symbolic.CBNZ (ARM64Symbolic.X12, tupleStringListValueDone)

                ARM64Symbolic.LDR (ARM64Symbolic.X12, ARM64Symbolic.X11, 0s)
                ARM64Symbolic.CBZ_offset (ARM64Symbolic.X12, 7 + List.length bufferRefcountUpdate)
                ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X13, 0xFFFFus, 0)
                ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 16)
                ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 32)
                ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0x7FFFus, 48)
                ARM64Symbolic.CMP_reg (ARM64Symbolic.X15, ARM64Symbolic.X13)
                ARM64Symbolic.B_cond (ARM64Symbolic.EQ, List.length bufferRefcountUpdate + 1)
            ]
            @ bufferRefcountUpdate
            @ [
                ARM64Symbolic.LDR (ARM64Symbolic.X11, ARM64Symbolic.SP, 104s)
                ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X11, 8s)
                ARM64Symbolic.BL listRefCountDecHelperLabel
            ]
            @ (if releaseLeafTupleStringListDictValue then
                   [
                       ARM64Symbolic.LDR (ARM64Symbolic.X11, ARM64Symbolic.SP, 104s)
                       ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X11, 16s)
                       ARM64Symbolic.BL dictRefCountDecHelperLabel
                   ]
               else
                   [])
            @ [
                ARM64Symbolic.LDR (ARM64Symbolic.X11, ARM64Symbolic.SP, 104s)
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X27, tuplePayloadSize)
                ARM64Symbolic.LDR (ARM64Symbolic.X14, ARM64Symbolic.X13, 0s)
                ARM64Symbolic.STR (ARM64Symbolic.X14, ARM64Symbolic.X11, 0s)
                ARM64Symbolic.STR (ARM64Symbolic.X11, ARM64Symbolic.X13, 0s)
            ]
            @ tupleLeakDec
            @ [
                ARM64Symbolic.Label tupleStringListValueDone
                ARM64Symbolic.LDR (ARM64Symbolic.X30, ARM64Symbolic.SP, 96s)
                ARM64Symbolic.LDP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 112s)
                ARM64Symbolic.Label skipLeafTupleStringListValueRelease
            ]
        else
            []

    let releaseLeafSumStringValueInstrs =
        if releaseLeafSumStringValue then
            let bufferLeakDec = leakDec
            let bufferRefcountUpdate =
                if List.isEmpty bufferLeakDec then
                    [
                        ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
                        ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)
                    ]
                else
                    [
                        ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
                        ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)
                        ARM64Symbolic.CBNZ (ARM64Symbolic.X15, sumStringBufferDone)
                    ] @ bufferLeakDec
            [
                ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 2us)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, skipLeafSumStringValueRelease)
                ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -112s)
                ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                ARM64Symbolic.STP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                ARM64Symbolic.STR (ARM64Symbolic.X30, ARM64Symbolic.SP, 96s)
                ARM64Symbolic.LDR (ARM64Symbolic.X11, ARM64Symbolic.X3, 8s)
                ARM64Symbolic.CBZ (ARM64Symbolic.X11, sumStringValueDone)
                ARM64Symbolic.LDR (ARM64Symbolic.X12, ARM64Symbolic.X11, 16s)
                ARM64Symbolic.SUB_imm (ARM64Symbolic.X12, ARM64Symbolic.X12, 1us)
                ARM64Symbolic.STR (ARM64Symbolic.X12, ARM64Symbolic.X11, 16s)
                ARM64Symbolic.CBNZ (ARM64Symbolic.X12, sumStringValueDone)

                ARM64Symbolic.LDR (ARM64Symbolic.X12, ARM64Symbolic.X11, 8s)
                ARM64Symbolic.CBZ (ARM64Symbolic.X12, sumStringBufferDone)
                ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X13, 0xFFFFus, 0)
                ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 16)
                ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 32)
                ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0x7FFFus, 48)
                ARM64Symbolic.CMP_reg (ARM64Symbolic.X15, ARM64Symbolic.X13)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, sumStringBufferDone)
            ]
            @ bufferRefcountUpdate
            @ [
                ARM64Symbolic.Label sumStringBufferDone
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X27, 16us)
                ARM64Symbolic.LDR (ARM64Symbolic.X14, ARM64Symbolic.X13, 0s)
                ARM64Symbolic.STR (ARM64Symbolic.X14, ARM64Symbolic.X11, 0s)
                ARM64Symbolic.STR (ARM64Symbolic.X11, ARM64Symbolic.X13, 0s)
            ]
            @ leakDec
            @ [
                ARM64Symbolic.Label sumStringValueDone
                ARM64Symbolic.LDR (ARM64Symbolic.X30, ARM64Symbolic.SP, 96s)
                ARM64Symbolic.LDP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 112s)
                ARM64Symbolic.Label skipLeafSumStringValueRelease
            ]
        else
            []

    [
        ARM64Symbolic.Label helperLabel
        // X0 = current tagged HAMT root, X1 = pending work stack count.
        ARM64Symbolic.MOVZ (ARM64Symbolic.X1, 0us, 0)
        ARM64Symbolic.B_label loopCheck

        ARM64Symbolic.Label loopCheck
        ARM64Symbolic.CBZ (ARM64Symbolic.X0, popOrRet)
        ARM64Symbolic.AND_imm (ARM64Symbolic.X2, ARM64Symbolic.X0, 3UL)
        ARM64Symbolic.CBZ (ARM64Symbolic.X2, popOrRet)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 3us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, popOrRet)
        // This contiguous all-ones mask is an encodable AArch64 logical immediate.
        ARM64Symbolic.AND_imm (ARM64Symbolic.X3, ARM64Symbolic.X0, 0xFFFFFFFFFFFFFFF8UL)
        ARM64Symbolic.CMP_reg (ARM64Symbolic.X3, ARM64Symbolic.X27)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, popOrRet)
        ARM64Symbolic.CMP_reg (ARM64Symbolic.X3, ARM64Symbolic.X28)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, popOrRet)

        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 1us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, internalTag)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 2us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, leafTag)
        ARM64Symbolic.B_label collisionTag

        ARM64Symbolic.Label leafTag
        ARM64Symbolic.MOVZ (ARM64Symbolic.X4, 16us, 0)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X5, 0us, 0)
        ARM64Symbolic.B_label haveOffset

        ARM64Symbolic.Label collisionTag
        ARM64Symbolic.LDR (ARM64Symbolic.X4, ARM64Symbolic.X3, 0s)
        ARM64Symbolic.LSL_imm (ARM64Symbolic.X4, ARM64Symbolic.X4, 4)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X4, ARM64Symbolic.X4, 8us)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X5, 0us, 0)
        ARM64Symbolic.B_label haveOffset

        ARM64Symbolic.Label internalTag
        ARM64Symbolic.LDR (ARM64Symbolic.X6, ARM64Symbolic.X3, 0s) // bitmap
        // D16 is reserved scratch: count the bitmap bytes, horizontally add
        // them, and zero-extend the resulting byte into the child count.
        ARM64Symbolic.FMOV_from_gp (ARM64Symbolic.D16, ARM64Symbolic.X6)
        ARM64Symbolic.CNT_8B (ARM64Symbolic.D16, ARM64Symbolic.D16)
        ARM64Symbolic.ADDV_8B (ARM64Symbolic.D16, ARM64Symbolic.D16)
        ARM64Symbolic.UMOV_byte (ARM64Symbolic.X5, ARM64Symbolic.D16)
        ARM64Symbolic.LSL_imm (ARM64Symbolic.X4, ARM64Symbolic.X5, 3)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X4, ARM64Symbolic.X4, 8us)

        ARM64Symbolic.Label haveOffset
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X6, ARM64Symbolic.X3, ARM64Symbolic.X4)
        ARM64Symbolic.LDR (ARM64Symbolic.X7, ARM64Symbolic.X6, 0s)
        ARM64Symbolic.SUB_imm (ARM64Symbolic.X7, ARM64Symbolic.X7, 1us)
        ARM64Symbolic.STR (ARM64Symbolic.X7, ARM64Symbolic.X6, 0s)
        ARM64Symbolic.CBNZ (ARM64Symbolic.X7, popOrRet)
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
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 1us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, collectInternal)
        ARM64Symbolic.B_label freeNode

        ARM64Symbolic.Label collectInternal
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X6, 0us, 0)
        ARM64Symbolic.Label collectLoop
        ARM64Symbolic.CMP_reg (ARM64Symbolic.X6, ARM64Symbolic.X5)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, freeNode)
        ARM64Symbolic.LSL_imm (ARM64Symbolic.X7, ARM64Symbolic.X6, 3)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X7, ARM64Symbolic.X7, 8us)
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X7, ARM64Symbolic.X3, ARM64Symbolic.X7)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X7, 0s)
    ]
    @ addChild "internal"
    @ [
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X6, ARM64Symbolic.X6, 1us)
        ARM64Symbolic.B_label collectLoop

        ARM64Symbolic.Label freeNode
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X4, 256us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, skipFreeList)
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X6, ARM64Symbolic.X27, ARM64Symbolic.X4)
        ARM64Symbolic.LDR (ARM64Symbolic.X7, ARM64Symbolic.X6, 0s)
        ARM64Symbolic.STR (ARM64Symbolic.X7, ARM64Symbolic.X3, 0s)
        ARM64Symbolic.STR (ARM64Symbolic.X3, ARM64Symbolic.X6, 0s)
        ARM64Symbolic.Label skipFreeList
    ]
    @ leakDec
    @ [
        ARM64Symbolic.B_label loopCheck

        ARM64Symbolic.Label popOrRet
        ARM64Symbolic.CBZ (ARM64Symbolic.X1, helperRet)
        ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.SP, 0s)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
        ARM64Symbolic.SUB_imm (ARM64Symbolic.X1, ARM64Symbolic.X1, 1us)
        ARM64Symbolic.B_label loopCheck

        ARM64Symbolic.Label helperRet
        ARM64Symbolic.RET
    ]

let internal generatePlannedDictRefCountDecHelper
    (helperLabel: string)
    (releasePlan: MemoryModel.RcReleasePlan)
    (ctx: CodeGenContext)
    : ARM64Symbolic.Instr list =
    let unsupported context release =
        Crash.crash $"ARM64 planned dict RefCountDec does not support {context} release plan {release}"

    match releasePlan with
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
            match valueRelease with
            | MemoryModel.NoReleasePlan ->
                false, false, None, false, false, None, false, false, false
            | MemoryModel.DynamicBufferRelease _ ->
                true, false, None, false, false, None, false, false, false
            | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
                false, true, None, false, false, None, false, false, false
            | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
                false, false, Some (dictDecHelperForReleasePlan valueRelease), false, false, None, false, false, false
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
            | other ->
                unsupported "value" other

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
        Crash.crash $"ARM64 planned dict RefCountDec helper requires a DictHeap release plan, got {other}"
