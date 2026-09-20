// ListReferenceCounts.fs - Generate tagged-list retain and iterative destruction helpers.

module ARM64ListReferenceCounts

open ARM64CodeGenTypes
open ARM64HeapAllocation

let internal generateListRefCountIncHelper () : ARM64Symbolic.Instr list =
    let label (name: string) : string = $"__dark_list_rc_inc_{name}"
    let size24 = label "size_24"
    let size32 = label "size_32"
    let haveSize = label "have_size"
    let helperRet = label "ret"

    [
        ARM64Symbolic.Label listRefCountIncHelperLabel
        // X0 = tagged list pointer (or 0)
        ARM64Symbolic.CBZ (ARM64Symbolic.X0, helperRet)
        ARM64Symbolic.AND_imm (ARM64Symbolic.X1, ARM64Symbolic.X0, 7UL)
        ARM64Symbolic.CBZ (ARM64Symbolic.X1, helperRet)  // Untagged pointer => not a skew-list node
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X1, 3us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, helperRet)
        // This contiguous all-ones mask is an encodable AArch64 logical immediate.
        ARM64Symbolic.AND_imm (ARM64Symbolic.X2, ARM64Symbolic.X0, 0xFFFFFFFFFFFFFFF8UL)

        ARM64Symbolic.CMP_imm (ARM64Symbolic.X1, 1us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, size32)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X1, 3us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, size24)

        // Tag 2 (LEAF).
        ARM64Symbolic.MOVZ (ARM64Symbolic.X3, 8us, 0)
        ARM64Symbolic.B_label haveSize

        ARM64Symbolic.Label size24
        ARM64Symbolic.MOVZ (ARM64Symbolic.X3, 24us, 0)
        ARM64Symbolic.B_label haveSize

        ARM64Symbolic.Label size32
        ARM64Symbolic.MOVZ (ARM64Symbolic.X3, 32us, 0)
        ARM64Symbolic.B_label haveSize

        ARM64Symbolic.Label haveSize
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X2, ARM64Symbolic.X2, ARM64Symbolic.X3)
        ARM64Symbolic.LDR (ARM64Symbolic.X4, ARM64Symbolic.X2, 0s)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X4, ARM64Symbolic.X4, 1us)
        ARM64Symbolic.STR (ARM64Symbolic.X4, ARM64Symbolic.X2, 0s)

        ARM64Symbolic.Label helperRet
        ARM64Symbolic.RET
    ]

let private generateListRefCountDecHelperWith
    (helperLabel: string)
    (ctx: CodeGenContext)
    (leafGenericPayloadSize: int option)
    (leafGenericReleasePlan: MemoryModel.RcReleasePlan option)
    (releaseLeafDynamicBufferPayload: bool)
    (releaseLeafListPayload: bool)
    (releaseLeafDictPayload: bool)
    (releaseLeafClosurePayload: bool)
    (managedLeafFieldTypes: AST.Type list)
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
            // Only traverse plausible tagged list pointers inside the managed heap range.
            ARM64Symbolic.AND_imm (ARM64Symbolic.X9, ARM64Symbolic.X8, 7UL)
            ARM64Symbolic.CBZ (ARM64Symbolic.X9, doneLabel)
            ARM64Symbolic.CMP_imm (ARM64Symbolic.X9, 3us)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, doneLabel)
            // This contiguous all-ones mask is an encodable AArch64 logical immediate.
            ARM64Symbolic.AND_imm (ARM64Symbolic.X10, ARM64Symbolic.X8, 0xFFFFFFFFFFFFFFF8UL)
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X10, ARM64Symbolic.X27)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, doneLabel)
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X10, ARM64Symbolic.X28)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, doneLabel)
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
    let size8 = label "size_8"
    let size24 = label "size_24"
    let size32 = label "size_32"
    let haveSize = label "have_size"
    let collectSingle = label "collect_single"
    let collectDeep = label "collect_deep"
    let collectNode2 = label "collect_node2"
    let collectNodeChildren = label "collect_node_children"
    let collectNode3 = label "collect_node3"
    let collectLeaf = label "collect_leaf"
    let releaseValue = label "release_value"
    let leafPayloadDone = label "leaf_payload_done"
    let afterPrefix = label "after_prefix"
    let afterSuffix = label "after_suffix"
    let freeNode = label "free_node"

    let releaseClosurePayload =
        [
            ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 0s)
            ARM64Symbolic.CBZ (ARM64Symbolic.X8, leafPayloadDone)
            ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -96s)
            ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
            ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
            ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
            ARM64Symbolic.STR (ARM64Symbolic.X30, ARM64Symbolic.SP, 80s)
            ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.X8)
            ARM64Symbolic.BL closureRefCountDecHelperLabel
            ARM64Symbolic.LDR (ARM64Symbolic.X30, ARM64Symbolic.SP, 80s)
            ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
            ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
            ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
            ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 96s)
            ARM64Symbolic.Label leafPayloadDone
        ]

    let releaseDynamicBufferPayload =
        let refcountUpdate =
            if List.isEmpty leakDec then
                [
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.X14, ARM64Symbolic.X14, 1us)
                    ARM64Symbolic.STR (ARM64Symbolic.X14, ARM64Symbolic.X12, 0s)
                ]
            else
                [
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.X14, ARM64Symbolic.X14, 1us)
                    ARM64Symbolic.STR (ARM64Symbolic.X14, ARM64Symbolic.X12, 0s)
                    ARM64Symbolic.CBNZ (ARM64Symbolic.X14, leafPayloadDone)
                ] @ leakDec
        let taggedGuard =
            if helperLabel = listRefCountDecBlobHelperLabel then
                [ ARM64Symbolic.AND_imm (ARM64Symbolic.X13, ARM64Symbolic.X12, 1UL)
                  ARM64Symbolic.CBNZ (ARM64Symbolic.X13, leafPayloadDone) ]
            else
                []
        [
            ARM64Symbolic.LDR (ARM64Symbolic.X12, ARM64Symbolic.X3, 0s)
            ARM64Symbolic.CBZ (ARM64Symbolic.X12, leafPayloadDone)
        ]
        @ taggedGuard
        @ [
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X12, ARM64Symbolic.X27)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, leafPayloadDone)
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X12, ARM64Symbolic.X28)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, leafPayloadDone)
            ARM64Symbolic.LDR (ARM64Symbolic.X14, ARM64Symbolic.X12, 0s)
            ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 0xFFFFus, 0)
            ARM64Symbolic.MOVK (ARM64Symbolic.X15, 0xFFFFus, 16)
            ARM64Symbolic.MOVK (ARM64Symbolic.X15, 0xFFFFus, 32)
            ARM64Symbolic.MOVK (ARM64Symbolic.X15, 0x7FFFus, 48)
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X14, ARM64Symbolic.X15)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, leafPayloadDone)
        ]
        @ refcountUpdate
        @ [ARM64Symbolic.Label leafPayloadDone]

    let releaseRecursivePayload (sourceType: AST.Type) =
        [
            ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 0s)
            ARM64Symbolic.CBZ (ARM64Symbolic.X8, leafPayloadDone)
            ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -96s)
            ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
            ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
            ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
            ARM64Symbolic.STR (ARM64Symbolic.X30, ARM64Symbolic.SP, 80s)
            ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.X8)
            ARM64Symbolic.BL (recursiveNominalRefCountDecHelperLabel sourceType)
            ARM64Symbolic.LDR (ARM64Symbolic.X30, ARM64Symbolic.SP, 80s)
            ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
            ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
            ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
            ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 96s)
            ARM64Symbolic.Label leafPayloadDone
        ]

    let leafListHelperLabelForReleasePlan (releasePlan: MemoryModel.RcReleasePlan) : string =
        match releasePlan with
        | MemoryModel.RootRelease (_, MemoryModel.TaggedList, MemoryModel.TaggedListPayloadRelease elementRelease) ->
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
            | MemoryModel.RecursiveRelease sourceType ->
                plannedListDecHelperLabelForReleasePlan (MemoryModel.RecursiveRelease sourceType)
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
                plannedListDecHelperLabelForReleasePlan elementRelease
            | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
                listRefCountDecDictHelperLabel
            | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
                listRefCountDecClosureHelperLabel
            | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) ->
                plannedListDecHelperLabelForReleasePlan elementRelease
            | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, _) ->
                plannedListDecHelperLabelForReleasePlan elementRelease
        | _ ->
            listRefCountDecHelperLabel

    let leafDictHelperLabelForReleasePlan (releasePlan: MemoryModel.RcReleasePlan) : string =
        match releasePlan with
        | (MemoryModel.RootRelease (
              _,
              MemoryModel.DictHeap,
              MemoryModel.DictPayloadRelease (MemoryModel.DynamicBufferRelease _, _)) as dictReleasePlan)
        | (MemoryModel.RootRelease (
              _,
              MemoryModel.DictHeap,
              MemoryModel.DictPayloadRelease (_, MemoryModel.DynamicBufferRelease _)) as dictReleasePlan) ->
            plannedDictDecHelperLabelForReleasePlan dictReleasePlan
        | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, MemoryModel.RootRelease (_, MemoryModel.TaggedList, _))) ->
            dictRefCountDecListValueHelperLabel
        | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, MemoryModel.RootRelease (_, MemoryModel.DictHeap, _))) ->
            dictRefCountDecDictValueHelperLabel
        | _ ->
            dictRefCountDecHelperLabel

    let releaseLeafDictPayloadWithHelper (dictHelperLabel: string) =
        [
            ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 0s)
            ARM64Symbolic.CBZ (ARM64Symbolic.X8, leafPayloadDone)
            ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -96s)
            ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
            ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
            ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
            ARM64Symbolic.STR (ARM64Symbolic.X30, ARM64Symbolic.SP, 80s)
            ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.X8)
            ARM64Symbolic.BL dictHelperLabel
            ARM64Symbolic.LDR (ARM64Symbolic.X30, ARM64Symbolic.SP, 80s)
            ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
            ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
            ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
            ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 96s)
            ARM64Symbolic.Label leafPayloadDone
        ]

    let releasePlanDynamicBufferFieldFrom
        (baseReg: ARM64Symbolic.Reg)
        (fieldOffset: int)
        (path: string)
        (operation: MemoryModel.RcOperation)
        : ARM64Symbolic.Instr list =
        let fieldDone = label $"leaf_plan_dynamic_{path}_{fieldOffset}_done"
        let refcountUpdate =
            if List.isEmpty leakDec then
                [
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.X14, ARM64Symbolic.X14, 1us)
                    ARM64Symbolic.STR (ARM64Symbolic.X14, ARM64Symbolic.X12, 0s)
                ]
            else
                [
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.X14, ARM64Symbolic.X14, 1us)
                    ARM64Symbolic.STR (ARM64Symbolic.X14, ARM64Symbolic.X12, 0s)
                    ARM64Symbolic.CBNZ (ARM64Symbolic.X14, fieldDone)
                ] @ leakDec
        let taggedGuard =
            match operation with
            | MemoryModel.DynamicIntBuffer ->
                [ ARM64Symbolic.AND_imm (ARM64Symbolic.X13, ARM64Symbolic.X12, 1UL)
                  ARM64Symbolic.CBNZ (ARM64Symbolic.X13, fieldDone) ]
            | _ -> []
        [
            ARM64Symbolic.LDR (ARM64Symbolic.X12, baseReg, int16 fieldOffset)
            ARM64Symbolic.CBZ (ARM64Symbolic.X12, fieldDone)
        ]
        @ taggedGuard
        @ [
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X12, ARM64Symbolic.X27)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, fieldDone)
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X12, ARM64Symbolic.X28)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, fieldDone)
            ARM64Symbolic.LDR (ARM64Symbolic.X14, ARM64Symbolic.X12, 0s)
            ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 0xFFFFus, 0)
            ARM64Symbolic.MOVK (ARM64Symbolic.X15, 0xFFFFus, 16)
            ARM64Symbolic.MOVK (ARM64Symbolic.X15, 0xFFFFus, 32)
            ARM64Symbolic.MOVK (ARM64Symbolic.X15, 0x7FFFus, 48)
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X14, ARM64Symbolic.X15)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, fieldDone)
        ]
        @ refcountUpdate
        @ [ARM64Symbolic.Label fieldDone]

    let releasePlanManagedRootFieldFrom
        (baseReg: ARM64Symbolic.Reg)
        (fieldOffset: int)
        (path: string)
        (helperLabel: string)
        : ARM64Symbolic.Instr list =
        let fieldDone = label $"leaf_plan_root_{path}_{fieldOffset}_done"
        [
            ARM64Symbolic.LDR (ARM64Symbolic.X12, baseReg, int16 fieldOffset)
            ARM64Symbolic.CBZ (ARM64Symbolic.X12, fieldDone)
            ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -112s)
            ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
            ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
            ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
            ARM64Symbolic.STP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
            ARM64Symbolic.STR (ARM64Symbolic.X30, ARM64Symbolic.SP, 96s)
            ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.X12)
            ARM64Symbolic.BL helperLabel
            ARM64Symbolic.LDR (ARM64Symbolic.X30, ARM64Symbolic.SP, 96s)
            ARM64Symbolic.LDP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
            ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
            ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
            ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
            ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 112s)
            ARM64Symbolic.Label fieldDone
        ]

    let rec releasePlanFieldFrom
        (baseReg: ARM64Symbolic.Reg)
        (fieldOffset: int)
        (path: string)
        (fieldReleasePlan: MemoryModel.RcReleasePlan)
        : ARM64Symbolic.Instr list =
        match fieldReleasePlan with
        | MemoryModel.DynamicBufferRelease operation ->
            releasePlanDynamicBufferFieldFrom baseReg fieldOffset path operation
        | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
            releasePlanManagedRootFieldFrom
                baseReg
                fieldOffset
                path
                (leafListHelperLabelForReleasePlan fieldReleasePlan)
        | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
            releasePlanManagedRootFieldFrom
                baseReg
                fieldOffset
                path
                (leafDictHelperLabelForReleasePlan fieldReleasePlan)
        | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
            releasePlanManagedRootFieldFrom baseReg fieldOffset path closureRefCountDecHelperLabel
        | MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease _)
        | MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease _) ->
            releasePlanGenericFieldFrom baseReg fieldOffset payloadSize path fieldReleasePlan
        | _ ->
            []

    and releasePlanBoxedSumVariantFieldsFrom
        (baseReg: ARM64Symbolic.Reg)
        (path: string)
        (variants: MemoryModel.RcBoxedSumVariantRelease list)
        : ARM64Symbolic.Instr list =
        let releaseVariant (variant: MemoryModel.RcBoxedSumVariantRelease) : (int * ARM64Symbolic.Instr list) option =
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
            let sumDone = label $"leaf_plan_sum_{path}_done"
            [
                ARM64Symbolic.LDR (ARM64Symbolic.X10, baseReg, 0s)
            ]
            @
            (cases
             |> List.mapi (fun index (tag, releaseInstrs) ->
                let nextCase = label $"leaf_plan_sum_{path}_variant_{index}_next"
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

    and releasePlanGenericFieldFrom
        (baseReg: ARM64Symbolic.Reg)
        (fieldOffset: int)
        (payloadSize: int)
        (path: string)
        (fieldReleasePlan: MemoryModel.RcReleasePlan)
        : ARM64Symbolic.Instr list =
        let fieldDone = label $"leaf_plan_generic_{path}_{fieldOffset}_done"
        let childFieldReleases =
            match fieldReleasePlan with
            | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases)) ->
                fieldReleases
                |> List.collect (fun (MemoryModel.FieldRelease (childOffset, childReleasePlan)) ->
                    releasePlanFieldFrom ARM64Symbolic.X11 childOffset $"{path}_{fieldOffset}" childReleasePlan)
            | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease (_, _, variants)) ->
                releasePlanBoxedSumVariantFieldsFrom ARM64Symbolic.X11 $"{path}_{fieldOffset}" variants
            | _ ->
                []
        [
            ARM64Symbolic.LDR (ARM64Symbolic.X12, baseReg, int16 fieldOffset)
            ARM64Symbolic.CBZ (ARM64Symbolic.X12, fieldDone)
            ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X12, int16 payloadSize)
            ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
            ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X12, int16 payloadSize)
            ARM64Symbolic.CBNZ (ARM64Symbolic.X15, fieldDone)
            ARM64Symbolic.MOV_reg (ARM64Symbolic.X11, ARM64Symbolic.X12)
            ARM64Symbolic.STP_pre (ARM64Symbolic.X12, ARM64Symbolic.X30, ARM64Symbolic.SP, -16s)
        ]
        @ childFieldReleases
        @ [
            ARM64Symbolic.LDR (ARM64Symbolic.X12, ARM64Symbolic.SP, 0s)
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
            ARM64Symbolic.LDP_post (ARM64Symbolic.X12, ARM64Symbolic.X30, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.Label fieldDone
        ]

    let releaseManagedLeafFieldsFromPlan (releasePlan: MemoryModel.RcReleasePlan) : ARM64Symbolic.Instr list =
        match releasePlan with
        | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases)) ->
            fieldReleases
            |> List.collect (fun (MemoryModel.FieldRelease (fieldOffset, fieldReleasePlan)) ->
                releasePlanFieldFrom ARM64Symbolic.X8 fieldOffset "root" fieldReleasePlan)
        | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease (_, _, variants)) ->
            releasePlanBoxedSumVariantFieldsFrom ARM64Symbolic.X8 "root" variants
        | _ ->
            []

    let managedLeafFieldReleasePlan (fieldType: AST.Type) : MemoryModel.RcReleasePlan option =
        match fieldType with
        | AST.TRecord (name, _) when not (Map.containsKey name ctx.RecordRegistry) ->
            None
        | _ ->
            Some (MemoryPlanning.rcReleasePlanOfTypeWithSums ctx.RecordRegistry ctx.SumShapeRegistry fieldType)

    let releaseManagedLeafFields =
        managedLeafFieldTypes
        |> List.mapi (fun index fieldType ->
            let fieldOffset = index * 8
            match managedLeafFieldReleasePlan fieldType with
            | Some fieldReleasePlan ->
                releasePlanFieldFrom
                    ARM64Symbolic.X8
                    fieldOffset
                    $"legacy_{index}"
                    fieldReleasePlan
            | None ->
                [])
        |> List.concat

    let releaseLeafFieldPayloads =
        match leafGenericReleasePlan with
        | Some releasePlan ->
            releaseManagedLeafFieldsFromPlan releasePlan
        | None ->
            releaseManagedLeafFields

    let releaseLeafPayload =
        match leafGenericPayloadSize, releaseLeafDynamicBufferPayload, releaseLeafListPayload, releaseLeafDictPayload, releaseLeafClosurePayload with
        | None, false, false, false, false ->
            match leafGenericReleasePlan with
            | Some (MemoryModel.RecursiveRelease sourceType) -> releaseRecursivePayload sourceType
            | Some (MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) as dictReleasePlan) ->
                releaseLeafDictPayloadWithHelper
                    (leafDictHelperLabelForReleasePlan dictReleasePlan)
            | _ -> []
        | None, true, _, _, _ ->
            releaseDynamicBufferPayload
        | None, false, true, _, _ ->
            [
                ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 0s)
            ]
            @ addChild "leaf_payload_list"
        | None, false, false, true, _ ->
            let helperLabel =
                if helperLabel = listRefCountDecDictListHelperLabel then
                    dictRefCountDecListValueHelperLabel
                else
                    dictRefCountDecHelperLabel
            releaseLeafDictPayloadWithHelper helperLabel
        | None, false, false, false, true ->
            releaseClosurePayload
        | Some payloadSize, _, _, _, _ ->
            [
                ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 0s)
                ARM64Symbolic.CBZ (ARM64Symbolic.X8, leafPayloadDone)
                // Specialized fixed-block helpers can encounter primitive leaves from nested lists.
                // Only aligned managed-heap payload pointers have trailing refcounts to decrement.
                ARM64Symbolic.AND_imm (ARM64Symbolic.X9, ARM64Symbolic.X8, 7UL)
                ARM64Symbolic.CBNZ (ARM64Symbolic.X9, leafPayloadDone)
                ARM64Symbolic.CMP_reg (ARM64Symbolic.X8, ARM64Symbolic.X27)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, leafPayloadDone)
                ARM64Symbolic.CMP_reg (ARM64Symbolic.X8, ARM64Symbolic.X28)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, leafPayloadDone)
                ARM64Symbolic.LDR (ARM64Symbolic.X9, ARM64Symbolic.X8, int16 payloadSize)
                ARM64Symbolic.SUB_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 1us)
                ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.X8, int16 payloadSize)
                ARM64Symbolic.CBNZ (ARM64Symbolic.X9, leafPayloadDone)
            ]
            @ releaseLeafFieldPayloads
            @ (if List.isEmpty releaseLeafFieldPayloads then
                []
               else
                // Field release may use X8 for nested payloads; reload the leaf payload before freeing it.
                [ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 0s)])
            @ (if payloadSize >= 0 && payloadSize < 256 then
                [
                    ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X27, int16 payloadSize)
                    ARM64Symbolic.STR (ARM64Symbolic.X10, ARM64Symbolic.X8, 0s)
                    ARM64Symbolic.STR (ARM64Symbolic.X8, ARM64Symbolic.X27, int16 payloadSize)
                ]
               else
                [])
            @ leakDec
            @ [ARM64Symbolic.Label leafPayloadDone]

    // Primitive elements have no payload-release code and cannot clobber the
    // current node. Only helpers with actual payload work need X19-X21 or a
    // callee-save frame; the separate DFS work stack is unchanged.
    let preserveForPayloadRelease instructions =
        if List.isEmpty releaseLeafPayload then [] else instructions

    [ARM64Symbolic.Label helperLabel]
    @ preserveForPayloadRelease [
        // Preserve callee-saved registers used to keep the current node stable
        // across payload-release helpers. Pending DFS entries are pushed above.
        ARM64Symbolic.STP_pre (ARM64Symbolic.X19, ARM64Symbolic.X20, ARM64Symbolic.SP, -32s)
        ARM64Symbolic.STR (ARM64Symbolic.X21, ARM64Symbolic.SP, 16s)
    ]
    @ [
        // X0 = current tagged list pointer to process, X1 = number of pending stack entries.
        ARM64Symbolic.MOVZ (ARM64Symbolic.X1, 0us, 0)
        ARM64Symbolic.B_label loopCheck

        ARM64Symbolic.Label loopCheck
        ARM64Symbolic.CBZ (ARM64Symbolic.X0, popOrRet)
        ARM64Symbolic.AND_imm (ARM64Symbolic.X2, ARM64Symbolic.X0, 7UL)
        ARM64Symbolic.CBZ (ARM64Symbolic.X2, popOrRet)
        // This contiguous all-ones mask is an encodable AArch64 logical immediate.
        ARM64Symbolic.AND_imm (ARM64Symbolic.X3, ARM64Symbolic.X0, 0xFFFFFFFFFFFFFFF8UL)
        ARM64Symbolic.CMP_reg (ARM64Symbolic.X3, ARM64Symbolic.X27)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, popOrRet)
        ARM64Symbolic.CMP_reg (ARM64Symbolic.X3, ARM64Symbolic.X28)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, popOrRet)

        // Resolve payload size from skew-list node tag.
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 1us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, size32)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 2us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, size8)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 3us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, size24)
        ARM64Symbolic.B_label popOrRet

        ARM64Symbolic.Label size8
        ARM64Symbolic.MOVZ (ARM64Symbolic.X4, 8us, 0)
        ARM64Symbolic.B_label haveSize
        ARM64Symbolic.Label size24
        ARM64Symbolic.MOVZ (ARM64Symbolic.X4, 24us, 0)
        ARM64Symbolic.B_label haveSize
        ARM64Symbolic.Label size32
        ARM64Symbolic.MOVZ (ARM64Symbolic.X4, 32us, 0)
        ARM64Symbolic.B_label haveSize
        ARM64Symbolic.Label haveSize
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X5, ARM64Symbolic.X3, ARM64Symbolic.X4)
        ARM64Symbolic.LDR (ARM64Symbolic.X6, ARM64Symbolic.X5, 0s)
        ARM64Symbolic.SUB_imm (ARM64Symbolic.X6, ARM64Symbolic.X6, 1us)
        ARM64Symbolic.STR (ARM64Symbolic.X6, ARM64Symbolic.X5, 0s)
        ARM64Symbolic.CBNZ (ARM64Symbolic.X6, popOrRet)

        // Refcount reached zero: collect child pointers for further decref work.
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 1us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, collectSingle)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 2us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, collectLeaf)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 3us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, collectNode2)
        ARM64Symbolic.B_label freeNode

        // DIGIT: tree and remaining-spine children at 16 and 24.
        ARM64Symbolic.Label collectSingle
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 16s)
    ]
    @ addChild "digit_tree"
    @ [
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 24s)
    ]
    @ addChild "digit_rest"
    @ [
        ARM64Symbolic.B_label freeNode

        // NODE: release the value before collecting structural children because
        // payload helpers may use X0 as scratch/work state.
        ARM64Symbolic.Label collectNode2
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
        ARM64Symbolic.B_label releaseValue

        ARM64Symbolic.Label collectNode3
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 0s)
    ]
    @ addChild "node3_0"
    @ [
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 8s)
    ]
    @ addChild "node3_1"
    @ [
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 16s)
    ]
    @ addChild "node3_2"
    @ [
        ARM64Symbolic.B_label freeNode

        ARM64Symbolic.Label collectDeep
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
        ARM64Symbolic.LDR (ARM64Symbolic.X7, ARM64Symbolic.X3, 8s)  // prefix_count
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X7, 0us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, afterPrefix)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 16s)
    ]
    @ addChild "deep_p0"
    @ [
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X7, 1us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, afterPrefix)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 24s)
    ]
    @ addChild "deep_p1"
    @ [
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X7, 2us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, afterPrefix)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 32s)
    ]
    @ addChild "deep_p2"
    @ [
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X7, 3us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, afterPrefix)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 40s)
    ]
    @ addChild "deep_p3"
    @ [
        ARM64Symbolic.Label afterPrefix
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 48s)  // middle tree
    ]
    @ addChild "deep_middle"
    @ [
        ARM64Symbolic.LDR (ARM64Symbolic.X7, ARM64Symbolic.X3, 56s)  // suffix_count
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X7, 0us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, afterSuffix)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 64s)
    ]
    @ addChild "deep_s0"
    @ [
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X7, 1us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, afterSuffix)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 72s)
    ]
    @ addChild "deep_s1"
    @ [
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X7, 2us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, afterSuffix)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 80s)
    ]
    @ addChild "deep_s2"
    @ [
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X7, 3us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, afterSuffix)
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 88s)
    ]
    @ addChild "deep_s3"
    @ [
        ARM64Symbolic.Label afterSuffix
        ARM64Symbolic.B_label freeNode

        ARM64Symbolic.Label collectLeaf
        ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
        ARM64Symbolic.Label releaseValue
    ]
    @ preserveForPayloadRelease [
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X19, ARM64Symbolic.X2)
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X20, ARM64Symbolic.X3)
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X21, ARM64Symbolic.X4)
    ]
    @ releaseLeafPayload
    @ preserveForPayloadRelease [
        // Leaves are done after payload release. Internal nodes still own two
        // complete-tree edges.
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X2, ARM64Symbolic.X19)
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X3, ARM64Symbolic.X20)
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X4, ARM64Symbolic.X21)
    ]
    @ [
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 3us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, freeNode)
        ARM64Symbolic.Label collectNodeChildren
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 8s)
    ]
    @ addChild "node_left"
    @ [
        ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X3, 16s)
    ]
    @ addChild "node_right"
    @ [
        ARM64Symbolic.Label freeNode
        // Recycle node memory by payload size class.
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X5, ARM64Symbolic.X27, ARM64Symbolic.X4)
        ARM64Symbolic.LDR (ARM64Symbolic.X6, ARM64Symbolic.X5, 0s)
        ARM64Symbolic.STR (ARM64Symbolic.X6, ARM64Symbolic.X3, 0s)
        ARM64Symbolic.STR (ARM64Symbolic.X3, ARM64Symbolic.X5, 0s)
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
    ]
    @ preserveForPayloadRelease [
        ARM64Symbolic.LDR (ARM64Symbolic.X21, ARM64Symbolic.SP, 16s)
        ARM64Symbolic.LDP_post (ARM64Symbolic.X19, ARM64Symbolic.X20, ARM64Symbolic.SP, 32s)
    ]
    @ [ARM64Symbolic.RET]

type internal ListRefCountDecHelperSpec = {
    Label: string
    ReleaseLeafListPayload: bool
    ReleaseLeafDictPayload: bool
    ReleaseLeafClosurePayload: bool
}

let internal listRefCountDecHelperSpecs : ListRefCountDecHelperSpec list =
    [
    { Label = listRefCountDecHelperLabel
      ReleaseLeafListPayload = false
      ReleaseLeafDictPayload = false
      ReleaseLeafClosurePayload = false }
    { Label = listRefCountDecListHelperLabel
      ReleaseLeafListPayload = true
      ReleaseLeafDictPayload = false
      ReleaseLeafClosurePayload = false }
    { Label = listRefCountDecDictHelperLabel
      ReleaseLeafListPayload = false
      ReleaseLeafDictPayload = true
      ReleaseLeafClosurePayload = false }
    { Label = listRefCountDecDictListHelperLabel
      ReleaseLeafListPayload = false
      ReleaseLeafDictPayload = true
      ReleaseLeafClosurePayload = false }
    { Label = listRefCountDecClosureHelperLabel
      ReleaseLeafListPayload = false
      ReleaseLeafDictPayload = false
      ReleaseLeafClosurePayload = true }
    ]

let internal generateNeededListRefCountDecHelpers
    (ctx: CodeGenContext)
    (neededHelperLabels: Set<string>)
    (plannedListDecHelpers: Map<string, int * MemoryModel.RcReleasePlan>)
    : ARM64Symbolic.Instr list =
    let staticHelpers =
        (listRefCountDecHelperSpecs
         |> List.collect (fun spec ->
             if Set.contains spec.Label neededHelperLabels then
                 generateListRefCountDecHelperWith
                     spec.Label
                     ctx
                     None
                     None
                     false
                     spec.ReleaseLeafListPayload
                     spec.ReleaseLeafDictPayload
                     spec.ReleaseLeafClosurePayload
                     []
             else
                 []))
        @ (if Set.contains listRefCountDecStringHelperLabel neededHelperLabels then
               generateListRefCountDecHelperWith
                   listRefCountDecStringHelperLabel
                   ctx
                   None
                   None
                   true
                   false
                   false
                   false
                   []
           else
               [])
        @ (if Set.contains listRefCountDecBlobHelperLabel neededHelperLabels then
               generateListRefCountDecHelperWith
                   listRefCountDecBlobHelperLabel
                   ctx
                   None
                   None
                   true
                   false
                   false
                   false
                   []
           else
               [])

    let plannedHelpers =
        plannedListDecHelpers
        |> Map.toList
        |> List.collect (fun (helperLabel, (payloadSize, releasePlan)) ->
            if Set.contains helperLabel neededHelperLabels then
                let plannedPayloadSize =
                    match releasePlan with
                    | MemoryModel.RecursiveRelease _ -> None
                    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) -> None
                    | _ -> Some payloadSize
                generateListRefCountDecHelperWith
                    helperLabel
                    ctx
                    plannedPayloadSize
                    (Some releasePlan)
                    false
                    false
                    false
                    false
                    []
            else
                [])

    staticHelpers @ plannedHelpers
