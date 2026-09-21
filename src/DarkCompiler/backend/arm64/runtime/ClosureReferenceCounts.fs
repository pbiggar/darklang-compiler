// ClosureReferenceCounts.fs - Generate closure and recursive-payload lifetime helpers.

module ARM64ClosureReferenceCounts

open ARM64CodeGenTypes
open ARM64HeapAllocation

let private generateClosurePayloadSizeResolver
    (ctx: CodeGenContext)
    (label: string -> string)
    (readyLabel: string)
    : ARM64Symbolic.Instr list =
    let cases =
        ctx.ClosurePayloadSizes
        |> Map.toList
        |> List.filter (fun (_, payloadSize) -> payloadSize <> 8)
        |> List.mapi (fun index (funcName, payloadSize) ->
            let nextLabel = label $"payload_next_{index}"
            [
                ARM64Symbolic.ADR (ARM64Symbolic.X11, codeLabel funcName)
                ARM64Symbolic.CMP_reg (ARM64Symbolic.X9, ARM64Symbolic.X11)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, nextLabel)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X10, uint16 payloadSize, 0)
                ARM64Symbolic.B_label readyLabel
                ARM64Symbolic.Label nextLabel
            ])
        |> List.concat

    [
        ARM64Symbolic.LDR (ARM64Symbolic.X9, ARM64Symbolic.X0, 0s)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X10, 8us, 0)
    ]
    @ cases

let internal generateClosureRefCountIncHelper (ctx: CodeGenContext) : ARM64Symbolic.Instr list =
    let label (name: string) : string = $"__dark_closure_rc_inc_{name}"
    let ready = label "payload_ready"
    let helperRet = label "ret"
    [
        ARM64Symbolic.Label closureRefCountIncHelperLabel
        ARM64Symbolic.CBZ (ARM64Symbolic.X0, helperRet)
    ]
    @ generateClosurePayloadSizeResolver ctx label ready
    @ [
        ARM64Symbolic.Label ready
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X12, ARM64Symbolic.X0, ARM64Symbolic.X10)
        ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
        ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)
        ARM64Symbolic.Label helperRet
        ARM64Symbolic.RET
    ]

let internal tryRcReleasePlanOfType
    (recordRegistry: LIR.RecordRegistry)
    (sumShapeRegistry: MemoryModel.RcSumShapeRegistry)
    (typ: AST.Type)
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

let internal generateRecursiveNominalRefCountDecHelper
    (ctx: CodeGenContext)
    (sourceType: AST.Type)
    : ARM64Symbolic.Instr list =
    let helperLabel = recursiveNominalRefCountDecHelperLabel sourceType
    let label suffix = $"{helperLabel}_{suffix}"
    let releasePlan =
        MemoryPlanning.rcReleasePlanOfTypeWithSums ctx.RecordRegistry ctx.SumShapeRegistry sourceType

    let rec releaseFromX0 (path: string) (plan: MemoryModel.RcReleasePlan) : ARM64Symbolic.Instr list =
        match plan with
        | MemoryModel.NoReleasePlan ->
            []
        | MemoryModel.DynamicBufferRelease operation ->
            let doneLabel = label $"{path}_dynamic_done"
            let leakRelease =
                if ctx.Options.EnableLeakCheck then
                    let labelRef = dataLabel leakCounterLabel
                    [
                        ARM64Symbolic.CBNZ (ARM64Symbolic.X1, doneLabel)
                        ARM64Symbolic.ADRP (ARM64Symbolic.X17, labelRef)
                        ARM64Symbolic.ADD_label (ARM64Symbolic.X17, ARM64Symbolic.X17, labelRef)
                        ARM64Symbolic.LDR (ARM64Symbolic.X16, ARM64Symbolic.X17, 0s)
                        ARM64Symbolic.SUB_imm (ARM64Symbolic.X16, ARM64Symbolic.X16, 1us)
                        ARM64Symbolic.STR (ARM64Symbolic.X16, ARM64Symbolic.X17, 0s)
                    ]
                else
                    []
            let taggedGuard =
                match operation with
                | MemoryModel.DynamicIntBuffer ->
                    [ ARM64Symbolic.AND_imm (ARM64Symbolic.X3, ARM64Symbolic.X0, 1UL)
                      ARM64Symbolic.CBNZ (ARM64Symbolic.X3, doneLabel) ]
                | _ -> []
            [
                ARM64Symbolic.CBZ (ARM64Symbolic.X0, doneLabel)
            ]
            @ taggedGuard
            @ [
                ARM64Symbolic.CMP_reg (ARM64Symbolic.X0, ARM64Symbolic.X27)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, doneLabel)
                ARM64Symbolic.CMP_reg (ARM64Symbolic.X0, ARM64Symbolic.X28)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.GT, doneLabel)
                ARM64Symbolic.LDR (ARM64Symbolic.X1, ARM64Symbolic.X0, 0s)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X3, 0xFFFFus, 0)
                ARM64Symbolic.MOVK (ARM64Symbolic.X3, 0xFFFFus, 16)
                ARM64Symbolic.MOVK (ARM64Symbolic.X3, 0xFFFFus, 32)
                ARM64Symbolic.MOVK (ARM64Symbolic.X3, 0x7FFFus, 48)
                ARM64Symbolic.CMP_reg (ARM64Symbolic.X1, ARM64Symbolic.X3)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, doneLabel)
                ARM64Symbolic.SUB_imm (ARM64Symbolic.X1, ARM64Symbolic.X1, 1us)
                ARM64Symbolic.STR (ARM64Symbolic.X1, ARM64Symbolic.X0, 0s)
            ]
            @ leakRelease
            @ [ARM64Symbolic.Label doneLabel]
        | MemoryModel.RecursiveRelease recursiveType ->
            [ARM64Symbolic.BL (recursiveNominalRefCountDecHelperLabel recursiveType)]
        | MemoryModel.RootRelease (_, MemoryModel.TaggedList, MemoryModel.TaggedListPayloadRelease elementRelease) ->
            let helper =
                match elementRelease with
                | MemoryModel.NoReleasePlan -> listRefCountDecHelperLabel
                | MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer -> listRefCountDecStringHelperLabel
                | MemoryModel.DynamicBufferRelease MemoryModel.DynamicBlobBuffer -> listRefCountDecBlobHelperLabel
                | MemoryModel.DynamicBufferRelease MemoryModel.DynamicIntBuffer -> listRefCountDecBlobHelperLabel
                | MemoryModel.DynamicBufferRelease _ -> Crash.crash "closure list release used a fixed-size dynamic-buffer operation"
                | MemoryModel.RecursiveRelease recursiveType ->
                    plannedListDecHelperLabelForReleasePlan (MemoryModel.RecursiveRelease recursiveType)
                | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) -> listRefCountDecListHelperLabel
                | MemoryModel.RootRelease (
                      _,
                      MemoryModel.DictHeap,
                      MemoryModel.DictPayloadRelease (MemoryModel.DynamicBufferRelease _, _))
                | MemoryModel.RootRelease (
                      _,
                      MemoryModel.DictHeap,
                      MemoryModel.DictPayloadRelease (_, MemoryModel.DynamicBufferRelease _)) ->
                    plannedListDecHelperLabelForReleasePlan elementRelease
                | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) -> listRefCountDecDictHelperLabel
                | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) -> listRefCountDecClosureHelperLabel
                | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) -> plannedListDecHelperLabelForReleasePlan elementRelease
                | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, _) -> plannedListDecHelperLabelForReleasePlan elementRelease
            [ARM64Symbolic.BL helper]
        | MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, payloadPlan) ->
            let doneLabel = label $"{path}_done"
            let payloadReleases = releasePayload path payloadPlan
            let freeRoot =
                if payloadSize >= 0 && payloadSize < 256 then
                    [
                        ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.SP, 0s)
                        ARM64Symbolic.LDR (ARM64Symbolic.X2, ARM64Symbolic.X27, int16 payloadSize)
                        ARM64Symbolic.STR (ARM64Symbolic.X2, ARM64Symbolic.X0, 0s)
                        ARM64Symbolic.STR (ARM64Symbolic.X0, ARM64Symbolic.X27, int16 payloadSize)
                    ]
                else
                    []
            [
                ARM64Symbolic.CBZ (ARM64Symbolic.X0, doneLabel)
                ARM64Symbolic.LDR (ARM64Symbolic.X1, ARM64Symbolic.X0, int16 payloadSize)
                ARM64Symbolic.SUB_imm (ARM64Symbolic.X1, ARM64Symbolic.X1, 1us)
                ARM64Symbolic.STR (ARM64Symbolic.X1, ARM64Symbolic.X0, int16 payloadSize)
                ARM64Symbolic.CBNZ (ARM64Symbolic.X1, doneLabel)
                ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X30, ARM64Symbolic.SP, -16s)
            ]
            @ payloadReleases
            @ freeRoot
            @ (if ctx.Options.EnableLeakCheck then
                let labelRef = dataLabel leakCounterLabel
                [
                    ARM64Symbolic.ADRP (ARM64Symbolic.X17, labelRef)
                    ARM64Symbolic.ADD_label (ARM64Symbolic.X17, ARM64Symbolic.X17, labelRef)
                    ARM64Symbolic.LDR (ARM64Symbolic.X16, ARM64Symbolic.X17, 0s)
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.X16, ARM64Symbolic.X16, 1us)
                    ARM64Symbolic.STR (ARM64Symbolic.X16, ARM64Symbolic.X17, 0s)
                ]
               else
                [])
            @ [
                ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X30, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.Label doneLabel
            ]
        | unsupported ->
            Crash.crash $"ARM64 recursive nominal RC helper does not support nested release plan {unsupported}"

    and releaseField (path: string) (index: int) (MemoryModel.FieldRelease (offset, plan)) : ARM64Symbolic.Instr list =
        [
            ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.SP, 0s)
            ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X0, int16 offset)
        ]
        @ releaseFromX0 $"{path}_field_{index}" plan

    and releaseFields (path: string) (fields: MemoryModel.RcFieldRelease list) : ARM64Symbolic.Instr list =
        fields
        |> List.mapi (releaseField path)
        |> List.concat

    and releasePayload (path: string) (payload: MemoryModel.RcPayloadReleasePlan) : ARM64Symbolic.Instr list =
        match payload with
        | MemoryModel.NoPayloadRelease ->
            []
        | MemoryModel.FixedBlockPayloadRelease (_, fields)
        | MemoryModel.ClosurePayloadRelease fields ->
            releaseFields path fields
        | MemoryModel.BoxedSumPayloadRelease (_, _, variants) ->
            let doneLabel = label $"{path}_variant_done"
            let cases =
                variants
                |> List.filter (fun variant -> not (List.isEmpty variant.FieldReleases))
                |> List.mapi (fun index variant ->
                    let nextLabel = label $"{path}_variant_{index}_next"
                    [
                        ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.SP, 0s)
                        ARM64Symbolic.LDR (ARM64Symbolic.X1, ARM64Symbolic.X0, 0s)
                        ARM64Symbolic.CMP_imm (ARM64Symbolic.X1, uint16 variant.Tag)
                        ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, nextLabel)
                    ]
                    @ releaseFields $"{path}_variant_{index}" variant.FieldReleases
                    @ [
                        ARM64Symbolic.B_label doneLabel
                        ARM64Symbolic.Label nextLabel
                    ])
                |> List.concat
            cases @ [ARM64Symbolic.Label doneLabel]
        | unsupported ->
            Crash.crash $"ARM64 recursive nominal RC helper does not support payload release plan {unsupported}"

    match releasePlan with
    | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, _) ->
        [ARM64Symbolic.Label helperLabel]
        @ releaseFromX0 "root" releasePlan
        @ [ARM64Symbolic.RET]
    | _ ->
        Crash.crash $"ARM64 recursive nominal RC helper requires a generic root release plan, got {releasePlan}"

let internal generateClosureRefCountDecHelper
    (dictHelperForReleasePlan: MemoryModel.RcReleasePlan -> string)
    (ctx: CodeGenContext)
    : ARM64Symbolic.Instr list =
    let label (name: string) : string = $"__dark_closure_rc_dec_{name}"
    let ready = label "payload_ready"
    let helperRet = label "ret"
    let skipFreelist = label "skip_freelist"
    let capturesReleased = label "captures_released"
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

    let rec releaseFixedChildField
        (baseReg: ARM64Symbolic.Reg)
        (fieldOffset: int)
        (payloadSize: int)
        (fieldReleasePlan: MemoryModel.RcReleasePlan)
        (doneLabel: string)
        : ARM64Symbolic.Instr list =
        let childDone = label $"{doneLabel}_child_{fieldOffset}_done"
        let childSkipFreelist = label $"{doneLabel}_child_{fieldOffset}_skip_freelist"
        let childFieldReleaseInstrs =
            match fieldReleasePlan with
            | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases)) ->
                fieldReleases
                |> List.collect (fun (MemoryModel.FieldRelease (childFieldOffset, childFieldReleasePlan)) ->
                    releaseFieldPlanFrom ARM64Symbolic.X11 childFieldOffset childFieldReleasePlan childDone)
            | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease (_, _, variants)) ->
                releaseBoxedSumVariantFieldsFrom ARM64Symbolic.X11 variants childDone
            | _ ->
                []
        let releaseChildFields =
            if List.isEmpty childFieldReleaseInstrs then
                []
            else
                [
                    ARM64Symbolic.STP_pre (ARM64Symbolic.X11, ARM64Symbolic.X12, ARM64Symbolic.SP, -16s)
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X11, ARM64Symbolic.X12)
                ]
                @ childFieldReleaseInstrs
                @ [ARM64Symbolic.LDP_post (ARM64Symbolic.X11, ARM64Symbolic.X12, ARM64Symbolic.SP, 16s)]
        [
            ARM64Symbolic.LDR (ARM64Symbolic.X12, baseReg, int16 fieldOffset)
            ARM64Symbolic.CBZ (ARM64Symbolic.X12, childDone)
            ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X12, int16 payloadSize)
            ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
            ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X12, int16 payloadSize)
            ARM64Symbolic.CBNZ (ARM64Symbolic.X15, childDone)
        ]
        @ releaseChildFields
        @ (if payloadSize >= 0 && payloadSize < 256 then
            [
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X27, uint16 payloadSize)
                ARM64Symbolic.LDR (ARM64Symbolic.X14, ARM64Symbolic.X13, 0s)
                ARM64Symbolic.STR (ARM64Symbolic.X14, ARM64Symbolic.X12, 0s)
                ARM64Symbolic.STR (ARM64Symbolic.X12, ARM64Symbolic.X13, 0s)
            ]
           else
            [ARM64Symbolic.B_label childSkipFreelist])
        @ [
            ARM64Symbolic.Label childSkipFreelist
        ]
        @ leakDec
        @ [ARM64Symbolic.Label childDone]

    and releaseDynamicBufferChildField
        (baseReg: ARM64Symbolic.Reg)
        (fieldOffset: int)
        (operation: MemoryModel.RcOperation)
        (doneLabel: string)
        : ARM64Symbolic.Instr list =
        let bufferDone = label $"{doneLabel}_dynamic_buffer_{fieldOffset}_done"
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
                    ARM64Symbolic.CBNZ (ARM64Symbolic.X15, bufferDone)
                ] @ leakDec
        let taggedGuard =
            match operation with
            | MemoryModel.DynamicIntBuffer ->
                [ ARM64Symbolic.AND_imm (ARM64Symbolic.X13, ARM64Symbolic.X12, 1UL)
                  ARM64Symbolic.CBNZ (ARM64Symbolic.X13, bufferDone) ]
            | _ -> []
        [
            ARM64Symbolic.LDR (ARM64Symbolic.X12, baseReg, int16 fieldOffset)
            ARM64Symbolic.CBZ (ARM64Symbolic.X12, bufferDone)
        ]
        @ taggedGuard
        @ [
            ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)
            ARM64Symbolic.MOVZ (ARM64Symbolic.X13, 0xFFFFus, 0)
            ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 16)
            ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 32)
            ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0x7FFFus, 48)
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X15, ARM64Symbolic.X13)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, bufferDone)
        ]
        @ refcountUpdate
        @ [ARM64Symbolic.Label bufferDone]

    and releaseManagedRootChildField
        (baseReg: ARM64Symbolic.Reg)
        (fieldOffset: int)
        (helperLabel: string)
        (doneLabel: string)
        : ARM64Symbolic.Instr list =
        let childDone = label $"{doneLabel}_child_root_{fieldOffset}_done"
        [
            ARM64Symbolic.LDR (ARM64Symbolic.X12, baseReg, int16 fieldOffset)
            ARM64Symbolic.CBZ (ARM64Symbolic.X12, childDone)
            ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X8, ARM64Symbolic.SP, -32s)
            ARM64Symbolic.STR (ARM64Symbolic.X30, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.X12)
            ARM64Symbolic.BL helperLabel
            ARM64Symbolic.LDR (ARM64Symbolic.X30, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X8, ARM64Symbolic.SP, 32s)
            ARM64Symbolic.Label childDone
        ]

    and releaseFieldPlanFrom
        (baseReg: ARM64Symbolic.Reg)
        (fieldOffset: int)
        (fieldReleasePlan: MemoryModel.RcReleasePlan)
        (doneLabel: string)
        : ARM64Symbolic.Instr list =
        let dictHelperForChildPlan (fieldReleasePlan: MemoryModel.RcReleasePlan) : string =
            match fieldReleasePlan with
            | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, MemoryModel.RootRelease (_, MemoryModel.TaggedList, _))) ->
                dictRefCountDecListValueHelperLabel
            | _ ->
                dictRefCountDecHelperLabel

        match fieldReleasePlan with
        | MemoryModel.DynamicBufferRelease operation ->
            releaseDynamicBufferChildField baseReg fieldOffset operation doneLabel
        | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
            releaseManagedRootChildField baseReg fieldOffset listRefCountDecHelperLabel doneLabel
        | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
            releaseManagedRootChildField baseReg fieldOffset (dictHelperForChildPlan fieldReleasePlan) doneLabel
        | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
            releaseManagedRootChildField baseReg fieldOffset closureRefCountDecHelperLabel doneLabel
        | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) ->
            releaseManagedRootChildField baseReg fieldOffset streamRefCountDecHelperLabel doneLabel
        | MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease _)
        | MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease _) ->
            releaseFixedChildField baseReg fieldOffset payloadSize fieldReleasePlan doneLabel
        | _ ->
            []

    and releaseBoxedSumVariantFieldsFrom
        (baseReg: ARM64Symbolic.Reg)
        (variants: MemoryModel.RcBoxedSumVariantRelease list)
        (doneLabel: string)
        : ARM64Symbolic.Instr list =
        let releaseVariant (variant: MemoryModel.RcBoxedSumVariantRelease) : (int * ARM64Symbolic.Instr list) option =
            let releaseInstrs =
                variant.FieldReleases
                |> List.collect (fun (MemoryModel.FieldRelease (fieldOffset, fieldReleasePlan)) ->
                    releaseFieldPlanFrom baseReg fieldOffset fieldReleasePlan doneLabel)

            if List.isEmpty releaseInstrs then
                None
            else
                Some (variant.Tag, releaseInstrs)

        let cases = variants |> List.choose releaseVariant

        if List.isEmpty cases then
            []
        else
            let sumDone = label $"{doneLabel}_sum_done"
            [
                ARM64Symbolic.LDR (ARM64Symbolic.X10, baseReg, 0s)
            ]
            @
            (cases
             |> List.mapi (fun index (tag, releaseInstrs) ->
                let nextCase = label $"{doneLabel}_sum_variant_{index}_next"
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

    let releaseDynamicCapture (skipTagged: bool) (fieldOffset: int) (doneLabel: string) : ARM64Symbolic.Instr list =
        let bufferDone = label $"{doneLabel}_dynamic_capture_{fieldOffset}_done"
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
                    ARM64Symbolic.CBNZ (ARM64Symbolic.X15, bufferDone)
                ] @ leakDec
        let taggedGuard =
            if skipTagged then
                [ ARM64Symbolic.AND_imm (ARM64Symbolic.X13, ARM64Symbolic.X12, 1UL)
                  ARM64Symbolic.CBNZ (ARM64Symbolic.X13, bufferDone) ]
            else
                []
        [
            ARM64Symbolic.LDR (ARM64Symbolic.X12, ARM64Symbolic.X0, int16 fieldOffset)
            ARM64Symbolic.CBZ (ARM64Symbolic.X12, bufferDone)
        ]
        @ taggedGuard
        @ [
            ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)
            ARM64Symbolic.MOVZ (ARM64Symbolic.X13, 0xFFFFus, 0)
            ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 16)
            ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 32)
            ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0x7FFFus, 48)
            ARM64Symbolic.CMP_reg (ARM64Symbolic.X15, ARM64Symbolic.X13)
            ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, bufferDone)
        ]
        @ refcountUpdate
        @ [ARM64Symbolic.Label bufferDone]

    let fixedBlockFieldReleases
        (releasePlan: MemoryModel.RcReleasePlan)
        (doneLabel: string)
        : ARM64Symbolic.Instr list =
        match releasePlan with
        | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases)) ->
            fieldReleases
            |> List.collect (fun (MemoryModel.FieldRelease (fieldOffset, fieldReleasePlan)) ->
                releaseFieldPlanFrom ARM64Symbolic.X8 fieldOffset fieldReleasePlan doneLabel)
        | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease (_, _, variants)) ->
            releaseBoxedSumVariantFieldsFrom ARM64Symbolic.X8 variants doneLabel
        | _ ->
            []

    let releaseFixedCapture (fieldOffset: int) (releasePlan: MemoryModel.RcReleasePlan) (payloadSize: int) (doneLabel: string) : ARM64Symbolic.Instr list =
        let captureDone = label $"{doneLabel}_field_{fieldOffset}_done"
        let captureSkipFreelist = label $"{doneLabel}_field_{fieldOffset}_skip_freelist"
        [
            ARM64Symbolic.LDR (ARM64Symbolic.X8, ARM64Symbolic.X0, int16 fieldOffset)
            ARM64Symbolic.CBZ (ARM64Symbolic.X8, captureDone)
            ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X8, int16 payloadSize)
            ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
            ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X8, int16 payloadSize)
            ARM64Symbolic.CBNZ (ARM64Symbolic.X15, captureDone)
        ]
        @ fixedBlockFieldReleases releasePlan doneLabel
        @ (if payloadSize >= 0 && payloadSize < 256 then
            [
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X27, uint16 payloadSize)
                ARM64Symbolic.LDR (ARM64Symbolic.X14, ARM64Symbolic.X13, 0s)
                ARM64Symbolic.STR (ARM64Symbolic.X14, ARM64Symbolic.X8, 0s)
                ARM64Symbolic.STR (ARM64Symbolic.X8, ARM64Symbolic.X13, 0s)
            ]
           else
            [ARM64Symbolic.B_label captureSkipFreelist])
        @ [
            ARM64Symbolic.Label captureSkipFreelist
        ]
        @ leakDec
        @ [ARM64Symbolic.Label captureDone]

    let releaseCaptureCases =
        ctx.ClosureCaptureTypes
        |> Map.toList
        |> List.mapi (fun index (funcName, captureTypes) ->
            let nextCase = label $"captures_next_{index}"
            let releaseLabel = label $"captures_release_{index}"
            let releaseInstrs =
                captureTypes
                |> List.mapi (fun captureIndex captureType ->
                    let fieldOffset = (captureIndex + 1) * 8
                    match captureType with
                    | AST.TString
                    | AST.TChar
                    | AST.TBlob ->
                        releaseDynamicCapture false fieldOffset $"captures_{index}_{captureIndex}"
                    | AST.TInt ->
                        releaseDynamicCapture true fieldOffset $"captures_{index}_{captureIndex}"
                    | _ ->
                        match tryRcReleasePlanOfType ctx.RecordRegistry ctx.SumShapeRegistry captureType with
                        | Some (MemoryModel.RootRelease (_, MemoryModel.TaggedList, MemoryModel.TaggedListPayloadRelease elementRelease)) ->
                            let helper =
                                match elementRelease with
                                | MemoryModel.NoReleasePlan -> listRefCountDecHelperLabel
                                | MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer -> listRefCountDecStringHelperLabel
                                | MemoryModel.DynamicBufferRelease MemoryModel.DynamicBlobBuffer -> listRefCountDecBlobHelperLabel
                                | MemoryModel.DynamicBufferRelease MemoryModel.DynamicIntBuffer -> listRefCountDecBlobHelperLabel
                                | MemoryModel.DynamicBufferRelease _ -> Crash.crash "closure list release used a fixed-size dynamic-buffer operation"
                                | MemoryModel.RecursiveRelease sourceType ->
                                    plannedListDecHelperLabelForReleasePlan (MemoryModel.RecursiveRelease sourceType)
                                | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) -> listRefCountDecListHelperLabel
                                | MemoryModel.RootRelease (
                                      _,
                                      MemoryModel.DictHeap,
                                      MemoryModel.DictPayloadRelease (MemoryModel.DynamicBufferRelease _, _))
                                | MemoryModel.RootRelease (
                                      _,
                                      MemoryModel.DictHeap,
                                      MemoryModel.DictPayloadRelease (_, MemoryModel.DynamicBufferRelease _)) ->
                                    plannedListDecHelperLabelForReleasePlan elementRelease
                                | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, MemoryModel.RootRelease (_, MemoryModel.TaggedList, _))) ->
                                    listRefCountDecDictListHelperLabel
                                | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) -> listRefCountDecDictHelperLabel
                                | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) -> listRefCountDecClosureHelperLabel
                                | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) ->
                                    plannedListDecHelperLabelForReleasePlan elementRelease
                                | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, _) ->
                                    plannedListDecHelperLabelForReleasePlan elementRelease
                            releaseManagedRootChildField ARM64Symbolic.X0 fieldOffset helper $"captures_{index}_{captureIndex}"
                        | Some (MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) as releasePlan) ->
                            releaseManagedRootChildField
                                ARM64Symbolic.X0
                                fieldOffset
                                (dictHelperForReleasePlan releasePlan)
                                $"captures_{index}_{captureIndex}"
                        | Some (MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _)) ->
                            releaseManagedRootChildField ARM64Symbolic.X0 fieldOffset closureRefCountDecHelperLabel $"captures_{index}_{captureIndex}"
                        | Some (MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _)) ->
                            releaseManagedRootChildField ARM64Symbolic.X0 fieldOffset streamRefCountDecHelperLabel $"captures_{index}_{captureIndex}"
                        | Some (MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, (MemoryModel.FixedBlockPayloadRelease _ | MemoryModel.BoxedSumPayloadRelease _)) as releasePlan) ->
                            releaseFixedCapture fieldOffset releasePlan payloadSize $"captures_{index}_{captureIndex}"
                        | _ ->
                            [])
                |> List.concat
            [
                ARM64Symbolic.ADR (ARM64Symbolic.X11, codeLabel funcName)
                ARM64Symbolic.CMP_reg (ARM64Symbolic.X9, ARM64Symbolic.X11)
                ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, nextCase)
                ARM64Symbolic.Label releaseLabel
            ]
            @ releaseInstrs
            @ [
                ARM64Symbolic.B_label capturesReleased
                ARM64Symbolic.Label nextCase
            ])
        |> List.concat

    [
        ARM64Symbolic.Label closureRefCountDecHelperLabel
        ARM64Symbolic.CBZ (ARM64Symbolic.X0, helperRet)
    ]
    @ generateClosurePayloadSizeResolver ctx label ready
    @ [
        ARM64Symbolic.Label ready
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X12, ARM64Symbolic.X0, ARM64Symbolic.X10)
        ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)
        ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
        ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)
        ARM64Symbolic.CBNZ (ARM64Symbolic.X15, helperRet)
    ]
    @ releaseCaptureCases
    @ [
        ARM64Symbolic.Label capturesReleased
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X10, 256us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, skipFreelist)
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X27, ARM64Symbolic.X10)
        ARM64Symbolic.LDR (ARM64Symbolic.X14, ARM64Symbolic.X13, 0s)
        ARM64Symbolic.STR (ARM64Symbolic.X14, ARM64Symbolic.X0, 0s)
        ARM64Symbolic.STR (ARM64Symbolic.X0, ARM64Symbolic.X13, 0s)
        ARM64Symbolic.Label skipFreelist
    ]
    @ leakDec
    @ [
        ARM64Symbolic.Label helperRet
        ARM64Symbolic.RET
    ]

let internal generateStreamRefCountDecHelper (ctx: CodeGenContext) : ARM64Symbolic.Instr list =
    let helperRet = $"{streamRefCountDecHelperLabel}_ret"
    let alreadyClosed = $"{streamRefCountDecHelperLabel}_closed"
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
    [
        ARM64Symbolic.Label streamRefCountDecHelperLabel
        ARM64Symbolic.CBZ (ARM64Symbolic.X0, helperRet)
        ARM64Symbolic.LDR (ARM64Symbolic.X1, ARM64Symbolic.X0, 24s)
        ARM64Symbolic.SUB_imm (ARM64Symbolic.X1, ARM64Symbolic.X1, 1us)
        ARM64Symbolic.STR (ARM64Symbolic.X1, ARM64Symbolic.X0, 24s)
        ARM64Symbolic.CBNZ (ARM64Symbolic.X1, helperRet)
        ARM64Symbolic.STP_pre (ARM64Symbolic.X19, ARM64Symbolic.X30, ARM64Symbolic.SP, -16s)
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X19, ARM64Symbolic.X0)
        ARM64Symbolic.LDR (ARM64Symbolic.X2, ARM64Symbolic.X19, 0s)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X2, 5us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, alreadyClosed)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 5us, 0)
        ARM64Symbolic.STR (ARM64Symbolic.X2, ARM64Symbolic.X19, 0s)
        ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X19, 16s)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X1, 0us, 0)
        ARM64Symbolic.LDR (ARM64Symbolic.X9, ARM64Symbolic.X0, 0s)
        ARM64Symbolic.BLR ARM64Symbolic.X9
        ARM64Symbolic.Label alreadyClosed
        ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X19, 8s)
        ARM64Symbolic.BL closureRefCountDecHelperLabel
        ARM64Symbolic.LDR (ARM64Symbolic.X0, ARM64Symbolic.X19, 16s)
        ARM64Symbolic.BL closureRefCountDecHelperLabel
        ARM64Symbolic.LDR (ARM64Symbolic.X14, ARM64Symbolic.X27, 24s)
        ARM64Symbolic.STR (ARM64Symbolic.X14, ARM64Symbolic.X19, 0s)
        ARM64Symbolic.STR (ARM64Symbolic.X19, ARM64Symbolic.X27, 24s)
    ]
    @ leakDec
    @ [
        ARM64Symbolic.LDP_post (ARM64Symbolic.X19, ARM64Symbolic.X30, ARM64Symbolic.SP, 16s)
        ARM64Symbolic.Label helperRet
        ARM64Symbolic.RET
    ]
