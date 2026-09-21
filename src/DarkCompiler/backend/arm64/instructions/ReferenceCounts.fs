// ReferenceCounts.fs - Emit arm64 instructions for referencecounts operations.

module ARM64EmitReferenceCounts

open ARM64CodeGenTypes
open ARM64ClosureReferenceCounts
open ARM64ReleaseSelection
open ARM64LeakAccounting
open ARM64Operands

let internal emitRefCountInc (ctx: CodeGenContext) (addr: LIR.Reg) (payloadSize: int) (kind: LIR.RcKind) : Result<ARM64Symbolic.Instr list, string> =
    // Generic RC increment for heap values.
    // RcKind controls list-helper dispatch explicitly (no payload-size heuristics).
    lirRegToARM64Reg addr
    |> Result.map (fun addrReg ->
        let tupleIncPath = [
            ARM64Symbolic.LDR (ARM64Symbolic.X15, addrReg, int16 payloadSize)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
            ARM64Symbolic.STR (ARM64Symbolic.X15, addrReg, int16 payloadSize)
        ]

        match kind with
        | LIR.TaggedList ->
            let listIncCall = [
                ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -64s)
                ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, addrReg)
                ARM64Symbolic.BL listRefCountIncHelperLabel
                ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 64s)
            ]
            let listCallLen = List.length listIncCall
            [
                ARM64Symbolic.CBZ_offset (addrReg, listCallLen + 1)
            ]
            @ listIncCall
        | LIR.DictHeap ->
            let dictIncCall = [
                ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -80s)
                ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, addrReg)
                ARM64Symbolic.BL dictRefCountIncHelperLabel
                ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 80s)
            ]
            [
                ARM64Symbolic.CBZ_offset (addrReg, List.length dictIncCall + 1)
            ]
            @ dictIncCall
        | LIR.ClosureHeap ->
            let closureIncCall = [
                ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -96s)
                ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                ARM64Symbolic.STP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, addrReg)
                ARM64Symbolic.BL closureRefCountIncHelperLabel
                ARM64Symbolic.LDP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 96s)
            ]
            [
                ARM64Symbolic.CBZ_offset (addrReg, List.length closureIncCall + 1)
            ]
            @ closureIncCall
        | LIR.GenericHeap
        | LIR.StreamHeap ->
            [
                ARM64Symbolic.CBZ_offset (addrReg, 4)
            ] @ tupleIncPath)

let internal emitRefCountDec (ctx: CodeGenContext) (addr: LIR.Reg) (payloadSize: int) (kind: LIR.RcKind) (metadata: MemoryModel.RcMetadata option) : Result<ARM64Symbolic.Instr list, string> =
    // Decrement ref count at [addr + payloadSize]
    // Skip if addr is null (e.g., empty list = 0)
    // When ref count hits 0, add block to free list for memory reuse
    //
    // Free list structure:
    // - X27 = base of free list heads (32 slots × 8 bytes = 256 bytes)
    // - Slot N contains head of free list for blocks of size (N+1)*8 bytes
    // - sizeClassOffset = payloadSize (for 8-aligned payloads)
    // - Freed blocks use first 8 bytes as next pointer
    //
    // Code structure (8 instructions, plus optional leak counter update):
    //   CBZ addr, +8                      ; If null, skip all 7 instructions
    //   LDR X15, [addr, payloadSize]      ; Load ref count
    //   SUB X15, X15, 1                   ; Decrement
    //   STR X15, [addr, payloadSize]      ; Store back
    //   CBNZ X15, +4                      ; If not zero, skip free list code (4 instrs)
    //   LDR X14, [X27, payloadSize]       ; Load current free list head
    //   STR X14, [addr, 0]                ; Store old head as next in freed block
    //   STR addr, [X27, payloadSize]      ; Update free list head to freed block
    //   (continue)
    lirRegToARM64Reg addr
    |> Result.map (fun addrReg ->
        let releasePlan = rcMetadataReleasePlan metadata
        let leakDec = generateLeakCounterDec ctx
        // Keep this expansion deferred. Most release kinds use a runtime
        // helper, and complex generic roots are normally outlined below.
        let tupleDecPath () =
            let releaseListFieldFromHelper (baseReg: ARM64Symbolic.Reg) (fieldOffset: int) (helperLabel: string) : ARM64Symbolic.Instr list =
                let fieldReg =
                    if baseReg = ARM64Symbolic.X12 then ARM64Symbolic.X16 else ARM64Symbolic.X12
                let callInstrs = [
                    ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -128s)
                    ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                    ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                    ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                    ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                    ARM64Symbolic.STP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                    ARM64Symbolic.STP (ARM64Symbolic.X12, ARM64Symbolic.X13, ARM64Symbolic.SP, 96s)
                    ARM64Symbolic.STP (ARM64Symbolic.X14, ARM64Symbolic.X15, ARM64Symbolic.SP, 112s)
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, fieldReg)
                    ARM64Symbolic.BL helperLabel
                    ARM64Symbolic.LDP (ARM64Symbolic.X14, ARM64Symbolic.X15, ARM64Symbolic.SP, 112s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X12, ARM64Symbolic.X13, ARM64Symbolic.SP, 96s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                    ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 128s)
                ]
                [
                    ARM64Symbolic.LDR (fieldReg, baseReg, int16 fieldOffset)
                    ARM64Symbolic.CBZ_offset (fieldReg, List.length callInstrs + 1)
                ] @ callInstrs

            let releaseListFieldFromPlan (baseReg: ARM64Symbolic.Reg) (fieldOffset: int) (fieldReleasePlan: MemoryModel.RcReleasePlan) : ARM64Symbolic.Instr list =
                releaseListFieldFromHelper baseReg fieldOffset (listDecHelperForReleasePlan fieldReleasePlan)

            let releaseDictFieldFromHelper (baseReg: ARM64Symbolic.Reg) (fieldOffset: int) (helperLabel: string) : ARM64Symbolic.Instr list =
                let fieldReg =
                    if baseReg = ARM64Symbolic.X12 then ARM64Symbolic.X16 else ARM64Symbolic.X12
                let callInstrs = [
                    ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -128s)
                    ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                    ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                    ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                    ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                    ARM64Symbolic.STP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                    ARM64Symbolic.STP (ARM64Symbolic.X12, ARM64Symbolic.X13, ARM64Symbolic.SP, 96s)
                    ARM64Symbolic.STP (ARM64Symbolic.X14, ARM64Symbolic.X15, ARM64Symbolic.SP, 112s)
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, fieldReg)
                    ARM64Symbolic.BL helperLabel
                    ARM64Symbolic.LDP (ARM64Symbolic.X14, ARM64Symbolic.X15, ARM64Symbolic.SP, 112s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X12, ARM64Symbolic.X13, ARM64Symbolic.SP, 96s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                    ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 128s)
                ]
                [
                    ARM64Symbolic.LDR (fieldReg, baseReg, int16 fieldOffset)
                    ARM64Symbolic.CBZ_offset (fieldReg, List.length callInstrs + 1)
                ] @ callInstrs

            let releaseDictFieldFromPlan (baseReg: ARM64Symbolic.Reg) (fieldOffset: int) (fieldReleasePlan: MemoryModel.RcReleasePlan) : ARM64Symbolic.Instr list =
                releaseDictFieldFromHelper baseReg fieldOffset (dictDecHelperForReleasePlan fieldReleasePlan)

            let releaseClosureFieldFrom (baseReg: ARM64Symbolic.Reg) (fieldOffset: int) : ARM64Symbolic.Instr list =
                let fieldReg =
                    if baseReg = ARM64Symbolic.X12 then ARM64Symbolic.X16 else ARM64Symbolic.X12
                let callInstrs = [
                    ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -128s)
                    ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                    ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                    ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                    ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                    ARM64Symbolic.STP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                    ARM64Symbolic.STP (ARM64Symbolic.X12, ARM64Symbolic.X13, ARM64Symbolic.SP, 96s)
                    ARM64Symbolic.STP (ARM64Symbolic.X14, ARM64Symbolic.X15, ARM64Symbolic.SP, 112s)
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, fieldReg)
                    ARM64Symbolic.BL closureRefCountDecHelperLabel
                    ARM64Symbolic.LDP (ARM64Symbolic.X14, ARM64Symbolic.X15, ARM64Symbolic.SP, 112s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X12, ARM64Symbolic.X13, ARM64Symbolic.SP, 96s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                    ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                    ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 128s)
                ]
                [
                    ARM64Symbolic.LDR (fieldReg, baseReg, int16 fieldOffset)
                    ARM64Symbolic.CBZ_offset (fieldReg, List.length callInstrs + 1)
                ] @ callInstrs

            let releaseDynamicBufferFieldFrom
                (baseReg: ARM64Symbolic.Reg)
                (fieldOffset: int)
                (operation: MemoryModel.RcOperation)
                : ARM64Symbolic.Instr list =
                let bufferLeakDec = generateLeakCounterDec ctx
                let refcountUpdate =
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
                let bcondOffset = List.length refcountUpdate + 1
                let taggedGuard =
                    match operation with
                    | MemoryModel.DynamicIntBuffer ->
                        [ ARM64Symbolic.AND_imm (ARM64Symbolic.X13, ARM64Symbolic.X12, 1UL)
                          ARM64Symbolic.CBNZ_offset (ARM64Symbolic.X13, 8 + List.length refcountUpdate) ]
                    | _ -> []
                let body =
                    [
                        ARM64Symbolic.LDR (ARM64Symbolic.X12, baseReg, int16 fieldOffset)
                        ARM64Symbolic.CBZ_offset (ARM64Symbolic.X12, List.length taggedGuard + 8 + List.length refcountUpdate)
                    ]
                    @ taggedGuard
                    @ [
                        ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X12, 0s)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X13, 0xFFFFus, 0)
                        ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 16)
                        ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0xFFFFus, 32)
                        ARM64Symbolic.MOVK (ARM64Symbolic.X13, 0x7FFFus, 48)
                        ARM64Symbolic.CMP_reg (ARM64Symbolic.X15, ARM64Symbolic.X13)
                        ARM64Symbolic.B_cond (ARM64Symbolic.EQ, bcondOffset)
                    ] @ refcountUpdate
                [
                    ARM64Symbolic.STP_pre (ARM64Symbolic.X12, ARM64Symbolic.X13, ARM64Symbolic.SP, -32s)
                    ARM64Symbolic.STP (ARM64Symbolic.X14, ARM64Symbolic.X15, ARM64Symbolic.SP, 16s)
                ]
                @ body
                @ [
                    ARM64Symbolic.LDP (ARM64Symbolic.X14, ARM64Symbolic.X15, ARM64Symbolic.SP, 16s)
                    ARM64Symbolic.LDP_post (ARM64Symbolic.X12, ARM64Symbolic.X13, ARM64Symbolic.SP, 32s)
                ]

            let rec releaseFieldPlanFrom
                (baseReg: ARM64Symbolic.Reg)
                (fieldOffset: int)
                (fieldReleasePlan: MemoryModel.RcReleasePlan)
                : ARM64Symbolic.Instr list =
                match fieldReleasePlan with
                | MemoryModel.DynamicBufferRelease operation ->
                    releaseDynamicBufferFieldFrom baseReg fieldOffset operation
                | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
                    releaseListFieldFromPlan baseReg fieldOffset fieldReleasePlan
                | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
                    releaseDictFieldFromPlan baseReg fieldOffset fieldReleasePlan
                | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
                    releaseClosureFieldFrom baseReg fieldOffset
                | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) ->
                    releaseListFieldFromHelper baseReg fieldOffset streamRefCountDecHelperLabel
                | MemoryModel.RootRelease (childPayloadSize, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease _)
                | MemoryModel.RootRelease (childPayloadSize, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease _) ->
                    releaseFixedBlockFieldWithPlan baseReg fieldOffset childPayloadSize fieldReleasePlan
                | MemoryModel.RecursiveRelease sourceType ->
                    releaseListFieldFromHelper
                        baseReg
                        fieldOffset
                        (recursiveNominalRefCountDecHelperLabel sourceType)
                | _ ->
                    []

            and releaseBoxedSumVariantFieldsFrom
                (baseReg: ARM64Symbolic.Reg)
                (variants: MemoryModel.RcBoxedSumVariantRelease list)
                : ARM64Symbolic.Instr list =
                let releaseVariant (variant: MemoryModel.RcBoxedSumVariantRelease) : (int * ARM64Symbolic.Instr list) option =
                    let releaseInstrs =
                        variant.FieldReleases
                        |> List.collect (fun (MemoryModel.FieldRelease (fieldOffset, fieldReleasePlan)) ->
                            releaseFieldPlanFrom baseReg fieldOffset fieldReleasePlan)

                    if List.isEmpty releaseInstrs then
                        None
                    else
                        Some (variant.Tag, releaseInstrs)

                let rec variantCases (cases: (int * ARM64Symbolic.Instr list) list) : ARM64Symbolic.Instr list =
                    match cases with
                    | [] ->
                        []
                    | (tag, releaseInstrs) :: rest ->
                        let restInstrs = variantCases rest
                        let branchToEnd =
                            if List.isEmpty restInstrs then
                                []
                            else
                                [ARM64Symbolic.B (List.length restInstrs + 1)]
                        [
                            ARM64Symbolic.CMP_imm (ARM64Symbolic.X10, uint16 tag)
                            ARM64Symbolic.B_cond (ARM64Symbolic.NE, List.length releaseInstrs + List.length branchToEnd + 1)
                        ]
                        @ releaseInstrs
                        @ branchToEnd
                        @ restInstrs

                let cases = variants |> List.choose releaseVariant

                if List.isEmpty cases then
                    []
                else
                    [ARM64Symbolic.LDR (ARM64Symbolic.X10, baseReg, 0s)]
                    @ variantCases cases

            and releaseFixedBlockFieldWithPlan
                (baseReg: ARM64Symbolic.Reg)
                (fieldOffset: int)
                (childPayloadSize: int)
                (fieldReleasePlan: MemoryModel.RcReleasePlan)
                : ARM64Symbolic.Instr list =
                    let childFieldReleaseInstrs =
                        match fieldReleasePlan with
                        | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases)) ->
                            fieldReleases
                            |> List.collect (fun (MemoryModel.FieldRelease (childFieldOffset, fieldReleasePlan)) ->
                                releaseFieldPlanFrom ARM64Symbolic.X11 childFieldOffset fieldReleasePlan)
                        | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease (_, _, variants)) ->
                            releaseBoxedSumVariantFieldsFrom ARM64Symbolic.X11 variants
                        | _ ->
                            []
                    let childLeakDec = generateLeakCounterDec ctx
                    let freeChild =
                        (if List.isEmpty childFieldReleaseInstrs then
                            []
                         else
                            [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X11, ARM64Symbolic.X12) ]
                            @ childFieldReleaseInstrs
                            @ [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X12, ARM64Symbolic.X11) ])
                        @
                        (if childPayloadSize >= 0 && childPayloadSize < 256 then
                            [
                                ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X27, uint16 childPayloadSize)
                                ARM64Symbolic.LDR (ARM64Symbolic.X14, ARM64Symbolic.X13, 0s)
                                ARM64Symbolic.STR (ARM64Symbolic.X14, ARM64Symbolic.X12, 0s)
                                ARM64Symbolic.STR (ARM64Symbolic.X12, ARM64Symbolic.X13, 0s)
                            ]
                         else
                            [])
                        @ childLeakDec

                    let afterDec =
                        if List.isEmpty freeChild then
                            []
                        else
                            [ARM64Symbolic.CBNZ_offset (ARM64Symbolic.X15, List.length freeChild + 1)]
                            @ freeChild
                    let body =
                        [
                            ARM64Symbolic.LDR (ARM64Symbolic.X12, baseReg, int16 fieldOffset)
                            ARM64Symbolic.CBZ_offset (ARM64Symbolic.X12, 3 + List.length afterDec + 1)
                            ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X12, int16 childPayloadSize)
                            ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
                            ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X12, int16 childPayloadSize)
                        ] @ afterDec

                    [
                        ARM64Symbolic.STP_pre (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, -48s)
                        ARM64Symbolic.STP (ARM64Symbolic.X12, ARM64Symbolic.X13, ARM64Symbolic.SP, 16s)
                        ARM64Symbolic.STP (ARM64Symbolic.X14, ARM64Symbolic.X15, ARM64Symbolic.SP, 32s)
                    ]
                    @ body
                    @ [
                        ARM64Symbolic.LDP (ARM64Symbolic.X14, ARM64Symbolic.X15, ARM64Symbolic.SP, 32s)
                        ARM64Symbolic.LDP (ARM64Symbolic.X12, ARM64Symbolic.X13, ARM64Symbolic.SP, 16s)
                        ARM64Symbolic.LDP_post (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 48s)
                    ]

            let fixedBlockFieldReleaseInstrs =
                releasePlan
                |> Option.map (function
                    | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases)) ->
                        fieldReleases
                        |> List.collect (fun (MemoryModel.FieldRelease (fieldOffset, fieldReleasePlan)) ->
                            releaseFieldPlanFrom ARM64Symbolic.X11 fieldOffset fieldReleasePlan)
                    | _ ->
                        [])
                |> Option.defaultValue []

            let releaseSumPayloadInstrs =
                match releasePlan with
                | Some (MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease (_, _, variants))) ->
                    if List.length variants = 1
                       && not (callerOwnsSinglePayloadSum ctx.FunctionName) then
                        []
                    else
                        releaseBoxedSumVariantFieldsFrom ARM64Symbolic.X11 variants
                | _ ->
                    []

            let fieldReleaseInstrs =
                fixedBlockFieldReleaseInstrs
                @ releaseSumPayloadInstrs

            // Field-release helpers use X10-X15 as scratch registers. Keep a
            // source allocated in that range in X11 while traversing, and
            // reload the generic root from the stack before free-list insertion.
            // Sources in X0-X9 remain directly addressable, which avoids an
            // unnecessary move and preserves the established instruction shape.
            let stableFieldReleaseInstrs =
                if List.isEmpty fieldReleaseInstrs then
                    []
                else
                    let stableBaseReg =
                        if List.contains
                               addrReg
                               [ ARM64Symbolic.X10; ARM64Symbolic.X11; ARM64Symbolic.X12
                                 ARM64Symbolic.X13; ARM64Symbolic.X14; ARM64Symbolic.X15 ] then
                            ARM64Symbolic.X11
                        else
                            addrReg
                    let releases =
                        releasePlan
                        |> Option.map (function
                            | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases)) ->
                                fieldReleases
                                |> List.collect (fun (MemoryModel.FieldRelease (fieldOffset, fieldReleasePlan)) ->
                                    releaseFieldPlanFrom stableBaseReg fieldOffset fieldReleasePlan)
                            | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease (_, _, variants)) ->
                                releaseBoxedSumVariantFieldsFrom stableBaseReg variants
                            | _ ->
                                [])
                        |> Option.defaultValue []
                    [
                        ARM64Symbolic.STP_pre (addrReg, ARM64Symbolic.X11, ARM64Symbolic.SP, -16s)
                    ]
                    @ (if stableBaseReg = ARM64Symbolic.X11 && addrReg <> ARM64Symbolic.X11 then
                           [ARM64Symbolic.MOV_reg (ARM64Symbolic.X11, addrReg)]
                       else
                           [])
                    @ releases
                    @ [
                        ARM64Symbolic.LDR (addrReg, ARM64Symbolic.SP, 0s)
                        ARM64Symbolic.LDR (ARM64Symbolic.X11, ARM64Symbolic.SP, 8s)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 16us)
                    ]

            let releaseInstrs =
                stableFieldReleaseInstrs
                @
                (if payloadSize >= 0 && payloadSize < 256 then
                    [
                        ARM64Symbolic.LDR (ARM64Symbolic.X14, ARM64Symbolic.X27, int16 payloadSize)
                        ARM64Symbolic.STR (ARM64Symbolic.X14, addrReg, 0s)
                        ARM64Symbolic.STR (addrReg, ARM64Symbolic.X27, int16 payloadSize)
                    ]
                 else
                    [])
                @ leakDec
            [
                ARM64Symbolic.LDR (ARM64Symbolic.X15, addrReg, int16 payloadSize)
                ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
                ARM64Symbolic.STR (ARM64Symbolic.X15, addrReg, int16 payloadSize)
            ]
            @ (if List.isEmpty releaseInstrs then
                []
               else
                [ARM64Symbolic.CBNZ_offset (ARM64Symbolic.X15, List.length releaseInstrs + 1)]
                @ releaseInstrs)

        match kind with
        | LIR.TaggedList ->
            let releasePlan =
                requiredRcMetadataReleasePlan "TaggedList RefCountDec" metadata
            let helperLabel =
                match releasePlan with
                | MemoryModel.RootRelease (_, _, MemoryModel.TaggedListPayloadRelease (MemoryModel.RootRelease (_, MemoryModel.GenericHeap, _) as elementRelease)) ->
                    plannedListDecHelperLabelForReleasePlan elementRelease
                | _ ->
                    listDecHelperForReleasePlan releasePlan
            let listDecCall = [
                ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -80s)
                ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, addrReg)
                ARM64Symbolic.BL helperLabel
                ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 80s)
            ]
            let listCallLen = List.length listDecCall
            [
                ARM64Symbolic.CBZ_offset (addrReg, listCallLen + 1)
            ]
            @ listDecCall
        | LIR.DictHeap ->
            let helperLabel =
                metadata
                |> requiredRcMetadataReleasePlan "DictHeap RefCountDec"
                |> dictDecHelperForReleasePlan
            let dictDecCall = [
                ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -96s)
                ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                ARM64Symbolic.STP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, addrReg)
                ARM64Symbolic.BL helperLabel
                ARM64Symbolic.LDP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 96s)
            ]
            [
                ARM64Symbolic.CBZ_offset (addrReg, List.length dictDecCall + 1)
            ]
            @ dictDecCall
        | LIR.ClosureHeap ->
            let closureDecCall = [
                ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -96s)
                ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                ARM64Symbolic.STP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, addrReg)
                ARM64Symbolic.BL closureRefCountDecHelperLabel
                ARM64Symbolic.LDP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 96s)
            ]
            [
                ARM64Symbolic.CBZ_offset (addrReg, List.length closureDecCall + 1)
            ]
            @ closureDecCall
        | LIR.StreamHeap ->
            let streamDecCall = [
                ARM64Symbolic.STP_pre (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, -96s)
                ARM64Symbolic.STP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.STP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                ARM64Symbolic.STP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                ARM64Symbolic.STP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                ARM64Symbolic.STP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, addrReg)
                ARM64Symbolic.BL streamRefCountDecHelperLabel
                ARM64Symbolic.LDP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 80s)
                ARM64Symbolic.LDP (ARM64Symbolic.X8, ARM64Symbolic.X9, ARM64Symbolic.SP, 64s)
                ARM64Symbolic.LDP (ARM64Symbolic.X6, ARM64Symbolic.X7, ARM64Symbolic.SP, 48s)
                ARM64Symbolic.LDP (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.SP, 32s)
                ARM64Symbolic.LDP (ARM64Symbolic.X2, ARM64Symbolic.X3, ARM64Symbolic.SP, 16s)
                ARM64Symbolic.LDP_post (ARM64Symbolic.X0, ARM64Symbolic.X1, ARM64Symbolic.SP, 96s)
            ]
            [ARM64Symbolic.CBZ_offset (addrReg, List.length streamDecCall + 1)] @ streamDecCall
        | LIR.GenericHeap ->
            let inlineDecPath = tupleDecPath ()
            let cbzOffset = List.length inlineDecPath + 1
            [ARM64Symbolic.CBZ_offset (addrReg, cbzOffset)] @ inlineDecPath)

let private emitRefCountIncBuffer (ctx: CodeGenContext) (skipTagged: bool) (str: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
    // Increment the leading refcount for a dynamic buffer.
    // Literal strings have refcount = INT64_MAX as sentinel (don't modify read-only memory)
    match str with
    | LIR.StringSymbol _ ->
        // Literal string - no refcount, no-op
        Ok []
    | LIR.Reg reg ->
        // Heap and materialized literal buffers keep the refcount at [addr].
        lirRegToARM64Reg reg
        |> Result.map (fun addrReg ->
            let refAddrReg, preserveAddr =
                if addrReg = ARM64Symbolic.X13 || addrReg = ARM64Symbolic.X15 then
                    ARM64Symbolic.X14, [ARM64Symbolic.MOV_reg (ARM64Symbolic.X14, addrReg)]
                else
                    addrReg, []
            let refcountPath = ([
                ARM64Symbolic.LDR (ARM64Symbolic.X15, refAddrReg, 0s)
            ] @ loadImmediate ARM64Symbolic.X13 System.Int64.MaxValue @ [
                ARM64Symbolic.CMP_reg (ARM64Symbolic.X15, ARM64Symbolic.X13)             // Compare with sentinel
                ARM64Symbolic.B_cond (ARM64Symbolic.EQ, 3)                       // If literal string, skip to end
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)        // X15++
                ARM64Symbolic.STR (ARM64Symbolic.X15, refAddrReg, 0s)             // store back
            ])
            let guards =
                if skipTagged then
                    [ ARM64Symbolic.CBZ_offset (refAddrReg, refcountPath.Length + 3)
                      ARM64Symbolic.AND_imm (ARM64Symbolic.X13, refAddrReg, 1UL)
                      ARM64Symbolic.CBNZ_offset (ARM64Symbolic.X13, refcountPath.Length + 1) ]
                else
                    []
            preserveAddr
            @ guards
            @ refcountPath)
    | _ -> Error "dynamic buffer RefCountInc requires StringSymbol or Reg operand"

let private emitRefCountDecBuffer (ctx: CodeGenContext) (skipTagged: bool) (str: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
    // Decrement the leading refcount for a dynamic buffer.
    // Literal strings have refcount = INT64_MAX as sentinel (don't modify read-only memory)
    match str with
    | LIR.StringSymbol _ ->
        // Literal string - no refcount, no-op
        Ok []
    | LIR.Reg reg ->
        // Heap and materialized literal buffers keep the refcount at [addr].
        lirRegToARM64Reg reg
        |> Result.map (fun addrReg ->
            let leakDec = generateLeakCounterDec ctx
            let refAddrReg, preserveAddr =
                if addrReg = ARM64Symbolic.X13 || addrReg = ARM64Symbolic.X15 then
                    ARM64Symbolic.X14, [ARM64Symbolic.MOV_reg (ARM64Symbolic.X14, addrReg)]
                else
                    addrReg, []
            let bcondOffset = if List.isEmpty leakDec then 3 else 9
            let refcountUpdate =
                if List.isEmpty leakDec then
                    [
                        ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)        // X15--
                        ARM64Symbolic.STR (ARM64Symbolic.X15, refAddrReg, 0s)             // store back
                    ]
                else
                    [
                        ARM64Symbolic.SUB_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)        // X15--
                        ARM64Symbolic.STR (ARM64Symbolic.X15, refAddrReg, 0s)             // store back
                        // If refcount hits 0, update leak counter (string freeing not implemented yet)
                        ARM64Symbolic.CBNZ_offset (ARM64Symbolic.X15, 6)                 // If not zero, skip leak counter
                    ] @ leakDec
            let refcountPath = ([
                ARM64Symbolic.LDR (ARM64Symbolic.X15, refAddrReg, 0s)
            ] @ loadImmediate ARM64Symbolic.X13 System.Int64.MaxValue @ [
                ARM64Symbolic.CMP_reg (ARM64Symbolic.X15, ARM64Symbolic.X13)             // Compare with sentinel
                ARM64Symbolic.B_cond (ARM64Symbolic.EQ, bcondOffset)             // If literal string, skip to end
            ] @ refcountUpdate)
            let guards =
                if skipTagged then
                    [ ARM64Symbolic.CBZ_offset (refAddrReg, refcountPath.Length + 3)
                      ARM64Symbolic.AND_imm (ARM64Symbolic.X13, refAddrReg, 1UL)
                      ARM64Symbolic.CBNZ_offset (ARM64Symbolic.X13, refcountPath.Length + 1) ]
                else
                    []
            preserveAddr
            @ guards
            @ refcountPath)
    | _ -> Error "dynamic buffer RefCountDec requires StringSymbol or Reg operand"

let internal emitRefCountIncString (ctx: CodeGenContext) (str: LIR.Operand) =
    emitRefCountIncBuffer ctx false str

let internal emitRefCountDecString (ctx: CodeGenContext) (str: LIR.Operand) =
    emitRefCountDecBuffer ctx false str

let internal emitRefCountIncInt (ctx: CodeGenContext) (value: LIR.Operand) =
    emitRefCountIncBuffer ctx true value

let internal emitRefCountDecInt (ctx: CodeGenContext) (value: LIR.Operand) =
    emitRefCountDecBuffer ctx true value
