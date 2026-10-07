(*
   ARM64EmitReferenceCounts.ml - Emit arm64 instructions for referencecounts operations.
*)
[@@@warning "-4"]
let isEmpty xs=xs=[]
let contains value values=List.mem value values
let optionDefault value = function None->value|Some result->result
let int16 value=let low=value land 65535 in if low>=32768 then low-65536 else low
let uint16 value=value land 65535



open ARM64CodeGenTypes
open ARM64ClosureReferenceCounts
open ARM64ReleaseSelection
open LeakAccounting
open ARM64Operands

(*
   Generic RC increment for heap values.
   RcKind controls list-helper dispatch explicitly (no payload-size heuristics).
*)
let emitRefCountInc (_ctx: codeGenContext) (addr: LIR.reg) (payloadSize: int) (kind: LIR.rcKind) : (Symbolic.instr list, string) result =


    lirRegToARM64Reg addr
    |> Result.map (fun addrReg ->
        let tupleIncPath = [
            Symbolic.LDR (Symbolic.X15, addrReg, int16 payloadSize);
            Symbolic.ADD_imm (Symbolic.X15, Symbolic.X15, 1);
            Symbolic.STR (Symbolic.X15, addrReg, int16 payloadSize)
        ]

        in
        (match kind with
        | LIR.TaggedList ->
            let listIncCall = [
                Symbolic.STP_pre (Symbolic.X0, Symbolic.X1, Symbolic.SP, -64);
                Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                Symbolic.MOV_reg (Symbolic.X0, addrReg);
                Symbolic.BL listRefCountIncHelperLabel;
                Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                Symbolic.LDP_post (Symbolic.X0, Symbolic.X1, Symbolic.SP, 64)
            ]
            in
            let listCallLen = List.length listIncCall
            in
            [
                Symbolic.CBZ_offset (addrReg, listCallLen + 1)
            ]
            @ listIncCall
        | LIR.DictHeap ->
            let dictIncCall = [
                Symbolic.STP_pre (Symbolic.X0, Symbolic.X1, Symbolic.SP, -80);
                Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                Symbolic.MOV_reg (Symbolic.X0, addrReg);
                Symbolic.BL dictRefCountIncHelperLabel;
                Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                Symbolic.LDP_post (Symbolic.X0, Symbolic.X1, Symbolic.SP, 80)
            ]
            in
            [
                Symbolic.CBZ_offset (addrReg, List.length dictIncCall + 1)
            ]
            @ dictIncCall
        | LIR.ClosureHeap ->
            let closureIncCall = [
                Symbolic.STP_pre (Symbolic.X0, Symbolic.X1, Symbolic.SP, -96);
                Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                Symbolic.STP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                Symbolic.MOV_reg (Symbolic.X0, addrReg);
                Symbolic.BL closureRefCountIncHelperLabel;
                Symbolic.LDP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                Symbolic.LDP_post (Symbolic.X0, Symbolic.X1, Symbolic.SP, 96)
            ]
            in
            [
                Symbolic.CBZ_offset (addrReg, List.length closureIncCall + 1)
            ]
            @ closureIncCall
        | LIR.GenericHeap
        | LIR.StreamHeap ->
            [
                Symbolic.CBZ_offset (addrReg, 4)
            ] @ tupleIncPath)

        )
(*
   Decrement ref count at [addr + payloadSize]
   Skip if addr is null (e.g., empty list = 0)
   When ref count hits 0, add block to free list for memory reuse
   Free list structure:
   - X27 = base of free list heads (32 slots × 8 bytes = 256 bytes)
   - Slot N contains head of free list for blocks of size (N+1)*8 bytes
   - sizeClassOffset = payloadSize (for 8-aligned payloads)
   - Freed blocks use first 8 bytes as next pointer
   Code structure (8 instructions, plus optional leak counter update):
   CBZ addr, +8                      ; If null, skip all 7 instructions
   LDR X15, [addr, payloadSize]      ; Load ref count
   SUB X15, X15, 1                   ; Decrement
   STR X15, [addr, payloadSize]      ; Store back
   CBNZ X15, +4                      ; If not zero, skip free list code (4 instrs)
   LDR X14, [X27, payloadSize]       ; Load current free list head
   STR X14, [addr, 0]                ; Store old head as next in freed block
   STR addr, [X27, payloadSize]      ; Update free list head to freed block
   (continue)
   Keep this expansion deferred. Most release kinds use a runtime
   helper, and complex generic roots are normally outlined below.
   Field-release helpers use X10-X15 as scratch registers. Keep a
   source allocated in that range in X11 while traversing, and
   reload the generic root from the stack before free-list insertion.
   Sources in X0-X9 remain directly addressable, which avoids an
   unnecessary move and preserves the established instruction shape.
*)
let emitRefCountDec (ctx: codeGenContext) (addr: LIR.reg) (payloadSize: int) (kind: LIR.rcKind) (metadata: MemoryModel.rcMetadata option) : (Symbolic.instr list, string) result =

    lirRegToARM64Reg addr
    |> Result.map (fun addrReg ->
        let releasePlan = rcMetadataReleasePlan metadata
        in
        let leakDec = generateLeakCounterDec ctx


        in
        let tupleDecPath () =
            let releaseListFieldFromHelper (baseReg: Symbolic.reg) (fieldOffset: int) (helperLabel: string) : Symbolic.instr list =
                let fieldReg =
                    if baseReg = Symbolic.X12 then Symbolic.X16 else Symbolic.X12
                in
                let callInstrs = [
                    Symbolic.STP_pre (Symbolic.X0, Symbolic.X1, Symbolic.SP, -128);
                    Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                    Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                    Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                    Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                    Symbolic.STP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                    Symbolic.STP (Symbolic.X12, Symbolic.X13, Symbolic.SP, 96);
                    Symbolic.STP (Symbolic.X14, Symbolic.X15, Symbolic.SP, 112);
                    Symbolic.MOV_reg (Symbolic.X0, fieldReg);
                    Symbolic.BL helperLabel;
                    Symbolic.LDP (Symbolic.X14, Symbolic.X15, Symbolic.SP, 112);
                    Symbolic.LDP (Symbolic.X12, Symbolic.X13, Symbolic.SP, 96);
                    Symbolic.LDP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                    Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                    Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                    Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                    Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                    Symbolic.LDP_post (Symbolic.X0, Symbolic.X1, Symbolic.SP, 128)
                ]
                in
                [
                    Symbolic.LDR (fieldReg, baseReg, int16 fieldOffset);
                    Symbolic.CBZ_offset (fieldReg, List.length callInstrs + 1)
                ] @ callInstrs

            in
            let releaseListFieldFromPlan (baseReg: Symbolic.reg) (fieldOffset: int) (fieldReleasePlan: MemoryModel.rcReleasePlan) : Symbolic.instr list =
                releaseListFieldFromHelper baseReg fieldOffset (listDecHelperForReleasePlan fieldReleasePlan)

            in
            let releaseDictFieldFromHelper (baseReg: Symbolic.reg) (fieldOffset: int) (helperLabel: string) : Symbolic.instr list =
                let fieldReg =
                    if baseReg = Symbolic.X12 then Symbolic.X16 else Symbolic.X12
                in
                let callInstrs = [
                    Symbolic.STP_pre (Symbolic.X0, Symbolic.X1, Symbolic.SP, -128);
                    Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                    Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                    Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                    Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                    Symbolic.STP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                    Symbolic.STP (Symbolic.X12, Symbolic.X13, Symbolic.SP, 96);
                    Symbolic.STP (Symbolic.X14, Symbolic.X15, Symbolic.SP, 112);
                    Symbolic.MOV_reg (Symbolic.X0, fieldReg);
                    Symbolic.BL helperLabel;
                    Symbolic.LDP (Symbolic.X14, Symbolic.X15, Symbolic.SP, 112);
                    Symbolic.LDP (Symbolic.X12, Symbolic.X13, Symbolic.SP, 96);
                    Symbolic.LDP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                    Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                    Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                    Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                    Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                    Symbolic.LDP_post (Symbolic.X0, Symbolic.X1, Symbolic.SP, 128)
                ]
                in
                [
                    Symbolic.LDR (fieldReg, baseReg, int16 fieldOffset);
                    Symbolic.CBZ_offset (fieldReg, List.length callInstrs + 1)
                ] @ callInstrs

            in
            let releaseDictFieldFromPlan (baseReg: Symbolic.reg) (fieldOffset: int) (fieldReleasePlan: MemoryModel.rcReleasePlan) : Symbolic.instr list =
                releaseDictFieldFromHelper baseReg fieldOffset (dictDecHelperForReleasePlan fieldReleasePlan)

            in
            let releaseClosureFieldFrom (baseReg: Symbolic.reg) (fieldOffset: int) : Symbolic.instr list =
                let fieldReg =
                    if baseReg = Symbolic.X12 then Symbolic.X16 else Symbolic.X12
                in
                let callInstrs = [
                    Symbolic.STP_pre (Symbolic.X0, Symbolic.X1, Symbolic.SP, -128);
                    Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                    Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                    Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                    Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                    Symbolic.STP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                    Symbolic.STP (Symbolic.X12, Symbolic.X13, Symbolic.SP, 96);
                    Symbolic.STP (Symbolic.X14, Symbolic.X15, Symbolic.SP, 112);
                    Symbolic.MOV_reg (Symbolic.X0, fieldReg);
                    Symbolic.BL closureRefCountDecHelperLabel;
                    Symbolic.LDP (Symbolic.X14, Symbolic.X15, Symbolic.SP, 112);
                    Symbolic.LDP (Symbolic.X12, Symbolic.X13, Symbolic.SP, 96);
                    Symbolic.LDP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                    Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                    Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                    Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                    Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                    Symbolic.LDP_post (Symbolic.X0, Symbolic.X1, Symbolic.SP, 128)
                ]
                in
                [
                    Symbolic.LDR (fieldReg, baseReg, int16 fieldOffset);
                    Symbolic.CBZ_offset (fieldReg, List.length callInstrs + 1)
                ] @ callInstrs

            in
            let releaseDynamicBufferFieldFrom
                (baseReg: Symbolic.reg)
                (fieldOffset: int)
                (operation: MemoryModel.rcOperation)
                : Symbolic.instr list =
                let bufferLeakDec = generateLeakCounterDec ctx
                in
                let refcountUpdate =
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
                let bcondOffset = List.length refcountUpdate + 1
                in
                let taggedGuard =
                    (match operation with
                    | MemoryModel.DynamicIntBuffer ->
                        [ Symbolic.AND_imm (Symbolic.X13, Symbolic.X12, 1L);
                          Symbolic.CBNZ_offset (Symbolic.X13, 8 + List.length refcountUpdate) ]
                    | _ -> []
                    )
                in
                let body =
                    [
                        Symbolic.LDR (Symbolic.X12, baseReg, int16 fieldOffset);
                        Symbolic.CBZ_offset (Symbolic.X12, List.length taggedGuard + 8 + List.length refcountUpdate)
                    ]
                    @ taggedGuard
                    @ [
                        Symbolic.LDR (Symbolic.X15, Symbolic.X12, 0);
                        Symbolic.MOVZ (Symbolic.X13, 0xFFFF, 0);
                        Symbolic.MOVK (Symbolic.X13, 0xFFFF, 16);
                        Symbolic.MOVK (Symbolic.X13, 0xFFFF, 32);
                        Symbolic.MOVK (Symbolic.X13, 0x7FFF, 48);
                        Symbolic.CMP_reg (Symbolic.X15, Symbolic.X13);
                        Symbolic.B_cond (Symbolic.EQ, bcondOffset)
                    ] @ refcountUpdate
                in
                [
                    Symbolic.STP_pre (Symbolic.X12, Symbolic.X13, Symbolic.SP, -32);
                    Symbolic.STP (Symbolic.X14, Symbolic.X15, Symbolic.SP, 16)
                ]
                @ body
                @ [
                    Symbolic.LDP (Symbolic.X14, Symbolic.X15, Symbolic.SP, 16);
                    Symbolic.LDP_post (Symbolic.X12, Symbolic.X13, Symbolic.SP, 32)
                ]

            in
            let rec releaseFieldPlanFrom
                (baseReg: Symbolic.reg)
                (fieldOffset: int)
                (fieldReleasePlan: MemoryModel.rcReleasePlan)
                : Symbolic.instr list =
                (match fieldReleasePlan with
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

                )
            and releaseBoxedSumVariantFieldsFrom
                (baseReg: Symbolic.reg)
                (variants: MemoryModel.rcBoxedSumVariantRelease list)
                : Symbolic.instr list =
                let releaseVariant (variant: MemoryModel.rcBoxedSumVariantRelease) : (int * Symbolic.instr list) option =
                    let releaseInstrs =
                        variant.MemoryModel.fieldReleases
                        |> List.concat_map (fun (MemoryModel.FieldRelease (fieldOffset, fieldReleasePlan)) ->
                            releaseFieldPlanFrom baseReg fieldOffset fieldReleasePlan)

                    in
                    if isEmpty releaseInstrs then
                        None
                    else
                        Some (variant.MemoryModel.tag, releaseInstrs)

                in
                let rec variantCases (cases: (int * Symbolic.instr list) list) : Symbolic.instr list =
                    (match cases with
                    | [] ->
                        []
                    | (tag, releaseInstrs) :: rest ->
                        let restInstrs = variantCases rest
                        in
                        let branchToEnd =
                            if isEmpty restInstrs then
                                []
                            else
                                [Symbolic.B (List.length restInstrs + 1)]
                        in
                        [
                            Symbolic.CMP_imm (Symbolic.X10, uint16 tag);
                            Symbolic.B_cond (Symbolic.NE, List.length releaseInstrs + List.length branchToEnd + 1)
                        ]
                        @ releaseInstrs
                        @ branchToEnd
                        @ restInstrs

                    )
                in
                let cases = variants |> List.filter_map releaseVariant

                in
                if isEmpty cases then
                    []
                else
                    [Symbolic.LDR (Symbolic.X10, baseReg, 0)]
                    @ variantCases cases

            and releaseFixedBlockFieldWithPlan
                (baseReg: Symbolic.reg)
                (fieldOffset: int)
                (childPayloadSize: int)
                (fieldReleasePlan: MemoryModel.rcReleasePlan)
                : Symbolic.instr list =
                    let childFieldReleaseInstrs =
                        (match fieldReleasePlan with
                        | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases)) ->
                            fieldReleases
                            |> List.concat_map (fun (MemoryModel.FieldRelease (childFieldOffset, fieldReleasePlan)) ->
                                releaseFieldPlanFrom Symbolic.X11 childFieldOffset fieldReleasePlan)
                        | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease (_, _, variants)) ->
                            releaseBoxedSumVariantFieldsFrom Symbolic.X11 variants
                        | _ ->
                            []
                        )
                    in
                    let childLeakDec = generateLeakCounterDec ctx
                    in
                    let freeChild =
                        (if isEmpty childFieldReleaseInstrs then
                            []
                         else
                            [ Symbolic.MOV_reg (Symbolic.X11, Symbolic.X12) ]
                            @ childFieldReleaseInstrs
                            @ [ Symbolic.MOV_reg (Symbolic.X12, Symbolic.X11) ])
                        @
                        (if childPayloadSize >= 0 && childPayloadSize < 256 then
                            [
                                Symbolic.ADD_imm (Symbolic.X13, Symbolic.X27, uint16 childPayloadSize);
                                Symbolic.LDR (Symbolic.X14, Symbolic.X13, 0);
                                Symbolic.STR (Symbolic.X14, Symbolic.X12, 0);
                                Symbolic.STR (Symbolic.X12, Symbolic.X13, 0)
                            ]
                         else
                            [])
                        @ childLeakDec

                    in
                    let afterDec =
                        if isEmpty freeChild then
                            []
                        else
                            [Symbolic.CBNZ_offset (Symbolic.X15, List.length freeChild + 1)]
                            @ freeChild
                    in
                    let body =
                        [
                            Symbolic.LDR (Symbolic.X12, baseReg, int16 fieldOffset);
                            Symbolic.CBZ_offset (Symbolic.X12, 3 + List.length afterDec + 1);
                            Symbolic.LDR (Symbolic.X15, Symbolic.X12, int16 childPayloadSize);
                            Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
                            Symbolic.STR (Symbolic.X15, Symbolic.X12, int16 childPayloadSize)
                        ] @ afterDec

                    in
                    [
                        Symbolic.STP_pre (Symbolic.X10, Symbolic.X11, Symbolic.SP, -48);
                        Symbolic.STP (Symbolic.X12, Symbolic.X13, Symbolic.SP, 16);
                        Symbolic.STP (Symbolic.X14, Symbolic.X15, Symbolic.SP, 32)
                    ]
                    @ body
                    @ [
                        Symbolic.LDP (Symbolic.X14, Symbolic.X15, Symbolic.SP, 32);
                        Symbolic.LDP (Symbolic.X12, Symbolic.X13, Symbolic.SP, 16);
                        Symbolic.LDP_post (Symbolic.X10, Symbolic.X11, Symbolic.SP, 48)
                    ]

            in
            let fixedBlockFieldReleaseInstrs =
                releasePlan
                |> Option.map (function
                    | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases)) ->
                        fieldReleases
                        |> List.concat_map (fun (MemoryModel.FieldRelease (fieldOffset, fieldReleasePlan)) ->
                            releaseFieldPlanFrom Symbolic.X11 fieldOffset fieldReleasePlan)
                    | _ ->
                        [])
                |> optionDefault []

            in
            let releaseSumPayloadInstrs =
                (match releasePlan with
                | Some (MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease (_, _, variants))) ->
                    if List.length variants = 1
                       && not (callerOwnsSinglePayloadSum ctx.functionName) then
                        []
                    else
                        releaseBoxedSumVariantFieldsFrom Symbolic.X11 variants
                | _ ->
                    []

                )
            in
            let fieldReleaseInstrs =
                fixedBlockFieldReleaseInstrs
                @ releaseSumPayloadInstrs

            in
            let stableFieldReleaseInstrs =
                if isEmpty fieldReleaseInstrs then
                    []
                else
                    let stableBaseReg =
                        if contains
                               addrReg
                               [ Symbolic.X10; Symbolic.X11; Symbolic.X12;
                                 Symbolic.X13; Symbolic.X14; Symbolic.X15 ] then
                            Symbolic.X11
                        else
                            addrReg
                    in
                    let releases =
                        releasePlan
                        |> Option.map (function
                            | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases)) ->
                                fieldReleases
                                |> List.concat_map (fun (MemoryModel.FieldRelease (fieldOffset, fieldReleasePlan)) ->
                                    releaseFieldPlanFrom stableBaseReg fieldOffset fieldReleasePlan)
                            | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease (_, _, variants)) ->
                                releaseBoxedSumVariantFieldsFrom stableBaseReg variants
                            | _ ->
                                [])
                        |> optionDefault []
                    in
                    [
                        Symbolic.STP_pre (addrReg, Symbolic.X11, Symbolic.SP, -16)
                    ]
                    @ (if stableBaseReg = Symbolic.X11 && addrReg <> Symbolic.X11 then
                           [Symbolic.MOV_reg (Symbolic.X11, addrReg)]
                       else
                           [])
                    @ releases
                    @ [
                        Symbolic.LDR (addrReg, Symbolic.SP, 0);
                        Symbolic.LDR (Symbolic.X11, Symbolic.SP, 8);
                        Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 16)
                    ]

            in
            let releaseInstrs =
                stableFieldReleaseInstrs
                @
                (if payloadSize >= 0 && payloadSize < 256 then
                    [
                        Symbolic.LDR (Symbolic.X14, Symbolic.X27, int16 payloadSize);
                        Symbolic.STR (Symbolic.X14, addrReg, 0);
                        Symbolic.STR (addrReg, Symbolic.X27, int16 payloadSize)
                    ]
                 else
                    [])
                @ leakDec
            in
            [
                Symbolic.LDR (Symbolic.X15, addrReg, int16 payloadSize);
                Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
                Symbolic.STR (Symbolic.X15, addrReg, int16 payloadSize)
            ]
            @ (if isEmpty releaseInstrs then
                []
               else
                [Symbolic.CBNZ_offset (Symbolic.X15, List.length releaseInstrs + 1)]
                @ releaseInstrs)

        in
        (match kind with
        | LIR.TaggedList ->
            let releasePlan =
                requiredRcMetadataReleasePlan "TaggedList RefCountDec" metadata
            in
            let helperLabel =
                (match releasePlan with
                | MemoryModel.RootRelease (_, _, MemoryModel.TaggedListPayloadRelease (MemoryModel.RootRelease (_, MemoryModel.GenericHeap, _) as elementRelease)) ->
                    plannedListDecHelperLabelForReleasePlan elementRelease
                | _ ->
                    listDecHelperForReleasePlan releasePlan
                )
            in
            let listDecCall = [
                Symbolic.STP_pre (Symbolic.X0, Symbolic.X1, Symbolic.SP, -80);
                Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                Symbolic.MOV_reg (Symbolic.X0, addrReg);
                Symbolic.BL helperLabel;
                Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                Symbolic.LDP_post (Symbolic.X0, Symbolic.X1, Symbolic.SP, 80)
            ]
            in
            let listCallLen = List.length listDecCall
            in
            [
                Symbolic.CBZ_offset (addrReg, listCallLen + 1)
            ]
            @ listDecCall
        | LIR.DictHeap ->
            let helperLabel =
                metadata
                |> requiredRcMetadataReleasePlan "DictHeap RefCountDec"
                |> dictDecHelperForReleasePlan
            in
            let dictDecCall = [
                Symbolic.STP_pre (Symbolic.X0, Symbolic.X1, Symbolic.SP, -96);
                Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                Symbolic.STP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                Symbolic.MOV_reg (Symbolic.X0, addrReg);
                Symbolic.BL helperLabel;
                Symbolic.LDP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                Symbolic.LDP_post (Symbolic.X0, Symbolic.X1, Symbolic.SP, 96)
            ]
            in
            [
                Symbolic.CBZ_offset (addrReg, List.length dictDecCall + 1)
            ]
            @ dictDecCall
        | LIR.ClosureHeap ->
            let closureDecCall = [
                Symbolic.STP_pre (Symbolic.X0, Symbolic.X1, Symbolic.SP, -96);
                Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                Symbolic.STP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                Symbolic.MOV_reg (Symbolic.X0, addrReg);
                Symbolic.BL closureRefCountDecHelperLabel;
                Symbolic.LDP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                Symbolic.LDP_post (Symbolic.X0, Symbolic.X1, Symbolic.SP, 96)
            ]
            in
            [
                Symbolic.CBZ_offset (addrReg, List.length closureDecCall + 1)
            ]
            @ closureDecCall
        | LIR.StreamHeap ->
            let streamDecCall = [
                Symbolic.STP_pre (Symbolic.X0, Symbolic.X1, Symbolic.SP, -96);
                Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                Symbolic.STP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                Symbolic.MOV_reg (Symbolic.X0, addrReg);
                Symbolic.BL streamRefCountDecHelperLabel;
                Symbolic.LDP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
                Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
                Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
                Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
                Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
                Symbolic.LDP_post (Symbolic.X0, Symbolic.X1, Symbolic.SP, 96)
            ]
            in
            [Symbolic.CBZ_offset (addrReg, List.length streamDecCall + 1)] @ streamDecCall
        | LIR.GenericHeap ->
            let inlineDecPath = tupleDecPath ()
            in
            let cbzOffset = List.length inlineDecPath + 1
            in
            [Symbolic.CBZ_offset (addrReg, cbzOffset)] @ inlineDecPath)

        )
(*
   Increment the leading refcount for a dynamic buffer.
   Literal strings have refcount = INT64_MAX as sentinel (don't modify read-only memory)
   Literal string - no refcount, no-op
   Heap and materialized literal buffers keep the refcount at [addr].
   Compare with sentinel
   If literal string, skip to end
   X15++
   store back
*)
let emitRefCountIncBuffer (_ctx: codeGenContext) (skipTagged: bool) (str: LIR.operand) : (Symbolic.instr list, string) result =


    (match str with
    | LIR.Imm 0L -> Ok []
    | LIR.StringSymbol _ ->

        Ok []
    | LIR.Reg reg ->

        lirRegToARM64Reg reg
        |> Result.map (fun addrReg ->
            let refAddrReg, preserveAddr =
                if addrReg = Symbolic.X13 || addrReg = Symbolic.X15 then
                    Symbolic.X14, [Symbolic.MOV_reg (Symbolic.X14, addrReg)]
                else
                    addrReg, []
            in
            let refcountPath = ([
                Symbolic.LDR (Symbolic.X15, refAddrReg, 0)
            ] @ loadImmediate Symbolic.X13 Int64.max_int @ [
                Symbolic.CMP_reg (Symbolic.X15, Symbolic.X13);
                Symbolic.B_cond (Symbolic.EQ, 3);
                Symbolic.ADD_imm (Symbolic.X15, Symbolic.X15, 1);
                Symbolic.STR (Symbolic.X15, refAddrReg, 0)
            ])
            in
            let guards =
                if skipTagged then
                    [ Symbolic.CBZ_offset (refAddrReg, (List.length refcountPath) + 3);
                      Symbolic.AND_imm (Symbolic.X13, refAddrReg, 1L);
                      Symbolic.CBNZ_offset (Symbolic.X13, (List.length refcountPath) + 1) ]
                else
                    []
            in
            preserveAddr
            @ guards
            @ refcountPath)
    | _ -> Error "dynamic buffer RefCountInc requires StringSymbol or Reg operand"

    )
(*
   Decrement the leading refcount for a dynamic buffer.
   Literal strings have refcount = INT64_MAX as sentinel (don't modify read-only memory)
   Literal string - no refcount, no-op
   Heap and materialized literal buffers keep the refcount at [addr].
   X15--
   store back
   If refcount hits 0, update leak counter (string freeing not implemented yet)
   If not zero, skip leak counter
   Compare with sentinel
   If literal string, skip to end
*)
let emitRefCountDecBuffer (ctx: codeGenContext) (skipTagged: bool) (str: LIR.operand) : (Symbolic.instr list, string) result =


    (match str with
    | LIR.Imm 0L -> Ok []
    | LIR.StringSymbol _ ->

        Ok []
    | LIR.Reg reg ->

        lirRegToARM64Reg reg
        |> Result.map (fun addrReg ->
            let leakDec = generateLeakCounterDec ctx
            in
            let refAddrReg, preserveAddr =
                if addrReg = Symbolic.X13 || addrReg = Symbolic.X15 then
                    Symbolic.X14, [Symbolic.MOV_reg (Symbolic.X14, addrReg)]
                else
                    addrReg, []
            in
            let bcondOffset = if isEmpty leakDec then 3 else 9
            in
            let refcountUpdate =
                if isEmpty leakDec then
                    [
                        Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
                        Symbolic.STR (Symbolic.X15, refAddrReg, 0)
                    ]
                else
                    [
                        Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
                        Symbolic.STR (Symbolic.X15, refAddrReg, 0);

                        Symbolic.CBNZ_offset (Symbolic.X15, 6)
                    ] @ leakDec
            in
            let refcountPath = ([
                Symbolic.LDR (Symbolic.X15, refAddrReg, 0)
            ] @ loadImmediate Symbolic.X13 Int64.max_int @ [
                Symbolic.CMP_reg (Symbolic.X15, Symbolic.X13);
                Symbolic.B_cond (Symbolic.EQ, bcondOffset)
            ] @ refcountUpdate)
            in
            let guards =
                if skipTagged then
                    [ Symbolic.CBZ_offset (refAddrReg, (List.length refcountPath) + 3);
                      Symbolic.AND_imm (Symbolic.X13, refAddrReg, 1L);
                      Symbolic.CBNZ_offset (Symbolic.X13, (List.length refcountPath) + 1) ]
                else
                    []
            in
            preserveAddr
            @ guards
            @ refcountPath)
    | _ -> Error "dynamic buffer RefCountDec requires StringSymbol or Reg operand"

    )
let emitRefCountIncString (ctx: codeGenContext) (str: LIR.operand) =
    emitRefCountIncBuffer ctx false str

let emitRefCountDecString (ctx: codeGenContext) (str: LIR.operand) =
    emitRefCountDecBuffer ctx false str

let emitRefCountIncInt (ctx: codeGenContext) (value: LIR.operand) =
    emitRefCountIncBuffer ctx true value

let emitRefCountDecInt (ctx: codeGenContext) (value: LIR.operand) =
    emitRefCountDecBuffer ctx true value
