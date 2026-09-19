// Buffers.fs - Emit arm64 instructions for buffers operations.

module ARM64EmitBuffers

open ARM64CodeGenTypes
open ARM64HeapAllocation
open ARM64LeakAccounting
open ARM64Operands

let internal emitCanonicalBufferEq (ctx: CodeGenContext) (dest: LIR.Reg) (left: LIR.Operand) (right: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
    // Canonical buffers share the [refcount:8][length:8][data:N] layout. Compare the
    // representation directly without allocating or calling stdlib code.
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        let materialize operand target =
            match operand with
            | LIR.Reg reg ->
                lirRegToARM64Reg reg
                |> Result.map (fun source ->
                    if source = target then [] else [ARM64Symbolic.MOV_reg (target, source)])
            | LIR.StringSymbol value ->
                let labelRef = stringDataLabel value
                Ok [ARM64Symbolic.ADRP (target, labelRef)
                    ARM64Symbolic.ADD_label (target, target, labelRef)]
            | _ -> Error "CanonicalBufferEq requires StringSymbol or Reg operands"

        let label suffix =
            $"__canonical_buffer_eq_{ctx.FunctionName}_{ctx.InstructionSite}_{suffix}"
        let wordLoop = label "words"
        let byteLoop = label "bytes"
        let equalLabel = label "equal"
        let unequalLabel = label "unequal"
        let doneLabel = label "done"

        materialize left ARM64Symbolic.X8
        |> Result.bind (fun leftInstrs ->
            materialize right ARM64Symbolic.X9
            |> Result.map (fun rightInstrs ->
                leftInstrs
                @ rightInstrs
                @ [ARM64Symbolic.CMP_reg (ARM64Symbolic.X8, ARM64Symbolic.X9)
                   ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, equalLabel)
                   ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X8, 8s)
                   ARM64Symbolic.LDR (ARM64Symbolic.X12, ARM64Symbolic.X9, 8s)
                   ARM64Symbolic.CMP_reg (ARM64Symbolic.X10, ARM64Symbolic.X12)
                   ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, unequalLabel)
                   ARM64Symbolic.ADD_imm (ARM64Symbolic.X8, ARM64Symbolic.X8, 16us)
                   ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 16us)
                   ARM64Symbolic.Label wordLoop
                   ARM64Symbolic.CMP_imm (ARM64Symbolic.X10, 8us)
                   ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, byteLoop)
                   ARM64Symbolic.LDR (ARM64Symbolic.X11, ARM64Symbolic.X8, 0s)
                   ARM64Symbolic.LDR (ARM64Symbolic.X13, ARM64Symbolic.X9, 0s)
                   ARM64Symbolic.CMP_reg (ARM64Symbolic.X11, ARM64Symbolic.X13)
                   ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, unequalLabel)
                   ARM64Symbolic.ADD_imm (ARM64Symbolic.X8, ARM64Symbolic.X8, 8us)
                   ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 8us)
                   ARM64Symbolic.SUB_imm (ARM64Symbolic.X10, ARM64Symbolic.X10, 8us)
                   ARM64Symbolic.B_label wordLoop
                   ARM64Symbolic.Label byteLoop
                   ARM64Symbolic.CMP_imm (ARM64Symbolic.X10, 0us)
                   ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, equalLabel)
                   ARM64Symbolic.LDRB_imm (ARM64Symbolic.X11, ARM64Symbolic.X8, 0)
                   ARM64Symbolic.LDRB_imm (ARM64Symbolic.X13, ARM64Symbolic.X9, 0)
                   ARM64Symbolic.CMP_reg (ARM64Symbolic.X11, ARM64Symbolic.X13)
                   ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, unequalLabel)
                   ARM64Symbolic.ADD_imm (ARM64Symbolic.X8, ARM64Symbolic.X8, 1us)
                   ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 1us)
                   ARM64Symbolic.SUB_imm (ARM64Symbolic.X10, ARM64Symbolic.X10, 1us)
                   ARM64Symbolic.B_label byteLoop
                   ARM64Symbolic.Label equalLabel
                   ARM64Symbolic.MOVZ (ARM64Symbolic.X11, 1us, 0)
                   ARM64Symbolic.B_label doneLabel
                   ARM64Symbolic.Label unequalLabel
                   ARM64Symbolic.MOVZ (ARM64Symbolic.X11, 0us, 0)
                   ARM64Symbolic.Label doneLabel]
                @ (if destReg = ARM64Symbolic.X11 then [] else [ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X11)]))))

let private emitStringConcatBinary (ctx: CodeGenContext) (dest: LIR.Reg) (left: LIR.Operand) (right: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
    // String concatenation:
    // Dynamic and literal strings share [refcount:8][length:8][data:N].
    //
    // Register usage:
    // X9  = left data address (for literal: string address, for heap: addr+8)
    // X10 = left length
    // X11 = right data address
    // X12 = right length
    // X13 = total length
    // X14 = result pointer
    // X15 = temp for byte copy
    //
    // Algorithm:
    // 1. Load left address and length into X9, X10
    // 2. Load right address and length into X11, X12
    // 3. Calculate total length: X13 = X10 + X12
    // 4. Allocate: total + 16 bytes using bump allocator
    // 5. Store refcount and total length in the fixed header
    // 6. Copy left bytes to [X14+16]
    // 7. Copy right bytes after the left bytes
    // 9. Move result to dest

    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        // Helper: load operand address and length into registers
        let loadOperandInfo (operand: LIR.Operand) (addrReg: ARM64Symbolic.Reg) (lenReg: ARM64Symbolic.Reg) : Result<ARM64Symbolic.Instr list, string> =
            match operand with
            | LIR.StringSymbol value ->
                // Literal string: address via ADRP+ADD, length from UTF-8 bytes
                // Skip the literal's fixed header to get its data address.
                let len = utf8Len value
                let labelRef = stringDataLabel value
                Ok ([
                    ARM64Symbolic.ADRP (addrReg, labelRef)
                    ARM64Symbolic.ADD_label (addrReg, addrReg, labelRef)
                    ARM64Symbolic.ADD_imm (addrReg, addrReg, 16us)
                ] @ loadImmediate lenReg (int64 len))
            | LIR.Reg reg ->
                // Dynamic string: length at [reg+8], data at [reg+16].
                lirRegToARM64Reg reg
                |> Result.map (fun srcReg ->
                    [
                        ARM64Symbolic.LDR (lenReg, srcReg, 8s)
                        ARM64Symbolic.ADD_imm (addrReg, srcReg, 16us)
                    ])
            | other -> Error $"StringConcat requires StringSymbol or Reg operand, got: {other}"

        // Load both operands
        loadOperandInfo left ARM64Symbolic.X9 ARM64Symbolic.X10
        |> Result.bind (fun leftInstrs ->
            loadOperandInfo right ARM64Symbolic.X11 ARM64Symbolic.X12
            |> Result.map (fun rightInstrs ->
                // Calculate total length
                let calcTotal = [ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X10, ARM64Symbolic.X12)]

                // Allocate: totalLen + 16 bytes (8 for length, 8 for refcount)
                // Using bump allocator (X28 = bump pointer)
                let allocate = [
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X14, ARM64Symbolic.X13, 16us)   // X14 = total + 16
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X14, ARM64Symbolic.X14, 7us)    // Align up
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 0xFFF8us, 0)          // ~7 mask (lower bits)
                    ARM64Symbolic.MOVK (ARM64Symbolic.X15, 0xFFFFus, 16)         // Bits 16-31
                    ARM64Symbolic.MOVK (ARM64Symbolic.X15, 0xFFFFus, 32)         // Bits 32-47
                    ARM64Symbolic.MOVK (ARM64Symbolic.X15, 0xFFFFus, 48)         // Bits 48-63
                    ARM64Symbolic.AND_reg (ARM64Symbolic.X14, ARM64Symbolic.X14, ARM64Symbolic.X15)  // X14 = aligned size
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X14, ARM64Symbolic.X28)            // X14 = current heap ptr (result)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X13, 16us)      // X15 = total + 16
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 7us)       // Align
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0xFFF8us, 0)              // ~7 mask again (X15 was clobbered)
                    ARM64Symbolic.MOVK (ARM64Symbolic.X0, 0xFFFFus, 16)             // Bits 16-31
                    ARM64Symbolic.MOVK (ARM64Symbolic.X0, 0xFFFFus, 32)             // Bits 32-47
                    ARM64Symbolic.MOVK (ARM64Symbolic.X0, 0xFFFFus, 48)             // Bits 48-63
                    ARM64Symbolic.AND_reg (ARM64Symbolic.X15, ARM64Symbolic.X15, ARM64Symbolic.X0)
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X28, ARM64Symbolic.X28, ARM64Symbolic.X15) // Bump heap pointer
                ]

                let storeHeader = [
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 1us, 0)
                    ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X14, 0s)
                    ARM64Symbolic.STR (ARM64Symbolic.X13, ARM64Symbolic.X14, 8s)
                ]

                // Copy left bytes after the fixed header.
                // IMPORTANT: Don't use X0-X7 as temps - they may hold function arguments!
                // Strategy: Use pointer-bumping loops instead of indexed addressing
                // X15 = source pointer (starts at X9, bumped each iteration)
                // X16 = dest pointer (starts at X14+8, bumped each iteration)
                // X13 = remaining count (starts at X10, decremented, reused since we stored total already)
                let copyLeft = [
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X15, ARM64Symbolic.X9)              // 0: X15 = src ptr
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X16, ARM64Symbolic.X14, 16us)
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X13, ARM64Symbolic.X10)             // 2: X13 = remaining = len1
                    // Loop: if X13 == 0, done (skip 7 instructions to exit past B at index 9)
                    ARM64Symbolic.CBZ_offset (ARM64Symbolic.X13, 7)                  // 3: Skip 7 instructions if done -> index 10 (past end)
                    ARM64Symbolic.LDRB_imm (ARM64Symbolic.X8, ARM64Symbolic.X15, 0)          // 4: X8 = byte at [X15]
                    ARM64Symbolic.STRB_reg (ARM64Symbolic.X8, ARM64Symbolic.X16)             // 5: [X16] = byte
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)        // 6: X15++ (src ptr)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X16, ARM64Symbolic.X16, 1us)        // 7: X16++ (dest ptr)
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.X13, ARM64Symbolic.X13, 1us)        // 8: X13-- (remaining)
                    ARM64Symbolic.B (-6)                                     // 9: Loop back to CBZ (index 3)
                ]

                // Copy right bytes: loop copying X12 bytes from X11 to [X14+8+X10]
                // X15 = source pointer (starts at X11)
                // X16 = dest pointer (starts at X14+8+X10, already in X16 from copyLeft end)
                // X13 = remaining count (use X12)
                // Note: X16 is already at X14+8+len1 after copyLeft loop ends!
                let copyRight = [
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X15, ARM64Symbolic.X11)             // 0: X15 = src ptr (right string)
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X13, ARM64Symbolic.X12)             // 1: X13 = remaining = len2
                    // Loop: if X13 == 0, done (skip 7 instructions to exit past B at index 8)
                    ARM64Symbolic.CBZ_offset (ARM64Symbolic.X13, 7)                  // 2: Skip 7 instructions if done -> index 9 (past end)
                    ARM64Symbolic.LDRB_imm (ARM64Symbolic.X8, ARM64Symbolic.X15, 0)          // 3: X8 = byte at [X15]
                    ARM64Symbolic.STRB_reg (ARM64Symbolic.X8, ARM64Symbolic.X16)             // 4: [X16] = byte
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)        // 5: X15++ (src ptr)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X16, ARM64Symbolic.X16, 1us)        // 6: X16++ (dest ptr)
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.X13, ARM64Symbolic.X13, 1us)        // 7: X13-- (remaining)
                    ARM64Symbolic.B (-6)                                     // 8: Loop back to CBZ (index 2)
                ]

                // Move result to dest
                let moveResult = [ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X14)]

                leftInstrs @ rightInstrs @ calcTotal @ allocate @ storeHeader @ copyLeft @ copyRight @ moveResult @ generateLeakCounterInc ctx
            )))

/// Lower a concat tree as one length pass, one allocation, and one ordered copy pass.
let private emitStringConcatMany
    (ctx: CodeGenContext)
    (dest: LIR.Reg)
    (first: LIR.Operand)
    (second: LIR.Operand)
    (remaining: LIR.Operand list)
    : Result<ARM64Symbolic.Instr list, string> =
    let operands = first :: second :: remaining

    let loadOperandInfo (operand: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
        match operand with
        | LIR.StringSymbol value ->
            let labelRef = stringDataLabel value
            Ok ([ ARM64Symbolic.ADRP (ARM64Symbolic.X9, labelRef)
                  ARM64Symbolic.ADD_label (ARM64Symbolic.X9, ARM64Symbolic.X9, labelRef)
                  ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 16us) ]
                @ loadImmediate ARM64Symbolic.X10 (int64 (utf8Len value)))
        | LIR.Reg reg ->
            lirRegToARM64Reg reg
            |> Result.map (fun source ->
                [ ARM64Symbolic.LDR (ARM64Symbolic.X10, source, 8s)
                  ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, source, 16us) ])
        | LIR.StackSlot offset ->
            loadStackSlot ARM64Symbolic.X9 offset
            |> Result.map (fun loads ->
                loads
                @ [ ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X9, 8s)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 16us) ])
        | other -> Error $"StringConcat requires string operands, got: {other}"

    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        operands
        |> ResultList.mapResults loadOperandInfo
        |> Result.map (fun operandLoads ->
            let measure =
                [ ARM64Symbolic.MOVZ (ARM64Symbolic.X13, 0us, 0) ]
                @ (operandLoads
                   |> List.collect (fun loads ->
                       loads @ [ ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X10) ]))

            let allocate =
                [ ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X13, 23us)
                  ARM64Symbolic.MOVZ (ARM64Symbolic.X17, 0xFFF8us, 0)
                  ARM64Symbolic.MOVK (ARM64Symbolic.X17, 0xFFFFus, 16)
                  ARM64Symbolic.MOVK (ARM64Symbolic.X17, 0xFFFFus, 32)
                  ARM64Symbolic.MOVK (ARM64Symbolic.X17, 0xFFFFus, 48)
                  ARM64Symbolic.AND_reg (ARM64Symbolic.X15, ARM64Symbolic.X15, ARM64Symbolic.X17)
                  ARM64Symbolic.MOV_reg (ARM64Symbolic.X14, ARM64Symbolic.X28)
                  ARM64Symbolic.ADD_reg (ARM64Symbolic.X28, ARM64Symbolic.X28, ARM64Symbolic.X15)
                  ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 1us, 0)
                  ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X14, 0s)
                  ARM64Symbolic.STR (ARM64Symbolic.X13, ARM64Symbolic.X14, 8s)
                  ARM64Symbolic.ADD_imm (ARM64Symbolic.X16, ARM64Symbolic.X14, 16us) ]

            let copyOne loads =
                loads
                @ [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X15, ARM64Symbolic.X9)
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X13, ARM64Symbolic.X10)
                    ARM64Symbolic.CBZ_offset (ARM64Symbolic.X13, 7)
                    ARM64Symbolic.LDRB_imm (ARM64Symbolic.X8, ARM64Symbolic.X15, 0)
                    ARM64Symbolic.STRB_reg (ARM64Symbolic.X8, ARM64Symbolic.X16)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X16, ARM64Symbolic.X16, 1us)
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.X13, ARM64Symbolic.X13, 1us)
                    ARM64Symbolic.B (-6) ]

            measure
            @ allocate
            @ (operandLoads |> List.collect copyOne)
            @ [ ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X14) ]
            @ generateLeakCounterInc ctx))

let internal emitStringConcat ctx dest first second remaining =
    match remaining with
    | [] -> emitStringConcatBinary ctx dest first second
    | _ -> emitStringConcatMany ctx dest first second remaining
