// 6_CodeGen.fs - Code Generation (Pass 6)
//
// Transforms LIR into ARM64 instructions.
//
// Code generation algorithm:
// - Maps LIR physical registers to ARM64 registers
// - Selects ARM64 instruction forms (immediate vs register operands)
// - Generates MOVZ instructions for loading immediate values
// - Handles 12-bit immediate constraints for ADD/SUB
//
// Assumes register allocation completed (no virtual registers remain)
//
// Example:
//   X0 <- Mov(Imm 42); X1 <- Add(X0, Imm 5)
//   →
//   MOVZ X0, #42, LSL #0; ADD X1, X0, #5

module CodeGen

/// Code generation options
type CodeGenOptions = {
    /// Disable free list memory reuse (always bump allocate)
    DisableFreeList: bool
}

/// Default code generation options
let defaultOptions : CodeGenOptions = {
    DisableFreeList = false
}

/// Code generation context (passed through to instruction conversion)
type CodeGenContext = {
    Options: CodeGenOptions
    StringPool: MIR.StringPool
}

/// Convert LIR.PhysReg to ARM64.Reg
let lirPhysRegToARM64Reg (physReg: LIR.PhysReg) : ARM64.Reg =
    match physReg with
    | LIR.X0 -> ARM64.X0
    | LIR.X1 -> ARM64.X1
    | LIR.X2 -> ARM64.X2
    | LIR.X3 -> ARM64.X3
    | LIR.X4 -> ARM64.X4
    | LIR.X5 -> ARM64.X5
    | LIR.X6 -> ARM64.X6
    | LIR.X7 -> ARM64.X7
    | LIR.X8 -> ARM64.X8
    | LIR.X9 -> ARM64.X9
    | LIR.X10 -> ARM64.X10
    | LIR.X11 -> ARM64.X11
    | LIR.X12 -> ARM64.X12
    | LIR.X13 -> ARM64.X13
    | LIR.X14 -> ARM64.X14
    | LIR.X15 -> ARM64.X15
    | LIR.X19 -> ARM64.X19
    | LIR.X20 -> ARM64.X20
    | LIR.X21 -> ARM64.X21
    | LIR.X22 -> ARM64.X22
    | LIR.X23 -> ARM64.X23
    | LIR.X24 -> ARM64.X24
    | LIR.X25 -> ARM64.X25
    | LIR.X26 -> ARM64.X26
    | LIR.X27 -> ARM64.X27
    | LIR.X29 -> ARM64.X29
    | LIR.X30 -> ARM64.X30
    | LIR.SP -> ARM64.SP

/// Convert LIR.PhysFPReg to ARM64.FReg
let lirPhysFPRegToARM64FReg (physReg: LIR.PhysFPReg) : ARM64.FReg =
    match physReg with
    | LIR.D0 -> ARM64.D0
    | LIR.D1 -> ARM64.D1
    | LIR.D2 -> ARM64.D2
    | LIR.D3 -> ARM64.D3
    | LIR.D4 -> ARM64.D4
    | LIR.D5 -> ARM64.D5
    | LIR.D6 -> ARM64.D6
    | LIR.D7 -> ARM64.D7
    | LIR.D8 -> ARM64.D8
    | LIR.D9 -> ARM64.D9
    | LIR.D10 -> ARM64.D10
    | LIR.D11 -> ARM64.D11
    | LIR.D12 -> ARM64.D12
    | LIR.D13 -> ARM64.D13
    | LIR.D14 -> ARM64.D14
    | LIR.D15 -> ARM64.D15

/// Convert LIR.FReg to ARM64.FReg
/// For FVirtual, we use a simple allocation scheme:
/// - FVirtual 1000 -> D1 (left operand temp)
/// - FVirtual 1001 -> D2 (right operand temp)
/// - FVirtual n (n < 100) -> D(n % 8) for computed results
let lirFRegToARM64FReg (freg: LIR.FReg) : Result<ARM64.FReg, string> =
    match freg with
    | LIR.FPhysical physReg -> Ok (lirPhysFPRegToARM64FReg physReg)
    | LIR.FVirtual 1000 -> Ok ARM64.D1  // Left temp
    | LIR.FVirtual 1001 -> Ok ARM64.D2  // Right temp
    | LIR.FVirtual n ->
        // Map virtual FP registers to physical D0-D7
        let regIdx = n % 8
        let physReg =
            match regIdx with
            | 0 -> ARM64.D0
            | 1 -> ARM64.D1
            | 2 -> ARM64.D2
            | 3 -> ARM64.D3
            | 4 -> ARM64.D4
            | 5 -> ARM64.D5
            | 6 -> ARM64.D6
            | _ -> ARM64.D7
        Ok physReg

/// Convert LIR.Reg to ARM64.Reg (assumes physical registers only)
let lirRegToARM64Reg (reg: LIR.Reg) : Result<ARM64.Reg, string> =
    match reg with
    | LIR.Physical physReg -> Ok (lirPhysRegToARM64Reg physReg)
    | LIR.Virtual vreg -> Error $"Virtual register {vreg} should have been allocated"

/// Generate ARM64 instructions to load an immediate into a register
let loadImmediate (dest: ARM64.Reg) (value: int64) : ARM64.Instr list =
    // Load 64-bit immediate using MOVZ + MOVK sequence
    // Extract each 16-bit chunk
    let chunk0 = uint16 (value >>> 0) &&& 0xFFFFus
    let chunk1 = uint16 (value >>> 16) &&& 0xFFFFus
    let chunk2 = uint16 (value >>> 32) &&& 0xFFFFus
    let chunk3 = uint16 (value >>> 48) &&& 0xFFFFus

    // Build instruction sequence functionally
    // Start with MOVZ for the first chunk (or chunk0 if all zero)
    // Add MOVK for remaining non-zero chunks
    [ARM64.MOVZ (dest, chunk0, 0)]
    @ (if chunk1 <> 0us then [ARM64.MOVK (dest, chunk1, 16)] else [])
    @ (if chunk2 <> 0us then [ARM64.MOVK (dest, chunk2, 32)] else [])
    @ (if chunk3 <> 0us then [ARM64.MOVK (dest, chunk3, 48)] else [])

/// Generate ARM64 instructions to load a stack slot into a register
/// Stack slots are accessed relative to FP (X29)
/// Uses LDUR for signed offsets (stack slots are negative from FP)
/// Returns Error if offset exceeds LDUR range (-256 to +255)
let loadStackSlot (dest: ARM64.Reg) (offset: int) : Result<ARM64.Instr list, string> =
    // LDUR dest, [X29, #offset] - signed offset version
    // Offset is in bytes, can be negative (-256 to +255)
    if offset < -256 || offset > 255 then
        Error $"Stack offset {offset} exceeds LDUR range (-256 to +255)"
    else
        Ok [ARM64.LDUR (dest, ARM64.X29, int16 offset)]

/// Generate ARM64 instructions to store a register to a stack slot
/// Stack slots are accessed relative to FP (X29)
/// Uses STUR for signed offsets (stack slots are negative from FP)
/// Returns Error if offset exceeds STUR range (-256 to +255)
let storeStackSlot (src: ARM64.Reg) (offset: int) : Result<ARM64.Instr list, string> =
    // STUR src, [X29, #offset] - signed offset version
    // Offset is in bytes, can be negative (-256 to +255)
    if offset < -256 || offset > 255 then
        Error $"Stack offset {offset} exceeds STUR range (-256 to +255)"
    else
        Ok [ARM64.STUR (src, ARM64.X29, int16 offset)]

/// Convert LIR operand to ARM64 register, loading from stack if needed
/// Returns (register, instruction list to load it)
/// Uses X9 as temporary register for stack slots
let operandToReg (operand: LIR.Operand) : Result<ARM64.Reg * ARM64.Instr list, string> =
    match operand with
    | LIR.Reg reg ->
        lirRegToARM64Reg reg
        |> Result.map (fun r -> (r, []))
    | LIR.Imm value ->
        // Load immediate into X9
        Ok (ARM64.X9, loadImmediate ARM64.X9 value)
    | LIR.FloatImm _ ->
        // Float support not yet implemented - will be added in later milestone
        Error "Float code generation not yet implemented"
    | LIR.StackSlot offset ->
        // Load stack slot into X9
        loadStackSlot ARM64.X9 offset
        |> Result.map (fun instrs -> (ARM64.X9, instrs))
    | LIR.StringRef _ ->
        // String address loading handled by PrintString instruction
        Error "StringRef cannot be directly used as register operand"
    | LIR.FloatRef _ ->
        // Float address loading handled by FLoad instruction
        Error "FloatRef cannot be directly used as register operand"
    | LIR.FuncAddr funcName ->
        // Load function address into X9 using ADR instruction
        Ok (ARM64.X9, [ARM64.ADR (ARM64.X9, funcName)])

/// Generate STP instructions to save callee-saved register pairs
/// Returns instructions and total bytes pushed
let generateCalleeSavedSaves (regs: LIR.PhysReg list) : ARM64.Instr list * int =
    // Sort registers for consistent ordering and pair them
    let sorted = regs |> List.sortBy (fun r ->
        match r with
        | LIR.X19 -> 19 | LIR.X20 -> 20 | LIR.X21 -> 21 | LIR.X22 -> 22
        | LIR.X23 -> 23 | LIR.X24 -> 24 | LIR.X25 -> 25 | LIR.X26 -> 26
        | LIR.X27 -> 27 | _ -> 99)

    // Process in pairs. If odd number, pad with X27 (or just save single)
    let rec savePairs (remaining: LIR.PhysReg list) (offset: int) (acc: ARM64.Instr list) =
        match remaining with
        | [] -> (List.rev acc, offset)
        | [single] ->
            // Single register: use STR instead of STP
            let instr = ARM64.STR (lirPhysRegToARM64Reg single, ARM64.SP, int16 offset)
            (List.rev (instr :: acc), offset + 8)
        | r1 :: r2 :: rest ->
            let instr = ARM64.STP (lirPhysRegToARM64Reg r1, lirPhysRegToARM64Reg r2, ARM64.SP, int16 offset)
            savePairs rest (offset + 16) (instr :: acc)

    if List.isEmpty sorted then
        ([], 0)
    else
        savePairs sorted 0 []

/// Generate LDP instructions to restore callee-saved register pairs
let generateCalleeSavedRestores (regs: LIR.PhysReg list) : ARM64.Instr list =
    let sorted = regs |> List.sortBy (fun r ->
        match r with
        | LIR.X19 -> 19 | LIR.X20 -> 20 | LIR.X21 -> 21 | LIR.X22 -> 22
        | LIR.X23 -> 23 | LIR.X24 -> 24 | LIR.X25 -> 25 | LIR.X26 -> 26
        | LIR.X27 -> 27 | _ -> 99)

    let rec restorePairs (remaining: LIR.PhysReg list) (offset: int) (acc: ARM64.Instr list) =
        match remaining with
        | [] -> List.rev acc
        | [single] ->
            let instr = ARM64.LDR (lirPhysRegToARM64Reg single, ARM64.SP, int16 offset)
            List.rev (instr :: acc)
        | r1 :: r2 :: rest ->
            let instr = ARM64.LDP (lirPhysRegToARM64Reg r1, lirPhysRegToARM64Reg r2, ARM64.SP, int16 offset)
            restorePairs rest (offset + 16) (instr :: acc)

    if List.isEmpty sorted then []
    else restorePairs sorted 0 []

/// Calculate stack space needed for callee-saved registers (16-byte aligned)
let calleeSavedStackSpace (regs: LIR.PhysReg list) : int =
    let count = List.length regs
    if count = 0 then 0
    else ((count * 8 + 15) / 16) * 16  // 16-byte aligned

/// Generate function prologue
/// Saves FP, LR, callee-saved registers, and allocates stack space
let generatePrologue (usedCalleeSaved: LIR.PhysReg list) (stackSize: int) : ARM64.Instr list =
    // Prologue sequence:
    // 1. Decrement SP and save FP (X29) and LR (X30)
    // 2. Set FP = SP: MOV X29, SP
    // 3. Save callee-saved registers (if any)
    // 4. Allocate stack space for spills: SUB SP, SP, #stackSize

    let saveFpLr = [
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)  // Decrement SP by 16
        ARM64.STP (ARM64.X29, ARM64.X30, ARM64.SP, 0s)  // Store at [SP]
    ]
    let setFp = [ARM64.MOV_reg (ARM64.X29, ARM64.SP)]

    // Save callee-saved registers
    let calleeSavedSpace = calleeSavedStackSpace usedCalleeSaved
    let (saveCalleeSavedInstrs, _) = generateCalleeSavedSaves usedCalleeSaved
    let allocCalleeSaved =
        if calleeSavedSpace > 0 then
            [ARM64.SUB_imm (ARM64.SP, ARM64.SP, uint16 calleeSavedSpace)]
        else
            []

    let allocStack =
        if stackSize > 0 then
            [ARM64.SUB_imm (ARM64.SP, ARM64.SP, uint16 stackSize)]
        else
            []

    saveFpLr @ setFp @ allocStack @ allocCalleeSaved @ saveCalleeSavedInstrs

/// Generate function epilogue
/// Restores callee-saved registers, FP, LR, and returns
let generateEpilogue (usedCalleeSaved: LIR.PhysReg list) (stackSize: int) : ARM64.Instr list =
    // Epilogue sequence (reverse of prologue):
    // 1. Deallocate stack: ADD SP, SP, #stackSize
    // 2. Restore callee-saved registers
    // 3. Deallocate callee-saved space
    // 4. Restore FP and LR from [SP], then increment SP
    // 5. Return: RET

    let deallocStack =
        if stackSize > 0 then
            [ARM64.ADD_imm (ARM64.SP, ARM64.SP, uint16 stackSize)]
        else
            []

    // Restore callee-saved registers
    let calleeSavedSpace = calleeSavedStackSpace usedCalleeSaved
    let restoreCalleeSavedInstrs = generateCalleeSavedRestores usedCalleeSaved
    let deallocCalleeSaved =
        if calleeSavedSpace > 0 then
            [ARM64.ADD_imm (ARM64.SP, ARM64.SP, uint16 calleeSavedSpace)]
        else
            []

    let restoreFpLr = [
        ARM64.LDP (ARM64.X29, ARM64.X30, ARM64.SP, 0s)  // Load from [SP]
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)  // Increment SP by 16
    ]
    let ret = [ARM64.RET]

    restoreCalleeSavedInstrs @ deallocCalleeSaved @ deallocStack @ restoreFpLr @ ret

/// Convert LIR instruction to ARM64 instructions
let convertInstr (ctx: CodeGenContext) (instr: LIR.Instr) : Result<ARM64.Instr list, string> =
    match instr with
    | LIR.Mov (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            match src with
            | LIR.Imm value ->
                Ok (loadImmediate destReg value)
            | LIR.FloatImm _ ->
                Error "Float code generation not yet implemented"
            | LIR.Reg srcReg ->
                lirRegToARM64Reg srcReg
                |> Result.map (fun srcARM64 -> [ARM64.MOV_reg (destReg, srcARM64)])
            | LIR.StackSlot offset ->
                // Load from stack slot into destination register
                loadStackSlot destReg offset
            | LIR.StringRef idx ->
                // Convert pool string to heap string format when storing in variable
                // This ensures all string variables have consistent heap layout:
                // [length:8][data:N][refcount:8]
                //
                // Algorithm:
                // 1. Get pool string address and length
                // 2. Allocate heap: length + 16 bytes
                // 3. Store length at [heap]
                // 4. Copy bytes from pool to [heap+8]
                // 5. Store refcount=1 at [heap+8+length]
                // 6. dest = heap address
                //
                // IMPORTANT: Use X13 for loop counter, not X0!
                // If destReg is X0, using X0 as loop counter would clobber the result.
                match Map.tryFind idx ctx.StringPool.Strings with
                | Some (_, len) ->
                    let label = sprintf "str_%d" idx
                    let totalSize = ((len + 16) + 7) &&& (~~~7)  // 8-byte aligned
                    Ok ([
                        // Load pool string address into X9
                        ARM64.ADRP (ARM64.X9, label)
                        ARM64.ADD_label (ARM64.X9, ARM64.X9, label)
                        // Allocate heap space (bump allocator)
                        ARM64.MOV_reg (destReg, ARM64.X28)  // dest = current heap pointer
                        ARM64.ADD_imm (ARM64.X28, ARM64.X28, uint16 totalSize)  // bump pointer
                        // Store length
                    ] @ loadImmediate ARM64.X10 (int64 len) @ [
                        ARM64.STR (ARM64.X10, destReg, 0s)  // [dest] = length
                        // Copy bytes: loop counter in X13 (NOT X0 - it might be destReg!)
                        ARM64.MOVZ (ARM64.X13, 0us, 0)  // X13 = 0
                    ] @ loadImmediate ARM64.X11 (int64 len) @ [
                        // Loop start (if X13 >= len, done)
                        ARM64.CMP_reg (ARM64.X13, ARM64.X11)
                        ARM64.B_cond (ARM64.GE, 7)  // Skip 7 instructions to exit loop (to after B)
                        ARM64.LDRB (ARM64.X15, ARM64.X9, ARM64.X13)  // X15 = pool[X13]
                        ARM64.ADD_imm (ARM64.X12, destReg, 8us)  // X12 = dest + 8
                        ARM64.ADD_reg (ARM64.X12, ARM64.X12, ARM64.X13)  // X12 = dest + 8 + X13
                        ARM64.STRB_reg (ARM64.X15, ARM64.X12)  // [X12] = byte
                        ARM64.ADD_imm (ARM64.X13, ARM64.X13, 1us)  // X13++
                        ARM64.B (-7)  // Loop back to CMP
                        // Store refcount
                        ARM64.ADD_imm (ARM64.X12, destReg, 8us)
                        ARM64.ADD_reg (ARM64.X12, ARM64.X12, ARM64.X10)  // X12 = dest + 8 + len
                        ARM64.MOVZ (ARM64.X15, 1us, 0)
                        ARM64.STR (ARM64.X15, ARM64.X12, 0s)  // [X12] = 1
                    ])
                | None -> Error $"String index {idx} not found in pool"
            | LIR.FloatRef _ ->
                Error "Cannot MOV float reference - use FLoad instruction"
            | LIR.FuncAddr funcName ->
                // Load function address using ADR instruction
                Ok [ARM64.ADR (destReg, funcName)])

    | LIR.Store (offset, src) ->
        // Store register to stack slot
        lirRegToARM64Reg src
        |> Result.bind (fun srcReg -> storeStackSlot srcReg offset)

    | LIR.Add (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                match right with
                | LIR.Imm value when value >= 0L && value < 4096L ->
                    // Can use immediate ADD
                    Ok [ARM64.ADD_imm (destReg, leftReg, uint16 value)]
                | LIR.Imm value ->
                    // Need to load immediate into register first
                    let tempReg = ARM64.X9  // Use X9 as temp
                    Ok (loadImmediate tempReg value @ [ARM64.ADD_reg (destReg, leftReg, tempReg)])
                | LIR.FloatImm _ ->
                    Error "Float code generation not yet implemented"
                | LIR.Reg rightReg ->
                    lirRegToARM64Reg rightReg
                    |> Result.map (fun rightARM64 -> [ARM64.ADD_reg (destReg, leftReg, rightARM64)])
                | LIR.StackSlot offset ->
                    // Load stack slot into temp register, then add
                    let tempReg = ARM64.X9
                    loadStackSlot tempReg offset
                    |> Result.map (fun loadInstrs -> loadInstrs @ [ARM64.ADD_reg (destReg, leftReg, tempReg)])
                | LIR.StringRef _ ->
                    Error "Cannot use string reference in arithmetic operation"
                | LIR.FloatRef _ ->
                    Error "Cannot use float reference in integer arithmetic"
                | LIR.FuncAddr _ ->
                    Error "Cannot use function address in arithmetic operation"))

    | LIR.Sub (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                match right with
                | LIR.Imm value when value >= 0L && value < 4096L ->
                    Ok [ARM64.SUB_imm (destReg, leftReg, uint16 value)]
                | LIR.Imm value ->
                    let tempReg = ARM64.X9
                    Ok (loadImmediate tempReg value @ [ARM64.SUB_reg (destReg, leftReg, tempReg)])
                | LIR.FloatImm _ ->
                    Error "Float code generation not yet implemented"
                | LIR.Reg rightReg ->
                    lirRegToARM64Reg rightReg
                    |> Result.map (fun rightARM64 -> [ARM64.SUB_reg (destReg, leftReg, rightARM64)])
                | LIR.StackSlot offset ->
                    // Load stack slot into temp register, then subtract
                    let tempReg = ARM64.X9
                    loadStackSlot tempReg offset
                    |> Result.map (fun loadInstrs -> loadInstrs @ [ARM64.SUB_reg (destReg, leftReg, tempReg)])
                | LIR.StringRef _ ->
                    Error "Cannot use string reference in arithmetic operation"
                | LIR.FloatRef _ ->
                    Error "Cannot use float reference in integer arithmetic"
                | LIR.FuncAddr _ ->
                    Error "Cannot use function address in arithmetic operation"))


    | LIR.Mul (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                lirRegToARM64Reg right
                |> Result.map (fun rightReg -> [ARM64.MUL (destReg, leftReg, rightReg)])))

    | LIR.Sdiv (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                lirRegToARM64Reg right
                |> Result.map (fun rightReg -> [ARM64.SDIV (destReg, leftReg, rightReg)])))

    | LIR.Msub (dest, mulLeft, mulRight, sub) ->
        // MSUB: dest = sub - mulLeft * mulRight
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg mulLeft
            |> Result.bind (fun mulLeftReg ->
                lirRegToARM64Reg mulRight
                |> Result.bind (fun mulRightReg ->
                    lirRegToARM64Reg sub
                    |> Result.map (fun subReg ->
                        [ARM64.MSUB (destReg, mulLeftReg, mulRightReg, subReg)]))))

    | LIR.Cmp (left, right) ->
        lirRegToARM64Reg left
        |> Result.bind (fun leftReg ->
            match right with
            | LIR.Imm value when value >= 0L && value < 4096L ->
                Ok [ARM64.CMP_imm (leftReg, uint16 value)]
            | LIR.Imm value ->
                let tempReg = ARM64.X9
                Ok (loadImmediate tempReg value @ [ARM64.CMP_reg (leftReg, tempReg)])
            | LIR.FloatImm _ ->
                Error "Float code generation not yet implemented"
            | LIR.Reg rightReg ->
                lirRegToARM64Reg rightReg
                |> Result.map (fun rightARM64 -> [ARM64.CMP_reg (leftReg, rightARM64)])
            | LIR.StackSlot offset ->
                // Load stack slot into temp register, then compare
                let tempReg = ARM64.X9
                loadStackSlot tempReg offset
                |> Result.map (fun loadInstrs -> loadInstrs @ [ARM64.CMP_reg (leftReg, tempReg)])
            | LIR.StringRef _ ->
                Error "Cannot compare string references directly"
            | LIR.FloatRef _ ->
                Error "Cannot compare float references directly - use FCmp"
            | LIR.FuncAddr _ ->
                Error "Cannot compare function addresses directly")

    | LIR.Cset (dest, cond) ->
        lirRegToARM64Reg dest
        |> Result.map (fun destReg ->
            let arm64Cond =
                match cond with
                | LIR.EQ -> ARM64.EQ
                | LIR.NE -> ARM64.NE
                | LIR.LT -> ARM64.LT
                | LIR.GT -> ARM64.GT
                | LIR.LE -> ARM64.LE
                | LIR.GE -> ARM64.GE
            [ARM64.CSET (destReg, arm64Cond)])

    | LIR.And (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                lirRegToARM64Reg right
                |> Result.map (fun rightReg -> [ARM64.AND_reg (destReg, leftReg, rightReg)])))

    | LIR.Orr (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                lirRegToARM64Reg right
                |> Result.map (fun rightReg -> [ARM64.ORR_reg (destReg, leftReg, rightReg)])))

    | LIR.Eor (dest, left, right) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg left
            |> Result.bind (fun leftReg ->
                lirRegToARM64Reg right
                |> Result.map (fun rightReg -> [ARM64.EOR_reg (destReg, leftReg, rightReg)])))

    | LIR.Lsl (dest, src, shift) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.bind (fun srcReg ->
                lirRegToARM64Reg shift
                |> Result.map (fun shiftReg -> [ARM64.LSL_reg (destReg, srcReg, shiftReg)])))

    | LIR.Lsr (dest, src, shift) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.bind (fun srcReg ->
                lirRegToARM64Reg shift
                |> Result.map (fun shiftReg -> [ARM64.LSR_reg (destReg, srcReg, shiftReg)])))

    | LIR.Mvn (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64.MVN (destReg, srcReg)]))

    | LIR.PrintBool reg ->
        // Print booleans as "true" or "false" (no exit)
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            if regARM64 <> ARM64.X0 then
                [ARM64.MOV_reg (ARM64.X0, regARM64)] @ Runtime.generatePrintBoolNoExit ()
            else
                Runtime.generatePrintBoolNoExit ())

    | LIR.Call (dest, funcName, args) ->
        // Function call: arguments already moved to X0-X7 by preceding MOVs
        // Caller-save is handled by SaveRegs/RestoreRegs instructions
        Ok [ARM64.BL funcName]

    | LIR.IndirectCall (dest, func, args) ->
        // Indirect call: call through function pointer in register
        // Use BLR instruction instead of BL
        lirRegToARM64Reg func
        |> Result.map (fun funcReg -> [ARM64.BLR funcReg])

    | LIR.ClosureAlloc (dest, funcName, captures) ->
        // Allocate closure on heap: (func_ptr, cap1, cap2, ...)
        // Each slot is 8 bytes
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            let numSlots = 1 + List.length captures  // func_ptr + captures
            let sizeBytes = numSlots * 8
            // Total size includes 8 bytes for ref count, aligned to 8 bytes
            let totalSize = ((sizeBytes + 8) + 7) &&& (~~~7)

            // Allocate using bump allocator
            let allocInstrs = [
                ARM64.MOV_reg (destReg, ARM64.X28)                      // dest = current heap pointer
                ARM64.MOVZ (ARM64.X15, 1us, 0)                          // X15 = 1 (initial ref count)
                ARM64.STR (ARM64.X15, ARM64.X28, int16 sizeBytes)       // store ref count after payload
                ARM64.ADD_imm (ARM64.X28, ARM64.X28, uint16 totalSize)  // bump pointer
            ]

            // Store function address at offset 0
            let storeFuncAddr = [
                ARM64.ADR (ARM64.X15, funcName)                         // X15 = function address
                ARM64.STR (ARM64.X15, destReg, 0s)                      // [dest] = func_ptr
            ]

            // Store captures at subsequent offsets
            let storeCaptures =
                captures
                |> List.mapi (fun i cap -> (i, cap))
                |> List.collect (fun (i, cap) ->
                    let offset = (i + 1) * 8
                    match cap with
                    | LIR.Imm value ->
                        loadImmediate ARM64.X15 value @
                        [ARM64.STR (ARM64.X15, destReg, int16 offset)]
                    | LIR.Reg reg ->
                        match lirRegToARM64Reg reg with
                        | Ok srcReg ->
                            // Avoid storing dest into itself at offset
                            if srcReg = destReg then
                                [ARM64.MOV_reg (ARM64.X15, srcReg); ARM64.STR (ARM64.X15, destReg, int16 offset)]
                            else
                                [ARM64.STR (srcReg, destReg, int16 offset)]
                        | Error _ -> []  // Shouldn't happen
                    | LIR.FuncAddr fname ->
                        [ARM64.ADR (ARM64.X15, fname); ARM64.STR (ARM64.X15, destReg, int16 offset)]
                    | _ -> [])  // Other operand types not expected

            Ok (allocInstrs @ storeFuncAddr @ storeCaptures))

    | LIR.ClosureCall (dest, funcPtr, args) ->
        // Call through closure - MIR_to_LIR already set up:
        // - X9: function pointer (loaded from closure[0])
        // - X0: closure
        // - X1-X7: args
        // Just do the BLR
        lirRegToARM64Reg funcPtr
        |> Result.map (fun funcPtrReg ->
            [ARM64.BLR funcPtrReg])

    | LIR.SaveRegs ->
        // Save caller-saved registers (X1-X10) before call
        // Allocate 80 bytes (10 regs * 8 bytes), store pairs
        Ok [
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 80us)
            ARM64.STP (ARM64.X1, ARM64.X2, ARM64.SP, 0s)
            ARM64.STP (ARM64.X3, ARM64.X4, ARM64.SP, 16s)
            ARM64.STP (ARM64.X5, ARM64.X6, ARM64.SP, 32s)
            ARM64.STP (ARM64.X7, ARM64.X8, ARM64.SP, 48s)
            ARM64.STP (ARM64.X9, ARM64.X10, ARM64.SP, 64s)
        ]

    | LIR.RestoreRegs ->
        // Restore caller-saved registers (X1-X10) after call
        Ok [
            ARM64.LDP (ARM64.X1, ARM64.X2, ARM64.SP, 0s)
            ARM64.LDP (ARM64.X3, ARM64.X4, ARM64.SP, 16s)
            ARM64.LDP (ARM64.X5, ARM64.X6, ARM64.SP, 32s)
            ARM64.LDP (ARM64.X7, ARM64.X8, ARM64.SP, 48s)
            ARM64.LDP (ARM64.X9, ARM64.X10, ARM64.SP, 64s)
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 80us)
        ]

    | LIR.PrintInt reg ->
        // Value to print should be in X0 (no exit)
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            if regARM64 <> ARM64.X0 then
                // Move to X0 if not already there
                [ARM64.MOV_reg (ARM64.X0, regARM64)] @ Runtime.generatePrintIntNoExit ()
            else
                Runtime.generatePrintIntNoExit ())

    | LIR.Exit ->
        // Exit program with code 0
        Ok (Runtime.generateExit ())

    | LIR.PrintFloat freg ->
        // Print float value from FP register
        // Value should be in D0 for generatePrintFloat
        lirFRegToARM64FReg freg
        |> Result.map (fun fregARM64 ->
            if fregARM64 <> ARM64.D0 then
                // Move to D0 if not already there
                [ARM64.FMOV_reg (ARM64.D0, fregARM64)] @ Runtime.generatePrintFloat ()
            else
                Runtime.generatePrintFloat ())

    | LIR.PrintString (idx, len) ->
        // To print a string, we need:
        // 1. ADRP + ADD to load string address into X0
        // 2. Call Runtime.generatePrintString which handles write syscall
        // String labels are named "str_0", "str_1", etc.
        let stringLabel = sprintf "str_%d" idx
        Ok ([
            ARM64.ADRP (ARM64.X0, stringLabel)  // Load page address of string
            ARM64.ADD_label (ARM64.X0, ARM64.X0, stringLabel)  // Add page offset
        ] @ Runtime.generatePrintString len)

    // Floating-point instructions
    | LIR.FMov (dest, src) ->
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg src
            |> Result.map (fun srcReg -> [ARM64.FMOV_reg (destReg, srcReg)]))

    | LIR.FLoad (dest, idx) ->
        // Load float from pool into FP register
        // Float labels are named "_float0", "_float1", etc.
        lirFRegToARM64FReg dest
        |> Result.map (fun destReg ->
            let floatLabel = sprintf "_float%d" idx
            [
                ARM64.ADRP (ARM64.X9, floatLabel)           // Load page address of float
                ARM64.ADD_label (ARM64.X9, ARM64.X9, floatLabel)  // Add page offset
                ARM64.LDR_fp (destReg, ARM64.X9, 0s)        // Load float from [X9]
            ])

    | LIR.FAdd (dest, left, right) ->
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg left
            |> Result.bind (fun leftReg ->
                lirFRegToARM64FReg right
                |> Result.map (fun rightReg -> [ARM64.FADD (destReg, leftReg, rightReg)])))

    | LIR.FSub (dest, left, right) ->
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg left
            |> Result.bind (fun leftReg ->
                lirFRegToARM64FReg right
                |> Result.map (fun rightReg -> [ARM64.FSUB (destReg, leftReg, rightReg)])))

    | LIR.FMul (dest, left, right) ->
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg left
            |> Result.bind (fun leftReg ->
                lirFRegToARM64FReg right
                |> Result.map (fun rightReg -> [ARM64.FMUL (destReg, leftReg, rightReg)])))

    | LIR.FDiv (dest, left, right) ->
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg left
            |> Result.bind (fun leftReg ->
                lirFRegToARM64FReg right
                |> Result.map (fun rightReg -> [ARM64.FDIV (destReg, leftReg, rightReg)])))

    | LIR.FNeg (dest, src) ->
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg src
            |> Result.map (fun srcReg -> [ARM64.FNEG (destReg, srcReg)]))

    | LIR.FCmp (left, right) ->
        lirFRegToARM64FReg left
        |> Result.bind (fun leftReg ->
            lirFRegToARM64FReg right
            |> Result.map (fun rightReg -> [ARM64.FCMP (leftReg, rightReg)]))

    // Heap operations
    | LIR.HeapAlloc (dest, sizeBytes) ->
        // Heap allocator with free list support
        // X27 = free list heads base, X28 = bump allocator pointer
        //
        // Memory layout with reference counting:
        //   [payload: sizeBytes][refcount: 8 bytes]
        //
        // Algorithm:
        // 1. Check free list for this size class (sizeClassOffset = sizeBytes)
        // 2. If free list non-empty: pop from list, initialize refcount, return
        // 3. If empty: bump allocate from X28
        //
        // Code structure (10 instructions):
        //   LDR X15, [X27, sizeBytes]         ; Load free list head
        //   CBZ X15, +5                       ; If empty, skip to bump alloc (5 instrs)
        //   MOV dest, X15                     ; dest = freed block
        //   LDR X14, [X15, 0]                 ; Load next pointer from freed block
        //   STR X14, [X27, sizeBytes]         ; Update free list head
        //   MOVZ X14, 1                       ; X14 = 1 (initial ref count)
        //   STR X14, [dest, sizeBytes]        ; Store ref count
        //   B +5                              ; Skip bump allocator (5 instrs)
        //   ; Bump allocator:
        //   MOV dest, X28                     ; dest = current heap pointer
        //   MOVZ X15, 1                       ; X15 = 1 (initial ref count)
        //   STR X15, [X28, sizeBytes]         ; store ref count after payload
        //   ADD X28, X28, totalSize           ; bump pointer
        //   (continue)
        lirRegToARM64Reg dest
        |> Result.map (fun destReg ->
            // Total size includes 8 bytes for ref count, aligned to 8 bytes
            let totalSize = ((sizeBytes + 8) + 7) &&& (~~~7)
            if ctx.Options.DisableFreeList then
                // Bump allocator only (no free list reuse)
                [
                    ARM64.MOV_reg (destReg, ARM64.X28)                  // dest = current heap pointer
                    ARM64.MOVZ (ARM64.X15, 1us, 0)                      // X15 = 1 (initial ref count)
                    ARM64.STR (ARM64.X15, ARM64.X28, int16 sizeBytes)   // store ref count after payload
                    ARM64.ADD_imm (ARM64.X28, ARM64.X28, uint16 totalSize) // bump pointer
                ]
            else
                // Full allocator with free list support
                [
                    // Check free list
                    ARM64.LDR (ARM64.X15, ARM64.X27, int16 sizeBytes)   // Load free list head
                    ARM64.CBZ_offset (ARM64.X15, 7)                     // If empty, skip to bump alloc
                    // Pop from free list
                    ARM64.MOV_reg (destReg, ARM64.X15)                  // dest = freed block
                    ARM64.LDR (ARM64.X14, ARM64.X15, 0s)                // Load next pointer
                    ARM64.STR (ARM64.X14, ARM64.X27, int16 sizeBytes)   // Update free list head
                    ARM64.MOVZ (ARM64.X14, 1us, 0)                      // X14 = 1 (initial ref count)
                    ARM64.STR (ARM64.X14, destReg, int16 sizeBytes)     // Store ref count
                    ARM64.B 4                                           // Skip bump allocator
                    // Bump allocator (fallback when free list empty)
                    ARM64.MOV_reg (destReg, ARM64.X28)                  // dest = current heap pointer
                    ARM64.MOVZ (ARM64.X15, 1us, 0)                      // X15 = 1 (initial ref count)
                    ARM64.STR (ARM64.X15, ARM64.X28, int16 sizeBytes)   // store ref count after payload
                    ARM64.ADD_imm (ARM64.X28, ARM64.X28, uint16 totalSize) // bump pointer
                ])

    | LIR.HeapStore (addr, offset, src) ->
        // Store value at addr + offset (offset is in bytes)
        lirRegToARM64Reg addr
        |> Result.bind (fun addrReg ->
            match src with
            | LIR.Imm value ->
                // Load immediate into temp register, then store
                let tempReg = ARM64.X9
                Ok (loadImmediate tempReg value @
                    [ARM64.STR (tempReg, addrReg, int16 offset)])
            | LIR.Reg srcReg ->
                lirRegToARM64Reg srcReg
                |> Result.map (fun srcARM64 ->
                    // If src and addr are the same register, we have a problem
                    // due to register allocation bug. Use temp register as workaround.
                    if srcARM64 = addrReg then
                        // Save value to temp, use temp for store
                        let tempReg = ARM64.X9
                        [ARM64.MOV_reg (tempReg, srcARM64); ARM64.STR (tempReg, addrReg, int16 offset)]
                    else
                        [ARM64.STR (srcARM64, addrReg, int16 offset)])
            | LIR.StackSlot slotOffset ->
                // Load from stack slot into temp, then store to heap
                let tempReg = ARM64.X9
                loadStackSlot tempReg slotOffset
                |> Result.map (fun loadInstrs ->
                    loadInstrs @ [ARM64.STR (tempReg, addrReg, int16 offset)])
            | LIR.FuncAddr funcName ->
                // Load function address into temp, then store to heap
                let tempReg = ARM64.X9
                Ok [ARM64.ADR (tempReg, funcName); ARM64.STR (tempReg, addrReg, int16 offset)]
            | _ -> Error "Unsupported operand type in HeapStore")

    | LIR.HeapLoad (dest, addr, offset) ->
        // Load value from addr + offset (offset is in bytes)
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg addr
            |> Result.map (fun addrReg ->
                [ARM64.LDR (destReg, addrReg, int16 offset)]))

    | LIR.RefCountInc (addr, payloadSize) ->
        // Increment ref count at [addr + payloadSize]
        // Use X15 as temp register
        lirRegToARM64Reg addr
        |> Result.map (fun addrReg ->
            [
                ARM64.LDR (ARM64.X15, addrReg, int16 payloadSize)   // Load ref count
                ARM64.ADD_imm (ARM64.X15, ARM64.X15, 1us)           // Increment
                ARM64.STR (ARM64.X15, addrReg, int16 payloadSize)   // Store back
            ])

    | LIR.RefCountDec (addr, payloadSize) ->
        // Decrement ref count at [addr + payloadSize]
        // When ref count hits 0, add block to free list for memory reuse
        //
        // Free list structure:
        // - X27 = base of free list heads (32 slots × 8 bytes = 256 bytes)
        // - Slot N contains head of free list for blocks of size (N+1)*8 bytes
        // - sizeClassOffset = payloadSize (for 8-aligned payloads)
        // - Freed blocks use first 8 bytes as next pointer
        //
        // Code structure (7 instructions):
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
            [
                ARM64.LDR (ARM64.X15, addrReg, int16 payloadSize)   // Load ref count
                ARM64.SUB_imm (ARM64.X15, ARM64.X15, 1us)           // Decrement
                ARM64.STR (ARM64.X15, addrReg, int16 payloadSize)   // Store back
                ARM64.CBNZ_offset (ARM64.X15, 4)                    // If not zero, skip 4 instructions
                ARM64.LDR (ARM64.X14, ARM64.X27, int16 payloadSize) // Load free list head
                ARM64.STR (ARM64.X14, addrReg, 0s)                  // Store as next pointer in freed block
                ARM64.STR (addrReg, ARM64.X27, int16 payloadSize)   // Update free list head
            ])

    | LIR.StringConcat (dest, left, right) ->
        // String concatenation:
        // Heap string layout: [length:8][data:N][refcount:8]
        // Pool string layout: just raw bytes (no length/refcount)
        //
        // Register usage:
        // X9  = left data address (for pool: string address, for heap: addr+8)
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
        // 5. Store total length at [X14]
        // 6. Copy left bytes to [X14+8]
        // 7. Copy right bytes to [X14+8+len1]
        // 8. Store refcount=1 at [X14+8+total]
        // 9. Move result to dest

        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            // Helper: load operand address and length into registers
            let loadOperandInfo (operand: LIR.Operand) (addrReg: ARM64.Reg) (lenReg: ARM64.Reg) : Result<ARM64.Instr list, string> =
                match operand with
                | LIR.StringRef idx ->
                    // Pool string: address via ADRP+ADD, length from pool (compile-time)
                    match Map.tryFind idx ctx.StringPool.Strings with
                    | Some (_, len) ->
                        let label = sprintf "str_%d" idx
                        Ok ([
                            ARM64.ADRP (addrReg, label)
                            ARM64.ADD_label (addrReg, addrReg, label)
                        ] @ loadImmediate lenReg (int64 len))
                    | None -> Error $"String index {idx} not found in pool"
                | LIR.Reg reg ->
                    // Heap string: address in reg, length at [reg], data at [reg+8]
                    lirRegToARM64Reg reg
                    |> Result.map (fun srcReg ->
                        [
                            ARM64.LDR (lenReg, srcReg, 0s)           // len = [srcReg]
                            ARM64.ADD_imm (addrReg, srcReg, 8us)     // addr = srcReg + 8 (data start)
                        ])
                | _ -> Error "StringConcat requires StringRef or Reg operand"

            // Load both operands
            loadOperandInfo left ARM64.X9 ARM64.X10
            |> Result.bind (fun leftInstrs ->
                loadOperandInfo right ARM64.X11 ARM64.X12
                |> Result.map (fun rightInstrs ->
                    // Calculate total length
                    let calcTotal = [ARM64.ADD_reg (ARM64.X13, ARM64.X10, ARM64.X12)]

                    // Allocate: totalLen + 16 bytes (8 for length, 8 for refcount)
                    // Using bump allocator (X28 = bump pointer)
                    let allocate = [
                        ARM64.ADD_imm (ARM64.X14, ARM64.X13, 16us)   // X14 = total + 16
                        ARM64.ADD_imm (ARM64.X14, ARM64.X14, 7us)    // Align up
                        ARM64.MOVZ (ARM64.X15, 0xFFF8us, 0)          // ~7 mask (lower bits)
                        ARM64.MOVK (ARM64.X15, 0xFFFFus, 1)
                        ARM64.MOVK (ARM64.X15, 0xFFFFus, 2)
                        ARM64.MOVK (ARM64.X15, 0xFFFFus, 3)
                        ARM64.AND_reg (ARM64.X14, ARM64.X14, ARM64.X15)  // X14 = aligned size
                        ARM64.MOV_reg (ARM64.X14, ARM64.X28)            // X14 = current heap ptr (result)
                        ARM64.ADD_imm (ARM64.X15, ARM64.X13, 16us)      // X15 = total + 16
                        ARM64.ADD_imm (ARM64.X15, ARM64.X15, 7us)       // Align
                        ARM64.MOVZ (ARM64.X0, 0xFFF8us, 0)              // ~7 mask again (X15 was clobbered)
                        ARM64.MOVK (ARM64.X0, 0xFFFFus, 1)
                        ARM64.MOVK (ARM64.X0, 0xFFFFus, 2)
                        ARM64.MOVK (ARM64.X0, 0xFFFFus, 3)
                        ARM64.AND_reg (ARM64.X15, ARM64.X15, ARM64.X0)
                        ARM64.ADD_reg (ARM64.X28, ARM64.X28, ARM64.X15) // Bump heap pointer
                    ]

                    // Store total length at [X14]
                    let storeLen = [ARM64.STR (ARM64.X13, ARM64.X14, 0s)]

                    // Copy left bytes: loop copying X10 bytes from X9 to [X14+8]
                    // Use X0 as loop counter, X15 as temp byte
                    let copyLeft = [
                        ARM64.MOVZ (ARM64.X0, 0us, 0)                    // 0: X0 = 0 (counter)
                        // Loop: if X0 >= X10, done
                        ARM64.CMP_reg (ARM64.X0, ARM64.X10)              // 1: compare
                        ARM64.B_cond (ARM64.GE, 7)                       // 2: Skip 7 instructions to exit (to index 9)
                        ARM64.LDRB (ARM64.X15, ARM64.X9, ARM64.X0)       // 3: X15 = byte at [X9 + X0]
                        ARM64.ADD_imm (ARM64.X1, ARM64.X14, 8us)         // 4: X1 = X14 + 8 (dest base)
                        ARM64.ADD_reg (ARM64.X1, ARM64.X1, ARM64.X0)     // 5: X1 = dest + counter
                        ARM64.STRB_reg (ARM64.X15, ARM64.X1)             // 6: [X1] = byte
                        ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us)          // 7: X0++
                        ARM64.B (-7)                                     // 8: Loop back to CMP (index 1)
                    ]

                    // Copy right bytes: loop copying X12 bytes from X11 to [X14+8+X10]
                    let copyRight = [
                        ARM64.MOVZ (ARM64.X0, 0us, 0)                    // X0 = 0 (counter)
                        ARM64.ADD_imm (ARM64.X1, ARM64.X14, 8us)         // X1 = X14 + 8
                        ARM64.ADD_reg (ARM64.X1, ARM64.X1, ARM64.X10)    // X1 = X14 + 8 + len1 (dest start for right)
                        // Loop: if X0 >= X12, done
                        ARM64.CMP_reg (ARM64.X0, ARM64.X12)
                        ARM64.B_cond (ARM64.GE, 6)                       // Skip 6 instructions if done
                        ARM64.LDRB (ARM64.X15, ARM64.X11, ARM64.X0)      // X15 = byte at [X11 + X0]
                        ARM64.ADD_reg (ARM64.X2, ARM64.X1, ARM64.X0)     // X2 = dest + counter
                        ARM64.STRB_reg (ARM64.X15, ARM64.X2)             // [X2] = byte
                        ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us)          // X0++
                        ARM64.B (-6)                                     // Loop back
                    ]

                    // Store refcount=1 at [X14+8+total]
                    let storeRefcount = [
                        ARM64.ADD_imm (ARM64.X0, ARM64.X14, 8us)         // X0 = X14 + 8
                        ARM64.ADD_reg (ARM64.X0, ARM64.X0, ARM64.X13)    // X0 = X14 + 8 + total
                        ARM64.MOVZ (ARM64.X15, 1us, 0)                   // X15 = 1
                        ARM64.STR (ARM64.X15, ARM64.X0, 0s)              // [X0] = 1
                    ]

                    // Move result to dest
                    let moveResult = [ARM64.MOV_reg (destReg, ARM64.X14)]

                    leftInstrs @ rightInstrs @ calcTotal @ allocate @ storeLen @ copyLeft @ copyRight @ storeRefcount @ moveResult
                )))

    | LIR.PrintHeapString reg ->
        // Print heap string: layout is [len:8][data:N]
        // Note: The syscall clobbers X0, X1, X2, X8. If the input register is one
        // of these, we save it to X9 before and restore after so subsequent code
        // can still use it.
        // 1. Save input to X9
        // 2. Load length from [X9] into X2
        // 3. Compute data pointer (X9 + 8) into X1
        // 4. Set X0 = 1 (stdout)
        // 5. write syscall
        // 6. Restore input register if it was clobbered
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            let isClobbered = regARM64 = ARM64.X0 || regARM64 = ARM64.X1 || regARM64 = ARM64.X2 || regARM64 = ARM64.X8
            let restoreInstrs = if isClobbered then [ARM64.MOV_reg (regARM64, ARM64.X9)] else []
            [
                ARM64.MOV_reg (ARM64.X9, regARM64)           // X9 = input (save in case regARM64 is X0/X1/X2)
                ARM64.LDR (ARM64.X2, ARM64.X9, 0s)           // X2 = length
                ARM64.ADD_imm (ARM64.X1, ARM64.X9, 8us)      // X1 = data pointer (X9 + 8)
                ARM64.MOVZ (ARM64.X0, 1us, 0)                // X0 = stdout fd
            ] @ Runtime.generateWriteSyscall () @ restoreInstrs)

    | LIR.LoadFuncAddr (dest, funcName) ->
        // Load the address of a function into the destination register using ADR
        lirRegToARM64Reg dest
        |> Result.map (fun destReg ->
            [ARM64.ADR (destReg, funcName)])

    | LIR.FileReadText (dest, path) ->
        // File reading: generates syscall sequence to read file contents
        // Returns Result<String, String>
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            match path with
            | LIR.Reg pathReg ->
                // Already a heap string pointer
                lirRegToARM64Reg pathReg
                |> Result.map (fun pathARM64 ->
                    Runtime.generateFileReadText destReg pathARM64)
            | LIR.StringRef idx ->
                // Pool string - convert to heap format first (same as FileExists)
                match Map.tryFind idx ctx.StringPool.Strings with
                | Some (_, len) ->
                    let label = sprintf "str_%d" idx
                    let totalSize = ((len + 16) + 7) &&& (~~~7)
                    Ok ([
                        ARM64.MOV_reg (ARM64.X15, ARM64.X28)
                        ARM64.ADD_imm (ARM64.X28, ARM64.X28, uint16 totalSize)
                        ARM64.ADRP (ARM64.X9, label)
                        ARM64.ADD_label (ARM64.X9, ARM64.X9, label)
                    ] @ loadImmediate ARM64.X10 (int64 len) @ [
                        ARM64.STR (ARM64.X10, ARM64.X15, 0s)
                        ARM64.MOVZ (ARM64.X0, 0us, 0)
                    ] @ loadImmediate ARM64.X11 (int64 len) @ [
                        ARM64.CMP_reg (ARM64.X0, ARM64.X11)
                        ARM64.B_cond (ARM64.GE, 7)
                        ARM64.LDRB (ARM64.X12, ARM64.X9, ARM64.X0)
                        ARM64.ADD_imm (ARM64.X13, ARM64.X15, 8us)
                        ARM64.ADD_reg (ARM64.X13, ARM64.X13, ARM64.X0)
                        ARM64.STRB_reg (ARM64.X12, ARM64.X13)
                        ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us)
                        ARM64.B (-7)
                        ARM64.ADD_imm (ARM64.X13, ARM64.X15, 8us)
                        ARM64.ADD_reg (ARM64.X13, ARM64.X13, ARM64.X10)
                        ARM64.MOVZ (ARM64.X12, 1us, 0)
                        ARM64.STR (ARM64.X12, ARM64.X13, 0s)
                    ] @ Runtime.generateFileReadText destReg ARM64.X15)
                | None -> Error $"String index {idx} not found in pool"
            | LIR.StackSlot offset ->
                loadStackSlot ARM64.X15 offset
                |> Result.map (fun loadInstrs ->
                    loadInstrs @ Runtime.generateFileReadText destReg ARM64.X15)
            | _ -> Error "FileReadText requires string operand")

    | LIR.FileExists (dest, path) ->
        // File exists check: generates syscall sequence to check file accessibility
        // Uses access/faccessat syscall to check if path exists
        // Path can be either a Reg (heap string pointer) or StringRef (pool string)
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            match path with
            | LIR.Reg pathReg ->
                // Already a heap string pointer
                lirRegToARM64Reg pathReg
                |> Result.map (fun pathARM64 ->
                    Runtime.generateFileExists destReg pathARM64)
            | LIR.StringRef idx ->
                // Pool string - convert to heap format first, then call FileExists
                // Heap format: [length:8][data:N][refcount:8]
                match Map.tryFind idx ctx.StringPool.Strings with
                | Some (_, len) ->
                    let label = sprintf "str_%d" idx
                    let totalSize = ((len + 16) + 7) &&& (~~~7)  // 8-byte aligned
                    // Use X15 as temp to hold heap string pointer
                    Ok ([
                        // Allocate heap space for converted string
                        ARM64.MOV_reg (ARM64.X15, ARM64.X28)  // X15 = heap pointer
                        ARM64.ADD_imm (ARM64.X28, ARM64.X28, uint16 totalSize)  // bump allocator
                        // Load pool string address into X9
                        ARM64.ADRP (ARM64.X9, label)
                        ARM64.ADD_label (ARM64.X9, ARM64.X9, label)
                        // Store length at [X15]
                    ] @ loadImmediate ARM64.X10 (int64 len) @ [
                        ARM64.STR (ARM64.X10, ARM64.X15, 0s)  // [X15] = length
                        // Copy bytes from pool string to heap
                        ARM64.MOVZ (ARM64.X0, 0us, 0)  // X0 = loop counter
                    ] @ loadImmediate ARM64.X11 (int64 len) @ [
                        // Copy loop
                        ARM64.CMP_reg (ARM64.X0, ARM64.X11)
                        ARM64.B_cond (ARM64.GE, 7)  // Exit if counter >= len
                        ARM64.LDRB (ARM64.X12, ARM64.X9, ARM64.X0)  // X12 = pool[X0]
                        ARM64.ADD_imm (ARM64.X13, ARM64.X15, 8us)  // X13 = X15 + 8
                        ARM64.ADD_reg (ARM64.X13, ARM64.X13, ARM64.X0)  // X13 = X15 + 8 + X0
                        ARM64.STRB_reg (ARM64.X12, ARM64.X13)  // [X13] = byte
                        ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us)  // X0++
                        ARM64.B (-7)  // Loop back
                        // Store refcount = 1 at [X15 + 8 + len]
                        ARM64.ADD_imm (ARM64.X13, ARM64.X15, 8us)
                        ARM64.ADD_reg (ARM64.X13, ARM64.X13, ARM64.X10)  // X13 = X15 + 8 + len
                        ARM64.MOVZ (ARM64.X12, 1us, 0)
                        ARM64.STR (ARM64.X12, ARM64.X13, 0s)  // [X13] = 1
                    ] @ Runtime.generateFileExists destReg ARM64.X15)
                | None -> Error $"String index {idx} not found in pool"
            | LIR.StackSlot offset ->
                // Load heap string from stack slot
                loadStackSlot ARM64.X15 offset
                |> Result.map (fun loadInstrs ->
                    loadInstrs @ Runtime.generateFileExists destReg ARM64.X15)
            | _ -> Error "FileExists requires string operand")

    | LIR.FileWriteText (dest, path, content) ->
        // File write: writes content string to file at path
        // Returns Result<Unit, String>
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            // Helper to get operand into a register
            let getOperandReg operand tempReg =
                match operand with
                | LIR.Reg reg ->
                    lirRegToARM64Reg reg |> Result.map (fun r -> ([], r))
                | LIR.StringRef idx ->
                    // Pool string - convert to heap format
                    match Map.tryFind idx ctx.StringPool.Strings with
                    | Some (_, len) ->
                        let label = sprintf "str_%d" idx
                        let totalSize = ((len + 16) + 7) &&& (~~~7)
                        Ok ([
                            ARM64.MOV_reg (tempReg, ARM64.X28)
                            ARM64.ADD_imm (ARM64.X28, ARM64.X28, uint16 totalSize)
                            ARM64.ADRP (ARM64.X9, label)
                            ARM64.ADD_label (ARM64.X9, ARM64.X9, label)
                        ] @ loadImmediate ARM64.X10 (int64 len) @ [
                            ARM64.STR (ARM64.X10, tempReg, 0s)
                            ARM64.MOVZ (ARM64.X0, 0us, 0)
                        ] @ loadImmediate ARM64.X11 (int64 len) @ [
                            ARM64.CMP_reg (ARM64.X0, ARM64.X11)
                            ARM64.B_cond (ARM64.GE, 7)
                            ARM64.LDRB (ARM64.X12, ARM64.X9, ARM64.X0)
                            ARM64.ADD_imm (ARM64.X13, tempReg, 8us)
                            ARM64.ADD_reg (ARM64.X13, ARM64.X13, ARM64.X0)
                            ARM64.STRB_reg (ARM64.X12, ARM64.X13)
                            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us)
                            ARM64.B (-7)  // Jump back to CMP
                            // Store refcount = 1 at [tempReg + 8 + len]
                            ARM64.ADD_imm (ARM64.X13, tempReg, 8us)
                            ARM64.ADD_reg (ARM64.X13, ARM64.X13, ARM64.X10)
                            ARM64.MOVZ (ARM64.X12, 1us, 0)
                            ARM64.STR (ARM64.X12, ARM64.X13, 0s)
                        ], tempReg)
                    | None -> Error $"String index {idx} not found in pool"
                | LIR.StackSlot offset ->
                    loadStackSlot tempReg offset |> Result.map (fun instrs -> (instrs, tempReg))
                | _ -> Error "FileWriteText requires string operands"

            getOperandReg path ARM64.X15
            |> Result.bind (fun (pathInstrs, pathReg) ->
                getOperandReg content ARM64.X14
                |> Result.map (fun (contentInstrs, contentReg) ->
                    pathInstrs @ contentInstrs @ Runtime.generateFileWriteText destReg pathReg contentReg false)))

    | LIR.FileAppendText (dest, path, content) ->
        // File append: appends content string to file at path
        // Returns Result<Unit, String>
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            // Same helper as FileWriteText
            let getOperandReg operand tempReg =
                match operand with
                | LIR.Reg reg ->
                    lirRegToARM64Reg reg |> Result.map (fun r -> ([], r))
                | LIR.StringRef idx ->
                    match Map.tryFind idx ctx.StringPool.Strings with
                    | Some (_, len) ->
                        let label = sprintf "str_%d" idx
                        let totalSize = ((len + 16) + 7) &&& (~~~7)
                        Ok ([
                            ARM64.MOV_reg (tempReg, ARM64.X28)
                            ARM64.ADD_imm (ARM64.X28, ARM64.X28, uint16 totalSize)
                            ARM64.ADRP (ARM64.X9, label)
                            ARM64.ADD_label (ARM64.X9, ARM64.X9, label)
                        ] @ loadImmediate ARM64.X10 (int64 len) @ [
                            ARM64.STR (ARM64.X10, tempReg, 0s)
                            ARM64.MOVZ (ARM64.X0, 0us, 0)
                        ] @ loadImmediate ARM64.X11 (int64 len) @ [
                            ARM64.CMP_reg (ARM64.X0, ARM64.X11)
                            ARM64.B_cond (ARM64.GE, 7)
                            ARM64.LDRB (ARM64.X12, ARM64.X9, ARM64.X0)
                            ARM64.ADD_imm (ARM64.X13, tempReg, 8us)
                            ARM64.ADD_reg (ARM64.X13, ARM64.X13, ARM64.X0)
                            ARM64.STRB_reg (ARM64.X12, ARM64.X13)
                            ARM64.ADD_imm (ARM64.X0, ARM64.X0, 1us)
                            ARM64.B (-7)  // Jump back to CMP
                            // Store refcount = 1 at [tempReg + 8 + len]
                            ARM64.ADD_imm (ARM64.X13, tempReg, 8us)
                            ARM64.ADD_reg (ARM64.X13, ARM64.X13, ARM64.X10)
                            ARM64.MOVZ (ARM64.X12, 1us, 0)
                            ARM64.STR (ARM64.X12, ARM64.X13, 0s)
                        ], tempReg)
                    | None -> Error $"String index {idx} not found in pool"
                | LIR.StackSlot offset ->
                    loadStackSlot tempReg offset |> Result.map (fun instrs -> (instrs, tempReg))
                | _ -> Error "FileAppendText requires string operands"

            getOperandReg path ARM64.X15
            |> Result.bind (fun (pathInstrs, pathReg) ->
                getOperandReg content ARM64.X14
                |> Result.map (fun (contentInstrs, contentReg) ->
                    pathInstrs @ contentInstrs @ Runtime.generateFileWriteText destReg pathReg contentReg true)))

/// Convert LIR terminator to ARM64 instructions
/// epilogueLabel: the label to jump to for function return (handles stack cleanup)
let convertTerminator (epilogueLabel: string) (terminator: LIR.Terminator) : Result<ARM64.Instr list, string> =
    match terminator with
    | LIR.Ret ->
        // Jump to function epilogue (handles stack cleanup and RET)
        Ok [ARM64.B_label epilogueLabel]

    | LIR.Branch (condReg, trueLabel, falseLabel) ->
        // Branch if register is non-zero (true), otherwise fall through to else
        // Use CBNZ (compare and branch if not zero) to true label
        // Then unconditional branch to false label
        lirRegToARM64Reg condReg
        |> Result.map (fun arm64Reg ->
            [
                ARM64.CBNZ (arm64Reg, trueLabel)  // If true, jump to then branch
                ARM64.B_label falseLabel           // Otherwise jump to else branch
            ])

    | LIR.Jump label ->
        Ok [ARM64.B_label label]

/// Convert LIR basic block to ARM64 instructions (with label)
/// epilogueLabel: passed through to terminator for Ret handling
let convertBlock (ctx: CodeGenContext) (epilogueLabel: string) (block: LIR.BasicBlock) : Result<ARM64.Instr list, string> =
    // Emit label for this block
    let labelInstr = ARM64.Label block.Label

    // Convert all instructions
    let instrResults = block.Instrs |> List.map (convertInstr ctx)

    // Convert terminator
    let termResult = convertTerminator epilogueLabel block.Terminator

    // Collect all results
    let rec collectResults acc results =
        match results with
        | [] -> Ok (List.rev acc)
        | (Ok instrs) :: rest -> collectResults (List.rev instrs @ acc) rest
        | (Error err) :: _ -> Error err

    match collectResults [] instrResults with
    | Error err -> Error err
    | Ok instrs ->
        match termResult with
        | Error err -> Error err
        | Ok termInstrs -> Ok (labelInstr :: instrs @ termInstrs)

/// Convert LIR CFG to ARM64 instructions
/// epilogueLabel: passed through to blocks for Ret handling
let convertCFG (ctx: CodeGenContext) (epilogueLabel: string) (cfg: LIR.CFG) : Result<ARM64.Instr list, string> =
    // Get blocks in a deterministic order (entry first, then sorted by label)
    let entryBlock =
        match Map.tryFind cfg.Entry cfg.Blocks with
        | Some block -> [block]
        | None -> []

    let otherBlocks =
        cfg.Blocks
        |> Map.toList
        |> List.filter (fun (label, _) -> label <> cfg.Entry)
        |> List.sortBy fst
        |> List.map snd

    let allBlocks = entryBlock @ otherBlocks

    // Convert each block
    allBlocks
    |> List.map (convertBlock ctx epilogueLabel)
    |> List.fold (fun acc result ->
        match acc, result with
        | Ok instrs, Ok newInstrs -> Ok (instrs @ newInstrs)
        | Error err, _ -> Error err
        | _, Error err -> Error err) (Ok [])

/// Generate heap initialization code for _start function
/// Reserves 64KB of stack space for heap allocations and initializes X27/X28
///
/// Memory layout:
///   X27 -> [free list heads: 256 bytes (32 entries × 8 bytes)]
///   X28 -> [heap allocation area: rest of 64KB]
///
/// Free list heads are indexed by (totalSize / 8), where totalSize includes
/// the 8-byte ref count. Size class 0 and 1 are unused (too small).
/// Size class 2 = 16 bytes, class 3 = 24 bytes, etc.
///
/// X27 is the base for free list heads (constant after init)
/// X28 is the bump pointer for new allocations
let generateHeapInit () : ARM64.Instr list =
    // Reserve 256 bytes for free list heads
    let freeListSize = 256
    [
        // Allocate 64KB (0x10000) of stack space for heap
        ARM64.MOVZ (ARM64.X28, 0us, 0)  // Start with 0
        ARM64.MOVK (ARM64.X28, 0x1us, 1)  // 0x10000 = 65536
        ARM64.SUB_reg (ARM64.SP, ARM64.SP, ARM64.X28)  // SP -= 64KB
        ARM64.MOV_reg (ARM64.X27, ARM64.SP)  // X27 = free list heads base
        // X28 = X27 + 256 (skip free list heads area)
        ARM64.ADD_imm (ARM64.X28, ARM64.X27, uint16 freeListSize)
        // Initialize free list heads to 0 (all empty)
        // We zero the first 256 bytes using a loop
        // For simplicity, just zero the common size classes (first 128 bytes = 16 entries)
        ARM64.MOVZ (ARM64.X15, 0us, 0)  // X15 = 0
        // Store zeros at each free list head slot
        // Size classes 2-17 (16 entries × 8 bytes = 128 bytes)
        ARM64.STR (ARM64.X15, ARM64.X27, 0s)
        ARM64.STR (ARM64.X15, ARM64.X27, 8s)
        ARM64.STR (ARM64.X15, ARM64.X27, 16s)
        ARM64.STR (ARM64.X15, ARM64.X27, 24s)
        ARM64.STR (ARM64.X15, ARM64.X27, 32s)
        ARM64.STR (ARM64.X15, ARM64.X27, 40s)
        ARM64.STR (ARM64.X15, ARM64.X27, 48s)
        ARM64.STR (ARM64.X15, ARM64.X27, 56s)
        ARM64.STR (ARM64.X15, ARM64.X27, 64s)
        ARM64.STR (ARM64.X15, ARM64.X27, 72s)
        ARM64.STR (ARM64.X15, ARM64.X27, 80s)
        ARM64.STR (ARM64.X15, ARM64.X27, 88s)
        ARM64.STR (ARM64.X15, ARM64.X27, 96s)
        ARM64.STR (ARM64.X15, ARM64.X27, 104s)
        ARM64.STR (ARM64.X15, ARM64.X27, 112s)
        ARM64.STR (ARM64.X15, ARM64.X27, 120s)
    ]

/// Convert LIR function to ARM64 instructions with prologue and epilogue
let convertFunction (ctx: CodeGenContext) (func: LIR.Function) : Result<ARM64.Instr list, string> =
    // Generate epilogue label for this function (passed to convertCFG for Ret terminators)
    let epilogueLabel = sprintf "_epilogue_%s" func.Name

    // Convert CFG to ARM64 instructions
    match convertCFG ctx epilogueLabel func.CFG with
    | Error err -> Error err
    | Ok cfgInstrs ->
        // Generate prologue (save FP/LR, allocate stack)
        let prologue = generatePrologue func.UsedCalleeSaved func.StackSize

        // Generate heap initialization for _start only
        let heapInit =
            if func.Name = "_start" then generateHeapInit ()
            else []

        // Generate parameter setup: move X0-X7 to allocated parameter registers
        // This must come AFTER the prologue but BEFORE the function body
        // Strategy: Save all source registers to temp regs first to avoid clobbering
        // Use X9, X10, X11, X12, X13, X14, X15 as temps
        let argRegs = [ARM64.X0; ARM64.X1; ARM64.X2; ARM64.X3; ARM64.X4; ARM64.X5; ARM64.X6; ARM64.X7]
        let tempRegs = [ARM64.X9; ARM64.X10; ARM64.X11; ARM64.X12; ARM64.X13; ARM64.X14; ARM64.X15]

        // Step 1: Save all calling convention registers to temps
        let saveToTemps =
            List.zip (List.take (List.length func.Params) argRegs) (List.take (List.length func.Params) tempRegs)
            |> List.map (fun (argReg, tempReg) -> ARM64.MOV_reg (tempReg, argReg))

        // Step 2: Move from temps to allocated parameter registers
        let moveFromTemps =
            List.zip func.Params (List.take (List.length func.Params) tempRegs)
            |> List.map (fun (paramReg, tempReg) ->
                match lirRegToARM64Reg paramReg with
                | Ok paramArm64 ->
                    if paramArm64 = tempReg then
                        []  // Already in the right place
                    else
                        [ARM64.MOV_reg (paramArm64, tempReg)]
                | Error _ -> [])  // Shouldn't happen
            |> List.concat

        let paramSetup = saveToTemps @ moveFromTemps

        // Generate epilogue (deallocate stack, restore FP/LR, return or exit)
        let epilogueLabelInstr = [ARM64.Label (sprintf "_epilogue_%s" func.Name)]
        let epilogue =
            if func.Name = "_start" then
                // For _start, exit instead of return
                generateEpilogue func.UsedCalleeSaved func.StackSize
                |> List.filter (function ARM64.RET -> false | _ -> true)  // Remove RET
                |> fun instrs -> instrs @ Runtime.generateExit ()  // Add Exit syscall
            else
                generateEpilogue func.UsedCalleeSaved func.StackSize

        // Add function entry label (for BL to branch to)
        let functionEntryLabel = [ARM64.Label func.Name]

        // Combine: function label + prologue + heap init + param setup + CFG body + epilogue label + epilogue
        // All Ret terminators jump to the epilogue label
        Ok (functionEntryLabel @ prologue @ heapInit @ paramSetup @ cfgInstrs @ epilogueLabelInstr @ epilogue)

/// Convert LIR program to ARM64 instructions with options
let generateARM64WithOptions (options: CodeGenOptions) (program: LIR.Program) : Result<ARM64.Instr list, string> =
    let (LIR.Program (functions, stringPool, _floatPool)) = program

    // Create code generation context with options and string pool
    let ctx = { Options = options; StringPool = stringPool }

    // Ensure _start is first (entry point)
    let sortedFunctions =
        match List.tryFind (fun (f: LIR.Function) -> f.Name = "_start") functions with
        | Some startFunc ->
            let otherFuncs = List.filter (fun (f: LIR.Function) -> f.Name <> "_start") functions
            startFunc :: otherFuncs
        | None -> functions  // No _start, keep original order

    sortedFunctions
    |> List.map (convertFunction ctx)
    |> List.fold (fun acc result ->
        match acc, result with
        | Ok instrs, Ok newInstrs -> Ok (instrs @ newInstrs)
        | Error err, _ -> Error err
        | _, Error err -> Error err) (Ok [])

/// Convert LIR program to ARM64 instructions (uses default options)
let generateARM64 (program: LIR.Program) : Result<ARM64.Instr list, string> =
    generateARM64WithOptions defaultOptions program
