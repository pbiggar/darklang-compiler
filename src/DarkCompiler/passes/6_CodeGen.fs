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

/// Convert LIR.FReg to ARM64.FReg (assumes physical registers only)
let lirFRegToARM64FReg (freg: LIR.FReg) : Result<ARM64.FReg, string> =
    match freg with
    | LIR.FPhysical physReg -> Ok (lirPhysFPRegToARM64FReg physReg)
    | LIR.FVirtual vreg -> Error $"Virtual FP register {vreg} should have been allocated"

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

    saveFpLr @ setFp @ allocCalleeSaved @ saveCalleeSavedInstrs @ allocStack

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

    deallocStack @ restoreCalleeSavedInstrs @ deallocCalleeSaved @ restoreFpLr @ ret

/// Convert LIR instruction to ARM64 instructions
let convertInstr (instr: LIR.Instr) : Result<ARM64.Instr list, string> =
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
            | LIR.StringRef _ ->
                Error "Cannot MOV string reference - use PrintString instruction"
            | LIR.FloatRef _ ->
                Error "Cannot MOV float reference - use FLoad instruction")

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
                    Error "Cannot use float reference in integer arithmetic"))

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
                    Error "Cannot use float reference in integer arithmetic"))


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
                Error "Cannot compare float references directly - use FCmp")

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

    | LIR.Mvn (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64.MVN (destReg, srcReg)]))

    | LIR.PrintBool reg ->
        // Print booleans as "true" or "false"
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            if regARM64 <> ARM64.X0 then
                [ARM64.MOV_reg (ARM64.X0, regARM64)] @ Runtime.generatePrintBool ()
            else
                Runtime.generatePrintBool ())

    | LIR.Call (dest, funcName, args) ->
        // Function call: arguments already moved to X0-X7 by preceding MOVs
        // Caller-save is handled by SaveRegs/RestoreRegs instructions
        Ok [ARM64.BL funcName]

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
        // Value to print should be in X0
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            if regARM64 <> ARM64.X0 then
                // Move to X0 if not already there
                [ARM64.MOV_reg (ARM64.X0, regARM64)] @ Runtime.generatePrintInt ()
            else
                Runtime.generatePrintInt ())

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
        // String labels are named "_str0", "_str1", etc.
        let stringLabel = sprintf "_str%d" idx
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
        // Inline bump allocator using X28 as heap pointer
        // X28 is reserved for heap allocation (initialized in _start prologue)
        //
        // Memory layout with reference counting:
        //   [payload: sizeBytes][refcount: 8 bytes]
        //
        // The ref count is placed AFTER the payload to avoid modifying
        // the returned pointer, which caused issues with spilling.
        //
        // Algorithm:
        // 1. Save current heap pointer to dest (payload starts at X28)
        // 2. Store ref count (=1) at X28 + sizeBytes
        // 3. Bump X28 by (sizeBytes + 8), aligned to 8 bytes
        lirRegToARM64Reg dest
        |> Result.map (fun destReg ->
            // Total size includes 8 bytes for ref count, aligned to 8 bytes
            let totalSize = ((sizeBytes + 8) + 7) &&& (~~~7)
            [
                ARM64.MOV_reg (destReg, ARM64.X28)  // dest = current heap pointer (payload)
                ARM64.MOVZ (ARM64.X15, 1us, 0)  // X15 = 1 (initial ref count)
                ARM64.STR (ARM64.X15, ARM64.X28, int16 sizeBytes)  // store ref count after payload
                ARM64.ADD_imm (ARM64.X28, ARM64.X28, uint16 totalSize)  // bump pointer
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
            | _ -> Error "Unsupported operand type in HeapStore")

    | LIR.HeapLoad (dest, addr, offset) ->
        // Load value from addr + offset (offset is in bytes)
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg addr
            |> Result.map (fun addrReg ->
                [ARM64.LDR (destReg, addrReg, int16 offset)]))

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
let convertBlock (epilogueLabel: string) (block: LIR.BasicBlock) : Result<ARM64.Instr list, string> =
    // Emit label for this block
    let labelInstr = ARM64.Label block.Label

    // Convert all instructions
    let instrResults = block.Instrs |> List.map convertInstr

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
let convertCFG (epilogueLabel: string) (cfg: LIR.CFG) : Result<ARM64.Instr list, string> =
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
    |> List.map (convertBlock epilogueLabel)
    |> List.fold (fun acc result ->
        match acc, result with
        | Ok instrs, Ok newInstrs -> Ok (instrs @ newInstrs)
        | Error err, _ -> Error err
        | _, Error err -> Error err) (Ok [])

/// Generate heap initialization code for _start function
/// Reserves 64KB of stack space for heap allocations and initializes X28
let generateHeapInit () : ARM64.Instr list =
    [
        // Allocate 64KB (0x10000) of stack space for heap
        // SUB SP, SP, #0x10000 (in chunks since immediate is 12-bit max)
        ARM64.MOVZ (ARM64.X28, 0us, 0)  // Start with 0
        ARM64.MOVK (ARM64.X28, 0x1us, 1)  // 0x10000 = 65536
        ARM64.SUB_reg (ARM64.SP, ARM64.SP, ARM64.X28)  // SP -= 64KB
        ARM64.MOV_reg (ARM64.X28, ARM64.SP)  // X28 = heap base (bottom of reserved area)
    ]

/// Convert LIR function to ARM64 instructions with prologue and epilogue
let convertFunction (func: LIR.Function) : Result<ARM64.Instr list, string> =
    // Generate epilogue label for this function (passed to convertCFG for Ret terminators)
    let epilogueLabel = sprintf "_epilogue_%s" func.Name

    // Convert CFG to ARM64 instructions
    match convertCFG epilogueLabel func.CFG with
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

        // Generate epilogue (deallocate stack, restore FP/LR, return)
        let epilogueLabel = [ARM64.Label (sprintf "_epilogue_%s" func.Name)]
        let epilogue = generateEpilogue func.UsedCalleeSaved func.StackSize

        // Add function entry label (for BL to branch to)
        let functionEntryLabel = [ARM64.Label func.Name]

        // Combine: function label + prologue + heap init + param setup + CFG body + epilogue label + epilogue
        // All Ret terminators jump to the epilogue label
        Ok (functionEntryLabel @ prologue @ heapInit @ paramSetup @ cfgInstrs @ epilogueLabel @ epilogue)

/// Convert LIR program to ARM64 instructions
let generateARM64 (program: LIR.Program) : Result<ARM64.Instr list, string> =
    let (LIR.Program (functions, _stringPool, _floatPool)) = program
    // Ensure _start is first (entry point)
    let sortedFunctions =
        match List.tryFind (fun (f: LIR.Function) -> f.Name = "_start") functions with
        | Some startFunc ->
            let otherFuncs = List.filter (fun (f: LIR.Function) -> f.Name <> "_start") functions
            startFunc :: otherFuncs
        | None -> functions  // No _start, keep original order

    sortedFunctions
    |> List.map convertFunction
    |> List.fold (fun acc result ->
        match acc, result with
        | Ok instrs, Ok newInstrs -> Ok (instrs @ newInstrs)
        | Error err, _ -> Error err
        | _, Error err -> Error err) (Ok [])
