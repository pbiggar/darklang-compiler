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
    /// Enable coverage instrumentation
    EnableCoverage: bool
    /// Number of coverage expressions (determines buffer size)
    CoverageExprCount: int
}

/// Default code generation options
let defaultOptions : CodeGenOptions = {
    DisableFreeList = false
    EnableCoverage = false
    CoverageExprCount = 0
}

/// Code generation context (passed through to instruction conversion)
type CodeGenContext = {
    Options: CodeGenOptions
    StringPool: MIR.StringPool
    // Function context for tail call epilogue generation
    StackSize: int
    UsedCalleeSaved: LIR.PhysReg list
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
    | LIR.X16 -> ARM64.X16
    | LIR.X17 -> ARM64.X17
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
/// For FVirtual, we use a two-tier allocation scheme to avoid collisions:
/// - FVirtual 1000 -> D18 (left operand temp for binary ops)
/// - FVirtual 1001 -> D17 (right operand temp for binary ops)
/// - FVirtual 3000-3007 -> D14-D15 (temps for float call args)
/// - FVirtual 0-7 -> D2-D9 (dedicated 1:1 mapping for parameters)
/// - FVirtual 8+ -> D10-D13 (4 temps with modulo, for SSA temps and locals)
///
/// The two-tier scheme ensures that parameter VRegs (0-7) never collide with
/// SSA-generated temps (which have high IDs like 12001). Parameters get D2-D9,
/// while temps get D10-D13 with modulo 4.
let lirFRegToARM64FReg (freg: LIR.FReg) : Result<ARM64.FReg, string> =
    match freg with
    | LIR.FPhysical physReg -> Ok (lirPhysFPRegToARM64FReg physReg)
    // Special temp registers for specific purposes
    | LIR.FVirtual 1000 -> Ok ARM64.D18  // Left temp for binary ops
    | LIR.FVirtual 1001 -> Ok ARM64.D17  // Right temp for binary ops
    | LIR.FVirtual 2000 -> Ok ARM64.D16  // Temp for FPhi cycle resolution
    | LIR.FVirtual n when n >= 3000 && n < 4000 ->
        // Temps for float call arguments - use D19-D26 (8 registers)
        // These must not collide with each other since up to 8 floats
        // can be loaded before FArgMoves. Using D19-D26 avoids collision
        // with argument regs D0-D7, parameter VRegs D2-D9, SSA temps D10-D13,
        // and binary op temps D17-D18.
        let tempIdx = (n - 3000) % 8
        match tempIdx with
        | 0 -> Ok ARM64.D19
        | 1 -> Ok ARM64.D20
        | 2 -> Ok ARM64.D21
        | 3 -> Ok ARM64.D22
        | 4 -> Ok ARM64.D23
        | 5 -> Ok ARM64.D24
        | 6 -> Ok ARM64.D25
        | _ -> Ok ARM64.D26
    | LIR.FVirtual n when n >= 0 && n <= 7 ->
        // Parameters (VRegs 0-7) get dedicated D2-D9 mapping
        // This prevents collisions with SSA-generated temps
        let physReg =
            match n with
            | 0 -> ARM64.D2
            | 1 -> ARM64.D3
            | 2 -> ARM64.D4
            | 3 -> ARM64.D5
            | 4 -> ARM64.D6
            | 5 -> ARM64.D7
            | 6 -> ARM64.D8
            | _ -> ARM64.D9
        Ok physReg
    | LIR.FVirtual n ->
        // SSA temps and other high-numbered VRegs
        // Available: D0, D1 (caller-saved), D10-D15 (not used elsewhere), D27-D31 (caller-saved)
        // Must avoid: D2-D9 (params), D16 (cycle temp), D17-D18 (binop temps), D19-D26 (arg temps)
        // Using 13 registers to avoid collisions between phi sources and destinations
        // (With only 8 registers, modulo collisions caused phi source/dest to share registers)
        let tempRegs = [| ARM64.D0; ARM64.D1; ARM64.D10; ARM64.D11; ARM64.D12; ARM64.D13; ARM64.D14; ARM64.D15;
                          ARM64.D27; ARM64.D28; ARM64.D29; ARM64.D30; ARM64.D31 |]
        let regIdx = n % tempRegs.Length
        let physReg = tempRegs.[regIdx]
        Ok physReg

/// Convert LIR.Reg to ARM64.Reg (assumes physical registers only)
let lirRegToARM64Reg (reg: LIR.Reg) : Result<ARM64.Reg, string> =
    match reg with
    | LIR.Physical physReg -> Ok (lirPhysRegToARM64Reg physReg)
    | LIR.Virtual vreg -> Error $"Virtual register {vreg} should have been allocated"

/// Convert LIR.Reg (Virtual) to LIR.FReg (FVirtual) for float HeapStore
/// This is used when a float value is stored via HeapStore - the register
/// ID is shared between Virtual and FVirtual address spaces
let virtualToFVirtual (reg: LIR.Reg) : LIR.FReg =
    match reg with
    | LIR.Virtual n -> LIR.FVirtual n
    | LIR.Physical p -> LIR.FPhysical (
        // Map GP physical registers to FP physical registers for edge cases
        match p with
        | LIR.X0 -> LIR.D0 | LIR.X1 -> LIR.D1 | LIR.X2 -> LIR.D2 | LIR.X3 -> LIR.D3
        | LIR.X4 -> LIR.D4 | LIR.X5 -> LIR.D5 | LIR.X6 -> LIR.D6 | LIR.X7 -> LIR.D7
        | _ -> LIR.D15)

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
/// Uses LDUR for small offsets (-256 to +255), computes address for larger offsets
let loadStackSlot (dest: ARM64.Reg) (offset: int) : Result<ARM64.Instr list, string> =
    if offset >= -256 && offset <= 255 then
        // Small offset: use LDUR directly
        Ok [ARM64.LDUR (dest, ARM64.X29, int16 offset)]
    elif offset < 0 && -offset <= 4095 then
        // Larger negative offset: compute address into X10, then load
        // X10 = X29 - (-offset), then LDR dest, [X10, #0]
        Ok [
            ARM64.SUB_imm (ARM64.X10, ARM64.X29, uint16 (-offset))
            ARM64.LDR (dest, ARM64.X10, 0s)
        ]
    elif offset > 0 && offset <= 4095 then
        // Larger positive offset: compute address into X10, then load
        Ok [
            ARM64.ADD_imm (ARM64.X10, ARM64.X29, uint16 offset)
            ARM64.LDR (dest, ARM64.X10, 0s)
        ]
    else
        Error $"Stack offset {offset} exceeds supported range (-4095 to +4095)"

/// Generate ARM64 instructions to store a register to a stack slot
/// Stack slots are accessed relative to FP (X29)
/// Uses STUR for small offsets (-256 to +255), computes address for larger offsets
let storeStackSlot (src: ARM64.Reg) (offset: int) : Result<ARM64.Instr list, string> =
    if offset >= -256 && offset <= 255 then
        // Small offset: use STUR directly
        Ok [ARM64.STUR (src, ARM64.X29, int16 offset)]
    elif offset < 0 && -offset <= 4095 then
        // Larger negative offset: compute address into X10, then store
        // X10 = X29 - (-offset), then STR src, [X10, #0]
        Ok [
            ARM64.SUB_imm (ARM64.X10, ARM64.X29, uint16 (-offset))
            ARM64.STR (src, ARM64.X10, 0s)
        ]
    elif offset > 0 && offset <= 4095 then
        // Larger positive offset: compute address into X10, then store
        Ok [
            ARM64.ADD_imm (ARM64.X10, ARM64.X29, uint16 offset)
            ARM64.STR (src, ARM64.X10, 0s)
        ]
    else
        Error $"Stack offset {offset} exceeds supported range (-4095 to +4095)"

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
    // 1. Save FP (X29) and LR (X30) with pre-indexed addressing (combines SUB and STP)
    // 2. Set FP = SP: MOV X29, SP
    // 3. Allocate stack space for spills and callee-saved registers
    // 4. Save callee-saved registers

    // Use pre-indexed STP to save FP/LR and decrement SP in one instruction
    let saveFpLr = [ARM64.STP_pre (ARM64.X29, ARM64.X30, ARM64.SP, -16s)]
    let setFp = [ARM64.MOV_reg (ARM64.X29, ARM64.SP)]

    // Calculate total additional stack space needed
    let calleeSavedSpace = calleeSavedStackSpace usedCalleeSaved
    let totalExtraStack = stackSize + calleeSavedSpace

    // Allocate all stack space at once (for spills + callee-saved)
    let allocStack =
        if totalExtraStack > 0 then
            [ARM64.SUB_imm (ARM64.SP, ARM64.SP, uint16 totalExtraStack)]
        else
            []

    // Save callee-saved registers at [SP]
    // (callee-saved are at the bottom of the frame, spill space is above them)
    let (saveCalleeSavedInstrs, _) = generateCalleeSavedSaves usedCalleeSaved

    saveFpLr @ setFp @ allocStack @ saveCalleeSavedInstrs

/// Generate function epilogue
/// Restores callee-saved registers, FP, LR, and returns
let generateEpilogue (usedCalleeSaved: LIR.PhysReg list) (stackSize: int) : ARM64.Instr list =
    // Epilogue sequence (reverse of prologue):
    // 1. Restore callee-saved registers from [SP + stackSize]
    // 2. Deallocate stack space (spills + callee-saved) at once
    // 3. Restore FP and LR with post-indexed addressing (combines LDP and ADD)
    // 4. Return: RET

    // Restore callee-saved registers from [SP]
    // (callee-saved are at the bottom of the frame, spill space is above them)
    let calleeSavedSpace = calleeSavedStackSpace usedCalleeSaved
    let restoreCalleeSavedInstrs = generateCalleeSavedRestores usedCalleeSaved

    // Deallocate all stack space at once
    let totalExtraStack = stackSize + calleeSavedSpace
    let deallocStack =
        if totalExtraStack > 0 then
            [ARM64.ADD_imm (ARM64.SP, ARM64.SP, uint16 totalExtraStack)]
        else
            []

    // Use post-indexed LDP to restore FP/LR and increment SP in one instruction
    let restoreFpLr = [ARM64.LDP_post (ARM64.X29, ARM64.X30, ARM64.SP, 16s)]
    let ret = [ARM64.RET]

    restoreCalleeSavedInstrs @ deallocStack @ restoreFpLr @ ret

/// Convert LIR instruction to ARM64 instructions
let convertInstr (ctx: CodeGenContext) (instr: LIR.Instr) : Result<ARM64.Instr list, string> =
    match instr with
    | LIR.Phi _ ->
        // Phi nodes should be eliminated before code generation (by register allocation)
        Error "Phi nodes should be eliminated before code generation"

    | LIR.FPhi _ ->
        // Float phi nodes should be eliminated before code generation (by register allocation)
        Error "Float phi nodes should be eliminated before code generation"

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
                |> Result.map (fun srcARM64 ->
                    // Skip self-moves (can happen after register allocation coalesces VRegs)
                    if destReg = srcARM64 then []
                    else [ARM64.MOV_reg (destReg, srcARM64)])
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
                    let label = "str_" + string idx
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
                        // Store refcount at aligned offset
                        // aligned(x) = ((x + 7) >> 3) << 3
                        ARM64.ADD_imm (ARM64.X12, ARM64.X10, 7us)        // X12 = len + 7
                        ARM64.MOVZ (ARM64.X15, 3us, 0)                   // X15 = 3
                        ARM64.LSR_reg (ARM64.X12, ARM64.X12, ARM64.X15)  // X12 = (len + 7) >> 3
                        ARM64.LSL_reg (ARM64.X12, ARM64.X12, ARM64.X15)  // X12 = aligned(len)
                        ARM64.ADD_imm (ARM64.X15, destReg, 8us)          // X15 = dest + 8
                        ARM64.ADD_reg (ARM64.X12, ARM64.X15, ARM64.X12)  // X12 = dest + 8 + aligned(len)
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

    // Sign/zero extension for integer overflow truncation
    | LIR.Sxtb (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64.SXTB (destReg, srcReg)]))

    | LIR.Sxth (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64.SXTH (destReg, srcReg)]))

    | LIR.Sxtw (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64.SXTW (destReg, srcReg)]))

    | LIR.Uxtb (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64.UXTB (destReg, srcReg)]))

    | LIR.Uxth (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64.UXTH (destReg, srcReg)]))

    | LIR.Uxtw (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64.UXTW (destReg, srcReg)]))

    | LIR.PrintBool reg ->
        // Print booleans as "true" or "false" (no exit)
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            if regARM64 <> ARM64.X0 then
                [ARM64.MOV_reg (ARM64.X0, regARM64)] @ Runtime.generatePrintBoolNoExit ()
            else
                Runtime.generatePrintBoolNoExit ())

    | LIR.PrintChars chars ->
        // Print literal characters (for tuple/list delimiters like "(", ", ", ")")
        Ok (Runtime.generatePrintChars chars)

    | LIR.PrintIntNoNewline reg ->
        // Print integer without newline (for tuple elements)
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            if regARM64 <> ARM64.X0 then
                [ARM64.MOV_reg (ARM64.X0, regARM64)] @ Runtime.generatePrintIntNoNewline ()
            else
                Runtime.generatePrintIntNoNewline ())

    | LIR.PrintBoolNoNewline reg ->
        // Print boolean without newline (for tuple elements)
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            if regARM64 <> ARM64.X0 then
                [ARM64.MOV_reg (ARM64.X0, regARM64)] @ Runtime.generatePrintBoolNoNewline ()
            else
                Runtime.generatePrintBoolNoNewline ())

    | LIR.PrintFloatNoNewline freg ->
        // Print float without newline (for tuple/list elements)
        lirFRegToARM64FReg freg
        |> Result.map (fun fregARM64 ->
            if fregARM64 <> ARM64.D0 then
                [ARM64.FMOV_reg (ARM64.D0, fregARM64)] @ Runtime.generatePrintFloatNoNewline ()
            else
                Runtime.generatePrintFloatNoNewline ())

    | LIR.PrintHeapStringNoNewline reg ->
        // Print heap string without newline (for tuple/list elements)
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            // Heap string layout: [len:8 bytes][data:N bytes]
            let loadInstrs = [ARM64.LDR (ARM64.X10, regARM64, 0s); ARM64.ADD_imm (ARM64.X9, regARM64, 8us)]
            let loadAndPrint = loadInstrs @ Runtime.generatePrintStringNoNewline ()
            if regARM64 <> ARM64.X9 then
                loadAndPrint
            else
                // Need to save the original address first
                let saveReg = [ARM64.MOV_reg (ARM64.X11, regARM64)]
                let loadFromSaved = [ARM64.LDR (ARM64.X10, ARM64.X11, 0s); ARM64.ADD_imm (ARM64.X9, ARM64.X11, 8us)]
                saveReg @ loadFromSaved @ Runtime.generatePrintStringNoNewline ())

    | LIR.PrintList (listPtr, elemType) ->
        // Print list as [elem1, elem2, ...]
        // List layout: Nil = 0, Cons = [tag=1, head, tail]
        // Uses X19 for list pointer (callee-saved), X20 for first flag
        lirRegToARM64Reg listPtr
        |> Result.map (fun listReg ->
            let os =
                match Platform.detectOS () with
                | Ok platform -> platform
                | Error _ -> Platform.Linux
            let syscalls = Platform.getSyscallNumbers os

            // Generate element print code based on type (uses X0 for value)
            let elemPrintCode =
                match elemType with
                | AST.TInt64 -> Runtime.generatePrintIntNoNewline ()
                | AST.TBool -> Runtime.generatePrintBoolNoNewline ()
                | AST.TFloat64 ->
                    // Need to move from X0 to D0 for float
                    [ARM64.FMOV_from_gp (ARM64.D0, ARM64.X0)] @ Runtime.generatePrintFloatNoNewline ()
                | AST.TString ->
                    // X0 has string address, load len/data and print
                    [ARM64.LDR (ARM64.X10, ARM64.X0, 0s); ARM64.ADD_imm (ARM64.X9, ARM64.X0, 8us)] @
                    Runtime.generatePrintStringNoNewline ()
                | _ ->
                    // For other types (nested lists, etc.), print as integer for now
                    Runtime.generatePrintIntNoNewline ()

            let elemPrintLen = List.length elemPrintCode

            // Print "[" - 9 instructions
            let printOpenBracket = [
                ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us);
                ARM64.MOVZ (ARM64.X0, uint16 (byte '['), 0);
                ARM64.STRB (ARM64.X0, ARM64.SP, 0);
                ARM64.MOVZ (ARM64.X0, 1us, 0);          // fd = stdout
                ARM64.MOV_reg (ARM64.X1, ARM64.SP);    // buffer
                ARM64.MOVZ (ARM64.X2, 1us, 0);         // len = 1
                ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0);
                ARM64.SVC syscalls.SvcImmediate;
                ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ]

            // Setup: X19 = list pointer, X20 = 1 (first element flag)
            let setup = [ARM64.MOV_reg (ARM64.X19, listReg); ARM64.MOVZ (ARM64.X20, 1us, 0)]

            // Print ", " - used inside loop when not first element
            let printCommaSpace = [
                ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us);
                ARM64.MOVZ (ARM64.X0, uint16 (byte ','), 0);
                ARM64.STRB (ARM64.X0, ARM64.SP, 0);
                ARM64.MOVZ (ARM64.X0, uint16 (byte ' '), 0);
                ARM64.STRB (ARM64.X0, ARM64.SP, 1);
                ARM64.MOVZ (ARM64.X0, 1us, 0);
                ARM64.MOV_reg (ARM64.X1, ARM64.SP);
                ARM64.MOVZ (ARM64.X2, 2us, 0);
                ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0);
                ARM64.SVC syscalls.SvcImmediate;
                ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ]
            let commaLen = List.length printCommaSpace

            // Loop structure:
            // loop_start:
            //   CBZ X19, loop_end           // if list == nil, exit
            //   CBNZ X20, skip_comma        // if first, skip comma
            //   <print ", ">
            // skip_comma:
            //   MOV X20, 0                  // first = false
            //   LDR X0, [X19, #8]           // X0 = head
            //   <print element>
            //   LDR X19, [X19, #16]         // X19 = tail
            //   B loop_start
            // loop_end:
            //   <print "]">

            // Calculate branch offsets
            // loopBodyLen = instructions after CBZ = CBNZ(1) + comma(11) + skipComma(2) + element(N) + loopEnd(2)
            let loopBodyLen = 1 + commaLen + 2 + elemPrintLen + 2
            // CBZ skips to loop_end (after B), which is at index loopBodyLen+1 (since CBZ is at index 0)
            let cbzOffset = loopBodyLen + 1
            // CBNZ skips commaLen instructions to reach skipComma
            let skipCommaOffset = commaLen

            let loopStart = [ARM64.CBZ_offset (ARM64.X19, cbzOffset); ARM64.CBNZ_offset (ARM64.X20, skipCommaOffset)]
            let skipComma = [ARM64.MOVZ (ARM64.X20, 0us, 0); ARM64.LDR (ARM64.X0, ARM64.X19, 8s)]
            // B is at index loopBodyLen, jump back to CBZ at index 0
            let loopEnd = [ARM64.LDR (ARM64.X19, ARM64.X19, 16s); ARM64.B (-loopBodyLen)]
            let loopCode = loopStart @ printCommaSpace @ skipComma @ elemPrintCode @ loopEnd

            // Print "]\n"
            let printCloseBracketNewline = [
                ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us);
                ARM64.MOVZ (ARM64.X0, uint16 (byte ']'), 0);
                ARM64.STRB (ARM64.X0, ARM64.SP, 0);
                ARM64.MOVZ (ARM64.X0, uint16 (byte '\n'), 0);
                ARM64.STRB (ARM64.X0, ARM64.SP, 1);
                ARM64.MOVZ (ARM64.X0, 1us, 0);
                ARM64.MOV_reg (ARM64.X1, ARM64.SP);
                ARM64.MOVZ (ARM64.X2, 2us, 0);
                ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0);
                ARM64.SVC syscalls.SvcImmediate;
                ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)
            ]

            printOpenBracket @ setup @ loopCode @ printCloseBracketNewline)

    | LIR.PrintSum (sumPtr, variants) ->
        // Print sum type: variant name + optional payload + newline
        // Sum layout depends on whether ANY variant has a payload:
        // - If any payload: [tag, payload] on heap
        // - If all nullary: just the tag value (integer)
        lirRegToARM64Reg sumPtr
        |> Result.map (fun sumReg ->
            let os =
                match Platform.detectOS () with
                | Ok platform -> platform
                | Error _ -> Platform.Linux
            let syscalls = Platform.getSyscallNumbers os

            // Check if any variant has a payload
            let hasAnyPayload = variants |> List.exists (fun (_, _, payload) -> Option.isSome payload)

            // Helper: generate code to print a string literal
            let printLiteral (s: string) =
                let bytes = System.Text.Encoding.UTF8.GetBytes(s)
                if bytes.Length = 0 then []
                else
                    let alignedSize = max 16 ((bytes.Length + 15) &&& ~~~15)
                    [ARM64.SUB_imm (ARM64.SP, ARM64.SP, uint16 alignedSize)] @
                    (bytes |> Array.toList |> List.mapi (fun i b ->
                        [ARM64.MOVZ (ARM64.X0, uint16 b, 0); ARM64.STRB (ARM64.X0, ARM64.SP, i)]
                    ) |> List.concat) @
                    [ARM64.MOVZ (ARM64.X0, 1us, 0);
                     ARM64.MOV_reg (ARM64.X1, ARM64.SP);
                     ARM64.MOVZ (ARM64.X2, uint16 bytes.Length, 0);
                     ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0);
                     ARM64.SVC syscalls.SvcImmediate;
                     ARM64.ADD_imm (ARM64.SP, ARM64.SP, uint16 alignedSize)]

            // Setup depends on representation
            let setup =
                if hasAnyPayload then
                    // Heap-allocated: X19 = sum pointer, load tag from [X19, 0] into X20
                    [ARM64.MOV_reg (ARM64.X19, sumReg); ARM64.LDR (ARM64.X20, ARM64.X19, 0s)]
                else
                    // All nullary: X19 = sum pointer (for consistency), X20 = tag (the value itself)
                    [ARM64.MOV_reg (ARM64.X19, sumReg); ARM64.MOV_reg (ARM64.X20, sumReg)]

            // Generate code for each variant: compare tag, branch, print name, optionally print payload
            // Structure: for each variant, generate:
            //   CMP X20, #tag
            //   B.NE next_variant
            //   <print variant name>
            //   <if payload: print "(", print payload, print ")">
            //   B end
            // next_variant:
            //   ... (repeat)
            // end:
            //   <print "\n">

            // Pre-calculate code blocks for each variant
            let variantBlocks =
                variants |> List.map (fun (variantName, _tag, payloadType) ->
                    let printName = printLiteral variantName
                    let printPayload =
                        match payloadType with
                        | None -> []
                        | Some pType ->
                            let printOpen = printLiteral "("
                            let loadPayload = [ARM64.LDR (ARM64.X0, ARM64.X19, 8s)]  // Load payload from offset 8
                            let printPayloadValue =
                                match pType with
                                | AST.TInt64 -> Runtime.generatePrintIntNoNewline ()
                                | AST.TBool -> Runtime.generatePrintBoolNoNewline ()
                                | AST.TFloat64 ->
                                    [ARM64.FMOV_from_gp (ARM64.D0, ARM64.X0)] @ Runtime.generatePrintFloatNoNewline ()
                                | AST.TString ->
                                    [ARM64.LDR (ARM64.X10, ARM64.X0, 0s); ARM64.ADD_imm (ARM64.X9, ARM64.X0, 8us)] @
                                    Runtime.generatePrintStringNoNewline ()
                                | t -> failwith $"Unsupported payload type in sum variant: {t}"
                            let printClose = printLiteral ")"
                            printOpen @ loadPayload @ printPayloadValue @ printClose
                    (printName, printPayload))

            // Calculate end label offset from each variant block
            // We'll build the code and calculate offsets manually

            // Print newline at end
            let printNewline = printLiteral "\n"
            let endBlockLen = List.length printNewline

            // Build variant blocks with branching
            // For each variant: CMP(1) + B.NE(1) + name + payload + B(1) to end
            let mutable codeBlocks : ARM64.Instr list list = []
            let mutable cumulativeOffset = 0

            // First pass: calculate total length to know where "end" is
            let blockLengths =
                variants
                |> List.mapi (fun i (_, _tag, _) ->
                    let (printName, printPayload) = variantBlocks.[i]
                    2 + List.length printName + List.length printPayload + 1)  // CMP + B.NE + name + payload + B

            let totalVariantCodeLen = List.sum blockLengths
            let endOffset = totalVariantCodeLen + endBlockLen

            // Second pass: build actual code with correct offsets
            let mutable currentPos = 0
            let variantCode =
                variants
                |> List.mapi (fun i (_, tag, _) ->
                    let (printName, printPayload) = variantBlocks.[i]
                    let blockLen = 2 + List.length printName + List.length printPayload + 1
                    // B.NE is at position 1, next block CMP is at position blockLen
                    // So offset = blockLen - 1 (forward jump from B.NE to next CMP)
                    let nextBlockOffset = blockLen - 1
                    let endFromHere = totalVariantCodeLen - currentPos - blockLen + 1  // Jump to after all variant blocks
                    currentPos <- currentPos + blockLen

                    let cmpInstr = ARM64.CMP_imm (ARM64.X20, uint16 tag)
                    let branchNeInstr = ARM64.B_cond (ARM64.NE, nextBlockOffset)  // Skip this variant's code
                    let branchEndInstr = ARM64.B endFromHere  // Jump to end (after all variant code)

                    [cmpInstr; branchNeInstr] @ printName @ printPayload @ [branchEndInstr])
                |> List.concat

            setup @ variantCode @ printNewline)

    | LIR.PrintRecord (recordPtr, typeName, fields) ->
        // Print record: TypeName { field1 = val1, field2 = val2, ... }\n
        // Record layout: [field0, field1, field2, ...] on heap (each 8 bytes)
        lirRegToARM64Reg recordPtr
        |> Result.map (fun recordReg ->
            let os =
                match Platform.detectOS () with
                | Ok platform -> platform
                | Error _ -> Platform.Linux
            let syscalls = Platform.getSyscallNumbers os

            // Helper: generate code to print a string literal
            let printLiteral (s: string) =
                let bytes = System.Text.Encoding.UTF8.GetBytes(s)
                if bytes.Length = 0 then []
                else
                    let alignedSize = max 16 ((bytes.Length + 15) &&& ~~~15)
                    [ARM64.SUB_imm (ARM64.SP, ARM64.SP, uint16 alignedSize)] @
                    (bytes |> Array.toList |> List.mapi (fun i b ->
                        [ARM64.MOVZ (ARM64.X0, uint16 b, 0); ARM64.STRB (ARM64.X0, ARM64.SP, i)]
                    ) |> List.concat) @
                    [ARM64.MOVZ (ARM64.X0, 1us, 0);
                     ARM64.MOV_reg (ARM64.X1, ARM64.SP);
                     ARM64.MOVZ (ARM64.X2, uint16 bytes.Length, 0);
                     ARM64.MOVZ (syscalls.SyscallRegister, syscalls.Write, 0);
                     ARM64.SVC syscalls.SvcImmediate;
                     ARM64.ADD_imm (ARM64.SP, ARM64.SP, uint16 alignedSize)]

            // Save record pointer in callee-saved register X19
            let setup = [ARM64.MOV_reg (ARM64.X19, recordReg)]

            // Print type name and opening brace
            let printHeader = printLiteral (typeName + " { ")

            // Print each field: "fieldName = value" with ", " separator between fields
            let printFields =
                fields
                |> List.mapi (fun i (fieldName, fieldType) ->
                    let printFieldName = printLiteral (fieldName + " = ")
                    let offset = int16 (i * 8)  // Each field is 8 bytes
                    let loadField = [ARM64.LDR (ARM64.X0, ARM64.X19, offset)]
                    let printValue =
                        match fieldType with
                        | AST.TInt64 -> Runtime.generatePrintIntNoNewline ()
                        | AST.TBool -> Runtime.generatePrintBoolNoNewline ()
                        | AST.TFloat64 ->
                            [ARM64.FMOV_from_gp (ARM64.D0, ARM64.X0)] @ Runtime.generatePrintFloatNoNewline ()
                        | AST.TString ->
                            // String is a pointer: load length, compute data ptr, print
                            [ARM64.LDR (ARM64.X10, ARM64.X0, 0s); ARM64.ADD_imm (ARM64.X9, ARM64.X0, 8us)] @
                            Runtime.generatePrintStringNoNewline ()
                        | t -> failwith $"Unsupported field type in record: {t}"
                    let separator =
                        if i < List.length fields - 1 then printLiteral ", "
                        else []
                    printFieldName @ loadField @ printValue @ separator)
                |> List.concat

            // Print closing brace and newline
            let printFooter = printLiteral " }\n"

            setup @ printHeader @ printFields @ printFooter)

    | LIR.Call (dest, funcName, args) ->
        // Function call: arguments already moved to X0-X7 by preceding MOVs
        // Caller-save is handled by SaveRegs/RestoreRegs instructions
        Ok [ARM64.BL funcName]

    | LIR.TailCall (funcName, args) ->
        // Tail call: restore stack frame, then branch (no link)
        // This is the same as the epilogue but with B instead of RET
        let calleeSavedSpace = calleeSavedStackSpace ctx.UsedCalleeSaved
        let restoreCalleeSavedInstrs = generateCalleeSavedRestores ctx.UsedCalleeSaved
        // Deallocate all stack at once
        let totalExtraStack = ctx.StackSize + calleeSavedSpace
        let deallocStack =
            if totalExtraStack > 0 then
                [ARM64.ADD_imm (ARM64.SP, ARM64.SP, uint16 totalExtraStack)]
            else
                []
        let restoreFpLr = [ARM64.LDP_post (ARM64.X29, ARM64.X30, ARM64.SP, 16s)]
        let branch = [ARM64.B_label funcName]
        Ok (restoreCalleeSavedInstrs @ deallocStack @ restoreFpLr @ branch)

    | LIR.IndirectCall (dest, func, args) ->
        // Indirect call: call through function pointer in register
        // Use BLR instruction instead of BL
        lirRegToARM64Reg func
        |> Result.map (fun funcReg -> [ARM64.BLR funcReg])

    | LIR.IndirectTailCall (func, args) ->
        // Indirect tail call: restore stack frame, then branch to register
        lirRegToARM64Reg func
        |> Result.map (fun funcReg ->
            let calleeSavedSpace = calleeSavedStackSpace ctx.UsedCalleeSaved
            let restoreCalleeSavedInstrs = generateCalleeSavedRestores ctx.UsedCalleeSaved
            let totalExtraStack = ctx.StackSize + calleeSavedSpace
            let deallocStack =
                if totalExtraStack > 0 then
                    [ARM64.ADD_imm (ARM64.SP, ARM64.SP, uint16 totalExtraStack)]
                else
                    []
            let restoreFpLr = [ARM64.LDP_post (ARM64.X29, ARM64.X30, ARM64.SP, 16s)]
            let branch = [ARM64.BR funcReg]
            restoreCalleeSavedInstrs @ deallocStack @ restoreFpLr @ branch)

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

    | LIR.ClosureTailCall (funcPtr, args) ->
        // Closure tail call: restore stack frame, then branch to register
        lirRegToARM64Reg funcPtr
        |> Result.map (fun funcPtrReg ->
            let calleeSavedSpace = calleeSavedStackSpace ctx.UsedCalleeSaved
            let restoreCalleeSavedInstrs = generateCalleeSavedRestores ctx.UsedCalleeSaved
            let totalExtraStack = ctx.StackSize + calleeSavedSpace
            let deallocStack =
                if totalExtraStack > 0 then
                    [ARM64.ADD_imm (ARM64.SP, ARM64.SP, uint16 totalExtraStack)]
                else
                    []
            let restoreFpLr = [ARM64.LDP_post (ARM64.X29, ARM64.X30, ARM64.SP, 16s)]
            let branch = [ARM64.BR funcPtrReg]
            restoreCalleeSavedInstrs @ deallocStack @ restoreFpLr @ branch)

    | LIR.SaveRegs (intRegs, floatRegs) ->
        // Save only the caller-saved registers that are live across this call
        // We maintain fixed offsets for ArgMoves compatibility:
        // Layout: X1-X10 at SP+0..SP+72 (fixed), D0-D7 at SP+80..SP+136
        // If no registers need saving, emit nothing (no stack allocation)
        if List.isEmpty intRegs && List.isEmpty floatRegs then
            Ok []  // Nothing to save - no stack allocation needed
        else
            // Determine stack size - we need fixed layout for ArgMoves compatibility
            // when any int registers are saved
            let hasIntRegs = not (List.isEmpty intRegs)
            let hasFloatRegs = not (List.isEmpty floatRegs)
            let intSlotSize = if hasIntRegs then 80 else 0  // X1-X10 (10 regs * 8 bytes)
            let floatSlotSize = if hasFloatRegs then 64 else 0  // D0-D7 (8 regs * 8 bytes)
            let totalSize = intSlotSize + floatSlotSize

            let allocStack = [ARM64.SUB_imm (ARM64.SP, ARM64.SP, uint16 totalSize)]

            // Save int registers using STP pairs where possible
            // Pairs: (X1,X2)@0, (X3,X4)@16, (X5,X6)@32, (X7,X8)@48, (X9,X10)@64
            let intPairs = [
                (LIR.X1, LIR.X2, 0s)
                (LIR.X3, LIR.X4, 16s)
                (LIR.X5, LIR.X6, 32s)
                (LIR.X7, LIR.X8, 48s)
                (LIR.X9, LIR.X10, 64s)
            ]

            let intSaves : ARM64.Instr list =
                intPairs |> List.collect (fun (r1, r2, offset) ->
                    let has1 = List.contains r1 intRegs
                    let has2 = List.contains r2 intRegs
                    match (has1, has2) with
                    | (true, true) ->
                        // Both registers - use STP
                        [ARM64.STP (lirPhysRegToARM64Reg r1, lirPhysRegToARM64Reg r2, ARM64.SP, offset)]
                    | (true, false) ->
                        // Only first register - use STR
                        [ARM64.STR (lirPhysRegToARM64Reg r1, ARM64.SP, offset)]
                    | (false, true) ->
                        // Only second register - use STR
                        [ARM64.STR (lirPhysRegToARM64Reg r2, ARM64.SP, offset + 8s)]
                    | (false, false) ->
                        // Neither register
                        [])

            // Save float registers using STP_fp pairs where possible
            // Pairs: (D0,D1)@0, (D2,D3)@16, (D4,D5)@32, (D6,D7)@48
            let baseFloatOffset = if hasIntRegs then 80s else 0s
            let floatPairs = [
                (LIR.D0, LIR.D1, 0s)
                (LIR.D2, LIR.D3, 16s)
                (LIR.D4, LIR.D5, 32s)
                (LIR.D6, LIR.D7, 48s)
            ]

            let floatSaves : ARM64.Instr list =
                floatPairs |> List.collect (fun (f1, f2, offset) ->
                    let has1 = List.contains f1 floatRegs
                    let has2 = List.contains f2 floatRegs
                    match (has1, has2) with
                    | (true, true) ->
                        // Both registers - use STP_fp
                        [ARM64.STP_fp (lirPhysFPRegToARM64FReg f1, lirPhysFPRegToARM64FReg f2, ARM64.SP, baseFloatOffset + offset)]
                    | (true, false) ->
                        // Only first register - use STR_fp
                        [ARM64.STR_fp (lirPhysFPRegToARM64FReg f1, ARM64.SP, baseFloatOffset + offset)]
                    | (false, true) ->
                        // Only second register - use STR_fp
                        [ARM64.STR_fp (lirPhysFPRegToARM64FReg f2, ARM64.SP, baseFloatOffset + offset + 8s)]
                    | (false, false) ->
                        // Neither register
                        [])

            Ok (allocStack @ intSaves @ floatSaves)

    | LIR.RestoreRegs (intRegs, floatRegs) ->
        // Restore only the caller-saved registers that are live across this call
        // Must match the layout from SaveRegs
        if List.isEmpty intRegs && List.isEmpty floatRegs then
            Ok []  // Nothing was saved - no stack deallocation needed
        else
            let hasIntRegs = not (List.isEmpty intRegs)
            let hasFloatRegs = not (List.isEmpty floatRegs)
            let intSlotSize = if hasIntRegs then 80 else 0
            let floatSlotSize = if hasFloatRegs then 64 else 0
            let totalSize = intSlotSize + floatSlotSize

            // Restore int registers using LDP pairs where possible
            // Pairs: (X1,X2)@0, (X3,X4)@16, (X5,X6)@32, (X7,X8)@48, (X9,X10)@64
            let intPairs = [
                (LIR.X1, LIR.X2, 0s)
                (LIR.X3, LIR.X4, 16s)
                (LIR.X5, LIR.X6, 32s)
                (LIR.X7, LIR.X8, 48s)
                (LIR.X9, LIR.X10, 64s)
            ]

            let intRestores : ARM64.Instr list =
                intPairs |> List.collect (fun (r1, r2, offset) ->
                    let has1 = List.contains r1 intRegs
                    let has2 = List.contains r2 intRegs
                    match (has1, has2) with
                    | (true, true) ->
                        // Both registers - use LDP
                        [ARM64.LDP (lirPhysRegToARM64Reg r1, lirPhysRegToARM64Reg r2, ARM64.SP, offset)]
                    | (true, false) ->
                        // Only first register - use LDR
                        [ARM64.LDR (lirPhysRegToARM64Reg r1, ARM64.SP, offset)]
                    | (false, true) ->
                        // Only second register - use LDR
                        [ARM64.LDR (lirPhysRegToARM64Reg r2, ARM64.SP, offset + 8s)]
                    | (false, false) ->
                        // Neither register
                        [])

            // Restore float registers using LDP_fp pairs where possible
            // Pairs: (D0,D1)@0, (D2,D3)@16, (D4,D5)@32, (D6,D7)@48
            let baseFloatOffset = if hasIntRegs then 80s else 0s
            let floatPairs = [
                (LIR.D0, LIR.D1, 0s)
                (LIR.D2, LIR.D3, 16s)
                (LIR.D4, LIR.D5, 32s)
                (LIR.D6, LIR.D7, 48s)
            ]

            let floatRestores : ARM64.Instr list =
                floatPairs |> List.collect (fun (f1, f2, offset) ->
                    let has1 = List.contains f1 floatRegs
                    let has2 = List.contains f2 floatRegs
                    match (has1, has2) with
                    | (true, true) ->
                        // Both registers - use LDP_fp
                        [ARM64.LDP_fp (lirPhysFPRegToARM64FReg f1, lirPhysFPRegToARM64FReg f2, ARM64.SP, baseFloatOffset + offset)]
                    | (true, false) ->
                        // Only first register - use LDR_fp
                        [ARM64.LDR_fp (lirPhysFPRegToARM64FReg f1, ARM64.SP, baseFloatOffset + offset)]
                    | (false, true) ->
                        // Only second register - use LDR_fp
                        [ARM64.LDR_fp (lirPhysFPRegToARM64FReg f2, ARM64.SP, baseFloatOffset + offset + 8s)]
                    | (false, false) ->
                        // Neither register
                        [])

            let deallocStack = [ARM64.ADD_imm (ARM64.SP, ARM64.SP, uint16 totalSize)]

            Ok (intRestores @ floatRestores @ deallocStack)

    | LIR.ArgMoves moves ->
        // Parallel move resolution for function arguments
        // After SaveRegs, X1-X10 are saved at [SP+0..SP+72]
        // If source is in X1-X7 and could be clobbered, load from stack instead
        //
        // Stack layout after SaveRegs: X1@[SP+0], X2@[SP+8], ..., X10@[SP+72]
        let saveRegsOffset (reg: LIR.PhysReg) : int option =
            match reg with
            | LIR.X1 -> Some 0
            | LIR.X2 -> Some 8
            | LIR.X3 -> Some 16
            | LIR.X4 -> Some 24
            | LIR.X5 -> Some 32
            | LIR.X6 -> Some 40
            | LIR.X7 -> Some 48
            | LIR.X8 -> Some 56
            | LIR.X9 -> Some 64
            | LIR.X10 -> Some 72
            | _ -> None

        // Find which destination registers (X0-X7) will be written
        let destRegs = moves |> List.map fst |> Set.ofList

        // For each move, determine how to execute it safely
        let generateMove (destReg: LIR.PhysReg, srcOp: LIR.Operand) : Result<ARM64.Instr list, string> =
            let destARM64 = lirPhysRegToARM64Reg destReg
            match srcOp with
            | LIR.Imm value ->
                Ok (loadImmediate destARM64 value)
            | LIR.Reg (LIR.Physical srcPhysReg) ->
                // If source equals destination, it's a no-op
                if srcPhysReg = destReg then
                    Ok []
                else
                    // Check if source register will be clobbered by an earlier move
                    // A register is clobbered if it's a destination of a move to a LOWER index
                    // (since we process X0, X1, X2, ... in order)
                    let srcWillBeClobbered =
                        match srcPhysReg with
                        | LIR.X1 | LIR.X2 | LIR.X3 | LIR.X4 | LIR.X5 | LIR.X6 | LIR.X7 ->
                            Set.contains srcPhysReg destRegs
                        | _ -> false
                    if srcWillBeClobbered then
                        // Load from SaveRegs stack instead of live register
                        match saveRegsOffset srcPhysReg with
                        | Some offset ->
                            Ok [ARM64.LDR (destARM64, ARM64.SP, int16 offset)]
                        | None ->
                            // Shouldn't happen, but fall back to MOV
                            let srcARM64 = lirPhysRegToARM64Reg srcPhysReg
                            Ok [ARM64.MOV_reg (destARM64, srcARM64)]
                    else
                        let srcARM64 = lirPhysRegToARM64Reg srcPhysReg
                        Ok [ARM64.MOV_reg (destARM64, srcARM64)]
            | LIR.Reg (LIR.Virtual _) ->
                Error "Virtual register in ArgMoves - should have been allocated"
            | LIR.StackSlot offset ->
                loadStackSlot destARM64 offset
            | LIR.StringRef idx ->
                // Convert pool string to heap format for function arguments
                // Functions expect heap strings: [length:8][data:N][refcount:8]
                // Pool strings are just raw data, so we must convert
                match Map.tryFind idx ctx.StringPool.Strings with
                | Some (_, len) ->
                    let label = "str_" + string idx
                    let totalSize = ((len + 16) + 7) &&& (~~~7)  // 8-byte aligned
                    Ok ([
                        // Load pool string address into X9
                        ARM64.ADRP (ARM64.X9, label)
                        ARM64.ADD_label (ARM64.X9, ARM64.X9, label)
                        // Allocate heap space (bump allocator)
                        ARM64.MOV_reg (destARM64, ARM64.X28)  // dest = current heap pointer
                        ARM64.ADD_imm (ARM64.X28, ARM64.X28, uint16 totalSize)  // bump pointer
                        // Store length
                    ] @ loadImmediate ARM64.X10 (int64 len) @ [
                        ARM64.STR (ARM64.X10, destARM64, 0s)  // [dest] = length
                        // Copy bytes: loop counter in X13 (NOT X0-X7 - those are arg registers!)
                        ARM64.MOVZ (ARM64.X13, 0us, 0)  // X13 = 0
                    ] @ loadImmediate ARM64.X11 (int64 len) @ [
                        // Loop start (if X13 >= len, done)
                        ARM64.CMP_reg (ARM64.X13, ARM64.X11)
                        ARM64.B_cond (ARM64.GE, 7)  // Skip 7 instructions to exit loop
                        ARM64.LDRB (ARM64.X15, ARM64.X9, ARM64.X13)  // X15 = pool[X13]
                        ARM64.ADD_imm (ARM64.X12, destARM64, 8us)  // X12 = dest + 8
                        ARM64.ADD_reg (ARM64.X12, ARM64.X12, ARM64.X13)  // X12 = dest + 8 + X13
                        ARM64.STRB_reg (ARM64.X15, ARM64.X12)  // [X12] = byte
                        ARM64.ADD_imm (ARM64.X13, ARM64.X13, 1us)  // X13++
                        ARM64.B (-7)  // Loop back to CMP
                        // Store refcount at aligned offset
                        // aligned(x) = ((x + 7) >> 3) << 3
                        ARM64.ADD_imm (ARM64.X12, ARM64.X10, 7us)        // X12 = len + 7
                        ARM64.MOVZ (ARM64.X15, 3us, 0)                   // X15 = 3
                        ARM64.LSR_reg (ARM64.X12, ARM64.X12, ARM64.X15)  // X12 = (len + 7) >> 3
                        ARM64.LSL_reg (ARM64.X12, ARM64.X12, ARM64.X15)  // X12 = aligned(len)
                        ARM64.ADD_imm (ARM64.X15, destARM64, 8us)        // X15 = dest + 8
                        ARM64.ADD_reg (ARM64.X12, ARM64.X15, ARM64.X12)  // X12 = dest + 8 + aligned(len)
                        ARM64.MOVZ (ARM64.X15, 1us, 0)
                        ARM64.STR (ARM64.X15, ARM64.X12, 0s)  // [X12] = 1
                    ])
                | None -> Error $"String index {idx} not found in pool"
            | LIR.FuncAddr funcName ->
                Ok [ARM64.ADR (destARM64, funcName)]
            | LIR.FloatImm _ | LIR.FloatRef _ ->
                Error "Float in ArgMoves not yet supported"

        // Generate all moves in order (X0, X1, X2, ...)
        let moveInstrs =
            moves
            |> List.sortBy (fun (destReg, _) ->
                match destReg with
                | LIR.X0 -> 0 | LIR.X1 -> 1 | LIR.X2 -> 2 | LIR.X3 -> 3
                | LIR.X4 -> 4 | LIR.X5 -> 5 | LIR.X6 -> 6 | LIR.X7 -> 7
                | _ -> 100)
            |> List.map generateMove
            |> List.fold (fun acc r ->
                match acc, r with
                | Ok instrs, Ok newInstrs -> Ok (instrs @ newInstrs)
                | Error e, _ -> Error e
                | _, Error e -> Error e) (Ok [])

        moveInstrs

    | LIR.TailArgMoves moves ->
        // Parallel move resolution for TAIL CALL arguments
        // Unlike ArgMoves, there is NO SaveRegs, so we can't load from stack.
        // We use the shared ParallelMoves module with X16 as the temp register.

        // Helper to get source register if operand is a physical register
        let getSrcPhysReg (srcOp: LIR.Operand) : LIR.PhysReg option =
            match srcOp with
            | LIR.Reg (LIR.Physical srcPhysReg) -> Some srcPhysReg
            | _ -> None

        // Generate a single move instruction (for non-register sources)
        let generateMoveInstr (destReg: LIR.PhysReg, srcOp: LIR.Operand) : Result<ARM64.Instr list, string> =
            let destARM64 = lirPhysRegToARM64Reg destReg
            match srcOp with
            | LIR.Imm value ->
                Ok (loadImmediate destARM64 value)
            | LIR.Reg (LIR.Physical srcPhysReg) ->
                let srcARM64 = lirPhysRegToARM64Reg srcPhysReg
                Ok [ARM64.MOV_reg (destARM64, srcARM64)]
            | LIR.Reg (LIR.Virtual _) ->
                Error "Virtual register in TailArgMoves - should have been allocated"
            | LIR.StackSlot offset ->
                loadStackSlot destARM64 offset
            | LIR.FuncAddr funcName ->
                Ok [ARM64.ADR (destARM64, funcName)]
            | LIR.StringRef idx ->
                // Convert pool string to heap format for tail call arguments
                // Same pattern as ArgMoves - functions expect heap strings
                match Map.tryFind idx ctx.StringPool.Strings with
                | Some (_, len) ->
                    let label = "str_" + string idx
                    let totalSize = ((len + 16) + 7) &&& (~~~7)  // 8-byte aligned
                    Ok ([
                        // Load pool string address into X9
                        ARM64.ADRP (ARM64.X9, label)
                        ARM64.ADD_label (ARM64.X9, ARM64.X9, label)
                        // Allocate heap space (bump allocator)
                        ARM64.MOV_reg (destARM64, ARM64.X28)  // dest = current heap pointer
                        ARM64.ADD_imm (ARM64.X28, ARM64.X28, uint16 totalSize)  // bump pointer
                        // Store length
                    ] @ loadImmediate ARM64.X10 (int64 len) @ [
                        ARM64.STR (ARM64.X10, destARM64, 0s)  // [dest] = length
                        // Copy bytes: loop counter in X13
                        ARM64.MOVZ (ARM64.X13, 0us, 0)  // X13 = 0
                    ] @ loadImmediate ARM64.X11 (int64 len) @ [
                        // Loop start (if X13 >= len, done)
                        ARM64.CMP_reg (ARM64.X13, ARM64.X11)
                        ARM64.B_cond (ARM64.GE, 7)  // Skip 7 instructions to exit loop
                        ARM64.LDRB (ARM64.X15, ARM64.X9, ARM64.X13)  // X15 = pool[X13]
                        ARM64.ADD_imm (ARM64.X12, destARM64, 8us)  // X12 = dest + 8
                        ARM64.ADD_reg (ARM64.X12, ARM64.X12, ARM64.X13)  // X12 = dest + 8 + X13
                        ARM64.STRB_reg (ARM64.X15, ARM64.X12)  // [X12] = byte
                        ARM64.ADD_imm (ARM64.X13, ARM64.X13, 1us)  // X13++
                        ARM64.B (-7)  // Loop back to CMP
                        // Store refcount
                        ARM64.ADD_imm (ARM64.X12, destARM64, 8us)
                        ARM64.ADD_reg (ARM64.X12, ARM64.X12, ARM64.X10)  // X12 = dest + 8 + len
                        ARM64.MOVZ (ARM64.X15, 1us, 0)
                        ARM64.STR (ARM64.X15, ARM64.X12, 0s)  // [X12] = 1
                    ])
                | None -> Error $"String index {idx} not found in pool"
            | LIR.FloatImm _ | LIR.FloatRef _ ->
                Error "Float in TailArgMoves not yet supported"

        // Use the shared parallel move resolution algorithm
        let actions = ParallelMoves.resolve moves getSrcPhysReg

        // Convert actions to ARM64 instructions
        let mutable allInstrs : ARM64.Instr list = []
        let mutable error : string option = None

        for action in actions do
            if error.IsNone then
                match action with
                | ParallelMoves.SaveToTemp reg ->
                    // Save register to X16 (temp)
                    allInstrs <- allInstrs @ [ARM64.MOV_reg (ARM64.X16, lirPhysRegToARM64Reg reg)]
                | ParallelMoves.Move (dest, src) ->
                    match generateMoveInstr (dest, src) with
                    | Ok instrs -> allInstrs <- allInstrs @ instrs
                    | Error e -> error <- Some e
                | ParallelMoves.MoveFromTemp dest ->
                    // Move from X16 (temp) to destination
                    allInstrs <- allInstrs @ [ARM64.MOV_reg (lirPhysRegToARM64Reg dest, ARM64.X16)]

        match error with
        | Some e -> Error e
        | None -> Ok allInstrs

    | LIR.FArgMoves moves ->
        // Float argument moves - move float values to D0-D7
        // Uses parallel move resolution to handle register conflicts correctly

        // First, convert all source FRegs to ARM64 FRegs
        let resolvedMoves =
            moves
            |> List.map (fun (destPhysReg, srcFReg) ->
                let destARM64 = lirPhysFPRegToARM64FReg destPhysReg
                match lirFRegToARM64FReg srcFReg with
                | Ok srcARM64 -> Ok (destARM64, srcARM64)
                | Error e -> Error e)
            |> List.fold (fun acc r ->
                match acc, r with
                | Ok moves, Ok move -> Ok (move :: moves)
                | Error e, _ -> Error e
                | _, Error e -> Error e) (Ok [])
            |> Result.map List.rev

        match resolvedMoves with
        | Error e -> Error e
        | Ok armMoves ->
            // Use ParallelMoves.resolve to get the correct move order
            // We treat ARM64.FReg as both dest and src type
            let getSrcReg (srcReg: ARM64.FReg) : ARM64.FReg option = Some srcReg
            let actions = ParallelMoves.resolve armMoves getSrcReg

            // Convert actions to ARM64 instructions
            // Use D16 as temp register for cycle breaking
            // D16-D31 are the upper half of the SIMD register file, not used elsewhere
            let mutable allInstrs : ARM64.Instr list = []
            for action in actions do
                match action with
                | ParallelMoves.SaveToTemp srcReg ->
                    // Save to D16 (temp) - using upper SIMD register
                    allInstrs <- allInstrs @ [ARM64.FMOV_reg (ARM64.D16, srcReg)]
                | ParallelMoves.Move (dest, src) ->
                    if dest <> src then
                        allInstrs <- allInstrs @ [ARM64.FMOV_reg (dest, src)]
                | ParallelMoves.MoveFromTemp dest ->
                    // Move from D16 (temp) to destination
                    allInstrs <- allInstrs @ [ARM64.FMOV_reg (dest, ARM64.D16)]

            Ok allInstrs

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
        let stringLabel = "str_" + string idx
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
            let floatLabel = "_float" + string idx
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

    | LIR.FAbs (dest, src) ->
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg src
            |> Result.map (fun srcReg -> [ARM64.FABS (destReg, srcReg)]))

    | LIR.FSqrt (dest, src) ->
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg src
            |> Result.map (fun srcReg -> [ARM64.FSQRT (destReg, srcReg)]))

    | LIR.FCmp (left, right) ->
        lirFRegToARM64FReg left
        |> Result.bind (fun leftReg ->
            lirFRegToARM64FReg right
            |> Result.map (fun rightReg -> [ARM64.FCMP (leftReg, rightReg)]))

    | LIR.IntToFloat (dest, src) ->
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64.SCVTF (destReg, srcReg)]))

    | LIR.FloatToInt (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirFRegToARM64FReg src
            |> Result.map (fun srcReg -> [ARM64.FCVTZS (destReg, srcReg)]))

    | LIR.GpToFp (dest, src) ->
        // Move bits from GP register to FP register (for floats loaded from heap)
        lirFRegToARM64FReg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg src
            |> Result.map (fun srcReg -> [ARM64.FMOV_from_gp (destReg, srcReg)]))

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

    | LIR.HeapStore (addr, offset, src, valueType) ->
        // Store value at addr + offset (offset is in bytes)
        lirRegToARM64Reg addr
        |> Result.bind (fun addrReg ->
            match src, valueType with
            | LIR.Imm value, _ ->
                // Load immediate into temp register, then store
                let tempReg = ARM64.X9
                Ok (loadImmediate tempReg value @
                    [ARM64.STR (tempReg, addrReg, int16 offset)])
            | LIR.Reg srcReg, Some AST.TFloat64 ->
                // Float value in register: interpret as FReg and use STR_fp
                // The srcReg ID is actually an FVirtual, convert to ARM64 FP register
                lirFRegToARM64FReg (virtualToFVirtual srcReg)
                |> Result.map (fun srcARM64FP ->
                    [ARM64.STR_fp (srcARM64FP, addrReg, int16 offset)])
            | LIR.Reg srcReg, _ ->
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
            | LIR.StackSlot slotOffset, _ ->
                // Load from stack slot into temp, then store to heap
                let tempReg = ARM64.X9
                loadStackSlot tempReg slotOffset
                |> Result.map (fun loadInstrs ->
                    loadInstrs @ [ARM64.STR (tempReg, addrReg, int16 offset)])
            | LIR.FuncAddr funcName, _ ->
                // Load function address into temp, then store to heap
                let tempReg = ARM64.X9
                Ok [ARM64.ADR (tempReg, funcName); ARM64.STR (tempReg, addrReg, int16 offset)]
            | LIR.StringRef idx, _ ->
                // Convert pool string to heap format when storing in tuples/data structures
                // Heap strings: [length:8][data:N][refcount:8]
                // Pool strings: just raw bytes (length known at compile time)
                // We must convert because tuple extraction expects heap format
                match Map.tryFind idx ctx.StringPool.Strings with
                | Some (_, len) ->
                    let label = "str_" + string idx
                    let totalSize = ((len + 16) + 7) &&& (~~~7)  // 8-byte aligned
                    Ok ([
                        // Load pool string address into X10
                        ARM64.ADRP (ARM64.X10, label)
                        ARM64.ADD_label (ARM64.X10, ARM64.X10, label)
                        // Allocate heap space (bump allocator), store address in X9
                        ARM64.MOV_reg (ARM64.X9, ARM64.X28)  // X9 = current heap pointer (result)
                        ARM64.ADD_imm (ARM64.X28, ARM64.X28, uint16 totalSize)  // bump pointer
                        // Store length (known at compile time)
                    ] @ loadImmediate ARM64.X11 (int64 len) @ [
                        ARM64.STR (ARM64.X11, ARM64.X9, 0s)   // Store length at heap[0]
                        // Copy bytes: counter in X12, limit in X11
                        ARM64.MOVZ (ARM64.X12, 0us, 0)  // X12 = 0
                        // Loop start (if X12 >= len, done)
                        ARM64.CMP_reg (ARM64.X12, ARM64.X11)
                        ARM64.B_cond (ARM64.GE, 7)  // Skip 7 instructions to exit loop
                        ARM64.LDRB (ARM64.X15, ARM64.X10, ARM64.X12)  // X15 = pool[X12]
                        ARM64.ADD_imm (ARM64.X14, ARM64.X9, 8us)  // X14 = heap + 8
                        ARM64.ADD_reg (ARM64.X14, ARM64.X14, ARM64.X12)  // X14 = heap + 8 + X12
                        ARM64.STRB_reg (ARM64.X15, ARM64.X14)  // heap_data[X12] = byte
                        ARM64.ADD_imm (ARM64.X12, ARM64.X12, 1us)  // X12++
                        ARM64.B (-7)  // Loop back to CMP
                        // Store refcount = 1
                        ARM64.ADD_imm (ARM64.X14, ARM64.X9, 8us)
                        ARM64.ADD_reg (ARM64.X14, ARM64.X14, ARM64.X11)  // X14 = heap + 8 + len
                        ARM64.MOVZ (ARM64.X15, 1us, 0)
                        ARM64.STR (ARM64.X15, ARM64.X14, 0s)  // refcount = 1
                        // Store heap string address to tuple slot
                        ARM64.STR (ARM64.X9, addrReg, int16 offset)
                    ])
                | None -> Error $"String index {idx} not found in pool"
            | LIR.FloatRef idx, _ ->
                // Load float VALUE from pool into temp FP register, then store to heap
                let floatLabel = "_float" + string idx
                Ok [
                    ARM64.ADRP (ARM64.X9, floatLabel)              // Load page address
                    ARM64.ADD_label (ARM64.X9, ARM64.X9, floatLabel) // Add offset
                    ARM64.LDR_fp (ARM64.D15, ARM64.X9, 0s)         // Load float into D15
                    ARM64.STR_fp (ARM64.D15, addrReg, int16 offset) // Store float to heap
                ]
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
                        let label = "str_" + string idx
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
                        ARM64.MOVK (ARM64.X15, 0xFFFFus, 16)         // Bits 16-31
                        ARM64.MOVK (ARM64.X15, 0xFFFFus, 32)         // Bits 32-47
                        ARM64.MOVK (ARM64.X15, 0xFFFFus, 48)         // Bits 48-63
                        ARM64.AND_reg (ARM64.X14, ARM64.X14, ARM64.X15)  // X14 = aligned size
                        ARM64.MOV_reg (ARM64.X14, ARM64.X28)            // X14 = current heap ptr (result)
                        ARM64.ADD_imm (ARM64.X15, ARM64.X13, 16us)      // X15 = total + 16
                        ARM64.ADD_imm (ARM64.X15, ARM64.X15, 7us)       // Align
                        ARM64.MOVZ (ARM64.X0, 0xFFF8us, 0)              // ~7 mask again (X15 was clobbered)
                        ARM64.MOVK (ARM64.X0, 0xFFFFus, 16)             // Bits 16-31
                        ARM64.MOVK (ARM64.X0, 0xFFFFus, 32)             // Bits 32-47
                        ARM64.MOVK (ARM64.X0, 0xFFFFus, 48)             // Bits 48-63
                        ARM64.AND_reg (ARM64.X15, ARM64.X15, ARM64.X0)
                        ARM64.ADD_reg (ARM64.X28, ARM64.X28, ARM64.X15) // Bump heap pointer
                    ]

                    // Store total length at [X14]
                    let storeLen = [ARM64.STR (ARM64.X13, ARM64.X14, 0s)]

                    // Copy left bytes: loop copying X10 bytes from X9 to [X14+8]
                    // IMPORTANT: Don't use X0-X7 as temps - they may hold function arguments!
                    // Strategy: Use pointer-bumping loops instead of indexed addressing
                    // X15 = source pointer (starts at X9, bumped each iteration)
                    // X16 = dest pointer (starts at X14+8, bumped each iteration)
                    // X13 = remaining count (starts at X10, decremented, reused since we stored total already)
                    let copyLeft = [
                        ARM64.MOV_reg (ARM64.X15, ARM64.X9)              // 0: X15 = src ptr
                        ARM64.ADD_imm (ARM64.X16, ARM64.X14, 8us)        // 1: X16 = dest ptr (X14 + 8)
                        ARM64.MOV_reg (ARM64.X13, ARM64.X10)             // 2: X13 = remaining = len1
                        // Loop: if X13 == 0, done (skip 7 instructions to exit past B at index 9)
                        ARM64.CBZ_offset (ARM64.X13, 7)                  // 3: Skip 7 instructions if done -> index 10 (past end)
                        ARM64.LDRB_imm (ARM64.X8, ARM64.X15, 0)          // 4: X8 = byte at [X15]
                        ARM64.STRB_reg (ARM64.X8, ARM64.X16)             // 5: [X16] = byte
                        ARM64.ADD_imm (ARM64.X15, ARM64.X15, 1us)        // 6: X15++ (src ptr)
                        ARM64.ADD_imm (ARM64.X16, ARM64.X16, 1us)        // 7: X16++ (dest ptr)
                        ARM64.SUB_imm (ARM64.X13, ARM64.X13, 1us)        // 8: X13-- (remaining)
                        ARM64.B (-6)                                     // 9: Loop back to CBZ (index 3)
                    ]

                    // Copy right bytes: loop copying X12 bytes from X11 to [X14+8+X10]
                    // X15 = source pointer (starts at X11)
                    // X16 = dest pointer (starts at X14+8+X10, already in X16 from copyLeft end)
                    // X13 = remaining count (use X12)
                    // Note: X16 is already at X14+8+len1 after copyLeft loop ends!
                    let copyRight = [
                        ARM64.MOV_reg (ARM64.X15, ARM64.X11)             // 0: X15 = src ptr (right string)
                        ARM64.MOV_reg (ARM64.X13, ARM64.X12)             // 1: X13 = remaining = len2
                        // Loop: if X13 == 0, done (skip 7 instructions to exit past B at index 8)
                        ARM64.CBZ_offset (ARM64.X13, 7)                  // 2: Skip 7 instructions if done -> index 9 (past end)
                        ARM64.LDRB_imm (ARM64.X8, ARM64.X15, 0)          // 3: X8 = byte at [X15]
                        ARM64.STRB_reg (ARM64.X8, ARM64.X16)             // 4: [X16] = byte
                        ARM64.ADD_imm (ARM64.X15, ARM64.X15, 1us)        // 5: X15++ (src ptr)
                        ARM64.ADD_imm (ARM64.X16, ARM64.X16, 1us)        // 6: X16++ (dest ptr)
                        ARM64.SUB_imm (ARM64.X13, ARM64.X13, 1us)        // 7: X13-- (remaining)
                        ARM64.B (-6)                                     // 8: Loop back to CBZ (index 2)
                    ]

                    // Recompute total length since we clobbered X13
                    let recomputeTotal = [
                        ARM64.ADD_reg (ARM64.X13, ARM64.X10, ARM64.X12)  // X13 = len1 + len2
                    ]

                    // Store refcount=1 at [X14+8+aligned(total)]
                    // where aligned(x) = ((x + 7) >> 3) << 3
                    let storeRefcount = [
                        ARM64.ADD_imm (ARM64.X15, ARM64.X13, 7us)        // X15 = total + 7
                        ARM64.MOVZ (ARM64.X16, 3us, 0)                   // X16 = 3 (shift amount)
                        ARM64.LSR_reg (ARM64.X15, ARM64.X15, ARM64.X16)  // X15 = (total + 7) >> 3
                        ARM64.LSL_reg (ARM64.X15, ARM64.X15, ARM64.X16)  // X15 = aligned(total)
                        ARM64.ADD_imm (ARM64.X16, ARM64.X14, 8us)        // X16 = dest + 8
                        ARM64.ADD_reg (ARM64.X15, ARM64.X16, ARM64.X15)  // X15 = dest + 8 + aligned(total)
                        ARM64.MOVZ (ARM64.X16, 1us, 0)                   // X16 = 1
                        ARM64.STR (ARM64.X16, ARM64.X15, 0s)             // [X15] = 1
                    ]

                    // Move result to dest
                    let moveResult = [ARM64.MOV_reg (destReg, ARM64.X14)]

                    leftInstrs @ rightInstrs @ calcTotal @ allocate @ storeLen @ copyLeft @ copyRight @ recomputeTotal @ storeRefcount @ moveResult
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
                    let label = "str_" + string idx
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
                    let label = "str_" + string idx
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
                        // Store refcount = 1 at [X15 + 8 + aligned(len)]
                        // aligned(x) = ((x + 7) >> 3) << 3
                        ARM64.ADD_imm (ARM64.X13, ARM64.X10, 7us)        // X13 = len + 7
                        ARM64.MOVZ (ARM64.X12, 3us, 0)                   // X12 = 3
                        ARM64.LSR_reg (ARM64.X13, ARM64.X13, ARM64.X12)  // X13 = (len + 7) >> 3
                        ARM64.LSL_reg (ARM64.X13, ARM64.X13, ARM64.X12)  // X13 = aligned(len)
                        ARM64.ADD_imm (ARM64.X12, ARM64.X15, 8us)        // X12 = X15 + 8
                        ARM64.ADD_reg (ARM64.X13, ARM64.X12, ARM64.X13)  // X13 = X15 + 8 + aligned(len)
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
                        let label = "str_" + string idx
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
                            // Store refcount = 1 at [tempReg + 8 + aligned(len)]
                            // aligned(x) = ((x + 7) >> 3) << 3
                            ARM64.ADD_imm (ARM64.X13, ARM64.X10, 7us)        // X13 = len + 7
                            ARM64.MOVZ (ARM64.X12, 3us, 0)                   // X12 = 3
                            ARM64.LSR_reg (ARM64.X13, ARM64.X13, ARM64.X12)  // X13 = (len + 7) >> 3
                            ARM64.LSL_reg (ARM64.X13, ARM64.X13, ARM64.X12)  // X13 = aligned(len)
                            ARM64.ADD_imm (ARM64.X12, tempReg, 8us)          // X12 = tempReg + 8
                            ARM64.ADD_reg (ARM64.X13, ARM64.X12, ARM64.X13)  // X13 = tempReg + 8 + aligned(len)
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
                        let label = "str_" + string idx
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
                            // Store refcount = 1 at [tempReg + 8 + aligned(len)]
                            // aligned(x) = ((x + 7) >> 3) << 3
                            ARM64.ADD_imm (ARM64.X13, ARM64.X10, 7us)        // X13 = len + 7
                            ARM64.MOVZ (ARM64.X12, 3us, 0)                   // X12 = 3
                            ARM64.LSR_reg (ARM64.X13, ARM64.X13, ARM64.X12)  // X13 = (len + 7) >> 3
                            ARM64.LSL_reg (ARM64.X13, ARM64.X13, ARM64.X12)  // X13 = aligned(len)
                            ARM64.ADD_imm (ARM64.X12, tempReg, 8us)          // X12 = tempReg + 8
                            ARM64.ADD_reg (ARM64.X13, ARM64.X12, ARM64.X13)  // X13 = tempReg + 8 + aligned(len)
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

    | LIR.FileDelete (dest, path) ->
        // File delete: deletes file at path
        // Uses unlink syscall to remove file
        // Returns Result<Unit, String>
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            match path with
            | LIR.Reg pathReg ->
                // Already a heap string pointer
                lirRegToARM64Reg pathReg
                |> Result.map (fun pathARM64 ->
                    Runtime.generateFileDelete destReg pathARM64)
            | LIR.StringRef idx ->
                // Pool string - convert to heap format first, then call FileDelete
                // Heap format: [length:8][data:N][refcount:8]
                match Map.tryFind idx ctx.StringPool.Strings with
                | Some (_, len) ->
                    let label = "str_" + string idx
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
                        // Store refcount = 1 at [X15 + 8 + aligned(len)]
                        // aligned(x) = ((x + 7) >> 3) << 3
                        ARM64.ADD_imm (ARM64.X13, ARM64.X10, 7us)        // X13 = len + 7
                        ARM64.MOVZ (ARM64.X12, 3us, 0)                   // X12 = 3
                        ARM64.LSR_reg (ARM64.X13, ARM64.X13, ARM64.X12)  // X13 = (len + 7) >> 3
                        ARM64.LSL_reg (ARM64.X13, ARM64.X13, ARM64.X12)  // X13 = aligned(len)
                        ARM64.ADD_imm (ARM64.X12, ARM64.X15, 8us)        // X12 = X15 + 8
                        ARM64.ADD_reg (ARM64.X13, ARM64.X12, ARM64.X13)  // X13 = X15 + 8 + aligned(len)
                        ARM64.MOVZ (ARM64.X12, 1us, 0)
                        ARM64.STR (ARM64.X12, ARM64.X13, 0s)  // [X13] = 1
                    ] @ Runtime.generateFileDelete destReg ARM64.X15)
                | None -> Error ("String index " + string idx + " not found in pool")
            | LIR.StackSlot offset ->
                // Load heap string from stack slot
                loadStackSlot ARM64.X15 offset
                |> Result.map (fun loadInstrs ->
                    loadInstrs @ Runtime.generateFileDelete destReg ARM64.X15)
            | _ -> Error "FileDelete requires string operand")

    | LIR.FileSetExecutable (dest, path) ->
        // File set executable: sets executable bit on file at path
        // Uses chmod syscall with executable permission
        // Returns Result<Unit, String>
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            match path with
            | LIR.Reg pathReg ->
                // Already a heap string pointer
                lirRegToARM64Reg pathReg
                |> Result.map (fun pathARM64 ->
                    Runtime.generateFileSetExecutable destReg pathARM64)
            | LIR.StringRef idx ->
                // Pool string - convert to heap format first
                match Map.tryFind idx ctx.StringPool.Strings with
                | Some (_, len) ->
                    let label = "str_" + string idx
                    let totalSize = ((len + 16) + 7) &&& (~~~7)  // 8-byte aligned
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
                    ] @ Runtime.generateFileSetExecutable destReg ARM64.X15)
                | None -> Error ("String index " + string idx + " not found in pool")
            | LIR.StackSlot offset ->
                loadStackSlot ARM64.X15 offset
                |> Result.map (fun loadInstrs ->
                    loadInstrs @ Runtime.generateFileSetExecutable destReg ARM64.X15)
            | _ -> Error "FileSetExecutable requires string operand")

    | LIR.FileWriteFromPtr (dest, path, ptr, length) ->
        // Write raw bytes from ptr to file at path
        // Returns 1 on success, 0 on failure
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg ptr
            |> Result.bind (fun ptrARM64 ->
                lirRegToARM64Reg length
                |> Result.bind (fun lengthARM64 ->
                    match path with
                    | LIR.Reg pathReg ->
                        // Already a heap string pointer
                        lirRegToARM64Reg pathReg
                        |> Result.map (fun pathARM64 ->
                            Runtime.generateFileWriteFromPtr destReg pathARM64 ptrARM64 lengthARM64)
                    | LIR.StringRef idx ->
                        // Pool string - convert to heap format first
                        match Map.tryFind idx ctx.StringPool.Strings with
                        | Some (_, len) ->
                            let label = "str_" + string idx
                            let totalSize = ((len + 16) + 7) &&& (~~~7)  // 8-byte aligned
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
                            ] @ Runtime.generateFileWriteFromPtr destReg ARM64.X15 ptrARM64 lengthARM64)
                        | None -> Error ("String index " + string idx + " not found in pool")
                    | LIR.StackSlot offset ->
                        loadStackSlot ARM64.X15 offset
                        |> Result.map (fun loadInstrs ->
                            loadInstrs @ Runtime.generateFileWriteFromPtr destReg ARM64.X15 ptrARM64 lengthARM64)
                    | _ -> Error "FileWriteFromPtr requires string path operand")))

    | LIR.RawAlloc (dest, numBytes) ->
        // Raw allocation: simple bump allocator without refcount header
        // Just allocates numBytes and returns pointer
        // numBytes is already in a physical register (from MIR_to_LIR)
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg numBytes
            |> Result.map (fun numBytesReg ->
                [
                    // Align numBytes to 8 bytes
                    ARM64.ADD_imm (ARM64.X15, numBytesReg, 7us)        // X15 = numBytes + 7
                    ARM64.MOVZ (ARM64.X14, 0xFFF8us, 0)                // Lower 16 bits of ~7
                    ARM64.MOVK (ARM64.X14, 0xFFFFus, 16)               // Bits 16-31
                    ARM64.MOVK (ARM64.X14, 0xFFFFus, 32)               // Bits 32-47
                    ARM64.MOVK (ARM64.X14, 0xFFFFus, 48)               // Bits 48-63
                    ARM64.AND_reg (ARM64.X15, ARM64.X15, ARM64.X14)    // X15 = aligned size
                    // Bump allocate
                    ARM64.MOV_reg (destReg, ARM64.X28)                 // dest = current heap pointer
                    ARM64.ADD_reg (ARM64.X28, ARM64.X28, ARM64.X15)    // bump pointer by aligned size
                ]))

    | LIR.RawFree ptr ->
        // Raw free: no-op for now (bump allocator doesn't support free)
        // In future: could add to a raw memory free list
        Ok []

    | LIR.RawGet (dest, ptr, byteOffset) ->
        // Load 8 bytes from ptr + byteOffset
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg ptr
            |> Result.bind (fun ptrReg ->
                lirRegToARM64Reg byteOffset
                |> Result.map (fun offsetReg ->
                    [
                        ARM64.ADD_reg (ARM64.X15, ptrReg, offsetReg)   // X15 = ptr + offset
                        ARM64.LDR (destReg, ARM64.X15, 0s)             // dest = [X15]
                    ])))

    | LIR.RawGetByte (dest, ptr, byteOffset) ->
        // Load 1 byte from ptr + byteOffset (zero-extended to 64 bits)
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            lirRegToARM64Reg ptr
            |> Result.bind (fun ptrReg ->
                lirRegToARM64Reg byteOffset
                |> Result.map (fun offsetReg ->
                    [
                        ARM64.ADD_reg (ARM64.X15, ptrReg, offsetReg)   // X15 = ptr + offset
                        ARM64.LDRB_imm (destReg, ARM64.X15, 0)         // dest = [X15] (byte, zero-extended)
                    ])))

    | LIR.RawSet (ptr, byteOffset, value) ->
        // Store 8 bytes at ptr + byteOffset
        // IMPORTANT: If any input reg is X15, use X14 as temp instead
        lirRegToARM64Reg ptr
        |> Result.bind (fun ptrReg ->
            lirRegToARM64Reg byteOffset
            |> Result.bind (fun offsetReg ->
                lirRegToARM64Reg value
                |> Result.map (fun valueReg ->
                    let tempReg =
                        if ptrReg = ARM64.X15 || offsetReg = ARM64.X15 || valueReg = ARM64.X15 then
                            ARM64.X14
                        else
                            ARM64.X15
                    [
                        ARM64.ADD_reg (tempReg, ptrReg, offsetReg)   // temp = ptr + offset
                        ARM64.STR (valueReg, tempReg, 0s)            // [temp] = value
                    ])))

    | LIR.RawSetByte (ptr, byteOffset, value) ->
        // Store 1 byte at ptr + byteOffset
        // IMPORTANT: If any input reg is X15, use X14 as temp instead
        lirRegToARM64Reg ptr
        |> Result.bind (fun ptrReg ->
            lirRegToARM64Reg byteOffset
            |> Result.bind (fun offsetReg ->
                lirRegToARM64Reg value
                |> Result.map (fun valueReg ->
                    let tempReg =
                        if ptrReg = ARM64.X15 || offsetReg = ARM64.X15 || valueReg = ARM64.X15 then
                            ARM64.X14
                        else
                            ARM64.X15
                    [
                        ARM64.ADD_reg (tempReg, ptrReg, offsetReg)   // temp = ptr + offset
                        ARM64.STRB_reg (valueReg, tempReg)           // [temp] = value (byte)
                    ])))

    | LIR.StringHash (dest, str) ->
        // FNV-1a hash of string
        // Heap string layout: [length:8][data:N][refcount:8]
        // Pool string layout: [length:8][data:N]
        //
        // FNV-1a constants:
        // FNV_OFFSET_BASIS = 14695981039346656037 (0xcbf29ce484222325)
        // FNV_PRIME = 1099511628211 (0x100000001b3)
        //
        // Register usage:
        // X9 = data address
        // X10 = length
        // X11 = hash accumulator
        // X12 = FNV prime
        // X13 = loop counter
        // X14 = temp byte

        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            // Helper to load string address and length
            let loadStringInfo (operand: LIR.Operand) : Result<ARM64.Instr list, string> =
                match operand with
                | LIR.StringRef idx ->
                    // Pool string: address via ADRP+ADD, then read length from memory
                    let label = "str_" + string idx
                    Ok [
                        ARM64.ADRP (ARM64.X15, label)
                        ARM64.ADD_label (ARM64.X15, ARM64.X15, label)
                        ARM64.LDR (ARM64.X10, ARM64.X15, 0s)      // X10 = length
                        ARM64.ADD_imm (ARM64.X9, ARM64.X15, 8us)  // X9 = data address
                    ]
                | LIR.Reg reg ->
                    // Heap string or passed pool string: length at [reg], data at [reg+8]
                    lirRegToARM64Reg reg
                    |> Result.map (fun srcReg ->
                        if srcReg = ARM64.X9 || srcReg = ARM64.X10 then
                            // Copy to X15 first to avoid clobbering the source
                            [
                                ARM64.MOV_reg (ARM64.X15, srcReg)
                                ARM64.LDR (ARM64.X10, ARM64.X15, 0s)       // X10 = length
                                ARM64.ADD_imm (ARM64.X9, ARM64.X15, 8us)   // X9 = data address
                            ]
                        else
                            [
                                ARM64.LDR (ARM64.X10, srcReg, 0s)          // X10 = length
                                ARM64.ADD_imm (ARM64.X9, srcReg, 8us)      // X9 = data address
                            ])
                | _ -> Error "StringHash requires StringRef or Reg operand"

            loadStringInfo str
            |> Result.map (fun loadInstrs ->
                // FNV offset basis: 0xcbf29ce484222325
                let loadOffsetBasis = [
                    ARM64.MOVZ (ARM64.X11, 0x2325us, 0)
                    ARM64.MOVK (ARM64.X11, 0x8422us, 16)
                    ARM64.MOVK (ARM64.X11, 0x9ce4us, 32)
                    ARM64.MOVK (ARM64.X11, 0xcbf2us, 48)
                ]

                // FNV prime: 0x100000001b3
                let loadPrime = [
                    ARM64.MOVZ (ARM64.X12, 0x01b3us, 0)
                    ARM64.MOVK (ARM64.X12, 0x0000us, 16)
                    ARM64.MOVK (ARM64.X12, 0x0001us, 32)
                    ARM64.MOVK (ARM64.X12, 0x0000us, 48)
                ]

                // Hash loop
                let hashLoop = [
                    ARM64.MOVZ (ARM64.X13, 0us, 0)                    // X13 = 0 (counter)
                    // Loop: if X13 >= X10, done
                    ARM64.CMP_reg (ARM64.X13, ARM64.X10)              // compare counter with length
                    ARM64.B_cond (ARM64.GE, 6)                        // skip 6 instructions if done (to moveResult)
                    ARM64.LDRB (ARM64.X14, ARM64.X9, ARM64.X13)       // X14 = byte at [X9 + X13]
                    ARM64.EOR_reg (ARM64.X11, ARM64.X11, ARM64.X14)   // hash ^= byte
                    ARM64.MUL (ARM64.X11, ARM64.X11, ARM64.X12)       // hash *= prime
                    ARM64.ADD_imm (ARM64.X13, ARM64.X13, 1us)         // counter++
                    ARM64.B (-6)                                      // loop back to CMP
                ]

                // Move result to dest
                let moveResult = [ARM64.MOV_reg (destReg, ARM64.X11)]

                loadInstrs @ loadOffsetBasis @ loadPrime @ hashLoop @ moveResult))

    | LIR.StringEq (dest, left, right) ->
        // Byte-wise string equality comparison
        // Returns 1 (true) if equal, 0 (false) if not
        //
        // Register usage:
        // X9 = left data address
        // X10 = left length
        // X11 = right data address
        // X12 = right length
        // X13 = loop counter
        // X14 = left byte
        // X15 = right byte

        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            // Helper to load string address and length into specified registers
            let loadStringInfo (operand: LIR.Operand) (addrReg: ARM64.Reg) (lenReg: ARM64.Reg) (tempReg: ARM64.Reg) : Result<ARM64.Instr list, string> =
                match operand with
                | LIR.StringRef idx ->
                    // Pool string: address via ADRP+ADD, then read length from memory
                    let label = "str_" + string idx
                    Ok [
                        ARM64.ADRP (tempReg, label)
                        ARM64.ADD_label (tempReg, tempReg, label)
                        ARM64.LDR (lenReg, tempReg, 0s)       // length at offset 0
                        ARM64.ADD_imm (addrReg, tempReg, 8us) // data at offset 8
                    ]
                | LIR.Reg reg ->
                    lirRegToARM64Reg reg
                    |> Result.map (fun srcReg ->
                        if srcReg = addrReg || srcReg = lenReg then
                            // Use tempReg as scratch to avoid clobbering
                            [
                                ARM64.MOV_reg (tempReg, srcReg)
                                ARM64.LDR (lenReg, tempReg, 0s)
                                ARM64.ADD_imm (addrReg, tempReg, 8us)
                            ]
                        else
                            [
                                ARM64.LDR (lenReg, srcReg, 0s)
                                ARM64.ADD_imm (addrReg, srcReg, 8us)
                            ])
                | _ -> Error "StringEq requires StringRef or Reg operand"

            // Use X8 as temp for left, X7 for right (won't conflict with X9-X15)
            loadStringInfo left ARM64.X9 ARM64.X10 ARM64.X8
            |> Result.bind (fun leftInstrs ->
                loadStringInfo right ARM64.X11 ARM64.X12 ARM64.X7
                |> Result.map (fun rightInstrs ->
                    // Compare lengths first
                    let compareLengths = [
                        ARM64.CMP_reg (ARM64.X10, ARM64.X12)
                        ARM64.B_cond (ARM64.NE, 12)                   // If lengths differ, jump to false (skip 12 instrs)
                    ]

                    // Byte comparison loop
                    let compareLoop = [
                        ARM64.MOVZ (ARM64.X13, 0us, 0)                // X13 = 0 (counter)
                        // Loop: if X13 >= X10, all bytes matched -> true
                        ARM64.CMP_reg (ARM64.X13, ARM64.X10)
                        ARM64.B_cond (ARM64.GE, 7)                    // If done, jump to true (skip 7 instrs)
                        ARM64.LDRB (ARM64.X14, ARM64.X9, ARM64.X13)   // X14 = left[X13]
                        ARM64.LDRB (ARM64.X15, ARM64.X11, ARM64.X13)  // X15 = right[X13]
                        ARM64.CMP_reg (ARM64.X14, ARM64.X15)
                        ARM64.B_cond (ARM64.NE, 5)                    // If bytes differ, jump to false (skip 5 instrs)
                        ARM64.ADD_imm (ARM64.X13, ARM64.X13, 1us)     // counter++
                        ARM64.B (-7)                                  // loop back to CMP
                    ]

                    // True result
                    let trueResult = [
                        ARM64.MOVZ (destReg, 1us, 0)                  // dest = 1 (true)
                        ARM64.B 2                                     // skip false
                    ]

                    // False result
                    let falseResult = [
                        ARM64.MOVZ (destReg, 0us, 0)                  // dest = 0 (false)
                    ]

                    leftInstrs @ rightInstrs @ compareLengths @ compareLoop @ trueResult @ falseResult)))

    | LIR.RefCountIncString str ->
        // Increment refcount for a heap string
        // Heap string layout: [length:8][data:N][padding:P][refcount:8] where P aligns to 8
        // Pool strings have refcount = INT64_MAX as sentinel (don't modify read-only memory)
        match str with
        | LIR.StringRef _ ->
            // Pool string - no refcount, no-op
            Ok []
        | LIR.Reg reg ->
            // Heap or pool string - refcount is at [addr + 8 + aligned(length)]
            lirRegToARM64Reg reg
            |> Result.map (fun addrReg ->
                [
                    // Save address to X12 in case addrReg is X13/X14/X15 which we clobber
                    ARM64.MOV_reg (ARM64.X12, addrReg)               // X12 = string address
                    ARM64.LDR (ARM64.X15, ARM64.X12, 0s)             // X15 = length
                    // Align length: X15 = ((X15 + 7) >> 3) << 3
                    ARM64.ADD_imm (ARM64.X15, ARM64.X15, 7us)        // X15 = length + 7
                    ARM64.MOVZ (ARM64.X13, 3us, 0)                   // X13 = 3 (shift amount)
                    ARM64.LSR_reg (ARM64.X15, ARM64.X15, ARM64.X13)  // X15 = (length + 7) >> 3
                    ARM64.LSL_reg (ARM64.X15, ARM64.X15, ARM64.X13)  // X15 = aligned(length)
                    ARM64.ADD_imm (ARM64.X14, ARM64.X12, 8us)        // X14 = addr + 8
                    ARM64.ADD_reg (ARM64.X14, ARM64.X14, ARM64.X15)  // X14 = addr + 8 + aligned(length) (refcount addr)
                    ARM64.LDR (ARM64.X15, ARM64.X14, 0s)             // X15 = refcount
                    // Load sentinel value 0x7FFFFFFFFFFFFFFF (INT64_MAX) into X13
                    ARM64.MOVZ (ARM64.X13, 0xFFFFus, 0)
                    ARM64.MOVK (ARM64.X13, 0xFFFFus, 16)
                    ARM64.MOVK (ARM64.X13, 0xFFFFus, 32)
                    ARM64.MOVK (ARM64.X13, 0x7FFFus, 48)
                    ARM64.CMP_reg (ARM64.X15, ARM64.X13)             // Compare with sentinel
                    ARM64.B_cond (ARM64.EQ, 3)                       // If pool string, skip to end
                    ARM64.ADD_imm (ARM64.X15, ARM64.X15, 1us)        // X15++
                    ARM64.STR (ARM64.X15, ARM64.X14, 0s)             // store back
                ])
        | _ -> Error "RefCountIncString requires StringRef or Reg operand"

    | LIR.RefCountDecString str ->
        // Decrement refcount for a heap string
        // Heap string layout: [length:8][data:N][padding:P][refcount:8] where P aligns to 8
        // Pool strings have refcount = INT64_MAX as sentinel (don't modify read-only memory)
        match str with
        | LIR.StringRef _ ->
            // Pool string - no refcount, no-op
            Ok []
        | LIR.Reg reg ->
            // Heap or pool string - refcount is at [addr + 8 + aligned(length)]
            lirRegToARM64Reg reg
            |> Result.map (fun addrReg ->
                [
                    // Save address to X12 in case addrReg is X13/X14/X15 which we clobber
                    ARM64.MOV_reg (ARM64.X12, addrReg)               // X12 = string address
                    ARM64.LDR (ARM64.X15, ARM64.X12, 0s)             // X15 = length
                    // Align length: X15 = ((X15 + 7) >> 3) << 3
                    ARM64.ADD_imm (ARM64.X15, ARM64.X15, 7us)        // X15 = length + 7
                    ARM64.MOVZ (ARM64.X13, 3us, 0)                   // X13 = 3 (shift amount)
                    ARM64.LSR_reg (ARM64.X15, ARM64.X15, ARM64.X13)  // X15 = (length + 7) >> 3
                    ARM64.LSL_reg (ARM64.X15, ARM64.X15, ARM64.X13)  // X15 = aligned(length)
                    ARM64.ADD_imm (ARM64.X14, ARM64.X12, 8us)        // X14 = addr + 8
                    ARM64.ADD_reg (ARM64.X14, ARM64.X14, ARM64.X15)  // X14 = addr + 8 + aligned(length) (refcount addr)
                    ARM64.LDR (ARM64.X15, ARM64.X14, 0s)             // X15 = refcount
                    // Load sentinel value 0x7FFFFFFFFFFFFFFF (INT64_MAX) into X13
                    ARM64.MOVZ (ARM64.X13, 0xFFFFus, 0)
                    ARM64.MOVK (ARM64.X13, 0xFFFFus, 16)
                    ARM64.MOVK (ARM64.X13, 0xFFFFus, 32)
                    ARM64.MOVK (ARM64.X13, 0x7FFFus, 48)
                    ARM64.CMP_reg (ARM64.X15, ARM64.X13)             // Compare with sentinel
                    ARM64.B_cond (ARM64.EQ, 3)                       // If pool string, skip to end
                    ARM64.SUB_imm (ARM64.X15, ARM64.X15, 1us)        // X15--
                    ARM64.STR (ARM64.X15, ARM64.X14, 0s)             // store back
                    // Note: When refcount hits 0, we should free the memory.
                    // For now, we skip this as strings have variable size.
                ])
        | _ -> Error "RefCountDecString requires StringRef or Reg operand"

    | LIR.RandomInt64 dest ->
        // Generate random 8 bytes as Int64
        lirRegToARM64Reg dest
        |> Result.map (fun destReg ->
            Runtime.generateRandomInt64 destReg)

    | LIR.CoverageHit exprId ->
        // Increment coverage counter at _coverage_data[exprId * 8]
        // Uses PC-relative addressing (ADRP+ADD) to get BSS buffer address
        // Uses X9 and X10 as scratch registers
        let offset = exprId * 8
        Ok ([
            // Get address of coverage buffer using PC-relative addressing
            ARM64.ADRP (ARM64.X9, "_coverage_data")
            ARM64.ADD_label (ARM64.X9, ARM64.X9, "_coverage_data")
        ] @
        // Add offset for this expression's counter
        (if offset = 0 then
            []
        elif offset < 4096 then
            [ARM64.ADD_imm (ARM64.X9, ARM64.X9, uint16 offset)]
        else
            loadImmediate ARM64.X10 (int64 offset) @ [ARM64.ADD_reg (ARM64.X9, ARM64.X9, ARM64.X10)]) @
        [
            ARM64.LDR (ARM64.X10, ARM64.X9, 0s)        // X10 = coverage_buffer[exprId]
            ARM64.ADD_imm (ARM64.X10, ARM64.X10, 1us)  // X10++
            ARM64.STR (ARM64.X10, ARM64.X9, 0s)        // coverage_buffer[exprId] = X10
        ])

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
            let (LIR.Label trueLbl) = trueLabel
            let (LIR.Label falseLbl) = falseLabel
            [
                ARM64.CBNZ (arm64Reg, trueLbl)  // If true, jump to then branch
                ARM64.B_label falseLbl           // Otherwise jump to else branch
            ])

    | LIR.BranchZero (condReg, zeroLabel, nonZeroLabel) ->
        // Branch if register is zero, otherwise fall through to non-zero case
        // Use CBZ (compare and branch if zero) to zero label
        // Then unconditional branch to non-zero label
        lirRegToARM64Reg condReg
        |> Result.map (fun arm64Reg ->
            let (LIR.Label zeroLbl) = zeroLabel
            let (LIR.Label nonZeroLbl) = nonZeroLabel
            [
                ARM64.CBZ (arm64Reg, zeroLbl)    // If zero, jump to zero branch
                ARM64.B_label nonZeroLbl          // Otherwise jump to non-zero branch
            ])

    | LIR.Jump label ->
        let (LIR.Label lbl) = label
        Ok [ARM64.B_label lbl]

    | LIR.CondBranch (cond, trueLabel, falseLabel) ->
        // Branch based on condition flags (set by previous CMP)
        // Use B.cond to true label, then unconditional branch to false label
        let (LIR.Label trueLbl) = trueLabel
        let (LIR.Label falseLbl) = falseLabel
        let arm64Cond =
            match cond with
            | LIR.EQ -> ARM64.EQ
            | LIR.NE -> ARM64.NE
            | LIR.LT -> ARM64.LT
            | LIR.GT -> ARM64.GT
            | LIR.LE -> ARM64.LE
            | LIR.GE -> ARM64.GE
        Ok [
            ARM64.B_cond_label (arm64Cond, trueLbl)  // If condition, jump to true branch
            ARM64.B_label falseLbl                   // Otherwise jump to false branch
        ]

/// Convert LIR basic block to ARM64 instructions (with label)
/// epilogueLabel: passed through to terminator for Ret handling
let convertBlock (ctx: CodeGenContext) (epilogueLabel: string) (block: LIR.BasicBlock) : Result<ARM64.Instr list, string> =
    // Emit label for this block
    let (LIR.Label lbl) = block.Label
    let labelInstr = ARM64.Label lbl

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

    // Convert each block (accumulate in reverse to avoid O(n²) list concat)
    let rec convertBlocks acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc)
        | block :: rest ->
            match convertBlock ctx epilogueLabel block with
            | Error err -> Error err
            | Ok instrs -> convertBlocks (List.rev instrs @ acc) rest
    convertBlocks [] allBlocks

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
        // Note: We use SUB_imm12 (shifted immediate) because:
        // 1. SUB_reg with SP doesn't work - register 31 is interpreted as XZR, not SP
        // 2. 64KB = 16 * 4096, so we use SUB_imm12 with imm=16 (value is imm << 12)
        ARM64.SUB_imm12 (ARM64.SP, ARM64.SP, 16us)  // SP -= 16 * 4096 = 64KB
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
    let epilogueLabel = "_epilogue_" + func.Name

    // Create function-specific context with stack info for tail call epilogue generation
    let funcCtx = { ctx with StackSize = func.StackSize; UsedCalleeSaved = func.UsedCalleeSaved }

    // Convert CFG to ARM64 instructions
    match convertCFG funcCtx epilogueLabel func.CFG with
    | Error err -> Error err
    | Ok cfgInstrs ->
        // Generate prologue (save FP/LR, allocate stack)
        let prologue = generatePrologue func.UsedCalleeSaved func.StackSize

        // Generate heap initialization for _start only
        let heapInit =
            if func.Name = "_start" then generateHeapInit ()
            else []

        // Note: Coverage buffer is in BSS section (zero-initialized by OS)
        // No runtime initialization needed - CoverageHit uses ADRP+ADD to access it

        // Generate parameter setup: move X0-X7/D0-D7 to allocated parameter registers
        // This must come AFTER the prologue but BEFORE the function body
        // Strategy: Save all source registers to temp regs first to avoid clobbering
        let argRegs = [ARM64.X0; ARM64.X1; ARM64.X2; ARM64.X3; ARM64.X4; ARM64.X5; ARM64.X6; ARM64.X7]
        let tempRegs = [ARM64.X9; ARM64.X10; ARM64.X11; ARM64.X12; ARM64.X13; ARM64.X14; ARM64.X15]

        // AAPCS64: int and float use SEPARATE register counters
        let paramsWithTypes = List.zip func.Params func.ParamTypes

        // Collect integer parameters with their calling convention index
        let intParamsWithIdx =
            paramsWithTypes
            |> List.indexed
            |> List.fold (fun (intIdx, acc) (_, (param, typ)) ->
                if typ = AST.TFloat64 then
                    (intIdx, acc)  // Skip float params
                else
                    (intIdx + 1, (param, intIdx) :: acc)
            ) (0, [])
            |> snd
            |> List.rev

        // Step 1a: Save integer calling convention registers to temps
        let saveIntToTemps =
            intParamsWithIdx
            |> List.map (fun (_, intIdx) ->
                let argReg = List.item intIdx argRegs
                let tempReg = List.item intIdx tempRegs
                ARM64.MOV_reg (tempReg, argReg))

        // Note: Float parameter setup is NOT done here - it's handled by RegisterAllocation
        // which inserts FMov instructions at the start of the CFG entry block.
        // Doing it here would corrupt D0/D1 before those CFG instructions run.

        // Step 2a: Move integers from temps to allocated parameter registers
        let moveIntFromTemps =
            intParamsWithIdx
            |> List.map (fun (paramReg, intIdx) ->
                let tempReg = List.item intIdx tempRegs
                match lirRegToARM64Reg paramReg with
                | Ok paramArm64 ->
                    if paramArm64 = tempReg then
                        []  // Already in the right place
                    else
                        [ARM64.MOV_reg (paramArm64, tempReg)]
                | Error _ -> [])  // Shouldn't happen
            |> List.concat

        let paramSetup = saveIntToTemps @ moveIntFromTemps

        // Generate epilogue (deallocate stack, restore FP/LR, return or exit)
        let epilogueLabelInstr = [ARM64.Label ("_epilogue_" + func.Name)]
        let epilogue =
            if func.Name = "_start" then
                // For _start, flush coverage (if enabled) then exit instead of return
                let coverageFlush =
                    if ctx.Options.EnableCoverage then
                        Runtime.generateCoverageFlush ctx.Options.CoverageExprCount
                    else []
                generateEpilogue func.UsedCalleeSaved func.StackSize
                |> List.filter (function ARM64.RET -> false | _ -> true)  // Remove RET
                |> fun instrs -> instrs @ coverageFlush @ Runtime.generateExit ()
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
    // StackSize and UsedCalleeSaved are set per-function in convertFunction
    let ctx = { Options = options; StringPool = stringPool; StackSize = 0; UsedCalleeSaved = [] }

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
