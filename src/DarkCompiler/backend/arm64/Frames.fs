// Frames.fs - Generate aligned frames and callee-saved register handling.

module ARM64Frames

open ARM64Operands

/// Generate STP instructions to save callee-saved register pairs
/// Returns instructions and total bytes pushed
let generateCalleeSavedSaves (regs: LIR.PhysReg list) : ARM64Symbolic.Instr list * int =
    // Process in pairs. If odd number, pad with X27 (or just save single)
    let rec savePairs (remaining: LIR.PhysReg list) (offset: int) (acc: ARM64Symbolic.Instr list) =
        match remaining with
        | [] -> (List.rev acc, offset)
        | [single] ->
            // Single register: use STR instead of STP
            let instr = ARM64Symbolic.STR (lirPhysRegToARM64Reg single, ARM64Symbolic.SP, int16 offset)
            (List.rev (instr :: acc), offset + 8)
        | r1 :: r2 :: rest ->
            let instr = ARM64Symbolic.STP (lirPhysRegToARM64Reg r1, lirPhysRegToARM64Reg r2, ARM64Symbolic.SP, int16 offset)
            savePairs rest (offset + 16) (instr :: acc)

    if List.isEmpty regs then
        ([], 0)
    else
        savePairs regs 0 []

/// Generate LDP instructions to restore callee-saved register pairs
let generateCalleeSavedRestores (regs: LIR.PhysReg list) : ARM64Symbolic.Instr list =
    let rec restorePairs (remaining: LIR.PhysReg list) (offset: int) (acc: ARM64Symbolic.Instr list) =
        match remaining with
        | [] -> List.rev acc
        | [single] ->
            let instr = ARM64Symbolic.LDR (lirPhysRegToARM64Reg single, ARM64Symbolic.SP, int16 offset)
            List.rev (instr :: acc)
        | r1 :: r2 :: rest ->
            let instr = ARM64Symbolic.LDP (lirPhysRegToARM64Reg r1, lirPhysRegToARM64Reg r2, ARM64Symbolic.SP, int16 offset)
            restorePairs rest (offset + 16) (instr :: acc)

    if List.isEmpty regs then []
    else restorePairs regs 0 []

/// Calculate stack space needed for callee-saved registers (16-byte aligned)
let calleeSavedStackSpace (regs: LIR.PhysReg list) : int =
    let count = List.length regs
    if count = 0 then 0
    else ((count * 8 + 15) / 16) * 16  // 16-byte aligned

let floatCalleeSavedStackSpace (regs: LIR.PhysFPReg list) : int =
    let count = List.length regs
    if count = 0 then 0 else ((count * 8 + 15) / 16) * 16

let generateFloatCalleeSavedSaves (regs: LIR.PhysFPReg list) (baseOffset: int) : ARM64Symbolic.Instr list =
    let rec emit remaining offset =
        match remaining with
        | [] -> []
        | [reg] ->
            [ARM64Symbolic.STR_fp (lirPhysFPRegToARM64FReg reg, ARM64Symbolic.SP, int16 offset)]
        | first :: second :: rest ->
            ARM64Symbolic.STP_fp
                (lirPhysFPRegToARM64FReg first,
                 lirPhysFPRegToARM64FReg second,
                 ARM64Symbolic.SP,
                 int16 offset)
            :: emit rest (offset + 16)
    emit regs baseOffset

let generateFloatCalleeSavedRestores (regs: LIR.PhysFPReg list) (baseOffset: int) : ARM64Symbolic.Instr list =
    let rec emit remaining offset =
        match remaining with
        | [] -> []
        | [reg] ->
            [ARM64Symbolic.LDR_fp (lirPhysFPRegToARM64FReg reg, ARM64Symbolic.SP, int16 offset)]
        | first :: second :: rest ->
            ARM64Symbolic.LDP_fp
                (lirPhysFPRegToARM64FReg first,
                 lirPhysFPRegToARM64FReg second,
                 ARM64Symbolic.SP,
                 int16 offset)
            :: emit rest (offset + 16)
    emit regs baseOffset

/// Generate function prologue
/// Saves FP, LR, callee-saved registers, and allocates stack space
let generatePrologue (usedCalleeSaved: LIR.PhysReg list) (usedCalleeSavedF: LIR.PhysFPReg list) (stackSize: int) : ARM64Symbolic.Instr list =
    // Prologue sequence:
    // 1. Save FP (X29) and LR (X30) with pre-indexed addressing (combines SUB and STP)
    // 2. Set FP = SP: MOV X29, SP
    // 3. Allocate stack space for spills and callee-saved registers
    // 4. Save callee-saved registers

    // Use pre-indexed STP to save FP/LR and decrement SP in one instruction
    let saveFpLr = [ARM64Symbolic.STP_pre (ARM64Symbolic.X29, ARM64Symbolic.X30, ARM64Symbolic.SP, -16s)]
    let setFp = [ARM64Symbolic.MOV_reg (ARM64Symbolic.X29, ARM64Symbolic.SP)]

    // Calculate total additional stack space needed
    let calleeSavedSpace = calleeSavedStackSpace usedCalleeSaved
    let totalExtraStack = stackSize + calleeSavedSpace + floatCalleeSavedStackSpace usedCalleeSavedF

    // Allocate all stack space at once (for spills + callee-saved)
    let allocStack =
        if totalExtraStack > 0 then
            [ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 totalExtraStack)]
        else
            []

    // Save callee-saved registers at [SP]
    // (callee-saved are at the bottom of the frame, spill space is above them)
    let (saveCalleeSavedInstrs, _) = generateCalleeSavedSaves usedCalleeSaved
    let saveFloatInstrs = generateFloatCalleeSavedSaves usedCalleeSavedF calleeSavedSpace

    saveFpLr @ setFp @ allocStack @ saveCalleeSavedInstrs @ saveFloatInstrs

/// Generate function epilogue
/// Restores callee-saved registers, FP, LR, and returns
let generateEpilogue (usedCalleeSaved: LIR.PhysReg list) (usedCalleeSavedF: LIR.PhysFPReg list) (stackSize: int) : ARM64Symbolic.Instr list =
    // Epilogue sequence (reverse of prologue):
    // 1. Restore callee-saved registers from [SP + stackSize]
    // 2. Deallocate stack space (spills + callee-saved) at once
    // 3. Restore FP and LR with post-indexed addressing (combines LDP and ADD)
    // 4. Return: RET

    // Restore callee-saved registers from [SP]
    // (callee-saved are at the bottom of the frame, spill space is above them)
    let calleeSavedSpace = calleeSavedStackSpace usedCalleeSaved
    let restoreCalleeSavedInstrs = generateCalleeSavedRestores usedCalleeSaved
    let restoreFloatInstrs = generateFloatCalleeSavedRestores usedCalleeSavedF calleeSavedSpace

    // Deallocate all stack space at once
    let totalExtraStack = stackSize + calleeSavedSpace + floatCalleeSavedStackSpace usedCalleeSavedF
    let deallocStack =
        if totalExtraStack > 0 then
            [ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, uint16 totalExtraStack)]
        else
            []

    // Use post-indexed LDP to restore FP/LR and increment SP in one instruction
    let restoreFpLr = [ARM64Symbolic.LDP_post (ARM64Symbolic.X29, ARM64Symbolic.X30, ARM64Symbolic.SP, 16s)]
    let ret = [ARM64Symbolic.RET]

    restoreFloatInstrs @ restoreCalleeSavedInstrs @ deallocStack @ restoreFpLr @ ret
