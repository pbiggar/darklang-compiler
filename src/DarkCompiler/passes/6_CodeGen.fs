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
    | LIR.X29 -> ARM64.X29
    | LIR.X30 -> ARM64.X30
    | LIR.SP -> ARM64.SP

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

    // Build instruction sequence
    // Start with MOVZ for the first non-zero chunk (or chunk0 if all zero)
    let mutable instrs = [ARM64.MOVZ (dest, chunk0, 0)]

    // Add MOVK for remaining chunks if non-zero
    if chunk1 <> 0us then
        instrs <- instrs @ [ARM64.MOVK (dest, chunk1, 16)]
    if chunk2 <> 0us then
        instrs <- instrs @ [ARM64.MOVK (dest, chunk2, 32)]
    if chunk3 <> 0us then
        instrs <- instrs @ [ARM64.MOVK (dest, chunk3, 48)]

    instrs

/// Generate ARM64 instructions to load a stack slot into a register
/// Stack slots are accessed relative to FP (X29)
/// Uses LDUR for signed offsets (stack slots are negative from FP)
let loadStackSlot (dest: ARM64.Reg) (offset: int) : ARM64.Instr list =
    // LDUR dest, [X29, #offset] - signed offset version
    // Offset is in bytes, can be negative (-256 to +255)
    [ARM64.LDUR (dest, ARM64.X29, int16 offset)]

/// Generate ARM64 instructions to store a register to a stack slot
/// Stack slots are accessed relative to FP (X29)
/// Uses STUR for signed offsets (stack slots are negative from FP)
let storeStackSlot (src: ARM64.Reg) (offset: int) : ARM64.Instr list =
    // STUR src, [X29, #offset] - signed offset version
    // Offset is in bytes, can be negative (-256 to +255)
    [ARM64.STUR (src, ARM64.X29, int16 offset)]

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
    | LIR.StackSlot offset ->
        // Load stack slot into X9
        Ok (ARM64.X9, loadStackSlot ARM64.X9 offset)

/// Generate function prologue
/// Saves FP, LR, callee-saved registers, and allocates stack space
let generatePrologue (usedCalleeSaved: LIR.PhysReg list) (stackSize: int) : ARM64.Instr list =
    // Prologue sequence:
    // 1. Decrement SP and save FP (X29) and LR (X30)
    // 2. Set FP = SP: MOV X29, SP
    // 3. Save callee-saved registers (if any): STP X19, X20, [SP, #-16]! ...
    // 4. Allocate stack space for spills: SUB SP, SP, #stackSize

    let saveFpLr = [
        ARM64.SUB_imm (ARM64.SP, ARM64.SP, 16us)  // Decrement SP by 16
        ARM64.STP (ARM64.X29, ARM64.X30, ARM64.SP, 0s)  // Store at [SP]
    ]
    let setFp = [ARM64.MOV_reg (ARM64.X29, ARM64.SP)]

    // TODO: Save callee-saved registers when we add X19-X28 to LIR.PhysReg
    let saveCalleeSaved = []

    let allocStack =
        if stackSize > 0 then
            [ARM64.SUB_imm (ARM64.SP, ARM64.SP, uint16 stackSize)]
        else
            []

    saveFpLr @ setFp @ saveCalleeSaved @ allocStack

/// Generate function epilogue
/// Restores callee-saved registers, FP, LR, and returns
let generateEpilogue (usedCalleeSaved: LIR.PhysReg list) (stackSize: int) : ARM64.Instr list =
    // Epilogue sequence (reverse of prologue):
    // 1. Deallocate stack: ADD SP, SP, #stackSize
    // 2. Restore callee-saved registers: LDP X19, X20, [SP], #16 ...
    // 3. Restore FP and LR from [SP], then increment SP
    // 4. Return: RET

    let deallocStack =
        if stackSize > 0 then
            [ARM64.ADD_imm (ARM64.SP, ARM64.SP, uint16 stackSize)]
        else
            []

    // TODO: Restore callee-saved registers when we add X19-X28 to LIR.PhysReg
    let restoreCalleeSaved = []

    let restoreFpLr = [
        ARM64.LDP (ARM64.X29, ARM64.X30, ARM64.SP, 0s)  // Load from [SP]
        ARM64.ADD_imm (ARM64.SP, ARM64.SP, 16us)  // Increment SP by 16
    ]
    let ret = [ARM64.RET]

    deallocStack @ restoreCalleeSaved @ restoreFpLr @ ret

/// Convert LIR instruction to ARM64 instructions
let convertInstr (instr: LIR.Instr) : Result<ARM64.Instr list, string> =
    match instr with
    | LIR.Mov (dest, src) ->
        lirRegToARM64Reg dest
        |> Result.bind (fun destReg ->
            match src with
            | LIR.Imm value ->
                Ok (loadImmediate destReg value)
            | LIR.Reg srcReg ->
                lirRegToARM64Reg srcReg
                |> Result.map (fun srcARM64 -> [ARM64.MOV_reg (destReg, srcARM64)])
            | LIR.StackSlot offset ->
                // Load from stack slot into destination register
                Ok (loadStackSlot destReg offset))

    | LIR.Store (offset, src) ->
        // Store register to stack slot
        lirRegToARM64Reg src
        |> Result.map (fun srcReg -> storeStackSlot srcReg offset)

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
                | LIR.Reg rightReg ->
                    lirRegToARM64Reg rightReg
                    |> Result.map (fun rightARM64 -> [ARM64.ADD_reg (destReg, leftReg, rightARM64)])
                | LIR.StackSlot offset ->
                    // Load stack slot into temp register, then add
                    let tempReg = ARM64.X9
                    Ok (loadStackSlot tempReg offset @ [ARM64.ADD_reg (destReg, leftReg, tempReg)])))

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
                | LIR.Reg rightReg ->
                    lirRegToARM64Reg rightReg
                    |> Result.map (fun rightARM64 -> [ARM64.SUB_reg (destReg, leftReg, rightARM64)])
                | LIR.StackSlot offset ->
                    // Load stack slot into temp register, then subtract
                    let tempReg = ARM64.X9
                    Ok (loadStackSlot tempReg offset @ [ARM64.SUB_reg (destReg, leftReg, tempReg)])))


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
            | LIR.Reg rightReg ->
                lirRegToARM64Reg rightReg
                |> Result.map (fun rightARM64 -> [ARM64.CMP_reg (leftReg, rightARM64)])
            | LIR.StackSlot offset ->
                // Load stack slot into temp register, then compare
                let tempReg = ARM64.X9
                Ok (loadStackSlot tempReg offset @ [ARM64.CMP_reg (leftReg, tempReg)]))

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
        // For now, print booleans as 0 or 1 (same as PrintInt)
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            if regARM64 <> ARM64.X0 then
                [ARM64.MOV_reg (ARM64.X0, regARM64)] @ Runtime.generatePrintInt ()
            else
                Runtime.generatePrintInt ())

    | LIR.Call (dest, funcName, args) ->
        // Function call: arguments already moved to X0-X7 by LIR pass
        // Just generate BL instruction
        // Return value will be in X0, already moved to dest by LIR pass
        Ok [ARM64.BL funcName]

    | LIR.PrintInt reg ->
        // Value to print should be in X0
        lirRegToARM64Reg reg
        |> Result.map (fun regARM64 ->
            if regARM64 <> ARM64.X0 then
                // Move to X0 if not already there
                [ARM64.MOV_reg (ARM64.X0, regARM64)] @ Runtime.generatePrintInt ()
            else
                Runtime.generatePrintInt ())

/// Convert LIR terminator to ARM64 instructions
let convertTerminator (terminator: LIR.Terminator) : Result<ARM64.Instr list, string> =
    match terminator with
    | LIR.Ret ->
        // Don't generate RET here - the function epilogue handles return
        Ok []

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
let convertBlock (block: LIR.BasicBlock) : Result<ARM64.Instr list, string> =
    // Emit label for this block
    let labelInstr = ARM64.Label block.Label

    // Convert all instructions
    let instrResults = block.Instrs |> List.map convertInstr

    // Convert terminator
    let termResult = convertTerminator block.Terminator

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
let convertCFG (cfg: LIR.CFG) : Result<ARM64.Instr list, string> =
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
    |> List.map convertBlock
    |> List.fold (fun acc result ->
        match acc, result with
        | Ok instrs, Ok newInstrs -> Ok (instrs @ newInstrs)
        | Error err, _ -> Error err
        | _, Error err -> Error err) (Ok [])

/// Convert LIR function to ARM64 instructions with prologue and epilogue
let convertFunction (func: LIR.Function) : Result<ARM64.Instr list, string> =
    // Convert CFG to ARM64 instructions
    match convertCFG func.CFG with
    | Error err -> Error err
    | Ok cfgInstrs ->
        // Generate prologue (save FP/LR, allocate stack)
        let prologue = generatePrologue func.UsedCalleeSaved func.StackSize

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
        let epilogue = generateEpilogue func.UsedCalleeSaved func.StackSize

        // Add function entry label (for BL to branch to)
        let functionEntryLabel = [ARM64.Label func.Name]

        // Combine: function label + prologue + param setup + CFG body + epilogue
        // Note: The RET in epilogue replaces any RET in the CFG body
        // TODO: Handle multiple return points properly when we have complex control flow
        Ok (functionEntryLabel @ prologue @ paramSetup @ cfgInstrs @ epilogue)

/// Convert LIR program to ARM64 instructions
let generateARM64 (program: LIR.Program) : Result<ARM64.Instr list, string> =
    let (LIR.Program functions) = program
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
