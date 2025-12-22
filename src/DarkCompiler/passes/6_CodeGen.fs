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
let lirRegToARM64Reg (reg: LIR.Reg) : ARM64.Reg =
    match reg with
    | LIR.Physical physReg -> lirPhysRegToARM64Reg physReg
    | LIR.Virtual _ -> failwith "Virtual registers should have been allocated"

/// Generate ARM64 instructions to load an immediate into a register
let loadImmediate (dest: ARM64.Reg) (value: int64) : ARM64.Instr list =
    // For now, handle simple case with MOVZ
    // TODO: Handle large constants with MOVZ+MOVK
    if value >= 0L && value < 65536L then
        [ARM64.MOVZ (dest, uint16 value, 0)]
    else
        // For now, just use MOVZ with lower 16 bits
        // A full implementation would use MOVZ + MOVK for all 64 bits
        [ARM64.MOVZ (dest, uint16 (value &&& 0xFFFFL), 0)]

/// Convert LIR instruction to ARM64 instructions
let convertInstr (instr: LIR.Instr) : ARM64.Instr list =
    match instr with
    | LIR.Mov (dest, src) ->
        let destReg = lirRegToARM64Reg dest
        match src with
        | LIR.Imm value ->
            loadImmediate destReg value
        | LIR.Reg srcReg ->
            let srcARM64 = lirRegToARM64Reg srcReg
            [ARM64.MOV_reg (destReg, srcARM64)]
        | LIR.StackSlot _ ->
            failwith "Stack slots not yet supported"

    | LIR.Add (dest, left, right) ->
        let destReg = lirRegToARM64Reg dest
        let leftReg = lirRegToARM64Reg left
        match right with
        | LIR.Imm value when value >= 0L && value < 4096L ->
            // Can use immediate ADD
            [ARM64.ADD_imm (destReg, leftReg, uint16 value)]
        | LIR.Imm value ->
            // Need to load immediate into register first
            let tempReg = ARM64.X9  // Use X9 as temp
            loadImmediate tempReg value @ [ARM64.ADD_reg (destReg, leftReg, tempReg)]
        | LIR.Reg rightReg ->
            let rightARM64 = lirRegToARM64Reg rightReg
            [ARM64.ADD_reg (destReg, leftReg, rightARM64)]
        | LIR.StackSlot _ ->
            failwith "Stack slots not yet supported"

    | LIR.Sub (dest, left, right) ->
        let destReg = lirRegToARM64Reg dest
        let leftReg = lirRegToARM64Reg left
        match right with
        | LIR.Imm value when value >= 0L && value < 4096L ->
            [ARM64.SUB_imm (destReg, leftReg, uint16 value)]
        | LIR.Imm value ->
            let tempReg = ARM64.X9
            loadImmediate tempReg value @ [ARM64.SUB_reg (destReg, leftReg, tempReg)]
        | LIR.Reg rightReg ->
            let rightARM64 = lirRegToARM64Reg rightReg
            [ARM64.SUB_reg (destReg, leftReg, rightARM64)]
        | LIR.StackSlot _ ->
            failwith "Stack slots not yet supported"

    | LIR.Mul (dest, left, right) ->
        let destReg = lirRegToARM64Reg dest
        let leftReg = lirRegToARM64Reg left
        let rightReg = lirRegToARM64Reg right
        [ARM64.MUL (destReg, leftReg, rightReg)]

    | LIR.Sdiv (dest, left, right) ->
        let destReg = lirRegToARM64Reg dest
        let leftReg = lirRegToARM64Reg left
        let rightReg = lirRegToARM64Reg right
        [ARM64.SDIV (destReg, leftReg, rightReg)]

    | LIR.Ret ->
        [ARM64.RET]

/// Convert LIR function to ARM64 instructions
let convertFunction (func: LIR.Function) : ARM64.Instr list =
    func.Body |> List.collect convertInstr

/// Convert LIR program to ARM64 instructions
let generateARM64 (program: LIR.Program) : ARM64.Instr list =
    let (LIR.Program functions) = program
    functions |> List.collect convertFunction
