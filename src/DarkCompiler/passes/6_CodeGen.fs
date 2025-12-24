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
        // Print X0 value to stdout, then exit
        // Convert integer in X0 to ASCII decimal string, handle negative numbers
        [
            // Allocate 32 bytes on stack for buffer (plenty for 64-bit number + sign + newline)
            ARM64.SUB_imm (ARM64.SP, ARM64.SP, 32us)

            // Setup: X1 = buffer pointer (start at end, work backwards)
            // X2 = value to print (from X0)
            ARM64.ADD_imm (ARM64.X1, ARM64.SP, 31us)  // X1 = SP + 31 (end of buffer)
            ARM64.MOV_reg (ARM64.X2, ARM64.X0)  // X2 = value to convert

            // Store newline at end of buffer
            ARM64.MOVZ (ARM64.X3, 10us, 0)  // '\n' = 10
            ARM64.STRB (ARM64.X3, ARM64.X1, 0)
            ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)  // Move pointer back

            // Instruction 6-7: Check if negative and set flag in X6
            ARM64.MOVZ (ARM64.X6, 0us, 0)  // 6: X6 = 0 (assume positive)
            ARM64.TBNZ (ARM64.X2, 63, 29)  // 7: If negative, branch forward +29 to inst 36 (handle_negative)

            // Instruction 8: Check if zero (branch forward +24 to print_zero at inst 32)
            ARM64.CBZ (ARM64.X2, 24)

            // Instruction 9-17: convert_loop - Extract digits by dividing by 10
            ARM64.MOVZ (ARM64.X3, 10us, 0)  // 9: divisor = 10
            ARM64.UDIV (ARM64.X4, ARM64.X2, ARM64.X3)  // 10: X4 = value / 10
            ARM64.MSUB (ARM64.X5, ARM64.X4, ARM64.X3, ARM64.X2)  // 11: X5 = value % 10
            ARM64.ADD_imm (ARM64.X5, ARM64.X5, 48us)  // 12: Convert to ASCII
            ARM64.STRB (ARM64.X5, ARM64.X1, 0)  // 13: Store digit
            ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)  // 14: Move pointer back
            ARM64.MOV_reg (ARM64.X2, ARM64.X4)  // 15: value = value / 10
            ARM64.CBZ (ARM64.X2, 2)  // 16: If zero, skip (+2) to store_minus_if_needed at inst 18
            ARM64.B (-8)  // 17: Loop back (-8) to inst 9 (convert_loop start)

            // Instruction 18-21: store_minus_if_needed - If X6=1 (was negative), store '-'
            ARM64.CBZ (ARM64.X6, 4)  // 18: If X6=0 (positive), skip (+4) to write_output at inst 22
            ARM64.MOVZ (ARM64.X3, 45us, 0)  // 19: '-' = ASCII 45
            ARM64.STRB (ARM64.X3, ARM64.X1, 0)  // 20: Store '-' at X1
            ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)  // 21: Move pointer back

            // Fall through to write_output
        ]
        @ [
            // Instruction 22-29: write_output - Write buffer to stdout
            ARM64.ADD_imm (ARM64.X1, ARM64.X1, 1us)  // 22: X1 was one past first char
            ARM64.ADD_imm (ARM64.X2, ARM64.SP, 32us)  // 23: End of buffer
            ARM64.SUB_reg (ARM64.X2, ARM64.X2, ARM64.X1)  // 24: length = end - start
            ARM64.MOVZ (ARM64.X0, 1us, 0)  // 25: stdout = 1
            ARM64.MOVZ (ARM64.X16, 4us, 0)  // 26: write syscall = 4
            ARM64.SVC 0x80us  // 27: call write
            ARM64.ADD_imm (ARM64.SP, ARM64.SP, 32us)  // 28: Deallocate stack
            ARM64.MOVZ (ARM64.X0, 0us, 0)  // 29: exit code = 0
            ARM64.MOVZ (ARM64.X16, 1us, 0)  // 30: exit syscall = 1
            ARM64.SVC 0x80us  // 31: call exit
        ]
        @ [
            // Instruction 32-35: print_zero - Special case for value 0
            ARM64.MOVZ (ARM64.X2, 48us, 0)  // 32: '0' = 48
            ARM64.STRB (ARM64.X2, ARM64.X1, 0)  // 33: Store '0'
            ARM64.SUB_imm (ARM64.X1, ARM64.X1, 1us)  // 34: Move pointer back
            ARM64.B (-13)  // 35: Branch back (-13) to inst 22 (write_output)
        ]
        @ [
            // Instruction 36-38: handle_negative - Negate value and set flag
            ARM64.NEG (ARM64.X2, ARM64.X2)  // 36: X2 = -X2 (get absolute value)
            ARM64.MOVZ (ARM64.X6, 1us, 0)  // 37: X6 = 1 (negative flag)
            ARM64.B (-30)  // 38: Branch back (-30) to inst 8 (CBZ zero check)
        ]

/// Convert LIR function to ARM64 instructions
let convertFunction (func: LIR.Function) : ARM64.Instr list =
    func.Body |> List.collect convertInstr

/// Convert LIR program to ARM64 instructions
let generateARM64 (program: LIR.Program) : ARM64.Instr list =
    let (LIR.Program functions) = program
    functions |> List.collect convertFunction
