// 4_MIR_to_LIR.fs - Instruction Selection (Pass 4)
//
// Transforms MIR into LIR.
//
// Instruction selection algorithm:
// - Selects appropriate ARM64 instructions for each MIR operation
// - Handles ARM64 operand constraints:
//   - ADD/SUB: support 12-bit immediates, left operand must be register
//   - MUL/SDIV: both operands must be registers
// - Inserts MOV instructions to load immediates when needed
// - Emits return value in X0 register per ARM64 calling convention
//
// Example:
//   v0 <- 42; v1 <- v0 + 100
//   →
//   v0 <- Mov(Imm 42); v1 <- Add(v0, Imm 100)

module MIR_to_LIR

/// Convert MIR.VReg to LIR.Reg (virtual)
let vregToLIRReg (MIR.VReg id) : LIR.Reg = LIR.Virtual id

/// Convert MIR.Operand to LIR.Operand
let convertOperand (operand: MIR.Operand) : LIR.Operand =
    match operand with
    | MIR.IntConst n -> LIR.Imm n
    | MIR.BoolConst b -> LIR.Imm (if b then 1L else 0L)  // Booleans as 0/1
    | MIR.Register vreg -> LIR.Reg (vregToLIRReg vreg)

/// Ensure operand is in a register (may need to load immediate)
let ensureInRegister (operand: MIR.Operand) (tempReg: LIR.Reg) : LIR.Instr list * LIR.Reg =
    match operand with
    | MIR.IntConst n ->
        // Need to load constant into a temporary register
        ([LIR.Mov (tempReg, LIR.Imm n)], tempReg)
    | MIR.BoolConst b ->
        // Load boolean (0 or 1) into register
        ([LIR.Mov (tempReg, LIR.Imm (if b then 1L else 0L))], tempReg)
    | MIR.Register vreg ->
        ([], vregToLIRReg vreg)

/// Convert MIR instruction to LIR instructions
let selectInstr (instr: MIR.Instr) : LIR.Instr list =
    match instr with
    | MIR.Mov (dest, src) ->
        let lirDest = vregToLIRReg dest
        let lirSrc = convertOperand src
        [LIR.Mov (lirDest, lirSrc)]

    | MIR.BinOp (dest, op, left, right) ->
        let lirDest = vregToLIRReg dest
        let leftOp = convertOperand left
        let rightOp = convertOperand right

        match op with
        | MIR.Add ->
            // ADD can have immediate or register as right operand
            // Left operand must be in a register
            let (leftInstrs, leftReg) = ensureInRegister left lirDest
            leftInstrs @ [LIR.Add (lirDest, leftReg, rightOp)]

        | MIR.Sub ->
            // SUB can have immediate or register as right operand
            let (leftInstrs, leftReg) = ensureInRegister left lirDest
            leftInstrs @ [LIR.Sub (lirDest, leftReg, rightOp)]

        | MIR.Mul ->
            // MUL requires both operands in registers
            let (leftInstrs, leftReg) = ensureInRegister left (LIR.Virtual 1000)
            let (rightInstrs, rightReg) = ensureInRegister right (LIR.Virtual 1001)
            leftInstrs @ rightInstrs @ [LIR.Mul (lirDest, leftReg, rightReg)]

        | MIR.Div ->
            // SDIV requires both operands in registers
            let (leftInstrs, leftReg) = ensureInRegister left (LIR.Virtual 1000)
            let (rightInstrs, rightReg) = ensureInRegister right (LIR.Virtual 1001)
            leftInstrs @ rightInstrs @ [LIR.Sdiv (lirDest, leftReg, rightReg)]

        // Comparisons: CMP + CSET sequence
        | MIR.Eq ->
            let (leftInstrs, leftReg) = ensureInRegister left (LIR.Virtual 1000)
            leftInstrs @ [LIR.Cmp (leftReg, rightOp); LIR.Cset (lirDest, LIR.EQ)]

        | MIR.Neq ->
            let (leftInstrs, leftReg) = ensureInRegister left (LIR.Virtual 1000)
            leftInstrs @ [LIR.Cmp (leftReg, rightOp); LIR.Cset (lirDest, LIR.NE)]

        | MIR.Lt ->
            let (leftInstrs, leftReg) = ensureInRegister left (LIR.Virtual 1000)
            leftInstrs @ [LIR.Cmp (leftReg, rightOp); LIR.Cset (lirDest, LIR.LT)]

        | MIR.Gt ->
            let (leftInstrs, leftReg) = ensureInRegister left (LIR.Virtual 1000)
            leftInstrs @ [LIR.Cmp (leftReg, rightOp); LIR.Cset (lirDest, LIR.GT)]

        | MIR.Lte ->
            let (leftInstrs, leftReg) = ensureInRegister left (LIR.Virtual 1000)
            leftInstrs @ [LIR.Cmp (leftReg, rightOp); LIR.Cset (lirDest, LIR.LE)]

        | MIR.Gte ->
            let (leftInstrs, leftReg) = ensureInRegister left (LIR.Virtual 1000)
            leftInstrs @ [LIR.Cmp (leftReg, rightOp); LIR.Cset (lirDest, LIR.GE)]

        // Boolean operations (bitwise for 0/1 values)
        | MIR.And ->
            let (leftInstrs, leftReg) = ensureInRegister left (LIR.Virtual 1000)
            let (rightInstrs, rightReg) = ensureInRegister right (LIR.Virtual 1001)
            leftInstrs @ rightInstrs @ [LIR.And (lirDest, leftReg, rightReg)]

        | MIR.Or ->
            let (leftInstrs, leftReg) = ensureInRegister left (LIR.Virtual 1000)
            let (rightInstrs, rightReg) = ensureInRegister right (LIR.Virtual 1001)
            leftInstrs @ rightInstrs @ [LIR.Orr (lirDest, leftReg, rightReg)]

    | MIR.UnaryOp (dest, op, src) ->
        let lirDest = vregToLIRReg dest
        let (srcInstrs, srcReg) = ensureInRegister src (LIR.Virtual 1000)

        match op with
        | MIR.Neg ->
            // Negation: 0 - src
            srcInstrs @ [LIR.Mov (lirDest, LIR.Imm 0L); LIR.Sub (lirDest, lirDest, LIR.Reg srcReg)]

        | MIR.Not ->
            // Boolean NOT: 1 - src (since booleans are 0 or 1)
            srcInstrs @ [
                LIR.Mov (lirDest, LIR.Imm 1L)
                LIR.Sub (lirDest, lirDest, LIR.Reg srcReg)
            ]

    | MIR.Ret operand ->
        // ARM64 returns value in X0
        // For top-level programs: print result before exiting
        let lirOp = convertOperand operand
        [
            LIR.Mov (LIR.Physical LIR.X0, lirOp)
            LIR.PrintInt (LIR.Physical LIR.X0)  // Print before return
            LIR.Ret
        ]

/// Convert MIR block to LIR instructions
let selectBlock (MIR.Block instrs) : LIR.Instr list =
    instrs |> List.collect selectInstr

/// Convert MIR program to LIR
let toLIR (program: MIR.Program) : LIR.Program =
    let (MIR.Program blocks) = program

    // For now, single function with all blocks
    let instrs = blocks |> List.collect selectBlock

    let func = {
        LIR.Name = "_start"
        LIR.Body = instrs
        LIR.StackSize = 0  // Will be determined by register allocation
    }

    LIR.Program [func]
