// 4_MIR_to_LIR.fs - Instruction Selection (Pass 4)
//
// Transforms MIR CFG into LIR CFG.
//
// Instruction selection algorithm:
// - Converts MIR basic blocks to LIR basic blocks
// - Selects appropriate ARM64 instructions for each MIR operation
// - Handles ARM64 operand constraints:
//   - ADD/SUB: support 12-bit immediates, left operand must be register
//   - MUL/SDIV: both operands must be registers
// - Inserts MOV instructions to load immediates when needed
// - Converts MIR terminators to LIR terminators
// - Preserves CFG structure (labels, branches, jumps)

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

/// Convert MIR terminator to LIR terminator
/// For Branch, need to convert operand to register (may add instructions)
let selectTerminator (terminator: MIR.Terminator) : LIR.Instr list * LIR.Terminator =
    match terminator with
    | MIR.Ret operand ->
        // Move operand to X0 (return register), print it, then return
        let lirOp = convertOperand operand
        let instrs = [
            LIR.Mov (LIR.Physical LIR.X0, lirOp)
            LIR.PrintInt (LIR.Physical LIR.X0)
        ]
        (instrs, LIR.Ret)

    | MIR.Branch (condOp, trueLabel, falseLabel) ->
        // Convert MIR.Label to LIR.Label (just unwrap)
        let (MIR.Label trueLbl) = trueLabel
        let (MIR.Label falseLbl) = falseLabel

        // Condition must be in a register for ARM64 branch instructions
        let (condInstrs, condReg) = ensureInRegister condOp (LIR.Virtual 1002)
        (condInstrs, LIR.Branch (condReg, trueLbl, falseLbl))

    | MIR.Jump label ->
        let (MIR.Label lbl) = label
        ([], LIR.Jump lbl)

/// Convert MIR label to LIR label
let convertLabel (MIR.Label lbl) : LIR.Label = lbl

/// Convert MIR basic block to LIR basic block
let selectBlock (block: MIR.BasicBlock) : LIR.BasicBlock =
    let lirLabel = convertLabel block.Label

    // Convert all instructions
    let lirInstrs = block.Instrs |> List.collect selectInstr

    // Convert terminator (may add instructions)
    let (termInstrs, lirTerm) = selectTerminator block.Terminator

    {
        LIR.Label = lirLabel
        LIR.Instrs = lirInstrs @ termInstrs
        LIR.Terminator = lirTerm
    }

/// Convert MIR CFG to LIR CFG
let selectCFG (cfg: MIR.CFG) : LIR.CFG =
    let lirEntry = convertLabel cfg.Entry
    let lirBlocks =
        cfg.Blocks
        |> Map.toList
        |> List.map (fun (label, block) -> (convertLabel label, selectBlock block))
        |> Map.ofList

    {
        LIR.Entry = lirEntry
        LIR.Blocks = lirBlocks
    }

/// Convert MIR program to LIR
let toLIR (program: MIR.Program) : LIR.Program =
    let (MIR.Program cfg) = program

    let lirCFG = selectCFG cfg

    let func = {
        LIR.Name = "_start"
        LIR.CFG = lirCFG
        LIR.StackSize = 0  // Will be determined by register allocation
    }

    LIR.Program [func]
