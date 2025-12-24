// 3_ANF_to_MIR.fs - MIR Transformation (Pass 3)
//
// Transforms ANF into MIR with Control Flow Graph (CFG).
//
// Algorithm:
// - Converts ANF expressions into MIR CFG with basic blocks
// - Maps ANF temporary variables to MIR virtual registers
// - Converts ANF If expressions into conditional branches with basic blocks
// - Each basic block has a label, instructions, and a terminator
//
// Example (with if):
//   if x then 10 else 20
//   →
//   entry:
//     branch x, then_block, else_block
//   then_block:
//     v0 <- 10
//     jump join_block
//   else_block:
//     v1 <- 20
//     jump join_block
//   join_block:
//     v2 <- phi(v0, v1)  // (simplified - actual implementation uses registers)
//     ret v2

module ANF_to_MIR

/// Convert ANF.BinOp to MIR.BinOp
let convertBinOp (op: ANF.BinOp) : MIR.BinOp =
    match op with
    | ANF.Add -> MIR.Add
    | ANF.Sub -> MIR.Sub
    | ANF.Mul -> MIR.Mul
    | ANF.Div -> MIR.Div
    | ANF.Eq -> MIR.Eq
    | ANF.Neq -> MIR.Neq
    | ANF.Lt -> MIR.Lt
    | ANF.Gt -> MIR.Gt
    | ANF.Lte -> MIR.Lte
    | ANF.Gte -> MIR.Gte
    | ANF.And -> MIR.And
    | ANF.Or -> MIR.Or

/// Convert ANF.UnaryOp to MIR.UnaryOp
let convertUnaryOp (op: ANF.UnaryOp) : MIR.UnaryOp =
    match op with
    | ANF.Neg -> MIR.Neg
    | ANF.Not -> MIR.Not

/// Map ANF TempId to MIR virtual register
let tempToVReg (ANF.TempId id) : MIR.VReg = MIR.VReg id

/// Convert ANF Atom to MIR Operand
let atomToOperand (atom: ANF.Atom) : MIR.Operand =
    match atom with
    | ANF.IntLiteral n -> MIR.IntConst n
    | ANF.BoolLiteral b -> MIR.BoolConst b
    | ANF.Var tempId -> MIR.Register (tempToVReg tempId)

/// CFG builder state
type CFGBuilder = {
    Blocks: Map<MIR.Label, MIR.BasicBlock>
    LabelGen: MIR.LabelGen
    RegGen: MIR.RegGen
}

/// Convert ANF expression to CFG
/// Returns: (final value operand, CFG builder with all blocks)
let rec convertExpr
    (expr: ANF.AExpr)
    (currentLabel: MIR.Label)
    (currentInstrs: MIR.Instr list)
    (builder: CFGBuilder)
    : MIR.Operand * CFGBuilder =

    match expr with
    | ANF.Return atom ->
        // Return: end current block with Ret terminator
        let operand = atomToOperand atom
        let block = {
            MIR.Label = currentLabel
            MIR.Instrs = currentInstrs
            MIR.Terminator = MIR.Ret operand
        }
        let builder' = { builder with Blocks = Map.add currentLabel block builder.Blocks }
        (operand, builder')

    | ANF.Let (tempId, cexpr, rest) ->
        // Let binding: add instruction to current block, continue
        let destReg = tempToVReg tempId

        let instr =
            match cexpr with
            | ANF.Atom atom ->
                MIR.Mov (destReg, atomToOperand atom)
            | ANF.Prim (op, leftAtom, rightAtom) ->
                MIR.BinOp (destReg, convertBinOp op, atomToOperand leftAtom, atomToOperand rightAtom)
            | ANF.UnaryPrim (op, atom) ->
                MIR.UnaryOp (destReg, convertUnaryOp op, atomToOperand atom)

        let newInstrs = currentInstrs @ [instr]
        convertExpr rest currentLabel newInstrs builder

    | ANF.If (condAtom, thenBranch, elseBranch) ->
        // If expression:
        // 1. End current block with Branch terminator
        // 2. Create then-block and else-block
        // 3. Create join-block where both branches meet
        // 4. Both branches put result in same register and jump to join

        let condOp = atomToOperand condAtom

        // Generate labels for then, else, and join blocks
        let (thenLabel, labelGen1) = MIR.freshLabel builder.LabelGen
        let (elseLabel, labelGen2) = MIR.freshLabel labelGen1
        let (joinLabel, labelGen3) = MIR.freshLabel labelGen2

        // Create a register to hold the result from both branches
        let (resultReg, regGen1) = MIR.freshReg builder.RegGen

        // End current block with conditional branch
        let currentBlock = {
            MIR.Label = currentLabel
            MIR.Instrs = currentInstrs
            MIR.Terminator = MIR.Branch (condOp, thenLabel, elseLabel)
        }

        let builder1 = {
            Blocks = Map.add currentLabel currentBlock builder.Blocks
            LabelGen = labelGen3
            RegGen = regGen1
        }

        // Convert then-branch: result goes into resultReg, then jump to join
        let (thenResult, builder2) = convertExprToOperand thenBranch thenLabel [] builder1
        let thenBlock = {
            MIR.Label = thenLabel
            MIR.Instrs = [MIR.Mov (resultReg, thenResult)]
            MIR.Terminator = MIR.Jump joinLabel
        }
        let builder3 = { builder2 with Blocks = Map.add thenLabel thenBlock builder2.Blocks }

        // Convert else-branch: result goes into resultReg, then jump to join
        let (elseResult, builder4) = convertExprToOperand elseBranch elseLabel [] builder3
        let elseBlock = {
            MIR.Label = elseLabel
            MIR.Instrs = [MIR.Mov (resultReg, elseResult)]
            MIR.Terminator = MIR.Jump joinLabel
        }
        let builder5 = { builder4 with Blocks = Map.add elseLabel elseBlock builder4.Blocks }

        // Continue at join block with result in resultReg
        let resultOp = MIR.Register resultReg
        (resultOp, { builder5 with Blocks = builder5.Blocks })  // Don't create join block yet, caller will

/// Helper: convert expression and extract final operand
/// Creates blocks but doesn't finalize the last one
and convertExprToOperand
    (expr: ANF.AExpr)
    (startLabel: MIR.Label)
    (startInstrs: MIR.Instr list)
    (builder: CFGBuilder)
    : MIR.Operand * CFGBuilder =

    match expr with
    | ANF.Return atom ->
        // Just return the operand, don't create a block yet
        (atomToOperand atom, builder)

    | ANF.Let (tempId, cexpr, rest) ->
        let destReg = tempToVReg tempId
        let instr =
            match cexpr with
            | ANF.Atom atom -> MIR.Mov (destReg, atomToOperand atom)
            | ANF.Prim (op, leftAtom, rightAtom) ->
                MIR.BinOp (destReg, convertBinOp op, atomToOperand leftAtom, atomToOperand rightAtom)
            | ANF.UnaryPrim (op, atom) ->
                MIR.UnaryOp (destReg, convertUnaryOp op, atomToOperand atom)

        convertExprToOperand rest startLabel (startInstrs @ [instr]) builder

    | ANF.If (condAtom, thenBranch, elseBranch) ->
        // Similar to convertExpr but return operand
        let condOp = atomToOperand condAtom
        let (thenLabel, labelGen1) = MIR.freshLabel builder.LabelGen
        let (elseLabel, labelGen2) = MIR.freshLabel labelGen1
        let (joinLabel, labelGen3) = MIR.freshLabel labelGen2
        let (resultReg, regGen1) = MIR.freshReg builder.RegGen

        let startBlock = {
            MIR.Label = startLabel
            MIR.Instrs = startInstrs
            MIR.Terminator = MIR.Branch (condOp, thenLabel, elseLabel)
        }

        let builder1 = {
            Blocks = Map.add startLabel startBlock builder.Blocks
            LabelGen = labelGen3
            RegGen = regGen1
        }

        let (thenResult, builder2) = convertExprToOperand thenBranch thenLabel [] builder1
        let thenBlock = {
            MIR.Label = thenLabel
            MIR.Instrs = [MIR.Mov (resultReg, thenResult)]
            MIR.Terminator = MIR.Jump joinLabel
        }
        let builder3 = { builder2 with Blocks = Map.add thenLabel thenBlock builder2.Blocks }

        let (elseResult, builder4) = convertExprToOperand elseBranch elseLabel [] builder3
        let elseBlock = {
            MIR.Label = elseLabel
            MIR.Instrs = [MIR.Mov (resultReg, elseResult)]
            MIR.Terminator = MIR.Jump joinLabel
        }
        let builder5 = { builder4 with Blocks = Map.add elseLabel elseBlock builder4.Blocks }

        (MIR.Register resultReg, builder5)

/// Convert ANF program to MIR CFG
let toMIR (program: ANF.Program) (regGen: MIR.RegGen) : MIR.Program * MIR.RegGen =
    let (ANF.Program anfExpr) = program

    let entryLabel = MIR.Label "entry"
    let initialBuilder = {
        Blocks = Map.empty
        LabelGen = MIR.initialLabelGen
        RegGen = regGen
    }

    let (_, finalBuilder) = convertExpr anfExpr entryLabel [] initialBuilder

    let cfg = {
        MIR.Entry = entryLabel
        MIR.Blocks = finalBuilder.Blocks
    }

    (MIR.Program cfg, finalBuilder.RegGen)
