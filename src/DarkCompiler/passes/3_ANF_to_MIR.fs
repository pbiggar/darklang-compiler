// 3_ANF_to_MIR.fs - MIR Transformation (Pass 3)
//
// Transforms ANF into MIR.
//
// Algorithm:
// - Converts ANF let-bindings into MIR instructions
// - Maps ANF temporary variables to MIR virtual registers
// - Converts ANF atoms to MIR operands (constants or registers)
// - Uses RegGen for fresh virtual register name generation
//
// Example:
//   let tmp0 = 2 + 3; return tmp0
//   →
//   v0 <- 2; v1 <- 3; v2 <- v0 + v1; ret v2

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
/// We use the same ID number for simplicity
let tempToVReg (ANF.TempId id) : MIR.VReg = MIR.VReg id

/// Convert ANF Atom to MIR Operand
let atomToOperand (atom: ANF.Atom) : MIR.Operand =
    match atom with
    | ANF.IntLiteral n -> MIR.IntConst n
    | ANF.BoolLiteral b -> MIR.BoolConst b
    | ANF.Var tempId -> MIR.Register (tempToVReg tempId)

/// Convert ANF to MIR
let toMIR (program: ANF.Program) (regGen: MIR.RegGen) : MIR.Program * MIR.RegGen =
    let (ANF.Program anfExpr) = program

    let rec convertExpr (expr: ANF.AExpr) (instrs: MIR.Instr list) (regGen: MIR.RegGen) : MIR.Instr list * MIR.RegGen =
        match expr with
        | ANF.Return atom ->
            let operand = atomToOperand atom
            (instrs @ [MIR.Ret operand], regGen)

        | ANF.Let (tempId, cexpr, rest) ->
            let destReg = tempToVReg tempId

            match cexpr with
            | ANF.Atom atom ->
                // Simple move
                let operand = atomToOperand atom
                let newInstrs = instrs @ [MIR.Mov (destReg, operand)]
                convertExpr rest newInstrs regGen

            | ANF.Prim (op, leftAtom, rightAtom) ->
                // Binary operation
                let leftOp = atomToOperand leftAtom
                let rightOp = atomToOperand rightAtom
                let mirOp = convertBinOp op
                let newInstrs = instrs @ [MIR.BinOp (destReg, mirOp, leftOp, rightOp)]
                convertExpr rest newInstrs regGen

            | ANF.UnaryPrim (op, atom) ->
                // Unary operation
                let operand = atomToOperand atom
                let mirOp = convertUnaryOp op
                let newInstrs = instrs @ [MIR.UnaryOp (destReg, mirOp, operand)]
                convertExpr rest newInstrs regGen

    let (instrs, regGen') = convertExpr anfExpr [] regGen
    (MIR.Program [MIR.Block instrs], regGen')
