// 3_ANF_to_MIR.fs - MIR Transformation (Pass 3)
//
// Transforms ANF into platform-independent MIR (three-address code).
//
// This pass converts ANF let-bindings into MIR instructions with virtual registers.
// Each ANF temporary variable becomes a virtual register.
//
// Example transformation:
//   Input ANF:  let tmp0 = 2 + 3
//               return tmp0
//   Output MIR: v0 <- 2
//               v1 <- 3
//               v2 <- v0 + v1
//               ret v2
//
// Uses the RegGen pattern for generating fresh virtual register names.

module ANF_to_MIR

/// Convert ANF.Op to MIR.Op
let convertOp (op: ANF.BinOp) : MIR.Op =
    match op with
    | ANF.Add -> MIR.Add
    | ANF.Sub -> MIR.Sub
    | ANF.Mul -> MIR.Mul
    | ANF.Div -> MIR.Div

/// Map ANF TempId to MIR virtual register
/// We use the same ID number for simplicity
let tempToVReg (ANF.TempId id) : MIR.VReg = MIR.VReg id

/// Convert ANF Atom to MIR Operand
let atomToOperand (atom: ANF.Atom) : MIR.Operand =
    match atom with
    | ANF.IntLiteral n -> MIR.IntConst n
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
                let mirOp = convertOp op
                let newInstrs = instrs @ [MIR.BinOp (destReg, mirOp, leftOp, rightOp)]
                convertExpr rest newInstrs regGen

    let (instrs, regGen') = convertExpr anfExpr [] regGen
    (MIR.Program [MIR.Block instrs], regGen')
