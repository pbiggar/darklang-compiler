module MIR

/// Virtual register (infinite supply)
type VReg = VReg of int

/// Operands
type Operand =
    | IntConst of int64
    | Register of VReg

/// Operations
type Op =
    | Add
    | Sub
    | Mul
    | Div

/// Instructions
type Instr =
    | Mov of dest:VReg * src:Operand
    | BinOp of dest:VReg * op:Op * left:Operand * right:Operand
    | Ret of Operand

/// Basic block (for future control flow)
type Block = Block of Instr list

/// MIR program
type Program = Program of Block list

/// Fresh register generator
type RegGen = RegGen of int

/// Generate a fresh virtual register
let freshReg (RegGen n) : VReg * RegGen =
    (VReg n, RegGen (n + 1))

/// Initial register generator
let initialRegGen = RegGen 0

/// Convert ANF.Op to MIR.Op
let convertOp (op: ANF.BinOp) : Op =
    match op with
    | ANF.Add -> Add
    | ANF.Sub -> Sub
    | ANF.Mul -> Mul
    | ANF.Div -> Div

/// Map ANF TempId to MIR virtual register
/// We use the same ID number for simplicity
let tempToVReg (ANF.TempId id) : VReg = VReg id

/// Convert ANF Atom to MIR Operand
let atomToOperand (atom: ANF.Atom) : Operand =
    match atom with
    | ANF.IntLiteral n -> IntConst n
    | ANF.Var tempId -> Register (tempToVReg tempId)

/// Convert ANF to MIR
let toMIR (program: ANF.Program) (regGen: RegGen) : Program * RegGen =
    let (ANF.Program anfExpr) = program

    let rec convertExpr (expr: ANF.AExpr) (instrs: Instr list) (regGen: RegGen) : Instr list * RegGen =
        match expr with
        | ANF.Return atom ->
            let operand = atomToOperand atom
            (instrs @ [Ret operand], regGen)

        | ANF.Let (tempId, cexpr, rest) ->
            let destReg = tempToVReg tempId

            match cexpr with
            | ANF.Atom atom ->
                // Simple move
                let operand = atomToOperand atom
                let newInstrs = instrs @ [Mov (destReg, operand)]
                convertExpr rest newInstrs regGen

            | ANF.Prim (op, leftAtom, rightAtom) ->
                // Binary operation
                let leftOp = atomToOperand leftAtom
                let rightOp = atomToOperand rightAtom
                let mirOp = convertOp op
                let newInstrs = instrs @ [BinOp (destReg, mirOp, leftOp, rightOp)]
                convertExpr rest newInstrs regGen

    let (instrs, regGen') = convertExpr anfExpr [] regGen
    (Program [Block instrs], regGen')
