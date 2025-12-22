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
