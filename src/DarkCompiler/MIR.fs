// MIR.fs - Mid-level Intermediate Representation
//
// Defines the MIR (Mid-level IR) data structures.
//
// MIR is a platform-independent three-address code representation where:
// - Each instruction has at most two operands and one destination
// - Virtual registers are used (infinite supply)
// - Instructions are organized into basic blocks
//
// Example MIR:
//   v0 <- 2
//   v1 <- 3
//   v2 <- v0 + v1
//   ret v2

module MIR

/// Virtual register (infinite supply)
type VReg = VReg of int

/// Operands
type Operand =
    | IntConst of int64
    | BoolConst of bool
    | Register of VReg

/// Binary operations
type BinOp =
    // Arithmetic
    | Add
    | Sub
    | Mul
    | Div
    // Comparisons
    | Eq
    | Neq
    | Lt
    | Gt
    | Lte
    | Gte
    // Boolean
    | And
    | Or

/// Unary operations
type UnaryOp =
    | Neg
    | Not

/// Instructions
type Instr =
    | Mov of dest:VReg * src:Operand
    | BinOp of dest:VReg * op:BinOp * left:Operand * right:Operand
    | UnaryOp of dest:VReg * op:UnaryOp * src:Operand
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
