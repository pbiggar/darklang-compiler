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

/// Instructions (non-control-flow)
type Instr =
    | Mov of dest:VReg * src:Operand
    | BinOp of dest:VReg * op:BinOp * left:Operand * right:Operand
    | UnaryOp of dest:VReg * op:UnaryOp * src:Operand
    | Call of dest:VReg * funcName:string * args:Operand list

/// Basic block label
type Label = Label of string

/// Terminator instructions (control flow)
type Terminator =
    | Ret of Operand                                      // Return from function
    | Branch of cond:Operand * trueLabel:Label * falseLabel:Label  // Conditional branch
    | Jump of Label                                        // Unconditional jump

/// Basic block with label, instructions, and terminator
type BasicBlock = {
    Label: Label
    Instrs: Instr list
    Terminator: Terminator
}

/// Control Flow Graph
type CFG = {
    Entry: Label
    Blocks: Map<Label, BasicBlock>
}

/// MIR function with CFG
type Function = {
    Name: string
    Params: VReg list
    CFG: CFG
}

/// MIR program (list of functions)
type Program = Program of Function list

/// Fresh register generator
type RegGen = RegGen of int

/// Generate a fresh virtual register
let freshReg (RegGen n) : VReg * RegGen =
    (VReg n, RegGen (n + 1))

/// Initial register generator
let initialRegGen = RegGen 0

/// Fresh label generator
type LabelGen = LabelGen of int

/// Generate a fresh label
let freshLabel (LabelGen n) : Label * LabelGen =
    (Label $"L{n}", LabelGen (n + 1))

/// Initial label generator
let initialLabelGen = LabelGen 0
