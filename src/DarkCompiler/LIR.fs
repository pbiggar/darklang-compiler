// LIR.fs - Low-level Intermediate Representation
//
// Defines the LIR (Low-level IR) data structures.
//
// LIR is an ARM64-specific representation that:
// - Uses ARM64-compatible instructions
// - Supports both virtual and physical registers
// - Handles immediate value constraints
// - Prepares code for register allocation and code generation
//
// Example LIR (after register allocation):
//   X0 <- Mov(Imm 42)
//   X1 <- Add(X0, Imm 5)
//   Ret

module LIR

/// ARM64 general-purpose registers
type PhysReg =
    | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9
    | X10 | X11 | X12 | X13 | X14 | X15
    | X29  // Frame pointer
    | X30  // Link register
    | SP   // Stack pointer

/// Register or virtual register (before allocation)
type Reg =
    | Physical of PhysReg
    | Virtual of int

/// Operands
type Operand =
    | Imm of int64           // Immediate value
    | Reg of Reg             // Register
    | StackSlot of int       // Stack offset (for spills)

/// Instructions (closer to ARM64 instructions)
type Instr =
    | Mov of dest:Reg * src:Operand
    | Add of dest:Reg * left:Reg * right:Operand
    | Sub of dest:Reg * left:Reg * right:Operand
    | Mul of dest:Reg * left:Reg * right:Reg
    | Sdiv of dest:Reg * left:Reg * right:Reg  // Signed division
    | Ret

/// Function (preparation for future functions)
type Function = {
    Name: string
    Body: Instr list
    StackSize: int  // Bytes needed for spills
}

/// LIR program
type Program = Program of Function list

/// Live range for a virtual register (future use)
type LiveRange = {
    Start: int
    End: int
}

/// Allocation result
type Allocation =
    | Register of PhysReg
    | Spill of int  // Stack slot offset

/// Register allocation result
type AllocResult = {
    Instrs: Instr list
    StackSize: int
}
