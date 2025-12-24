// ARM64.fs - ARM64 Instruction Types
//
// Defines ARM64 instruction and register types.
//
// ARM64 is a RISC architecture with fixed 32-bit instruction width.
// These types represent ARM64 assembly instructions that will be encoded
// to machine code by the ARM64_Encoding pass.
//
// Supported instructions:
// - MOVZ/MOVK: Load immediate values (16-bit chunks)
// - ADD/SUB: Arithmetic (immediate and register forms)
// - MUL/SDIV: Multiplication and signed division
// - MOV: Register-to-register move
// - RET: Return from function
//
// Example instructions:
//   MOVZ X0, #42, LSL #0
//   ADD X1, X0, #5
//   RET

module ARM64

/// ARM64 registers (subset for now)
type Reg =
    | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9
    | X10 | X11 | X12 | X13 | X14 | X15 | X16
    | X29 | X30 | SP

/// ARM64 instruction types
type Instr =
    | MOVZ of dest:Reg * imm:uint16 * shift:int  // Move with zero
    | MOVK of dest:Reg * imm:uint16 * shift:int  // Move with keep
    | ADD_imm of dest:Reg * src:Reg * imm:uint16
    | ADD_reg of dest:Reg * src1:Reg * src2:Reg
    | SUB_imm of dest:Reg * src:Reg * imm:uint16
    | SUB_reg of dest:Reg * src1:Reg * src2:Reg
    | MUL of dest:Reg * src1:Reg * src2:Reg
    | SDIV of dest:Reg * src1:Reg * src2:Reg
    | UDIV of dest:Reg * src1:Reg * src2:Reg  // Unsigned division (for positive integers)
    | MSUB of dest:Reg * src1:Reg * src2:Reg * src3:Reg  // Multiply-subtract (for modulo)
    | MOV_reg of dest:Reg * src:Reg
    | STRB of src:Reg * base:Reg * offset:int  // Store byte [base + offset] = src (lower 8 bits)
    | CBZ of reg:Reg * offset:int  // Compare and branch if zero
    | TBNZ of reg:Reg * bit:int * offset:int  // Test bit and branch if not zero
    | B of offset:int  // Unconditional branch
    | NEG of dest:Reg * src:Reg  // Negate: dest = 0 - src
    | RET
    | SVC of imm:uint16  // Supervisor call (syscall)

/// Machine code (32-bit instruction)
type MachineCode = uint32
