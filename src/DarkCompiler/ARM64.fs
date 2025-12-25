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
// - STP/LDP: Store/Load pair (for stack frames)
// - STR/LDR: Store/Load register (for stack slots)
// - BL: Branch with link (function calls)
// - RET: Return from function
//
// Example instructions:
//   MOVZ X0, #42, LSL #0
//   ADD X1, X0, #5
//   RET

module ARM64

/// ARM64 general-purpose registers (subset for now)
type Reg =
    | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9
    | X10 | X11 | X12 | X13 | X14 | X15 | X16
    | X28  // Reserved for heap pointer
    | X29 | X30 | SP

/// ARM64 floating-point registers (D0-D15 for double precision)
type FReg =
    | D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7
    | D8 | D9 | D10 | D11 | D12 | D13 | D14 | D15

/// Comparison conditions (for CSET)
type Condition =
    | EQ   // Equal (Z set)
    | NE   // Not equal (Z clear)
    | LT   // Less than (signed)
    | GT   // Greater than (signed)
    | LE   // Less than or equal (signed)
    | GE   // Greater than or equal (signed)

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
    | CMP_imm of src:Reg * imm:uint16  // Compare with immediate (sets condition flags)
    | CMP_reg of src1:Reg * src2:Reg  // Compare registers (sets condition flags)
    | CSET of dest:Reg * cond:Condition  // Set register to 1 if condition, 0 otherwise
    | AND_reg of dest:Reg * src1:Reg * src2:Reg  // Bitwise AND
    | ORR_reg of dest:Reg * src1:Reg * src2:Reg  // Bitwise OR
    | MVN of dest:Reg * src:Reg  // Bitwise NOT
    | MOV_reg of dest:Reg * src:Reg
    | STRB of src:Reg * addr:Reg * offset:int  // Store byte [addr + offset] = src (lower 8 bits)
    // Stack operations (for function calls and stack frames)
    | STP of reg1:Reg * reg2:Reg * addr:Reg * offset:int16  // Store pair: [addr + offset] = reg1, [addr + offset + 8] = reg2
    | LDP of reg1:Reg * reg2:Reg * addr:Reg * offset:int16  // Load pair: reg1 = [addr + offset], reg2 = [addr + offset + 8]
    | STR of src:Reg * addr:Reg * offset:int16  // Store register (unsigned offset): [addr + offset] = src (64-bit)
    | LDR of dest:Reg * addr:Reg * offset:int16  // Load register (unsigned offset): dest = [addr + offset] (64-bit)
    | STUR of src:Reg * addr:Reg * offset:int16  // Store register (signed offset, -256 to +255): [addr + offset] = src (64-bit)
    | LDUR of dest:Reg * addr:Reg * offset:int16  // Load register (signed offset, -256 to +255): dest = [addr + offset] (64-bit)
    | BL of label:string  // Branch with link: call function at label (sets X30/LR to return address)
    // Label-based branches (for compiler-generated code with CFG)
    | CBZ of reg:Reg * label:string  // Compare and branch if zero (label will be resolved)
    | CBNZ of reg:Reg * label:string  // Compare and branch if not zero
    | B_label of label:string  // Unconditional branch to label
    // Offset-based branches (for handcrafted runtime code with known offsets)
    | CBZ_offset of reg:Reg * offset:int  // CBZ with immediate offset
    | CBNZ_offset of reg:Reg * offset:int  // CBNZ with immediate offset
    | TBNZ of reg:Reg * bit:int * offset:int  // Test bit and branch if not zero
    | B of offset:int  // Unconditional branch with immediate offset
    | NEG of dest:Reg * src:Reg  // Negate: dest = 0 - src
    | RET
    | SVC of imm:uint16  // Supervisor call (syscall)
    | Label of string  // Pseudo-instruction: marks a label position
    // PC-relative addressing for .rodata access
    | ADRP of dest:Reg * label:string  // Address page: dest = PC-relative page address of label
    | ADD_label of dest:Reg * src:Reg * label:string  // Add label offset: dest = src + page offset of label
    // Floating-point instructions
    | LDR_fp of dest:FReg * addr:Reg * offset:int16  // Load double from [addr + offset]
    | STR_fp of src:FReg * addr:Reg * offset:int16   // Store double to [addr + offset]
    | FADD of dest:FReg * src1:FReg * src2:FReg      // FP add: dest = src1 + src2
    | FSUB of dest:FReg * src1:FReg * src2:FReg      // FP sub: dest = src1 - src2
    | FMUL of dest:FReg * src1:FReg * src2:FReg      // FP mul: dest = src1 * src2
    | FDIV of dest:FReg * src1:FReg * src2:FReg      // FP div: dest = src1 / src2
    | FNEG of dest:FReg * src:FReg                   // FP negate: dest = -src
    | FABS of dest:FReg * src:FReg                   // FP absolute value: dest = |src|
    | FCMP of src1:FReg * src2:FReg                  // FP compare (sets condition flags)
    | FMOV_reg of dest:FReg * src:FReg               // FP move between registers
    | FMOV_to_gp of dest:Reg * src:FReg              // FP to GP register (bit-for-bit)
    | SCVTF of dest:FReg * src:Reg                   // Signed int to FP: dest = (double)src
    | FCVTZS of dest:Reg * src:FReg                  // FP to signed int (truncate): dest = (int64)src

/// Machine code (32-bit instruction)
type MachineCode = uint32
