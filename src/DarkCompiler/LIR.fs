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
    | X19 | X20 | X21 | X22 | X23 | X24 | X25 | X26 | X27  // Callee-saved
    | X29  // Frame pointer
    | X30  // Link register
    | SP   // Stack pointer

/// ARM64 floating-point registers
type PhysFPReg =
    | D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7
    | D8 | D9 | D10 | D11 | D12 | D13 | D14 | D15

/// Register or virtual register (before allocation)
type Reg =
    | Physical of PhysReg
    | Virtual of int

/// Floating-point register or virtual FP register (before allocation)
type FReg =
    | FPhysical of PhysFPReg
    | FVirtual of int

/// Operands
type Operand =
    | Imm of int64           // Immediate value
    | FloatImm of float      // Float immediate value (legacy - prefer FloatRef)
    | Reg of Reg             // Register
    | StackSlot of int       // Stack offset (for spills)
    | StringRef of int       // Reference to string in pool (by index)
    | FloatRef of int        // Reference to float in pool (by index)
    | FuncAddr of string     // Address of a function (for higher-order functions)

/// Comparison conditions (for CSET)
type Condition =
    | EQ   // Equal
    | NE   // Not equal
    | LT   // Less than (signed)
    | GT   // Greater than (signed)
    | LE   // Less than or equal (signed)
    | GE   // Greater than or equal (signed)

/// Instructions (closer to ARM64 instructions, non-control-flow)
type Instr =
    | Mov of dest:Reg * src:Operand
    | Store of stackSlot:int * src:Reg          // Store register to stack slot (for spills)
    | Add of dest:Reg * left:Reg * right:Operand
    | Sub of dest:Reg * left:Reg * right:Operand
    | Mul of dest:Reg * left:Reg * right:Reg
    | Sdiv of dest:Reg * left:Reg * right:Reg  // Signed division
    | Msub of dest:Reg * mulLeft:Reg * mulRight:Reg * sub:Reg  // dest = sub - mulLeft * mulRight
    | Cmp of left:Reg * right:Operand           // Compare (sets flags)
    | Cset of dest:Reg * cond:Condition         // Set register based on condition
    | And of dest:Reg * left:Reg * right:Reg    // Bitwise AND (for boolean && and &)
    | Orr of dest:Reg * left:Reg * right:Reg    // Bitwise OR (for boolean || and |)
    | Eor of dest:Reg * left:Reg * right:Reg    // Bitwise XOR (^)
    | Lsl of dest:Reg * src:Reg * shift:Reg     // Logical shift left (<<)
    | Lsr of dest:Reg * src:Reg * shift:Reg     // Logical shift right (>>)
    | Mvn of dest:Reg * src:Reg                 // Bitwise NOT
    | Call of dest:Reg * funcName:string * args:Operand list  // Direct function call (BL instruction)
    | IndirectCall of dest:Reg * func:Reg * args:Operand list  // Call through function pointer (BLR instruction)
    | ClosureAlloc of dest:Reg * funcName:string * captures:Operand list  // Allocate closure: (func_addr, caps...)
    | ClosureCall of dest:Reg * closure:Reg * args:Operand list  // Call through closure with hidden first arg
    | SaveRegs                                   // Save caller-saved registers (X1-X10) before call
    | RestoreRegs                                // Restore caller-saved registers (X1-X10) after call
    | ArgMoves of (PhysReg * Operand) list       // Move arguments to X0-X7 (parallel move - handles clobber issues)
    | PrintInt of Reg                           // Print integer register to stdout (no exit)
    | PrintBool of Reg                          // Print boolean register to stdout (no exit)
    | PrintFloat of FReg                        // Print float from FP register to stdout
    | PrintString of stringIndex:int * stringLen:int  // Print string from pool to stdout
    | Exit                                       // Exit program with code 0
    // Floating-point instructions
    | FMov of dest:FReg * src:FReg              // Move between FP registers
    | FLoad of dest:FReg * floatIdx:int         // Load float from pool into FP register
    | FAdd of dest:FReg * left:FReg * right:FReg
    | FSub of dest:FReg * left:FReg * right:FReg
    | FMul of dest:FReg * left:FReg * right:FReg
    | FDiv of dest:FReg * left:FReg * right:FReg
    | FNeg of dest:FReg * src:FReg
    | FAbs of dest:FReg * src:FReg              // Absolute value
    | FSqrt of dest:FReg * src:FReg             // Square root
    | FCmp of left:FReg * right:FReg            // Compare FP values (sets flags)
    | IntToFloat of dest:FReg * src:Reg         // Convert Int64 to Float64 (SCVTF)
    | FloatToInt of dest:Reg * src:FReg         // Convert Float64 to Int64 (FCVTZS)
    // Heap operations for tuples and other compound types
    | HeapAlloc of dest:Reg * sizeBytes:int       // Allocate heap memory
    | HeapStore of addr:Reg * offset:int * src:Operand  // Store at heap[addr+offset]
    | HeapLoad of dest:Reg * addr:Reg * offset:int     // Load from heap[addr+offset]
    // Reference counting operations
    | RefCountInc of addr:Reg * payloadSize:int   // Increment ref count at [addr + payloadSize]
    | RefCountDec of addr:Reg * payloadSize:int   // Decrement ref count, free if zero
    // String operations
    | StringConcat of dest:Reg * left:Operand * right:Operand  // Concatenate strings on heap
    | PrintHeapString of Reg                       // Print heap string [len:8][data:N]
    // Higher-order function support
    | LoadFuncAddr of dest:Reg * funcName:string   // Load address of a function (ADR instruction)
    // File I/O intrinsics (generate syscalls)
    | FileReadText of dest:Reg * path:Operand     // Read file, returns Result<String, String>
    | FileExists of dest:Reg * path:Operand       // Check if file exists, returns Bool
    | FileWriteText of dest:Reg * path:Operand * content:Operand   // Write file, returns Result<Unit, String>
    | FileAppendText of dest:Reg * path:Operand * content:Operand  // Append to file, returns Result<Unit, String>
    // Raw memory intrinsics (internal, for HAMT implementation)
    | RawAlloc of dest:Reg * numBytes:Reg         // Allocate raw bytes (no header), returns RawPtr
    | RawFree of ptr:Reg                          // Manually free raw memory
    | RawGet of dest:Reg * ptr:Reg * byteOffset:Reg  // Read 8 bytes at offset
    | RawSet of ptr:Reg * byteOffset:Reg * value:Reg  // Write 8 bytes at offset
    // String intrinsics (for Dict with string keys)
    | StringHash of dest:Reg * str:Operand        // FNV-1a hash of string, returns Int64
    | StringEq of dest:Reg * left:Operand * right:Operand // Byte-wise string equality
    // String reference counting (at dynamic offset)
    | RefCountIncString of str:Operand            // Increment string ref count (at [str + 8 + len])
    | RefCountDecString of str:Operand            // Decrement string ref count, free if zero

/// Basic block label
type Label = string

/// Terminator instructions (control flow)
type Terminator =
    | Ret                                              // Return from function
    | Branch of cond:Reg * trueLabel:Label * falseLabel:Label  // Conditional branch
    | Jump of Label                                     // Unconditional jump

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

/// Function with CFG
type Function = {
    Name: string
    Params: Reg list  // Parameter registers (before allocation: Virtual, after: Physical)
    CFG: CFG
    StackSize: int  // Bytes needed for spills (16-byte aligned)
    UsedCalleeSaved: PhysReg list  // Callee-saved registers used (for prologue/epilogue)
}

/// LIR program with functions, string pool, and float pool
type Program = Program of functions:Function list * strings:MIR.StringPool * floats:MIR.FloatPool

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
