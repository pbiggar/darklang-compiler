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
    | X10 | X11 | X12 | X13 | X14 | X15 | X16 | X17  // X16/X17 are IP0/IP1 scratch registers
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

/// Basic block label (wrapper type for type safety)
type Label = Label of string

/// Instructions (closer to ARM64 instructions, non-control-flow)
type Instr =
    | Mov of dest:Reg * src:Operand
    | Phi of dest:Reg * sources:(Operand * Label) list * valueType:AST.Type option  // SSA phi node: merge values from predecessors
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
    // Sign/zero extension for integer overflow (truncation to target width)
    | Sxtb of dest:Reg * src:Reg               // Sign-extend byte (8-bit) to 64-bit
    | Sxth of dest:Reg * src:Reg               // Sign-extend halfword (16-bit) to 64-bit
    | Sxtw of dest:Reg * src:Reg               // Sign-extend word (32-bit) to 64-bit
    | Uxtb of dest:Reg * src:Reg               // Zero-extend byte (8-bit) to 64-bit
    | Uxth of dest:Reg * src:Reg               // Zero-extend halfword (16-bit) to 64-bit
    | Uxtw of dest:Reg * src:Reg               // Zero-extend word (32-bit) to 64-bit
    | Call of dest:Reg * funcName:string * args:Operand list  // Direct function call (BL instruction)
    | TailCall of funcName:string * args:Operand list  // Tail call (B instruction, no return)
    | IndirectCall of dest:Reg * func:Reg * args:Operand list  // Call through function pointer (BLR instruction)
    | IndirectTailCall of func:Reg * args:Operand list  // Indirect tail call (BR instruction)
    | ClosureAlloc of dest:Reg * funcName:string * captures:Operand list  // Allocate closure: (func_addr, caps...)
    | ClosureCall of dest:Reg * closure:Reg * args:Operand list  // Call through closure with hidden first arg
    | ClosureTailCall of closure:Reg * args:Operand list  // Tail call through closure (BR instruction)
    | SaveRegs of intRegs:PhysReg list * floatRegs:PhysFPReg list  // Save specified caller-saved registers before call
    | RestoreRegs of intRegs:PhysReg list * floatRegs:PhysFPReg list  // Restore specified caller-saved registers after call
    | ArgMoves of (PhysReg * Operand) list       // Move arguments to X0-X7 (parallel move - loads clobbered from SaveRegs stack)
    | TailArgMoves of (PhysReg * Operand) list   // Move arguments for tail calls (uses temp registers, no SaveRegs)
    | FArgMoves of (PhysFPReg * FReg) list       // Move float arguments to D0-D7
    | PrintInt of Reg                           // Print integer register to stdout (no exit)
    | PrintBool of Reg                          // Print boolean register to stdout (no exit)
    | PrintIntNoNewline of Reg                  // Print integer without newline (for tuple elements)
    | PrintBoolNoNewline of Reg                 // Print boolean without newline (for tuple elements)
    | PrintFloat of FReg                        // Print float from FP register to stdout
    | PrintFloatNoNewline of FReg               // Print float without newline (for tuple/list elements)
    | PrintString of stringIndex:int * stringLen:int  // Print string from pool to stdout
    | PrintHeapStringNoNewline of Reg           // Print heap string without newline (for tuple/list elements)
    | PrintChars of byte list                   // Print literal characters (for tuple/list delimiters)
    | PrintList of listPtr:Reg * elemType:AST.Type  // Print list [elem1, elem2, ...] with proper element formatting
    | PrintSum of sumPtr:Reg * variants:(string * int * AST.Type option) list  // Print sum type: variantName, tag, payloadType
    | PrintRecord of recordPtr:Reg * typeName:string * fields:(string * AST.Type) list  // Print record: TypeName { field1 = val1, ... }
    | Exit                                       // Exit program with code 0
    // Floating-point instructions
    | FPhi of dest:FReg * sources:(FReg * Label) list  // Float SSA phi node: merge float values from predecessors
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
    | GpToFp of dest:FReg * src:Reg             // Move bits from GP to FP register (for float in tuple)
    // Heap operations for tuples and other compound types
    | HeapAlloc of dest:Reg * sizeBytes:int       // Allocate heap memory
    | HeapStore of addr:Reg * offset:int * src:Operand * valueType:AST.Type option  // Store at heap[addr+offset], valueType for float/int
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
    | FileDelete of dest:Reg * path:Operand       // Delete file, returns Result<Unit, String>
    | FileSetExecutable of dest:Reg * path:Operand // Set executable bit, returns Result<Unit, String>
    | FileWriteFromPtr of dest:Reg * path:Operand * ptr:Reg * length:Reg // Write raw bytes to file, returns Bool
    // Raw memory intrinsics (internal, for HAMT implementation)
    | RawAlloc of dest:Reg * numBytes:Reg         // Allocate raw bytes (no header), returns RawPtr
    | RawFree of ptr:Reg                          // Manually free raw memory
    | RawGet of dest:Reg * ptr:Reg * byteOffset:Reg  // Read 8 bytes at offset
    | RawGetByte of dest:Reg * ptr:Reg * byteOffset:Reg  // Read 1 byte at offset (zero-extended)
    | RawSet of ptr:Reg * byteOffset:Reg * value:Reg  // Write 8 bytes at offset
    | RawSetByte of ptr:Reg * byteOffset:Reg * value:Reg  // Write 1 byte at offset
    // String intrinsics (for Dict with string keys)
    | StringHash of dest:Reg * str:Operand        // FNV-1a hash of string, returns Int64
    | StringEq of dest:Reg * left:Operand * right:Operand // Byte-wise string equality
    // String reference counting (at dynamic offset)
    | RefCountIncString of str:Operand            // Increment string ref count (at [str + 8 + len])
    | RefCountDecString of str:Operand            // Decrement string ref count, free if zero
    // Random intrinsics
    | RandomInt64 of dest:Reg                     // Get 8 random bytes as Int64
    // Coverage instrumentation - records that expression was executed
    | CoverageHit of exprId:int

/// Terminator instructions (control flow)
type Terminator =
    | Ret                                              // Return from function
    | Branch of cond:Reg * trueLabel:Label * falseLabel:Label  // Conditional branch (test register)
    | CondBranch of cond:Condition * trueLabel:Label * falseLabel:Label  // Conditional branch (test flags)
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
    ParamTypes: AST.Type list  // Parameter types (for distinguishing int vs float)
    CFG: CFG
    StackSize: int  // Bytes needed for spills (16-byte aligned)
    UsedCalleeSaved: PhysReg list  // Callee-saved registers used (for prologue/epilogue)
}

/// LIR program with functions, string pool, and float pool
type Program = Program of functions:Function list * strings:MIR.StringPool * floats:MIR.FloatPool

/// Count the number of CoverageHit instructions in a program
/// This is used to size the coverage data section
let countCoverageHits (Program (functions, _, _)) : int =
    functions
    |> List.collect (fun f ->
        f.CFG.Blocks
        |> Map.toList
        |> List.collect (fun (_, block) -> block.Instrs))
    |> List.filter (function CoverageHit _ -> true | _ -> false)
    |> List.length

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
