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

/// String pool for deduplicating string literals
/// Maps pool index to (string value, length)
type StringPool = {
    Strings: Map<int, string * int>  // index -> (string, length)
    StringToId: Map<string, int>     // reverse index for O(log n) lookup
    NextId: int
}

/// Empty string pool
let emptyStringPool = { Strings = Map.empty; StringToId = Map.empty; NextId = 0 }

/// Add a string to the pool, returning its index
/// If string already exists, returns existing index
let addString (pool: StringPool) (s: string) : int * StringPool =
    // O(log n) lookup via reverse index
    match Map.tryFind s pool.StringToId with
    | Some idx -> (idx, pool)
    | None ->
        let idx = pool.NextId
        // Use UTF-8 byte count, not UTF-16 char count
        let utf8Length = System.Text.Encoding.UTF8.GetByteCount(s)
        let pool' = {
            Strings = Map.add idx (s, utf8Length) pool.Strings
            StringToId = Map.add s idx pool.StringToId
            NextId = idx + 1
        }
        (idx, pool')

/// Float pool for storing float constants in data section
/// Maps pool index to float value
type FloatPool = {
    Floats: Map<int, float>    // index -> float value
    FloatToId: Map<float, int> // reverse index for O(log n) lookup
    NextId: int
}

/// Empty float pool
let emptyFloatPool = { Floats = Map.empty; FloatToId = Map.empty; NextId = 0 }

/// Add a float to the pool, returning its index
/// If float already exists, returns existing index
let addFloat (pool: FloatPool) (f: float) : int * FloatPool =
    // O(log n) lookup via reverse index
    match Map.tryFind f pool.FloatToId with
    | Some idx -> (idx, pool)
    | None ->
        let idx = pool.NextId
        let pool' = {
            Floats = Map.add idx f pool.Floats
            FloatToId = Map.add f idx pool.FloatToId
            NextId = idx + 1
        }
        (idx, pool')

/// Operands
type Operand =
    | IntConst of int64
    | BoolConst of bool
    | FloatRef of int   // Index into float pool
    | StringRef of int  // Index into string pool
    | Register of VReg
    | FuncAddr of string  // Address of a function (for higher-order functions)

/// Binary operations
type BinOp =
    // Arithmetic
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    // Bitwise
    | Shl     // << (left shift)
    | Shr     // >> (right shift)
    | BitAnd  // & (bitwise and)
    | BitOr   // | (bitwise or)
    | BitXor  // ^ (bitwise xor)
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

/// Basic block label (defined early for use in Phi nodes)
type Label = Label of string

/// Instructions (non-control-flow)
type Instr =
    | Mov of dest:VReg * src:Operand * valueType:AST.Type option  // valueType for float/int distinction
    | BinOp of dest:VReg * op:BinOp * left:Operand * right:Operand * operandType:AST.Type
    | UnaryOp of dest:VReg * op:UnaryOp * src:Operand
    | Call of dest:VReg * funcName:string * args:Operand list * argTypes:AST.Type list * returnType:AST.Type  // Direct function call (BL instruction)
    | IndirectCall of dest:VReg * func:Operand * args:Operand list * argTypes:AST.Type list * returnType:AST.Type  // Call through function pointer (BLR instruction)
    | ClosureAlloc of dest:VReg * funcName:string * captures:Operand list  // Allocate closure: (func_addr, caps...)
    | ClosureCall of dest:VReg * closure:Operand * args:Operand list  // Call through closure with hidden first arg
    // Heap operations for tuples and other compound types
    | HeapAlloc of dest:VReg * sizeBytes:int       // Allocate heap memory
    | HeapStore of addr:VReg * offset:int * src:Operand  // Store at heap[addr+offset]
    | HeapLoad of dest:VReg * addr:VReg * offset:int     // Load from heap[addr+offset]
    // String operations
    | StringConcat of dest:VReg * left:Operand * right:Operand  // Concatenate strings
    // Reference counting operations
    | RefCountInc of addr:VReg * payloadSize:int   // Increment ref count at [addr + payloadSize]
    | RefCountDec of addr:VReg * payloadSize:int   // Decrement ref count, free if zero
    // Output operations (for main expression result printing)
    | Print of src:Operand * valueType:AST.Type    // Print value with type-appropriate formatting
    // File I/O intrinsics (generate syscalls)
    | FileReadText of dest:VReg * path:Operand    // Read file, returns Result<String, String>
    | FileExists of dest:VReg * path:Operand      // Check if file exists, returns Bool
    | FileWriteText of dest:VReg * path:Operand * content:Operand   // Write file, returns Result<Unit, String>
    | FileAppendText of dest:VReg * path:Operand * content:Operand  // Append to file, returns Result<Unit, String>
    | FileDelete of dest:VReg * path:Operand      // Delete file, returns Result<Unit, String>
    | FileSetExecutable of dest:VReg * path:Operand  // Set executable bit, returns Result<Unit, String>
    // Float intrinsics
    | FloatSqrt of dest:VReg * src:Operand        // Square root: sqrt(x)
    | FloatAbs of dest:VReg * src:Operand         // Absolute value: |x|
    | FloatNeg of dest:VReg * src:Operand         // Negate: -x
    | IntToFloat of dest:VReg * src:Operand       // Convert Int64 to Float64
    | FloatToInt of dest:VReg * src:Operand       // Convert Float64 to Int64 (truncate)
    // Raw memory intrinsics (internal, for HAMT implementation)
    | RawAlloc of dest:VReg * numBytes:Operand    // Allocate raw bytes (no header), returns RawPtr
    | RawFree of ptr:Operand                      // Manually free raw memory
    | RawGet of dest:VReg * ptr:Operand * byteOffset:Operand  // Read 8 bytes at offset
    | RawGetByte of dest:VReg * ptr:Operand * byteOffset:Operand  // Read 1 byte at offset (zero-extended)
    | RawSet of ptr:Operand * byteOffset:Operand * value:Operand  // Write 8 bytes at offset
    | RawSetByte of ptr:Operand * byteOffset:Operand * value:Operand  // Write 1 byte at offset
    // String intrinsics (for Dict with string keys)
    | StringHash of dest:VReg * str:Operand       // FNV-1a hash of string, returns Int64
    | StringEq of dest:VReg * left:Operand * right:Operand  // Byte-wise string equality
    // String reference counting (at dynamic offset)
    | RefCountIncString of str:Operand             // Increment string ref count (at [str + 8 + len])
    | RefCountDecString of str:Operand             // Decrement string ref count, free if zero
    // Random intrinsics
    | RandomInt64 of dest:VReg                     // Get 8 random bytes as Int64
    // SSA phi node - merges values from different predecessor blocks
    // Phi nodes must appear at the beginning of a basic block, before other instructions
    | Phi of dest:VReg * sources:(Operand * Label) list

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
    ParamTypes: AST.Type list  // Parameter types (for distinguishing int vs float)
    ReturnType: AST.Type       // Return type (for distinguishing int vs float returns)
    CFG: CFG
}

/// Variant info for sum type printing
/// Maps type name -> list of (variant name, tag index, payload type)
type VariantRegistry = Map<string, (string * int * AST.Type option) list>

/// Record field info for record printing
/// Maps type name -> list of (field name, field type)
type RecordRegistry = Map<string, (string * AST.Type) list>

/// MIR program (list of functions with string and float pools)
type Program = Program of functions:Function list * strings:StringPool * floats:FloatPool * variants:VariantRegistry * records:RecordRegistry

/// Fresh register generator
type RegGen = RegGen of int

/// Generate a fresh virtual register
let freshReg (RegGen n) : VReg * RegGen =
    (VReg n, RegGen (n + 1))

/// Initial register generator
let initialRegGen = RegGen 0

/// Fresh label generator
type LabelGen = LabelGen of int

/// Generate a fresh label with optional function prefix (for uniqueness across functions)
let freshLabelWithPrefix (prefix: string) (LabelGen n) : Label * LabelGen =
    (Label $"{prefix}_L{n}", LabelGen (n + 1))

/// Generate a fresh label (backward compatible - uses empty prefix)
let freshLabel (LabelGen n) : Label * LabelGen =
    (Label $"L{n}", LabelGen (n + 1))

/// Initial label generator
let initialLabelGen = LabelGen 0
