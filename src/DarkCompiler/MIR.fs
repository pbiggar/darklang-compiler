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
    NextId: int
}

/// Empty string pool
let emptyStringPool = { Strings = Map.empty; NextId = 0 }

/// Add a string to the pool, returning its index
/// If string already exists, returns existing index
let addString (pool: StringPool) (s: string) : int * StringPool =
    // Check if string already exists
    let existing =
        pool.Strings
        |> Map.tryFindKey (fun _ (str, _) -> str = s)
    match existing with
    | Some idx -> (idx, pool)
    | None ->
        let idx = pool.NextId
        let pool' = { Strings = Map.add idx (s, String.length s) pool.Strings; NextId = idx + 1 }
        (idx, pool')

/// Float pool for storing float constants in data section
/// Maps pool index to float value
type FloatPool = {
    Floats: Map<int, float>  // index -> float value
    NextId: int
}

/// Empty float pool
let emptyFloatPool = { Floats = Map.empty; NextId = 0 }

/// Add a float to the pool, returning its index
/// If float already exists, returns existing index
let addFloat (pool: FloatPool) (f: float) : int * FloatPool =
    // Check if float already exists
    let existing =
        pool.Floats
        |> Map.tryFindKey (fun _ v -> v = f)
    match existing with
    | Some idx -> (idx, pool)
    | None ->
        let idx = pool.NextId
        let pool' = { Floats = Map.add idx f pool.Floats; NextId = idx + 1 }
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
    | Call of dest:VReg * funcName:string * args:Operand list  // Direct function call (BL instruction)
    | IndirectCall of dest:VReg * func:Operand * args:Operand list  // Call through function pointer (BLR instruction)
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

/// MIR program (list of functions with string and float pools)
type Program = Program of functions:Function list * strings:StringPool * floats:FloatPool

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
