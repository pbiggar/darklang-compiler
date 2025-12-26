// ANF.fs - A-Normal Form Intermediate Representation
//
// Defines the ANF (A-Normal Form) data structures.
//
// ANF is an intermediate representation where:
// - All intermediate computations are named with temporary variables
// - All operands to operations are simple (variables or literals, called "atoms")
// - Evaluation order is completely explicit through let-bindings
//
// This representation simplifies subsequent compiler passes by eliminating
// nested expressions.
//
// Example ANF for "2 + 3 * 4":
//   let tmp0 = 3
//   let tmp1 = 4
//   let tmp2 = tmp0 * tmp1
//   let tmp3 = 2
//   let tmp4 = tmp3 + tmp2
//   return tmp4

module ANF

/// Unique identifier for temporary variables
type TempId = TempId of int

/// Atomic expressions (cannot be decomposed further)
type Atom =
    | IntLiteral of int64
    | BoolLiteral of bool
    | StringLiteral of string
    | FloatLiteral of float
    | Var of TempId
    | FuncRef of string  // Reference to a function by name (for higher-order functions)

/// Binary operations on atoms
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

/// Unary operations on atoms
type UnaryOp =
    | Neg
    | Not

/// Complex expressions (produce values)
type CExpr =
    | Atom of Atom
    | Prim of BinOp * Atom * Atom
    | UnaryPrim of UnaryOp * Atom
    | IfValue of cond:Atom * thenValue:Atom * elseValue:Atom  // If-expression that produces a value
    | Call of funcName:string * args:Atom list  // Function call (direct: BL instruction)
    | IndirectCall of func:Atom * args:Atom list  // Call through function variable (BLR instruction)
    | ClosureAlloc of funcName:string * captures:Atom list  // Allocate closure: (func_addr, cap1, cap2, ...)
    | ClosureCall of closure:Atom * args:Atom list  // Call through closure, passing closure as hidden first arg
    | TupleAlloc of Atom list                   // Create tuple: (a, b, c)
    | TupleGet of tuple:Atom * index:int        // Get tuple element: t.0
    // String operations (heap-allocating)
    | StringConcat of left:Atom * right:Atom    // Concatenate strings: s1 ++ s2
    // Reference counting operations
    | RefCountInc of Atom * payloadSize:int    // Increment ref count of heap value
    | RefCountDec of Atom * payloadSize:int    // Decrement ref count, free if zero
    // Output operations (for main expression result)
    | Print of Atom * AST.Type                 // Print value with type-appropriate formatting

/// ANF expressions with explicit sequencing
type AExpr =
    | Let of TempId * CExpr * AExpr
    | Return of Atom
    | If of cond:Atom * thenBranch:AExpr * elseBranch:AExpr

/// ANF function definition
type Function = {
    Name: string
    Params: TempId list
    Body: AExpr
}

/// ANF program (functions and main expression)
type Program = Program of functions:Function list * main:AExpr

/// Fresh variable generator (functional style)
type VarGen = VarGen of int

/// Generate a fresh temporary variable
let freshVar (VarGen n) : TempId * VarGen =
    (TempId n, VarGen (n + 1))

/// Initial variable generator
let initialVarGen = VarGen 0

/// Type map for tracking TempId -> Type mappings
/// Used by reference counting pass to determine which values are heap-allocated
type TypeMap = Map<TempId, AST.Type>

/// Program with type information for reference counting
type TypedProgram = {
    Program: Program
    TypeMap: TypeMap
}

/// Check if a type requires reference counting (heap-allocated)
let isHeapType (t: AST.Type) : bool =
    match t with
    | AST.TTuple _ -> true
    | AST.TRecord _ -> true
    | AST.TList _ -> true  // All lists are heap-allocated regardless of element type
    | AST.TSum _ -> true  // Conservative: sum types with payloads are heap-allocated
    | _ -> false

/// Calculate payload size in bytes for a heap-allocated type
let payloadSize (t: AST.Type) (typeReg: Map<string, (string * AST.Type) list>) : int =
    match t with
    | AST.TTuple ts -> List.length ts * 8
    | AST.TRecord name ->
        match Map.tryFind name typeReg with
        | Some fields -> List.length fields * 8
        | None -> 16  // Fallback for unknown records
    | AST.TList _ -> 24  // [tag, head, tail] - same size for all element types
    | AST.TSum _ -> 16  // [tag, payload]
    | _ -> 0  // Non-heap types
