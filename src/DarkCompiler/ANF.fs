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
    | Var of TempId

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
    | Call of funcName:string * args:Atom list  // Function call

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

/// ANF program (functions and optional main expression)
type Program = Program of functions:Function list * main:AExpr option

/// Fresh variable generator (functional style)
type VarGen = VarGen of int

/// Generate a fresh temporary variable
let freshVar (VarGen n) : TempId * VarGen =
    (TempId n, VarGen (n + 1))

/// Initial variable generator
let initialVarGen = VarGen 0
