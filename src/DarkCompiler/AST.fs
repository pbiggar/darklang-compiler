// AST.fs - Abstract Syntax Tree
//
// Defines the abstract syntax tree data structures that represent the parsed
// program structure. The AST is the output of the Parser and input to the ANF
// transformation.
//
// Current language features:
// - Integer literals (64-bit signed)
// - Float literals (64-bit double)
// - Binary operators: +, -, *, /
// - Parenthesized expressions
// - Let bindings: let x = expr in body
// - Variables: identifiers bound by let
//
// Example AST for "let x = 5 in x + 3":
//   Let("x", IntLiteral(5), BinOp(Add, Var("x"), IntLiteral(3)))

module AST

/// Type system - will be used for type checking in Phase 0+
type Type =
    | TInt64
    | TBool
    | TFloat64
    | TString
    | TUnit
    | TFunction of Type list * Type  // parameter types * return type
    | TTuple of Type list             // tuple type: (Int, Bool, String)
    | TRecord of string               // record type by name: Point, Color, etc.

/// Binary operators
type BinOp =
    // Arithmetic
    | Add
    | Sub
    | Mul
    | Div
    // Comparisons (return bool)
    | Eq   // ==
    | Neq  // !=
    | Lt   // <
    | Gt   // >
    | Lte  // <=
    | Gte  // >=
    // Boolean operations
    | And  // &&
    | Or   // ||

/// Unary operators
type UnaryOp =
    | Neg  // Unary negation: -expr
    | Not  // Boolean not: !expr

/// Expression nodes
type Expr =
    | IntLiteral of int64
    | BoolLiteral of bool
    | StringLiteral of string
    | FloatLiteral of float
    | BinOp of BinOp * Expr * Expr
    | UnaryOp of UnaryOp * Expr
    | Let of name:string * value:Expr * body:Expr  // Let binding: let name = value in body
    | Var of string  // Variable reference
    | If of cond:Expr * thenBranch:Expr * elseBranch:Expr  // If expression: if cond then thenBranch else elseBranch
    | Call of funcName:string * args:Expr list  // Function call: funcName(arg1, arg2, ...)
    | TupleLiteral of Expr list              // Tuple literal: (1, 2, 3)
    | TupleAccess of tuple:Expr * index:int  // Tuple access: t.0, t.1, etc.
    | RecordLiteral of typeName:string * fields:(string * Expr) list  // { x = 1, y = 2 }
    | RecordAccess of record:Expr * fieldName:string                  // p.x, p.y

/// Function definition
type FunctionDef = {
    Name: string
    Params: (string * Type) list  // Parameter names with REQUIRED type annotations
    ReturnType: Type               // REQUIRED return type annotation
    Body: Expr
}

/// Type definition (record types, sum types, etc.)
type TypeDef =
    | RecordDef of name:string * fields:(string * Type) list  // type Point = { x: Int, y: Int }

/// Top-level program elements
type TopLevel =
    | FunctionDef of FunctionDef
    | TypeDef of TypeDef
    | Expression of Expr

/// Program is a list of top-level definitions (functions and/or expressions)
type Program = Program of TopLevel list
