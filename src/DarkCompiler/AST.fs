// AST.fs - Abstract Syntax Tree
//
// Defines the abstract syntax tree data structures that represent the parsed
// program structure. The AST is the output of the Parser and input to the ANF
// transformation.
//
// Current language features:
// - Integer literals (64-bit signed)
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

/// Binary operators
type BinOp =
    | Add
    | Sub
    | Mul
    | Div

/// Expression nodes
type Expr =
    | IntLiteral of int64
    | BinOp of BinOp * Expr * Expr
    | Neg of Expr  // Unary negation: -expr
    | Let of name:string * value:Expr * body:Expr  // Let binding: let name = value in body
    | Var of string  // Variable reference

/// Program is just an expression for now
type Program = Program of Expr
