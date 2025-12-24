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
//
// Example AST for "2 + 3 * 4":
//   BinOp(Add, IntLiteral(2), BinOp(Mul, IntLiteral(3), IntLiteral(4)))

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

/// Program is just an expression for now
type Program = Program of Expr
