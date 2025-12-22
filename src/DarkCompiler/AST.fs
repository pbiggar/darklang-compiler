module AST

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

/// Program is just an expression for now
type Program = Program of Expr
