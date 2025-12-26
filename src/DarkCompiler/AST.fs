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
    | TSum of string * Type list      // sum type by name with type args: Result<Int64, String>
    | TList of Type                    // List<T> - polymorphic list type
    | TVar of string                  // type variable: T, A, B, etc. (for generics)

/// Binary operators
type BinOp =
    // Arithmetic
    | Add
    | Sub
    | Mul
    | Div
    // String operations
    | StringConcat  // ++
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

/// Pattern matching patterns
type Pattern =
    | PUnit                                                // () - matches unit value
    | PWildcard                                            // _
    | PVar of string                                       // x (binds value to variable)
    | PConstructor of variantName:string * payload:Pattern option  // Red, Some(x)
    | PLiteral of int64                                    // 42 (integer literal)
    | PBool of bool                                        // true, false
    | PString of string                                    // "hello"
    | PFloat of float                                      // 3.14
    | PTuple of Pattern list                               // (a, b, c)
    | PRecord of typeName:string * fields:(string * Pattern) list  // { x = a, y = b }
    | PList of Pattern list                                // [a, b, c] - exact length match
    | PListCons of head:Pattern list * tail:Pattern        // [a, b, ...t] - head elements + rest

/// Expression nodes
type Expr =
    | UnitLiteral                           // Unit value: ()
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
    | TypeApp of funcName:string * typeArgs:Type list * args:Expr list  // Generic call: funcName<T, U>(args)
    | TupleLiteral of Expr list              // Tuple literal: (1, 2, 3)
    | TupleAccess of tuple:Expr * index:int  // Tuple access: t.0, t.1, etc.
    | RecordLiteral of typeName:string * fields:(string * Expr) list  // { x = 1, y = 2 }
    | RecordAccess of record:Expr * fieldName:string                  // p.x, p.y
    | Constructor of typeName:string * variantName:string * payload:Expr option  // Red, Some(42)
    | Match of scrutinee:Expr * cases:(Pattern * Expr) list  // match e with | p1 -> e1 | p2 -> e2
    | ListLiteral of Expr list                               // [1, 2, 3]
    | Lambda of parameters:(string * Type) list * body:Expr  // (x: int) => x + 1
    | Apply of func:Expr * args:Expr list                    // Apply function expr: f(x) where f is expression
    | FuncRef of funcName:string                             // Reference to a function (for passing as value)
    | Closure of funcName:string * captures:Expr list        // Closure: function + captured values

/// Function definition
type FunctionDef = {
    Name: string
    TypeParams: string list           // Type parameters for generics: ["T", "U", etc.], empty for non-generic
    Params: (string * Type) list      // Parameter names with REQUIRED type annotations
    ReturnType: Type                  // REQUIRED return type annotation
    Body: Expr
}

/// Variant in a sum type (with optional payload type for M4)
type Variant = {
    Name: string
    Payload: Type option  // None for simple enums, Some for payloads (M4)
}

/// Type definition (record types, sum types, etc.)
type TypeDef =
    | RecordDef of name:string * typeParams:string list * fields:(string * Type) list  // type Point<T> = { x: T, y: T }
    | SumTypeDef of name:string * typeParams:string list * variants:Variant list       // type Result<T, E> = Ok of T | Error of E

/// Top-level program elements
type TopLevel =
    | FunctionDef of FunctionDef
    | TypeDef of TypeDef
    | Expression of Expr

/// Program is a list of top-level definitions (functions and/or expressions)
type Program = Program of TopLevel list

/// Module function definition - a function within a module
type ModuleFunc = {
    Name: string                     // Function name (e.g., "add")
    ParamTypes: Type list            // Parameter types
    ReturnType: Type                 // Return type
}

/// Module definition - represents a namespace of functions
type ModuleDef = {
    Name: string                     // Full module path (e.g., "Stdlib.Int64")
    Functions: ModuleFunc list       // Functions in this module
}

/// Module registry - maps full function paths to their definitions
type ModuleRegistry = Map<string, ModuleFunc>
