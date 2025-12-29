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
    // Signed integers
    | TInt8
    | TInt16
    | TInt32
    | TInt64
    // Unsigned integers
    | TUInt8
    | TUInt16
    | TUInt32
    | TUInt64
    // Other primitives
    | TBool
    | TFloat64
    | TString
    | TBytes     // Byte array: [length:8][data:N][refcount:8]
    | TChar      // Extended Grapheme Cluster (single visual character)
    | TUnit
    | TFunction of Type list * Type  // parameter types * return type
    | TTuple of Type list             // tuple type: (Int, Bool, String)
    | TRecord of string               // record type by name: Point, Color, etc.
    | TSum of string * Type list      // sum type by name with type args: Result<Int64, String>
    | TList of Type                    // List<T> - polymorphic list type
    | TVar of string                  // type variable: T, A, B, etc. (for generics)
    | TRawPtr                         // Raw pointer to unmanaged memory (internal, for HAMT)
    | TDict of keyType:Type * valueType:Type  // Dict<K, V> - HAMT dictionary (K=Int64 for now)

/// Binary operators
type BinOp =
    // Arithmetic
    | Add
    | Sub
    | Mul
    | Div
    | Mod  // %
    // Bitwise operations
    | Shl     // << (left shift)
    | Shr     // >> (right shift)
    | BitAnd  // & (bitwise and)
    | BitOr   // | (bitwise or)
    | BitXor  // ^ (bitwise xor)
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

/// Part of an interpolated string: either a literal or an expression
type StringPart =
    | StringText of string    // Literal text: "Hello "
    | StringExpr of Expr      // Interpolated expression: {name}

/// Expression nodes
and Expr =
    | UnitLiteral                           // Unit value: ()
    | IntLiteral of int64                   // 64-bit signed (default): 42, 42L
    | Int8Literal of sbyte                  // 8-bit signed: 42y
    | Int16Literal of int16                 // 16-bit signed: 42s
    | Int32Literal of int32                 // 32-bit signed: 42l
    | UInt8Literal of byte                  // 8-bit unsigned: 42uy
    | UInt16Literal of uint16               // 16-bit unsigned: 42us
    | UInt32Literal of uint32               // 32-bit unsigned: 42ul
    | UInt64Literal of uint64               // 64-bit unsigned: 42UL
    | BoolLiteral of bool
    | StringLiteral of string
    | CharLiteral of string   // Single Extended Grapheme Cluster stored as UTF-8 string
    | FloatLiteral of float
    | InterpolatedString of StringPart list // $"Hello {name}!"
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
    | RecordUpdate of record:Expr * updates:(string * Expr) list      // { record with x = 1, y = 2 }
    | RecordAccess of record:Expr * fieldName:string                  // p.x, p.y
    | Constructor of typeName:string * variantName:string * payload:Expr option  // Red, Some(42)
    | Match of scrutinee:Expr * cases:MatchCase list  // match e with | p1 when g -> e1 | p2 -> e2
    | ListLiteral of Expr list                               // [1, 2, 3]
    | ListCons of head:Expr list * tail:Expr                 // [a, b, ...rest]
    | Lambda of parameters:(string * Type) list * body:Expr  // (x: int) => x + 1
    | Apply of func:Expr * args:Expr list                    // Apply function expr: f(x) where f is expression
    | FuncRef of funcName:string                             // Reference to a function (for passing as value)
    | Closure of funcName:string * captures:Expr list        // Closure: function + captured values

/// Match case with optional guard clause and pattern grouping
/// Syntax: | pat1 | pat2 when guard -> body
and MatchCase = {
    Patterns: Pattern list    // One or more patterns (pattern grouping via |)
    Guard: Expr option        // Optional guard clause (when condition)
    Body: Expr                // Body expression
}

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
    | TypeAlias of name:string * typeParams:string list * targetType:Type              // type Id = String

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
    TypeParams: string list          // Type parameters (e.g., ["v"] for generic intrinsics)
    ParamTypes: Type list            // Parameter types (may contain TVar references)
    ReturnType: Type                 // Return type (may contain TVar references)
}

/// Module definition - represents a namespace of functions
type ModuleDef = {
    Name: string                     // Full module path (e.g., "Stdlib.Int64")
    Functions: ModuleFunc list       // Functions in this module
}

/// Module registry - maps full function paths to their definitions
type ModuleRegistry = Map<string, ModuleFunc>
