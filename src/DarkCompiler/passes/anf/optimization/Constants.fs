// Constants.fs - Fold typed ANF constants and strength-reduce scalar operations.

module ANFConstants

open MemoryModel
open ANF

type ConstEnv = Map<TempId, Atom>

/// Environment mapping TempIds to source types for type-sensitive rewrites.
type TypeEnv = Map<TempId, AST.Type>

/// Environment mapping locally allocated tuples to ownership-safe element atoms.
type TupleEnv = Map<TempId, Map<int, Atom>>

/// Optimization toggles for ANF optimization passes
type OptimizeOptions = {
    EnableConstFolding: bool
    EnableConstProp: bool
    EnableCopyProp: bool
    EnableDCE: bool
    EnableCSE: bool
    EnableStrengthReduction: bool
    EnableTailRecursionModuloOperation: bool
}

/// Type metadata needed for ownership-sensitive optimizer decisions.
type OptimizeContext = {
    TypeReg: Map<string, (string * AST.Type) list>
    RecordTypeParams: Map<string, string list>
    SumShapeReg: RcSumShapeRegistry
    FunctionNames: Map<AST.FunctionId, string>
}

let defaultOptimizeOptions = {
    EnableConstFolding = true
    EnableConstProp = true
    EnableCopyProp = true
    EnableDCE = true
    EnableCSE = true
    EnableStrengthReduction = true
    EnableTailRecursionModuloOperation = true
}

/// Check if n is a power of 2, and if so return its log2
/// Returns None if n is not a power of 2 or is <= 0
let tryLog2 (n: int64) : int64 option =
    if n <= 0L || (n &&& (n - 1L)) <> 0L then None
    else
        let rec countBits acc x =
            if x = 1L then acc
            else countBits (acc + 1L) (x >>> 1)
        Some (countBits 0L n)

/// Check if an unsigned n is a power of 2, and if so return its log2
let tryLog2UInt64 (n: uint64) : int64 option =
    if n = 0UL || (n &&& (n - 1UL)) <> 0UL then None
    else
        let rec countBits acc x =
            if x = 1UL then acc
            else countBits (acc + 1L) (x >>> 1)
        Some (countBits 0L n)

/// Euclidean modulo: result has the sign of the divisor
let euclideanMod (a: int64) (b: int64) : int64 =
    let remainder = a % b
    if remainder = 0L then 0L
    elif (remainder > 0L && b < 0L) || (remainder < 0L && b > 0L) then remainder + b
    else remainder

let tryTruncateFloatToInt64 (f: float) : int64 option =
    let int64Min = -9223372036854775808.0
    let int64MaxExclusive = 9223372036854775808.0
    if System.Double.IsFinite f && f >= int64Min && f < int64MaxExclusive then
        Some (int64 (System.Math.Truncate f))
    else
        None

/// Fold a binary operation on constants
let foldBinOp (op: BinOp) (left: Atom) (right: Atom) : CExpr option =
    match op, left, right with
    // Int64 arithmetic (unchecked - overflow wraps)
    | Add, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (IntLiteral (Int64 (a + b))))
    | Sub, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (IntLiteral (Int64 (a - b))))
    | Mul, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (IntLiteral (Int64 (a * b))))
    | Div, IntLiteral (Int64 a), IntLiteral (Int64 b) when b <> 0L && not (a = System.Int64.MinValue && b = -1L) -> Some (Atom (IntLiteral (Int64 (a / b))))
    // Skip folding INT64_MIN / -1 - F# throws but runtime handles it (returns INT64_MIN)
    | Div, IntLiteral (Int64 _), IntLiteral (Int64 _) -> None
    | Mod, IntLiteral (Int64 a), IntLiteral (Int64 b) when b > 0L -> Some (Atom (IntLiteral (Int64 (euclideanMod a b))))
    | Shl, IntLiteral (Int64 a), IntLiteral (Int64 b) when b >= 0L && b < 64L -> Some (Atom (IntLiteral (Int64 (a <<< int b))))
    | Shr, IntLiteral (Int64 a), IntLiteral (Int64 b) when b >= 0L && b < 64L -> Some (Atom (IntLiteral (Int64 (int64 (uint64 a >>> int b)))))
    | BitAnd, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (IntLiteral (Int64 (a &&& b))))
    | BitOr, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (IntLiteral (Int64 (a ||| b))))
    | BitXor, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (IntLiteral (Int64 (a ^^^ b))))

    // UInt64 arithmetic (unchecked - overflow wraps)
    | Add, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (IntLiteral (UInt64 (a + b))))
    | Sub, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (IntLiteral (UInt64 (a - b))))
    | Mul, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (IntLiteral (UInt64 (a * b))))
    | Div, IntLiteral (UInt64 a), IntLiteral (UInt64 b) when b <> 0UL -> Some (Atom (IntLiteral (UInt64 (a / b))))
    | Mod, IntLiteral (UInt64 a), IntLiteral (UInt64 b) when b <> 0UL -> Some (Atom (IntLiteral (UInt64 (a % b))))

    // UInt64 bitwise operations
    | BitAnd, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (IntLiteral (UInt64 (a &&& b))))
    | BitOr, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (IntLiteral (UInt64 (a ||| b))))
    | BitXor, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (IntLiteral (UInt64 (a ^^^ b))))

    // Float arithmetic
    | Add, FloatLiteral a, FloatLiteral b -> Some (Atom (FloatLiteral (a + b)))
    | Sub, FloatLiteral a, FloatLiteral b -> Some (Atom (FloatLiteral (a - b)))
    | Mul, FloatLiteral a, FloatLiteral b -> Some (Atom (FloatLiteral (a * b)))
    | Div, FloatLiteral a, FloatLiteral b -> Some (Atom (FloatLiteral (a / b)))

    // Float comparisons
    | Eq, FloatLiteral a, FloatLiteral b -> Some (Atom (BoolLiteral (a = b)))
    | Neq, FloatLiteral a, FloatLiteral b -> Some (Atom (BoolLiteral (a <> b)))
    | Lt, FloatLiteral a, FloatLiteral b -> Some (Atom (BoolLiteral (a < b)))
    | Gt, FloatLiteral a, FloatLiteral b -> Some (Atom (BoolLiteral (a > b)))
    | Lte, FloatLiteral a, FloatLiteral b -> Some (Atom (BoolLiteral (a <= b)))
    | Gte, FloatLiteral a, FloatLiteral b -> Some (Atom (BoolLiteral (a >= b)))

    // Int64 comparisons
    | Eq, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a = b)))
    | Neq, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a <> b)))
    | Lt, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a < b)))
    | Gt, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a > b)))
    | Lte, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a <= b)))
    | Gte, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a >= b)))

    // UInt64 comparisons
    | Eq, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (BoolLiteral (a = b)))
    | Neq, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (BoolLiteral (a <> b)))
    | Lt, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (BoolLiteral (a < b)))
    | Gt, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (BoolLiteral (a > b)))
    | Lte, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (BoolLiteral (a <= b)))
    | Gte, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (BoolLiteral (a >= b)))

    // Boolean comparisons
    | Eq, BoolLiteral a, BoolLiteral b -> Some (Atom (BoolLiteral (a = b)))
    | Neq, BoolLiteral a, BoolLiteral b -> Some (Atom (BoolLiteral (a <> b)))
    | Eq, x, BoolLiteral true -> Some (Atom x)
    | Eq, BoolLiteral true, x -> Some (Atom x)
    | Eq, x, BoolLiteral false -> Some (UnaryPrim (Not, x))
    | Eq, BoolLiteral false, x -> Some (UnaryPrim (Not, x))
    | Neq, x, BoolLiteral true -> Some (UnaryPrim (Not, x))
    | Neq, BoolLiteral true, x -> Some (UnaryPrim (Not, x))
    | Neq, x, BoolLiteral false -> Some (Atom x)
    | Neq, BoolLiteral false, x -> Some (Atom x)

    // Boolean operations
    | And, BoolLiteral a, BoolLiteral b -> Some (Atom (BoolLiteral (a && b)))
    | Or, BoolLiteral a, BoolLiteral b -> Some (Atom (BoolLiteral (a || b)))

    // String comparisons
    | Eq, StringLiteral a, StringLiteral b -> Some (Atom (BoolLiteral (a = b)))
    | Neq, StringLiteral a, StringLiteral b -> Some (Atom (BoolLiteral (a <> b)))

    // Algebraic identities (strength reduction) - Int64
    | Add, IntLiteral (Int64 0L), x -> Some (Atom x)
    | Add, x, IntLiteral (Int64 0L) -> Some (Atom x)
    | Add, x, IntLiteral (Int64 n) when n < 0L && n <> System.Int64.MinValue ->
        Some (Prim (Sub, x, IntLiteral (Int64 (-n))))
    | Add, IntLiteral (Int64 n), x when n < 0L && n <> System.Int64.MinValue ->
        Some (Prim (Sub, x, IntLiteral (Int64 (-n))))
    | Sub, x, IntLiteral (Int64 0L) -> Some (Atom x)
    | Sub, x, IntLiteral (Int64 n) when n < 0L && n <> System.Int64.MinValue ->
        Some (Prim (Add, x, IntLiteral (Int64 (-n))))
    | Sub, IntLiteral (Int64 0L), x -> Some (UnaryPrim (Neg, x))
    | Mul, IntLiteral (Int64 1L), x -> Some (Atom x)
    | Mul, x, IntLiteral (Int64 1L) -> Some (Atom x)
    | Mul, IntLiteral (Int64 -1L), x -> Some (UnaryPrim (Neg, x))
    | Mul, x, IntLiteral (Int64 -1L) -> Some (UnaryPrim (Neg, x))
    | Mul, IntLiteral (Int64 0L), _ -> Some (Atom (IntLiteral (Int64 0L)))
    | Mul, _, IntLiteral (Int64 0L) -> Some (Atom (IntLiteral (Int64 0L)))
    | Div, x, IntLiteral (Int64 1L) -> Some (Atom x)
    | Div, x, IntLiteral (Int64 -1L) -> Some (UnaryPrim (Neg, x))
    | Mod, _, IntLiteral (Int64 1L) -> Some (Atom (IntLiteral (Int64 0L)))
    | Mod, _, IntLiteral (Int64 -1L) -> Some (Atom (IntLiteral (Int64 0L)))
    | Shl, x, IntLiteral (Int64 0L) -> Some (Atom x)
    | Shr, x, IntLiteral (Int64 0L) -> Some (Atom x)
    | Shl, IntLiteral (Int64 0L), _ -> Some (Atom (IntLiteral (Int64 0L)))
    | Shr, IntLiteral (Int64 0L), _ -> Some (Atom (IntLiteral (Int64 0L)))
    | BitAnd, _, IntLiteral (Int64 0L) -> Some (Atom (IntLiteral (Int64 0L)))
    | BitAnd, IntLiteral (Int64 0L), _ -> Some (Atom (IntLiteral (Int64 0L)))
    | BitAnd, x, IntLiteral (Int64 -1L) -> Some (Atom x)
    | BitAnd, IntLiteral (Int64 -1L), x -> Some (Atom x)
    | BitOr, x, IntLiteral (Int64 0L) -> Some (Atom x)
    | BitOr, IntLiteral (Int64 0L), x -> Some (Atom x)
    | BitOr, _, IntLiteral (Int64 -1L) -> Some (Atom (IntLiteral (Int64 -1L)))
    | BitOr, IntLiteral (Int64 -1L), _ -> Some (Atom (IntLiteral (Int64 -1L)))
    | BitXor, x, IntLiteral (Int64 0L) -> Some (Atom x)
    | BitXor, IntLiteral (Int64 0L), x -> Some (Atom x)
    | BitXor, x, IntLiteral (Int64 -1L) -> Some (UnaryPrim (BitNot, x))
    | BitXor, IntLiteral (Int64 -1L), x -> Some (UnaryPrim (BitNot, x))

    // Algebraic identities - UInt64
    | Add, IntLiteral (UInt64 0UL), x -> Some (Atom x)
    | Add, x, IntLiteral (UInt64 0UL) -> Some (Atom x)
    | Sub, x, IntLiteral (UInt64 0UL) -> Some (Atom x)
    | Mul, IntLiteral (UInt64 1UL), x -> Some (Atom x)
    | Mul, x, IntLiteral (UInt64 1UL) -> Some (Atom x)
    | Mul, IntLiteral (UInt64 0UL), _ -> Some (Atom (IntLiteral (UInt64 0UL)))
    | Mul, _, IntLiteral (UInt64 0UL) -> Some (Atom (IntLiteral (UInt64 0UL)))
    | Div, x, IntLiteral (UInt64 1UL) -> Some (Atom x)
    | Mod, _, IntLiteral (UInt64 1UL) -> Some (Atom (IntLiteral (UInt64 0UL)))
    | BitAnd, _, IntLiteral (UInt64 0UL) -> Some (Atom (IntLiteral (UInt64 0UL)))
    | BitAnd, IntLiteral (UInt64 0UL), _ -> Some (Atom (IntLiteral (UInt64 0UL)))
    | BitOr, x, IntLiteral (UInt64 0UL) -> Some (Atom x)
    | BitOr, IntLiteral (UInt64 0UL), x -> Some (Atom x)
    | BitXor, x, IntLiteral (UInt64 0UL) -> Some (Atom x)
    | BitXor, IntLiteral (UInt64 0UL), x -> Some (Atom x)

    // Algebraic identities - Float
    // Note: We skip 0.0 * x -> 0.0 because 0.0 * inf = NaN, 0.0 * NaN = NaN
    | Add, FloatLiteral 0.0, x -> Some (Atom x)
    | Add, x, FloatLiteral 0.0 -> Some (Atom x)
    | Sub, x, FloatLiteral 0.0 -> Some (Atom x)
    | Sub, FloatLiteral 0.0, x -> Some (FloatNeg x)
    | Mul, FloatLiteral 1.0, x -> Some (Atom x)
    | Mul, x, FloatLiteral 1.0 -> Some (Atom x)
    | Mul, FloatLiteral -1.0, x -> Some (FloatNeg x)
    | Mul, x, FloatLiteral -1.0 -> Some (FloatNeg x)
    | Div, x, FloatLiteral 1.0 -> Some (Atom x)
    | Div, x, FloatLiteral -1.0 -> Some (FloatNeg x)

    // Self-identities on integer operations. Float is excluded where NaN changes identity laws.
    | Sub, Var a, Var b when a = b -> Some (Atom (IntLiteral (Int64 0L)))
    | BitAnd, Var a, Var b when a = b -> Some (Atom (Var a))
    | BitOr, Var a, Var b when a = b -> Some (Atom (Var a))
    | BitXor, Var a, Var b when a = b -> Some (Atom (IntLiteral (Int64 0L)))
    | And, Var a, Var b when a = b -> Some (Atom (Var a))
    | Or, Var a, Var b when a = b -> Some (Atom (Var a))

    // Short-circuit boolean
    | And, BoolLiteral false, _ -> Some (Atom (BoolLiteral false))
    | And, _, BoolLiteral false -> Some (Atom (BoolLiteral false))
    | And, BoolLiteral true, x -> Some (Atom x)
    | And, x, BoolLiteral true -> Some (Atom x)
    | Or, BoolLiteral true, _ -> Some (Atom (BoolLiteral true))
    | Or, _, BoolLiteral true -> Some (Atom (BoolLiteral true))
    | Or, BoolLiteral false, x -> Some (Atom x)
    | Or, x, BoolLiteral false -> Some (Atom x)

    | _ -> None

let internal isInt64Atom (typeEnv: TypeEnv) (atom: Atom) : bool =
    match atom with
    | IntLiteral (Int64 _) -> true
    | Var tid ->
        match Map.tryFind tid typeEnv with
        | Some AST.TInt64 -> true
        | _ -> false
    | _ -> false

let private isUInt64Atom (typeEnv: TypeEnv) (atom: Atom) : bool =
    match atom with
    | IntLiteral (UInt64 _) -> true
    | Var tid ->
        match Map.tryFind tid typeEnv with
        | Some AST.TUInt64 -> true
        | _ -> false
    | _ -> false

let private isBoolAtom (typeEnv: TypeEnv) (atom: Atom) : bool =
    match atom with
    | BoolLiteral _ -> true
    | Var tid ->
        match Map.tryFind tid typeEnv with
        | Some AST.TBool -> true
        | _ -> false
    | _ -> false

let private isFloatAtom (typeEnv: TypeEnv) (atom: Atom) : bool =
    match atom with
    | FloatLiteral _ -> true
    | Var tid ->
        match Map.tryFind tid typeEnv with
        | Some AST.TFloat64 -> true
        | _ -> false
    | _ -> false

let private isUnsignedIntegerAtom (typeEnv: TypeEnv) (atom: Atom) : bool =
    match atom with
    | IntLiteral (UInt8 _)
    | IntLiteral (UInt16 _)
    | IntLiteral (UInt32 _)
    | IntLiteral (UInt64 _) -> true
    | Var tid ->
        match Map.tryFind tid typeEnv with
        | Some AST.TUInt8
        | Some AST.TUInt16
        | Some AST.TUInt32
        | Some AST.TUInt64 -> true
        | _ -> false
    | _ -> false

let internal isIntegerAtom (typeEnv: TypeEnv) (atom: Atom) : bool =
    match atom with
    | IntLiteral _ -> true
    | Var tid ->
        match Map.tryFind tid typeEnv with
        | Some AST.TInt8
        | Some AST.TInt16
        | Some AST.TInt32
        | Some AST.TInt64
        | Some AST.TInt128
        | Some AST.TUInt8
        | Some AST.TUInt16
        | Some AST.TUInt32
        | Some AST.TUInt64
        | Some AST.TUInt128 -> true
        | _ -> false
    | _ -> false

let tryStrengthReduce (typeEnv: TypeEnv) (op: BinOp) (left: Atom) (right: Atom) : CExpr option =
    match op, left, right with
    | Add, Var leftTid, Var rightTid when leftTid = rightTid && isInt64Atom typeEnv left ->
        Some (Prim (Shl, left, IntLiteral (Int64 1L)))
    | Eq, Var leftTid, Var rightTid when leftTid = rightTid && isBoolAtom typeEnv left ->
        Some (Atom (BoolLiteral true))
    | Neq, Var leftTid, Var rightTid when leftTid = rightTid && isBoolAtom typeEnv left ->
        Some (Atom (BoolLiteral false))
    | Eq, Var leftTid, Var rightTid when leftTid = rightTid && isIntegerAtom typeEnv left ->
        Some (Atom (BoolLiteral true))
    | Neq, Var leftTid, Var rightTid when leftTid = rightTid && isIntegerAtom typeEnv left ->
        Some (Atom (BoolLiteral false))
    // Strict self-comparisons are false for every IEEE 754 value, including NaN.
    | Lt, Var leftTid, Var rightTid when leftTid = rightTid && isFloatAtom typeEnv left ->
        Some (Atom (BoolLiteral false))
    | Gt, Var leftTid, Var rightTid when leftTid = rightTid && isFloatAtom typeEnv left ->
        Some (Atom (BoolLiteral false))
    | Lt, Var leftTid, Var rightTid when leftTid = rightTid && isIntegerAtom typeEnv left ->
        Some (Atom (BoolLiteral false))
    | Gt, Var leftTid, Var rightTid when leftTid = rightTid && isIntegerAtom typeEnv left ->
        Some (Atom (BoolLiteral false))
    | Lte, Var leftTid, Var rightTid when leftTid = rightTid && isIntegerAtom typeEnv left ->
        Some (Atom (BoolLiteral true))
    | Gte, Var leftTid, Var rightTid when leftTid = rightTid && isIntegerAtom typeEnv left ->
        Some (Atom (BoolLiteral true))
    | Mul, x, IntLiteral (Int64 n) ->
        match tryLog2 n with
        | Some shift -> Some (Prim (Shl, x, IntLiteral (Int64 shift)))
        | None -> None
    | Mul, IntLiteral (Int64 n), x ->
        match tryLog2 n with
        | Some shift -> Some (Prim (Shl, x, IntLiteral (Int64 shift)))
        | None -> None
    | Mul, x, IntLiteral (UInt64 n) when isUInt64Atom typeEnv x ->
        match tryLog2UInt64 n with
        | Some shift -> Some (Prim (Shl, x, IntLiteral (Int64 shift)))
        | None -> None
    | Mul, IntLiteral (UInt64 n), x when isUInt64Atom typeEnv x ->
        match tryLog2UInt64 n with
        | Some shift -> Some (Prim (Shl, x, IntLiteral (Int64 shift)))
        | None -> None
    | Mod, x, IntLiteral (Int64 n) when n > 0L ->
        // For positive power-of-two divisors, Euclidean remainder equals x & (n - 1)
        match tryLog2 n with
        | Some _ -> Some (Prim (BitAnd, x, IntLiteral (Int64 (n - 1L))))
        | None -> None
    | Div, x, IntLiteral (Int64 n) when n > 0L && isUnsignedIntegerAtom typeEnv x ->
        match tryLog2 n with
        | Some shift -> Some (Prim (Shr, x, IntLiteral (Int64 shift)))
        | None -> None
    | Div, x, IntLiteral (UInt64 n) when isUInt64Atom typeEnv x ->
        match tryLog2UInt64 n with
        | Some shift -> Some (Prim (Shr, x, IntLiteral (Int64 shift)))
        | None -> None
    | Mod, x, IntLiteral (UInt64 n) when isUInt64Atom typeEnv x ->
        match tryLog2UInt64 n with
        | Some _ -> Some (Prim (BitAnd, x, IntLiteral (UInt64 (n - 1UL))))
        | None -> None
    // Float strength reduction: 2.0 * x -> x + x
    | Mul, FloatLiteral 2.0, x -> Some (Prim (Add, x, x))
    | Mul, x, FloatLiteral 2.0 -> Some (Prim (Add, x, x))
    // Float division by power of 2 -> multiplication by reciprocal
    // These reciprocals are exactly representable in IEEE 754
    | Div, x, FloatLiteral 2.0 -> Some (Prim (Mul, x, FloatLiteral 0.5))
    | Div, x, FloatLiteral -2.0 -> Some (Prim (Mul, x, FloatLiteral -0.5))
    | Div, x, FloatLiteral 4.0 -> Some (Prim (Mul, x, FloatLiteral 0.25))
    | Div, x, FloatLiteral -4.0 -> Some (Prim (Mul, x, FloatLiteral -0.25))
    | Div, x, FloatLiteral 8.0 -> Some (Prim (Mul, x, FloatLiteral 0.125))
    | Div, x, FloatLiteral -8.0 -> Some (Prim (Mul, x, FloatLiteral -0.125))
    | Div, x, FloatLiteral 16.0 -> Some (Prim (Mul, x, FloatLiteral 0.0625))
    | Div, x, FloatLiteral -16.0 -> Some (Prim (Mul, x, FloatLiteral -0.0625))
    | Div, x, FloatLiteral 32.0 -> Some (Prim (Mul, x, FloatLiteral 0.03125))
    | Div, x, FloatLiteral -32.0 -> Some (Prim (Mul, x, FloatLiteral -0.03125))
    | Div, x, FloatLiteral 64.0 -> Some (Prim (Mul, x, FloatLiteral 0.015625))
    | Div, x, FloatLiteral -64.0 -> Some (Prim (Mul, x, FloatLiteral -0.015625))
    | Div, x, FloatLiteral 128.0 -> Some (Prim (Mul, x, FloatLiteral 0.0078125))
    | Div, x, FloatLiteral -128.0 -> Some (Prim (Mul, x, FloatLiteral -0.0078125))
    | Div, x, FloatLiteral 256.0 -> Some (Prim (Mul, x, FloatLiteral 0.00390625))
    | Div, x, FloatLiteral -256.0 -> Some (Prim (Mul, x, FloatLiteral -0.00390625))
    | _ -> None

/// Fold a unary operation on constants
let foldUnaryOp (op: UnaryOp) (src: Atom) : CExpr option =
    match op, src with
    // Int64 negation (unchecked - INT64_MIN wraps to itself)
    | Neg, IntLiteral (Int64 n) -> Some (Atom (IntLiteral (Int64 (-n))))
    | Neg, FloatLiteral f -> Some (Atom (FloatLiteral (-f)))
    | Not, BoolLiteral b -> Some (Atom (BoolLiteral (not b)))
    // Bitwise NOT: flip all bits
    | BitNot, IntLiteral (Int64 n) -> Some (Atom (IntLiteral (Int64 (~~~n))))
    | _ -> None
