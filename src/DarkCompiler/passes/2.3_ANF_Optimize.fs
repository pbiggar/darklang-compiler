// 2.3_ANF_Optimize.fs - ANF Optimization Pass
//
// Performs optimizations on ANF before reference counting:
// - Constant folding: evaluate constant expressions at compile time
// - Constant propagation: replace variable uses with constant definitions
// - Copy propagation: eliminate trivial bindings
// - Dead code elimination: remove unused bindings
// - Common subexpression elimination: reuse earlier pure computations
// - Branch code motion: hoist identical pure leading branch computations
// - Reassociation: combine constants across adjacent Int64 additions and multiplications
// - Algebraic cancellation: remove paired Int64 additions and subtractions
// - Algebraic factoring: combine an Int64 product with its shared operand
// - Control-flow simplification: collapse Boolean literal branches
// - Instruction combining: fold single-use negation into integer subtraction
// - Strength reduction: replace pow2 mul/div/mod with shifts/bitwise ops
// - Closure devirtualization: directly call capture-free local closures that do not escape
// - Tail recursion modulo addition: turn safe Int64 sibling recursion into an accumulator loop
//
// These optimizations run in a loop until no more changes occur.

module ANF_Optimize

open ANF

/// Environment mapping TempIds to their constant values (for propagation)
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

let private isInt64Atom (typeEnv: TypeEnv) (atom: Atom) : bool =
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

let private isIntegerAtom (typeEnv: TypeEnv) (atom: Atom) : bool =
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

let private typeNeedsTypedAtomDceProtection (context: OptimizeContext) (typ: AST.Type) : bool =
    typ
    |> rcShapeOfTypeWithSums context.TypeReg context.RecordTypeParams context.SumShapeReg
    |> rcShapeNeedsOwnedScopeRelease

/// Projection bindings carry borrowing information into RC insertion, so only
/// bypass them when the selected element cannot own managed memory.
let private canForwardTupleElement (context: OptimizeContext) (typeEnv: TypeEnv) (atom: Atom) : bool =
    match atom with
    | UnitLiteral
    | IntLiteral _
    | BoolLiteral _
    | StringLiteral _
    | FloatLiteral _
    | FuncRef _ -> true
    | Var tid ->
        Map.tryFind tid typeEnv
        |> Option.exists (typeNeedsTypedAtomDceProtection context >> not)

/// Check if a CExpr has side effects
let hasSideEffects (context: OptimizeContext) (cexpr: CExpr) : bool =
    match cexpr with
    | Atom _ -> false
    | TypedAtom (_, typ) ->
        // Some internal lowerings materialize ownership only after tagging a
        // raw pointer with its heap type. Dropping that marker before RC
        // insertion can orphan the allocation even though the cast itself is
        // computationally pure.
        typeNeedsTypedAtomDceProtection context typ
    | Prim _ -> false
    | UnaryPrim _ -> false
    | IfValue _ -> false
    | TupleAlloc _ -> false
    | TupleGet _ -> false
    | RecordAlloc _ -> false
    | RecordGet _ -> false
    | RecordClone _ -> false
    // These have side effects
    | Call _ -> true
    | BorrowedCall _ -> true
    | TailCall _ -> true
    | IndirectCall _ -> true
    | IndirectTailCall _ -> true
    | ClosureAlloc _ -> true  // Allocates memory
    | ClosureCall _ -> true
    | ClosureTailCall _ -> true
    | StringConcat _ -> true  // Allocates memory
    | RefCountInc _ -> true
    | RefCountDec _ -> true
    | Print _ -> true
    | StdoutWrite _ -> true
    | StdinReadLine -> true
    | FileReadText _ -> true
    | FileExists _ -> true
    | FileWriteText _ -> true
    | FileAppendText _ -> true
    | FileDelete _ -> true
    | FileSetExecutable _ -> true
    | FileWriteFromPtr _ -> true  // File I/O
    | RawAlloc _ -> true  // Allocates memory
    | RawFree _ -> true   // Frees memory
    | RawGet _ -> false   // Pure memory read
    | RawTake _ -> true   // Ownership transfer; paired with clearing the source slot
    | RawGetByte _ -> false  // Pure memory read (byte)
    | RawWriteWord _ -> true    // Memory mutation
    | RawWriteByte _ -> true  // Memory mutation (byte)
    | RawSlotInit _ -> true  // Memory mutation plus possible ownership edge
    | StringToRawPtr _ -> false
    | RawPtrToString _ -> false
    | BlobToRawPtr _ -> false
    | RawPtrToBlob _ -> false
    | DictToRawPtr _ -> false
    | RawPtrToDict _ -> false
    | ListToRawPtr _ -> false
    | RawPtrToList _ -> false
    | FloatSqrt _ -> false  // Pure float operation
    | FloatAbs _ -> false   // Pure float operation
    | FloatNeg _ -> false   // Pure float operation
    | Int64ToFloat _ -> false // Pure conversion
    | FloatToInt64 _ -> false // Pure conversion
    | FloatToBits _ -> false // Pure conversion
    | RefCountIncString _ -> true   // Mutates refcount
    | RefCountDecString _ -> true   // Mutates refcount
    | RefCountIncBlob _ -> true    // Mutates refcount
    | RefCountDecBlob _ -> true    // Mutates refcount
    | RandomInt64 -> true   // Reads from OS random source
    | DateTimeNow -> true       // Reads current time (syscall)
    | Sleep _ -> true           // Blocks the current process
    | CliNative _ -> true
    | FloatToString _ -> false  // Pure conversion (but allocates - maybe should be true?)
    | RuntimeError _ -> true
    | RuntimeErrorString _ -> true

/// Add the TempId used by an atom to an existing liveness set.
let private addAtomUse (atom: Atom) (uses: Set<TempId>) : Set<TempId> =
    match atom with
    | Var tid -> Set.add tid uses
    | _ -> uses

let private addAtomUses (atoms: Atom list) (uses: Set<TempId>) : Set<TempId> =
    List.fold (fun uses atom -> addAtomUse atom uses) uses atoms

let atomUsesTemp (tid: TempId) (atom: Atom) : bool =
    match atom with
    | Var usedTid -> usedTid = tid
    | _ -> false

let atomsUseTemp (tid: TempId) (atoms: Atom list) : bool =
    List.exists (atomUsesTemp tid) atoms

/// Add every TempId used by a CExpr to an existing liveness set.
let private addCExprUses (cexpr: CExpr) (uses: Set<TempId>) : Set<TempId> =
    match cexpr with
    | Atom a -> addAtomUse a uses
    | TypedAtom (a, _) -> addAtomUse a uses
    | Prim (_, left, right) -> uses |> addAtomUse left |> addAtomUse right
    | UnaryPrim (_, src) -> addAtomUse src uses
    | IfValue (cond, thenVal, elseVal) ->
        uses |> addAtomUse cond |> addAtomUse thenVal |> addAtomUse elseVal
    | Call (_, args) -> addAtomUses args uses
    | BorrowedCall (_, args) -> addAtomUses args uses
    | TailCall (_, args) -> addAtomUses args uses
    | IndirectCall (func, args) ->
        uses |> addAtomUse func |> addAtomUses args
    | IndirectTailCall (func, args) ->
        uses |> addAtomUse func |> addAtomUses args
    | ClosureAlloc (_, captures) -> addAtomUses captures uses
    | ClosureCall (closure, args) ->
        uses |> addAtomUse closure |> addAtomUses args
    | ClosureTailCall (closure, args) ->
        uses |> addAtomUse closure |> addAtomUses args
    | TupleAlloc elems -> addAtomUses elems uses
    | TupleGet (tuple, _) -> addAtomUse tuple uses
    | RecordAlloc (_, fields) -> addAtomUses fields uses
    | RecordGet (_, record, _) -> addAtomUse record uses
    | RecordClone (_, record, fields) ->
        uses |> addAtomUse record |> addAtomUses fields
    | StringConcat (left, right) -> uses |> addAtomUse left |> addAtomUse right
    | RefCountInc (atom, _, _, _) -> addAtomUse atom uses
    | RefCountDec (atom, _, _, _) -> addAtomUse atom uses
    | Print (atom, _) -> addAtomUse atom uses
    | StdoutWrite (atom, _) -> addAtomUse atom uses
    | StdinReadLine -> uses
    | FileReadText path -> addAtomUse path uses
    | FileExists path -> addAtomUse path uses
    | FileWriteText (path, content) -> uses |> addAtomUse path |> addAtomUse content
    | FileAppendText (path, content) -> uses |> addAtomUse path |> addAtomUse content
    | FileDelete path -> addAtomUse path uses
    | FileSetExecutable path -> addAtomUse path uses
    | FileWriteFromPtr (path, ptr, length) ->
        uses |> addAtomUse path |> addAtomUse ptr |> addAtomUse length
    | RawAlloc numBytes -> addAtomUse numBytes uses
    | RawFree ptr -> addAtomUse ptr uses
    | RawGet (ptr, byteOffset, _) -> uses |> addAtomUse ptr |> addAtomUse byteOffset
    | RawTake (ptr, byteOffset, _) -> uses |> addAtomUse ptr |> addAtomUse byteOffset
    | RawGetByte (ptr, byteOffset) -> uses |> addAtomUse ptr |> addAtomUse byteOffset
    | RawWriteWord (ptr, byteOffset, value) ->
        uses |> addAtomUse ptr |> addAtomUse byteOffset |> addAtomUse value
    | RawWriteByte (ptr, byteOffset, value) ->
        uses |> addAtomUse ptr |> addAtomUse byteOffset |> addAtomUse value
    | RawSlotInit (ptr, byteOffset, value, _) ->
        uses |> addAtomUse ptr |> addAtomUse byteOffset |> addAtomUse value
    | StringToRawPtr value -> addAtomUse value uses
    | RawPtrToString ptr -> addAtomUse ptr uses
    | BlobToRawPtr value -> addAtomUse value uses
    | RawPtrToBlob ptr -> addAtomUse ptr uses
    | DictToRawPtr dict -> addAtomUse dict uses
    | RawPtrToDict (ptr, tag, _) -> uses |> addAtomUse ptr |> addAtomUse tag
    | ListToRawPtr list -> addAtomUse list uses
    | RawPtrToList (ptr, tag, _) -> uses |> addAtomUse ptr |> addAtomUse tag
    | FloatSqrt atom -> addAtomUse atom uses
    | FloatAbs atom -> addAtomUse atom uses
    | FloatNeg atom -> addAtomUse atom uses
    | Int64ToFloat atom -> addAtomUse atom uses
    | FloatToInt64 atom -> addAtomUse atom uses
    | FloatToBits atom -> addAtomUse atom uses
    | RefCountIncString str -> addAtomUse str uses
    | RefCountDecString str -> addAtomUse str uses
    | RefCountIncBlob bytes -> addAtomUse bytes uses
    | RefCountDecBlob bytes -> addAtomUse bytes uses
    | RandomInt64 -> uses  // No atoms
    | DateTimeNow -> uses      // No atoms
    | Sleep delayMs -> addAtomUse delayMs uses
    | CliNative (_, args) -> addAtomUses args uses
    | FloatToString atom -> addAtomUse atom uses
    | RuntimeError _ -> uses
    | RuntimeErrorString atom -> addAtomUse atom uses

/// Test whether a CExpr uses a TempId without constructing a liveness set.
let cexprUsesTemp (tid: TempId) (cexpr: CExpr) : bool =
    let used = atomUsesTemp tid
    let anyUsed = atomsUseTemp tid

    match cexpr with
    | Atom atom
    | TypedAtom (atom, _)
    | UnaryPrim (_, atom)
    | TupleGet (atom, _)
    | RecordGet (_, atom, _)
    | RefCountInc (atom, _, _, _)
    | RefCountDec (atom, _, _, _)
    | Print (atom, _)
    | FileReadText atom
    | FileExists atom
    | FileDelete atom
    | FileSetExecutable atom
    | RawAlloc atom
    | RawFree atom
    | StringToRawPtr atom
    | RawPtrToString atom
    | BlobToRawPtr atom
    | RawPtrToBlob atom
    | DictToRawPtr atom
    | ListToRawPtr atom
    | FloatSqrt atom
    | FloatAbs atom
    | FloatNeg atom
    | Int64ToFloat atom
    | FloatToInt64 atom
    | FloatToBits atom
    | RefCountIncString atom
    | RefCountDecString atom
    | RefCountIncBlob atom
    | RefCountDecBlob atom
    | FloatToString atom -> used atom
    | Sleep atom -> used atom
    | StdoutWrite (atom, _) -> used atom
    | Prim (_, left, right)
    | StringConcat (left, right)
    | FileWriteText (left, right)
    | FileAppendText (left, right)
    | RawGet (left, right, _)
    | RawTake (left, right, _)
    | RawGetByte (left, right)
    | RawPtrToDict (left, right, _)
    | RawPtrToList (left, right, _) -> used left || used right
    | IfValue (first, second, third)
    | FileWriteFromPtr (first, second, third)
    | RawWriteWord (first, second, third)
    | RawWriteByte (first, second, third)
    | RawSlotInit (first, second, third, _) ->
        used first || used second || used third
    | Call (_, atoms)
    | BorrowedCall (_, atoms)
    | TailCall (_, atoms)
    | ClosureAlloc (_, atoms)
    | TupleAlloc atoms
    | RecordAlloc (_, atoms) -> anyUsed atoms
    | RecordClone (_, record, fields) -> used record || anyUsed fields
    | CliNative (_, atoms) -> anyUsed atoms
    | IndirectCall (first, rest)
    | IndirectTailCall (first, rest)
    | ClosureCall (first, rest)
    | ClosureTailCall (first, rest) -> used first || anyUsed rest
    | RandomInt64
    | DateTimeNow
    | StdinReadLine
    | RuntimeError _ -> false
    | RuntimeErrorString atom -> used atom

/// Substitute atom in another atom
let substAtom (env: Map<TempId, Atom>) (atom: Atom) : Atom =
    match atom with
    | Var tid -> Map.tryFind tid env |> Option.defaultValue atom
    | _ -> atom

/// Substitute operands in one pass, preserving the original list when no atom
/// changes and sharing the untouched suffix after the final replacement.
let rec private substAtoms (env: Map<TempId, Atom>) (atoms: Atom list) : Atom list =
    match atoms with
    | [] -> atoms
    | atom :: rest ->
        let atom' = substAtom env atom
        let rest' = substAtoms env rest
        if atom' = atom && obj.ReferenceEquals (rest', rest) then
            atoms
        else
            atom' :: rest'

/// Substitute atoms in CExpr
let private substCExprValue (env: Map<TempId, Atom>) (cexpr: CExpr) : CExpr =
    let s = substAtom env
    match cexpr with
    | Atom a -> Atom (s a)
    | TypedAtom (a, t) -> TypedAtom (s a, t)
    | Prim (op, left, right) -> Prim (op, s left, s right)
    | UnaryPrim (op, src) -> UnaryPrim (op, s src)
    | IfValue (cond, thenVal, elseVal) -> IfValue (s cond, s thenVal, s elseVal)
    | Call (name, args) ->
        let args' = substAtoms env args
        if obj.ReferenceEquals (args', args) then cexpr else Call (name, args')
    | BorrowedCall (name, args) ->
        let args' = substAtoms env args
        if obj.ReferenceEquals (args', args) then cexpr else BorrowedCall (name, args')
    | TailCall (name, args) ->
        let args' = substAtoms env args
        if obj.ReferenceEquals (args', args) then cexpr else TailCall (name, args')
    | IndirectCall (func, args) ->
        let func' = s func
        let args' = substAtoms env args
        if func' = func && obj.ReferenceEquals (args', args) then cexpr else IndirectCall (func', args')
    | IndirectTailCall (func, args) ->
        let func' = s func
        let args' = substAtoms env args
        if func' = func && obj.ReferenceEquals (args', args) then cexpr else IndirectTailCall (func', args')
    | ClosureAlloc (name, captures) ->
        let captures' = substAtoms env captures
        if obj.ReferenceEquals (captures', captures) then cexpr else ClosureAlloc (name, captures')
    | ClosureCall (closure, args) ->
        let closure' = s closure
        let args' = substAtoms env args
        if closure' = closure && obj.ReferenceEquals (args', args) then cexpr else ClosureCall (closure', args')
    | ClosureTailCall (closure, args) ->
        let closure' = s closure
        let args' = substAtoms env args
        if closure' = closure && obj.ReferenceEquals (args', args) then cexpr else ClosureTailCall (closure', args')
    | TupleAlloc elems ->
        let elems' = substAtoms env elems
        if obj.ReferenceEquals (elems', elems) then cexpr else TupleAlloc elems'
    | TupleGet (tuple, idx) -> TupleGet (s tuple, idx)
    | RecordAlloc (descriptor, fields) ->
        let fields' = substAtoms env fields
        if obj.ReferenceEquals (fields', fields) then cexpr else RecordAlloc (descriptor, fields')
    | RecordGet (descriptor, record, idx) -> RecordGet (descriptor, s record, idx)
    | RecordClone (descriptor, record, fields) ->
        RecordClone (descriptor, s record, substAtoms env fields)
    | StringConcat (left, right) -> StringConcat (s left, s right)
    | RefCountInc (atom, size, kind, sourceType) -> RefCountInc (s atom, size, kind, sourceType)
    | RefCountDec (atom, size, kind, sourceType) -> RefCountDec (s atom, size, kind, sourceType)
    | Print (atom, t) -> Print (s atom, t)
    | StdoutWrite (atom, appendNewline) -> StdoutWrite (s atom, appendNewline)
    | StdinReadLine -> StdinReadLine
    | FileReadText path -> FileReadText (s path)
    | FileExists path -> FileExists (s path)
    | FileWriteText (path, content) -> FileWriteText (s path, s content)
    | FileAppendText (path, content) -> FileAppendText (s path, s content)
    | FileDelete path -> FileDelete (s path)
    | FileSetExecutable path -> FileSetExecutable (s path)
    | FileWriteFromPtr (path, ptr, length) -> FileWriteFromPtr (s path, s ptr, s length)
    | RawAlloc numBytes -> RawAlloc (s numBytes)
    | RawFree ptr -> RawFree (s ptr)
    | RawGet (ptr, byteOffset, valueType) -> RawGet (s ptr, s byteOffset, valueType)
    | RawTake (ptr, byteOffset, valueType) -> RawTake (s ptr, s byteOffset, valueType)
    | RawGetByte (ptr, byteOffset) -> RawGetByte (s ptr, s byteOffset)
    | RawWriteWord (ptr, byteOffset, value) -> RawWriteWord (s ptr, s byteOffset, s value)
    | RawWriteByte (ptr, byteOffset, value) -> RawWriteByte (s ptr, s byteOffset, s value)
    | RawSlotInit (ptr, byteOffset, value, valueType) -> RawSlotInit (s ptr, s byteOffset, s value, valueType)
    | StringToRawPtr value -> StringToRawPtr (s value)
    | RawPtrToString ptr -> RawPtrToString (s ptr)
    | BlobToRawPtr value -> BlobToRawPtr (s value)
    | RawPtrToBlob ptr -> RawPtrToBlob (s ptr)
    | DictToRawPtr dict -> DictToRawPtr (s dict)
    | RawPtrToDict (ptr, tag, dictType) -> RawPtrToDict (s ptr, s tag, dictType)
    | ListToRawPtr list -> ListToRawPtr (s list)
    | RawPtrToList (ptr, tag, listType) -> RawPtrToList (s ptr, s tag, listType)
    | FloatSqrt atom -> FloatSqrt (s atom)
    | FloatAbs atom -> FloatAbs (s atom)
    | FloatNeg atom -> FloatNeg (s atom)
    | Int64ToFloat atom -> Int64ToFloat (s atom)
    | FloatToInt64 atom -> FloatToInt64 (s atom)
    | FloatToBits atom -> FloatToBits (s atom)
    | RefCountIncString str -> RefCountIncString (s str)
    | RefCountDecString str -> RefCountDecString (s str)
    | RefCountIncBlob bytes -> RefCountIncBlob (s bytes)
    | RefCountDecBlob bytes -> RefCountDecBlob (s bytes)
    | RandomInt64 -> RandomInt64
    | DateTimeNow -> DateTimeNow
    | Sleep delayMs -> Sleep (s delayMs)
    | CliNative (operation, args) -> CliNative (operation, List.map s args)
    | FloatToString atom -> FloatToString (s atom)
    | RuntimeError message -> RuntimeError message
    | RuntimeErrorString atom -> RuntimeErrorString (s atom)

/// Substitute atoms in a CExpr, preserving the original value when there is no
/// substitution environment.
let substCExpr (env: Map<TempId, Atom>) (cexpr: CExpr) : CExpr =
    if Map.isEmpty env then
        cexpr
    else
        substCExprValue env cexpr

/// Substitute atoms while reporting list-bearing no-op expressions by identity,
/// avoiding a structural comparison of their operands.
let private substCExprWithChange (env: Map<TempId, Atom>) (cexpr: CExpr) : struct (CExpr * bool) =
    if Map.isEmpty env then
        struct (cexpr, false)
    else
        let cexpr' = substCExprValue env cexpr
        let changed =
            match cexpr with
            | Call _
            | BorrowedCall _
            | TailCall _
            | IndirectCall _
            | IndirectTailCall _
            | ClosureAlloc _
            | ClosureCall _
            | ClosureTailCall _
            | TupleAlloc _ -> not (obj.ReferenceEquals (cexpr', cexpr))
            | _ -> cexpr' <> cexpr
        struct (cexpr', changed)

/// Optimize a CExpr with constant folding
let optimizeCExpr (options: OptimizeOptions) (env: ConstEnv) (typeEnv: TypeEnv) (tupleEnv: TupleEnv) (cexpr: CExpr) : CExpr * bool =
    // First, substitute known constants
    let struct (cexpr', substitutionChanged) = substCExprWithChange env cexpr

    let tryConstFold () =
        if options.EnableConstFolding then
            match cexpr' with
            | Prim (op, left, right) ->
                match foldBinOp op left right with
                | Some folded -> Some folded
                | None -> None
            | UnaryPrim (op, src) -> foldUnaryOp op src
            | FloatNeg (FloatLiteral f) -> Some (Atom (FloatLiteral (-f)))
            | FloatAbs (FloatLiteral f) -> Some (Atom (FloatLiteral (abs f)))
            | FloatSqrt (FloatLiteral f) -> Some (Atom (FloatLiteral (sqrt f)))
            | Int64ToFloat (IntLiteral (Int64 n)) -> Some (Atom (FloatLiteral (float n)))
            | FloatToInt64 (FloatLiteral f) ->
                tryTruncateFloatToInt64 f
                |> Option.map (fun n -> Atom (IntLiteral (Int64 n)))
            | FloatToBits (FloatLiteral f) ->
                Some (Atom (IntLiteral (UInt64 (System.BitConverter.DoubleToUInt64Bits f))))
            | StringConcat (StringLiteral left, StringLiteral right) ->
                Some (Atom (StringLiteral (left + right)))
            | StringConcat (left, StringLiteral "") -> Some (Atom left)
            | StringConcat (StringLiteral "", right) -> Some (Atom right)
            | Call ("Stdlib.String.__appendNormalized", [StringLiteral left; StringLiteral right]) ->
                let normalized = (left + right).Normalize(System.Text.NormalizationForm.FormC)
                Some (Atom (StringLiteral normalized))
            | Call ("Stdlib.String.__appendNormalized", [left; StringLiteral ""]) ->
                Some (Atom left)
            | Call ("Stdlib.String.__appendNormalized", [StringLiteral ""; right]) ->
                Some (Atom right)
            | TupleGet (Var tupleTid, index) ->
                Map.tryFind tupleTid tupleEnv
                |> Option.bind (Map.tryFind index)
                |> Option.map Atom
            | Call ("__string_eq", [StringLiteral left; StringLiteral right]) ->
                Some (Atom (BoolLiteral (left = right)))
            | Call ("__string_eq", [Var leftTid; Var rightTid]) when leftTid = rightTid ->
                Some (Atom (BoolLiteral true))
            | IfValue (BoolLiteral true, thenVal, _) -> Some (Atom thenVal)
            | IfValue (BoolLiteral false, _, elseVal) -> Some (Atom elseVal)
            | IfValue (_, thenVal, elseVal) when thenVal = elseVal -> Some (Atom thenVal)
            | _ -> None
        else
            None

    match tryConstFold () with
    | Some folded -> (folded, true)
    | None ->
        if options.EnableStrengthReduction then
            match cexpr' with
            | Prim (op, left, right) ->
                match tryStrengthReduce typeEnv op left right with
                | Some reduced -> (reduced, true)
                | None -> (cexpr', substitutionChanged)
            | _ -> (cexpr', substitutionChanged)
        else
            (cexpr', substitutionChanged)

type OptimizeAExprResult = {
    Expr: AExpr
    Changed: bool
    Uses: Set<TempId>
}

type CSEnv = Map<CExpr, TempId>

let private isCommutativeBinOp (op: BinOp) : bool =
    match op with
    | Add
    | Mul
    | Eq
    | Neq
    | And
    | Or
    | BitAnd
    | BitOr
    | BitXor -> true
    | Sub
    | Div
    | Mod
    | Lt
    | Gt
    | Lte
    | Gte
    | Shl
    | Shr -> false

let private cseKey (cexpr: CExpr) : CExpr =
    match cexpr with
    // Canonicalize relational comparisons to their less-than spelling so
    // reversing both the operator and operands produces the same CSE key.
    | Prim (Gt, left, right) -> Prim (Lt, right, left)
    | Prim (Gte, left, right) -> Prim (Lte, right, left)
    | Prim (op, left, right) when isCommutativeBinOp op && compare right left < 0 ->
        Prim (op, right, left)
    | _ -> cexpr

let private isCSEEligible (cexpr: CExpr) : bool =
    match cexpr with
    | Prim _
    | UnaryPrim _
    | IfValue _
    | FloatNeg _
    | FloatAbs _
    | FloatToBits _
    | FloatSqrt _
    | TupleGet _
    | Int64ToFloat _
    | FloatToInt64 _ -> true
    | _ -> false

let private tryAbsorbedAtom (outer: Atom) (nestedLeft: Atom) (nestedRight: Atom) : Atom option =
    if outer = nestedLeft || outer = nestedRight then Some outer
    else None

let rec private aExprUsesTemp (tid: TempId) (expr: AExpr) : bool =
    match expr with
    | Return atom -> atomUsesTemp tid atom
    | Let (_, cexpr, body) ->
        cexprUsesTemp tid cexpr || aExprUsesTemp tid body
    | If (cond, thenBranch, elseBranch) ->
        atomUsesTemp tid cond
        || aExprUsesTemp tid thenBranch
        || aExprUsesTemp tid elseBranch

/// Replace uses of one branch-local binding with a shared binding. The bound
/// TempId is removed from the substitution when crossing a shadowing Let so
/// this remains correct for hand-built ANF as well as globally fresh output.
let rec private replaceTempUses (sourceTid: TempId) (replacement: Atom) (expr: AExpr) : AExpr =
    let substitution = Map.ofList [(sourceTid, replacement)]

    match expr with
    | Return atom -> Return (substAtom substitution atom)
    | Let (tid, cexpr, body) ->
        let cexpr' = substCExpr substitution cexpr
        let body' =
            if tid = sourceTid then body
            else replaceTempUses sourceTid replacement body
        Let (tid, cexpr', body')
    | If (cond, thenBranch, elseBranch) ->
        If (
            substAtom substitution cond,
            replaceTempUses sourceTid replacement thenBranch,
            replaceTempUses sourceTid replacement elseBranch
        )

let rec private aExprHasSideEffects (context: OptimizeContext) (expr: AExpr) : bool =
    match expr with
    | Return _ -> false
    | Let (_, cexpr, body) ->
        hasSideEffects context cexpr || aExprHasSideEffects context body
    | If (_, thenBranch, elseBranch) ->
        aExprHasSideEffects context thenBranch || aExprHasSideEffects context elseBranch

/// Hoist before the binding that computes a local condition so the shared
/// expression does not separate a comparison from its branch during lowering.
let private tryHoistSharedLeadingBranchBinding
    (context: OptimizeContext)
    (options: OptimizeOptions)
    (expr: AExpr)
    : AExpr option =
    let sharedIf
        (cond: Atom)
        (thenTid: TempId)
        (thenBody: AExpr)
        (elseTid: TempId)
        (elseBody: AExpr)
        : AExpr =
        let elseBody' = replaceTempUses elseTid (Var thenTid) elseBody
        If (cond, thenBody, elseBody')

    if not options.EnableCSE then
        None
    else
        match expr with
        | Let (
            condTid,
            condCExpr,
            If (
                Var ifCondTid,
                Let (thenTid, thenCExpr, thenBody),
                Let (elseTid, elseCExpr, elseBody)
            )
          )
            when ifCondTid = condTid
                 && thenCExpr = elseCExpr
                 && not (hasSideEffects context condCExpr)
                 && not (hasSideEffects context thenCExpr)
                 && not (aExprHasSideEffects context thenBody)
                 && not (aExprHasSideEffects context elseBody)
                 && not (cexprUsesTemp condTid thenCExpr) ->
            let conditional = sharedIf (Var condTid) thenTid thenBody elseTid elseBody
            Some (Let (thenTid, thenCExpr, Let (condTid, condCExpr, conditional)))
        | _ -> None

let private tryComplementIntegerComparison (op: BinOp) : BinOp option =
    match op with
    | Eq -> Some Neq
    | Neq -> Some Eq
    | Lt -> Some Gte
    | Gt -> Some Lte
    | Lte -> Some Gt
    | Gte -> Some Lt
    | _ -> None

let private trySimplifyAdjacentLet (typeEnv: TypeEnv) (tid: TempId) (cexpr: CExpr) (body: AExpr) : AExpr option =
    match cexpr, body with
    | UnaryPrim (Not, source), If (Var conditionTid, thenBranch, elseBranch)
        when conditionTid = tid
             && not (aExprUsesTemp tid thenBranch)
             && not (aExprUsesTemp tid elseBranch) ->
        Some (If (source, elseBranch, thenBranch))
    | Prim (op, left, right), Let (notTid, UnaryPrim (Not, Var sourceTid), notBody)
        when sourceTid = tid
             && isIntegerAtom typeEnv left
             && isIntegerAtom typeEnv right
             && not (aExprUsesTemp tid notBody) ->
        // Ordered integer comparisons have exact complements. Float relations
        // do not: both x < NaN and x >= NaN are false.
        tryComplementIntegerComparison op
        |> Option.map (fun complement -> Let (notTid, Prim (complement, left, right), notBody))
    | UnaryPrim (Neg, negated), Let (resultTid, Prim (Add, other, Var negatedTid), resultBody)
    | UnaryPrim (Neg, negated), Let (resultTid, Prim (Add, Var negatedTid, other), resultBody)
        when negatedTid = tid
             && not (atomUsesTemp tid other)
             && not (aExprUsesTemp tid resultBody)
             && isInt64Atom typeEnv negated
             && isInt64Atom typeEnv other ->
        Some (Let (resultTid, Prim (Sub, other, negated), resultBody))
    | Prim (Add, source, IntLiteral (Int64 a)),
      Let (addTid, Prim (Add, Var sourceTid, IntLiteral (Int64 b)), addBody)
        when sourceTid = tid ->
        // Keep the inner binding for this rewrite; the recursive optimization
        // removes it only when the reassociated expression was its final use.
        let combined = IntLiteral (Int64 (a + b))
        Some (Let (tid, cexpr, Let (addTid, Prim (Add, source, combined), addBody)))
    | Prim (Mul, source, IntLiteral (Int64 a)),
      Let (multiplyTid, Prim (Mul, Var sourceTid, IntLiteral (Int64 b)), multiplyBody)
        when sourceTid = tid ->
        // Int64 multiplication is associative modulo 2^64. Keeping the inner
        // binding here lets the recursive liveness pass remove it only when
        // the reassociated expression was its final use.
        let combined = IntLiteral (Int64 (a * b))
        Some (Let (tid, cexpr, Let (multiplyTid, Prim (Mul, source, combined), multiplyBody)))
    | Prim (Mul, source, IntLiteral (Int64 coefficient)),
      Let (resultTid, Prim (Add, Var productTid, outerSource), resultBody)
    | Prim (Mul, source, IntLiteral (Int64 coefficient)),
      Let (resultTid, Prim (Add, outerSource, Var productTid), resultBody)
    | Prim (Mul, IntLiteral (Int64 coefficient), source),
      Let (resultTid, Prim (Add, Var productTid, outerSource), resultBody)
    | Prim (Mul, IntLiteral (Int64 coefficient), source),
      Let (resultTid, Prim (Add, outerSource, Var productTid), resultBody)
        when productTid = tid
             && source = outerSource
             && isInt64Atom typeEnv source ->
        // Int64 arithmetic wraps modulo 2^64, so c*x + x = (c+1)*x.
        // Retain the product until recursive liveness cleanup proves it dead.
        let combined = IntLiteral (Int64 (coefficient + 1L))
        Some (Let (tid, cexpr, Let (resultTid, Prim (Mul, source, combined), resultBody)))
    | Prim (Add, source, cancelled),
      Let (resultTid, Prim (Sub, Var intermediateTid, outerCancelled), resultBody)
        when intermediateTid = tid
             && cancelled = outerCancelled
             && isInt64Atom typeEnv source
             && isInt64Atom typeEnv cancelled ->
        Some (Let (tid, cexpr, Let (resultTid, Atom source, resultBody)))
    | Prim (Add, source, remaining),
      Let (resultTid, Prim (Sub, Var intermediateTid, outerCancelled), resultBody)
        when intermediateTid = tid
             && source = outerCancelled
             && isInt64Atom typeEnv source
             && isInt64Atom typeEnv remaining ->
        Some (Let (tid, cexpr, Let (resultTid, Atom remaining, resultBody)))
    | Prim (Sub, source, cancelled),
      Let (resultTid, Prim (Add, Var intermediateTid, outerCancelled), resultBody)
        when intermediateTid = tid
             && cancelled = outerCancelled
             && isInt64Atom typeEnv source
             && isInt64Atom typeEnv cancelled ->
        Some (Let (tid, cexpr, Let (resultTid, Atom source, resultBody)))
    | Prim (Sub, source, cancelled),
      Let (resultTid, Prim (Add, outerCancelled, Var intermediateTid), resultBody)
        when intermediateTid = tid
             && cancelled = outerCancelled
             && isInt64Atom typeEnv source
             && isInt64Atom typeEnv cancelled ->
        Some (Let (tid, cexpr, Let (resultTid, Atom source, resultBody)))
    | UnaryPrim (Not, source), Let (notTid, UnaryPrim (Not, Var sourceTid), notBody)
        when sourceTid = tid ->
        Some (Let (notTid, Atom source, notBody))
    | UnaryPrim (BitNot, source), Let (notTid, UnaryPrim (BitNot, Var sourceTid), notBody)
        when sourceTid = tid ->
        Some (Let (notTid, Atom source, notBody))
    | UnaryPrim (Neg, source), Let (negTid, UnaryPrim (Neg, Var sourceTid), negBody)
        when sourceTid = tid ->
        Some (Let (negTid, Atom source, negBody))
    | FloatNeg source, Let (negTid, FloatNeg (Var sourceTid), negBody)
        when sourceTid = tid ->
        Some (Let (negTid, Atom source, negBody))
    | FloatAbs source, Let (absTid, FloatAbs (Var sourceTid), absBody)
        when sourceTid = tid ->
        Some (Let (absTid, FloatAbs source, absBody))
    | FloatNeg source, Let (absTid, FloatAbs (Var sourceTid), absBody)
        when sourceTid = tid ->
        Some (Let (absTid, FloatAbs source, absBody))
    | Prim (Or, nestedLeft, nestedRight), Let (andTid, Prim (And, outer, Var nestedTid), andBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> Option.map (fun absorbed -> Let (andTid, Atom absorbed, andBody))
    | Prim (Or, nestedLeft, nestedRight), Let (andTid, Prim (And, Var nestedTid, outer), andBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> Option.map (fun absorbed -> Let (andTid, Atom absorbed, andBody))
    | Prim (And, nestedLeft, nestedRight), Let (orTid, Prim (Or, outer, Var nestedTid), orBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> Option.map (fun absorbed -> Let (orTid, Atom absorbed, orBody))
    | Prim (And, nestedLeft, nestedRight), Let (orTid, Prim (Or, Var nestedTid, outer), orBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> Option.map (fun absorbed -> Let (orTid, Atom absorbed, orBody))
    | Prim (BitOr, nestedLeft, nestedRight), Let (andTid, Prim (BitAnd, outer, Var nestedTid), andBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> Option.map (fun absorbed -> Let (andTid, Atom absorbed, andBody))
    | Prim (BitOr, nestedLeft, nestedRight), Let (andTid, Prim (BitAnd, Var nestedTid, outer), andBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> Option.map (fun absorbed -> Let (andTid, Atom absorbed, andBody))
    | Prim (BitAnd, nestedLeft, nestedRight), Let (orTid, Prim (BitOr, outer, Var nestedTid), orBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> Option.map (fun absorbed -> Let (orTid, Atom absorbed, orBody))
    | Prim (BitAnd, nestedLeft, nestedRight), Let (orTid, Prim (BitOr, Var nestedTid, outer), orBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> Option.map (fun absorbed -> Let (orTid, Atom absorbed, orBody))
    | _ -> None

let private trySimplifyBoolComplement (tid: TempId) (cexpr: CExpr) (body: AExpr) : AExpr option =
    let replacementForBoolOp op =
        match op with
        | And -> Some (BoolLiteral false)
        | Or -> Some (BoolLiteral true)
        | _ -> None

    match cexpr, body with
    | UnaryPrim (Not, source), Let (boolTid, Prim (op, Var sourceTid, Var notTid), boolBody)
    | UnaryPrim (Not, source), Let (boolTid, Prim (op, Var notTid, Var sourceTid), boolBody)
        when notTid = tid ->
        match source with
        | Var originalTid when originalTid = sourceTid ->
            replacementForBoolOp op
            |> Option.map (fun replacement -> Let (boolTid, Atom replacement, boolBody))
        | _ -> None
    | _ -> None

let private trySimplifyInt64BitwiseComplement
    (typeEnv: TypeEnv)
    (tid: TempId)
    (cexpr: CExpr)
    (body: AExpr)
    : AExpr option =
    let replacementForBitwiseOp op =
        match op with
        | BitAnd -> Some (IntLiteral (Int64 0L))
        | BitOr
        | BitXor -> Some (IntLiteral (Int64 -1L))
        | _ -> None

    match cexpr, body with
    | UnaryPrim (BitNot, source), Let (bitwiseTid, Prim (op, Var sourceTid, Var notTid), bitwiseBody)
    | UnaryPrim (BitNot, source), Let (bitwiseTid, Prim (op, Var notTid, Var sourceTid), bitwiseBody)
        when notTid = tid && isInt64Atom typeEnv source ->
        match source with
        | Var originalTid when originalTid = sourceTid ->
            replacementForBitwiseOp op
            |> Option.map (fun replacement ->
                let foldedBody = Let (bitwiseTid, Atom replacement, bitwiseBody)
                if aExprUsesTemp tid bitwiseBody then
                    Let (tid, cexpr, foldedBody)
                else
                    foldedBody)
        | _ -> None
    | _ -> None

/// Optimize an AExpr, returning optimized expression, change flag, and used TempIds
let rec private optimizeAExprWithUses
    (context: OptimizeContext)
    (options: OptimizeOptions)
    (env: ConstEnv)
    (typeEnv: TypeEnv)
    (tupleEnv: TupleEnv)
    (cseEnv: CSEnv)
    (aexpr: AExpr)
    : OptimizeAExprResult =
    match tryHoistSharedLeadingBranchBinding context options aexpr with
    | Some replacement ->
        let replacementResult =
            optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv replacement
        { replacementResult with Changed = true }
    | None ->
        optimizeAExprWithoutBranchHoisting context options env typeEnv tupleEnv cseEnv aexpr

and private optimizeAExprWithoutBranchHoisting
    (context: OptimizeContext)
    (options: OptimizeOptions)
    (env: ConstEnv)
    (typeEnv: TypeEnv)
    (tupleEnv: TupleEnv)
    (cseEnv: CSEnv)
    (aexpr: AExpr)
    : OptimizeAExprResult =
    match aexpr with
    | Return atom ->
        let atom' = substAtom env atom
        {
            Expr = Return atom'
            Changed = atom' <> atom
            Uses = addAtomUse atom' Set.empty
        }

    | Let (tid, cexpr, body) ->
        // Optimize the CExpr
        let (cexpr', cexprChanged) = optimizeCExpr options env typeEnv tupleEnv cexpr
        let (cexpr'', cseChanged, cseEnv') =
            if options.EnableCSE && isCSEEligible cexpr' then
                let key = cseKey cexpr'
                match Map.tryFind key cseEnv with
                | Some existingTid -> (Atom (Var existingTid), true, cseEnv)
                | None -> (cexpr', false, Map.add key tid cseEnv)
            else
                (cexpr', false, cseEnv)

        // Check for copy propagation: if cexpr is just an Atom, substitute it
        let (env', skipBinding) =
            match cexpr'' with
            | Atom a when options.EnableCopyProp && not (hasSideEffects context cexpr'') ->
                // Copy propagation: don't emit binding, just substitute
                (Map.add tid a env, true)
            | Atom (IntLiteral _ | BoolLiteral _ | FloatLiteral _ | StringLiteral _ | UnitLiteral as constAtom)
                when options.EnableConstProp ->
                // Constant propagation
                (Map.add tid constAtom env, false)
            | _ ->
                (env, false)

        // Optimize the body
        let tupleEnv' =
            match cexpr'' with
            | TupleAlloc elements ->
                let forwardableElements =
                    elements
                    |> List.indexed
                    |> List.choose (fun (index, element) ->
                        if canForwardTupleElement context typeEnv element then
                            Some (index, element)
                        else
                            None)
                    |> Map.ofList
                Map.add tid forwardableElements tupleEnv
            | _ -> tupleEnv

        let bodyResult = optimizeAExprWithUses context options env' typeEnv tupleEnv' cseEnv' body

        // Dead code elimination: if tid is not used in body and cexpr has no side effects
        let usesInBody = bodyResult.Uses
        let isDead = options.EnableDCE && not (Set.contains tid usesInBody) && not (hasSideEffects context cexpr'')
        let usesInBodyWithoutTid = Set.remove tid usesInBody

        let adjacentSimplification =
            if options.EnableConstFolding then
                trySimplifyAdjacentLet typeEnv tid cexpr'' bodyResult.Expr
                |> Option.orElseWith (fun () -> trySimplifyBoolComplement tid cexpr'' bodyResult.Expr)
                |> Option.orElseWith (fun () ->
                    trySimplifyInt64BitwiseComplement typeEnv tid cexpr'' bodyResult.Expr)
            else
                None

        match adjacentSimplification with
        | Some replacement ->
            let replacementResult = optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv replacement
            { replacementResult with Changed = true }
        | None when skipBinding ->
            // Copy propagation: skip this binding entirely
            {
                Expr = bodyResult.Expr
                Changed = true
                Uses = usesInBodyWithoutTid
            }
        | _ when isDead ->
            // Dead code elimination
            {
                Expr = bodyResult.Expr
                Changed = true
                Uses = usesInBodyWithoutTid
            }
        | _ ->
            let uses = addCExprUses cexpr'' usesInBodyWithoutTid
            {
                Expr = Let (tid, cexpr'', bodyResult.Expr)
                Changed = cexprChanged || cseChanged || bodyResult.Changed
                Uses = uses
            }

    | If (cond, thenBranch, elseBranch) ->
        let cond' = substAtom env cond

        // Fold constant conditions
        match cond' with
        | BoolLiteral true when options.EnableConstFolding ->
            let thenResult = optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv thenBranch
            {
                Expr = thenResult.Expr
                Changed = true
                Uses = thenResult.Uses
            }
        | BoolLiteral false when options.EnableConstFolding ->
            let elseResult = optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv elseBranch
            {
                Expr = elseResult.Expr
                Changed = true
                Uses = elseResult.Uses
            }
        | _ ->
            let thenResult = optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv thenBranch
            let elseResult = optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv elseBranch
            if options.EnableConstFolding && thenResult.Expr = Return (BoolLiteral true) && elseResult.Expr = Return (BoolLiteral false) then
                {
                    Expr = Return cond'
                    Changed = true
                    Uses = addAtomUse cond' Set.empty
                }
            elif options.EnableConstFolding && thenResult.Expr = elseResult.Expr then
                {
                    Expr = thenResult.Expr
                    Changed = true
                    Uses = thenResult.Uses
                }
            else
                let uses = Set.union thenResult.Uses elseResult.Uses |> addAtomUse cond'
                {
                    Expr = If (cond', thenResult.Expr, elseResult.Expr)
                    Changed = cond' <> cond || thenResult.Changed || elseResult.Changed
                    Uses = uses
                }

/// Optimize an AExpr
let optimizeAExpr (context: OptimizeContext) (options: OptimizeOptions) (env: ConstEnv) (typeEnv: TypeEnv) (aexpr: AExpr) : AExpr * bool =
    let result = optimizeAExprWithUses context options env typeEnv Map.empty Map.empty aexpr
    (result.Expr, result.Changed)

/// Optimize a function using the stable type metadata for its parameters.
let optimizeFunction (context: OptimizeContext) (options: OptimizeOptions) (typeEnv: TypeEnv) (func: Function) : Function * bool =
    // Initialize env with function parameters (they're not constants)
    let env = Map.empty
    let (body', changed) = optimizeAExpr context options env typeEnv func.Body
    ({ func with Body = body' }, changed)

/// Optimize until fixed point
let optimizeToFixedPoint (context: OptimizeContext) (options: OptimizeOptions) (func: Function) (maxIterations: int) : Function =
    let typeEnv =
        func.TypedParams
        |> List.map (fun param -> (param.Id, param.Type))
        |> Map.ofList

    let rec optimize (func: Function) (remainingIterations: int) : Function =
        if remainingIterations <= 0 then func
        else
            let (func', changed) = optimizeFunction context options typeEnv func
            if changed then
                optimize func' (remainingIterations - 1)
            else
                func'

    optimize func maxIterations

let rec private collectAExprTempIds (expr: AExpr) (tempIds: Set<TempId>) : Set<TempId> =
    match expr with
    | Return atom -> addAtomUse atom tempIds
    | Let (tid, cexpr, body) ->
        tempIds
        |> Set.add tid
        |> addCExprUses cexpr
        |> collectAExprTempIds body
    | If (cond, thenBranch, elseBranch) ->
        tempIds
        |> addAtomUse cond
        |> collectAExprTempIds thenBranch
        |> collectAExprTempIds elseBranch

/// Count uses of a local closure while rejecting every use that is not a call
/// through that exact closure value. A positive result proves the allocation
/// neither escapes nor reaches storage or an unknown callee.
let rec private countKnownClosureCalls (closureId: TempId) (expr: AExpr) : int option =
    let combine left right =
        match left, right with
        | Some leftCount, Some rightCount -> Some (leftCount + rightCount)
        | _ -> None

    let classifyCExpr cexpr =
        match cexpr with
        | ClosureCall (Var calledId, args) when calledId = closureId && not (atomsUseTemp closureId args) ->
            Some 1
        | ClosureTailCall (Var calledId, args) when calledId = closureId && not (atomsUseTemp closureId args) ->
            Some 1
        | _ when not (cexprUsesTemp closureId cexpr) ->
            Some 0
        | _ ->
            None

    match expr with
    | Return atom ->
        if atomUsesTemp closureId atom then None else Some 0
    | Let (boundId, cexpr, body) ->
        match classifyCExpr cexpr with
        | None -> None
        | Some callCount when boundId = closureId -> Some callCount
        | Some callCount ->
            countKnownClosureCalls closureId body
            |> Option.map (fun bodyCount -> callCount + bodyCount)
    | If (condition, thenBranch, elseBranch) ->
        if atomUsesTemp closureId condition then
            None
        else
            combine
                (countKnownClosureCalls closureId thenBranch)
                (countKnownClosureCalls closureId elseBranch)

/// Replace proven calls through one capture-free local closure with calls to
/// its lifted target. The unused hidden closure argument remains explicit so
/// the lifted function's established ABI does not change.
let rec private rewriteKnownCaptureFreeCalls
    (closureId: TempId)
    (funcName: string)
    (expr: AExpr)
    : AExpr =
    let rewriteCExpr cexpr =
        match cexpr with
        | ClosureCall (Var calledId, args) when calledId = closureId ->
            Call (funcName, UnitLiteral :: args)
        | ClosureTailCall (Var calledId, args) when calledId = closureId ->
            TailCall (funcName, UnitLiteral :: args)
        | _ -> cexpr

    match expr with
    | Return _ -> expr
    | Let (boundId, cexpr, body) ->
        let body' =
            if boundId = closureId then body
            else rewriteKnownCaptureFreeCalls closureId funcName body
        Let (boundId, rewriteCExpr cexpr, body')
    | If (condition, thenBranch, elseBranch) ->
        If (
            condition,
            rewriteKnownCaptureFreeCalls closureId funcName thenBranch,
            rewriteKnownCaptureFreeCalls closureId funcName elseBranch)

/// Eliminate only capture-free closure allocations whose complete lexical use
/// set consists of one or more known calls.
let rec private devirtualizeCaptureFreeClosures (expr: AExpr) : AExpr =
    match expr with
    | Return _ -> expr
    | Let (closureId, ClosureAlloc (funcName, []), body) ->
        let body' = devirtualizeCaptureFreeClosures body
        match countKnownClosureCalls closureId body' with
        | Some callCount when callCount > 0 ->
            rewriteKnownCaptureFreeCalls closureId funcName body'
        | _ ->
            Let (closureId, ClosureAlloc (funcName, []), body')
    | Let (tempId, cexpr, body) ->
        Let (tempId, cexpr, devirtualizeCaptureFreeClosures body)
    | If (condition, thenBranch, elseBranch) ->
        If (
            condition,
            devirtualizeCaptureFreeClosures thenBranch,
            devirtualizeCaptureFreeClosures elseBranch)

let private freshVarGenForProgram (Program (functions, mainExpr)) : VarGen =
    let tempIds =
        functions
        |> List.fold
            (fun tempIds func ->
                func.TypedParams
                |> List.fold (fun ids param -> Set.add param.Id ids) tempIds
                |> collectAExprTempIds func.Body)
            Set.empty
        |> collectAExprTempIds mainExpr

    match
        tempIds
        |> Set.fold
            (fun greatest (TempId tempId) ->
                match greatest with
                | None -> Some tempId
                | Some greatestId -> Some (max greatestId tempId))
            None
    with
    | None -> initialVarGen
    | Some greatestId -> VarGen (greatestId + 1)

type private SiblingAddition = {
    FirstCallId: TempId
    FirstArgs: Atom list
    SecondCallId: TempId
    SecondArgs: Atom list
    ResultId: TempId
}

let private tryLinearBindings (expr: AExpr) : ((TempId * CExpr) list * Atom) option =
    let rec collect reversedBindings remaining =
        match remaining with
        | Let (tempId, cexpr, body) ->
            collect ((tempId, cexpr) :: reversedBindings) body
        | Return atom ->
            Some (List.rev reversedBindings, atom)
        | If _ ->
            None
    collect [] expr

/// Recognize a complete linear sibling-recursion arm. Requiring exactly two
/// self calls and a final addition keeps effect order and the rewrite boundary
/// explicit; the function-level gate rejects any recursion outside this shape.
let private trySiblingAddition (funcName: string) (expr: AExpr) : SiblingAddition option =
    match tryLinearBindings expr with
    | Some (bindings, Var returnedId) ->
        match List.rev bindings with
        | (resultId, Prim (Add, Var leftId, Var rightId)) :: _ when resultId = returnedId ->
            let selfCalls =
                bindings
                |> List.choose (fun (tempId, cexpr) ->
                    match cexpr with
                    | Call (target, args) when target = funcName -> Some (tempId, args)
                    | _ -> None)
            match selfCalls with
            | [(firstCallId, firstArgs); (secondCallId, secondArgs)]
                when (leftId = firstCallId && rightId = secondCallId)
                     || (leftId = secondCallId && rightId = firstCallId) ->
                Some {
                    FirstCallId = firstCallId
                    FirstArgs = firstArgs
                    SecondCallId = secondCallId
                    SecondArgs = secondArgs
                    ResultId = resultId
                }
            | _ -> None
        | _ -> None
    | _ -> None

let private selfCallCount (funcName: string) (expr: AExpr) : int =
    let rec count expr =
        match expr with
        | Return _ -> 0
        | Let (_, cexpr, body) ->
            let current =
                match cexpr with
                | Call (target, _) when target = funcName -> 1
                | _ -> 0
            current + count body
        | If (_, thenBranch, elseBranch) ->
            count thenBranch + count elseBranch
    count expr

let private siblingAdditionCount (funcName: string) (expr: AExpr) : int =
    let rec count expr =
        match trySiblingAddition funcName expr with
        | Some _ -> 1
        | None ->
            match expr with
            | Return _ -> 0
            | Let (_, _, body) -> count body
            | If (_, thenBranch, elseBranch) -> count thenBranch + count elseBranch
    count expr

let private int64Zero = IntLiteral (Int64 0L)

let private rebuildBindings (bindings: (TempId * CExpr) list) (body: AExpr) : AExpr =
    List.foldBack (fun (tempId, cexpr) acc -> Let (tempId, cexpr, acc)) bindings body

let private transformSiblingAddition
    (helperName: string)
    (accumulatorId: TempId)
    (varGen: VarGen)
    (sibling: SiblingAddition)
    (bindings: (TempId * CExpr) list)
    : AExpr * VarGen =
    let (nextAccumulatorId, varGen') = freshVar varGen
    let rec rewrite remaining =
        match remaining with
        | [] -> Return (Var sibling.SecondCallId)
        | (tempId, _) :: rest when tempId = sibling.ResultId ->
            rewrite rest
        | (tempId, Call (_, _)) :: rest when tempId = sibling.FirstCallId ->
            Let (
                tempId,
                Call (helperName, sibling.FirstArgs @ [int64Zero]),
                rewrite rest
            )
        | (tempId, Call (_, _)) :: rest when tempId = sibling.SecondCallId ->
            Let (
                nextAccumulatorId,
                Prim (Add, Var accumulatorId, Var sibling.FirstCallId),
                Let (
                    tempId,
                    Call (helperName, sibling.SecondArgs @ [Var nextAccumulatorId]),
                    rewrite rest
                )
            )
        | binding :: rest ->
            rebuildBindings [binding] (rewrite rest)
    (rewrite bindings, varGen')

let rec private transformAccumulatorBody
    (funcName: string)
    (helperName: string)
    (accumulatorId: TempId)
    (varGen: VarGen)
    (expr: AExpr)
    : AExpr * VarGen =
    match trySiblingAddition funcName expr, tryLinearBindings expr with
    | Some sibling, Some (bindings, _) ->
        transformSiblingAddition helperName accumulatorId varGen sibling bindings
    | _ ->
        match expr with
        | Return atom ->
            let (resultId, varGen') = freshVar varGen
            (Let (resultId, Prim (Add, Var accumulatorId, atom), Return (Var resultId)), varGen')
        | Let (tempId, cexpr, body) ->
            let (body', varGen') =
                transformAccumulatorBody funcName helperName accumulatorId varGen body
            (Let (tempId, cexpr, body'), varGen')
        | If (cond, thenBranch, elseBranch) ->
            let (thenBranch', varGenAfterThen) =
                transformAccumulatorBody funcName helperName accumulatorId varGen thenBranch
            let (elseBranch', varGenAfterElse) =
                transformAccumulatorBody funcName helperName accumulatorId varGenAfterThen elseBranch
            (If (cond, thenBranch', elseBranch'), varGenAfterElse)

let private freshHelperName (usedNames: Set<string>) (funcName: string) : string =
    let rec choose suffix =
        let candidate =
            if suffix = 0 then $"{funcName}$trmo"
            else $"{funcName}$trmo{suffix}"
        if Set.contains candidate usedNames then choose (suffix + 1) else candidate
    choose 0

let private transformTailRecursionModuloAddition (program: Program) : Program =
    let (Program (functions, mainExpr)) = program
    let initialNames = functions |> List.map (fun func -> func.Name) |> Set.ofList
    let initialVarGen = freshVarGenForProgram program
    let (functionsReversed, _, _) =
        functions
        |> List.fold
            (fun (rewritten, usedNames, varGen) func ->
                let pairs = siblingAdditionCount func.Name func.Body
                let recursiveCalls = selfCallCount func.Name func.Body
                let eligible =
                    func.ReturnType = AST.TInt64
                    && pairs > 0
                    && recursiveCalls = pairs * 2
                if not eligible then
                    (func :: rewritten, Set.add func.Name usedNames, varGen)
                else
                    let helperName = freshHelperName usedNames func.Name
                    let (accumulatorId, varGenAfterAccumulator) = freshVar varGen
                    let (helperBody, varGenAfterHelper) =
                        transformAccumulatorBody
                            func.Name
                            helperName
                            accumulatorId
                            varGenAfterAccumulator
                            func.Body
                    let (wrapperResultId, varGenAfterWrapper) = freshVar varGenAfterHelper
                    let helper = {
                        func with
                            Name = helperName
                            TypedParams =
                                func.TypedParams @ [{ Id = accumulatorId; Type = AST.TInt64 }]
                            Body = helperBody
                    }
                    let wrapper = {
                        func with
                            Body =
                                Let (
                                    wrapperResultId,
                                    Call (
                                        helperName,
                                        (func.TypedParams |> List.map (fun param -> Var param.Id))
                                        @ [int64Zero]
                                    ),
                                    Return (Var wrapperResultId)
                                )
                    }
                    (helper :: wrapper :: rewritten, Set.add helperName usedNames, varGenAfterWrapper))
            ([], initialNames, initialVarGen)
    Program (List.rev functionsReversed, mainExpr)

let rec private rewriteInvertedBoolLiteralBranches (varGen: VarGen) (expr: AExpr) : AExpr * VarGen =
    match expr with
    | Return _ -> (expr, varGen)
    | Let (tid, cexpr, body) ->
        let (body', varGen') = rewriteInvertedBoolLiteralBranches varGen body
        (Let (tid, cexpr, body'), varGen')
    | If (cond, thenBranch, elseBranch) ->
        let (thenBranch', varGenAfterThen) = rewriteInvertedBoolLiteralBranches varGen thenBranch
        let (elseBranch', varGenAfterElse) = rewriteInvertedBoolLiteralBranches varGenAfterThen elseBranch

        match thenBranch', elseBranch' with
        | Return (BoolLiteral false), Return (BoolLiteral true) ->
            let (resultId, varGen') = freshVar varGenAfterElse
            (Let (resultId, UnaryPrim (Not, cond), Return (Var resultId)), varGen')
        | _ ->
            (If (cond, thenBranch', elseBranch'), varGenAfterElse)

let private rewriteInvertedBoolLiteralBranchesInProgram (program: Program) : Program =
    let (Program (functions, mainExpr)) = program
    let initialFreshVarGen = freshVarGenForProgram program
    let (functionsReversed, varGenAfterFunctions) =
        functions
        |> List.fold
            (fun (rewritten, varGen) func ->
                let (body', varGen') = rewriteInvertedBoolLiteralBranches varGen func.Body
                ({ func with Body = body' } :: rewritten, varGen'))
            ([], initialFreshVarGen)
    let (mainExpr', _) = rewriteInvertedBoolLiteralBranches varGenAfterFunctions mainExpr
    Program (List.rev functionsReversed, mainExpr')

/// Optimize a program with explicit options
let optimizeProgramWithOptions (context: OptimizeContext) (options: OptimizeOptions) (program: Program) : Program =
    let program' =
        if options.EnableConstFolding then
            rewriteInvertedBoolLiteralBranchesInProgram program
        else
            program
    let (Program (functions, mainExpr)) = program'

    // Copy propagation first removes administrative aliases introduced by the
    // public `fun` syntax, exposing the exact local closure use set. Running
    // devirtualization afterwards does not need another fixed-point iteration:
    // it only removes a zero-capture allocation and changes known call forms.
    let functions' =
        functions
        |> List.map (fun func ->
            let optimized = optimizeToFixedPoint context options func 10
            { optimized with Body = devirtualizeCaptureFreeClosures optimized.Body })

    // Optimize main expression
    let mainFunc = { Name = "__main__"
                     TypedParams = []
                     ReturnType = AST.TUnit
                     ReturnOwnership = OwnedReturn
                     Body = mainExpr }
    let mainOptimized = optimizeToFixedPoint context options mainFunc 10

    let optimizedProgram =
        Program (functions', devirtualizeCaptureFreeClosures mainOptimized.Body)
    if options.EnableTailRecursionModuloOperation then
        transformTailRecursionModuloAddition optimizedProgram
    else
        optimizedProgram

/// Optimize a program with default options
let optimizeProgram (context: OptimizeContext) (program: Program) : Program =
    optimizeProgramWithOptions context defaultOptimizeOptions program

let optimizeConstFolding (context: OptimizeContext) (program: Program) : Program =
    optimizeProgramWithOptions
        context
        { defaultOptimizeOptions with
            EnableConstFolding = true
            EnableConstProp = false
            EnableCopyProp = false
            EnableDCE = false
            EnableCSE = false
            EnableStrengthReduction = false
            EnableTailRecursionModuloOperation = false }
        program

let optimizeCopyProp (context: OptimizeContext) (program: Program) : Program =
    optimizeProgramWithOptions
        context
        { defaultOptimizeOptions with
            EnableConstFolding = false
            EnableConstProp = false
            EnableCopyProp = true
            EnableDCE = false
            EnableCSE = false
            EnableStrengthReduction = false
            EnableTailRecursionModuloOperation = false }
        program

let optimizeDCE (context: OptimizeContext) (program: Program) : Program =
    optimizeProgramWithOptions
        context
        { defaultOptimizeOptions with
            EnableConstFolding = false
            EnableConstProp = false
            EnableCopyProp = false
            EnableDCE = true
            EnableCSE = false
            EnableStrengthReduction = false
            EnableTailRecursionModuloOperation = false }
        program
