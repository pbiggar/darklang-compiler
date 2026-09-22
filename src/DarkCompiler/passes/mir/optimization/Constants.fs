// Constants.fs - Fold typed scalar operations with native-width arithmetic semantics.

module MIRConstants

open MIR

/// Truncate a 64-bit value to the appropriate integer type width
/// This ensures proper overflow/wraparound behavior for smaller integer types
let truncateToType (value: int64) (opType: AST.SemanticType) : int64 =
    match opType with
    | AST.TInt8 -> int64 (int8 value)      // Truncate to signed 8-bit
    | AST.TInt16 -> int64 (int16 value)    // Truncate to signed 16-bit
    | AST.TInt32 -> int64 (int32 value)    // Truncate to signed 32-bit
    | AST.TUInt8 -> int64 (uint8 value)    // Truncate to unsigned 8-bit
    | AST.TUInt16 -> int64 (uint16 value)  // Truncate to unsigned 16-bit
    | AST.TUInt32 -> int64 (uint32 value)  // Truncate to unsigned 32-bit
    | _ -> value                            // Int64/UInt64 and other types: no truncation

let truncateOperandToType (operand: Operand) (opType: AST.SemanticType) : Operand =
    match operand with
    | Int64Const value -> Int64Const (truncateToType value opType)
    | _ -> operand

/// Euclidean modulo: result has the sign of the divisor
let euclideanMod (a: int64) (b: int64) : int64 =
    let remainder = a % b
    if remainder = 0L then 0L
    elif (remainder > 0L && b < 0L) || (remainder < 0L && b > 0L) then remainder + b
    else remainder

let isReflexiveEqualityType (opType: AST.SemanticType) : bool =
    match opType with
    | AST.TInt8
    | AST.TInt16
    | AST.TInt32
    | AST.TInt64
    | AST.TInt128
    | AST.TUInt8
    | AST.TUInt16
    | AST.TUInt32
    | AST.TUInt64
    | AST.TUInt128
    | AST.TBool
    | AST.TChar
    | AST.TDateTime
    | AST.TUnit -> true
    | _ -> false

let isTotallyOrderedIntegerType (opType: AST.SemanticType) : bool =
    match opType with
    | AST.TInt8
    | AST.TInt16
    | AST.TInt32
    | AST.TInt64
    | AST.TInt128
    | AST.TUInt8
    | AST.TUInt16
    | AST.TUInt32
    | AST.TUInt64
    | AST.TUInt128 -> true
    | _ -> false

let isUnsignedIntegerType (opType: AST.SemanticType) : bool =
    match opType with
    | AST.TUInt8
    | AST.TUInt16
    | AST.TUInt32
    | AST.TUInt64 -> true
    | _ -> false

/// Constant Folding for MIR
/// Evaluate operations on constants at compile time
let tryFoldBinOp (op: BinOp) (left: Operand) (right: Operand) (opType: AST.SemanticType) : Operand option =
    match op, left, right with
    // Integer arithmetic - apply truncation for proper overflow behavior
    | Add, Int64Const a, Int64Const b -> Some (Int64Const (truncateToType (a + b) opType))
    | Sub, Int64Const a, Int64Const b -> Some (Int64Const (truncateToType (a - b) opType))
    | Mul, Int64Const a, Int64Const b -> Some (Int64Const (truncateToType (a * b) opType))
    // Division: avoid divide by zero and INT64_MIN / -1 overflow
    | Div, Int64Const a, Int64Const b when isUnsignedIntegerType opType && b <> 0L ->
        Some (Int64Const (int64 (uint64 a / uint64 b)))
    | Div, Int64Const a, Int64Const b when b <> 0L && not (a = System.Int64.MinValue && b = -1L) ->
        Some (Int64Const (truncateToType (a / b) opType))
    | Mod, Int64Const a, Int64Const b when isUnsignedIntegerType opType && b <> 0L ->
        Some (Int64Const (int64 (uint64 a % uint64 b)))
    | Mod, Int64Const a, Int64Const b when b > 0L -> Some (Int64Const (truncateToType (euclideanMod a b) opType))

    // Comparisons
    | Eq, Int64Const a, Int64Const b -> Some (BoolConst (a = b))
    | Neq, Int64Const a, Int64Const b -> Some (BoolConst (a <> b))
    | Lt, Int64Const a, Int64Const b when isUnsignedIntegerType opType -> Some (BoolConst (uint64 a < uint64 b))
    | Gt, Int64Const a, Int64Const b when isUnsignedIntegerType opType -> Some (BoolConst (uint64 a > uint64 b))
    | Lte, Int64Const a, Int64Const b when isUnsignedIntegerType opType -> Some (BoolConst (uint64 a <= uint64 b))
    | Gte, Int64Const a, Int64Const b when isUnsignedIntegerType opType -> Some (BoolConst (uint64 a >= uint64 b))
    | Lt, Int64Const a, Int64Const b -> Some (BoolConst (a < b))
    | Gt, Int64Const a, Int64Const b -> Some (BoolConst (a > b))
    | Lte, Int64Const a, Int64Const b -> Some (BoolConst (a <= b))
    | Gte, Int64Const a, Int64Const b -> Some (BoolConst (a >= b))
    | Eq, x, y when x = y && isReflexiveEqualityType opType -> Some (BoolConst true)
    | Neq, x, y when x = y && isReflexiveEqualityType opType -> Some (BoolConst false)
    | Lt, x, y when x = y && isTotallyOrderedIntegerType opType -> Some (BoolConst false)
    | Gt, x, y when x = y && isTotallyOrderedIntegerType opType -> Some (BoolConst false)
    | Lte, x, y when x = y && isTotallyOrderedIntegerType opType -> Some (BoolConst true)
    | Gte, x, y when x = y && isTotallyOrderedIntegerType opType -> Some (BoolConst true)

    // Boolean operations
    | And, BoolConst a, BoolConst b -> Some (BoolConst (a && b))
    | Or, BoolConst a, BoolConst b -> Some (BoolConst (a || b))

    // Algebraic identities
    | Add, Int64Const 0L, x -> Some x
    | Add, x, Int64Const 0L -> Some x
    | Sub, x, Int64Const 0L -> Some x
    | Sub, x, y when x = y -> Some (Int64Const 0L)  // x - x = 0
    | Mul, Int64Const 1L, x -> Some x
    | Mul, x, Int64Const 1L -> Some x
    | Mul, Int64Const 0L, _ -> Some (Int64Const 0L)
    | Mul, _, Int64Const 0L -> Some (Int64Const 0L)
    | Mul, Int64Const -1L, x -> None  // Could transform to Neg, but need instruction change
    | Mul, x, Int64Const -1L -> None  // Could transform to Neg
    | Div, x, Int64Const 1L -> Some x
    // Do not fold x / x: when x is zero, preserving the runtime operation matters.
    | Mod, _, Int64Const 1L -> Some (Int64Const 0L)  // x % 1 = 0

    // Bitwise identities
    | BitAnd, Int64Const 0L, _ -> Some (Int64Const 0L)
    | BitAnd, _, Int64Const 0L -> Some (Int64Const 0L)
    | BitAnd, Int64Const -1L, x -> Some (truncateOperandToType x opType)  // -1 = all bits set
    | BitAnd, x, Int64Const -1L -> Some (truncateOperandToType x opType)
    | BitAnd, x, y when x = y -> Some x  // x & x = x
    | BitOr, Int64Const 0L, x -> Some (truncateOperandToType x opType)
    | BitOr, x, Int64Const 0L -> Some (truncateOperandToType x opType)
    | BitOr, Int64Const -1L, _ -> Some (Int64Const (truncateToType -1L opType))
    | BitOr, _, Int64Const -1L -> Some (Int64Const (truncateToType -1L opType))
    | BitOr, x, y when x = y -> Some x  // x | x = x
    | BitXor, Int64Const 0L, x -> Some (truncateOperandToType x opType)
    | BitXor, x, Int64Const 0L -> Some (truncateOperandToType x opType)
    | BitXor, x, y when x = y -> Some (Int64Const 0L)  // x ^ x = 0

    // Shift identities
    | Shl, x, Int64Const 0L -> Some x  // x << 0 = x
    | Shr, x, Int64Const 0L -> Some x  // x >> 0 = x
    | Shl, Int64Const 0L, _ -> Some (Int64Const 0L)  // 0 << n = 0
    | Shr, Int64Const 0L, _ -> Some (Int64Const 0L)  // 0 >> n = 0

    // Boolean short-circuit
    | And, BoolConst false, _ -> Some (BoolConst false)
    | And, _, BoolConst false -> Some (BoolConst false)
    | And, BoolConst true, x -> Some x
    | And, x, BoolConst true -> Some x
    | Or, BoolConst true, _ -> Some (BoolConst true)
    | Or, _, BoolConst true -> Some (BoolConst true)
    | Or, BoolConst false, x -> Some x
    | Or, x, BoolConst false -> Some x

    | _ -> None
