(* MIRConstants.ml - Fold typed scalar operations with native-width arithmetic semantics. *)
[@@@warning "-4"]
open MIR
(*
   Truncate a 64-bit value to the appropriate integer type width
   This ensures proper overflow/wraparound behavior for smaller integer types
   Truncate to signed 8-bit
   Truncate to signed 16-bit
   Truncate to signed 32-bit
   Truncate to unsigned 8-bit
   Truncate to unsigned 16-bit
   Truncate to unsigned 32-bit
   Int64/UInt64 and other types: no truncation
*)
let truncateToType value = function
 | AST.TInt8 -> Int64.shift_right (Int64.shift_left value 56) 56
 | AST.TInt16 -> Int64.shift_right (Int64.shift_left value 48) 48
 | AST.TInt32 -> Int64.of_int32 (Int64.to_int32 value)
 | AST.TUInt8 -> Int64.logand value 255L
 | AST.TUInt16 -> Int64.logand value 65535L
 | AST.TUInt32 -> Int64.logand value 4294967295L
 | _ -> value
let truncateOperandToType operand typ = match operand with Int64Const value -> Int64Const (truncateToType value typ) | _ -> operand
(*
   Euclidean modulo: result has the sign of the divisor
*)
let euclideanMod left right = let remainder = Int64.rem left right in if remainder = 0L then 0L else if (remainder > 0L && right < 0L) || (remainder < 0L && right > 0L) then Int64.add remainder right else remainder
let isReflexiveEqualityType = function AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt128 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TUInt128 | AST.TBool | AST.TChar | AST.TDateTime | AST.TUnit -> true | _ -> false
let isTotallyOrderedIntegerType = function AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt128 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TUInt128 -> true | _ -> false
let isUnsignedIntegerType = function AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 -> true | _ -> false
(*
   Constant Folding for MIR
   Evaluate operations on constants at compile time
   Integer arithmetic - apply truncation for proper overflow behavior
   Division: avoid divide by zero and INT64_MIN / -1 overflow
   Comparisons
   Boolean operations
   Algebraic identities
   x - x = 0
   Could transform to Neg, but need instruction change
   Could transform to Neg
   Do not fold x / x: when x is zero, preserving the runtime operation matters.
   x % 1 = 0
   Bitwise identities
   -1 = all bits set
   x & x = x
   x | x = x
   x ^ x = 0
   Shift identities
   x << 0 = x
   x >> 0 = x
   0 << n = 0
   0 >> n = 0
   Boolean short-circuit
*)
let tryFoldBinOp (op : binOp) left right typ =
 let integer value = Some (Int64Const (truncateToType value typ)) in
 match op, left, right with
 | Add, Int64Const a, Int64Const b -> integer (Int64.add a b)
 | Sub, Int64Const a, Int64Const b -> integer (Int64.sub a b)
 | Mul, Int64Const a, Int64Const b -> integer (Int64.mul a b)
 | Div, Int64Const a, Int64Const b when isUnsignedIntegerType typ && b <> 0L -> Some (Int64Const (Int64.unsigned_div a b))
 | Div, Int64Const a, Int64Const b when b <> 0L && not (a = Int64.min_int && b = -1L) -> integer (Int64.div a b)
 | Mod, Int64Const a, Int64Const b when isUnsignedIntegerType typ && b <> 0L -> Some (Int64Const (Int64.unsigned_rem a b))
 | Mod, Int64Const a, Int64Const b when b > 0L -> integer (euclideanMod a b)
 | Eq, Int64Const a, Int64Const b -> Some (BoolConst (a = b))
 | Neq, Int64Const a, Int64Const b -> Some (BoolConst (a <> b))
 | Lt, Int64Const a, Int64Const b when isUnsignedIntegerType typ -> Some (BoolConst (Int64.unsigned_compare a b < 0))
 | Gt, Int64Const a, Int64Const b when isUnsignedIntegerType typ -> Some (BoolConst (Int64.unsigned_compare a b > 0))
 | Lte, Int64Const a, Int64Const b when isUnsignedIntegerType typ -> Some (BoolConst (Int64.unsigned_compare a b <= 0))
 | Gte, Int64Const a, Int64Const b when isUnsignedIntegerType typ -> Some (BoolConst (Int64.unsigned_compare a b >= 0))
 | Lt, Int64Const a, Int64Const b -> Some (BoolConst (a < b))
 | Gt, Int64Const a, Int64Const b -> Some (BoolConst (a > b))
 | Lte, Int64Const a, Int64Const b -> Some (BoolConst (a <= b))
 | Gte, Int64Const a, Int64Const b -> Some (BoolConst (a >= b))
 | Eq, x, y when x = y && isReflexiveEqualityType typ -> Some (BoolConst true)
 | Neq, x, y when x = y && isReflexiveEqualityType typ -> Some (BoolConst false)
 | Lt, x, y when x = y && isTotallyOrderedIntegerType typ -> Some (BoolConst false)
 | Gt, x, y when x = y && isTotallyOrderedIntegerType typ -> Some (BoolConst false)
 | Lte, x, y when x = y && isTotallyOrderedIntegerType typ -> Some (BoolConst true)
 | Gte, x, y when x = y && isTotallyOrderedIntegerType typ -> Some (BoolConst true)
 | And, BoolConst a, BoolConst b -> Some (BoolConst (a && b))
 | Or, BoolConst a, BoolConst b -> Some (BoolConst (a || b))
 | Add, Int64Const 0L, x -> Some x | Add, x, Int64Const 0L -> Some x
 | Sub, x, Int64Const 0L -> Some x | Sub, x, y when x = y -> Some (Int64Const 0L)
 | Mul, Int64Const 1L, x -> Some x | Mul, x, Int64Const 1L -> Some x
 | Mul, Int64Const 0L, _ -> Some (Int64Const 0L) | Mul, _, Int64Const 0L -> Some (Int64Const 0L)
 | Mul, Int64Const (-1L), _ -> None | Mul, _, Int64Const (-1L) -> None
 | Div, x, Int64Const 1L -> Some x | Mod, _, Int64Const 1L -> Some (Int64Const 0L)
 | BitAnd, Int64Const 0L, _ -> Some (Int64Const 0L) | BitAnd, _, Int64Const 0L -> Some (Int64Const 0L)
 | BitAnd, Int64Const (-1L), x -> Some (truncateOperandToType x typ) | BitAnd, x, Int64Const (-1L) -> Some (truncateOperandToType x typ)
 | BitAnd, x, y when x = y -> Some x
 | BitOr, Int64Const 0L, x -> Some (truncateOperandToType x typ) | BitOr, x, Int64Const 0L -> Some (truncateOperandToType x typ)
 | BitOr, Int64Const (-1L), _ -> integer (-1L) | BitOr, _, Int64Const (-1L) -> integer (-1L)
 | BitOr, x, y when x = y -> Some x
 | BitXor, Int64Const 0L, x -> Some (truncateOperandToType x typ) | BitXor, x, Int64Const 0L -> Some (truncateOperandToType x typ)
 | BitXor, x, y when x = y -> Some (Int64Const 0L)
 | Shl, x, Int64Const 0L -> Some x | Shr, x, Int64Const 0L -> Some x
 | Shl, Int64Const 0L, _ -> Some (Int64Const 0L) | Shr, Int64Const 0L, _ -> Some (Int64Const 0L)
 | And, BoolConst false, _ -> Some (BoolConst false) | And, _, BoolConst false -> Some (BoolConst false)
 | And, BoolConst true, x -> Some x | And, x, BoolConst true -> Some x
 | Or, BoolConst true, _ -> Some (BoolConst true) | Or, _, BoolConst true -> Some (BoolConst true)
 | Or, BoolConst false, x -> Some x | Or, x, BoolConst false -> Some x
 | _ -> None
