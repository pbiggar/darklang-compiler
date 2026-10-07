(* ANFConstants.ml - Fold typed ANF constants and strength-reduce scalar operations. *)
[@@@warning "-4"]
open ANF
module TempMap = InliningCommon.TempMap
module IntMap = Map.Make (Int)
type constEnv = atom TempMap.t
(*
   Environment mapping TempIds to source types for type-sensitive rewrites.
*)
type typeEnv = AST.semanticType TempMap.t
(*
   Environment mapping locally allocated tuples to ownership-safe element atoms.
*)
type tupleEnv = atom IntMap.t TempMap.t
(*
   Optimization toggles for ANF optimization passes
*)
type optimizeOptions = {enableConstFolding : bool; enableConstProp : bool; enableCopyProp : bool; enableDCE : bool; enableCSE : bool; enableStrengthReduction : bool; enableTailRecursionModuloOperation : bool}
(*
   Type metadata needed for ownership-sensitive optimizer decisions.
*)
type optimizeContext = {typeReg : (string * AST.semanticType) list StringOrder.Map.t; recordTypeParams : string list StringOrder.Map.t; sumShapeReg : MemoryModel.rcSumShapeRegistry; functionNames : string FunctionIdMap.t; functionIds : AST.functionId StringOrder.Map.t}
let defaultOptimizeOptions = {enableConstFolding=true; enableConstProp=true; enableCopyProp=true; enableDCE=true; enableCSE=true; enableStrengthReduction=true; enableTailRecursionModuloOperation=true}
(*
   Check if n is a power of 2, and if so return its log2
   Returns None if n is not a power of 2 or is <= 0
*)
let tryLog2 n =
 if n <= 0L || Int64.logand n (Int64.sub n 1L) <> 0L then None else
 let rec countBits acc x = if x = 1L then acc else countBits (Int64.add acc 1L) (Int64.shift_right x 1) in
 Some (countBits 0L n)
(*
   Check if an unsigned n is a power of 2, and if so return its log2
*)
let tryLog2UInt64 n =
 if n = 0L || Int64.logand n (Int64.sub n 1L) <> 0L then None else
 let rec countBits acc x = if x = 1L then acc else countBits (Int64.add acc 1L) (Int64.shift_right_logical x 1) in
 Some (countBits 0L n)
(*
   Euclidean modulo: result has the sign of the divisor
*)
let euclideanMod a b =
 let remainder = Int64.rem a b in
 if remainder = 0L then 0L else if (remainder > 0L && b < 0L) || (remainder < 0L && b > 0L) then Int64.add remainder b else remainder
let tryTruncateFloatToInt64 f =
 if Float.is_finite f && f >= -9223372036854775808. && f < 9223372036854775808. then Some (Int64.of_float (Float.trunc f)) else None
(*
   Fold a binary operation on constants
*)
let foldBinOp op left right = match op, left, right with
 (* Int64 arithmetic (unchecked - overflow wraps) *)
 | Add, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (IntLiteral (Int64 (Int64.add a b))))
 | Sub, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (IntLiteral (Int64 (Int64.sub a b))))
 | Mul, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (IntLiteral (Int64 (Int64.mul a b))))
 | Div, IntLiteral (Int64 a), IntLiteral (Int64 b) when b <> 0L && not (a = Int64.min_int && b = -1L) -> Some (Atom (IntLiteral (Int64 (Int64.div a b))))
 (* Keep INT64_MIN / -1 at runtime, where fixed-width arithmetic wraps to INT64_MIN *)
 | Div, IntLiteral (Int64 _), IntLiteral (Int64 _) -> None
 | Mod, IntLiteral (Int64 a), IntLiteral (Int64 b) when b > 0L -> Some (Atom (IntLiteral (Int64 (euclideanMod a b))))
 | Shl, IntLiteral (Int64 a), IntLiteral (Int64 b) when b >= 0L && b < 64L -> Some (Atom (IntLiteral (Int64 (Int64.shift_left a (Int64.to_int b)))))
 | Shr, IntLiteral (Int64 a), IntLiteral (Int64 b) when b >= 0L && b < 64L -> Some (Atom (IntLiteral (Int64 (Int64.shift_right_logical a (Int64.to_int b)))))
 | BitAnd, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (IntLiteral (Int64 (Int64.logand a b))))
 | BitOr, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (IntLiteral (Int64 (Int64.logor a b))))
 | BitXor, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (IntLiteral (Int64 (Int64.logxor a b))))
 (* UInt64 arithmetic (unchecked - overflow wraps) *)
 | Add, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (IntLiteral (UInt64 (Int64.add a b))))
 | Sub, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (IntLiteral (UInt64 (Int64.sub a b))))
 | Mul, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (IntLiteral (UInt64 (Int64.mul a b))))
 | Div, IntLiteral (UInt64 a), IntLiteral (UInt64 b) when b <> 0L -> Some (Atom (IntLiteral (UInt64 (Int64.unsigned_div a b))))
 | Mod, IntLiteral (UInt64 a), IntLiteral (UInt64 b) when b <> 0L -> Some (Atom (IntLiteral (UInt64 (Int64.unsigned_rem a b))))
 (* UInt64 bitwise operations *)
 | BitAnd, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (IntLiteral (UInt64 (Int64.logand a b))))
 | BitOr, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (IntLiteral (UInt64 (Int64.logor a b))))
 | BitXor, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (IntLiteral (UInt64 (Int64.logxor a b))))
 (* Float arithmetic *)
 | Add, FloatLiteral a, FloatLiteral b -> Some (Atom (FloatLiteral (a +. b)))
 | Sub, FloatLiteral a, FloatLiteral b -> Some (Atom (FloatLiteral (a -. b)))
 | Mul, FloatLiteral a, FloatLiteral b -> Some (Atom (FloatLiteral (a *. b)))
 | Div, FloatLiteral a, FloatLiteral b -> Some (Atom (FloatLiteral (a /. b)))
 (* Float comparisons *)
 | Eq, FloatLiteral a, FloatLiteral b -> Some (Atom (BoolLiteral (a = b)))
 | Neq, FloatLiteral a, FloatLiteral b -> Some (Atom (BoolLiteral (a <> b)))
 | Lt, FloatLiteral a, FloatLiteral b -> Some (Atom (BoolLiteral (a < b)))
 | Gt, FloatLiteral a, FloatLiteral b -> Some (Atom (BoolLiteral (a > b)))
 | Lte, FloatLiteral a, FloatLiteral b -> Some (Atom (BoolLiteral (a <= b)))
 | Gte, FloatLiteral a, FloatLiteral b -> Some (Atom (BoolLiteral (a >= b)))
 (* Int64 comparisons *)
 | Eq, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a = b)))
 | Neq, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a <> b)))
 | Lt, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a < b)))
 | Gt, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a > b)))
 | Lte, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a <= b)))
 | Gte, IntLiteral (Int64 a), IntLiteral (Int64 b) -> Some (Atom (BoolLiteral (a >= b)))
 (* UInt64 comparisons *)
 | Eq, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (BoolLiteral (a = b)))
 | Neq, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (BoolLiteral (a <> b)))
 | Lt, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (BoolLiteral (Int64.unsigned_compare a b < 0)))
 | Gt, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (BoolLiteral (Int64.unsigned_compare a b > 0)))
 | Lte, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (BoolLiteral (Int64.unsigned_compare a b <= 0)))
 | Gte, IntLiteral (UInt64 a), IntLiteral (UInt64 b) -> Some (Atom (BoolLiteral (Int64.unsigned_compare a b >= 0)))
 (* Boolean comparisons *)
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
 (* Boolean operations *)
 | And, BoolLiteral a, BoolLiteral b -> Some (Atom (BoolLiteral (a && b)))
 | Or, BoolLiteral a, BoolLiteral b -> Some (Atom (BoolLiteral (a || b)))
 (* String comparisons *)
 | Eq, StringLiteral a, StringLiteral b -> Some (Atom (BoolLiteral (a = b)))
 | Neq, StringLiteral a, StringLiteral b -> Some (Atom (BoolLiteral (a <> b)))
 (* Algebraic identities (strength reduction) - Int64 *)
 | Add, IntLiteral (Int64 0L), x -> Some (Atom x)
 | Add, x, IntLiteral (Int64 0L) -> Some (Atom x)
 | Add, x, IntLiteral (Int64 n) when n < 0L && n <> Int64.min_int ->
 Some (Prim (Sub, x, IntLiteral (Int64 (Int64.neg n))))
 | Add, IntLiteral (Int64 n), x when n < 0L && n <> Int64.min_int ->
 Some (Prim (Sub, x, IntLiteral (Int64 (Int64.neg n))))
 | Sub, x, IntLiteral (Int64 0L) -> Some (Atom x)
 | Sub, x, IntLiteral (Int64 n) when n < 0L && n <> Int64.min_int ->
 Some (Prim (Add, x, IntLiteral (Int64 (Int64.neg n))))
 | Sub, IntLiteral (Int64 0L), x -> Some (UnaryPrim (Neg, x))
 | Mul, IntLiteral (Int64 1L), x -> Some (Atom x)
 | Mul, x, IntLiteral (Int64 1L) -> Some (Atom x)
 | Mul, IntLiteral (Int64 (-1L)), x -> Some (UnaryPrim (Neg, x))
 | Mul, x, IntLiteral (Int64 (-1L)) -> Some (UnaryPrim (Neg, x))
 | Mul, IntLiteral (Int64 0L), _ -> Some (Atom (IntLiteral (Int64 0L)))
 | Mul, _, IntLiteral (Int64 0L) -> Some (Atom (IntLiteral (Int64 0L)))
 | Div, x, IntLiteral (Int64 1L) -> Some (Atom x)
 | Div, x, IntLiteral (Int64 (-1L)) -> Some (UnaryPrim (Neg, x))
 | Mod, _, IntLiteral (Int64 1L) -> Some (Atom (IntLiteral (Int64 0L)))
 | Mod, _, IntLiteral (Int64 (-1L)) -> Some (Atom (IntLiteral (Int64 0L)))
 | Shl, x, IntLiteral (Int64 0L) -> Some (Atom x)
 | Shr, x, IntLiteral (Int64 0L) -> Some (Atom x)
 | Shl, IntLiteral (Int64 0L), _ -> Some (Atom (IntLiteral (Int64 0L)))
 | Shr, IntLiteral (Int64 0L), _ -> Some (Atom (IntLiteral (Int64 0L)))
 | BitAnd, _, IntLiteral (Int64 0L) -> Some (Atom (IntLiteral (Int64 0L)))
 | BitAnd, IntLiteral (Int64 0L), _ -> Some (Atom (IntLiteral (Int64 0L)))
 | BitAnd, x, IntLiteral (Int64 (-1L)) -> Some (Atom x)
 | BitAnd, IntLiteral (Int64 (-1L)), x -> Some (Atom x)
 | BitOr, x, IntLiteral (Int64 0L) -> Some (Atom x)
 | BitOr, IntLiteral (Int64 0L), x -> Some (Atom x)
 | BitOr, _, IntLiteral (Int64 (-1L)) -> Some (Atom (IntLiteral (Int64 (-1L))))
 | BitOr, IntLiteral (Int64 (-1L)), _ -> Some (Atom (IntLiteral (Int64 (-1L))))
 | BitXor, x, IntLiteral (Int64 0L) -> Some (Atom x)
 | BitXor, IntLiteral (Int64 0L), x -> Some (Atom x)
 | BitXor, x, IntLiteral (Int64 (-1L)) -> Some (UnaryPrim (BitNot, x))
 | BitXor, IntLiteral (Int64 (-1L)), x -> Some (UnaryPrim (BitNot, x))
 (* Algebraic identities - UInt64 *)
 | Add, IntLiteral (UInt64 0L), x -> Some (Atom x)
 | Add, x, IntLiteral (UInt64 0L) -> Some (Atom x)
 | Sub, x, IntLiteral (UInt64 0L) -> Some (Atom x)
 | Mul, IntLiteral (UInt64 1L), x -> Some (Atom x)
 | Mul, x, IntLiteral (UInt64 1L) -> Some (Atom x)
 | Mul, IntLiteral (UInt64 0L), _ -> Some (Atom (IntLiteral (UInt64 0L)))
 | Mul, _, IntLiteral (UInt64 0L) -> Some (Atom (IntLiteral (UInt64 0L)))
 | Div, x, IntLiteral (UInt64 1L) -> Some (Atom x)
 | Mod, _, IntLiteral (UInt64 1L) -> Some (Atom (IntLiteral (UInt64 0L)))
 | BitAnd, _, IntLiteral (UInt64 0L) -> Some (Atom (IntLiteral (UInt64 0L)))
 | BitAnd, IntLiteral (UInt64 0L), _ -> Some (Atom (IntLiteral (UInt64 0L)))
 | BitOr, x, IntLiteral (UInt64 0L) -> Some (Atom x)
 | BitOr, IntLiteral (UInt64 0L), x -> Some (Atom x)
 | BitXor, x, IntLiteral (UInt64 0L) -> Some (Atom x)
 | BitXor, IntLiteral (UInt64 0L), x -> Some (Atom x)
 (* Algebraic identities - Float *)
 (* Note: We skip 0.0 * x -> 0.0 because 0.0 * inf = NaN, 0.0 * NaN = NaN *)
 | Add, FloatLiteral floatConstant0, x when floatConstant0 = 0.0 -> Some (Atom x)
 | Add, x, FloatLiteral floatConstant0 when floatConstant0 = 0.0 -> Some (Atom x)
 | Sub, x, FloatLiteral floatConstant0 when floatConstant0 = 0.0 -> Some (Atom x)
 | Sub, FloatLiteral floatConstant0, x when floatConstant0 = 0.0 -> Some (FloatNeg x)
 | Mul, FloatLiteral floatConstant0, x when floatConstant0 = 1.0 -> Some (Atom x)
 | Mul, x, FloatLiteral floatConstant0 when floatConstant0 = 1.0 -> Some (Atom x)
 | Mul, FloatLiteral floatConstant0, x when floatConstant0 = -1.0 -> Some (FloatNeg x)
 | Mul, x, FloatLiteral floatConstant0 when floatConstant0 = -1.0 -> Some (FloatNeg x)
 | Div, x, FloatLiteral floatConstant0 when floatConstant0 = 1.0 -> Some (Atom x)
 | Div, x, FloatLiteral floatConstant0 when floatConstant0 = -1.0 -> Some (FloatNeg x)
 (* Self-identities on integer operations. Float is excluded where NaN changes identity laws. *)
 | Sub, Var a, Var b when a = b -> Some (Atom (IntLiteral (Int64 0L)))
 | BitAnd, Var a, Var b when a = b -> Some (Atom (Var a))
 | BitOr, Var a, Var b when a = b -> Some (Atom (Var a))
 | BitXor, Var a, Var b when a = b -> Some (Atom (IntLiteral (Int64 0L)))
 | And, Var a, Var b when a = b -> Some (Atom (Var a))
 | Or, Var a, Var b when a = b -> Some (Atom (Var a))
 (* Short-circuit boolean *)
 | And, BoolLiteral false, _ -> Some (Atom (BoolLiteral false))
 | And, _, BoolLiteral false -> Some (Atom (BoolLiteral false))
 | And, BoolLiteral true, x -> Some (Atom x)
 | And, x, BoolLiteral true -> Some (Atom x)
 | Or, BoolLiteral true, _ -> Some (Atom (BoolLiteral true))
 | Or, _, BoolLiteral true -> Some (Atom (BoolLiteral true))
 | Or, BoolLiteral false, x -> Some (Atom x)
 | Or, x, BoolLiteral false -> Some (Atom x)
 | _ -> None

let isInt64Atom typeEnv = function
 | IntLiteral (Int64 _) -> true | Var tid -> TempMap.find_opt tid typeEnv = Some AST.TInt64 | _ -> false
let isUInt64Atom typeEnv = function
 | IntLiteral (UInt64 _) -> true | Var tid -> TempMap.find_opt tid typeEnv = Some AST.TUInt64 | _ -> false
let isBoolAtom typeEnv = function
 | BoolLiteral _ -> true | Var tid -> TempMap.find_opt tid typeEnv = Some AST.TBool | _ -> false
let isFloatAtom typeEnv = function
 | FloatLiteral _ -> true | Var tid -> TempMap.find_opt tid typeEnv = Some AST.TFloat64 | _ -> false
let isUnsignedIntegerAtom typeEnv = function
 | IntLiteral (UInt8 _ | UInt16 _ | UInt32 _ | UInt64 _) -> true
 | Var tid -> (match TempMap.find_opt tid typeEnv with Some (AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64) -> true | _ -> false) | _ -> false
let isIntegerAtom typeEnv = function
 | IntLiteral _ -> true
 | Var tid -> (match TempMap.find_opt tid typeEnv with Some (AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt128 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TUInt128) -> true | _ -> false) | _ -> false
let tryStrengthReduce typeEnv op left right = match op, left, right with
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
 (* Strict self-comparisons are false for every IEEE 754 value, including NaN. *)
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
 (match tryLog2 n with
 | Some shift -> Some (Prim (Shl, x, IntLiteral (Int64 shift)))
 | None -> None)
 | Mul, IntLiteral (Int64 n), x ->
 (match tryLog2 n with
 | Some shift -> Some (Prim (Shl, x, IntLiteral (Int64 shift)))
 | None -> None)
 | Mul, x, IntLiteral (UInt64 n) when isUInt64Atom typeEnv x ->
 (match tryLog2UInt64 n with
 | Some shift -> Some (Prim (Shl, x, IntLiteral (Int64 shift)))
 | None -> None)
 | Mul, IntLiteral (UInt64 n), x when isUInt64Atom typeEnv x ->
 (match tryLog2UInt64 n with
 | Some shift -> Some (Prim (Shl, x, IntLiteral (Int64 shift)))
 | None -> None)
 | Mod, x, IntLiteral (Int64 n) when n > 0L ->
 (* For positive power-of-two divisors, Euclidean remainder equals x & (n - 1) *)
 (match tryLog2 n with
 | Some _ -> Some (Prim (BitAnd, x, IntLiteral (Int64 (Int64.sub n 1L))))
 | None -> None)
 | Div, x, IntLiteral (Int64 n) when n > 0L && isUnsignedIntegerAtom typeEnv x ->
 (match tryLog2 n with
 | Some shift -> Some (Prim (Shr, x, IntLiteral (Int64 shift)))
 | None -> None)
 | Div, x, IntLiteral (UInt64 n) when isUInt64Atom typeEnv x ->
 (match tryLog2UInt64 n with
 | Some shift -> Some (Prim (Shr, x, IntLiteral (Int64 shift)))
 | None -> None)
 | Mod, x, IntLiteral (UInt64 n) when isUInt64Atom typeEnv x ->
 (match tryLog2UInt64 n with
 | Some _ -> Some (Prim (BitAnd, x, IntLiteral (UInt64 (Int64.sub n 1L))))
 | None -> None)
 (* Float strength reduction: 2.0 * x -> x + x *)
 | Mul, FloatLiteral floatConstant0, x when floatConstant0 = 2.0 -> Some (Prim (Add, x, x))
 | Mul, x, FloatLiteral floatConstant0 when floatConstant0 = 2.0 -> Some (Prim (Add, x, x))
 (* Float division by power of 2 -> multiplication by reciprocal *)
 (* These reciprocals are exactly representable in IEEE 754 *)
 | Div, x, FloatLiteral floatConstant0 when floatConstant0 = 2.0 -> Some (Prim (Mul, x, FloatLiteral 0.5))
 | Div, x, FloatLiteral floatConstant0 when floatConstant0 = -2.0 -> Some (Prim (Mul, x, FloatLiteral (-0.5)))
 | Div, x, FloatLiteral floatConstant0 when floatConstant0 = 4.0 -> Some (Prim (Mul, x, FloatLiteral 0.25))
 | Div, x, FloatLiteral floatConstant0 when floatConstant0 = -4.0 -> Some (Prim (Mul, x, FloatLiteral (-0.25)))
 | Div, x, FloatLiteral floatConstant0 when floatConstant0 = 8.0 -> Some (Prim (Mul, x, FloatLiteral 0.125))
 | Div, x, FloatLiteral floatConstant0 when floatConstant0 = -8.0 -> Some (Prim (Mul, x, FloatLiteral (-0.125)))
 | Div, x, FloatLiteral floatConstant0 when floatConstant0 = 16.0 -> Some (Prim (Mul, x, FloatLiteral 0.0625))
 | Div, x, FloatLiteral floatConstant0 when floatConstant0 = -16.0 -> Some (Prim (Mul, x, FloatLiteral (-0.0625)))
 | Div, x, FloatLiteral floatConstant0 when floatConstant0 = 32.0 -> Some (Prim (Mul, x, FloatLiteral 0.03125))
 | Div, x, FloatLiteral floatConstant0 when floatConstant0 = -32.0 -> Some (Prim (Mul, x, FloatLiteral (-0.03125)))
 | Div, x, FloatLiteral floatConstant0 when floatConstant0 = 64.0 -> Some (Prim (Mul, x, FloatLiteral 0.015625))
 | Div, x, FloatLiteral floatConstant0 when floatConstant0 = -64.0 -> Some (Prim (Mul, x, FloatLiteral (-0.015625)))
 | Div, x, FloatLiteral floatConstant0 when floatConstant0 = 128.0 -> Some (Prim (Mul, x, FloatLiteral 0.0078125))
 | Div, x, FloatLiteral floatConstant0 when floatConstant0 = -128.0 -> Some (Prim (Mul, x, FloatLiteral (-0.0078125)))
 | Div, x, FloatLiteral floatConstant0 when floatConstant0 = 256.0 -> Some (Prim (Mul, x, FloatLiteral 0.00390625))
 | Div, x, FloatLiteral floatConstant0 when floatConstant0 = -256.0 -> Some (Prim (Mul, x, FloatLiteral (-0.00390625)))
 | _ -> None

(*
   Fold a unary operation on constants
   Int64 negation (unchecked - INT64_MIN wraps to itself)
   Bitwise NOT: flip all bits
*)
let foldUnaryOp op src = match op, src with
 | Neg, IntLiteral (Int64 n) -> Some (Atom (IntLiteral (Int64 (Int64.neg n))))
 | Neg, FloatLiteral f -> Some (Atom (FloatLiteral (-.f)))
 | Not, BoolLiteral b -> Some (Atom (BoolLiteral (not b)))
 | BitNot, IntLiteral (Int64 n) -> Some (Atom (IntLiteral (Int64 (Int64.lognot n))))
 | _ -> None
