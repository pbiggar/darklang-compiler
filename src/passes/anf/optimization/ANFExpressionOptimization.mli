(* ANFExpressionOptimization.mli - Value-numbering keys and fresh temporary allocation shared by SSA and ANF lowering. *)

type scalarUnaryCSEOp =
  | PrimitiveUnary of ANF.unaryOp
  | FloatSqrtOp
  | FloatAbsOp
  | FloatNegOp
  | Int64ToFloatOp
  | FloatToInt64Op
  | FloatToBitsOp

type cSEKey =
  | BinaryValue of ANF.binOp * ANF.atom * ANF.atom
  | UnaryValue of scalarUnaryCSEOp * ANF.atom
  | ConditionalValue of ANF.atom * ANF.atom * ANF.atom
  | TupleProjection of ANF.atom * int
  | RecordProjection of ANF.recordDescriptor * ANF.atom * int

module CSEnv : Map.S with type key = cSEKey

val tryCSEKey : ANF.cExpr -> cSEKey option
val freshVarGenForProgram : ANF.program -> ANF.varGen
