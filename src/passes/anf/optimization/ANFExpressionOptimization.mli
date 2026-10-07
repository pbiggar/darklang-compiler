(* ANFExpressionOptimization.mli - Propagate facts and common expressions through lexical ANF control flow. *)
type optimizeAExprResult = {
  expr : ANF.aExpr;
  changed : bool;
  uses : ANFEffects.TempSet.t;
}

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

val optimizeAExpr :
  ANFConstants.optimizeContext ->
  ANFConstants.optimizeOptions ->
  ANFConstants.constEnv ->
  ANFConstants.typeEnv ->
  ANF.aExpr ->
  ANF.aExpr * bool

val optimizeFunction :
  ANFConstants.optimizeContext ->
  ANFConstants.optimizeOptions ->
  ANFConstants.typeEnv ->
  ANF.functionDef ->
  ANF.functionDef * bool

val optimizeToFixedPoint :
  ANFConstants.optimizeContext ->
  ANFConstants.optimizeOptions ->
  ANF.functionDef ->
  int ->
  ANF.functionDef

val devirtualizeCaptureFreeClosures : ANF.aExpr -> ANF.aExpr
val freshVarGenForProgram : ANF.program -> ANF.varGen
