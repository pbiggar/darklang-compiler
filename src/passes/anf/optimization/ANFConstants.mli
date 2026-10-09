(* ANFConstants.mli - Fold typed ANF constants and strength-reduce scalar operations. *)
module TempMap = InliningCommon.TempMap
module IntMap : Map.S with type key = int

type constEnv = ANF.atom TempMap.t
type typeEnv = AST.semanticType TempMap.t
type tupleEnv = ANF.atom IntMap.t TempMap.t

type optimizeOptions = {
  enableConstFolding : bool;
  enableConstProp : bool;
  enableCopyProp : bool;
  enableDCE : bool;
  enableCSE : bool;
  enableStrengthReduction : bool;
  enableTailRecursionModuloOperation : bool;
}

type optimizeContext = {
  typeReg : (string * AST.semanticType) list StringOrder.Map.t;
  recordTypeParams : string list StringOrder.Map.t;
  sumShapeReg : MemoryModel.rcSumShapeRegistry;
  functionNames : string FunctionIdMap.t;
  functionIds : AST.functionId StringOrder.Map.t;
}

val defaultOptimizeOptions : optimizeOptions
val tryLog2 : int64 -> int64 option
val tryLog2UInt64 : int64 -> int64 option
val euclideanMod : int64 -> int64 -> int64
val tryTruncateFloatToInt64 : float -> int64 option
val foldBinOp : ANF.binOp -> ANF.atom -> ANF.atom -> ANF.cExpr option
val isInt64Atom : typeEnv -> ANF.atom -> bool
val isIntegerAtom : typeEnv -> ANF.atom -> bool

val tryStrengthReduce :
  typeEnv -> ANF.binOp -> ANF.atom -> ANF.atom -> ANF.cExpr option

val foldUnaryOp : ANF.unaryOp -> ANF.atom -> ANF.cExpr option
