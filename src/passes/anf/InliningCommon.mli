(* InliningCommon.mli - Call eligibility, external candidate analysis, and ANF renaming. *)
type inliningConfig = {
  maxFunctionSize : int;
  maxInlineDepth : int;
  maxExternalInlineSites : int;
  maxBoundedLoopIterations : int;
  maxBoundedLoopExpansion : int;
  maxProjectedTupleInlineSize : int;
  maxProjectedTupleInlineSites : int;
}

val defaultConfig : inliningConfig

type functionInfo = {
  func : ANF.functionDef;
  calls : SpecializationIdentity.FunctionSet.t;
  size : int;
  isRecursive : bool;
  hasClosures : bool;
  hasTailCalls : bool;
  isExternal : bool;
}

module TempMap : Map.S with type key = ANF.tempId

val buildReverseCallGraph :
  SpecializationIdentity.FunctionSet.t FunctionIdMap.t ->
  SpecializationIdentity.FunctionSet.t FunctionIdMap.t

val dfsFinishOrder :
  SpecializationIdentity.FunctionSet.t FunctionIdMap.t ->
  AST.functionId ->
  SpecializationIdentity.FunctionSet.t ->
  AST.functionId list ->
  SpecializationIdentity.FunctionSet.t * AST.functionId list

val dfsCollectSCC :
  SpecializationIdentity.FunctionSet.t FunctionIdMap.t ->
  AST.functionId ->
  SpecializationIdentity.FunctionSet.t ->
  SpecializationIdentity.FunctionSet.t ->
  SpecializationIdentity.FunctionSet.t * SpecializationIdentity.FunctionSet.t

val findSCCs :
  SpecializationIdentity.FunctionSet.t ->
  SpecializationIdentity.FunctionSet.t FunctionIdMap.t ->
  SpecializationIdentity.FunctionSet.t list

val findRecursiveFunctions :
  ANF.functionDef list ->
  SpecializationIdentity.FunctionSet.t FunctionIdMap.t ->
  SpecializationIdentity.FunctionSet.t

val buildFunctionInfoMap : ANF.functionDef list -> functionInfo FunctionIdMap.t
val renameAtom : ANF.tempId TempMap.t -> ANF.atom -> ANF.atom
val renameCExpr : ANF.tempId TempMap.t -> ANF.cExpr -> ANF.cExpr

val renameExpr :
  ANF.tempId TempMap.t -> ANF.varGen -> ANF.aExpr -> ANF.aExpr * ANF.varGen

val shouldInline : functionInfo -> inliningConfig -> int -> bool

val buildExternalCandidateInfoMap :
  inliningConfig -> ANF.functionDef list -> functionInfo FunctionIdMap.t
