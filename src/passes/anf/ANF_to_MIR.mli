(* Transform ANF and SSA ANF to typed MIR control-flow graphs. *)
module TempMap = RcTypeFacts.TempMap

type cfgBuilder = {
  blocks : MIR.basicBlock MIR.LabelMap.t;
  joins : (MIR.label * AST.semanticType) TempMap.t;
  joinIncoming : (MIR.operand * MIR.label) list TempMap.t;
  selfTailIncoming : (MIR.label * MIR.operand list) list;
  labelGen : MIR.labelGen;
  regGen : MIR.regGen;
  typeById : ANF.typeMap;
  sourceTempIdMax : int;
  extraTypeMap : AST.semanticType TempMap.t;
  typeReg : (string * AST.semanticType) list StringOrder.Map.t;
  returnTypeReg : AST.semanticType FunctionIdMap.t;
  functionNames : string FunctionIdMap.t;
  funcId : AST.functionId;
  funcName : string;
  paramRegs : MIR.vReg list;
  floatRegs : MIR.IntSet.t;
  closureFuncs : AST.functionId TempMap.t;
  enableCoverage : bool;
  exprIdGen : ANF.exprIdGen;
  coverageMapping : ANF.coverageMapping;
}

type exprExit = Returned of MIR.operand * MIR.label | Terminated

val buildVariantRegistry :
  LoweringPrimitives.variantLookup -> MIR.variantRegistry

val buildRecordRegistry :
  (string * AST.semanticType) list StringOrder.Map.t -> MIR.recordRegistry

val convertBinOp : ANF.binOp -> MIR.binOp
val convertUnaryOp : ANF.unaryOp -> MIR.unaryOp
val convertCliOperation : ANF.cliOperation -> MIR.cliOperation
val tempToVReg : ANF.tempId -> MIR.vReg
val maxTempIdInAtom : ANF.atom -> int
val maxTempIdInCExpr : ANF.cExpr -> int
val maxTempIdInAExpr : ANF.aExpr -> int
val maxTempIdInFunction : ANF.functionDef -> int
val maxTempIdInProgram : ANF.program -> int
val isFloatAtom : MIR.IntSet.t -> ANF.atom -> bool

val cexprProducesFloat :
  MIR.IntSet.t -> AST.semanticType FunctionIdMap.t -> ANF.cExpr -> bool

val buildReturnTypeReg :
  ANF.functionDef list ->
  (string * AST.semanticType) FunctionIdMap.t ->
  AST.semanticType FunctionIdMap.t

val tryGetIntrinsicReturnType : string -> AST.semanticType option
val atomToOperand : cfgBuilder -> ANF.atom -> (MIR.operand, string) result
val atomType : cfgBuilder -> ANF.atom -> AST.semanticType
val binOpType : cfgBuilder -> ANF.atom -> ANF.atom -> AST.semanticType
val operandType : cfgBuilder -> MIR.operand -> AST.semanticType
val cexprDescription : ANF.cExpr -> string
val withCoverage : cfgBuilder -> ANF.cExpr -> MIR.instr list * cfgBuilder

val collectSelfTailCallCleanup :
  cfgBuilder -> ANF.tempId -> ANF.aExpr -> (MIR.instr list, string) result

val collectPreSelfTailCallCleanup :
  MIR.instr list -> MIR.instr list * MIR.instr list

val transferOverlappingArgOwnership :
  MIR.operand list ->
  MIR.instr list ->
  MIR.instr list ->
  MIR.instr list * MIR.instr list

val convertExpr :
  AST.semanticType ->
  ANF.aExpr ->
  MIR.label ->
  MIR.instr list ->
  cfgBuilder ->
  (exprExit * cfgBuilder, string) result

val convertSSAANFFunction :
  SSAANF.functionDef ->
  ANF.typeMap ->
  (string * AST.semanticType) list StringOrder.Map.t ->
  AST.semanticType FunctionIdMap.t ->
  string FunctionIdMap.t ->
  bool ->
  (MIR.functionDef, string) result

val convertANFFunction :
  ANF.functionDef ->
  ANF.typeMap ->
  (string * AST.semanticType) list StringOrder.Map.t ->
  AST.semanticType FunctionIdMap.t ->
  string FunctionIdMap.t ->
  bool ->
  (MIR.functionDef, string) result

val toMIR :
  ANF.program ->
  ANF.typeMap ->
  (string * AST.semanticType) list StringOrder.Map.t ->
  AST.semanticType ->
  LoweringPrimitives.variantLookup ->
  (string * AST.semanticType) list StringOrder.Map.t ->
  bool ->
  (string * AST.semanticType) FunctionIdMap.t ->
  string FunctionIdMap.t ->
  (MIR.program, string) result

val toMIRFunctionsOnly :
  ANF.program ->
  ANF.typeMap ->
  (string * AST.semanticType) list StringOrder.Map.t ->
  LoweringPrimitives.variantLookup ->
  (string * AST.semanticType) list StringOrder.Map.t ->
  bool ->
  (string * AST.semanticType) FunctionIdMap.t ->
  string FunctionIdMap.t ->
  ( MIR.functionDef list * MIR.variantRegistry * MIR.recordRegistry,
    string )
  result

val toMIRFunctionsOnlyWithTrace :
  (string -> float -> unit) option ->
  (MIR.variantRegistry * MIR.recordRegistry) option ->
  AST.loweredRecursiveMember FunctionIdMap.t ->
  bool ->
  ANF.program ->
  ANF.typeMap ->
  (string * AST.semanticType) list StringOrder.Map.t ->
  LoweringPrimitives.variantLookup ->
  (string * AST.semanticType) list StringOrder.Map.t ->
  bool ->
  AST.semanticType FunctionIdMap.t ->
  string FunctionIdMap.t ->
  ( MIR.functionDef list * MIR.variantRegistry * MIR.recordRegistry,
    string )
  result

val toMIRSSAFunctionsOnlyWithTrace :
  (string -> float -> unit) option ->
  (MIR.variantRegistry * MIR.recordRegistry) option ->
  AST.loweredRecursiveMember FunctionIdMap.t ->
  bool ->
  SSAANF.functionDef list ->
  ANF.typeMap ->
  (string * AST.semanticType) list StringOrder.Map.t ->
  LoweringPrimitives.variantLookup ->
  (string * AST.semanticType) list StringOrder.Map.t ->
  bool ->
  AST.semanticType FunctionIdMap.t ->
  string FunctionIdMap.t ->
  ( MIR.functionDef list * MIR.variantRegistry * MIR.recordRegistry,
    string )
  result
