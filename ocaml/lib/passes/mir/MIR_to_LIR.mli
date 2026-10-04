(* Select target-neutral LIR instructions and preserve typed MIR CFG edges. *)
type integerErrorLabels={divideByZero:LIR.label;moduloByZero:LIR.label;moduloNegativeDivisor:LIR.label}
type tempState={nextRegId:int;nextFRegId:int}
type printRcContext={recordFields:(string*AST.semanticType) list StringOrder.Map.t;recordTypeParams:string list StringOrder.Map.t;sumShapes:MemoryModel.rcSumShapeRegistry}
val convertCliOperation : MIR.cliOperation -> LIR.cliOperation
val vregToLIRReg : MIR.vReg -> LIR.reg
val vregToLIRFReg : MIR.vReg -> LIR.fReg
val convertOperand : MIR.operand -> LIR.operand
val applyTypeSubst : string list -> AST.semanticType list -> AST.semanticType -> AST.semanticType
val freshTempReg : tempState -> LIR.reg * tempState
val freshTempFReg : tempState -> LIR.fReg * tempState
val ensureInRegister : MIR.operand -> tempState -> (LIR.instr list * LIR.reg * tempState,string) result
val ensureBlobInRegister : MIR.operand -> tempState -> (LIR.instr list * LIR.reg * tempState,string) result
val ensureInFRegister : MIR.operand -> tempState -> (LIR.instr list * LIR.fReg * tempState,string) result
val truncateForType : LIR.reg -> AST.semanticType -> LIR.instr list
val shouldCheckNegativeDivisor : AST.semanticType -> bool
val isUnsignedIntegerType : AST.semanticType -> bool
val shiftCountMask : AST.semanticType -> int64
val usesNativeVariableShiftMask : AST.semanticType -> bool
val comparisonCondition : AST.semanticType -> MIR.binOp -> LIR.condition
val buildIntegerModuloParts : LIR.reg -> MIR.operand -> MIR.operand -> AST.semanticType -> tempState -> (LIR.instr list * LIR.reg * LIR.instr list * tempState,string) result
val buildFloatArgMoves : MIR.operand list -> LIR.physFPReg list -> tempState -> (LIR.instr list * tempState,string) result
val selectInstr : Platform.arch -> MIR.instr -> MIR.variantRegistry -> MIR.recordRegistry -> printRcContext -> MIR.IntSet.t -> tempState -> (LIR.instr list * tempState,string) result
val selectTerminator : MIR.terminator -> AST.semanticType -> tempState -> (LIR.instr list * LIR.terminator * tempState,string) result
val convertLabel : MIR.label -> LIR.label
val maxVRegId : MIR.vReg -> int -> int
val maxVRegIdFromOperand : MIR.operand -> int -> int
val maxVRegIdsFromOperands : MIR.operand list -> int -> int
val maxVRegIdFromInstr : MIR.instr -> int -> int
val maxVRegIdFromTerminator : MIR.terminator -> int -> int
val initTempState : MIR.functionDef -> tempState
val integerErrorBlock : LIR.label -> string -> LIR.basicBlock
val selectBlocksWithModuloChecks : Platform.arch -> string -> MIR.basicBlock -> MIR.variantRegistry -> MIR.recordRegistry -> printRcContext -> AST.semanticType -> MIR.IntSet.t -> integerErrorLabels -> tempState -> (LIR.basicBlock list * LIR.label * tempState,string) result
val selectCFG : Platform.arch -> string -> MIR.cfg -> MIR.variantRegistry -> MIR.recordRegistry -> printRcContext -> AST.semanticType -> MIR.IntSet.t -> integerErrorLabels -> tempState -> (LIR.cfg,string) result
val toLIRFunctionsForWithTrace : (string -> float -> unit) option -> Platform.arch -> MIR.program -> (LIR.functionDef list,string) result
val toLIRFunctionsForWithTraceAndRcRegistries : (string -> float -> unit) option -> Platform.arch -> (string*AST.semanticType) list StringOrder.Map.t -> string list StringOrder.Map.t -> MemoryModel.rcSumShapeRegistry -> MIR.program -> (LIR.functionDef list,string) result
val toLIRForWithTrace : (string -> float -> unit) option -> Platform.arch -> MIR.program -> (LIR.program,string) result
val toLIRFor : Platform.arch -> MIR.program -> (LIR.program,string) result
val toLIR : MIR.program -> (LIR.program,string) result
