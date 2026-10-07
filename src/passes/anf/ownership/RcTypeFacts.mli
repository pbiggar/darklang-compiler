(* RcTypeFacts.mli - Track ANF value types and immutable context projections for RC insertion. *)
module TempMap = InliningCommon.TempMap
type rcTypePlanningContext = {mutable recordRegistries : ((string * AST.semanticType) list StringOrder.Map.t * string list StringOrder.Map.t) option; shapes : (AST.semanticType, MemoryModel.rcShape) Hashtbl.t; metadata : (AST.semanticType, MemoryModel.rcMetadata) Hashtbl.t}
type typeContext = {typeReg : TypeRegistries.typeRegistry; variantLookup : LoweringPrimitives.variantLookup; sumShapeReg : MemoryModel.rcSumShapeRegistry; funcReg : TypeRegistries.functionRegistry; funcParams : (string * AST.semanticType) list StringOrder.Map.t; tempTypes : AST.semanticType TempMap.t; closureFuncs : AST.functionId TempMap.t; typePlanning : rcTypePlanningContext}
val createRcTypePlanningContext : unit -> rcTypePlanningContext
val createContext : AST_to_ANF.conversionResult -> typeContext
val withTempTypes : typeContext -> AST.semanticType TempMap.t -> typeContext
val addClosureFunc : typeContext -> ANF.tempId -> AST.functionId -> typeContext
val tryGetClosureFunc : typeContext -> ANF.atom -> AST.functionId option
val tryGetType : typeContext -> ANF.tempId -> AST.semanticType option
val tryGetFuncReturnTypeFromReg : typeContext -> AST.functionId -> AST.semanticType option
val inferAtomType : typeContext -> ANF.atom -> AST.semanticType option
val inferCExprType : typeContext -> ANF.cExpr -> AST.semanticType option
