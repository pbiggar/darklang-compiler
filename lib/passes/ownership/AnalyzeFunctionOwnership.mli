(* AnalyzeFunctionOwnership.fs - Schedule verified whole-function ownership over checked HIR. *)
type context = {typeReg : TypeRegistries.typeRegistry; typeNames : TypeRegistries.typeNameRegistry; recordFieldsReg : (string * AST.semanticType) list StringOrder.Map.t; recordTypeParamsReg : string list StringOrder.Map.t; variantLookup : LoweringPrimitives.variantLookup; sumMetadata : LoweringPrimitives.sumMetadata; rcSumShapeReg : MemoryModel.rcSumShapeRegistry; funcReg : TypeRegistries.functionRegistry; functionNames : TypeRegistries.functionNameRegistry; moduleRegistry : AST.moduleRegistry}
module Ownership : module type of OwnedIR.Make (ListLiveness.Identity)
module Verification : module type of VerifyOwnedHIR.Make (ListLiveness.Identity)
module Scheduler : module type of ScheduleOwnershipVariants.Make (ListLiveness.Identity)
type analysisError = HIRConstructionFailed of ConstructHIRFunctions.constructionError | OwnershipElaborationFailed of ElaborateFunctionOwnership.elaborationError | OwnedHIRVerificationFailed of Verification.verificationError | FunctionOwnershipVerificationFailed of AST.functionId * Ownership.verificationError | SpecializationSchedulingFailed of Scheduler.schedulingError
type analysis
val functions : analysis -> (ConstructHIRFunctions.primitive, HIR.valueId) OwnedIR.functionDef list
val semantics : analysis -> ConstructHIRFunctions.primitive Ownership.semantics
val hirContracts : analysis -> ConstructHIRFunctions.primitive VerifyOwnedHIR.hirContracts
val schedule : analysis -> (ConstructHIRFunctions.primitive, HIR.valueId) ScheduleOwnershipVariants.plan
val originalFunctions : analysis -> (ConstructHIRFunctions.primitive, HIR.valueId) OwnedIR.functionDef list
val analyzeWithTrace : (string -> float -> unit) option -> context -> CheckedAST.functionDef list -> (analysis, analysisError) result
val analyze : context -> CheckedAST.functionDef list -> (analysis, analysisError) result
