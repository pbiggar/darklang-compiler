(* ANFAccumulatorOptimization.mli - Lower eligible recursion through scalar accumulators or constructor destinations. *)
val planTailRecursionModuloHelpers : int64 -> TypeRegistries.functionNameRegistry -> TypeRegistries.functionIdRegistry -> SpecializationIdentity.FunctionSet.t -> (string * AST.functionId) FunctionIdMap.t
val transformTailRecursionModuloFixedConstructors : (string * AST.functionId) FunctionIdMap.t -> ANF.varGen -> ANF.program -> ANF.program * ANF.varGen
val transformTailRecursionModuloAddition : (string * AST.functionId) FunctionIdMap.t -> ANF.varGen -> ANF.program -> ANF.program * ANF.varGen
val transformTailRecursionModuloMultiplication : (string * AST.functionId) FunctionIdMap.t -> ANF.varGen -> ANF.program -> ANF.program * ANF.varGen
val transformTailRecursionModuloSubtraction : (string * AST.functionId) FunctionIdMap.t -> ANF.varGen -> ANF.program -> ANF.program * ANF.varGen
val transformTailRecursionModuloListConstructors : (string * AST.functionId) FunctionIdMap.t -> ANF.functionDef StringOrder.Map.t -> ANF.varGen -> ANF.program -> ANF.program * ANF.varGen
