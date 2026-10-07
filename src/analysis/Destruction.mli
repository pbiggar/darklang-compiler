(* Destruction.mli - Prove inert destruction locally and through function scope contracts. *)
val hasInertDestruction : AST.semanticType -> bool
type scopeDestruction = InertScope | UnprovenScope
type functionScopeContract = {localDestruction : scopeDestruction; calls : SpecializationIdentity.FunctionSet.t}
val inertFunctionScopesWithBase : SpecializationIdentity.FunctionSet.t -> AST.functionId StringOrder.Map.t -> functionScopeContract FunctionIdMap.t -> SpecializationIdentity.FunctionSet.t
val inertFunctionScopes : AST.functionId StringOrder.Map.t -> functionScopeContract FunctionIdMap.t -> SpecializationIdentity.FunctionSet.t
