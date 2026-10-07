(* OwnedFunctionGroups.mli - Discover deterministic call groups in owned HIR. *)
type ('leaf, 'id) group
type groupingError = DuplicateFunctionName of AST.functionId
val functions : ('leaf, 'id) group -> ('leaf, 'id) OwnedIR.functionDef list
val isRecursive : ('leaf, 'id) group -> bool
val internalDependencies : ('leaf, 'id) group -> SpecializationIdentity.FunctionSet.t
val externalTargets : ('leaf, 'id) group -> SpecializationIdentity.FunctionSet.t
val orderedFunctionIds : AST.functionId list -> AST.functionId list FunctionIdMap.t -> AST.functionId list list
val discover : ('leaf, 'id) OwnedIR.functionDef list -> (('leaf, 'id) group list, groupingError) result
