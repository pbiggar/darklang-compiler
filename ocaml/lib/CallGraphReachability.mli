(* CallGraphReachability.fs - Shared call graph traversal helpers *)
val findReachable : SpecializationIdentity.FunctionSet.t FunctionIdMap.t -> SpecializationIdentity.FunctionSet.t -> SpecializationIdentity.FunctionSet.t
