(* Expressions.fs - Tie recursive ANF lowering handlers and list-region selection together. *)
val toANFCore : TypeRegistries.functionIdRegistry -> LoweringCallbacks.expressionLowerer
val toAtomCore : TypeRegistries.functionIdRegistry -> LoweringCallbacks.atomLowerer
val toANFBoundAtomCore : TypeRegistries.functionIdRegistry -> LoweringCallbacks.boundAtomLowerer
