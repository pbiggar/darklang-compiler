(* AtomLowering.mli - Lower atom-producing expressions and their ordered binding prefixes. *)
val lowerAtom : LoweringCallbacks.expressionLowerer -> LoweringCallbacks.atomLowerer -> LoweringCallbacks.boundAtomLowerer -> TypeRegistries.functionIdRegistry -> LoweringCallbacks.atomLowerer
