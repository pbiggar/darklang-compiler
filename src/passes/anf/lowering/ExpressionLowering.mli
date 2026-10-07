(* ExpressionLowering.mli - Lower expressions while delegating recursive children through typed callbacks. *)
val lowerExpression : LoweringCallbacks.expressionLowerer -> LoweringCallbacks.atomLowerer -> LoweringCallbacks.boundAtomLowerer -> TypeRegistries.functionIdRegistry -> LoweringCallbacks.expressionLowerer
