(* ANF_Intrinsics.mli - Give named fixed-width arithmetic and operators one ANF operation. *)
type arithmeticIntrinsic = {
  operandType : AST.semanticType;
  operation : ANF.binOp;
}

val canonicalizeProgram :
  TypeRegistries.functionIdRegistry ->
  TypeRegistries.functionRegistry ->
  ANF.program ->
  ANF.program
