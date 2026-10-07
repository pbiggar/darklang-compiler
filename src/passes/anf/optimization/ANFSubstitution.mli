(* ANFSubstitution.mli - Substitute ANF atoms and simplify individual expressions. *)
val substAtom : ANFConstants.constEnv -> ANF.atom -> ANF.atom
val substCExpr : ANFConstants.constEnv -> ANF.cExpr -> ANF.cExpr

val optimizeCExpr :
  ANFConstants.optimizeContext ->
  ANFConstants.optimizeOptions ->
  ANFConstants.constEnv ->
  ANFConstants.typeEnv ->
  ANFConstants.tupleEnv ->
  ANF.cExpr ->
  ANF.cExpr * bool
