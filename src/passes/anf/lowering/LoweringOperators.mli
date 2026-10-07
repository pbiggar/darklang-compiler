(* LoweringOperators.mli - Lower numeric operators and structural equality into ANF. *)
val convertBinOp : AST.binOp -> ANF.binOp

val integerFunctionForBinOp :
  (string -> AST.functionId) ->
  AST.semanticType ->
  AST.binOp ->
  AST.functionId option

val convertUnaryOp : AST.unaryOp -> ANF.unaryOp
val isCompoundType : AST.semanticType -> bool

val generateStructuralEquality :
  (string -> AST.functionId) ->
  ANF.atom ->
  ANF.atom ->
  AST.semanticType ->
  ANF.varGen ->
  TypeRegistries.typeRegistry ->
  LoweringPrimitives.variantLookup ->
  LoweringPrimitives.sumRepresentationIndex ->
  (ANF.tempId * ANF.cExpr) list * ANF.atom * ANF.varGen
