(* PrintInsertion.mli - Insert result output before ownership analysis. *)
val unsupportedListDisplay : AST.semanticType -> 'a

val wrapReturnWithPrint :
  (string -> AST.functionId) ->
  AST.semanticType ->
  ANF.varGen ->
  ANF.aExpr ->
  ANF.aExpr * ANF.varGen

val insertPrint :
  TypeRegistries.functionIdRegistry ->
  ANF.functionDef list ->
  ANF.aExpr ->
  AST.semanticType ->
  ANF.program

val insertPrintInEntry :
  TypeRegistries.functionIdRegistry ->
  string ->
  AST.semanticType ->
  ANF.functionDef list ->
  (ANF.functionDef list, string) result

val insertRootWordProbeInEntry :
  TypeRegistries.functionNameRegistry ->
  string ->
  bool ->
  ANF.functionDef list ->
  (ANF.functionDef list, string) result
