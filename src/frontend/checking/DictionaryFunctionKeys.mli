(* Prove that public keys with function-bearing types have no occupied function payloads. *)
val check :
  CheckedAST.functionDef FunctionIdMap.t ->
  CheckedAST.symbols ->
  Types.aliasRegistry ->
  Types.indexedTypeRegistry ->
  Types.indexedSumTypeRegistry ->
  CheckedAST.topLevel list ->
  (unit, string) result
