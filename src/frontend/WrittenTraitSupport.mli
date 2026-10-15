(* WrittenTraitSupport.mli - Semantic catalog composition and static concrete method selection. *)
val predeclare :
  bool ->
  WrittenTypeSupport.typeInventory ->
  WrittenTypeSupport.globals ->
  WrittenSource.item list ->
  CheckedAST.symbols ->
  (WrittenTypeSupport.globals * CheckedAST.symbols, string) result

val materialize :
  int ->
  WrittenSource.item list ->
  WrittenSource.item list
  * (string list * string list * WrittenTypes.implDecl) list

val validate :
  WrittenTypeSupport.globals ->
  (string list * string list * WrittenTypes.implDecl) list ->
  (WrittenTypeSupport.globals, string) result

val functions :
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.functionSignature StringOrder.Map.t

val resolveBounds :
  WrittenTypeSupport.globals ->
  string list ->
  string list ->
  WrittenTypes.typeParamBound list ->
  (TraitCatalog.obligation list, string) result

val prepareBounds :
  WrittenTypeSupport.globals ->
  WrittenSource.item list ->
  (WrittenTypeSupport.globals, string) result

val catalog :
  WrittenTypeSupport.globals ->
  CheckedAST.symbols ->
  (TraitCatalog.t, string) result

val checkSignatureBounds :
  WrittenTypeSupport.globals ->
  CheckedAST.symbols ->
  WrittenTypeSupport.functionSignature ->
  AST.semanticType list ->
  (unit, string) result

val selectCall :
  CheckedAST.symbols ->
  string ->
  string ->
  AST.semanticType list ->
  (AST.functionId * AST.semanticType list, string) result

val namedMethod :
  WrittenTypes.range -> string -> string -> WrittenTypes.qualifiedFnIdentifier

val receiverMethod :
  WrittenTypeSupport.globals ->
  CheckedAST.symbols ->
  AST.semanticType ->
  string ->
  (string option, string) result

val proofAvailable :
  WrittenTypeSupport.globals ->
  CheckedAST.symbols ->
  string ->
  AST.semanticType ->
  bool

val inferFromBounds :
  WrittenTypeSupport.globals ->
  WrittenTypeSupport.functionSignature ->
  AST.semanticType list ->
  AST.semanticType list
