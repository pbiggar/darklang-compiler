(* Unification.mli - Structural matching, inference consolidation, and reconciliation. *)
val unificationVar : AST.semanticType -> string option

val matchConcrete :
  AST.semanticType ->
  AST.semanticType ->
  ((string * AST.semanticType) list, string) result

val matchTypes :
  AST.semanticType ->
  AST.semanticType ->
  ((string * AST.semanticType) list, string) result

val emptyListElementVar : string
val isInferenceVar : string -> bool
val containsTVar : AST.semanticType -> bool
val typesCompatible : AST.semanticType -> AST.semanticType -> bool

val typesCompatibleWithAliases :
  Types.aliasRegistry -> AST.semanticType -> AST.semanticType -> bool

val consolidateBindings :
  (string * AST.semanticType) list -> (Types.substitution, string) result

val unifyTypes :
  AST.semanticType -> AST.semanticType -> (Types.substitution, string) result

val reconcileTypes :
  Types.aliasRegistry option ->
  AST.semanticType ->
  AST.semanticType ->
  AST.semanticType option

val inferTypeArgs :
  string list ->
  AST.semanticType list ->
  AST.semanticType list ->
  AST.semanticType option ->
  AST.semanticType option ->
  (AST.semanticType list, string) result

val tryLookupResolved : string -> 'a StringOrder.Map.t -> ('a * string) option
