(* TraitResolution.mli - Static trait obligations and canonical ordinary call targets. *)
val substitute :
  Types.substitution -> TraitCatalog.obligation -> TraitCatalog.obligation

val check :
  TraitCatalog.t ->
  TraitCatalog.obligation list ->
  TraitCatalog.obligation ->
  (unit, string) result

val checkBounds :
  TraitCatalog.t ->
  TraitCatalog.obligation list ->
  Types.substitution ->
  TraitCatalog.obligation list ->
  (unit, string) result

val functionBounds :
  TraitCatalog.t ->
  TraitCatalog.obligation list ->
  string ->
  AST.semanticType list ->
  (unit, string) result

val select :
  TraitCatalog.t ->
  TraitCatalog.dispatch ->
  AST.semanticType list ->
  (string * AST.semanticType list, string) result

val checkType :
  TraitCatalog.t ->
  TraitCatalog.obligation list ->
  AST.semanticType ->
  (unit, string) result

val matchArguments :
  AST.semanticType list -> AST.semanticType list -> Types.substitution option
