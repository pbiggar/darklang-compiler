(* WrittenSource.mli - Preserve source declarations and their module scopes. *)
type item =
  | Function of string list * WrittenTypes.fnDecl
  | Value of string list * WrittenTypes.valueDecl
  | Type of string list * WrittenTypes.typeDecl
  | Expression of string list * WrittenTypes.expr

val items : Validation.validatedSourceFile -> (item list, string) result

val validateSourceUnits :
  bool ->
  (string * NameSyntax.SourceUnitPurpose.t * Validation.validatedSourceFile)
  list ->
  (Validation.validatedSourceFile list, string) result

val expressionNames : WrittenTypes.expr -> string list

val qualifiedNames :
  Validation.validatedSourceFile list -> (string list, string) result
