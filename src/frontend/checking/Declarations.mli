(* Declarations.mli - Validate type declarations and summarize source inventories. *)
val typeDefName : AST.typeDef -> string

val validateTopLevelTypeDeclarations :
  Types.typeCheckEnv option ->
  AST.topLevel list ->
  (unit, CheckingDiagnostics.typeError) result

val summarizeTopLevelDeclarations :
  AST.topLevel list -> ResolveDeclarations.topLevelDeclarationSummary
