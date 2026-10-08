(* UserCompilation.mli - Compile a user source unit through the typed pipeline stages. *)
val compileUserWithPlan :
  ?writtenSources:WrittenTypes.sourceFile option list ->
  PackageCatalog.userCompilePlan -> CompilerOptions.compileReport
