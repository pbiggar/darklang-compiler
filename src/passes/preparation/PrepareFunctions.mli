(* PrepareFunctions.mli - Expose whole-program generic preparation entry points. *)
val monomorphize : CheckedAST.program -> (CheckedAST.program, string) result

val monomorphizeWithExternalDefs :
  SpecializationIdentity.genericFuncDefs ->
  CheckedAST.program ->
  (CheckedAST.program, string) result
