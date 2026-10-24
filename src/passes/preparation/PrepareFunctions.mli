(* PrepareFunctions.mli - Expose whole-program generic preparation entry points. *)
val monomorphize :
  ?prepare:(CheckedAST.program -> CheckedAST.program) ->
  CheckedAST.program ->
  (CheckedAST.program, string) result

val monomorphizeWithExternalDefs :
  ?prepare:(CheckedAST.program -> CheckedAST.program) ->
  SpecializationIdentity.genericFuncDefs ->
  CheckedAST.program ->
  (CheckedAST.program, string) result
