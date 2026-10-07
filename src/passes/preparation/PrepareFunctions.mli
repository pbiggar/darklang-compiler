(* PrepareFunctions.mli - Expose whole-program generic preparation entry points. *)
val monomorphize : CheckedAST.program -> CheckedAST.program
val monomorphizeWithExternalDefs : SpecializationIdentity.genericFuncDefs -> CheckedAST.program -> CheckedAST.program
