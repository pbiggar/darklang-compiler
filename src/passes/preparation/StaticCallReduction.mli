(* Remove statically unused generic callbacks before representation selection. *)
val reduce :
  SpecializationIdentity.genericFuncDefs -> CheckedAST.program -> CheckedAST.program
