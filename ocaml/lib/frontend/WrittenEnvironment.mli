(* Publish completed direct-checked declarations to later compiler passes. *)
val typeCheckEnvironment : CheckedAST.program -> Types.typeCheckEnv
