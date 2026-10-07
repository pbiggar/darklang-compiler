(* Minimal context constructor for native runtime execution checks. *)
val context : string -> Dark_compiler.ARM64.targetConfig -> bool -> Dark_compiler.ARM64CodeGenTypes.codeGenContext
