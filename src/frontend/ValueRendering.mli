(* ValueRendering.fs - Interpreter-compatible result rendering.
   Builds monomorphic Dark functions for all native eval boundaries. *)
val rewriteProgram : Types.indexedTypeRegistry -> Types.indexedSumTypeRegistry -> AST.semanticType -> CheckedAST.program -> CheckedAST.program
val rewriteDictionaryKeyRenderers : Types.indexedTypeRegistry -> Types.indexedSumTypeRegistry -> CheckedAST.program -> CheckedAST.program
