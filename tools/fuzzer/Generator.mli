(* Generator.mli - Construct typed Dark ASTs for differential execution. *)
val generate : Random.State.t -> int -> Dark_compiler.AST.program
