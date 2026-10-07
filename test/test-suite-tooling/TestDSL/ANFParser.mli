(* Frozen ANF fixture parser and its component boundaries. *)
val parseTempId : string -> (Dark_compiler.ANF.tempId, string) result
val parseAtom : string -> (Dark_compiler.ANF.atom, string) result
val parseOp : string -> (Dark_compiler.ANF.binOp, string) result
val parseCExpr : string -> (Dark_compiler.ANF.cExpr, string) result
val parseAExpr : int -> string list -> (Dark_compiler.ANF.aExpr, string) result
val parseANF : string -> (Dark_compiler.ANF.program, string) result
