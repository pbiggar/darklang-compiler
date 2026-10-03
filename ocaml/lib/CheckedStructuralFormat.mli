(* Frozen F# structural spelling for checked diagnostics. *)
val value : CheckedAST.expr -> StructuralValue.value
val expr : CheckedAST.expr -> string
val toString : CheckedAST.expr -> string
val pattern : CheckedAST.pattern -> string
