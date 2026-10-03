(* Complete type-resolution observations for the immutable reference corpus. *)
val observe : string -> Yojson.Basic.t
val additionalExpressions : string -> Dark_compiler.AST.expr list
