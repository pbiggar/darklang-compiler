(* WrittenFormatter.mli - Range-free syntax fingerprints and conservative formatting. *)
val syntaxKey : WrittenTypes.sourceFile -> string
val format : string -> WrittenTypes.sourceFile -> string
