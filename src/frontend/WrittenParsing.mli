(* WrittenParsing.mli - Executable source parsing with rendered diagnostics. *)
val parse :
  Validation.mode -> string -> (Validation.validatedSourceFile, string) result
