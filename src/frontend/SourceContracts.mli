(* SourceContracts.mli - Check public replacement contracts without checking discarded bodies. *)
val check :
  sources:Validation.validatedSourceFile list ->
  (string list * WrittenTypes.declaration * WrittenTypes.declaration) list ->
  (unit, string) result
