(* SourceContracts.mli - Check public replacement contracts without checking discarded bodies. *)
type context = {
  types : WrittenTypeSupport.typeInventory;
  traits : StringOrder.Set.t;
  environment : WrittenChecking.environment option;
}

val emptyContext : context

val check :
  context:context ->
  sources:Validation.validatedSourceFile list ->
  (string list * WrittenTypes.declaration * WrittenTypes.declaration) list ->
  (unit, string) result
