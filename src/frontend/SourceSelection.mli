(* SourceSelection.mli - Select annotated source implementations before body checking. *)
type unavailable = { name : string; reason : string; excluded : bool }

type selected = {
  units : (string * Validation.validatedSourceFile) list;
  unavailable : unavailable list;
}

val select :
  roots:string list ->
  sources:(string * SourceInventory.source) list ->
  (selected, string) result

val require : selected -> string -> (unit, string) result
