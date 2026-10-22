(* SourceSelection.mli - Select annotated source implementations before body checking. *)
type unavailable = { name : string; reason : string; excluded : bool }

type selected = {
  units : (string * Validation.validatedSourceFile) list;
  unavailable : unavailable list;
}

type node = {
  file : string;
  path : string list;
  name : string;
  declaration : WrittenTypes.declaration;
  disposition : CompilerAnnotations.disposition;
}

val nodes : string -> SourceInventory.source -> node list
val kind : WrittenTypes.declaration -> string

val select :
  context:SourceContracts.context ->
  roots:string list ->
  supplements:string list ->
  sources:(string * SourceInventory.source) list ->
  (selected, string) result

val require : selected -> string -> (unit, string) result
