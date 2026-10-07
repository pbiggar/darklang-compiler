(* Original RuntimeDataLayoutTests declarations. *)
type testResult = (unit, string) result

val tests : (string * (unit -> testResult)) list
