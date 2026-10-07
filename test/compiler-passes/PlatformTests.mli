(** Original supported/rejected target-pair expectations. *)
type testResult = (unit, string) result
val tests : (string * (unit -> testResult)) list
