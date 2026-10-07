type testResult = (unit, string) result
(** Original supported/rejected target-pair expectations. *)

val tests : (string * (unit -> testResult)) list
