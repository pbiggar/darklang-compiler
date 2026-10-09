type testResult = (unit, string) result
(** Translated unchanged BitsetTests expectations. *)

val tests : (string * (unit -> testResult)) list
