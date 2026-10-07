(* Reducer.mli - Shrink parsed syntax while preserving a differential failure. *)
val minimize : (string -> Oracle.outcome) -> string -> (string * Oracle.outcome * int * int, string) result
