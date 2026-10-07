(* Capture both native console streams and restore them on every exit path. *)
val run : (unit -> 'a) -> 'a * string * string
