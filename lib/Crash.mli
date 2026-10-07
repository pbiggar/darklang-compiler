(** Internal invariant failures; recoverable errors remain explicit results. *)
val crash : string -> 'a
val todo : string -> 'a
