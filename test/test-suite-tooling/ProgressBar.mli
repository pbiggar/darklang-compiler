(* Progress bar state and synchronized updates. *)
type state = {
  total : int;
  mutable completed : int;
  mutable failed : int;
  label : string;
}

val create : string -> int -> state
val update : state -> unit
val increment : state -> bool -> unit
val finish : state -> unit
