(* RevBuffer.mli - Ordered local collectors used by the translated parser. *)
type 'a t

val create : unit -> 'a t
val add : 'a t -> 'a -> unit
val toList : 'a t -> 'a list
val length : 'a t -> int
val last : 'a t -> 'a option
