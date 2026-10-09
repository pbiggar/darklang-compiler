(* NonEmptyList.mli - Nonempty semantic AST collections. *)
type 'a t = { head : 'a; tail : 'a list }

val singleton : 'a -> 'a t
val cons : 'a -> 'a t -> 'a t
val toList : 'a t -> 'a list
val map : ('a -> 'b) -> 'a t -> 'b t
val length : 'a t -> int
val appendList : 'a t -> 'a list -> 'a t
val snoc : 'a t -> 'a -> 'a t
val head : 'a t -> 'a
val tryFromList : 'a list -> 'a t option
val fromList : 'a list -> 'a t
