(* AsyncStorageGraph.mli - Immutable finite graphs for recursive transport layouts. *)
module Nodes : Map.S with type key = int

type 'a slot = { pending : 'a option; nested : int option }
type ('kind, 'a) node = { kind : 'kind; slots : 'a slot list }
type ('kind, 'a) t

val make : root:int -> ('kind, 'a) node Nodes.t -> ('kind, 'a) t
val root : ('kind, 'a) t -> int
val nodes : ('kind, 'a) t -> ('kind, 'a) node Nodes.t
val at : ('kind, 'a) t -> int -> ('kind, 'a) t
val node : ('kind, 'a) t -> ('kind, 'a) node
val reachable : ('kind, 'a) t -> int list

val assemble :
  'kind -> ('a option * ('kind, 'a) t option) list -> ('kind, 'a) t

val map : ('a -> 'b) -> ('kind, 'a) t -> ('kind, 'b) t

val equal :
  kind:('kind -> 'kind -> bool) ->
  ('kind, unit) t -> ('kind, unit) t -> bool

val join :
  compatible:('kind -> 'kind -> bool) ->
  ('kind, unit) t -> ('kind, unit) t -> ('kind, unit) t

val close :
  recursive:('kind -> bool) ->
  compatible:('kind -> 'kind -> bool) ->
  ('kind, unit) t -> ('kind, unit) t
