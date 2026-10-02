(* StringOrder.mli - Compiler map/set ordering under the reference UTF-16 contract. *)
type t = string
val compare : string -> string -> int
module Map : Map.S with type key = string
module Set : Set.S with type elt = string
