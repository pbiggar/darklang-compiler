(* StringOrder.mli - Native OCaml map/set ordering for compiler names and keys. *)
type t = string
val compare : string -> string -> int
module Map : Map.S with type key = string
module Set : Set.S with type elt = string
