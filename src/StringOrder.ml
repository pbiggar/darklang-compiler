(* StringOrder.ml - Native OCaml byte ordering for UTF-8 compiler names and keys. *)
type t = string

let compare = String.compare

module Map = Map.Make (String)
module Set = Set.Make (String)
