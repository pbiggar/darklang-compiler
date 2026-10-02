(* StringOrder.ml - Preserve ordinal UTF-16 ordering for compiler identities and maps. *)
type t = string
let compare left right =
  let left = HostText.utf16Units left and right = HostText.utf16Units right in
  let common = min (Array.length left) (Array.length right) in
  let rec loop index =
    if index = common then Int.compare (Array.length left) (Array.length right)
    else let order = Int.compare left.(index) right.(index) in if order = 0 then loop (index + 1) else order in
  loop 0
module Ordered = struct type t = string let compare = compare end
module Map = Map.Make (Ordered)
module Set = Set.Make (Ordered)
