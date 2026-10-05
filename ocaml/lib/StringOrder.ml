(* StringOrder.ml - Preserve ordinal UTF-16 ordering for compiler identities and maps. *)
type t = string
(* Map/set lookups must not materialize both keys as UTF-16 arrays. Compare
   their unit streams directly, retaining WTF-8 and malformed-input checks. *)
external compare : string -> string -> int = "dark_compare_utf16"
module Ordered = struct type t = string let compare = compare end
module Map = Map.Make (Ordered)
module Set = Set.Make (Ordered)
