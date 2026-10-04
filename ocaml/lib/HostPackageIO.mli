(* Native host boundaries for the package resolver's SQLite cache and HTTP. *)
val cacheRead : string -> string -> (int * string) option
val cacheWrite : string -> string -> int -> string -> unit
val resolveUrl : string -> string -> string
type client
val create : unit -> client
val dispose : client -> unit
val decodeContent : string option -> string -> string
val get : client -> string -> int * string
