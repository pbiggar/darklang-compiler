(* PackageIO.mli - SQLite response cache and verified OCaml HTTP/TLS transport. *)
val cacheRead : string -> string -> (int * string) option
val cacheWrite : string -> string -> int -> string -> unit
type client
val create : unit -> client
val dispose : client -> unit
val get : client -> string -> int * string
