(* Package transport bindings for the package resolver's SQLite cache and HTTP. *)
external cacheRead : string -> string -> (int * string) option = "dark_package_cache_read"
external cacheWrite : string -> string -> int -> string -> unit = "dark_package_cache_write"
type client
external create : unit -> client = "dark_package_http_create"
external dispose : client -> unit = "dark_package_http_dispose"
external getBytes : client -> string -> int * string * string option = "dark_package_http_get"
let get client url = let status,body,contentType=getBytes client url in status,ContentEncoding.decodeContent contentType body
