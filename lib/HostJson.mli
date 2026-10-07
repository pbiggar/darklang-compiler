(* Bounded JSON documents retaining original element text for diagnostics. *)
type t
type kind=Object|Array|String|Number|Boolean|Null
val parse : ?maxDepth:int -> string -> t
val rawText : t -> string
val kind : t -> kind
val fields : t -> (string * t) list
val items : t -> t list
val string : t -> string
val boolean : t -> bool
val tryInt32 : t -> int option
