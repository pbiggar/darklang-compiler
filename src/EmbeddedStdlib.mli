(* EmbeddedStdlib.mli - Ordered standard-library sources stored in the executable. *)
val sources : (string * string) list
val upstreamSources : (string * string) list
val compilerReasons : string
