(* Capture both streams and enforce the fixture process deadline. *)
val capture :
  string -> string list -> int -> (int * string * string, string) result

val captureWithInputAndEnvironment :
  string ->
  string list ->
  (string * string) list ->
  bytes ->
  int ->
  (int * string * string, string) result
