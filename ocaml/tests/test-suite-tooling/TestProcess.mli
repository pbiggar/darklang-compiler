(* Capture both streams and enforce the fixture process deadline. *)
val capture : string -> string list -> int -> (int*string*string,string) result
