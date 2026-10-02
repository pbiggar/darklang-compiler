(* TestFileIO.mli - Deterministic fixture file reads for the translated test tooling. *)
val exists : string -> bool
val readAllText : string -> string
val readAllLines : string -> string array
