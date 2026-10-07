(* FileIO.mli - Source-file paths and BOM-aware text input. *)
val absolutePath : string -> string
val exists : string -> bool
val readText : string -> string
