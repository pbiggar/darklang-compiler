(* ContentEncoding.mli - BOM and HTTP charset decoding for source and response text. *)
val decodeContent : string option -> string -> string
