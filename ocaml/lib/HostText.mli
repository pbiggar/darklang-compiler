(* HostText.mli - UTF-8 host text and Unicode scalar operations. *)
val scalars : string -> int array
val ofScalars : int array -> string
val length : string -> int
val first : string -> Uchar.t option
val lowerInvariant : string -> string
val caseFold : string -> string
val trim : string -> string
val contains : string -> string -> bool
val tryParseInt32 : string -> int32 option
val normalize : string -> string
val isLetter : int -> bool
val isDigit : int -> bool
val isUpper : int -> bool
val graphemeClusters : string -> string list
(* Read only the first cluster of already validated UTF-8 text. *)
val firstGrapheme : string -> string option
val startsWith : string -> string -> bool
val endsWith : string -> string -> bool
