(* HostText.mli - Explicit .NET text semantics used by the compiler and runner. *)
val lowerInvariant : string -> string
val trim : string -> string
val contains : string -> string -> bool
val tryParseInt32 : string -> int32 option
val utf16Units : string -> int array
val ofUtf16Units : int array -> string
val normalize : string -> string
val isLetterUnit : int -> bool
val isDigitUnit : int -> bool
val isUpperUnit : int -> bool
val graphemeClusters : string -> string list
val startsWithCurrentCulture : string -> string -> bool
