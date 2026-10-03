(* Printing.fs - Escape values and select functions for representation-local printers. *)
val escapeStringContent : string -> string
val appendTypeSuffix : AST.semanticType option -> string -> string
val commaSeparated : ('a -> string) -> 'a list -> string
val functionNameMatches : string option -> string -> bool
val noFunctionMatchText : string option -> string
