(* SemanticJson.mli - Temporary complete migration observations. *)
val tokens : (Dark_compiler.Lexer.spannedToken list * (Dark_compiler.Tokenizer.tokenRange * string) list, string) result -> Yojson.Basic.t
val parserSupport : string -> Yojson.Basic.t
val patterns : string -> Yojson.Basic.t
val types : string -> Yojson.Basic.t
val bindings : string -> Yojson.Basic.t
val declarationSupport : string -> string -> Yojson.Basic.t
val ast : string -> Yojson.Basic.t
val validated : string -> Yojson.Basic.t
val rendered : string -> Yojson.Basic.t
val writtenSource : string -> Yojson.Basic.t
val names : string -> Yojson.Basic.t
