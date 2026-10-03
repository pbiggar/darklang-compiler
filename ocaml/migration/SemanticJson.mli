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
val astHelpers : string -> Yojson.Basic.t
val formatter : string -> Yojson.Basic.t
val resolution : string -> Yojson.Basic.t
val checkingDiagnostics : string -> Yojson.Basic.t
val freeVariables : string -> Yojson.Basic.t
val functionIdMap : string -> Yojson.Basic.t
(* Shared typed encoders for additional migration stages. *)
val string : string -> Yojson.Basic.t
val int32 : int -> Yojson.Basic.t
val union : string -> string -> Yojson.Basic.t list -> Yojson.Basic.t
val record : string -> (string * Yojson.Basic.t) list -> Yojson.Basic.t

val typeDefinition : Dark_compiler.WrittenTypes.typeDefinition -> Yojson.Basic.t
