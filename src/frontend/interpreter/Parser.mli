(* Parser.mli - Recoverable syntax, validated execution input, and diagnostic rendering. *)
val parseExpr : ParserSupport.parserState -> int -> WrittenTypes.expr * int

val parseTokensAt :
  int ->
  ParserSupport.ItemScope.t ->
  Lexer.spannedToken array ->
  ParserSupport.parseResult

val parseTokens : Lexer.spannedToken array -> ParserSupport.parseResult
val parse : string -> ParserSupport.parseResult

val parseFor :
  Validation.mode ->
  string ->
  (Validation.validatedSourceFile, ParserSupport.diagnostic list) result

val parseTestFile : string -> ParserSupport.parseResult
val renderDiagnostic : string -> ParserSupport.diagnostic -> string
