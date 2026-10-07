(* ExpressionInterpolation.mli - Range-aware embedded expression parsing. *)
type parseTokensAt =
  int ->
  ParserSupport.ItemScope.t ->
  Lexer.spannedToken array ->
  ParserSupport.parseResult

val parseInterpString :
  parseTokensAt -> ParserSupport.parserState -> int -> WrittenTypes.expr * int
