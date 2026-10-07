(* FileParser.mli - File/module item scopes and test assertions. *)
type grammar = {
  parseExpr : ParserSupport.parserState -> int -> WrittenTypes.expr * int;
  parseBlock : ParserSupport.parserState -> int -> WrittenTypes.expr * int;
}

val parseFile :
  grammar ->
  ParserSupport.ItemScope.t ->
  ParserSupport.parserState ->
  ParserSupport.parseResult
