(* ExpressionPrimary.mli - Primary expressions with recursive grammar callbacks. *)
type grammar = {
  parseExpr : ParserSupport.parserState -> int -> WrittenTypes.expr * int;
  parseBlock : ParserSupport.parserState -> int -> WrittenTypes.expr * int;
  parseApp : ParserSupport.parserState -> int -> WrittenTypes.expr * int;
  parseCtorParenFields :
    ParserSupport.parserState -> int -> WrittenTypes.expr RevBuffer.t -> int;
  parseInterpString :
    ParserSupport.parserState -> int -> WrittenTypes.expr * int;
}

val parsePrimary :
  grammar -> ParserSupport.parserState -> int -> WrittenTypes.expr * int
