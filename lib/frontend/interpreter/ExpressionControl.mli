(* ExpressionControl.mli - Recursive control grammar supplied with expression precedence. *)
type grammar = {
  parseExpr : ParserSupport.parserState -> int -> WrittenTypes.expr * int;
  parseInfix : ParserSupport.parserState -> int -> WrittenTypes.expr * int;
}
val parseExpr : grammar -> ParserSupport.parserState -> int -> WrittenTypes.expr * int
val parseBlock : grammar -> ParserSupport.parserState -> int -> WrittenTypes.expr * int
val toPipeExpr : WrittenTypes.expr -> WrittenTypes.pipeExpr option
