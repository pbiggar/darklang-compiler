(* ExpressionPrecedence.mli - Original operator binding powers and application grammar. *)
type grammar = {
  parseExpr : ParserSupport.parserState -> int -> WrittenTypes.expr * int;
  parsePrimary : ParserSupport.parserState -> int -> WrittenTypes.expr * int;
}

val infixBindingPower : Tokenizer.token -> (int * bool) option

val parseInfix :
  grammar -> ParserSupport.parserState -> int -> WrittenTypes.expr * int

val parseApp :
  grammar -> ParserSupport.parserState -> int -> WrittenTypes.expr * int

val parseAtom :
  grammar -> ParserSupport.parserState -> int -> WrittenTypes.expr * int

val parseCtorParenFields :
  grammar ->
  ParserSupport.parserState ->
  int ->
  WrittenTypes.expr RevBuffer.t ->
  int

val parsePrefixBuiltin :
  grammar ->
  ParserSupport.parserState ->
  int ->
  string ->
  WrittenTypes.expr * int
