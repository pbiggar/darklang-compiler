(* ExpressionCollections.mli - Grouping, collection literals, and record updates. *)
type parseExpr = ParserSupport.parserState -> int -> WrittenTypes.expr * int
val parseParen : parseExpr -> ParserSupport.parserState -> int -> WrittenTypes.expr * int
val parseList : parseExpr -> ParserSupport.parserState -> int -> WrittenTypes.expr * int
val parseRecord : parseExpr -> ParserSupport.parserState -> WrittenTypes.qualifiedTypeIdentifier -> Tokenizer.tokenRange -> int -> WrittenTypes.expr * int
val parseDict : parseExpr -> ParserSupport.parserState -> Tokenizer.tokenRange -> int -> WrittenTypes.expr * int
val parseRecordUpdate : parseExpr -> ParserSupport.parserState -> int -> WrittenTypes.expr * int
