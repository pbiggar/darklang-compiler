(* DeclarationParser.mli - Function and value declarations with recursive bodies. *)
val parseDecl : (ParserSupport.parserState -> int -> WrittenTypes.expr * int) -> ParserSupport.parserState -> int -> WrittenTypes.declaration * int
