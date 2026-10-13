(* TraitDeclarationParser.mli - Trait and implementation declaration entry points. *)
val parseTraitDecl : (ParserSupport.parserState -> int -> WrittenTypes.expr * int) -> ParserSupport.parserState -> int -> WrittenTypes.declaration * int
val parseImplDecl : (ParserSupport.parserState -> int -> WrittenTypes.expr * int) -> ParserSupport.parserState -> int -> WrittenTypes.declaration * int
