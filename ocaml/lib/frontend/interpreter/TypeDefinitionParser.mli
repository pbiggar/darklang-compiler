(* TypeDefinitionParser.mli - Alias, record, and enum declaration syntax. *)
val parseTypeDecl : ParserSupport.parserState -> int -> WrittenTypes.declaration * int
val parseTypeDefinition : ParserSupport.parserState -> int -> WrittenTypes.typeDefinition * int
