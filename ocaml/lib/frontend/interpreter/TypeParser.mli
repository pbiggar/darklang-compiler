(* TypeParser.mli - Source type grammar, generic closers, and exact recovery. *)
val parseTypeRef : ParserSupport.parserState -> int -> WrittenTypes.typeReference * int
val parseAtomType : ParserSupport.parserState -> int -> WrittenTypes.typeReference * int
val parseTypeArgs : ParserSupport.parserState -> int -> WrittenTypes.typeReference list * Tokenizer.tokenRange option * int
