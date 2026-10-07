(* DeclarationSupport.mli - Typed parameters and closed effect-row parsing. *)
val parseParam : ParserSupport.parserState -> int -> WrittenTypes.fnParam * int
val parseEffectRow : ParserSupport.parserState -> int -> WrittenTypes.identifier list option * int
