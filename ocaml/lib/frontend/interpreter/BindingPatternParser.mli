(* BindingPatternParser.mli - Variable, unit, wildcard, and tuple binding grammar. *)
val parseLetPattern : ParserSupport.parserState -> int -> WrittenTypes.letPattern * int
