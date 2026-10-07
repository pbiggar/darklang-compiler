(* PatternParser.mli - Original match-pattern precedence and recovery rules. *)
val parseMatchPattern :
  ParserSupport.parserState -> int -> WrittenTypes.matchPattern * int

val parsePatternOr :
  ParserSupport.parserState -> int -> WrittenTypes.matchPattern * int

val parsePatternBase :
  ParserSupport.parserState -> int -> WrittenTypes.matchPattern * int
