(* TypeParameterParser.mli - Range-bearing generic parameters and trait references. *)
val parseBoundTrait :
  ParserSupport.parserState ->
  int ->
  WrittenTypes.qualifiedTypeIdentifier option * int

val parseTypeParams :
  ParserSupport.parserState ->
  int ->
  (string * Tokenizer.tokenRange) list * WrittenTypes.typeParamBound list * int
