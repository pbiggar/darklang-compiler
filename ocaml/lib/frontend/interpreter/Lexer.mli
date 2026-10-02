(* Lexer.mli - Range-complete tokens, trivia, recovery, and shared string scans. *)
type triviaKind = LineComment | DocComment | BlockComment
type trivia = { kind : triviaKind; text : string; range : Tokenizer.tokenRange }
type spannedToken = {
  token : Tokenizer.token;
  text : string;
  range : Tokenizer.tokenRange;
  docComment : string option;
  leadingTrivia : trivia list;
}
val unescape : string -> string
val hasInvalidEscape : string -> bool
val findInterpExprClose : string -> int -> int -> int
val hasInvalidEscapeInterp : string -> bool
val hasSingleCloseBraceInterp : string -> bool -> bool
val maxInterpNesting : int
val tokenize : string -> (spannedToken list * (Tokenizer.tokenRange * string) list, string) result
