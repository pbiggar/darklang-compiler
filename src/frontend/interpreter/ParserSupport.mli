(* ParserSupport.mli - Shared parser state, exact ranges, recovery, and layout. *)
type diagnosticSeverity = DiagError | DiagWarning

module DiagnosticCode : sig
  val expected : string
  val unclosed : string
  val escape : string
  val intRange : string
  val tooDeep : string
  val unexpected : string
  val pipeSegment : string
  val pattern : string
  val effect_ : string
  val interpolation : string
  val internalLoop : string
  val lex : string
end

type diagnostic = {
  code : string;
  severity : diagnosticSeverity;
  range : Tokenizer.tokenRange;
  message : string;
  related : (Tokenizer.tokenRange * string) list;
  hint : string option;
}

type parseResult = {
  parsed : WrittenTypes.parsedFile option;
  diagnostics : diagnostic list;
}

type offsideScope = { mutable stmtCol : int; mutable stmtExact : bool }

type parserState = {
  toks : Lexer.spannedToken array;
  tokenCount : int;
  diagnostics : diagnostic list ref;
  scopes : offsideScope Stack.t;
  mutable matchArms : (int * int) list;
  mutable pendingGt : int;
  mutable pendingGtRange : Tokenizer.tokenRange;
  mutable declAnchor : int;
  mutable depth : int;
  mutable abandoned : bool;
  mutable steps : int;
  interpDepth : int;
}

module ItemScope : sig
  type t = Script | Module
end

val makeState : int -> Lexer.spannedToken array -> parserState
val diagnosticOfValidationIssue : Validation.issue -> diagnostic
val infixOf : Tokenizer.token -> WrittenTypes.infix option
val isIntLit : Tokenizer.token -> bool
val canStartAtom : Tokenizer.token -> bool
val canStartPattern : Tokenizer.token -> bool
val closesOrSeparates : Tokenizer.token -> bool
val isRecoveryBarrier : Tokenizer.token -> bool
val maxDepth : int
val tok : parserState -> int -> Tokenizer.token
val rng : parserState -> int -> Tokenizer.tokenRange
val txt : parserState -> int -> string
val docOf : parserState -> int -> string
val zeroWidthAtEnd : Tokenizer.tokenRange -> Tokenizer.tokenRange
val span : Tokenizer.tokenRange -> Tokenizer.tokenRange -> Tokenizer.tokenRange
val advancePos : Tokenizer.pos -> string -> int -> Tokenizer.pos

val splitTrailingRange :
  parserState -> int -> int -> Tokenizer.tokenRange * Tokenizer.tokenRange

val literalTextRanges :
  parserState ->
  int ->
  string ->
  Tokenizer.tokenRange * Tokenizer.tokenRange * Tokenizer.tokenRange

val errFull :
  parserState ->
  string ->
  int ->
  string ->
  (Tokenizer.tokenRange * string) list ->
  string option ->
  unit

val err : parserState -> string -> int -> string -> unit
val foundDesc : parserState -> int -> string
val errExpected : parserState -> int -> string -> unit

val errUnclosed :
  parserState -> int -> string -> string -> Tokenizer.tokenRange -> unit

val outOfFuel : parserState -> int -> bool
val tooDeep : parserState -> int -> bool
val checkBareMinMagnitude : parserState -> int -> unit
val floatParts : parserState -> int -> float -> string * string
val stripDelims : string -> string -> string -> string
val validateLiterals : parserState -> unit

val parseQualified :
  parserState ->
  int ->
  (WrittenTypes.identifier * Tokenizer.tokenRange) list
  * WrittenTypes.identifier
  * int

val expectGt : parserState -> int -> Tokenizer.tokenRange * int

val parseTypeParams :
  parserState -> int -> (string * Tokenizer.tokenRange) list * int

val setStmtCol : parserState -> int -> unit
val withStmtScope : parserState -> int -> (unit -> 'a) -> 'a
val barStartsArm : parserState -> int -> bool
val withoutMatchArms : parserState -> (unit -> 'a) -> 'a
val withElementScope : parserState -> (unit -> 'a) -> 'a
val withStmtColExact : parserState -> int -> (unit -> 'a) -> 'a
val declBarrier : parserState -> int -> bool
val offsideContinues : parserState -> int -> int -> bool
val isNegLitArg : parserState -> int -> bool
val errListSemicolon : parserState -> int -> string -> unit

val requireElementSeparator :
  parserState -> Tokenizer.tokenRange -> int -> string -> unit

val effectCaseNames : string list
