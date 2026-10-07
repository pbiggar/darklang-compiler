(* ParserSupport.ml - Preserve the parser's explicit state and scoped restoration. *)
open Tokenizer
open Lexer
module WT = WrittenTypes
(*
   Stable diagnostic codes — documented in GRAMMAR.md; editors/tests key on
   these, never on the message text.
   expected X, found Y
   missing closing delimiter (opener in `related`)
   invalid escape/codepoint in a literal
   integer literal out of range
   nesting beyond the recursion cap
   stray token inside a construct
   pipe RHS isn't a valid segment
   invalid match pattern shape
   unknown effect name in a `:{…}` row
   malformed interpolation body/braces
   parser step budget exhausted (parser bug)
   tokenizer-level recovery (unterminated literal, …)
*)
type diagnosticSeverity = DiagError | DiagWarning
module DiagnosticCode = struct
  let expected = "PARSE-EXPECTED"
  let unclosed = "PARSE-UNCLOSED"
  let escape = "PARSE-ESCAPE"
  let intRange = "PARSE-INT-RANGE"
  let tooDeep = "PARSE-TOO-DEEP"
  let unexpected = "PARSE-UNEXPECTED"
  let pipeSegment = "PARSE-PIPE-SEGMENT"
  let pattern = "PARSE-PATTERN"
  let effect_ = "PARSE-EFFECT"
  let interpolation = "PARSE-INTERPOLATION"
  let internalLoop = "PARSE-INTERNAL-LOOP"
  let lex = "LEX"
end
(*
   one of DiagnosticCode — stable across releases
   secondary locations, e.g. the opening delimiter of an unclosed pair
*)
type diagnostic = { code : string; severity : diagnosticSeverity; range : tokenRange;
  message : string; related : (tokenRange * string) list; hint : string option }
type parseResult = { parsed : WT.parsedFile option; diagnostics : diagnostic list }
(*
   One offside scope: the current statement's anchor column (`stmtCol`; -1 = none)
   and `stmtExact`, a flag marking a parenthesized body — there the closing `)`
   is the real delimiter, so only a token at EXACTLY the anchor column starts a
   new statement; dedented continuations (`|> (fn\n  args-below-callee)`) stay
   part of the current one. Constructs enter scopes only through the `with*`
   helpers in `parseTokens`, so prior state is restored by construction.
*)
type offsideScope = { mutable stmtCol : int; mutable stmtExact : bool }
(*
   All parser state, threaded explicitly through every parse function (no
   closure): the token stream, the diagnostics sink, and the offside/recovery
   registers. One value per parse; `parseTokens` constructs it.
   the token stream being parsed
   = toks.Length, cached (read on every bounds check)
   parse errors collected during recovery
   offside anchor stack (a frame per let/if/match/paren body)
   `|` is both bitwise-or and the match-arm separator. Each anchor records an
   enclosing match's arm row and column so `barStartsArm` leaves arm-position
   bars for `parseMatch`. Regions where an arm cannot start clear the anchors.
   Closing a nested generic like `Dict<List<Int>>` ends in `>>`, which the
   lexer produces as ONE token but which must close TWO levels. Closing the
   inner `List<Int>` uses only the first `>`, so the second is "left over"
   for the outer `Dict<…>` to close. `pendingGt` counts those left-over `>`s;
   `pendingGtRange` is the source range of the next one to spend.
   start column of the declaration being parsed (-1 = none); a token at-or-left ends the construct
   current recursion depth — stack-overflow guard (see `maxDepth`)
   a guard aborted the parse; silences the unwind's cascade of secondary diagnostics
   parseExpr/pattern/type entry count — the no-progress backstop (see
   `outOfFuel`); any runaway loop exhausts it and abandons with a
   diagnostic instead of hanging
   How deeply string interpolations are nested. Each `{expr}` body is parsed
   by a FRESH recursive `parseTokens`, whose own `depth` guard restarts at 0 —
   so `depth` can't see an interpolation bomb like `$"{$"{$"…"}"}"`, where the
   nesting is in the chain of recursive parses, not one deep expression.
   Threaded as parent + 1 and capped at `maxInterpNesting` to stay stack-safe.
*)
type parserState = {
  toks : spannedToken array; tokenCount : int; diagnostics : diagnostic list ref;
  scopes : offsideScope Stack.t; mutable matchArms : (int * int) list;
  mutable pendingGt : int; mutable pendingGtRange : tokenRange;
  mutable declAnchor : int; mutable depth : int; mutable abandoned : bool;
  mutable steps : int; interpDepth : int;
}
module ItemScope = struct type t = Script | Module end
let makeState interpDepth toks =
  let scopes = Stack.create () in
  Stack.push {stmtCol = -1; stmtExact = false} scopes;
  {toks; tokenCount = Array.length toks; diagnostics = ref []; scopes; matchArms = [];
   pendingGt = 0; pendingGtRange = WT.synthRange; declAnchor = -1; depth = 0;
   abandoned = false; steps = 0; interpDepth}
let diagnosticOfValidationIssue (issue : Validation.issue) =
  {code = Validation.IssueCode.toString issue.Validation.code; severity = DiagError;
   range = issue.Validation.range; message = issue.Validation.message;
   related = issue.Validation.related; hint = issue.Validation.hint}
let infixOf = function
  | TPlus -> Some (WT.InfixFnCall WT.ArithmeticPlus)
  | TMinus -> Some (WT.InfixFnCall WT.ArithmeticMinus)
  | TStar -> Some (WT.InfixFnCall WT.ArithmeticMultiply)
  | TSlash -> Some (WT.InfixFnCall WT.ArithmeticDivide)
  | TPercent -> Some (WT.InfixFnCall WT.ArithmeticModulo)
  | TPlusPlus -> Some (WT.InfixFnCall WT.StringConcat)
  | TEqEq -> Some (WT.InfixFnCall WT.ComparisonEquals)
  | TNeq -> Some (WT.InfixFnCall WT.ComparisonNotEquals)
  | TLt -> Some (WT.InfixFnCall WT.ComparisonLessThan)
  | TGt -> Some (WT.InfixFnCall WT.ComparisonGreaterThan)
  | TLte -> Some (WT.InfixFnCall WT.ComparisonLessThanOrEqual)
  | TGte -> Some (WT.InfixFnCall WT.ComparisonGreaterThanOrEqual)
  | TAnd -> Some (WT.BinOp WT.BinOpAnd) | TOr -> Some (WT.BinOp WT.BinOpOr)
  | TStarStar -> Some (WT.InfixFnCall WT.ArithmeticPower)
  | TBitAnd -> Some (WT.InfixFnCall WT.BitwiseAnd)
  | TBar -> Some (WT.InfixFnCall WT.BitwiseOr)
  | TBitXor -> Some (WT.InfixFnCall WT.BitwiseXor)
  | TShl -> Some (WT.InfixFnCall WT.ShiftLeft) | TShr -> Some (WT.InfixFnCall WT.ShiftRight)
  | _ -> None
  [@@warning "-4"]
(*
   The one definition of "an integer-literal token" — `canStartAtom` and
   `canStartPattern` both build on it so the lists can't drift from each other.
   Matched EXHAUSTIVELY (no `_`): a new `Token` case won't compile until it's
   classified here, so a new integer type can't silently fall through to `false`
   — the drift that once made `| Ok 5y ->` unparseable while `Ok 5y` worked.
*)
let isIntLit = function
  | TInt _ | TInt64 _ | TInt8 _ | TUInt8 _ | TInt16 _ | TUInt16 _ | TInt32 _
  | TUInt32 _ | TUInt64 _ | TInt128 _ | TUInt128 _ -> true
  | TFloat _ | TStringLit _ | TCharLit _ | TInterpString | TTrue | TFalse
  | TPlus | TPlusPlus | TMinus | TStar | TStarStar | TSlash | TLParen | TRParen
  | TLet | TVal | TIn | TIf | TElif | TThen | TElse | TType | TCons | TColon
  | TComma | TSemicolon | TDot | TLBrace | TRBrace | TBar | TOf | TMatch | TWith
  | TFun | TArrow | TUnderscore | TWhen | TLBracket | TRBracket | TEquals | TEqEq
  | TNeq | TLt | TGt | TLte | TGte | TAnd | TOr | TNot | TPipe | TDotDotDot
  | TPercent | TShl | TShr | TBitAnd | TBitXor | TBitNot | TAt | TIdent _ | TEOF -> false
(*
   a record/anonymous-record/update `{ … }` can be a function argument, e.g.
   `parseArgs tail { acc with port = p }`
   prefix `!`/`~`: `f !x` is `f (!x)`. Unambiguous — neither token has an
   infix reading, and `!=` lexes as one token, so `a != b` is untouched.
*)
let canStartAtom token = isIntLit token ||
  List.mem token [TTrue; TFalse; TInterpString; TLParen; TLBracket; TLBrace; TNot; TBitNot] ||
  (match token with TFloat _ | TCharLit _ | TStringLit _ | TIdent _ -> true | _ -> false)
  [@@warning "-4"]
(*
   `TMinus` is included so a negative-literal enum-pattern field (`| Ok -4y ->`)
   parses; parsePatternBase's TMinus case handles it.
*)
let canStartPattern token = isIntLit token ||
  List.mem token [TUnderscore; TMinus; TTrue; TFalse; TLParen; TLBracket] ||
  (match token with TFloat _ | TCharLit _ | TStringLit _ | TIdent _ -> true | _ -> false)
  [@@warning "-4"]
(*
   tokens that close/separate a block — another statement can't start with these
*)
let closesOrSeparates token = List.mem token [TRParen; TRBracket; TRBrace; TSemicolon; TComma; TIn; TThen; TElse; TWith; TArrow; TEOF]
(*
   tokens hole-recovery must never consume: closing/separating tokens (the enclosing
   construct needs them to close cleanly) and declaration starters (the next
   declaration must survive a broken one before it)
*)
let isRecoveryBarrier token = closesOrSeparates token || List.mem token [TLet; TType; TVal]
let maxDepth = 512
let tok state index = if index < state.tokenCount then state.toks.(index).token else TEOF
let rng state index = if index < state.tokenCount then state.toks.(index).range else state.toks.(state.tokenCount - 1).range
(*
   raw source text of the token (e.g. a type variable `'a` lexes to an
   ident "a" but its text keeps the leading tick)
*)
let txt state index = if index < state.tokenCount then state.toks.(index).text else ""
(*
   `///` doc comment attached to the token at `i` (a declaration keyword), if any
*)
let docOf state index = if index < state.tokenCount then Option.value ~default:"" state.toks.(index).docComment else ""
(*
   Zero-width range at `r`'s end: the range for a synthetic/missing node (an
   absent `>`, a bare tuple's missing parens, an unsplit int suffix) — points at
   "where it should be" without claiming any real source characters.
*)
let zeroWidthAtEnd range = {start = range.end_; end_ = range.end_}
let span first last = {start = first.start; end_ = last.end_}
let advancePos position text count =
  let chars = Text.scalars text in
  let current = ref position in
  for index = 0 to min count (Array.length chars) - 1 do
    current := if chars.(index) = 10 then {row = !current.row + 1; column = 0} else {!current with column = !current.column + 1}
  done;
  !current
let splitTrailingRange state index trailingLength =
  let whole = rng state index and text = txt state index in
  let boundary = advancePos whole.start text (max 0 (Text.length text - trailingLength)) in
  {start = whole.start; end_ = boundary}, {start = boundary; end_ = whole.end_}
let literalTextRanges state index delimiter =
  let whole = rng state index and text = txt state index in
  let length = Text.length text and delimiterLength = Text.length delimiter in
  let hasClose = length >= delimiterLength * 2 && String.ends_with ~suffix:delimiter text in
  let openEnd = advancePos whole.start text (min delimiterLength length) in
  let contentEnd = advancePos whole.start text (if hasClose then length - delimiterLength else length) in
  let closeEnd = if hasClose then whole.end_ else contentEnd in
  {start = whole.start; end_ = openEnd}, {start = openEnd; end_ = contentEnd}, {start = contentEnd; end_ = closeEnd}
(*
   set when a guard abandons the parse: suppresses the cascade of secondary
   diagnostics from the unwinding frames
*)
let errFull state code index message related hint =
  if not state.abandoned then
    state.diagnostics := {code; severity = DiagError; range = rng state index; message; related; hint} :: !(state.diagnostics)
let err state code index message = errFull state code index message [] None
(*
   what the parser is looking at, for "expected X, found Y" messages
*)
let foundDesc state index =
  if index >= state.tokenCount || tok state index = TEOF then "end of file" else
  let text = String.concat "\\n" (String.split_on_char '\n' (txt state index)) in
  let chars = Text.scalars text in
  if Array.length chars > 24 then "'" ^ Text.ofScalars (Array.sub chars 0 24) ^ "…'" else "'" ^ text ^ "'"
let errExpected state index expected = err state DiagnosticCode.expected index ("expected " ^ expected ^ ", found " ^ foundDesc state index)
(*
   a missing closing delimiter: point back at its opener
*)
let errUnclosed state index closing opening openRange =
  errFull state DiagnosticCode.unclosed index
    (Printf.sprintf "expected '%s' to close the '%s' at line %d:%d, found %s" closing opening (openRange.start.row + 1) (openRange.start.column + 1) (foundDesc state index))
    [openRange, "the '" ^ opening ^ "' opened here"] None
(*
   --- recursion-depth guard ---
   parseExpr/parseTypeRef/parsePatternBase recurse per nesting level, so a
   pathological `((((…` can overflow the stack before diagnostics are returned.
   At the cap we diagnose once and skip to
   EOF. Generated E2E batches can contain several hundred nested `let`
   bindings, so the cap must permit those while still guarding against a
   runaway recursive parse.
   No-progress backstop: each parseExpr/parsePatternBase/parseTypeRef entry
   spends one step. A real parse of n tokens uses ≪ 300·n (corpus-measured); a
   loop that stops consuming tokens spends them forever — so exhaustion means a
   parser bug, and we abandon with a diagnostic instead of hanging the host.
*)
let outOfFuel state index =
  state.steps <- state.steps + 1;
  if state.steps <= 4000 + state.tokenCount * 300 then false else begin
    errFull state DiagnosticCode.internalLoop index
      "internal parser error: step budget exhausted (parser loop?); parsing abandoned" []
      (Some "please report this — it indicates a parser bug, not a problem with your code");
    state.abandoned <- true; true
  end
let tooDeep state index =
  if state.depth < maxDepth then false else begin
    errFull state DiagnosticCode.tooDeep index
      (Printf.sprintf "nesting too deep (over %d levels); parsing abandoned" maxDepth) []
      (Some "split the expression with intermediate `let` bindings");
    state.abandoned <- true; true
  end
(*
   A sized-int literal whose magnitude is the type's |MinValue| lexes to
   MinValue (so the NEGATED literal can exist: `-128y`); consumed WITHOUT the
   minus, the written magnitude is out of range — diagnose instead of silently
   wrapping (`128y` is NOT -128). Only the negating TMinus branches consume
   these tokens without passing through here.
*)
let checkBareMinMagnitude state index =
  let minimum = match tok state index with
    | TInt8 value -> value = -128 | TInt16 value -> value = -32768
    | TInt32 value -> value = Int32.min_int | TInt64 value -> value = Int64.min_int
    | TInt128 value -> Z.equal value (Z.neg (Z.shift_left Z.one 127))
    | _ -> false in
  if minimum then
    let text = txt state index in
    errFull state DiagnosticCode.intRange index
      ("integer literal " ^ text ^ " is out of range (this magnitude is only valid negated: -" ^ text ^ ")") [] (Some ("write it negated: -" ^ text))
  [@@warning "-4"]
(*
   whole/fraction decimal strings of a float literal at token `i`. The
   double's shortest round-trip form is used when it's a plain decimal (the
   usual case); when it needs an exponent (`1e300`, `0.00000001`) or isn't
   finite-decimal at all, the SOURCE text is decimal-shifted instead — the
   PT float representation is exponent-free strings, and an exponent leaking
   into the whole part crashes `makeFloat` downstream.
   decimal-shift the literal text `mant[.frac][eE][+-]exp` (exact, no
   floating-point re-derivation)
*)
let floatParts state index value =
  let roundTrip = FloatFormat.roundTrip (Float.abs value) in
  if String.for_all (fun char -> (char >= '0' && char <= '9') || char = '.') roundTrip then
    match String.split_on_char '.' roundTrip with [whole; fraction] -> whole, fraction | _ -> roundTrip, "0"
  else
    let text = txt state index in
    let rec leadingMinus offset = if offset < String.length text && text.[offset] = '-' then leadingMinus (offset + 1) else offset in
    let offset = leadingMinus 0 in
    let text = String.sub text offset (String.length text - offset) in
    let parts = String.split_on_char 'e' (String.map (fun char -> if char = 'E' then 'e' else char) text) in
    let mantissa, exponent = match parts with
      | [mantissa; exponent] ->
          let exponent = match Text.tryParseInt32 exponent with
            | Some value when value >= -400l && value <= 400l -> Int32.to_int value
            | Some value -> err state DiagnosticCode.intRange index
                (Printf.sprintf "Float exponent %ld is outside the supported range -400..400" value);
                max (-400) (min 400 (Int32.to_int value))
            | None -> err state DiagnosticCode.intRange index "Float exponent is too large to represent"; 0 in
          mantissa, exponent
      | _ -> text, 0 in
    let whole, fraction = match String.split_on_char '.' mantissa with [whole; fraction] -> whole, fraction | _ -> mantissa, "" in
    let digits = whole ^ fraction and point = String.length whole + exponent in
    if point <= 0 then "0", String.make (-point) '0' ^ digits
    else if point >= String.length digits then digits ^ String.make (point - String.length digits) '0', "0"
    else String.sub digits 0 point, String.sub digits point (String.length digits - point)
(*
   Reject invalid escapes / codepoints in string, char, and interpolated-string
   literals (triple-quoted forms are raw, so skipped). A diagnostic here becomes a
   `ParseError.Message` — the escape is otherwise silently error-recovered.
*)
let stripDelims raw lead close =
  let start = if String.starts_with ~prefix:lead raw then String.length lead else 0 in
  let stop = if String.length raw > start && String.ends_with ~suffix:close raw then String.length raw - String.length close else String.length raw in
  if stop > start then String.sub raw start (stop - start) else ""
let validateLiterals state =
  for index = 0 to state.tokenCount - 1 do
    let text = txt state index in
    match state.toks.(index).token with
    | TStringLit _ when not (String.starts_with ~prefix:"\"\"\"" text) ->
        if Lexer.hasInvalidEscape (stripDelims text "\"" "\"") then err state DiagnosticCode.escape index "Invalid escape sequence or codepoint in string literal"
    | TCharLit value ->
        if Lexer.hasInvalidEscape (stripDelims text "'" "'") then err state DiagnosticCode.escape index "Invalid escape sequence or codepoint in character literal";
        if List.length (Text.graphemeClusters value) <> 1 then err state DiagnosticCode.escape index "Character literal must contain exactly one grapheme"
    | TInterpString when not (String.starts_with ~prefix:"$\"\"\"" text) ->
        let inner = stripDelims text "$\"" "\"" in
        if Lexer.hasInvalidEscapeInterp inner then err state DiagnosticCode.escape index "Invalid escape sequence or codepoint in interpolated string";
        if Lexer.hasSingleCloseBraceInterp inner false then err state DiagnosticCode.interpolation index "Single '}' in interpolated string text; use '}}' or '\\}' for a literal brace"
    | TInterpString ->
        let inner = stripDelims text "$\"\"\"" "\"\"\"" in
        if Lexer.hasSingleCloseBraceInterp inner true then err state DiagnosticCode.interpolation index "Single '}' in raw interpolated string text; use '}}' for a literal brace"
    | _ -> ()
  done
  [@@warning "-4"]
(*
   qualified name: ident (. ident)*  → (modules, finalIdent, nextIndex)
   only step into `.seg` as a module path when the CURRENT segment is
   uppercase (a module); a lowercase ident's `.field` is postfix access,
   left for parsePostfix to handle.
   previous segment becomes a module, with the dot range
   Matches a written type name against the primitive types.
*)
let parseQualified state index =
  let first : WT.identifier = match tok state index with
    | TIdent name -> {WT.range = rng state index; name}
    | _ -> errExpected state index "an identifier"; {WT.range = rng state index; name = "_"} in
  let rec scan reversed (current : WT.identifier) next =
    let chars = Text.scalars current.WT.name in
    let upper = Array.length chars > 0 && Text.isUpper chars.(0) in
    match tok state next, tok state (next + 1) with
    | TDot, TIdent name when upper -> scan ((current, rng state next) :: reversed) {WT.range = rng state (next + 1); name} (next + 2)
    | _ -> List.rev reversed, current, next in
  scan [] first (index + 1)
  [@@warning "-4"]
(*
   `>>` lexes as one TShr token but closes two generic levels. `state.pendingGt`
   carries the leftover `>` to the enclosing type so `List<List<T>>` parses.
   Close one generic level (`>`), splitting a `>>` (TShr) into one consumed `>`
   and one pending. Returns (close-`>` range, next index).
*)
let expectGt state index =
  if state.pendingGt > 0 then begin state.pendingGt <- state.pendingGt - 1; state.pendingGtRange, index end
  else if tok state index = TGt then rng state index, index + 1
  else if tok state index = TShr then begin
    let range = rng state index in
    let middle = {row = range.start.row; column = range.start.column + 1} in
    state.pendingGtRange <- {start = middle; end_ = range.end_};
    state.pendingGt <- state.pendingGt + 1;
    {start = range.start; end_ = middle}, index + 1
  end else begin errExpected state index "'>'"; zeroWidthAtEnd (rng state index), index end
(*
   Skip a `< … >` type-argument list (not modelled yet); a trailing `>>` leaves
   one `>` pending for the enclosing generic.
   declaration type parameters `<'a, 'b>` — collect the (tick-stripped) names
   so generic types/fns keep their params (needed for runtime type unification).
*)
let parseTypeParams state index =
  if tok state index <> TLt then [], index else begin
    if index > 0 && ((rng state index).start.row <> (rng state (index - 1)).end_.row || (rng state index).start.column <> (rng state (index - 1)).end_.column) then
      err state DiagnosticCode.expected index "Generic type parameters must be adjacent to the declaration name";
    let names = ref [] and next = ref (index + 1) and expectingName = ref true in
    while not (List.mem (tok state !next) [TGt; TShr; TEOF]) do
      match !expectingName, tok state !next with
      | true, TIdent name ->
          if not (String.starts_with ~prefix:"'" (txt state !next)) then err state DiagnosticCode.expected !next "Declared type parameters must start with an apostrophe, such as 'a";
          names := (name, rng state !next) :: !names; expectingName := false; incr next
      | false, TComma -> expectingName := true; incr next
      | false, TIdent _ -> errExpected state !next "a comma between type parameters"; expectingName := true
      | _ -> errExpected state !next "a type parameter"; incr next
    done;
    if !names = [] then errExpected state (index + 1) "at least one type parameter"
    else if !expectingName then errExpected state !next "a type parameter after ','";
    let stop = if List.mem (tok state !next) [TGt; TShr] then !next + 1 else begin errExpected state !next "'>' to close the type-parameter list"; !next end in
    List.rev !names, stop
  end
  [@@warning "-4"]
(*
   --- offside scope stack ---
   One scope = `stmtCol` (the current statement's anchor column; -1 = none) +
   `stmtExact` (a parenthesized body: only a token at EXACTLY the anchor column
   starts a new statement — the `)` is the real delimiter). let/if/match push a
   fresh scope so their sub-expressions keep normal offside (a let value must
   not swallow the next statement). All state transitions go through the
   `with*` helpers below, which restore the prior scope by construction — a
   leftover flag can't leak into what follows.
   anchor the current scope's statement column (per statement / element)
*)
let setStmtCol state column = (Stack.top state.scopes).stmtCol <- column
(*
   fresh sub-statement scope, statement anchored at `col`
*)
let withStmtScope state column run =
  Stack.push {stmtCol = column; stmtExact = false} state.scopes;
  Fun.protect run ~finally:(fun () -> ignore (Stack.pop state.scopes))
(*
   Would a `|` at `i` start a match arm rather than continue an expression?
   True when it sits on the arm row or at the arm column of ANY enclosing
   match -- a `|` less indented than the innermost match belongs to an outer
   one, and must not be eaten as an operator either.
*)
let barStartsArm state index =
  match state.matchArms with
  | [] -> false
  | arms ->
      let range = rng state index in
      List.exists (fun (row, column) -> range.start.row = row || range.start.column = column) arms
(*
   Run `f` without enclosing match-arm anchors, where `|` can only be an operator.
*)
let withoutMatchArms state run =
  let saved = state.matchArms in state.matchArms <- [];
  Fun.protect run ~finally:(fun () -> state.matchArms <- saved)
(*
   fresh scope; statement anchor inherited (managed per-element by `f`)
*)
let withElementScope state run =
  Stack.push {stmtCol = (Stack.top state.scopes).stmtCol; stmtExact = false} state.scopes;
  Fun.protect (fun () -> withoutMatchArms state run) ~finally:(fun () -> ignore (Stack.pop state.scopes))
(*
   re-anchor the statement column within the CURRENT scope as a
   parenthesized (exact-column) anchor, restoring both after
*)
let withStmtColExact state column run =
  let scope = Stack.top state.scopes in
  let savedColumn = scope.stmtCol and savedExact = scope.stmtExact in
  scope.stmtCol <- column; scope.stmtExact <- true;
  Fun.protect (fun () -> withoutMatchArms state run) ~finally:(fun () -> scope.stmtCol <- savedColumn; scope.stmtExact <- savedExact)
(*
   Column of the DECLARATION currently being parsed (set per item by
   parseItems), or -1. Recovery only: a decl-start keyword at or left of this
   column can never belong to a construct inside the declaration, so an
   unclosed delimiter above must stop instead of swallowing the next
   declaration (`let broken = [1L;` must not eat the `let fine …` below it).
*)
let declBarrier state index =
  List.mem (tok state index) [TLet; TType; TVal] && state.declAnchor >= 0 && (rng state index).start.column <= state.declAnchor
(*
   Offside: a trailing operand (application arg, enum-constructor/pattern field)
   at `k` continues the construct started at `headIdx` only if it's on the same
   line as the head, or indented further (or we're inside parens). A token on a
   new line at the same-or-lower indent starts a new statement.
*)
let offsideContinues state head index =
  let scope = Stack.top state.scopes in
  (rng state index).start.row = (rng state head).start.row || (rng state index).start.column > (rng state head).start.column ||
  (scope.stmtCol >= 0 && if scope.stmtExact then (rng state index).start.column <> scope.stmtCol else (rng state index).start.column > scope.stmtCol)
(*
   A `-` GLUED to a following number, with a space before it, is a negative-literal
   ARGUMENT (`f a -1`), not subtraction (`f a - 1` = `(f a) - 1`). The application
   arg loop accepts it so `Float.multiply a -1.0` / `add 5L -1L` parse correctly
   (application binds more tightly than infix operations).
   no space after `-`
   space before `-`
*)
let isNegLitArg state index =
  let signed = match tok state (index + 1) with TInt _ | TInt64 _ | TInt8 _ | TInt16 _ | TInt32 _ | TInt128 _ | TFloat _ -> true | _ -> false in
  tok state index = TMinus && signed &&
  (rng state (index + 1)).start.row = (rng state index).end_.row && (rng state (index + 1)).start.column = (rng state index).end_.column &&
  (index = 0 || (rng state index).start.row <> (rng state (index - 1)).end_.row || (rng state index).start.column > (rng state (index - 1)).end_.column)
  [@@warning "-4"]
(*
   `;` used to separate list elements and list-pattern elements; `,` is the only
   separator now. Report it precisely and keep parsing as if it had been a `,`, so
   a file written in the old style yields one diagnostic per `;` instead of a
   cascade of recovery noise from the elements after it.
*)
let errListSemicolon state index what =
  errFull state DiagnosticCode.expected index ("expected ',' between " ^ what ^ ", found ';'") [] (Some "use ',' instead")
let requireElementSeparator state previousRange next expected =
  if tok state next <> TEOF && (rng state next).start.row <= previousRange.end_.row then errExpected state next expected
(*
   The effect names a `:{…}` row may use: the `Effects.Effect` case names.
*)
let effectCaseNames = List.map LibExecution_Effects.caseName LibExecution_Effects.all
