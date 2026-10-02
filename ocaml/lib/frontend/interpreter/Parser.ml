(*
   The hand-written parser: source → range-complete `WrittenTypes` tree, capturing
   fine-grained keyword/symbol/operator ranges (not just node spans) for the highlighter /
   LSP. Recovers from errors, returning diagnostics alongside a best-effort tree.
   Pos, TokenRange, Token
   SpannedToken, tokenize
*)
(* Parser.ml - Assemble the frozen grammar and enforce post-syntax validation. *)
[@@@warning "-4"]
open Tokenizer
open ParserSupport
module WT = WrittenTypes
(*
   anchor arg-offside at this construct's column so a let value / if cond /
   match expr doesn't grab the following body (`let x = v\n body`)
*)
let rec parseExpr state index =
  ExpressionControl.parseExpr {ExpressionControl.parseExpr; parseInfix} state index
and parseInfix state index =
  ExpressionPrecedence.parseInfix {ExpressionPrecedence.parseExpr; parsePrimary} state index
(*
   space application: `f a b`
   A spaced enum ctor followed by a parenthesized list is a FIELD list, not a
   single tuple arg: `KeyPressed (a, b, c)` → 3 fields.
   The content is parsed as a comma list, so double parens `Ctor ((a, b))` keep the
   tuple as ONE field (no top-level comma). Only fires in head position (the ctor IS
   the callee), so `f None (g)` is unaffected. Adjacent `Ctor(a,b)` was already
   handled in the enum branch, so a nullary EEnum callee here means a spaced paren.
   only names/applications take args
   a field access can yield a function too: `c.onKey state key …`
   a lambda literal can be applied directly: `(fun a b -> …) x y`, and an
   operator section `(op)` lowers to such a lambda; a nullary enum constructor
   in head position takes its fields as space args (`Ok 5` → `Ok(5)`)
   callee already carries explicit type args (`parse<T> arg`) — fold the
   value args into that same EApply rather than nesting.
   a nullary enum constructor in head position: its space args are its FIELDS
   (`Ok 5` → `EEnum(Ok, [5])`), matching the adjacent-paren `Ok(5)` form.
   a bare lowercase variable callee becomes a fn name when applied
*)
and parseApp state index =
  ExpressionPrecedence.parseApp {ExpressionPrecedence.parseExpr; parsePrimary} state index
(*
   an indentation-delimited sequence of statements (function / if-branch / match
   arm / lambda body): same-column statements on new lines fold into nested
   EStatement. Fresh scope so statements separate by column.
*)
and parseBlock state index =
  ExpressionControl.parseBlock {ExpressionControl.parseExpr; parseInfix} state index
(*
   `128y` etc. — only valid negated
   unary minus on a numeric literal: `-5L`, `-2.0` (infix `a - b` is handled in
   parseInfixRhs, so a `-` reaching here always prefixes a literal)
   unary minus on a non-literal (`-x`, `-(expr)`, `-f x`) → `Builtin.negate`
   applied to the whole application, so `-f x` groups as `-(f x)`, not `(-f) x`
   (minus binds looser than application). Infix ops still bind looser than the
   minus: `-a + b` is `(-a) + b`, since `+` can't start an application arg.
   Adjacent `<T,…>` type args on the name (no space before `<`): a generic fn
   call `parse<T> arg`, an enum ctor `Type<T>.Case`, or a record `Type<T> { … }`.
   A SPACED `<` is a comparison, not type args, so adjacency is required.
   `Type<T>.Case`: fold `Type` (which carries the type args) into the module
   path so the enum branch treats the trailing `.Case` as the case name.
   `Type { … }` record literal (the whole qualified name is the type).
   A record LITERAL is `{ }` or `{ field = … }`. `{ expr with … }` is NOT a
   literal of this type — there the brace is a standalone update expression
   passed as an argument (e.g. `Ctor { r with f = v } x`), so don't consume
   it as this name's record; let it flow into the payload/arg position.
   `Type { }` or `Type { field = … }` is unambiguously a record literal — Dark
   has no `{ }` blocks, and a bare `Type` isn't a statement, so this holds even
   when the `{` wraps to the next line LESS-indented than a wrapped type name
   (`… = (Combo\n  { e1 = … })`). `{ expr with … }` is NOT a literal of this
   type (it's a standalone update passed as an arg), so it's excluded below.
   a bare `Dict { … }` is a dict LITERAL, not a record of a type named `Dict`;
   `Dict` is a keyword here, so it parses to its own node with the keyword range.
   enum constructor: `[Mod.Path.]Type.Case fields…` — the LAST segment is
   the case, the segments before it form the type.
   `Case(e1, e2, …)` — parenthesized arg list ADJACENT to the case name
   (no space): commas separate FIELDS, so `EInt64(r, n, s)` is three fields,
   NOT one tuple. 1 item ⇒ 1 field. Adjacency matters: `Case (x) y` (space)
   is two space-separated args, and `… Ok` then `(x, [])` on the next line is
   a separate statement — both go through the general offside loop below.
   A non-adjacent-paren constructor is NULLARY here; space-separated fields
   (`Ok 5`, `Ok -4y`) are folded by parseApp — but ONLY in application-head
   position, so a constructor used as an ARGUMENT (`f None (g)`) stays nullary
   instead of over-grabbing its sibling arg.
   fn / value reference. Any adjacent `<T>` type args were parsed above into
   `nameTypeArgs`; carry them on a zero-arg EApply so parseApp folds in any
   value args that follow (`parse<T> arg`).
   bare `{` — anonymous record `{ f = v }` (or `{ }`) vs update `{ r with … }`
   Prefix `!` (boolean NOT) and `~` (bitwise NOT). Shaped exactly like unary
   minus on a non-literal: the operand is a whole APPLICATION, so `!f x` is
   `!(f x)`, and infix operators still bind looser (`!a && b` is `(!a) && b`,
   since `&&` cannot start an application argument).
   `::` parses in PATTERNS only; the expression-side way to prepend is `Stdlib.List.push` (or a
   literal). Volunteered here because the bare "expected an expression" reads as a typo, and the
   recovery lookup costs an agent two calls every time.
   recovery: an explicit error-hole node; leave closing/separating/decl-start
   tokens for the enclosing construct (so a group/list still closes and
   the next declaration survives); skip one token otherwise
*)
and parsePrimary state index =
  ExpressionPrimary.parsePrimary {ExpressionPrimary.parseExpr; parseBlock; parseApp; parseCtorParenFields; parseInterpString} state index
(*
   A parenthesized enum-constructor field list: `(e1, e2, …)` with `i` at the
   `(`. Commas separate FIELDS (so `Pair(a, b)` is two fields; a tuple field
   needs double parens). Fields are anchored exactly like a paren body
   (stmtExact — the `)` is the real delimiter), sunk into `sink`; returns the
   index after the `)`. Shared by the adjacent (`Ctor(…)`) and spaced
   (`Ctor (…)`) forms.
   `Ctor()` is `Ctor` applied to unit — one unit field, not zero
*)
and parseCtorParenFields state index sink =
  ExpressionPrecedence.parseCtorParenFields {ExpressionPrecedence.parseExpr; parsePrimary} state index sink
(*
   `$"text {expr} text"` — re-scan the token's source text, deriving exact ranges
   for the literal segments and each `{expr}`; the embedded expression is parsed by
   re-tokenizing its slice and offsetting the sub-token ranges to real positions.
   includes `$"` … `"`
   position of `$`
   offsets of each line start within fullText, computed once — posAt is then
   a binary search instead of a from-zero rescan (which was O(len²) across a
   long interpolated string's many segment boundaries)
   last line start <= off
   `{{`/`}}` are the source-level doubling escape for literal braces; resolve
   them on the RAW text FIRST so braces produced by `\{`/`\}` unescaping
   below aren't then collapsed (`\{\{` must yield `{{`, not `{`).
   regular `$'…'` literal parts get escapes processed (`\'`, `\n`, `\{`, …);
   triple-quoted `$"""…"""` stays raw but is NFC-normalized (like `unescape`)
   so both lowerings see canonical bytes.
   skip `\X` so an escaped quote `\'` doesn't end the string (regular only)
   Each `{expr}` body parses via a recursive parseTokensAt with fresh
   state, so interpolation nesting = recursion depth REGARDLESS of the
   expression depth guard. Uncapped, a `$"{$"{…}"}"` bomb is an
   uncatchable StackOverflow that kills the process.
   sub-token/diagnostic positions are relative to exprText; offset
   them to real source positions
   lexical-recovery diagnostics from inside `{…}` (unterminated
   literals etc.) surface like any other — previously dropped
   Parse `{...}` as an expression. Package declaration scope applies
   only to the outer file, not to interpolation contents.
   surface parse errors from inside the interpolation `{…}` (their
   ranges are already offset to the outer source) rather than dropping them
   a hard tokenize failure inside `{…}` (e.g. nesting cap) was
   previously swallowed as a silent unit
*)
and parseInterpString state index = ExpressionInterpolation.parseInterpString parseTokensAt state index
(*
   Parse a pre-tokenized stream. Part of the rec chain so string interpolation
   can recursively parse the (range-offset) sub-tokens of each `{expr}`.
*)
and parseTokensAt depth scope tokens =
  let state = makeState depth tokens in
  FileParser.parseFile {FileParser.parseExpr; parseBlock} scope state
let parseTokens tokens = parseTokensAt 0 ItemScope.Script tokens
let lexical range message = {code = DiagnosticCode.lex; severity = DiagError; range; message; related = []; hint = None}
let zero = {start = {row = 0; column = 0}; end_ = {row = 0; column = 0}}
(*
   lexical-recovery diagnostics (malformed lexemes the tokenizer recovered from)
   are surfaced alongside the parser's own diagnostics.
*)
let parseSyntaxWithRootScope rootScope source =
  match Lexer.tokenize source with
  | Error message -> {parsed = None; diagnostics = [lexical zero message]}
  | Ok (tokens, lexDiagnostics) ->
      let result = parseTokensAt 0 rootScope (Array.of_list tokens) in
      {result with diagnostics = List.map (fun (range, message) -> lexical range message) lexDiagnostics @ result.diagnostics}
(*
   Parse for tooling: return a recoverable tree and include mode-independent
   structural diagnostics after a clean syntax pass.
   Tree-wide rules have one implementation in Validation. Run them only
   after a clean syntax pass so recovery holes do not create cascaded errors.
*)
let parse source =
  let result = parseSyntaxWithRootScope ItemScope.Script source in
  let structural = match result.diagnostics, result.parsed with
    | [], Some (WT.SourceFile file) -> List.map diagnosticOfValidationIssue (Validation.validateStructure file)
    | _ -> [] in
  {result with diagnostics = result.diagnostics @ structural}
(*
   Parse for execution: syntax, structural, and file-purpose validation run
   once, and only a validated source file can be returned on success.
*)
let parseFor mode source =
  let scope = match mode with Validation.Package -> ItemScope.Module | Validation.Script | Validation.Test -> ItemScope.Script in
  let result = parseSyntaxWithRootScope scope source in
  match result.diagnostics, result.parsed with
  | [], Some (WT.SourceFile file) ->
      (match Validation.validate mode file with Ok value -> Ok value
      | Error issues -> Error (List.map diagnosticOfValidationIssue (ParserDependencies.toList issues)))
  | (_ :: _ as diagnostics), _ -> Error diagnostics
  | [], None -> Error [{code = DiagnosticCode.unexpected; severity = DiagError; range = zero;
      message = "Parser did not produce a source tree"; related = []; hint = None}]
(*
   Kept as a compatibility entrypoint. Test syntax has the same parse shape as
   all other source; parseFor Validation.Test applies the Test purpose rules.
*)
let parseTestFile = parse
(*
   Render a diagnostic for humans: code, position, message, a source snippet
   with caret markers, related locations, and the hint if any. E.g.
   error[PARSE-UNCLOSED] at 1:9: expected ']' to close the '[' at line 1:9, found end of file
   1 | let x = [1L; 2L
   |         ^
   note: the '[' opened here (1:9)
*)
let renderDiagnostic source (diagnostic : diagnostic) =
  let lines = Array.of_list (String.split_on_char '\n' source) in
  let snippet range =
    if range.start.row < 0 || range.start.row >= Array.length lines then [] else
    let original = HostText.utf16Units lines.(range.start.row) in
    let length = ref (Array.length original) in
    while !length > 0 && original.(!length - 1) = 13 do decr length done;
    let line = HostText.ofUtf16Units (Array.sub original 0 !length) in
    let number = string_of_int (range.start.row + 1) in
    let column = max 0 (min range.start.column !length) in
    let width = if range.start.row = range.end_.row then max 1 (min (range.end_.column - range.start.column) (max 1 (!length - column))) else 1 in
    ["  " ^ number ^ " | " ^ line; "  " ^ String.make (String.length number) ' ' ^ " | " ^ String.make column ' ' ^ String.make width '^'] in
  let first = Printf.sprintf "error[%s] at %d:%d: %s" diagnostic.code (diagnostic.range.start.row + 1) (diagnostic.range.start.column + 1) diagnostic.message in
  let related = List.concat_map (fun (range, note) ->
    Printf.sprintf "  note: %s (%d:%d)" note (range.start.row + 1) (range.start.column + 1) :: snippet range) diagnostic.related in
  let hint = match diagnostic.hint with None -> [] | Some text -> ["  hint: " ^ text] in
  String.concat "\n" (first :: snippet diagnostic.range @ related @ hint)
