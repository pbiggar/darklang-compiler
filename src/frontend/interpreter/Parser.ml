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
   one of DiagnosticCode — stable across releases
   secondary locations, e.g. the opening delimiter of an unclosed pair
   One offside scope: the current statement's anchor column (`stmtCol`; -1 = none)
   and `stmtExact`, a flag marking a parenthesized body — there the closing `)`
   is the real delimiter, so only a token at EXACTLY the anchor column starts a
   new statement; dedented continuations (`|> (fn\n  args-below-callee)`) stay
   part of the current one. Constructs enter scopes only through the `with*`
   helpers in `parseTokens`, so prior state is restored by construction.
   The one definition of "an integer-literal token" — `canStartAtom` and
   `canStartPattern` both build on it so the lists can't drift from each other.
   Matched EXHAUSTIVELY (no `_`): a new `Token` case won't compile until it's
   classified here, so a new integer type can't silently fall through to `false`
   — the drift that once made `| Ok 5y ->` unparseable while `Ok 5y` worked.
   a record/anonymous-record/update `{ … }` can be a function argument, e.g.
   `parseArgs tail { acc with port = p }`
   prefix `!`/`~`: `f !x` is `f (!x)`. Unambiguous — neither token has an
   infix reading, and `!=` lexes as one token, so `a != b` is untouched.
   `TMinus` is included so a negative-literal enum-pattern field (`| Ok -4y ->`)
   parses; parsePatternBase's TMinus case handles it.
   tokens that close/separate a block — another statement can't start with these
   tokens hole-recovery must never consume: closing/separating tokens (the enclosing
   construct needs them to close cleanly) and declaration starters (the next
   declaration must survive a broken one before it)
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
   raw source text of the token (e.g. a type variable `'a` lexes to an
   ident "a" but its text keeps the leading tick)
   `///` doc comment attached to the token at `i` (a declaration keyword), if any
   Zero-width range at `r`'s end: the range for a synthetic/missing node (an
   absent `>`, a bare tuple's missing parens, an unsplit int suffix) — points at
   "where it should be" without claiming any real source characters.
   set when a guard abandons the parse: suppresses the cascade of secondary
   diagnostics from the unwinding frames
   what the parser is looking at, for "expected X, found Y" messages
   a missing closing delimiter: point back at its opener
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
   A sized-int literal whose magnitude is the type's |MinValue| lexes to
   MinValue (so the NEGATED literal can exist: `-128y`); consumed WITHOUT the
   minus, the written magnitude is out of range — diagnose instead of silently
   wrapping (`128y` is NOT -128). Only the negating TMinus branches consume
   these tokens without passing through here.
   whole/fraction decimal strings of a float literal at token `i`. The
   double's shortest round-trip form is used when it's a plain decimal (the
   usual case); when it needs an exponent (`1e300`, `0.00000001`) or isn't
   finite-decimal at all, the SOURCE text is decimal-shifted instead — the
   PT float representation is exponent-free strings, and an exponent leaking
   into the whole part crashes `makeFloat` downstream.
   decimal-shift the literal text `mant[.frac][eE][+-]exp` (exact, no
   floating-point re-derivation)
   Reject invalid escapes / codepoints in string, char, and interpolated-string
   literals (triple-quoted forms are raw, so skipped). A diagnostic here becomes a
   `ParseError.Message` — the escape is otherwise silently error-recovered.
   qualified name: ident (. ident)*  → (modules, finalIdent, nextIndex)
   only step into `.seg` as a module path when the CURRENT segment is
   uppercase (a module); a lowercase ident's `.field` is postfix access,
   left for parsePostfix to handle.
   previous segment becomes a module, with the dot range
   Matches a written type name against the primitive types.
   `>>` lexes as one TShr token but closes two generic levels. `state.pendingGt`
   carries the leftover `>` to the enclosing type so `List<List<T>>` parses.
   Close one generic level (`>`), splitting a `>>` (TShr) into one consumed `>`
   and one pending. Returns (close-`>` range, next index).
   Skip a `< … >` type-argument list (not modelled yet); a trailing `>>` leaves
   one `>` pending for the enclosing generic.
   declaration type parameters `<'a, 'b>` — collect the (tick-stripped) names
   so generic types/fns keep their params (needed for runtime type unification).
   --- offside scope stack ---
   One scope = `stmtCol` (the current statement's anchor column; -1 = none) +
   `stmtExact` (a parenthesized body: only a token at EXACTLY the anchor column
   starts a new statement — the `)` is the real delimiter). let/if/match push a
   fresh scope so their sub-expressions keep normal offside (a let value must
   not swallow the next statement). All state transitions go through the
   `with*` helpers below, which restore the prior scope by construction — a
   leftover flag can't leak into what follows.
   anchor the current scope's statement column (per statement / element)
   fresh sub-statement scope, statement anchored at `col`
   Would a `|` at `i` start a match arm rather than continue an expression?
   True when it sits on the arm row or at the arm column of ANY enclosing
   match -- a `|` less indented than the innermost match belongs to an outer
   one, and must not be eaten as an operator either.
   Run `f` without enclosing match-arm anchors, where `|` can only be an operator.
   fresh scope; statement anchor inherited (managed per-element by `f`)
   re-anchor the statement column within the CURRENT scope as a
   parenthesized (exact-column) anchor, restoring both after
   Column of the DECLARATION currently being parsed (set per item by
   parseItems), or -1. Recovery only: a decl-start keyword at or left of this
   column can never belong to a construct inside the declaration, so an
   unclosed delimiter above must stop instead of swallowing the next
   declaration (`let broken = [1L;` must not eat the `let fine …` below it).
   Offside: a trailing operand (application arg, enum-constructor/pattern field)
   at `k` continues the construct started at `headIdx` only if it's on the same
   line as the head, or indented further (or we're inside parens). A token on a
   new line at the same-or-lower indent starts a new statement.
   A `-` GLUED to a following number, with a space before it, is a negative-literal
   ARGUMENT (`f a -1`), not subtraction (`f a - 1` = `(f a) - 1`). The application
   arg loop accepts it so `Float.multiply a -1.0` / `add 5L -1L` parse correctly
   (application binds more tightly than infix operations).
   no space after `-`
   space before `-`
   `;` used to separate list elements and list-pattern elements; `,` is the only
   separator now. Report it precisely and keep parsing as if it had been a `,`, so
   a file written in the old style yields one diagnostic per `;` instead of a
   cascade of recovery noise from the elements after it.
   The effect names a `:{…}` row may use: the `Effects.Effect` case names.
   pipe is the lowest precedence: `expr |> seg |> seg …`
   `x |> (op) y` desugars to `x op y` (the piped value is the LEFT operand),
   so an operator section directly after `|>` with an argument becomes a
   pipe-infix — not the section lambda applied (which would flip the order).
   → diagnostic
   convert a parsed pipe RHS expression into a structured pipe segment
   keep the callee's type args — `x |> parse<T>` needs `T` (dropping it left the
   piped fn call with no type args, so `parse` had no target type: "type 'a")
   iterative (a 10k-statement body must not recurse 10k deep); statements
   fold right-nested into EStatement afterwards
   no progress — stop (avoids a spin)
   bare tuple expr: `match a, b with`
   arms align on the first `|`'s column; a `|` LESS indented than that belongs
   to an enclosing match (so a nested match doesn't swallow the outer's arms).
   While the arm BODIES are parsed, a `|` on this match's arm row or at its
   arm column is an arm separator, not the bitwise-or operator. The subject
   expression above is parsed outside the anchor, where `|` is still an
   operator (`match a | b with …`).
   or-level: `p1 | p2 | …` (stops at `->` / `when`)
   top level: a bare tuple `a, b` (comma-separated, no parens); else an or-pattern
   A full match-arm pattern. Precedence, from lowest to highest: `|` (or) is LOOSEST, then
   `,` (tuple), then `::` (cons). So `1, 2 | 3, 4` is `(1,2) | (3,4)` — an or of
   two tuples, NOT a 3-tuple with an or in the middle. Hence `|` is the OUTER
   level here, wrapping tuples (`parsePatternTuple`).
   tuple level: `p1, p2, …` (bare — no parens). Elements are cons-patterns; `|`
   binds looser (handled above) so it can't appear as a bare tuple element.
   bare tuple: no parens
   or of cons-patterns, NO tuple — used for enum-ctor fields, where a bare `,`
   separates FIELDS (`Case(a, b)` = two fields), not tuple elements.
   cons-level: `h :: t` (right-assoc)
   unary minus on a numeric literal pattern: `-5L`, `-1y`, `-2.0` (unsigned
   types can't be negative, so only the signed literals + float are handled).
   parens hold a full pattern (or > tuple > cons). Parse it, then attach the
   real paren ranges when it's a bare tuple; otherwise the parens are just
   grouping (`(a | b)`, `(p)`) and drop away.
   enum pattern: `[Mod.]Case [fieldPats…]` — last segment is the case
   A qualified path (`Result.Ok`, `Stdlib.Result.Result.Ok`) is not a valid enum
   pattern — patterns use the unqualified case name. Reject rather than silently
   building a truncated pattern from just the last segment.
   `Case(p1, p2, …)` is a parenthesized arg list: commas separate FIELDS, so
   `Pair(a, b)` is two fields — NOT one tuple `Pair((a, b))`. This holds
   whether or not there's a space before the `(` .
   `Case()` is one unit field (`Case` applied to unit)
   TODO: Support `...` list rest patterns once WrittenTypes and ProgramTypes
   represent their binding and matching semantics.
   `else`/`elif` binds to the `if` at `minCol` or to the left; a less-indented
   `else` belongs to an ENCLOSING `if`, so a nested inner `if` in the THEN block
   must not greedily grab it (`if… then (if… then c) else b` → the `else` is the
   OUTER's). `minCol` is the column of the FIRST `if` in a chain — so `else if …`
   chains still bind at the chain's column even though each nested `if` sits
   further right (after `else `). Same-row `else` always binds (col > minCol).
   same-line `else if …` continues the chain → the nested `if` inherits this
   chain's `minCol`; any other else-body is a normal block at its own indent.
   `elif` is `else (if …)` — the nested EIf is the else branch (its own
   `if`-keyword range colors the `elif`, so no separate else-keyword range).
   a simple binding pattern: variable / wildcard / `()` unit
   tuple pattern `(a, b, …)` or a parenthesized pattern `(a)`
   recovery: keep a benign binder (LetPattern has no error case yet);
   nested function definition: `let f (x: T) (y) [: R] = body` — bind a lambda
   to the name (params lowered to untyped lambda patterns, types discarded)
   no real `fun`/`->` tokens in this sugar
   Value annotations are not part of Dark. Consume the type for recovery,
   but reject the source instead of silently discarding it.
   the value is an offside block, not a single expr, so a multi-statement
   binding (`let x =\n  doThing ()\n  result`) sequences instead of gluing
   the following statement onto the first as an application argument.
   `in` is optional
   left-assoc binary level
   --- infix expressions: one precedence-climbing loop ---
   Binding powers, loosest → tightest (higher binds tighter); a right-assoc
   op recurses at its own power so it nests to the right.
   1 `||`   2 `&&`   3 `== != < > <= >=`   4 `|`   5 `^`   6 `&`
   7 `<< >>`   8 `@` (right)   9 `+ - ++`   10 `* / %`   11 `**` (right)
   The bitwise levels follow Python's order rather than C's: they bind TIGHTER
   than the comparisons, so `a & b == c` is `(a & b) == c` and not C's
   `a & (b == c)`. Every pre-existing operator keeps its relative position.
   `@` desugars to `Stdlib.List.append` (there is no WT infix for it); `**` is
   exponentiation and nests right: `2 ** 3 ** 2 = 2 ** (3 ** 2)`.
   The operator must belong to THIS statement: same row as the left
   operand's end, or inside parens, or an indented continuation. Otherwise
   a following statement that starts with a prefix operator (`1L\n-8L …`)
   would be wrongly glued on as `1L - 8L …`. On a new line, a pure infix
   operator at the statement column continues (`x\n++ y` — `++` can't start
   a statement), but `-` there begins a new statement (a negative literal),
   so it must be indented PAST it. This rule is identical for every caller.
   A `|` belonging to an enclosing match is that match's arm separator, not
   bitwise-or; leave it for `parseMatch`.
   climb: the RHS folds in everything binding tighter (or equally
   tight, for a right-assoc op) before this level continues.
   A prefix operator: `op operand` → `Builtin.<name> operand`, with the operand
   parsed as a whole APPLICATION so `op f x` is `op (f x)`. Shared by `!`, `~`
   and the non-literal case of unary `-`.
   postfix `.field` record access (left-assoc, chains)
   `()` unit / `( e )` group / `( a, b, … )` tuple
   operator section `(op)` → `fun a b -> a op b` (a 2-arg fn value)
   Anchor arg-offside at the inner column instead of blanket-suspending
   it, so a wrapped arg (indented past its callee or the statement)
   still continues but a sibling statement at the inner column is a NEW
   statement. This lets a paren-wrapped body be a newline-separated
   statement BLOCK — `(stmt1 \n stmt2)` — folded into EStatement, not
   one over-grabbing application.
   group or statement block: fold newline-separated statements
   `[ e , e , … ]` (or newline separators); each element keeps its
   trailing-separator range
   list elements are offside-delimited in their own scope
   (else an element swallows the next one as an application, e.g. inside `( … )`:
   `[ [a]\n (f x) ]` must not read as `[a] (f x)`). Mirrors parseRecord.
   each element is its own offside statement, so a wrapped element's args
   don't grab the NEXT element (`[ f a\n f b ]` stays two elements)
   `Type { name = value ; … }`. `i` is the `{`.
   record fields are offside-delimited in their own scope
   (else a field value swallows the next field name, e.g. inside `( … )`)
   each field is its own offside statement (value args don't grab the next field)
   record update `{ expr with name = value ; … }`. `i` is the `{`.
   fields are offside-delimited, same as parseRecord — suspend paren relaxation
   and anchor each field value at its own column so it can't grab the next field.
   `{ r with }` updates nothing — reject it rather than lower a degenerate
   update (both lowerings otherwise have to special-case the empty list).
   Type references. Precedence (loosest first): function `A -> B`, tuple
   `A * B`, then atoms (prim / List / Dict / custom / `'a` / parenthesized).
   Defensive: `state.pendingGt` is always 0 here in well-formed input (a `>>`-induced
   pending is consumed by the enclosing generic before the next parseTypeRef).
   Clearing it stops a malformed `>>` in a prior parse from leaking a phantom `>`
   into this one.
   `A -> B -> C` (right-nested): arguments = [(A,->),(B,->)], ret = C
   `A * B * C` (bare tuple, e.g. inside `List<…>`); parenthesized tuples fill
   in real paren ranges at the atom level.
   a pending `>` (from splitting a `>>`) means we're still inside an enclosing
   generic, so a following `*` belongs to an OUTER tuple — don't absorb it here
   (otherwise `List<List<A>> * B` mis-parses as `List<List<A> * B>`).
   `<T1, T2, …>` generic type-args on a custom type; uses expectGt so a trailing
   `>>` splits correctly. Returns the args, the real or recovered closing `>`
   range, and the index after it.
   stop taking args once a `>>` has left a `>` pending for THIS level, else a
   nested `Option<Result<T,S>>` would swallow the enclosing type's next arg
   (`Option<Result<T,S>, S>`). Mirrors the tuple loop's `state.pendingGt = 0` guard.
   `(T)` grouping or `(A * B)` parenthesized tuple
   a tick-prefixed name is ALWAYS a type variable, even when uppercase
   (`'TModel`) — the lexer drops the tick so the case-based check below would
   otherwise mistake it for a custom type. The token text keeps the tick.
   a lowercase ident in type position is a type variable: `'a` lexes to the
   bare name "a" with the token range covering the apostrophe.
   recovery: leave closing/separating/decl-start tokens for the enclosing construct
   a function parameter `(name: Type)` or `()`
   `_` names a parameter you don't intend to use. It's also what `()` is stored as, so accepting it
   here is what makes `(_: Unit)` and `()` both parse to the same thing and either form round-trip.
   A `///` for a parameter attaches to whichever token follows it: the `(` when the comment is
   written above the whole parameter, the NAME when it is written just inside the paren. Both
   spellings occur, so both are read.
   A declaration-scope function (`let f (p: T) … : R = body`) or value
   (`val x = body`). Legacy module-level `let x = body` also comes through here
   to retain a recovery DValue beside its focused diagnostic.
   `:{Http, Clock}` immediately after a declaration's return colon. Absent
   means no ceiling; `{}` means effect-free. Unknown names are diagnostics,
   not wildcards: the row fails closed.
   `type Name [<'a>] = Definition`
   `type X = {}` — an empty record isn't valid. Diagnose so the `"_"` placeholder
   the normalizer inserts (records need ≥1 field) is honest recovery, not a
   silently-accepted phantom field. (A `{ garbage }` already errored in the loop
   and won't be at `}` here, so this only fires for a genuinely empty record.)
   an enum case's fields are separated by `*` (`Case of A * B` = two fields), so
   the field type is parsed at ATOM level — a bare `*` is the separator, not a
   tuple. A tuple field must be parenthesized (`(A * B)`), which parseAtomType handles.
   Only the first case may omit the leading `|` (`type X = A | B`); every case
   after it REQUIRES a `|`. Otherwise the following statement — which often
   starts with an uppercase name (`type X = | A | B | C` then `Foo.bar = …`) —
   would be swallowed as another case, orphaning the rest of that line.
   A `///` above a case attaches to whichever token starts it: the leading `|` usually, or the
   case name itself when the first case omits its bar (`type X = A | B`).
   `type X = |` with no case — diagnose so the `"_"` placeholder the normalizer
   inserts (enums need ≥1 case) is honest recovery, not a silent phantom case.
   Test-mode only: the expected side of an assertion `actual = expected`. Either
   an `error="msg"` / `sqlerror="msg"` marker (also the bare `error "msg"` shape)
   or a plain value expression. The message string is kept raw (the tokenizer has
   already unescaped it); normalization happens in the lowering.
   `[<DB>]` attribute prefix on a type decl: 5 tokens. Parsing represents it;
   post-parse validation restricts it to Test source.
   `[` `<` `DB` `>` `]`.
   Parse declarations/expressions whose start column is >= minCol (offside): a
   less-indented item ends the scope. Used for the file body and, recursively,
   for nested `module X =` blocks, so module nesting is preserved (FQN paths).
   `itemScope` distinguishes module declaration rules (values require `val`)
   from script rules (a no-param `let x = …` is an expression that sequences
   with what follows).
   Test assertions and `[<DB>]` declarations are represented in every parse.
   Validation later decides whether Script, Package, or Test source may contain
   them. `itemScope` only distinguishes declaration-scope `let` from a script
   binding.
   each item is its own offside statement (anchored per item in the body);
   the enclosing scope's anchor + decl anchor are restored on exit
   `[<DB>] type Name = AliasedType` — a user DB declaration
   `val x = …` is ALWAYS a value DECLARATION (the explicit value-decl
   keyword), in a module or at file top level alike.
   the decl parse below is speculative (a top-level no-param `let` is
   reparsed as an expression) — drop its diagnostics on reparse so
   errors aren't reported twice
   A no-param `let x = …` is a script EXPRESSION at file top level and
   with explicit `in`. At module declaration scope it is retained as a
   DValue recovery node with a diagnostic requiring `val`; `let f (p) …`
   is a DFunction and is never reparsed.
   `module X =`: body is offside-indented under the keyword.
   `module Darklang.X` (no `=`): file-level header wrapping the rest.
   A module's trailing expressions belong to the module (as `DExpr`
   declarations), not the enclosing scope — so they pretty-print nested.
   span the whole module (header through last child), like other decls —
   a header-only range makes range-gated walkers (hover) skip the body
   file header consumed the rest
   A source-level `actual = expected` is represented as a test node.
   Its validity for this caller is decided after parsing.
*)
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
  ExpressionControl.parseExpr
    { ExpressionControl.parseExpr; parseInfix }
    state index

and parseInfix state index =
  ExpressionPrecedence.parseInfix
    { ExpressionPrecedence.parseExpr; parsePrimary }
    state index

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
  ExpressionPrecedence.parseApp
    { ExpressionPrecedence.parseExpr; parsePrimary }
    state index

(*
   an indentation-delimited sequence of statements (function / if-branch / match
   arm / lambda body): same-column statements on new lines fold into nested
   EStatement. Fresh scope so statements separate by column.
*)
and parseBlock state index =
  ExpressionControl.parseBlock
    { ExpressionControl.parseExpr; parseInfix }
    state index

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
  ExpressionPrimary.parsePrimary
    {
      ExpressionPrimary.parseExpr;
      parseBlock;
      parseApp;
      parseCtorParenFields;
      parseInterpString;
    }
    state index

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
  ExpressionPrecedence.parseCtorParenFields
    { ExpressionPrecedence.parseExpr; parsePrimary }
    state index sink

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
and parseInterpString state index =
  ExpressionInterpolation.parseInterpString parseTokensAt state index

(*
   Parse a pre-tokenized stream. Part of the rec chain so string interpolation
   can recursively parse the (range-offset) sub-tokens of each `{expr}`.
*)
and parseTokensAt depth scope tokens =
  let state = makeState depth tokens in
  FileParser.parseFile { FileParser.parseExpr; parseBlock } scope state

let parseTokens tokens = parseTokensAt 0 ItemScope.Script tokens

let lexical range message =
  {
    code = DiagnosticCode.lex;
    severity = DiagError;
    range;
    message;
    related = [];
    hint = None;
  }

let zero = { start = { row = 0; column = 0 }; end_ = { row = 0; column = 0 } }

(*
   lexical-recovery diagnostics (malformed lexemes the tokenizer recovered from)
   are surfaced alongside the parser's own diagnostics.
*)
let parseSyntaxWithRootScope rootScope source =
  match Lexer.tokenize source with
  | Error message -> { parsed = None; diagnostics = [ lexical zero message ] }
  | Ok (tokens, lexDiagnostics) ->
      let result = parseTokensAt 0 rootScope (Array.of_list tokens) in
      {
        result with
        diagnostics =
          List.map
            (fun (range, message) -> lexical range message)
            lexDiagnostics
          @ result.diagnostics;
      }

(*
   Parse for tooling: return a recoverable tree and include mode-independent
   structural diagnostics after a clean syntax pass.
   Tree-wide rules have one implementation in Validation. Run them only
   after a clean syntax pass so recovery holes do not create cascaded errors.
*)
let parse source =
  let result = parseSyntaxWithRootScope ItemScope.Script source in
  let structural =
    match (result.diagnostics, result.parsed) with
    | [], Some (WT.SourceFile file) ->
        List.map diagnosticOfValidationIssue (Validation.validateStructure file)
    | _ -> []
  in
  { result with diagnostics = result.diagnostics @ structural }

(*
   Parse for execution: syntax, structural, and file-purpose validation run
   once, and only a validated source file can be returned on success.
*)
let parseFor mode source =
  let scope =
    match mode with
    | Validation.Package -> ItemScope.Module
    | Validation.Script | Validation.Test -> ItemScope.Script
  in
  let result = parseSyntaxWithRootScope scope source in
  match (result.diagnostics, result.parsed) with
  | [], Some (WT.SourceFile file) -> (
      match Validation.validate mode file with
      | Ok value -> Ok value
      | Error issues ->
          Error
            (List.map diagnosticOfValidationIssue
               (ParserDependencies.toList issues)))
  | (_ :: _ as diagnostics), _ -> Error diagnostics
  | [], None ->
      Error
        [
          {
            code = DiagnosticCode.unexpected;
            severity = DiagError;
            range = zero;
            message = "Parser did not produce a source tree";
            related = [];
            hint = None;
          };
        ]

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
    if range.start.row < 0 || range.start.row >= Array.length lines then []
    else
      let original = Text.scalars lines.(range.start.row) in
      let length = ref (Array.length original) in
      while !length > 0 && original.(!length - 1) = 13 do
        decr length
      done;
      let line = Text.ofScalars (Array.sub original 0 !length) in
      let number = string_of_int (range.start.row + 1) in
      let column = max 0 (min range.start.column !length) in
      let width =
        if range.start.row = range.end_.row then
          max 1
            (min
               (range.end_.column - range.start.column)
               (max 1 (!length - column)))
        else 1
      in
      [
        "  " ^ number ^ " | " ^ line;
        "  "
        ^ String.make (String.length number) ' '
        ^ " | " ^ String.make column ' ' ^ String.make width '^';
      ]
  in
  let first =
    Printf.sprintf "error[%s] at %d:%d: %s" diagnostic.code
      (diagnostic.range.start.row + 1)
      (diagnostic.range.start.column + 1)
      diagnostic.message
  in
  let related =
    List.concat_map
      (fun (range, note) ->
        Printf.sprintf "  note: %s (%d:%d)" note (range.start.row + 1)
          (range.start.column + 1)
        :: snippet range)
      diagnostic.related
  in
  let hint =
    match diagnostic.hint with None -> [] | Some text -> [ "  hint: " ^ text ]
  in
  String.concat "\n" ((first :: snippet diagnostic.range) @ related @ hint)
