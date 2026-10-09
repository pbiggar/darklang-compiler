(* FileParser.ml - Preserve speculative declarations, module nesting, and source assertions. *)
[@@@warning "-4"]

open Tokenizer
open ParserSupport
module WT = WrittenTypes

type grammar = {
  parseExpr : parserState -> int -> WT.expr * int;
  parseBlock : parserState -> int -> WT.expr * int;
}

(*
   `[<DB>]` attribute prefix on a type decl: 5 tokens. Parsing represents it;
   post-parse validation restricts it to Test source.
   `[` `<` `DB` `>` `]`.
*)
let isDbAttr state index =
  tok state index = TLBracket
  && tok state (index + 1) = TLt
  && tok state (index + 2) = TIdent "DB"
  && tok state (index + 3) = TGt
  && tok state (index + 4) = TRBracket

(*
   Test-mode only: the expected side of an assertion `actual = expected`. Either
   an `error="msg"` / `sqlerror="msg"` marker (also the bare `error "msg"` shape)
   or a plain value expression. The message string is kept raw (the tokenizer has
   already unescaped it); normalization happens in the lowering.
*)
let parseTestExpected grammar state index =
  match (tok state index, tok state (index + 1), tok state (index + 2)) with
  | TIdent "error", TEquals, TStringLit text -> (WT.TEError text, index + 3)
  | TIdent "sqlerror", TEquals, TStringLit text ->
      (WT.TESqlError text, index + 3)
  | TIdent "error", TStringLit text, _ -> (WT.TEError text, index + 2)
  | TIdent "sqlerror", TStringLit text, _ -> (WT.TESqlError text, index + 2)
  | _ ->
      let value, next = grammar.parseExpr state index in
      (WT.TEExpr value, next)

(*
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
*)
let rec parseItems grammar state itemScope start minColumn =
  let saved = state.declAnchor in
  let result =
    withElementScope state (fun () ->
        parseItemsBody grammar state itemScope start minColumn)
  in
  state.declAnchor <- saved;
  result

(*
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
and parseItemsBody grammar state itemScope start minColumn =
  let declarations = RevBuffer.create ()
  and expressions = RevBuffer.create ()
  and stop = ref start
  and more = ref true in
  while !more && tok state !stop <> TEOF do
    if (rng state !stop).start.column < minColumn then more := false
    else begin
      let before = !stop in
      setStmtCol state (rng state !stop).start.column;
      state.declAnchor <- (rng state !stop).start.column;
      (match (tok state !stop, tok state (!stop + 1)) with
      | TLBracket, TLt
        when isDbAttr state !stop && tok state (!stop + 5) = TType ->
          let declaration, after =
            TypeDefinitionParser.parseTypeDecl state (!stop + 5)
          in
          (match declaration with
          | WT.DType typ ->
              (match typ.WT.definition with
              | WT.TDAlias _ -> ()
              | _ ->
                  err state DiagnosticCode.expected (!stop + 5)
                    "[<DB>] type must be a type alias");
              RevBuffer.add declarations (WT.DTypeDB typ)
          | other -> RevBuffer.add declarations other);
          stop := after
      | TVal, TIdent _ ->
          let declaration, after =
            DeclarationParser.parseDecl grammar.parseBlock state !stop
          in
          (match declaration with
          | WT.DFunction _ ->
              err state DiagnosticCode.expected !stop
                "'val' declares a value and cannot have function parameters"
          | _ -> ());
          RevBuffer.add declarations declaration;
          stop := after
      | TLet, TIdent _ ->
          let savedDiagnostics = !(state.diagnostics) in
          let declaration, after =
            DeclarationParser.parseDecl grammar.parseBlock state !stop
          in
          let asExpr =
            match declaration with
            | WT.DValue _ ->
                tok state after = TIn || itemScope = ItemScope.Script
            | _ -> false
          in
          if asExpr then begin
            state.diagnostics := savedDiagnostics;
            let expression, after = grammar.parseExpr state !stop in
            RevBuffer.add expressions expression;
            stop := after
          end
          else begin
            (match declaration with
            | WT.DValue _ when itemScope = ItemScope.Module ->
                state.diagnostics :=
                  {
                    code = DiagnosticCode.unexpected;
                    severity = DiagError;
                    range = rng state !stop;
                    message =
                      "Module value declarations must use 'val'; 'let' is \
                       reserved for functions and local bindings";
                    related = [];
                    hint = Some "replace 'let' with 'val'";
                  }
                  :: !(state.diagnostics)
            | _ -> ());
            RevBuffer.add declarations declaration;
            stop := after
          end
      | TType, TIdent _ ->
          let declaration, after =
            TypeDefinitionParser.parseTypeDecl state !stop
          in
          RevBuffer.add declarations declaration;
          stop := after
      | TIdent "module", TIdent name ->
          let keyword = rng state !stop
          and column = (rng state !stop).start.column in
          let parts = RevBuffer.create ()
          and next = ref (!stop + 2)
          and scanning = ref true in
          RevBuffer.add parts name;
          while !scanning do
            match (tok state !next, tok state (!next + 1)) with
            | TDot, TIdent name ->
                RevBuffer.add parts name;
                next := !next + 2
            | _ -> scanning := false
          done;
          let nameRange =
            span (rng state (!stop + 1)) (rng state (!next - 1))
          in
          let children, childExpressions, after =
            if tok state !next = TEquals then
              parseItems grammar state ItemScope.Module (!next + 1) (column + 1)
            else parseItems grammar state ItemScope.Module !next minColumn
          in
          if tok state !next = TEquals && children = [] && childExpressions = []
          then errExpected state (!next + 1) "an indented module body";
          let ending = if after > 0 then rng state (after - 1) else nameRange in
          RevBuffer.add declarations
            (WT.DModule
               {
                 WT.range = span keyword ending;
                 name = (nameRange, String.concat "." (RevBuffer.toList parts));
                 declarations =
                   children
                   @ List.map
                       (fun expression -> WT.DExpr expression)
                       childExpressions;
                 keywordModule = keyword;
               });
          stop := after;
          if tok state !next <> TEquals then more := false
      | _ ->
          let expression, after = grammar.parseExpr state !stop in
          if tok state after = TEquals then begin
            let expected, next = parseTestExpected grammar state (after + 1) in
            let ending =
              if next > 0 then rng state (next - 1) else rng state after
            in
            RevBuffer.add declarations
              (WT.DTest
                 {
                   WT.range = span (WT.exprRange expression) ending;
                   actual = expression;
                   expected;
                 });
            stop := next
          end
          else begin
            RevBuffer.add expressions expression;
            stop := after
          end);
      if !stop = before then
        if tok state !stop = TEOF then more := false else incr stop
    end
  done;
  (RevBuffer.toList declarations, RevBuffer.toList expressions, !stop)

let parseFile grammar rootScope state =
  validateLiterals state;
  let declarations, exprsToEval, _ = parseItems grammar state rootScope 0 0 in
  let range =
    if state.tokenCount > 1 then
      span (rng state 0) (rng state (state.tokenCount - 2))
    else rng state 0
  in
  {
    parsed = Some (WT.SourceFile { WT.range; declarations; exprsToEval });
    diagnostics = List.rev !(state.diagnostics);
  }
