(* ExpressionControl.ml - Offside blocks, conditionals, lets, matches, and pipes. *)
[@@@warning "-4"]

open Tokenizer
open ParserSupport
module WT = WrittenTypes

type grammar = {
  parseExpr : parserState -> int -> WT.expr * int;
  parseInfix : parserState -> int -> WT.expr * int;
}

let last buffer =
  match RevBuffer.last buffer with
  | Some value -> value
  | None -> Crash.crash "Empty parser collector"

let rec parseExpr grammar state index =
  if tooDeep state index || outOfFuel state index then
    (WT.EError (rng state index), state.tokenCount - 1)
  else begin
    state.depth <- state.depth + 1;
    let result =
      match tok state index with
      | TLet | TIf | TMatch ->
          withStmtScope state (rng state index).start.column (fun () ->
              match tok state index with
              | TLet -> parseLet grammar state index
              | TIf ->
                  parseIf grammar state (rng state index).start.column index
              | _ -> parseMatch grammar state index)
      | _ -> parsePipe grammar state index
    in
    state.depth <- state.depth - 1;
    result
  end

(*
   pipe is the lowest precedence: `expr |> seg |> seg …`
   `x |> (op) y` desugars to `x op y` (the piped value is the LEFT operand),
   so an operator section directly after `|>` with an argument becomes a
   pipe-infix — not the section lambda applied (which would flip the order).
   → diagnostic
*)
and parsePipe grammar state index =
  let expression, next = grammar.parseInfix state index in
  if tok state next <> TPipe then (expression, next)
  else
    let parts = RevBuffer.create () and current = ref next in
    while tok state !current = TPipe do
      let pipeRange = rng state !current in
      if
        tok state (!current + 1) = TLParen
        && Option.is_some (infixOf (tok state (!current + 2)))
        && tok state (!current + 3) = TRParen
        && (canStartAtom (tok state (!current + 4))
           || isNegLitArg state (!current + 4))
      then begin
        let opRange = rng state (!current + 2) in
        let infix =
          match infixOf (tok state (!current + 2)) with
          | Some infix -> infix
          | None -> Crash.crash "Validated pipe operator has no infix operation"
        in
        let argument, stop = grammar.parseInfix state (!current + 4) in
        RevBuffer.add parts
          ( pipeRange,
            WT.EPipeInfix
              (span opRange (WT.exprRange argument), (opRange, infix), argument)
          );
        current := if stop > !current then stop else !current + 1
      end
      else begin
        let rhs, stop = grammar.parseInfix state (!current + 1) in
        (match toPipeExpr rhs with
        | Some segment -> RevBuffer.add parts (pipeRange, segment)
        | None ->
            err state DiagnosticCode.pipeSegment (!current + 1)
              "unsupported pipe segment");
        current := if stop > !current then stop else !current + 1
      end
    done;
    let endRange =
      if !current > 0 then rng state (!current - 1) else rng state next
    in
    ( WT.EPipe
        ( span (WT.exprRange expression) endRange,
          expression,
          RevBuffer.toList parts ),
      !current )

(*
   convert a parsed pipe RHS expression into a structured pipe segment
   keep the callee's type args — `x |> parse<T>` needs `T` (dropping it left the
   piped fn call with no type args, so `parse` had no target type: "type 'a")
*)
and toPipeExpr = function
  | WT.EApply (range, WT.EFnName (_, name), typeArgs, args) ->
      Some (WT.EPipeFnCall (range, name, typeArgs, args))
  | WT.EFnName (range, name) -> Some (WT.EPipeFnCall (range, name, [], []))
  | WT.EVariable (range, name) -> Some (WT.EPipeVariableOrFnCall (range, name))
  | WT.ELambda (range, patterns, body, keyword, arrow) ->
      Some (WT.EPipeLambda (range, patterns, body, keyword, arrow))
  | WT.EEnum (range, typeName, caseName, fields, dot) ->
      Some (WT.EPipeEnum (range, typeName, caseName, fields, dot))
  | WT.EApply (range, WT.EVariable (variableRange, name), typeArgs, args) ->
      Some
        (WT.EPipeFnCall
           ( range,
             { WT.range; modules = []; fn = { WT.range = variableRange; name } },
             typeArgs,
             args ))
  | _ -> None

and parseBlock grammar state index =
  withStmtScope state (rng state index).start.column (fun () ->
      parseBlockAt grammar state (rng state index).start.column index)

(*
   iterative (a 10k-statement body must not recurse 10k deep); statements
   fold right-nested into EStatement afterwards
   no progress — stop (avoids a spin)
*)
and parseBlockAt grammar state column index =
  let statements = RevBuffer.create ()
  and current = ref index
  and scanning = ref true in
  while !scanning do
    let statement, stop = grammar.parseExpr state !current in
    RevBuffer.add statements statement;
    if stop = !current then scanning := false
    else if tok state stop = TSemicolon then current := stop + 1
    else if hasNextStmt state column stop then current := stop
    else begin
      current := stop;
      scanning := false
    end
  done;
  let folded =
    match List.rev (RevBuffer.toList statements) with
    | [] -> Crash.crash "Empty parsed statement sequence"
    | final :: earlier ->
        List.fold_left
          (fun acc statement ->
            WT.EStatement
              (span (WT.exprRange statement) (WT.exprRange acc), statement, acc))
          final earlier
  in
  (folded, !current)

and hasNextStmt state column index =
  tok state index <> TEOF
  && (rng state index).start.column = column
  && tok state index <> TBar
  && not (closesOrSeparates (tok state index))

(*
   bare tuple expr: `match a, b with`
   arms align on the first `|`'s column; a `|` LESS indented than that belongs
   to an enclosing match (so a nested match doesn't swallow the outer's arms).
   While the arm BODIES are parsed, a `|` on this match's arm row or at its
   arm column is an arm separator, not the bitwise-or operator. The subject
   expression above is parsed outside the anchor, where `|` is still an
   operator (`match a | b with …`).
*)
and parseMatch grammar state index =
  let keywordMatch = rng state index in
  let first, next = grammar.parseExpr state (index + 1) in
  let expression, next =
    if tok state next <> TComma then (first, next)
    else begin
      let comma = rng state next in
      let second, stop = grammar.parseExpr state (next + 1) in
      let rest = RevBuffer.create ()
      and current = ref stop
      and scanning = ref true in
      while !scanning && tok state !current = TComma do
        let comma = rng state !current in
        let value, stop = grammar.parseExpr state (!current + 1) in
        RevBuffer.add rest (comma, value);
        if stop > !current then current := stop else scanning := false
      done;
      let zero = zeroWidthAtEnd (WT.exprRange first) in
      let final =
        if RevBuffer.length rest > 0 then snd (last rest) else second
      in
      ( WT.ETuple
          ( span (WT.exprRange first) (WT.exprRange final),
            first,
            comma,
            second,
            RevBuffer.toList rest,
            zero,
            zero ),
        !current )
    end
  in
  let keywordWith, afterWith =
    if tok state next = TWith then (rng state next, next + 1)
    else begin
      errExpected state next "'with' in match";
      (zeroWidthAtEnd (rng state next), next)
    end
  in
  let cases = RevBuffer.create () in
  let armColumn =
    if tok state afterWith = TBar then (rng state afterWith).start.column else 0
  in
  let armRow =
    if tok state afterWith = TBar then (rng state afterWith).start.row else -1
  in
  let current = ref afterWith in
  let savedArms = state.matchArms in
  state.matchArms <- (armRow, armColumn) :: savedArms;
  Fun.protect
    (fun () ->
      while
        tok state !current = TBar
        && ((rng state !current).start.row = armRow
           || (rng state !current).start.column = armColumn)
      do
        let barRange = rng state !current in
        let pattern, next =
          PatternParser.parseMatchPattern state (!current + 1)
        in
        let condition, next =
          if tok state next = TWhen then
            let whenRange = rng state next in
            let value, stop = grammar.parseExpr state (next + 1) in
            (Some (whenRange, value), stop)
          else (None, next)
        in
        let arrow, next =
          if tok state next = TArrow then (rng state next, next + 1)
          else begin
            errExpected state next "'->' in match case";
            (zeroWidthAtEnd (rng state next), next)
          end
        in
        let rhs, stop = parseBlock grammar state next in
        RevBuffer.add cases
          {
            WT.barRange;
            pat = pattern;
            arrowRange = arrow;
            whenCondition = condition;
            rhs;
          };
        current := if stop > !current then stop else !current + 1
      done)
    ~finally:(fun () -> state.matchArms <- savedArms);
  if RevBuffer.length cases = 0 then
    errExpected state afterWith "at least one match case starting with '|'";
  let endRange =
    if RevBuffer.length cases > 0 then WT.exprRange (last cases).WT.rhs
    else keywordWith
  in
  ( WT.EMatch
      ( span keywordMatch endRange,
        expression,
        RevBuffer.toList cases,
        keywordMatch,
        keywordWith ),
    !current )

(*
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
*)
and parseIf grammar state minColumn index =
  let keywordIf = rng state index in
  let condition, next = grammar.parseExpr state (index + 1) in
  let keywordThen, next =
    if tok state next = TThen then (rng state next, next + 1)
    else begin
      errExpected state next "'then'";
      (zeroWidthAtEnd (rng state next), next)
    end
  in
  let thenExpr, next = parseBlock grammar state next in
  let elseBinds = (rng state next).start.column >= minColumn in
  if tok state next = TElse && elseBinds then
    let keywordElse = rng state next in
    let elseExpr, after =
      if
        tok state (next + 1) = TIf
        && (rng state (next + 1)).start.row = (rng state next).start.row
      then parseIf grammar state minColumn (next + 1)
      else parseBlock grammar state (next + 1)
    in
    ( WT.EIf
        ( span keywordIf (WT.exprRange elseExpr),
          condition,
          thenExpr,
          Some elseExpr,
          keywordIf,
          keywordThen,
          Some keywordElse ),
      after )
  else if tok state next = TElif && elseBinds then
    let elseExpr, after = parseIf grammar state minColumn next in
    ( WT.EIf
        ( span keywordIf (WT.exprRange elseExpr),
          condition,
          thenExpr,
          Some elseExpr,
          keywordIf,
          keywordThen,
          None ),
      after )
  else
    ( WT.EIf
        ( span keywordIf (WT.exprRange thenExpr),
          condition,
          thenExpr,
          None,
          keywordIf,
          keywordThen,
          None ),
      next )

(*
   nested function definition: `let f (x: T) (y) [: R] = body` — bind a lambda
   to the name (params lowered to untyped lambda patterns, types discarded)
   no real `fun`/`->` tokens in this sugar
   Value annotations are not part of Dark. Consume the type for recovery,
   but reject the source instead of silently discarding it.
   the value is an offside block, not a single expr, so a multi-statement
   binding (`let x =\n  doThing ()\n  result`) sequences instead of gluing
   the following statement onto the first as an application argument.
   `in` is optional
*)
and parseLet grammar state index =
  let keywordLet = rng state index in
  let pattern, next = BindingPatternParser.parseLetPattern state (index + 1) in
  match pattern with
  | WT.LPVariable _ when tok state next = TLParen ->
      let patterns = RevBuffer.create ()
      and stop = ref next
      and more = ref true in
      while !more && tok state !stop = TLParen do
        if tok state (!stop + 1) = TRParen then begin
          RevBuffer.add patterns
            (WT.LPUnit (span (rng state !stop) (rng state (!stop + 1))));
          stop := !stop + 2
        end
        else begin
          let name =
            match tok state (!stop + 1) with TIdent name -> name | _ -> "_"
          in
          RevBuffer.add patterns (WT.LPVariable (rng state (!stop + 1), name));
          let closing =
            if tok state (!stop + 2) = TColon then
              snd (TypeParser.parseTypeRef state (!stop + 3))
            else !stop + 2
          in
          if tok state closing = TRParen then stop := closing + 1
          else begin
            errExpected state closing "')'";
            more := false
          end
        end
      done;
      let afterReturn =
        if tok state !stop = TColon then
          snd (TypeParser.parseTypeRef state (!stop + 1))
        else !stop
      in
      let equals, bodyStart =
        if tok state afterReturn = TEquals then
          (rng state afterReturn, afterReturn + 1)
        else begin
          errExpected state afterReturn "'=' in function binding";
          (zeroWidthAtEnd (rng state afterReturn), afterReturn)
        end
      in
      let functionBody, afterFunction = parseBlock grammar state bodyStart in
      let bodyStart =
        if tok state afterFunction = TIn then afterFunction + 1
        else afterFunction
      in
      let body, after = parseBlock grammar state bodyStart in
      let zero = zeroWidthAtEnd keywordLet in
      let lambda =
        WT.ELambda
          ( span (rng state next) (WT.exprRange functionBody),
            RevBuffer.toList patterns,
            functionBody,
            zero,
            zero )
      in
      ( WT.ELet
          ( span keywordLet (WT.exprRange body),
            pattern,
            lambda,
            body,
            keywordLet,
            equals ),
        after )
  | _ ->
      let afterAnnotation =
        if tok state next = TColon then begin
          state.diagnostics :=
            {
              code = DiagnosticCode.unexpected;
              severity = DiagError;
              range = rng state next;
              message = "Value annotations are not supported";
              related = [];
              hint = Some "remove ': Type' from this value binding";
            }
            :: !(state.diagnostics);
          snd (TypeParser.parseTypeRef state (next + 1))
        end
        else next
      in
      let equals, valueStart =
        if tok state afterAnnotation = TEquals then
          (rng state afterAnnotation, afterAnnotation + 1)
        else begin
          errExpected state afterAnnotation "'=' in let binding";
          (zeroWidthAtEnd (rng state afterAnnotation), afterAnnotation)
        end
      in
      let value, bodyStart = parseBlock grammar state valueStart in
      let bodyStart =
        if tok state bodyStart = TIn then bodyStart + 1 else bodyStart
      in
      let body, after = parseBlock grammar state bodyStart in
      ( WT.ELet
          ( span keywordLet (rng state (after - 1)),
            pattern,
            value,
            body,
            keywordLet,
            equals ),
        after )
