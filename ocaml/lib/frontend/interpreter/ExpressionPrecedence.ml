(* ExpressionPrecedence.ml - Operator precedence, offside applications, and field postfixes. *)
[@@@warning "-4"]
open Tokenizer
open ParserSupport
module WT = WrittenTypes
type grammar = {
  parseExpr : parserState -> int -> WT.expr * int;
  parsePrimary : parserState -> int -> WT.expr * int;
}
(*
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
*)
let infixBindingPower = function
  | TOr -> Some (1, false) | TAnd -> Some (2, false)
  | TEqEq | TNeq | TLt | TGt | TLte | TGte -> Some (3, false)
  | TBar -> Some (4, false) | TBitXor -> Some (5, false) | TBitAnd -> Some (6, false)
  | TShl | TShr -> Some (7, false) | TAt -> Some (8, true)
  | TPlus | TMinus | TPlusPlus -> Some (9, false)
  | TStar | TSlash | TPercent -> Some (10, false) | TStarStar -> Some (11, true)
  | _ -> None
let rec parseInfix grammar state index =
  let left, next = parseApp grammar state index in
  parseInfixRhs grammar state 1 left next
(*
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
*)
and parseInfixRhs grammar state minimum first next =
  let left = ref first and stop = ref next and more = ref true in
  while !more do
    let scope = Stack.top state.scopes in
    let anchor = scope.stmtCol and column = (rng state !stop).start.column in
    let continues = (rng state !stop).start.row = (WT.exprRange !left).end_.row || anchor < 0 ||
      (if scope.stmtExact then tok state !stop <> TMinus || column <> anchor
       else if tok state !stop = TMinus then column > anchor else column >= anchor) in
    match infixBindingPower (tok state !stop) with
    | Some _ when tok state !stop = TBar && barStartsArm state !stop -> more := false
    | Some (power, rightAssociative) when power >= minimum && continues ->
        let operator = tok state !stop and opRange = rng state !stop in
        let right, next = parseApp grammar state (!stop + 1) in
        let right, next = parseInfixRhs grammar state (if rightAssociative then power else power + 1) right next in
        let range = span (WT.exprRange !left) (WT.exprRange right) in
        left := (match operator with
        | TAt ->
            let name = { WT.range = opRange;
              modules = [({WT.range = opRange; name = "Stdlib"}, opRange); ({WT.range = opRange; name = "List"}, opRange)];
              fn = {WT.range = opRange; name = "append"} } in
            WT.EApply (range, WT.EFnName (opRange, name), [], [!left; right])
        | _ -> WT.EInfix (range, (opRange, (match infixOf operator with Some infix -> infix | None -> Crash.crash "Validated precedence operator has no infix operation")), !left, right));
        stop := next
    | _ -> more := false
  done;
  !left, !stop
and parseCtorParenFields grammar state index sink =
  let opening = rng state index in
  withStmtColExact state (rng state (index + 1)).start.column (fun () ->
    let stop = ref (index + 1) and more = ref true in
    while !more && tok state !stop <> TRParen && tok state !stop <> TEOF && not (declBarrier state !stop) do
      let value, after = grammar.parseExpr state !stop in
      RevBuffer.add sink value;
      if tok state after = TComma then stop := after + 1
      else if after > !stop then begin
        if tok state after <> TRParen then requireElementSeparator state (WT.exprRange value) after "a comma or newline between constructor fields";
        stop := after
      end else more := false
    done;
    let after = if tok state !stop = TRParen then !stop + 1
      else begin errUnclosed state !stop ")" "(" opening; !stop end in
    if RevBuffer.length sink = 0 then RevBuffer.add sink (WT.EUnit (span opening (rng state (after - 1))));
    after)
(*
   A prefix operator: `op operand` → `Builtin.<name> operand`, with the operand
   parsed as a whole APPLICATION so `op f x` is `op (f x)`. Shared by `!`, `~`
   and the non-literal case of unary `-`.
*)
and parsePrefixBuiltin grammar state index builtinName =
  let operand, after = parseApp grammar state (index + 1) in
  let range = rng state index in
  let name = { WT.range; modules = [({WT.range; name = "Builtin"}, range)]; fn = {WT.range; name = builtinName} } in
  WT.EApply (span range (WT.exprRange operand), WT.EFnName (range, name), [], [operand]), after
and parseApp grammar state index =
  let callee, next = parseAtom grammar state index in
  let args = RevBuffer.create () and stop = ref next in
  (match callee with
  | WT.EEnum (_, _, _, [], _) when tok state next = TLParen && offsideContinues state index next ->
      stop := parseCtorParenFields grammar state next args
  | _ -> ());
  let accepts = match callee with
    | WT.EVariable _ | WT.EFnName _ | WT.EApply _ | WT.ERecordFieldAccess _ | WT.ELambda _ | WT.EEnum (_, _, _, [], _) -> true
    | _ -> false in
  if accepts then
    while (canStartAtom (tok state !stop) || isNegLitArg state !stop) && offsideContinues state index !stop do
      let value, after = parseAtom grammar state !stop in RevBuffer.add args value; stop := after
    done;
  if RevBuffer.length args = 0 then callee, !stop else
  let ending = match RevBuffer.last args with Some value -> WT.exprRange value | None -> Crash.crash "Empty application arguments" in
  match callee with
  | WT.EApply (range, fn, typeArgs, []) -> WT.EApply (span range ending, fn, typeArgs, RevBuffer.toList args), !stop
  | WT.EEnum (range, typeName, caseName, [], dot) -> WT.EEnum (span range ending, typeName, caseName, RevBuffer.toList args, dot), !stop
  | _ ->
      let lhs = match callee with
        | WT.EVariable (range, name) -> WT.EFnName (range, {WT.range; modules = []; fn = {WT.range; name}})
        | other -> other in
      WT.EApply (span (WT.exprRange lhs) ending, lhs, [], RevBuffer.toList args), !stop
and parseAtom grammar state index =
  let value, next = grammar.parsePrimary state index in
  parsePostfix state value next
(*
   postfix `.field` record access (left-assoc, chains)
*)
and parsePostfix state value index =
  match tok state index, tok state (index + 1) with
  | TDot, TIdent name ->
      let dot = rng state index and field = rng state (index + 1) in
      parsePostfix state (WT.ERecordFieldAccess (span (WT.exprRange value) field, value, (field, name), dot)) (index + 2)
  | _ -> value, index
