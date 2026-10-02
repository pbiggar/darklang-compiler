(* ExpressionCollections.ml - Exact separators, offside element scopes, and delimiter recovery. *)
[@@@warning "-4"]
open Tokenizer
open ParserSupport
module WT = WrittenTypes
type parseExpr = parserState -> int -> WT.expr * int
let close state index token closing openingText opening =
  if tok state index = token then rng state index, index + 1
  else begin errUnclosed state index closing openingText opening; zeroWidthAtEnd (rng state index), index end
let parseParen parseExpr state index =
  let opening = rng state index in
  if tok state (index + 1) = TRParen then WT.EUnit (span opening (rng state (index + 1))), index + 2
  else if (Option.is_some (infixOf (tok state (index + 1))) || tok state (index + 1) = TAt) && tok state (index + 2) = TRParen then
    let operator = tok state (index + 1) and opRange = rng state (index + 1) in
    let range = span opening (rng state (index + 2)) in
    let zero = zeroWidthAtEnd opRange in
    let a = WT.EVariable (zero, "a") and b = WT.EVariable (zero, "b") in
    let body = match operator with
      | TAt ->
          let name = {WT.range = opRange; modules = [({WT.range = opRange; name = "Stdlib"}, opRange); ({WT.range = opRange; name = "List"}, opRange)]; fn = {WT.range = opRange; name = "append"}} in
          WT.EApply (range, WT.EFnName (opRange, name), [], [a; b])
      | _ -> WT.EInfix (range, (opRange, Option.get (infixOf operator)), a, b) in
    WT.ELambda (range, [WT.LPVariable (zero, "a"); WT.LPVariable (zero, "b")], body, zero, zero), index + 3
  else withStmtColExact state (rng state (index + 1)).start.column (fun () ->
    let first, next = parseExpr state (index + 1) in
    if tok state next = TComma then
      let comma = rng state next in
      let second, after = parseExpr state (next + 1) in
      let rest = RevBuffer.create () and stop = ref after and more = ref true in
      while !more && tok state !stop = TComma do
        let comma = rng state !stop in
        let value, after = parseExpr state (!stop + 1) in
        RevBuffer.add rest (comma, value);
        if after = !stop + 1 && tok state after = tok state !stop then more := false else stop := after
      done;
      let closing, after = close state !stop TRParen ")" "(" opening in
      WT.ETuple (span opening closing, first, comma, second, RevBuffer.toList rest, opening, closing), after
    else
      let statements = RevBuffer.create () and stop = ref next and more = ref true in
      RevBuffer.add statements first;
      while !more && tok state !stop <> TRParen && tok state !stop <> TEOF && tok state !stop <> TComma && not (declBarrier state !stop) do
        let value, after = parseExpr state !stop in
        if after > !stop then begin RevBuffer.add statements value; stop := after end else more := false
      done;
      let folded = match List.rev (RevBuffer.toList statements) with
        | final :: earlier -> List.fold_left (fun acc value -> WT.EStatement (span (WT.exprRange value) (WT.exprRange acc), value, acc)) final earlier
        | [] -> Crash.crash "Empty grouped statements" in
      if tok state !stop = TRParen then folded, !stop + 1
      else begin errUnclosed state !stop ")" "(" opening; folded, !stop end)
let parseList parseExpr state index =
  let opening = rng state index in
  withElementScope state (fun () ->
    let elements = RevBuffer.create () and stop = ref (index + 1) and more = ref true in
    while !more && tok state !stop <> TRBracket && tok state !stop <> TEOF && not (declBarrier state !stop) do
      setStmtCol state (rng state !stop).start.column;
      let value, after = parseExpr state !stop in
      if after = !stop then begin err state DiagnosticCode.unexpected !stop ("unexpected " ^ foundDesc state !stop ^ " in list"); more := false end
      else if tok state after = TComma || tok state after = TSemicolon then begin
        if tok state after = TSemicolon then errListSemicolon state after "list elements";
        RevBuffer.add elements (value, Some (rng state after)); stop := after + 1
      end else begin
        RevBuffer.add elements (value, None);
        if tok state after <> TRBracket then requireElementSeparator state (WT.exprRange value) after "a comma or newline between list elements";
        stop := after
      end
    done;
    let closing, after = close state !stop TRBracket "]" "[" opening in
    WT.EList (span opening closing, RevBuffer.toList elements, opening, closing), after)
let parseRecord parseExpr state typeName nameRange index =
  let opening = rng state index in
  withElementScope state (fun () ->
    let fields = RevBuffer.create () and stop = ref (index + 1) and more = ref true in
    while !more && tok state !stop <> TRBrace && tok state !stop <> TEOF do
      match tok state !stop, tok state (!stop + 1) with
      | TIdent name, TEquals ->
          let nameRange = rng state !stop in
          setStmtCol state nameRange.start.column;
          let value, after = parseExpr state (!stop + 2) in
          RevBuffer.add fields (span nameRange (WT.exprRange value), (nameRange, name), value);
          if tok state after = TSemicolon || tok state after = TComma then stop := after + 1
          else if after > !stop then begin
            if tok state after <> TRBrace then requireElementSeparator state (WT.exprRange value) after "a comma, semicolon, or newline between record fields";
            stop := after
          end else more := false
      | _ -> errExpected state !stop "a record field 'name = value'"; more := false
    done;
    let closing, after = close state !stop TRBrace "}" "{" opening in
    WT.ERecord (span nameRange closing, typeName, RevBuffer.toList fields, opening, closing), after)
let parseDict parseExpr state keyword index =
  let opening = rng state index in
  withElementScope state (fun () ->
    let entries = RevBuffer.create () and stop = ref (index + 1) and more = ref true in
    while !more && tok state !stop <> TRBrace && tok state !stop <> TEOF do
      setStmtCol state (rng state !stop).start.column;
      let key, afterKey = parseExpr state !stop in
      if afterKey <= !stop then begin errExpected state !stop "a dict entry 'key: value'"; more := false end
      else if tok state afterKey <> TColon then begin errExpected state afterKey "`:` between a dict key and its value"; more := false end
      else begin
        let colon = rng state afterKey in
        let value, after = parseExpr state (afterKey + 1) in
        RevBuffer.add entries (span (WT.exprRange key) (WT.exprRange value), key, colon, value);
        if tok state after = TSemicolon || tok state after = TComma then stop := after + 1
        else if after > afterKey then begin
          if tok state after <> TRBrace then requireElementSeparator state (WT.exprRange value) after "a comma, semicolon, or newline between dict entries";
          stop := after
        end else more := false
      end
    done;
    let closing, after = close state !stop TRBrace "}" "{" opening in
    WT.EDict (span keyword closing, RevBuffer.toList entries, keyword, opening, closing), after)
let parseRecordUpdate parseExpr state index =
  let opening = rng state index in
  withElementScope state (fun () ->
    let record, next = parseExpr state (index + 1) in
    let keyword, next = if tok state next = TWith then rng state next, next + 1
      else begin errExpected state next "'with' in record update"; zeroWidthAtEnd (rng state next), next end in
    let updates = RevBuffer.create () and stop = ref next and more = ref true in
    while !more && tok state !stop <> TRBrace && tok state !stop <> TEOF do
      match tok state !stop, tok state (!stop + 1) with
      | TIdent name, TEquals ->
          let nameRange = rng state !stop and equals = rng state (!stop + 1) in
          setStmtCol state nameRange.start.column;
          let value, after = parseExpr state (!stop + 2) in
          RevBuffer.add updates ((nameRange, name), equals, value);
          if tok state after = TSemicolon || tok state after = TComma then stop := after + 1
          else if after > !stop then begin
            if tok state after <> TRBrace then requireElementSeparator state (WT.exprRange value) after "a comma, semicolon, or newline between record-update fields";
            stop := after
          end else more := false
      | _ -> errExpected state !stop "a record-update field 'name = value'"; more := false
    done;
    if RevBuffer.length updates = 0 then errExpected state next "at least one 'field = value' in a record update";
    let closing, after = close state !stop TRBrace "}" "{" opening in
    WT.ERecordUpdate (span opening closing, record, RevBuffer.toList updates, opening, closing, keyword), after)
