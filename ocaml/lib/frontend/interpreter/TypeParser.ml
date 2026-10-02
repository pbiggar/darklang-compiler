(* TypeParser.ml - Preserve function/tuple precedence and split generic closers. *)
[@@@warning "-4"]
open Tokenizer
open ParserSupport
module WT = WrittenTypes
let upperName name =
  let units = HostText.utf16Units name in
  Array.length units > 0 && HostText.isUpperUnit units.(0)
let variable range name =
  let tick = { start = range.start; end_ = { range.start with column = range.start.column + 1 } } in
  let nameRange = { start = { range.start with column = range.start.column + 1 }; end_ = range.end_ } in
  WT.TVariable (range, tick, (nameRange, name))
let requireAdjacent state index previous =
  let next = rng state index in
  if next.start.row <> previous.end_.row || next.start.column <> previous.end_.column then
    err state DiagnosticCode.expected index "Generic type arguments must be adjacent to the type name"
let rec parseTypeRef state index =
  if tooDeep state index || outOfFuel state index then WT.TUnit (rng state index), state.tokenCount - 1
  else begin
    state.pendingGt <- 0;
    state.depth <- state.depth + 1;
    let result = parseFnType state index in
    state.depth <- state.depth - 1;
    result
  end
and parseFnType state index =
  let first, next = parseTupleType state index in
  if tok state next <> TArrow then first, next else
  let args = RevBuffer.create () and current = ref first and stop = ref next in
  while tok state !stop = TArrow do
    RevBuffer.add args (!current, rng state !stop);
    let value, after = parseTupleType state (!stop + 1) in
    current := value; stop := if after > !stop then after else !stop + 1
  done;
  WT.TFn (span (WT.typeReferenceRange first) (WT.typeReferenceRange !current), RevBuffer.toList args, !current), !stop
and parseTupleType state index =
  let first, next = parseAtomType state index in
  if tok state next <> TStar || state.pendingGt > 0 then first, next else
  let star = rng state next in
  let second, after = parseAtomType state (next + 1) in
  let rest = RevBuffer.create () and stop = ref after in
  while tok state !stop = TStar && state.pendingGt = 0 do
    let range = rng state !stop in
    let value, after = parseAtomType state (!stop + 1) in
    RevBuffer.add rest (range, value); stop := if after > !stop then after else !stop + 1
  done;
  let final = match RevBuffer.last rest with Some (_, value) -> value | None -> second in
  let zero = zeroWidthAtEnd (WT.typeReferenceRange first) in
  WT.TTuple (span (WT.typeReferenceRange first) (WT.typeReferenceRange final), first, star, second, RevBuffer.toList rest, zero, zero), !stop
and parseTypeArgs state index =
  if tok state index <> TLt then [], None, index else
  let args = RevBuffer.create () in
  let first, next = parseTypeRef state (index + 1) in
  RevBuffer.add args first;
  let stop = ref next in
  while tok state !stop = TComma && state.pendingGt = 0 do
    let value, after = parseTypeRef state (!stop + 1) in
    RevBuffer.add args value; stop := if after > !stop then after else !stop + 1
  done;
  let close, after = expectGt state !stop in
  RevBuffer.toList args, Some close, after
and parseAtomType state index =
  match tok state index with
  | TLParen ->
      let opening = rng state index in
      let inner, next = parseFnType state (index + 1) in
      if tok state next = TRParen then
        let closing = rng state next in
        (match inner with
        | WT.TTuple (_, first, star, second, rest, _, _) ->
            WT.TTuple (span opening closing, first, star, second, rest, opening, closing), next + 1
        | other -> other, next + 1)
      else begin errUnclosed state next ")" "(" opening; inner, next end
  | TIdent "List" when tok state (index + 1) = TLt ->
      let keyword = rng state index and opening = rng state (index + 1) in
      requireAdjacent state (index + 1) keyword;
      let inner, next = parseTypeRef state (index + 2) in
      let closing, after = expectGt state next in
      WT.TList (span keyword closing, keyword, opening, inner, closing), after
  | TIdent "Dict" when tok state (index + 1) = TLt ->
      let keyword = rng state index and opening = rng state (index + 1) in
      requireAdjacent state (index + 1) keyword;
      let key, next = parseTypeRef state (index + 2) in
      let comma, next = if tok state next = TComma then rng state next, next + 1
        else begin errExpected state next "a comma between Dict's key and value types"; zeroWidthAtEnd (rng state next), next end in
      let value, next = parseTypeRef state next in
      let closing, after = expectGt state next in
      WT.TDict (span keyword closing, keyword, opening, key, comma, value, closing), after
  | TIdent name ->
      (match WT.primTypeFromName name with
      | Some constructor when not (String.starts_with ~prefix:"'" (txt state index)) -> constructor (rng state index), index + 1
      | _ when String.starts_with ~prefix:"'" (txt state index) -> variable (rng state index) name, index + 1
      | _ when upperName name ->
          let modules, final, next = parseQualified state index in
          if tok state next = TLt then requireAdjacent state next final.WT.range;
          let typeArgs, closing, after = parseTypeArgs state next in
          let ending = match closing, List.rev typeArgs with
            | Some range, _ -> range
            | None, value :: _ -> WT.typeReferenceRange value
            | None, [] -> final.WT.range in
          WT.TCustom { WT.range = span (rng state index) ending; modules; typ = final; typeArgs }, after
      | _ -> variable (rng state index) name, index + 1)
  | _ ->
      errExpected state index "a type";
      WT.TUnit (rng state index), (if index < state.tokenCount && not (isRecoveryBarrier (tok state index)) then index + 1 else index)
