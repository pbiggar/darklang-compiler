(* PatternParser.ml - Preserve or/tuple/cons precedence and literal pattern ranges. *)
(* Fallback cases are the frozen parser's intentional recovery boundaries. *)
[@@@warning "-4"]

open Tokenizer
open ParserSupport
module WT = WrittenTypes

let last buffer =
  match RevBuffer.last buffer with
  | Some value -> value
  | None -> Crash.crash "Empty pattern collector"

let upperName = NameSyntax.isUpperIdentifier

(*
   or-level: `p1 | p2 | …` (stops at `->` / `when`)
   top level: a bare tuple `a, b` (comma-separated, no parens); else an or-pattern
   A full match-arm pattern. Precedence, from lowest to highest: `|` (or) is LOOSEST, then
   `,` (tuple), then `::` (cons). So `1, 2 | 3, 4` is `(1,2) | (3,4)` — an or of
   two tuples, NOT a 3-tuple with an or in the middle. Hence `|` is the OUTER
   level here, wrapping tuples (`parsePatternTuple`).
*)
let rec parseMatchPattern state index =
  let first, next = parsePatternTuple state index in
  if tok state next <> TBar then (first, next)
  else
    let patterns = RevBuffer.create () and current = ref next in
    RevBuffer.add patterns first;
    while tok state !current = TBar do
      let pattern, stop = parsePatternTuple state (!current + 1) in
      RevBuffer.add patterns pattern;
      current := if stop > !current then stop else !current + 1
    done;
    ( WT.MPOr
        ( span (WT.mpRange first) (WT.mpRange (last patterns)),
          RevBuffer.toList patterns ),
      !current )

(*
   tuple level: `p1, p2, …` (bare — no parens). Elements are cons-patterns; `|`
   binds looser (handled above) so it can't appear as a bare tuple element.
   bare tuple: no parens
*)
and parsePatternTuple state index =
  let first, next = parsePatternCons state index in
  if tok state next <> TComma then (first, next)
  else
    let comma = rng state next in
    let second, stop = parsePatternCons state (next + 1) in
    let rest = RevBuffer.create ()
    and current = ref stop
    and scanning = ref true in
    while !scanning && tok state !current = TComma do
      let comma = rng state !current in
      let pattern, stop = parsePatternCons state (!current + 1) in
      RevBuffer.add rest (comma, pattern);
      if stop > !current then current := stop else scanning := false
    done;
    let zero = zeroWidthAtEnd (WT.mpRange first) in
    let final = if RevBuffer.length rest > 0 then snd (last rest) else second in
    ( WT.MPTuple
        ( span (WT.mpRange first) (WT.mpRange final),
          first,
          comma,
          second,
          RevBuffer.toList rest,
          zero,
          zero ),
      !current )

(*
   or of cons-patterns, NO tuple — used for enum-ctor fields, where a bare `,`
   separates FIELDS (`Case(a, b)` = two fields), not tuple elements.
*)
and parsePatternOr state index =
  let first, next = parsePatternCons state index in
  if tok state next <> TBar then (first, next)
  else
    let patterns = RevBuffer.create () and current = ref next in
    RevBuffer.add patterns first;
    while tok state !current = TBar do
      let pattern, stop = parsePatternCons state (!current + 1) in
      RevBuffer.add patterns pattern;
      current := if stop > !current then stop else !current + 1
    done;
    ( WT.MPOr
        ( span (WT.mpRange first) (WT.mpRange (last patterns)),
          RevBuffer.toList patterns ),
      !current )

(*
   cons-level: `h :: t` (right-assoc)
*)
and parsePatternCons state index =
  let head, next = parsePatternBase state index in
  if tok state next <> TCons then (head, next)
  else
    let consRange = rng state next in
    let tail, stop = parsePatternCons state (next + 1) in
    ( WT.MPListCons
        (span (WT.mpRange head) (WT.mpRange tail), head, tail, consRange),
      stop )

and parsePatternBase state index =
  if tooDeep state index || outOfFuel state index then
    (WT.MPError (rng state index), state.tokenCount - 1)
  else begin
    state.depth <- state.depth + 1;
    let result = parsePatternBaseInner state index in
    state.depth <- state.depth - 1;
    result
  end

(*
   `128y` etc. — only valid negated
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
   recovery: an explicit error-hole node; leave closing/separating/decl-start
   tokens for the enclosing construct
*)
and parsePatternBaseInner state index =
  checkBareMinMagnitude state index;
  match tok state index with
  | TUnderscore -> (WT.MPVariable (rng state index, "_"), index + 1)
  | TInt value ->
      (WT.MPInt (rng state index, (rng state index, value)), index + 1)
  | TInt64 value ->
      let digits, suffix = splitTrailingRange state index 1 in
      (WT.MPInt64 (rng state index, (digits, value), suffix), index + 1)
  | TInt32 value ->
      let digits, suffix = splitTrailingRange state index 1 in
      (WT.MPInt32 (rng state index, (digits, value), suffix), index + 1)
  | TInt8 value ->
      let digits, suffix = splitTrailingRange state index 1 in
      (WT.MPInt8 (rng state index, (digits, value), suffix), index + 1)
  | TUInt8 value ->
      let digits, suffix = splitTrailingRange state index 2 in
      (WT.MPUInt8 (rng state index, (digits, value), suffix), index + 1)
  | TInt16 value ->
      let digits, suffix = splitTrailingRange state index 1 in
      (WT.MPInt16 (rng state index, (digits, value), suffix), index + 1)
  | TUInt16 value ->
      let digits, suffix = splitTrailingRange state index 2 in
      (WT.MPUInt16 (rng state index, (digits, value), suffix), index + 1)
  | TUInt32 value ->
      let digits, suffix = splitTrailingRange state index 2 in
      (WT.MPUInt32 (rng state index, (digits, value), suffix), index + 1)
  | TUInt64 value ->
      let digits, suffix = splitTrailingRange state index 2 in
      (WT.MPUInt64 (rng state index, (digits, value), suffix), index + 1)
  | TInt128 value ->
      let digits, suffix = splitTrailingRange state index 1 in
      (WT.MPInt128 (rng state index, (digits, value), suffix), index + 1)
  | TUInt128 value ->
      let digits, suffix = splitTrailingRange state index 1 in
      (WT.MPUInt128 (rng state index, (digits, value), suffix), index + 1)
  | TMinus -> (
      let range = span (rng state index) (rng state (index + 1)) in
      match tok state (index + 1) with
      | TInt value -> (WT.MPInt (range, (range, Z.neg value)), index + 2)
      | TInt64 value ->
          let digits, suffix = splitTrailingRange state (index + 1) 1 in
          ( WT.MPInt64
              (range, (span (rng state index) digits, Int64.neg value), suffix),
            index + 2 )
      | TInt32 value ->
          let digits, suffix = splitTrailingRange state (index + 1) 1 in
          ( WT.MPInt32
              (range, (span (rng state index) digits, Int32.neg value), suffix),
            index + 2 )
      | TInt8 value ->
          let digits, suffix = splitTrailingRange state (index + 1) 1 in
          ( WT.MPInt8
              ( range,
                (span (rng state index) digits, FixedInteger.negateInt8 value),
                suffix ),
            index + 2 )
      | TInt16 value ->
          let digits, suffix = splitTrailingRange state (index + 1) 1 in
          ( WT.MPInt16
              ( range,
                (span (rng state index) digits, FixedInteger.negateInt16 value),
                suffix ),
            index + 2 )
      | TInt128 value ->
          let digits, suffix = splitTrailingRange state (index + 1) 1 in
          ( WT.MPInt128
              ( range,
                (span (rng state index) digits, FixedInteger.negateInt128 value),
                suffix ),
            index + 2 )
      | TFloat value ->
          let whole, fraction = floatParts state (index + 1) value in
          (WT.MPFloat (range, true, whole, fraction), index + 2)
      | _ ->
          errExpected state index "a pattern";
          (WT.MPError (rng state index), index + 1))
  | TFloat value ->
      let whole, fraction = floatParts state index value in
      (WT.MPFloat (rng state index, value < 0., whole, fraction), index + 1)
  | TTrue -> (WT.MPBool (rng state index, true), index + 1)
  | TFalse -> (WT.MPBool (rng state index, false), index + 1)
  | TStringLit value ->
      let delimiter =
        if String.starts_with ~prefix:"\"\"\"" (txt state index) then "\"\"\""
        else "\""
      in
      let opening, contents, closing =
        literalTextRanges state index delimiter
      in
      ( WT.MPString (rng state index, Some (contents, value), opening, closing),
        index + 1 )
  | TCharLit value ->
      let opening, contents, closing = literalTextRanges state index "'" in
      ( WT.MPChar (rng state index, Some (contents, value), opening, closing),
        index + 1 )
  | TLParen -> (
      if tok state (index + 1) = TRParen then
        (WT.MPUnit (span (rng state index) (rng state (index + 1))), index + 2)
      else
        let opening = rng state index in
        let inner, next = parseMatchPattern state (index + 1) in
        let closing, stop =
          if tok state next = TRParen then (rng state next, next + 1)
          else begin
            errUnclosed state next ")" "(" opening;
            (zeroWidthAtEnd (rng state next), next)
          end
        in
        match inner with
        | WT.MPTuple (_, first, comma, second, rest, _, _) ->
            ( WT.MPTuple
                ( span opening closing,
                  first,
                  comma,
                  second,
                  rest,
                  opening,
                  closing ),
              stop )
        | _ -> (inner, stop))
  | TLBracket ->
      let opening = rng state index in
      let elements = RevBuffer.create ()
      and current = ref (index + 1)
      and scanning = ref true in
      while
        !scanning
        && tok state !current <> TRBracket
        && tok state !current <> TEOF
      do
        let pattern, stop = parsePatternBase state !current in
        if stop = !current then begin
          err state DiagnosticCode.unexpected !current
            ("unexpected " ^ foundDesc state !current ^ " in list pattern");
          scanning := false
        end
        else if tok state stop = TComma then begin
          RevBuffer.add elements (pattern, Some (rng state stop));
          current := stop + 1
        end
        else if tok state stop = TSemicolon then begin
          errListSemicolon state stop "list-pattern elements";
          RevBuffer.add elements (pattern, Some (rng state stop));
          current := stop + 1
        end
        else begin
          RevBuffer.add elements (pattern, None);
          if tok state stop <> TRBracket then
            requireElementSeparator state (WT.mpRange pattern) stop
              "a comma or newline between list-pattern elements";
          current := stop
        end
      done;
      let closing, stop =
        if tok state !current = TRBracket then (rng state !current, !current + 1)
        else begin
          errUnclosed state !current "]" "[" opening;
          (zeroWidthAtEnd (rng state !current), !current)
        end
      in
      ( WT.MPList
          (span opening closing, RevBuffer.toList elements, opening, closing),
        stop )
  | TIdent name when upperName name ->
      let modules, final, next = parseQualified state index in
      if modules <> [] then begin
        let fullPath =
          String.concat "."
            (List.map (fun ((name : WT.identifier), _) -> name.WT.name) modules
            @ [ final.WT.name ])
        in
        err state DiagnosticCode.pattern index
          (Printf.sprintf
             "Invalid match pattern. Enum patterns use the unqualified case \
              name (e.g. `| %s n`), not a qualified path like `| %s n`."
             final.WT.name fullPath)
      end;
      if tok state next = TLParen then begin
        let opening = rng state next in
        let fields = RevBuffer.create ()
        and current = ref (next + 1)
        and scanning = ref true in
        while
          !scanning
          && tok state !current <> TRParen
          && tok state !current <> TEOF
        do
          let pattern, stop = parsePatternOr state !current in
          RevBuffer.add fields pattern;
          if tok state stop = TComma then current := stop + 1
          else if stop > !current then begin
            if tok state stop <> TRParen then
              requireElementSeparator state (WT.mpRange pattern) stop
                "a comma or newline between constructor-pattern fields";
            current := stop
          end
          else scanning := false
        done;
        let stop =
          if tok state !current = TRParen then !current + 1
          else begin
            errUnclosed state !current ")" "(" opening;
            !current
          end
        in
        if RevBuffer.length fields = 0 then
          RevBuffer.add fields
            (WT.MPUnit (span opening (rng state (max next (stop - 1)))));
        ( WT.MPEnum
            ( span (rng state index) (rng state (max next (stop - 1))),
              (final.WT.range, final.WT.name),
              RevBuffer.toList fields ),
          stop )
      end
      else begin
        let fields = RevBuffer.create () and current = ref next in
        while
          canStartPattern (tok state !current)
          && offsideContinues state index !current
        do
          let pattern, stop = parsePatternBase state !current in
          RevBuffer.add fields pattern;
          current := if stop > !current then stop else !current + 1
        done;
        let endRange =
          if RevBuffer.length fields > 0 then WT.mpRange (last fields)
          else final.WT.range
        in
        ( WT.MPEnum
            ( span (rng state index) endRange,
              (final.WT.range, final.WT.name),
              RevBuffer.toList fields ),
          !current )
      end
  | TIdent name -> (WT.MPVariable (rng state index, name), index + 1)
  | TDotDotDot ->
      err state DiagnosticCode.unexpected index
        "'...' rest patterns are reserved but not supported";
      (WT.MPError (rng state index), index + 1)
  | _ ->
      errExpected state index "a pattern";
      let location = (rng state index).start in
      ( WT.MPError { start = location; end_ = location },
        if index < state.tokenCount && not (isRecoveryBarrier (tok state index))
        then index + 1
        else index )
