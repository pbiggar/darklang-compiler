(* BindingPatternParser.ml - Binding patterns with original grouping and recovery. *)
[@@@warning "-4"]
open Tokenizer
open ParserSupport
module WT = WrittenTypes
(*
   a simple binding pattern: variable / wildcard / `()` unit
   tuple pattern `(a, b, …)` or a parenthesized pattern `(a)`
   recovery: keep a benign binder (LetPattern has no error case yet);
   leave closing/separating/decl-start tokens for the enclosing construct
*)
let rec parseLetPattern state index =
  match tok state index with
  | TUnderscore -> WT.LPWildcard (rng state index), index + 1
  | TIdent name -> WT.LPVariable (rng state index, name), index + 1
  | TLParen when tok state (index + 1) = TRParen ->
      WT.LPUnit (span (rng state index) (rng state (index + 1))), index + 2
  | TLParen ->
      let opening = rng state index in
      let first, next = parseLetPattern state (index + 1) in
      if tok state next = TComma then
        let comma = rng state next in
        let second, after = parseLetPattern state (next + 1) in
        let rest = RevBuffer.create () and stop = ref after in
        while tok state !stop = TComma do
          let comma = rng state !stop in
          let pattern, after = parseLetPattern state (!stop + 1) in
          RevBuffer.add rest (comma, pattern);
          stop := if after > !stop then after else !stop + 1
        done;
        let closing, after = if tok state !stop = TRParen then rng state !stop, !stop + 1
          else begin errUnclosed state !stop ")" "(" opening; zeroWidthAtEnd (rng state !stop), !stop end in
        WT.LPTuple (span opening closing, first, comma, second, RevBuffer.toList rest, opening, closing), after
      else if tok state next = TRParen then first, next + 1
      else begin errUnclosed state next ")" "(" opening; first, next end
  | _ ->
      errExpected state index "a pattern";
      WT.LPVariable (rng state index, "_"),
      (if index < state.tokenCount && not (isRecoveryBarrier (tok state index)) then index + 1 else index)
