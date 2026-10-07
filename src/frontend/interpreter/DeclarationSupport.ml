(* DeclarationSupport.ml - Preserve parameter docs, delimiter ranges, and effect diagnostics. *)
[@@@warning "-4"]

open Tokenizer
open ParserSupport
module WT = WrittenTypes

(*
   a function parameter `(name: Type)` or `()`
   `_` names a parameter you don't intend to use. It's also what `()` is stored as, so accepting it
   here is what makes `(_: Unit)` and `()` both parse to the same thing and either form round-trip.
   A `///` for a parameter attaches to whichever token follows it: the `(` when the comment is
   written above the whole parameter, the NAME when it is written just inside the paren. Both
   spellings occur, so both are read.
*)
let parseParam state index =
  let opening = rng state index in
  if tok state (index + 1) = TRParen then
    (WT.FPUnit (span opening (rng state (index + 1))), index + 2)
  else
    let name =
      match tok state (index + 1) with
      | TIdent name -> { WT.range = rng state (index + 1); name }
      | TUnderscore -> { WT.range = rng state (index + 1); name = "_" }
      | _ ->
          errExpected state (index + 1) "a parameter name";
          { WT.range = rng state (index + 1); name = "_" }
    in
    let colon, next =
      if tok state (index + 2) = TColon then (rng state (index + 2), index + 3)
      else begin
        errExpected state (index + 2) "':'";
        (zeroWidthAtEnd (rng state (index + 2)), index + 2)
      end
    in
    let typ, next = TypeParser.parseTypeRef state next in
    let closing, after =
      if tok state next = TRParen then (rng state next, next + 1)
      else begin
        errUnclosed state next ")" "(" opening;
        (zeroWidthAtEnd (rng state next), next)
      end
    in
    let atParen = docOf state index in
    let description =
      if atParen <> "" then atParen else docOf state (index + 1)
    in
    ( WT.FPNormal
        (span opening closing, name, typ, opening, colon, closing, description),
      after )

(*
   A declaration-scope function (`let f (p: T) … : R = body`) or value
   (`val x = body`). Legacy module-level `let x = body` also comes through here
   to retain a recovery DValue beside its focused diagnostic.
   `:{Http, Clock}` immediately after a declaration's return colon. Absent
   means no ceiling; `{}` means effect-free. Unknown names are diagnostics,
   not wildcards: the row fails closed.
*)
let parseEffectRow state index =
  if tok state index <> TLBrace then (None, index)
  else
    let names = RevBuffer.create () and stop = ref (index + 1) in
    let more = ref (tok state !stop <> TRBrace) in
    while !more do
      (match tok state !stop with
      | TIdent name ->
          if not (List.mem name effectCaseNames) then
            err state DiagnosticCode.effect_ !stop
              ("unknown effect '" ^ name ^ "'; effects are "
              ^ String.concat ", " effectCaseNames);
          RevBuffer.add names { WT.range = rng state !stop; name };
          incr stop;
          if tok state !stop = TComma then incr stop
          else if tok state !stop <> TRBrace then begin
            errExpected state !stop "',' or '}' in the effect row";
            more := false
          end
      | _ ->
          errExpected state !stop "an effect name";
          more := false);
      if tok state !stop = TRBrace then more := false
    done;
    ( Some (RevBuffer.toList names),
      if tok state !stop = TRBrace then !stop + 1 else !stop )
