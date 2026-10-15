(* TypeParameterParser.ml - Interpreter generic parameter and trait-bound grammar. *)
[@@@warning "-4"]

open Tokenizer
open ParserSupport
module WT = WrittenTypes

let parseBoundTrait state index =
  let reference, after = TypeParser.parseAtomType state index in
  match reference with
  | WT.TCustom name -> (Some name, after)
  | other ->
      state.diagnostics :=
        {
          code = DiagnosticCode.bound;
          severity = DiagError;
          range = WT.typeReferenceRange other;
          message = "A bound names a trait, not a type";
          related = [];
          hint =
            Some
              "write `'a: Show`, where `Show` is a trait; `'a: String` is not \
               a bound";
        }
        :: !(state.diagnostics);
      (None, after)

let parseTypeParams state index =
  if tok state index <> TLt then ([], [], index)
  else begin
    if
      index > 0
      && ((rng state index).start.row <> (rng state (index - 1)).end_.row
         || (rng state index).start.column
            <> (rng state (index - 1)).end_.column)
    then
      err state DiagnosticCode.expected index
        "Generic type parameters must be adjacent to the declaration name";
    let bounds = ref [] in
    let names = ref []
    and next = ref (index + 1)
    and expectingName = ref true in
    while
      state.pendingGt = 0
      && not (List.mem (tok state !next) [ TGt; TShr; TEOF ])
    do
      match (!expectingName, tok state !next) with
      | true, TIdent name ->
          if not (String.starts_with ~prefix:"'" (txt state !next)) then
            err state DiagnosticCode.expected !next
              "Declared type parameters must start with an apostrophe, such as \
               'a";
          names := (name, rng state !next) :: !names;
          expectingName := false;
          incr next;
          if tok state !next = TColon then begin
            let colon = rng state !next in
            incr next;
            let more = ref true in
            while !more do
              let trait_, after = parseBoundTrait state !next in
              Option.iter
                (fun (trait_ : WT.qualifiedTypeIdentifier) ->
                  bounds :=
                    {
                      WT.range = span colon trait_.WT.range;
                      param = name;
                      trait_;
                      symbolColon = colon;
                    }
                    :: !bounds)
                trait_;
              next := if after > !next then after else !next + 1;
              if tok state !next = TPlus then incr next else more := false
            done
          end
      | false, TComma ->
          expectingName := true;
          incr next
      | false, TIdent _ ->
          errExpected state !next "a comma between type parameters";
          expectingName := true
      | _ ->
          errExpected state !next "a type parameter";
          incr next
    done;
    if !names = [] then
      errExpected state (index + 1) "at least one type parameter"
    else if !expectingName then
      errExpected state !next "a type parameter after ','";
    let stop =
      if state.pendingGt > 0 then begin
        state.pendingGt <- state.pendingGt - 1;
        !next
      end
      else if tok state !next = TGt then !next + 1
      else if tok state !next = TShr then begin
        errExpected state !next "'>' to close the type-parameter list";
        !next + 1
      end
      else begin
        errExpected state !next "'>' to close the type-parameter list";
        !next
      end
    in
    (List.rev !names, List.rev !bounds, stop)
  end
[@@warning "-4"]
