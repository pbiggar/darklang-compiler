(* TypeDefinitionParser.ml - Type declarations with exact docs and field separators. *)
[@@@warning "-4"]

open Tokenizer
open ParserSupport
module WT = WrittenTypes

let upperName name =
  let units = Text.scalars name in
  Array.length units > 0 && Text.isUpper units.(0)

(*
   `type Name [<'a>] = Definition`
*)
let rec parseTypeDecl state index =
  let keyword = rng state index in
  let name =
    match tok state (index + 1) with
    | TIdent name -> { WT.range = rng state (index + 1); name }
    | _ ->
        errExpected state (index + 1) "a type name";
        { WT.range = rng state (index + 1); name = "_" }
  in
  let typeParams, next = parseTypeParams state (index + 2) in
  let equals, next =
    if tok state next = TEquals then (rng state next, next + 1)
    else begin
      errExpected state next "'=' in type definition";
      (zeroWidthAtEnd (rng state next), next)
    end
  in
  let definition, after = parseTypeDefinition state next in
  let ending = if after > 0 then rng state (after - 1) else equals in
  ( WT.DType
      {
        WT.range = span keyword ending;
        name;
        typeParams;
        definition;
        keywordType = keyword;
        symbolEquals = equals;
        description = docOf state index;
      },
    after )

and parseTypeDefinition state index =
  let enum =
    tok state index = TBar
    ||
    match (tok state index, tok state (index + 1)) with
    | TIdent name, next when upperName name -> next = TOf || next = TBar
    | _ -> false
  in
  match tok state index with
  | TLBrace -> parseRecordDef state index
  | _ when enum -> parseEnumDef state index
  | _ ->
      let value, next = TypeParser.parseTypeRef state index in
      (WT.TDAlias value, next)

(*
   `type X = {}` — an empty record isn't valid. Diagnose so the `"_"` placeholder
   the normalizer inserts (records need ≥1 field) is honest recovery, not a
   silently-accepted phantom field. (A `{ garbage }` already errored in the loop
   and won't be at `}` here, so this only fires for a genuinely empty record.)
*)
and parseRecordDef state index =
  let fields = RevBuffer.create ()
  and stop = ref (index + 1)
  and more = ref true in
  while !more && tok state !stop <> TRBrace && tok state !stop <> TEOF do
    match (tok state !stop, tok state (!stop + 1)) with
    | TIdent name, TColon ->
        let nameRange = rng state !stop and colon = rng state (!stop + 1) in
        let typ, after = TypeParser.parseTypeRef state (!stop + 2) in
        let field =
          {
            WT.range = span nameRange (WT.typeReferenceRange typ);
            name = (nameRange, name);
            typ;
            description = docOf state !stop;
            symbolColon = colon;
          }
        in
        if tok state after = TSemicolon || tok state after = TComma then begin
          RevBuffer.add fields (field, Some (rng state after));
          stop := after + 1
        end
        else if after > !stop then begin
          RevBuffer.add fields (field, None);
          if tok state after <> TRBrace then
            requireElementSeparator state
              (WT.typeReferenceRange typ)
              after "a comma, semicolon, or newline between record-type fields";
          stop := after
        end
        else more := false
    | _ ->
        errExpected state !stop "a record field 'name : Type'";
        more := false
  done;
  if RevBuffer.length fields = 0 && tok state !stop = TRBrace then
    err state DiagnosticCode.expected !stop
      "a record type needs at least one field";
  let after =
    if tok state !stop = TRBrace then !stop + 1
    else begin
      errUnclosed state !stop "}" "{" (rng state index);
      !stop
    end
  in
  (WT.TDRecord (RevBuffer.toList fields), after)

(*
   an enum case's fields are separated by `*` (`Case of A * B` = two fields), so
   the field type is parsed at ATOM level — a bare `*` is the separator, not a
   tuple. A tuple field must be parenthesized (`(A * B)`), which parseAtomType handles.
*)
and parseEnumField state index =
  match (tok state index, tok state (index + 1)) with
  | TIdent label, TColon ->
      let nameRange = rng state index and colon = rng state (index + 1) in
      let typ, next = TypeParser.parseAtomType state (index + 2) in
      ( {
          WT.range = span nameRange (WT.typeReferenceRange typ);
          typ;
          label = Some (nameRange, label);
          symbolColon = Some colon;
        },
        next )
  | _ ->
      let typ, next = TypeParser.parseAtomType state index in
      ( {
          WT.range = WT.typeReferenceRange typ;
          typ;
          label = None;
          symbolColon = None;
        },
        next )

(*
   Only the first case may omit the leading `|` (`type X = A | B`); every case
   after it REQUIRES a `|`. Otherwise the following statement — which often
   starts with an uppercase name (`type X = | A | B | C` then `Foo.bar = …`) —
   would be swallowed as another case, orphaning the rest of that line.
   A `///` above a case attaches to whichever token starts it: the leading `|` usually, or the
   case name itself when the first case omits its bar (`type X = A | B`).
   `type X = |` with no case — diagnose so the `"_"` placeholder the normalizer
   inserts (enums need ≥1 case) is honest recovery, not a silent phantom case.
*)
and parseEnumDef state index =
  let cases = RevBuffer.create ()
  and stop = ref index
  and more = ref true
  and first = ref true in
  while !more do
    if tok state !stop <> TBar && not !first then more := false
    else begin
      let barDoc = docOf state !stop in
      let barRange =
        if tok state !stop = TBar then (
          let range = rng state !stop in
          incr stop;
          range)
        else zeroWidthAtEnd (rng state !stop)
      in
      match tok state !stop with
      | TIdent name when upperName name ->
          first := false;
          let nameRange = rng state !stop and nameIndex = !stop in
          incr stop;
          let fields = RevBuffer.create () and keywordOf = ref None in
          if tok state !stop = TOf then begin
            keywordOf := Some (rng state !stop);
            incr stop;
            let field, next = parseEnumField state !stop in
            RevBuffer.add fields field;
            stop := next;
            while tok state !stop = TStar do
              incr stop;
              let field, next = parseEnumField state !stop in
              RevBuffer.add fields field;
              stop := next
            done
          end;
          let ending =
            match RevBuffer.last fields with
            | Some field -> field.WT.range
            | None -> nameRange
          in
          RevBuffer.add cases
            ( barRange,
              {
                WT.range = span nameRange ending;
                name = (nameRange, name);
                fields = RevBuffer.toList fields;
                description =
                  (if barDoc <> "" then barDoc else docOf state nameIndex);
                keywordOf = !keywordOf;
              } )
      | _ -> more := false
    end
  done;
  if RevBuffer.length cases = 0 then
    err state DiagnosticCode.expected !stop
      "an enum type needs at least one case";
  (WT.TDEnum (RevBuffer.toList cases), !stop)
