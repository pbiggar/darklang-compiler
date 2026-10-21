(* DeclarationParser.ml - Declaration annotations, parameter diagnostics, and body scopes. *)
[@@@warning "-4"]

open Tokenizer
open ParserSupport
module WT = WrittenTypes

let parseDecl parseBlock state index =
  let keyword = rng state index and nameIndex = index + 1 in
  let name =
    match tok state nameIndex with
    | TIdent name -> { WT.range = rng state nameIndex; name }
    | _ ->
        errExpected state nameIndex "a declaration name";
        { WT.range = rng state nameIndex; name = "_" }
  in
  let typeParams, bounds, next =
    TypeParameterParser.parseTypeParams state (nameIndex + 1)
  in
  if tok state next = TLParen then
    let parameters, afterParams = DeclarationSupport.parseParams state next in
    let colon, next =
      if tok state afterParams = TColon then
        (rng state afterParams, afterParams + 1)
      else begin
        errExpected state afterParams "':' before the return type";
        (zeroWidthAtEnd (rng state afterParams), afterParams)
      end
    in
    let effects, next = DeclarationSupport.parseEffectRow state next in
    let returnType, next = TypeParser.parseTypeRef state next in
    let equals, next =
      if tok state next = TEquals then (rng state next, next + 1)
      else begin
        errExpected state next "'='";
        (zeroWidthAtEnd (rng state next), next)
      end
    in
    let body, after = parseBlock state next in
    ( WT.DFunction
        {
          WT.range = span keyword (WT.exprRange body);
          name;
          typeParams;
          bounds;
          parameters;
          effects;
          returnType;
          body;
          keywordLet = keyword;
          symbolColon = colon;
          symbolEquals = equals;
          description = docOf state index;
        },
      after )
  else
    let next =
      if tok state next = TColon then begin
        state.diagnostics :=
          {
            code = DiagnosticCode.unexpected;
            severity = DiagError;
            range = rng state next;
            message = "Value annotations are not supported";
            related = [];
            hint = Some "remove ': Type' from this value declaration";
          }
          :: !(state.diagnostics);
        snd (TypeParser.parseTypeRef state (next + 1))
      end
      else next
    in
    let equals, next =
      if tok state next = TEquals then (rng state next, next + 1)
      else begin
        errExpected state next "'='";
        (zeroWidthAtEnd (rng state next), next)
      end
    in
    let body, after = parseBlock state next in
    ( WT.DValue
        {
          WT.range = span keyword (WT.exprRange body);
          name;
          body;
          keywordVal = keyword;
          symbolEquals = equals;
          description = docOf state index;
        },
      after )
