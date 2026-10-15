(* TraitDeclarationParser.ml - Port interpreter trait signatures and ordered impl members. *)
[@@@warning "-4"]

open Tokenizer
open ParserSupport
module WT = WrittenTypes

let identifier state index expected =
  match tok state index with
  | TIdent name -> { WT.range = rng state index; name }
  | _ ->
      errExpected state index expected;
      { WT.range = rng state index; name = "_" }

let equals state index expected =
  if tok state index = TEquals then (rng state index, index + 1)
  else begin
    errExpected state index expected;
    (zeroWidthAtEnd (rng state index), index)
  end

let parseTraitDecl parseBlock state index =
  let keyword = rng state index in
  let name = identifier state (index + 1) "a trait name" in
  let typeParams, bounds, next =
    TypeParameterParser.parseTypeParams state (index + 2)
  in
  if typeParams = [] then
    state.diagnostics :=
      {
        code = DiagnosticCode.bound;
        severity = DiagError;
        range = name.WT.range;
        message =
          "A trait needs a type parameter for the type it is implemented for";
        related = [];
        hint = Some "write `trait Show<'a> = ...`; `'a` is the self type";
      }
      :: !(state.diagnostics);
  let eq, next = equals state next "'=' after the trait name" in
  let methods = RevBuffer.create () and stop = ref next and more = ref true in
  while
    !more
    && tok state !stop = TLet
    && (rng state !stop).start.column > keyword.start.column
  do
    let start = !stop in
    let keywordLet = rng state start in
    let name = identifier state (start + 1) "a method name" in
    let typeParams, bounds, next =
      TypeParameterParser.parseTypeParams state (start + 2)
    in
    let parameters = RevBuffer.create () and afterParams = ref next in
    while tok state !afterParams = TLParen do
      let parameter, after = DeclarationSupport.parseParam state !afterParams in
      RevBuffer.add parameters parameter;
      afterParams := if after > !afterParams then after else !afterParams + 1
    done;
    let colon, next =
      if tok state !afterParams = TColon then
        (rng state !afterParams, !afterParams + 1)
      else begin
        errExpected state !afterParams "':' before the method's return type";
        (zeroWidthAtEnd (rng state !afterParams), !afterParams)
      end
    in
    let effects, next = DeclarationSupport.parseEffectRow state next in
    let returnType, next = TypeParser.parseTypeRef state next in
    let body, after =
      if tok state next = TEquals then begin
        let body, after = parseBlock state (next + 1) in
        state.diagnostics :=
          {
            code = DiagnosticCode.bound;
            severity = DiagError;
            range = WT.exprRange body;
            message =
              "Default method bodies are not supported yet; a trait lists \
               signatures only";
            related = [];
            hint =
              Some "remove the `= …` and implement the method in each `impl`";
          }
          :: !(state.diagnostics);
        (Some body, after)
      end
      else (None, next)
    in
    let ending =
      match body with
      | Some body -> WT.exprRange body
      | None -> WT.typeReferenceRange returnType
    in
    RevBuffer.add methods
      ({
         WT.range = span keywordLet ending;
         name;
         typeParams;
         bounds;
         parameters = RevBuffer.toList parameters;
         effects;
         returnType;
         body;
         keywordLet;
         symbolColon = colon;
         description = docOf state start;
       }
        : WT.traitMethodDecl);
    if after > !stop then stop := after else more := false
  done;
  if RevBuffer.length methods = 0 then
    errExpected state next
      "at least one method signature, indented under the trait";
  let ending = if !stop > 0 then rng state (!stop - 1) else eq in
  ( WT.DTrait
      {
        WT.range = span keyword ending;
        name;
        typeParams;
        bounds;
        methods = RevBuffer.toList methods;
        keywordTrait = keyword;
        symbolEquals = eq;
        description = docOf state index;
      },
    !stop )

let parseImplDecl parseBlock state index =
  let keyword = rng state index in
  let typeParams, bounds, next =
    TypeParameterParser.parseTypeParams state (index + 1)
  in
  let trait_, next =
    match TypeParameterParser.parseBoundTrait state next with
    | Some trait_, after -> (trait_, after)
    | None, after ->
        ( {
            WT.range = rng state next;
            modules = [];
            typ = { WT.range = rng state next; name = "_" };
            typeArgs = [];
          },
          if after > next then after else next + 1 )
  in
  let keywordFor, next =
    match tok state next with
    | TIdent "for" -> (rng state next, next + 1)
    | _ ->
        errExpected state next "'for' between the trait and the type";
        (zeroWidthAtEnd (rng state next), next)
  in
  let forType, next = TypeParser.parseTypeRef state next in
  let eq, next = equals state next "'=' after the type" in
  let members = RevBuffer.create () and stop = ref next and more = ref true in
  while
    !more
    && tok state !stop = TLet
    && (rng state !stop).start.column > keyword.start.column
  do
    let declaration, after =
      DeclarationParser.parseDecl parseBlock state !stop
    in
    (match declaration with
    | WT.DFunction fn -> RevBuffer.add members (WT.IMethod fn)
    | WT.DValue ({ WT.body = WT.EVariable _; _ } as value)
    | WT.DValue ({ WT.body = WT.EFnName _; _ } as value) ->
        RevBuffer.add members (WT.IAlias value)
    | _ ->
        state.diagnostics :=
          {
            code = DiagnosticCode.bound;
            severity = DiagError;
            range = rng state !stop;
            message =
              "An impl body holds method functions (`let name (p: T) : R = …`) \
               or " ^ "aliases of existing ones (`let name = Module.fn`)";
            related = [];
            hint = None;
          }
          :: !(state.diagnostics));
    if after > !stop then stop := after else more := false
  done;
  if RevBuffer.length members = 0 then
    errExpected state next "at least one method, indented under the impl";
  let ending = if !stop > 0 then rng state (!stop - 1) else eq in
  ( WT.DImpl
      {
        WT.range = span keyword ending;
        typeParams;
        bounds;
        trait_;
        forType;
        members = RevBuffer.toList members;
        keywordImpl = keyword;
        keywordFor;
        symbolEquals = eq;
        description = docOf state index;
      },
    !stop )
