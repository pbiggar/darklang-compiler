(* DeclarationParser.ml - Declaration annotations, parameter diagnostics, and body scopes. *)
[@@@warning "-4"]
open Tokenizer
open ParserSupport
module WT = WrittenTypes
let parseDecl parseBlock state index =
  let keyword = rng state index and nameIndex = index + 1 in
  let name = match tok state nameIndex with TIdent name -> {WT.range = rng state nameIndex; name}
    | _ -> errExpected state nameIndex "a declaration name"; {WT.range = rng state nameIndex; name = "_"} in
  let typeParams, next = parseTypeParams state (nameIndex + 1) in
  if tok state next = TLParen then
    let parameters = RevBuffer.create () and stop = ref next and more = ref true in
    while !more && tok state !stop = TLParen do
      let parameter, after = DeclarationSupport.parseParam state !stop in
      RevBuffer.add parameters parameter; if after = !stop then more := false else stop := after
    done;
    List.iter (function
      | WT.FPNormal (_, name, _, _, _, _, _) when name.WT.name = "" ->
          state.diagnostics := {code = DiagnosticCode.pattern; severity = DiagError; range = name.WT.range;
            message = "Blank parameter '___' is not allowed in a package function"; related = [];
            hint = Some "use () for a unit parameter or give the parameter a name"} :: !(state.diagnostics)
      | _ -> ()) (RevBuffer.toList parameters);
    let colon, next = if tok state !stop = TColon then rng state !stop, !stop + 1
      else begin errExpected state !stop "':' before the return type"; zeroWidthAtEnd (rng state !stop), !stop end in
    let effects, next = DeclarationSupport.parseEffectRow state next in
    let returnType, next = TypeParser.parseTypeRef state next in
    let equals, next = if tok state next = TEquals then rng state next, next + 1
      else begin errExpected state next "'='"; zeroWidthAtEnd (rng state next), next end in
    let body, after = parseBlock state next in
    WT.DFunction {WT.range = span keyword (WT.exprRange body); name; typeParams; parameters = RevBuffer.toList parameters;
      effects; returnType; body; keywordLet = keyword; symbolColon = colon; symbolEquals = equals; description = docOf state index}, after
  else
    let next = if tok state next = TColon then begin
      state.diagnostics := {code = DiagnosticCode.unexpected; severity = DiagError; range = rng state next;
        message = "Value annotations are not supported"; related = []; hint = Some "remove ': Type' from this value declaration"} :: !(state.diagnostics);
      snd (TypeParser.parseTypeRef state (next + 1))
    end else next in
    let equals, next = if tok state next = TEquals then rng state next, next + 1
      else begin errExpected state next "'='"; zeroWidthAtEnd (rng state next), next end in
    let body, after = parseBlock state next in
    WT.DValue {WT.range = span keyword (WT.exprRange body); name; body; keywordVal = keyword; symbolEquals = equals; description = docOf state index}, after
