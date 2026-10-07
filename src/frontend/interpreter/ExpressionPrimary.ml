(* ExpressionPrimary.ml - Literal widths, qualified names, constructors, and lambda syntax. *)
[@@@warning "-4"]
open Tokenizer
open ParserSupport
module WT = WrittenTypes
type grammar = {
  parseExpr : parserState -> int -> WT.expr * int;
  parseBlock : parserState -> int -> WT.expr * int;
  parseApp : parserState -> int -> WT.expr * int;
  parseCtorParenFields : parserState -> int -> WT.expr RevBuffer.t -> int;
  parseInterpString : parserState -> int -> WT.expr * int;
}
let upperName name =
  let units = Text.scalars name in Array.length units > 0 && Text.isUpper units.(0)
let parsePrefix grammar state index builtinName =
  let operand, after = grammar.parseApp state (index + 1) in
  let range = rng state index in
  let name = {WT.range; modules = [({WT.range; name = "Builtin"}, range)]; fn = {WT.range; name = builtinName}} in
  WT.EApply (span range (WT.exprRange operand), WT.EFnName (range, name), [], [operand]), after
let parsePrimary grammar state index =
  checkBareMinMagnitude state index;
  match tok state index with
  | TInt value -> WT.EInt (rng state index, (rng state index, value)), index + 1
  | TInt64 value ->
      let digits, suffix = splitTrailingRange state index 1 in
      WT.EInt64 (rng state index, (digits, value), suffix), index + 1
  | TInt32 value ->
      let digits, suffix = splitTrailingRange state index 1 in
      WT.EInt32 (rng state index, (digits, value), suffix), index + 1
  | TInt8 value ->
      let digits, suffix = splitTrailingRange state index 1 in
      WT.EInt8 (rng state index, (digits, value), suffix), index + 1
  | TUInt8 value ->
      let digits, suffix = splitTrailingRange state index 2 in
      WT.EUInt8 (rng state index, (digits, value), suffix), index + 1
  | TInt16 value ->
      let digits, suffix = splitTrailingRange state index 1 in
      WT.EInt16 (rng state index, (digits, value), suffix), index + 1
  | TUInt16 value ->
      let digits, suffix = splitTrailingRange state index 2 in
      WT.EUInt16 (rng state index, (digits, value), suffix), index + 1
  | TUInt32 value ->
      let digits, suffix = splitTrailingRange state index 2 in
      WT.EUInt32 (rng state index, (digits, value), suffix), index + 1
  | TUInt64 value ->
      let digits, suffix = splitTrailingRange state index 2 in
      WT.EUInt64 (rng state index, (digits, value), suffix), index + 1
  | TInt128 value ->
      let digits, suffix = splitTrailingRange state index 1 in
      WT.EInt128 (rng state index, (digits, value), suffix), index + 1
  | TUInt128 value ->
      let digits, suffix = splitTrailingRange state index 1 in
      WT.EUInt128 (rng state index, (digits, value), suffix), index + 1
  | TMinus ->
      let range = span (rng state index) (rng state (index + 1)) in
      (match tok state (index + 1) with
      | TInt value -> WT.EInt (range, (range, Z.neg value)), index + 2
      | TInt64 value ->
          let digits, suffix = splitTrailingRange state (index + 1) 1 in
          WT.EInt64 (range, (span (rng state index) digits, Int64.neg value), suffix), index + 2
      | TInt32 value ->
          let digits, suffix = splitTrailingRange state (index + 1) 1 in
          WT.EInt32 (range, (span (rng state index) digits, Int32.neg value), suffix), index + 2
      | TInt8 value ->
          let digits, suffix = splitTrailingRange state (index + 1) 1 in
          WT.EInt8 (range, (span (rng state index) digits, FixedInteger.negateInt8 value), suffix), index + 2
      | TInt16 value ->
          let digits, suffix = splitTrailingRange state (index + 1) 1 in
          WT.EInt16 (range, (span (rng state index) digits, FixedInteger.negateInt16 value), suffix), index + 2
      | TInt128 value ->
          let digits, suffix = splitTrailingRange state (index + 1) 1 in
          WT.EInt128 (range, (span (rng state index) digits, FixedInteger.negateInt128 value), suffix), index + 2
      | TFloat value -> let whole, fraction = floatParts state (index + 1) value in WT.EFloat (range, true, whole, fraction), index + 2
      | _ -> parsePrefix grammar state index ProgramTypesShim.InfixFnName.negateBuiltinName)
  | TFloat value -> let whole, fraction = floatParts state index value in WT.EFloat (rng state index, value < 0., whole, fraction), index + 1
  | TTrue -> WT.EBool (rng state index, true), index + 1
  | TFalse -> WT.EBool (rng state index, false), index + 1
  | TStringLit value ->
      let delimiter = if String.starts_with ~prefix:"\"\"\"" (txt state index) then "\"\"\"" else "\"" in
      let opening, contents, closing = literalTextRanges state index delimiter in
      WT.EString (rng state index, None, [WT.StringText (contents, value)], opening, closing), index + 1
  | TCharLit value ->
      let opening, contents, closing = literalTextRanges state index "'" in
      WT.EChar (rng state index, Some (contents, value), opening, closing), index + 1
  | TInterpString -> grammar.parseInterpString state index
  | TIdent name ->
      let originalModules, originalFinal, afterName = parseQualified state index in
      let adjacent = tok state afterName = TLt && (rng state afterName).start = originalFinal.WT.range.end_ in
      let nameTypeArgs, afterTypes = if adjacent then
        let args, _, after = TypeParser.parseTypeArgs state afterName in args, after
        else [], afterName in
      let modules, final, next = match nameTypeArgs, tok state afterTypes, tok state (afterTypes + 1) with
        | _ :: _, TDot, TIdent caseName when upperName caseName ->
            originalModules @ [(originalFinal, rng state afterTypes)], {WT.range = rng state (afterTypes + 1); name = caseName}, afterTypes + 2
        | _ -> originalModules, originalFinal, afterTypes in
      let fullRange = span (rng state index) final.WT.range in
      let finalUpper = upperName final.WT.name in
      let braceRecord = finalUpper && tok state next = TLBrace &&
        (match tok state (next + 1), tok state (next + 2) with TRBrace, _ | TIdent _, TEquals -> true | _ -> false) in
      if modules = [] && final.WT.name = "Dict" && tok state next = TLBrace then
        ExpressionCollections.parseDict grammar.parseExpr state final.WT.range next
      else if braceRecord then
        ExpressionCollections.parseRecord grammar.parseExpr state
          {WT.range = fullRange; modules; typ = final; typeArgs = nameTypeArgs} fullRange next
      else if finalUpper then
        let typeName, dot = match List.rev modules with
          | (typ, dot) :: reverseModules ->
              let typeModules = List.rev reverseModules in
              let start = match typeModules with (first, _) :: _ -> first.WT.range | [] -> typ.WT.range in
              {WT.range = span start typ.WT.range; modules = typeModules; typ; typeArgs = nameTypeArgs}, dot
          | [] ->
              let zero = zeroWidthAtEnd (rng state index) in
              {WT.range = zero; modules = []; typ = {WT.range = zero; name = ""}; typeArgs = []}, zero in
        let fields = RevBuffer.create () in
        let after = if tok state next = TLParen && (rng state next).start = final.WT.range.end_
          then grammar.parseCtorParenFields state next fields else next in
        let ending = if tok state next = TLParen && after > next then rng state (after - 1)
          else match RevBuffer.last fields with Some value -> WT.exprRange value | None -> final.WT.range in
        WT.EEnum (span (rng state index) ending, typeName, (final.WT.range, final.WT.name), RevBuffer.toList fields, dot), after
      else
        let expression = if modules = [] then WT.EVariable (rng state index, name)
          else WT.EFnName (fullRange, {WT.range = fullRange; modules; fn = final}) in
        if nameTypeArgs <> [] then
          let fn = match expression with
            | WT.EVariable (range, name) -> WT.EFnName (range, {WT.range; modules = []; fn = {WT.range; name}})
            | other -> other in
          WT.EApply (span fullRange (rng state (next - 1)), fn, nameTypeArgs, []), next
        else expression, next
  | TLParen -> ExpressionCollections.parseParen grammar.parseExpr state index
  | TLBracket -> ExpressionCollections.parseList grammar.parseExpr state index
  | TFun ->
      let keyword = rng state index in
      let patterns = RevBuffer.create () and stop = ref (index + 1) in
      while (match tok state !stop with TIdent _ | TUnderscore | TLParen -> true | _ -> false) do
        let pattern, after = BindingPatternParser.parseLetPattern state !stop in
        RevBuffer.add patterns pattern; stop := if after = !stop then !stop + 1 else after
      done;
      if RevBuffer.length patterns = 0 then errExpected state !stop "at least one lambda parameter";
      let arrow, next = if tok state !stop = TArrow then rng state !stop, !stop + 1
        else begin errExpected state !stop "'->' in lambda"; zeroWidthAtEnd (rng state !stop), !stop end in
      let body, after = grammar.parseBlock state next in
      WT.ELambda (span keyword (WT.exprRange body), RevBuffer.toList patterns, body, keyword, arrow), after
  | TLBrace ->
      (match tok state (index + 1), tok state (index + 2) with
      | TRBrace, _ | TIdent _, TEquals ->
          err state DiagnosticCode.expected index "Anonymous records are not supported; use a named record type";
          let zero = zeroWidthAtEnd (rng state index) in
          let typ = {WT.range = zero; modules = []; typ = {WT.range = zero; name = ""}; typeArgs = []} in
          ExpressionCollections.parseRecord grammar.parseExpr state typ (rng state index) index
      | _ -> ExpressionCollections.parseRecordUpdate grammar.parseExpr state index)
  | TNot -> parsePrefix grammar state index ProgramTypesShim.InfixFnName.boolNotBuiltinName
  | TBitNot -> parsePrefix grammar state index ProgramTypesShim.InfixFnName.bitwiseNotBuiltinName
  | TDotDotDot ->
      err state DiagnosticCode.unexpected index ("'" ^ txt state index ^ "' is reserved but not supported by the expression grammar");
      WT.EError (rng state index), index + 1
  | _ ->
      if txt state index = "::" then
        err state DiagnosticCode.expected index "'::' is a pattern; to build a list in an expression use `Stdlib.List.push` or a literal"
      else errExpected state index "an expression";
      let start = (rng state index).start in
      WT.EError {start; end_ = start},
      (if index < state.tokenCount && not (isRecoveryBarrier (tok state index)) then index + 1 else index)
