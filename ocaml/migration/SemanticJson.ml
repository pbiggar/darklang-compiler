(* SemanticJson.ml - Encode every semantic field with the frozen source names. *)
open Dark_compiler
open Tokenizer
let scalar kind value = `Assoc ["kind", `String kind; "value", `String value]
let union typ case fields = `Assoc ["type", `String typ; "case", `String case; "fields", `List fields]
let record name fields = `Assoc ["record", `String name; "fields", `List (List.map (fun (name, value) -> `List [`String name; value]) fields)]
let tuple values = `Assoc ["tuple", `List values]
let string value =
  let units = HostText.utf16Units value in
  let rec unpaired index =
    if index >= Array.length units then false
    else if units.(index) >= 0xd800 && units.(index) <= 0xdbff then
      if index + 1 < Array.length units && units.(index + 1) >= 0xdc00 && units.(index + 1) <= 0xdfff then unpaired (index + 2) else true
    else if units.(index) >= 0xdc00 && units.(index) <= 0xdfff then true
    else unpaired (index + 1) in
  if unpaired 0 then `Assoc ["utf16String", `List (Array.to_list (Array.map (fun unit -> `String (Printf.sprintf "%04x" unit)) units))]
  else `String value
let option encode = function None -> union "FSharpOption" "None" [] | Some value -> union "FSharpOption" "Some" [encode value]
let int32 value = scalar "int32" (string_of_int value)
let unsigned64 value = if value < 0L then Z.add (Z.of_int64 value) (Z.shift_left Z.one 64) |> Z.to_string else Int64.to_string value
let token value =
  match value with
  | TInt value -> union "Token" "TInt" [scalar "bigint" (Z.to_string value)]
  | TInt64 value -> union "Token" "TInt64" [scalar "int64" (Int64.to_string value)]
  | TInt128 value -> union "Token" "TInt128" [scalar "int128" (Z.to_string value)]
  | TInt8 value -> union "Token" "TInt8" [scalar "int8" (string_of_int value)]
  | TInt16 value -> union "Token" "TInt16" [scalar "int16" (string_of_int value)]
  | TInt32 value -> union "Token" "TInt32" [scalar "int32" (Int32.to_string value)]
  | TUInt8 value -> union "Token" "TUInt8" [scalar "uint8" (string_of_int value)]
  | TUInt16 value -> union "Token" "TUInt16" [scalar "uint16" (string_of_int value)]
  | TUInt32 value -> union "Token" "TUInt32" [scalar "uint32" (Int64.to_string value)]
  | TUInt64 value -> union "Token" "TUInt64" [scalar "uint64" (unsigned64 value)]
  | TUInt128 value -> union "Token" "TUInt128" [scalar "uint128" (Z.to_string value)]
  | TFloat value -> union "Token" "TFloat" [scalar "float64" (Printf.sprintf "%016Lx" (Int64.bits_of_float value))]
  | TStringLit value -> union "Token" "TStringLit" [string value]
  | TCharLit value -> union "Token" "TCharLit" [string value]
  | TInterpString -> union "Token" "TInterpString" []
  | TTrue -> union "Token" "TTrue" []
  | TFalse -> union "Token" "TFalse" []
  | TPlus -> union "Token" "TPlus" []
  | TPlusPlus -> union "Token" "TPlusPlus" []
  | TMinus -> union "Token" "TMinus" []
  | TStar -> union "Token" "TStar" []
  | TStarStar -> union "Token" "TStarStar" []
  | TSlash -> union "Token" "TSlash" []
  | TLParen -> union "Token" "TLParen" []
  | TRParen -> union "Token" "TRParen" []
  | TLet -> union "Token" "TLet" []
  | TVal -> union "Token" "TVal" []
  | TIn -> union "Token" "TIn" []
  | TIf -> union "Token" "TIf" []
  | TElif -> union "Token" "TElif" []
  | TThen -> union "Token" "TThen" []
  | TElse -> union "Token" "TElse" []
  | TType -> union "Token" "TType" []
  | TCons -> union "Token" "TCons" []
  | TColon -> union "Token" "TColon" []
  | TComma -> union "Token" "TComma" []
  | TSemicolon -> union "Token" "TSemicolon" []
  | TDot -> union "Token" "TDot" []
  | TLBrace -> union "Token" "TLBrace" []
  | TRBrace -> union "Token" "TRBrace" []
  | TBar -> union "Token" "TBar" []
  | TOf -> union "Token" "TOf" []
  | TMatch -> union "Token" "TMatch" []
  | TWith -> union "Token" "TWith" []
  | TFun -> union "Token" "TFun" []
  | TArrow -> union "Token" "TArrow" []
  | TUnderscore -> union "Token" "TUnderscore" []
  | TWhen -> union "Token" "TWhen" []
  | TLBracket -> union "Token" "TLBracket" []
  | TRBracket -> union "Token" "TRBracket" []
  | TEquals -> union "Token" "TEquals" []
  | TEqEq -> union "Token" "TEqEq" []
  | TNeq -> union "Token" "TNeq" []
  | TLt -> union "Token" "TLt" []
  | TGt -> union "Token" "TGt" []
  | TLte -> union "Token" "TLte" []
  | TGte -> union "Token" "TGte" []
  | TAnd -> union "Token" "TAnd" []
  | TOr -> union "Token" "TOr" []
  | TNot -> union "Token" "TNot" []
  | TPipe -> union "Token" "TPipe" []
  | TDotDotDot -> union "Token" "TDotDotDot" []
  | TPercent -> union "Token" "TPercent" []
  | TShl -> union "Token" "TShl" []
  | TShr -> union "Token" "TShr" []
  | TBitAnd -> union "Token" "TBitAnd" []
  | TBitXor -> union "Token" "TBitXor" []
  | TBitNot -> union "Token" "TBitNot" []
  | TAt -> union "Token" "TAt" []
  | TIdent value -> union "Token" "TIdent" [string value]
  | TEOF -> union "Token" "TEOF" []
let pos (value : Tokenizer.pos) = record "Pos" ["row", int32 value.row; "column", int32 value.column]
let range (value : Tokenizer.tokenRange) = record "TokenRange" ["start", pos value.start; "end_", pos value.end_]
let rec matchPattern value =
  let pair encoder (location, value) = tuple [range location; encoder value] in
  match value with
  | WrittenTypes.MPInt (location, integer) -> union "MatchPattern" "MPInt" [range location; pair (fun value -> scalar "bigint" (Z.to_string value)) integer]
  | WrittenTypes.MPInt8 (location, integer, suffix) -> union "MatchPattern" "MPInt8" [range location; pair (fun value -> scalar "int8" (string_of_int value)) integer; range suffix]
  | WrittenTypes.MPUInt8 (location, integer, suffix) -> union "MatchPattern" "MPUInt8" [range location; pair (fun value -> scalar "uint8" (string_of_int value)) integer; range suffix]
  | WrittenTypes.MPInt16 (location, integer, suffix) -> union "MatchPattern" "MPInt16" [range location; pair (fun value -> scalar "int16" (string_of_int value)) integer; range suffix]
  | WrittenTypes.MPUInt16 (location, integer, suffix) -> union "MatchPattern" "MPUInt16" [range location; pair (fun value -> scalar "uint16" (string_of_int value)) integer; range suffix]
  | WrittenTypes.MPInt32 (location, integer, suffix) -> union "MatchPattern" "MPInt32" [range location; pair (fun value -> scalar "int32" (Int32.to_string value)) integer; range suffix]
  | WrittenTypes.MPUInt32 (location, integer, suffix) -> union "MatchPattern" "MPUInt32" [range location; pair (fun value -> scalar "uint32" (Int64.to_string value)) integer; range suffix]
  | WrittenTypes.MPInt64 (location, integer, suffix) -> union "MatchPattern" "MPInt64" [range location; pair (fun value -> scalar "int64" (Int64.to_string value)) integer; range suffix]
  | WrittenTypes.MPUInt64 (location, integer, suffix) -> union "MatchPattern" "MPUInt64" [range location; pair (fun value -> scalar "uint64" (unsigned64 value)) integer; range suffix]
  | WrittenTypes.MPInt128 (location, integer, suffix) -> union "MatchPattern" "MPInt128" [range location; pair (fun value -> scalar "int128" (Z.to_string value)) integer; range suffix]
  | WrittenTypes.MPUInt128 (location, integer, suffix) -> union "MatchPattern" "MPUInt128" [range location; pair (fun value -> scalar "uint128" (Z.to_string value)) integer; range suffix]
  | WrittenTypes.MPVariable (location, name) -> union "MatchPattern" "MPVariable" [range location; string name]
  | WrittenTypes.MPFloat (location, negative, whole, fraction) -> union "MatchPattern" "MPFloat" [range location; `Bool negative; string whole; string fraction]
  | WrittenTypes.MPBool (location, value) -> union "MatchPattern" "MPBool" [range location; `Bool value]
  | WrittenTypes.MPString (location, contents, opening, closing) -> union "MatchPattern" "MPString" [range location; option (pair string) contents; range opening; range closing]
  | WrittenTypes.MPChar (location, contents, opening, closing) -> union "MatchPattern" "MPChar" [range location; option (pair string) contents; range opening; range closing]
  | WrittenTypes.MPUnit location -> union "MatchPattern" "MPUnit" [range location]
  | WrittenTypes.MPEnum (location, case, fields) -> union "MatchPattern" "MPEnum" [range location; pair string case; `List (List.map matchPattern fields)]
  | WrittenTypes.MPTuple (location, first, comma, second, rest, opening, closing) -> union "MatchPattern" "MPTuple" [range location; matchPattern first; range comma; matchPattern second; `List (List.map (pair matchPattern) rest); range opening; range closing]
  | WrittenTypes.MPList (location, contents, opening, closing) -> union "MatchPattern" "MPList" [range location; `List (List.map (fun (pattern, separator) -> tuple [matchPattern pattern; option range separator]) contents); range opening; range closing]
  | WrittenTypes.MPListCons (location, head, tail, cons) -> union "MatchPattern" "MPListCons" [range location; matchPattern head; matchPattern tail; range cons]
  | WrittenTypes.MPOr (location, alternatives) -> union "MatchPattern" "MPOr" [range location; `List (List.map matchPattern alternatives)]
  | WrittenTypes.MPError location -> union "MatchPattern" "MPError" [range location]

let triviaKind = function Lexer.LineComment -> union "TriviaKind" "LineComment" [] | Lexer.DocComment -> union "TriviaKind" "DocComment" [] | Lexer.BlockComment -> union "TriviaKind" "BlockComment" []
let trivia (value : Lexer.trivia) = record "Trivia" ["kind", triviaKind value.Lexer.kind; "text", string value.Lexer.text; "range", range value.Lexer.range]
let spanned (value : Lexer.spannedToken) = record "SpannedToken" ["token", token value.Lexer.token; "text", string value.Lexer.text; "range", range value.Lexer.range; "docComment", option string value.Lexer.docComment; "leadingTrivia", `List (List.map trivia value.Lexer.leadingTrivia)]
let tokens = function
  | Error error -> union "FSharpResult" "Error" [string error]
  | Ok (tokens, diagnostics) -> union "FSharpResult" "Ok" [tuple [`List (List.map spanned tokens); `List (List.map (fun (location, message) -> tuple [range location; string message]) diagnostics)]]

let diagnostic (value : ParserSupport.diagnostic) =
  let severity = match value.ParserSupport.severity with
    | ParserSupport.DiagError -> union "DiagnosticSeverity" "DiagError" []
    | ParserSupport.DiagWarning -> union "DiagnosticSeverity" "DiagWarning" [] in
  record "Diagnostic" ["code", string value.ParserSupport.code; "severity", severity;
    "range", range value.ParserSupport.range; "message", string value.ParserSupport.message;
    "related", `List (List.map (fun (location, message) -> tuple [range location; string message]) value.ParserSupport.related);
    "hint", option string value.ParserSupport.hint]
let identifier (value : WrittenTypes.identifier) = record "Identifier" ["range", range value.WrittenTypes.range; "name", string value.WrittenTypes.name]
let infixName = function
  | WrittenTypes.ArithmeticPlus -> "ArithmeticPlus" | WrittenTypes.ArithmeticMinus -> "ArithmeticMinus"
  | WrittenTypes.ArithmeticMultiply -> "ArithmeticMultiply" | WrittenTypes.ArithmeticDivide -> "ArithmeticDivide"
  | WrittenTypes.ArithmeticModulo -> "ArithmeticModulo" | WrittenTypes.ArithmeticPower -> "ArithmeticPower"
  | WrittenTypes.BitwiseAnd -> "BitwiseAnd" | WrittenTypes.BitwiseOr -> "BitwiseOr" | WrittenTypes.BitwiseXor -> "BitwiseXor"
  | WrittenTypes.ShiftLeft -> "ShiftLeft" | WrittenTypes.ShiftRight -> "ShiftRight"
  | WrittenTypes.ComparisonGreaterThan -> "ComparisonGreaterThan" | WrittenTypes.ComparisonGreaterThanOrEqual -> "ComparisonGreaterThanOrEqual"
  | WrittenTypes.ComparisonLessThan -> "ComparisonLessThan" | WrittenTypes.ComparisonLessThanOrEqual -> "ComparisonLessThanOrEqual"
  | WrittenTypes.ComparisonEquals -> "ComparisonEquals" | WrittenTypes.ComparisonNotEquals -> "ComparisonNotEquals"
  | WrittenTypes.StringConcat -> "StringConcat"
let infix = function
  | WrittenTypes.InfixFnCall name -> union "Infix" "InfixFnCall" [union "InfixFnName" (infixName name) []]
  | WrittenTypes.BinOp operation ->
      let case = match operation with WrittenTypes.BinOpAnd -> "BinOpAnd" | WrittenTypes.BinOpOr -> "BinOpOr" in
      union "Infix" "BinOp" [union "BinaryOperation" case []]
let parserSupport source =
  match Lexer.tokenize source with
  | Error error -> union "FSharpResult" "Error" [string error]
  | Ok (tokens, _) ->
      let tokens = Array.of_list tokens in
      let state = ParserSupport.makeState 0 tokens in
      let observations = Array.mapi (fun index (token : Lexer.spannedToken) ->
        let found = string (ParserSupport.foundDesc state index) in
        let flags = `List (List.map (fun classify -> `Bool (classify token.Lexer.token))
          [ParserSupport.isIntLit; ParserSupport.canStartAtom; ParserSupport.canStartPattern;
           ParserSupport.closesOrSeparates; ParserSupport.isRecoveryBarrier]) in
        let parts = match token.Lexer.token with Tokenizer.TFloat value -> Some (ParserSupport.floatParts state index value) | _ -> None in
        let qualified = match token.Lexer.token with Tokenizer.TIdent _ -> Some (ParserSupport.parseQualified state index) | _ -> None in
        let parameters = if token.Lexer.token = Tokenizer.TLt then Some (ParserSupport.parseTypeParams state index) else None in
        let gt = if token.Lexer.token = Tokenizer.TGt || token.Lexer.token = Tokenizer.TShr then begin
          state.ParserSupport.pendingGt <- 0;
          let first = ParserSupport.expectGt state index in
          let second = if state.ParserSupport.pendingGt > 0 then Some (ParserSupport.expectGt state (snd first)) else None in
          Some (first, second)
        end else None in
        ParserSupport.checkBareMinMagnitude state index;
        `Assoc ["index", int32 index; "found", found; "flags", flags;
          "infix", option infix (ParserSupport.infixOf token.Lexer.token);
          "floatParts", option (fun (whole, fraction) -> tuple [string whole; string fraction]) parts;
          "qualified", option (fun (modules, final, next) -> tuple [
            `List (List.map (fun (name, dot) -> tuple [identifier name; range dot]) modules);
            identifier final; int32 next]) qualified;
          "typeParams", option (fun (names, next) -> tuple [
            `List (List.map (fun (name, location) -> tuple [string name; range location]) names); int32 next]) parameters;
          "gt", option (fun (first, second) ->
            let encode (location, next) = tuple [range location; int32 next] in
            tuple [encode first; option encode second]) gt]) tokens in
      ParserSupport.validateLiterals state;
      `Assoc ["tokens", `List (Array.to_list observations);
        "diagnostics", `List (List.map diagnostic (List.rev !(state.ParserSupport.diagnostics)))]
  [@@warning "-4"]

let patterns source =
  match Lexer.tokenize source with
  | Error error -> union "FSharpResult" "Error" [string error]
  | Ok (tokens, _) ->
      let state = ParserSupport.makeState 0 (Array.of_list tokens) in
      let pattern, next = PatternParser.parseMatchPattern state 0 in
      `Assoc ["pattern", matchPattern pattern; "next", int32 next;
        "diagnostics", `List (List.map diagnostic (List.rev !(state.ParserSupport.diagnostics)))]

let rec typeReference value =
  let u name fields = union "TypeReference" name fields in
  let pair encoder (location, value) = tuple [range location; encoder value] in
  match value with
  | WrittenTypes.TUnit location -> u "TUnit" [range location]
  | WrittenTypes.TBool location -> u "TBool" [range location]
  | WrittenTypes.TInt location -> u "TInt" [range location]
  | WrittenTypes.TInt8 location -> u "TInt8" [range location]
  | WrittenTypes.TUInt8 location -> u "TUInt8" [range location]
  | WrittenTypes.TInt16 location -> u "TInt16" [range location]
  | WrittenTypes.TUInt16 location -> u "TUInt16" [range location]
  | WrittenTypes.TInt32 location -> u "TInt32" [range location]
  | WrittenTypes.TUInt32 location -> u "TUInt32" [range location]
  | WrittenTypes.TInt64 location -> u "TInt64" [range location]
  | WrittenTypes.TUInt64 location -> u "TUInt64" [range location]
  | WrittenTypes.TInt128 location -> u "TInt128" [range location]
  | WrittenTypes.TUInt128 location -> u "TUInt128" [range location]
  | WrittenTypes.TFloat location -> u "TFloat" [range location]
  | WrittenTypes.TChar location -> u "TChar" [range location]
  | WrittenTypes.TString location -> u "TString" [range location]
  | WrittenTypes.TDateTime location -> u "TDateTime" [range location]
  | WrittenTypes.TUuid location -> u "TUuid" [range location]
  | WrittenTypes.TBlob location -> u "TBlob" [range location]
  | WrittenTypes.TList (r, kw, opening, inner, closing) -> u "TList" [range r; range kw; range opening; typeReference inner; range closing]
  | WrittenTypes.TDict (r, kw, opening, key, comma, value, closing) -> u "TDict" [range r; range kw; range opening; typeReference key; range comma; typeReference value; range closing]
  | WrittenTypes.TVariable (r, tick, name) -> u "TVariable" [range r; range tick; pair string name]
  | WrittenTypes.TTuple (r, first, star, second, rest, opening, closing) -> u "TTuple" [range r; typeReference first; range star; typeReference second; `List (List.map (pair typeReference) rest); range opening; range closing]
  | WrittenTypes.TFn (r, args, result) -> u "TFn" [range r; `List (List.map (fun (arg, arrow) -> tuple [typeReference arg; range arrow]) args); typeReference result]
  | WrittenTypes.TCustom name -> u "TCustom" [record "QualifiedTypeIdentifier" ["range", range name.WrittenTypes.range; "modules", `List (List.map (fun (name, dot) -> tuple [identifier name; range dot]) name.WrittenTypes.modules); "typ", identifier name.WrittenTypes.typ; "typeArgs", `List (List.map typeReference name.WrittenTypes.typeArgs)]]

let types source =
  match Lexer.tokenize source with
  | Error error -> union "FSharpResult" "Error" [string error]
  | Ok (tokens, _) ->
      let state = ParserSupport.makeState 0 (Array.of_list tokens) in
      let value, next = TypeParser.parseTypeRef state 0 in
      `Assoc ["type", typeReference value; "next", int32 next;
        "diagnostics", `List (List.map diagnostic (List.rev !(state.ParserSupport.diagnostics)))]

let rec letPattern = function
  | WrittenTypes.LPUnit r -> union "LetPattern" "LPUnit" [range r]
  | WrittenTypes.LPVariable (r, name) -> union "LetPattern" "LPVariable" [range r; string name]
  | WrittenTypes.LPWildcard r -> union "LetPattern" "LPWildcard" [range r]
  | WrittenTypes.LPTuple (r, first, comma, second, rest, opening, closing) ->
      union "LetPattern" "LPTuple" [range r; letPattern first; range comma; letPattern second;
        `List (List.map (fun (comma, pattern) -> tuple [range comma; letPattern pattern]) rest); range opening; range closing]
let bindings source =
  match Lexer.tokenize source with
  | Error error -> union "FSharpResult" "Error" [string error]
  | Ok (tokens, _) ->
      let state = ParserSupport.makeState 0 (Array.of_list tokens) in
      let pattern, next = BindingPatternParser.parseLetPattern state 0 in
      `Assoc ["pattern", letPattern pattern; "next", int32 next;
        "diagnostics", `List (List.map diagnostic (List.rev !(state.ParserSupport.diagnostics)))]

let fnParam = function
  | WrittenTypes.FPUnit r -> union "FnParam" "FPUnit" [range r]
  | WrittenTypes.FPNormal (r, name, typ, opening, colon, closing, description) ->
      union "FnParam" "FPNormal" [range r; identifier name; typeReference typ; range opening; range colon; range closing; string description]
let declarationSupport stage source =
  match Lexer.tokenize source with
  | Error error -> union "FSharpResult" "Error" [string error]
  | Ok (tokens, _) ->
      let state = ParserSupport.makeState 0 (Array.of_list tokens) in
      let value, next = if stage = "parameters" then
        let value, next = DeclarationSupport.parseParam state 0 in fnParam value, next
        else let value, next = DeclarationSupport.parseEffectRow state 0 in option (fun values -> `List (List.map identifier values)) value, next in
      `Assoc ["value", value; "next", int32 next;
        "diagnostics", `List (List.map diagnostic (List.rev !(state.ParserSupport.diagnostics)))]

let rec expr (value : WrittenTypes.expr) = match value with
  | WrittenTypes.EUnit field0 -> union "Expr" "EUnit" [range field0]
  | WrittenTypes.EBool (field0, field1) -> union "Expr" "EBool" [range field0; (fun value -> `Bool value) field1]
  | WrittenTypes.EInt (location, integer) -> union "Expr" "EInt" [range location; (fun (location, value) -> tuple [range location; (fun value -> scalar "bigint" (Z.to_string value)) value]) integer]
  | WrittenTypes.EInt64 (location, integer, suffix) -> union "Expr" "EInt64" [range location; (fun (location, value) -> tuple [range location; (fun value -> scalar "int64" (Int64.to_string value)) value]) integer; range suffix]
  | WrittenTypes.EInt8 (location, integer, suffix) -> union "Expr" "EInt8" [range location; (fun (location, value) -> tuple [range location; (fun value -> scalar "int8" (string_of_int value)) value]) integer; range suffix]
  | WrittenTypes.EUInt8 (location, integer, suffix) -> union "Expr" "EUInt8" [range location; (fun (location, value) -> tuple [range location; (fun value -> scalar "uint8" (string_of_int value)) value]) integer; range suffix]
  | WrittenTypes.EInt16 (location, integer, suffix) -> union "Expr" "EInt16" [range location; (fun (location, value) -> tuple [range location; (fun value -> scalar "int16" (string_of_int value)) value]) integer; range suffix]
  | WrittenTypes.EUInt16 (location, integer, suffix) -> union "Expr" "EUInt16" [range location; (fun (location, value) -> tuple [range location; (fun value -> scalar "uint16" (string_of_int value)) value]) integer; range suffix]
  | WrittenTypes.EInt32 (location, integer, suffix) -> union "Expr" "EInt32" [range location; (fun (location, value) -> tuple [range location; (fun value -> scalar "int32" (Int32.to_string value)) value]) integer; range suffix]
  | WrittenTypes.EUInt32 (location, integer, suffix) -> union "Expr" "EUInt32" [range location; (fun (location, value) -> tuple [range location; (fun value -> scalar "uint32" (Int64.to_string value)) value]) integer; range suffix]
  | WrittenTypes.EUInt64 (location, integer, suffix) -> union "Expr" "EUInt64" [range location; (fun (location, value) -> tuple [range location; (fun value -> scalar "uint64" (unsigned64 value)) value]) integer; range suffix]
  | WrittenTypes.EInt128 (location, integer, suffix) -> union "Expr" "EInt128" [range location; (fun (location, value) -> tuple [range location; (fun value -> scalar "int128" (Z.to_string value)) value]) integer; range suffix]
  | WrittenTypes.EUInt128 (location, integer, suffix) -> union "Expr" "EUInt128" [range location; (fun (location, value) -> tuple [range location; (fun value -> scalar "uint128" (Z.to_string value)) value]) integer; range suffix]
  | WrittenTypes.EFloat (field0, field1, field2, field3) -> union "Expr" "EFloat" [range field0; (fun value -> `Bool value) field1; string field2; string field3]
  | WrittenTypes.EChar (field0, field1, field2, field3) -> union "Expr" "EChar" [range field0; option (fun item -> (let part0, part1 = item in tuple [range part0; string part1])) field1; range field2; range field3]
  | WrittenTypes.EString (field0, field1, field2, field3, field4) -> union "Expr" "EString" [range field0; option (fun item -> range item) field1; `List (List.map (fun item -> stringSegment item) field2); range field3; range field4]
  | WrittenTypes.EVariable (field0, field1) -> union "Expr" "EVariable" [range field0; string field1]
  | WrittenTypes.EFnName (field0, field1) -> union "Expr" "EFnName" [range field0; qualifiedFnIdentifier field1]
  | WrittenTypes.EInfix (field0, field1, field2, field3) -> union "Expr" "EInfix" [range field0; (let part0, part1 = field1 in tuple [range part0; infix part1]); expr field2; expr field3]
  | WrittenTypes.ELet (field0, field1, field2, field3, field4, field5) -> union "Expr" "ELet" [range field0; letPattern field1; expr field2; expr field3; range field4; range field5]
  | WrittenTypes.EApply (field0, field1, field2, field3) -> union "Expr" "EApply" [range field0; expr field1; `List (List.map (fun item -> typeReference item) field2); `List (List.map (fun item -> expr item) field3)]
  | WrittenTypes.EList (field0, field1, field2, field3) -> union "Expr" "EList" [range field0; `List (List.map (fun item -> (let part0, part1 = item in tuple [expr part0; option (fun item -> range item) part1])) field1); range field2; range field3]
  | WrittenTypes.ETuple (field0, field1, field2, field3, field4, field5, field6) -> union "Expr" "ETuple" [range field0; expr field1; range field2; expr field3; `List (List.map (fun item -> (let part0, part1 = item in tuple [range part0; expr part1])) field4); range field5; range field6]
  | WrittenTypes.EIf (field0, field1, field2, field3, field4, field5, field6) -> union "Expr" "EIf" [range field0; expr field1; expr field2; option (fun item -> expr item) field3; range field4; range field5; option (fun item -> range item) field6]
  | WrittenTypes.ERecordFieldAccess (field0, field1, field2, field3) -> union "Expr" "ERecordFieldAccess" [range field0; expr field1; (let part0, part1 = field2 in tuple [range part0; string part1]); range field3]
  | WrittenTypes.ELambda (field0, field1, field2, field3, field4) -> union "Expr" "ELambda" [range field0; `List (List.map (fun item -> letPattern item) field1); expr field2; range field3; range field4]
  | WrittenTypes.ERecord (field0, field1, field2, field3, field4) -> union "Expr" "ERecord" [range field0; qualifiedTypeIdentifier field1; `List (List.map (fun item -> (let part0, part1, part2 = item in tuple [range part0; (let part0, part1 = part1 in tuple [range part0; string part1]); expr part2])) field2); range field3; range field4]
  | WrittenTypes.EDict (field0, field1, field2, field3, field4) -> union "Expr" "EDict" [range field0; `List (List.map (fun item -> (let part0, part1, part2, part3 = item in tuple [range part0; expr part1; range part2; expr part3])) field1); range field2; range field3; range field4]
  | WrittenTypes.ERecordUpdate (field0, field1, field2, field3, field4, field5) -> union "Expr" "ERecordUpdate" [range field0; expr field1; `List (List.map (fun item -> (let part0, part1, part2 = item in tuple [(let part0, part1 = part0 in tuple [range part0; string part1]); range part1; expr part2])) field2); range field3; range field4; range field5]
  | WrittenTypes.EEnum (field0, field1, field2, field3, field4) -> union "Expr" "EEnum" [range field0; qualifiedTypeIdentifier field1; (let part0, part1 = field2 in tuple [range part0; string part1]); `List (List.map (fun item -> expr item) field3); range field4]
  | WrittenTypes.EMatch (field0, field1, field2, field3, field4) -> union "Expr" "EMatch" [range field0; expr field1; `List (List.map (fun item -> matchCase item) field2); range field3; range field4]
  | WrittenTypes.EPipe (field0, field1, field2) -> union "Expr" "EPipe" [range field0; expr field1; `List (List.map (fun item -> (let part0, part1 = item in tuple [range part0; pipeExpr part1])) field2)]
  | WrittenTypes.EStatement (field0, field1, field2) -> union "Expr" "EStatement" [range field0; expr field1; expr field2]
  | WrittenTypes.EError field0 -> union "Expr" "EError" [range field0]
and stringSegment (value : WrittenTypes.stringSegment) = match value with
  | WrittenTypes.StringText (field0, field1) -> union "StringSegment" "StringText" [range field0; string field1]
  | WrittenTypes.StringInterpolation (field0, field1, field2, field3) -> union "StringSegment" "StringInterpolation" [range field0; expr field1; range field2; range field3]
and matchCase (value : WrittenTypes.matchCase) = record "MatchCase" [
  "barRange", range value.WrittenTypes.barRange;
  "pat", matchPattern value.WrittenTypes.pat;
  "arrowRange", range value.WrittenTypes.arrowRange;
  "whenCondition", option (fun item -> (let part0, part1 = item in tuple [range part0; expr part1])) value.WrittenTypes.whenCondition;
  "rhs", expr value.WrittenTypes.rhs]
and pipeExpr (value : WrittenTypes.pipeExpr) = match value with
  | WrittenTypes.EPipeInfix (field0, field1, field2) -> union "PipeExpr" "EPipeInfix" [range field0; (let part0, part1 = field1 in tuple [range part0; infix part1]); expr field2]
  | WrittenTypes.EPipeLambda (field0, field1, field2, field3, field4) -> union "PipeExpr" "EPipeLambda" [range field0; `List (List.map (fun item -> letPattern item) field1); expr field2; range field3; range field4]
  | WrittenTypes.EPipeEnum (field0, field1, field2, field3, field4) -> union "PipeExpr" "EPipeEnum" [range field0; qualifiedTypeIdentifier field1; (let part0, part1 = field2 in tuple [range part0; string part1]); `List (List.map (fun item -> expr item) field3); range field4]
  | WrittenTypes.EPipeFnCall (field0, field1, field2, field3) -> union "PipeExpr" "EPipeFnCall" [range field0; qualifiedFnIdentifier field1; `List (List.map (fun item -> typeReference item) field2); `List (List.map (fun item -> expr item) field3)]
  | WrittenTypes.EPipeVariableOrFnCall (field0, field1) -> union "PipeExpr" "EPipeVariableOrFnCall" [range field0; string field1]
and qualifiedFnIdentifier (value : WrittenTypes.qualifiedFnIdentifier) = record "QualifiedFnIdentifier" [
  "range", range value.WrittenTypes.range;
  "modules", `List (List.map (fun item -> (let part0, part1 = item in tuple [identifier part0; range part1])) value.WrittenTypes.modules);
  "fn", identifier value.WrittenTypes.fn]
and qualifiedTypeIdentifier (value : WrittenTypes.qualifiedTypeIdentifier) = record "QualifiedTypeIdentifier" [
  "range", range value.WrittenTypes.range;
  "modules", `List (List.map (fun item -> (let part0, part1 = item in tuple [identifier part0; range part1])) value.WrittenTypes.modules);
  "typ", identifier value.WrittenTypes.typ;
  "typeArgs", `List (List.map (fun item -> typeReference item) value.WrittenTypes.typeArgs)]
and fnDecl (value : WrittenTypes.fnDecl) = record "FnDecl" [
  "range", range value.WrittenTypes.range;
  "name", identifier value.WrittenTypes.name;
  "typeParams", `List (List.map (fun item -> (let part0, part1 = item in tuple [string part0; range part1])) value.WrittenTypes.typeParams);
  "parameters", `List (List.map (fun item -> fnParam item) value.WrittenTypes.parameters);
  "effects", option (fun item -> `List (List.map (fun item -> identifier item) item)) value.WrittenTypes.effects;
  "returnType", typeReference value.WrittenTypes.returnType;
  "body", expr value.WrittenTypes.body;
  "keywordLet", range value.WrittenTypes.keywordLet;
  "symbolColon", range value.WrittenTypes.symbolColon;
  "symbolEquals", range value.WrittenTypes.symbolEquals;
  "description", string value.WrittenTypes.description]
and valueDecl (value : WrittenTypes.valueDecl) = record "ValueDecl" [
  "range", range value.WrittenTypes.range;
  "name", identifier value.WrittenTypes.name;
  "body", expr value.WrittenTypes.body;
  "keywordVal", range value.WrittenTypes.keywordVal;
  "symbolEquals", range value.WrittenTypes.symbolEquals;
  "description", string value.WrittenTypes.description]
and recordFieldSyntax (value : WrittenTypes.recordFieldSyntax) = record "RecordFieldSyntax" [
  "range", range value.WrittenTypes.range;
  "name", (let part0, part1 = value.WrittenTypes.name in tuple [range part0; string part1]);
  "typ", typeReference value.WrittenTypes.typ;
  "description", string value.WrittenTypes.description;
  "symbolColon", range value.WrittenTypes.symbolColon]
and enumFieldSyntax (value : WrittenTypes.enumFieldSyntax) = record "EnumFieldSyntax" [
  "range", range value.WrittenTypes.range;
  "typ", typeReference value.WrittenTypes.typ;
  "label", option (fun item -> (let part0, part1 = item in tuple [range part0; string part1])) value.WrittenTypes.label;
  "symbolColon", option (fun item -> range item) value.WrittenTypes.symbolColon]
and enumCaseSyntax (value : WrittenTypes.enumCaseSyntax) = record "EnumCaseSyntax" [
  "range", range value.WrittenTypes.range;
  "name", (let part0, part1 = value.WrittenTypes.name in tuple [range part0; string part1]);
  "fields", `List (List.map (fun item -> enumFieldSyntax item) value.WrittenTypes.fields);
  "description", string value.WrittenTypes.description;
  "keywordOf", option (fun item -> range item) value.WrittenTypes.keywordOf]
and typeDefinition (value : WrittenTypes.typeDefinition) = match value with
  | WrittenTypes.TDAlias field0 -> union "TypeDefinition" "TDAlias" [typeReference field0]
  | WrittenTypes.TDRecord field0 -> union "TypeDefinition" "TDRecord" [`List (List.map (fun item -> (let part0, part1 = item in tuple [recordFieldSyntax part0; option (fun item -> range item) part1])) field0)]
  | WrittenTypes.TDEnum field0 -> union "TypeDefinition" "TDEnum" [`List (List.map (fun item -> (let part0, part1 = item in tuple [range part0; enumCaseSyntax part1])) field0)]
and typeDecl (value : WrittenTypes.typeDecl) = record "TypeDecl" [
  "range", range value.WrittenTypes.range;
  "name", identifier value.WrittenTypes.name;
  "typeParams", `List (List.map (fun item -> (let part0, part1 = item in tuple [string part0; range part1])) value.WrittenTypes.typeParams);
  "definition", typeDefinition value.WrittenTypes.definition;
  "keywordType", range value.WrittenTypes.keywordType;
  "symbolEquals", range value.WrittenTypes.symbolEquals;
  "description", string value.WrittenTypes.description]
and moduleDecl (value : WrittenTypes.moduleDecl) = record "ModuleDecl" [
  "range", range value.WrittenTypes.range;
  "name", (let part0, part1 = value.WrittenTypes.name in tuple [range part0; string part1]);
  "declarations", `List (List.map (fun item -> declaration item) value.WrittenTypes.declarations);
  "keywordModule", range value.WrittenTypes.keywordModule]
and testExpected (value : WrittenTypes.testExpected) = match value with
  | WrittenTypes.TEExpr field0 -> union "TestExpected" "TEExpr" [expr field0]
  | WrittenTypes.TEError field0 -> union "TestExpected" "TEError" [string field0]
  | WrittenTypes.TESqlError field0 -> union "TestExpected" "TESqlError" [string field0]
and test (value : WrittenTypes.test) = record "Test" [
  "range", range value.WrittenTypes.range;
  "actual", expr value.WrittenTypes.actual;
  "expected", testExpected value.WrittenTypes.expected]
and declaration (value : WrittenTypes.declaration) = match value with
  | WrittenTypes.DFunction field0 -> union "Declaration" "DFunction" [fnDecl field0]
  | WrittenTypes.DValue field0 -> union "Declaration" "DValue" [valueDecl field0]
  | WrittenTypes.DModule field0 -> union "Declaration" "DModule" [moduleDecl field0]
  | WrittenTypes.DType field0 -> union "Declaration" "DType" [typeDecl field0]
  | WrittenTypes.DExpr field0 -> union "Declaration" "DExpr" [expr field0]
  | WrittenTypes.DTypeDB field0 -> union "Declaration" "DTypeDB" [typeDecl field0]
  | WrittenTypes.DTest field0 -> union "Declaration" "DTest" [test field0]
and sourceFile (value : WrittenTypes.sourceFile) = record "SourceFile" [
  "range", range value.WrittenTypes.range;
  "declarations", `List (List.map (fun item -> declaration item) value.WrittenTypes.declarations);
  "exprsToEval", `List (List.map (fun item -> expr item) value.WrittenTypes.exprsToEval)]
and parsedFile (value : WrittenTypes.parsedFile) = match value with
  | WrittenTypes.SourceFile field0 -> union "ParsedFile" "SourceFile" [sourceFile field0]
let ast source =
  let result = Parser.parse source in
  record "ParseResult" ["parsed", option parsedFile result.ParserSupport.parsed;
    "diagnostics", `List (List.map diagnostic result.ParserSupport.diagnostics)]

let validated source =
  `List (List.map (fun mode ->
    match Parser.parseFor mode source with
    | Ok value -> union "FSharpResult" "Ok" [sourceFile (Validation.ValidatedSourceFile.toWrittenTypes value)]
    | Error diagnostics -> union "FSharpResult" "Error" [`List (List.map diagnostic diagnostics)])
    [Validation.Script; Validation.Package; Validation.Test])
let rendered source =
  let result = Parser.parse source in
  `List (List.map (fun diagnostic -> string (Parser.renderDiagnostic source diagnostic)) result.ParserSupport.diagnostics)

let sourceItem = function
  | WrittenSource.Function (path, value) -> union "Item" "Function" [`List (List.map string path); fnDecl value]
  | WrittenSource.Value (path, value) -> union "Item" "Value" [`List (List.map string path); valueDecl value]
  | WrittenSource.Type (path, value) -> union "Item" "Type" [`List (List.map string path); typeDecl value]
  | WrittenSource.Expression (path, value) -> union "Item" "Expression" [`List (List.map string path); expr value]
let result encode = function Ok value -> union "FSharpResult" "Ok" [encode value] | Error error -> union "FSharpResult" "Error" [string error]
let writtenSource source =
  match WrittenParsing.parse Validation.Script source with
  | Error error -> union "FSharpResult" "Error" [string error]
  | Ok validated ->
      let units = List.concat_map (fun require -> List.map (fun purpose ->
        result (fun values -> `List (List.map (fun value -> sourceFile (Validation.ValidatedSourceFile.toWrittenTypes value)) values))
          (WrittenSource.validateSourceUnits require ["probe", purpose, validated]))
        [NameSyntax.SourceUnitPurpose.Executable; NameSyntax.SourceUnitPurpose.Library; NameSyntax.SourceUnitPurpose.Package]) [false; true] in
      `Assoc ["items", result (fun values -> `List (List.map sourceItem values)) (WrittenSource.items validated);
        "names", result (fun values -> `List (List.map string values)) (WrittenSource.qualifiedNames [validated]); "units", `List units]
let nameIdentifier = function NameSyntax.OrdinaryIdentifier text -> union "Identifier" "OrdinaryIdentifier" [string text] | NameSyntax.BlankIdentifier -> union "Identifier" "BlankIdentifier" []
let keywordName = function
  | NameSyntax.Keyword.Let -> "Let" | NameSyntax.Keyword.Val -> "Val" | NameSyntax.Keyword.In -> "In"
  | NameSyntax.Keyword.If -> "If" | NameSyntax.Keyword.Elif -> "Elif" | NameSyntax.Keyword.Then -> "Then"
  | NameSyntax.Keyword.Else -> "Else" | NameSyntax.Keyword.Type -> "Type" | NameSyntax.Keyword.Of -> "Of"
  | NameSyntax.Keyword.Match -> "Match" | NameSyntax.Keyword.With -> "With" | NameSyntax.Keyword.Fun -> "Fun"
  | NameSyntax.Keyword.When -> "When" | NameSyntax.Keyword.True -> "True" | NameSyntax.Keyword.False -> "False" | NameSyntax.Keyword.Underscore -> "Underscore"
let nameToken = function
  | NameSyntax.IdentifierToken identifier -> union "IdentifierToken" "IdentifierToken" [nameIdentifier identifier]
  | NameSyntax.KeywordToken keyword -> union "IdentifierToken" "KeywordToken" [union "Keyword" (keywordName keyword) []]
let names source =
  let identifier = NameSyntax.identifierFromText source in
  let qualified = NameSyntax.tryParseLegacySpelling source in
  let qualifiedValue name = tuple [string (NameSyntax.formatQualifiedName name); `List (List.map nameIdentifier (NameSyntax.segments name));
    option (fun (prefix, last) -> tuple [string (NameSyntax.formatQualifiedName prefix); nameIdentifier last]) (NameSyntax.trySplitLast name)] in
  let scan = if Array.length (HostText.utf16Units source) = 0 then None else Some (NameSyntax.scanOrdinary source 0) in
  let quoted = if String.starts_with ~prefix:"``" source then Some (NameSyntax.scanQuoted source 0) else None in
  `Assoc ["identifier", nameIdentifier identifier; "classify", nameToken (NameSyntax.classify source);
    "bare", `Bool (NameSyntax.isBareIdentifier identifier); "format", string (NameSyntax.formatIdentifier identifier);
    "qualified", option qualifiedValue qualified;
    "header", option (fun (name, body) -> tuple [string (NameSyntax.formatQualifiedName name); string body]) (NameSyntax.tryExtractModuleHeader source);
    "sourceUnit", result (fun value -> string (NameSyntax.sourceUnitNameText value)) (NameSyntax.sourceUnitName source);
    "scan", option (fun (name, next) -> tuple [nameIdentifier name; int32 next]) scan;
    "quoted", option (result (fun (name, next) -> tuple [nameIdentifier name; int32 next])) quoted]
let rec astLetPattern = function
  | AST.LPUnit -> union "LetPattern" "LPUnit" []
  | AST.LPWildcard -> union "LetPattern" "LPWildcard" []
  | AST.LPVariable name -> union "LetPattern" "LPVariable" [string name]
  | AST.LPTuple (first, second, rest) -> union "LetPattern" "LPTuple" [astLetPattern first; astLetPattern second; `List (List.map astLetPattern rest)]
let astHelpers source =
  let spellings = [source; "a"; "z"; "\u{E000}"; "\u{10000}"; "a"] in
  let allocation values = `List (List.map (fun (name, id) -> tuple [string name; scalar "uint64" (unsigned64 (AST.functionIdValue id))]) (StringOrder.Map.bindings values)) in
  let allocated = `List (List.map (fun first -> allocation (AST.allocateFunctionIdsFromOrdinal first (List.to_seq spellings))) [0L; 1L; Int64.max_int; Int64.min_int; -16L]) in
  let existing = allocation (AST.allocateFunctionIds (List.to_seq (List.map AST.functionId [0L; 10L; Int64.max_int])) (List.to_seq spellings)) in
  let ordered = `List (List.map (fun value -> scalar "uint64" (unsigned64 (AST.functionIdValue value))) (List.sort Stdlib.compare (List.map AST.functionId [0L; 1L; Int64.max_int; Int64.min_int; -1L]))) in
  let reference = AST.resolvedConstructorReferenceWithTypeArgs source [AST.TInt64; AST.TList AST.TString] in
  let referenceJson = match reference with
    | AST.ResolvedConstructor (modules, name, _) -> union "ConstructorReference" "ResolvedConstructor" [`List (List.map string modules); string name;
        `List [union "SemanticType" "TInt64" []; union "SemanticType" "TList" [union "SemanticType" "TString" []]]]
    | AST.UnresolvedConstructor name -> union "ConstructorReference" "UnresolvedConstructor" [option string name] in
  let pattern = AST.LPTuple (AST.LPVariable source, AST.LPVariable "x", [AST.LPWildcard; AST.LPVariable "_ignored"]) in
  let patterns = [AST.LetBinderPatterns [pattern]; AST.LetBinderPatterns [AST.LPVariable source; AST.LPVariable source];
    AST.MatchBinderPattern (AST.POr (NonEmptyList.fromList [AST.PVar source; AST.PVar "other"]));
    AST.MatchBinderPattern (AST.PListCons ([AST.PVar source; AST.PVar "x"], AST.PVar "tail"));
    AST.MatchBinderPattern (AST.PResolvedConstructor (source, "Case", 2, [AST.PVar source; AST.PVar "x"]))] in
  let definitions = [AST.SumTypeDef ("A", [], [{AST.name = source; fields = []}; {AST.name = "Case"; fields = []}]);
    AST.SumTypeDef ("B", [], [{AST.name = source; fields = []}]); AST.SumTypeDef ("A", [], [{AST.name = "Case"; fields = []}])] in
  let id = AST.constructorId (AST.typeId 1) source 7 and field = AST.fieldId (AST.typeId 2) 3 in
  `Assoc ["allocated", allocated; "existing", existing; "ordered", ordered;
    "hashes", `List (List.map (fun name -> int32 (AST.constructorRuntimeIdentity source name)) ["Some"; "None"; "Ok"; "Error"; source]);
    "reference", referenceJson; "typeName", option string (AST.constructorReferenceTypeName reference);
    "bindings", `List (List.map string (AST.letPatternBindings pattern)); "mapped", astLetPattern (AST.mapLetPatternBindings (fun name -> name ^ "!") pattern);
    "validated", `List (List.map (fun pattern -> result (fun values -> `List (List.map string values)) (AST.validateBinders pattern)) patterns);
    "collisions", `List (List.map string (StringOrder.Set.elements (AST.collidingConstructorCaseNames definitions)));
    "identityProjections", tuple [`Bool (AST.constructorIdOwner id = AST.typeId 1); string (AST.constructorIdValue id); int32 (AST.constructorRuntimeTag id);
      `Bool (AST.fieldIdOwner field = AST.typeId 2); int32 (AST.fieldRuntimeIndex field);
      option string (AST.bindingDisplayName (AST.bindingId 4)); option string (AST.bindingDisplayName (AST.namedBindingId 4 source)); option string (AST.bindingDisplayName (AST.topLevelValueId source))]]

let formatter source =
  result (fun validated ->
    let parsed = Validation.ValidatedSourceFile.toWrittenTypes validated in
    let printed = WrittenFormatter.format source parsed in
    let reparsed = match WrittenParsing.parse Validation.Script printed with Ok value -> Some (Validation.ValidatedSourceFile.toWrittenTypes value) | Error _ -> None in
    tuple [string (WrittenFormatter.syntaxKey parsed); string printed;
      option (fun parsed -> tuple [string (WrittenFormatter.syntaxKey parsed); string (WrittenFormatter.format printed parsed)]) reparsed])
    (WrittenParsing.parse Validation.Script source)

let resolution source =
  let open NameResolution in
  let namespaceKey = function RootNamespace -> ["RootNamespace"] | ModuleNamespace path -> "ModuleNamespace" :: NonEmptyList.toList path | PackageNamespace (owner, modules) -> "PackageNamespace" :: owner :: modules | BuiltinNamespace -> ["BuiltinNamespace"] in
  let identityKey = function
    | LocalValue name -> ["LocalValue"; name]
    | ModuleValue (ns, name) -> "ModuleValue" :: name :: namespaceKey ns
    | PackageValue (ns, name) -> "PackageValue" :: name :: namespaceKey ns
    | BuiltinValue (name, version) -> ["BuiltinValue"; name; string_of_int version]
    | ModuleFunction (ns, name, id) -> "ModuleFunction" :: name :: id :: namespaceKey ns
    | PackageFunction (ns, name, id) -> "PackageFunction" :: name :: id :: namespaceKey ns
    | BuiltinFunction (name, version) -> ["BuiltinFunction"; name; string_of_int version]
    | ConstructorSymbol (owner, caseName) -> ["ConstructorSymbol"; owner; caseName]
    | UserType name -> ["UserType"; name] | BuiltinType name -> ["BuiltinType"; name] in
  let provenanceKey = function LexicalBinding s -> ["LexicalBinding"; s] | SourceDeclaration s -> ["SourceDeclaration"; s] | ModuleDeclaration s -> ["ModuleDeclaration"; s] | PackageDeclaration s -> ["PackageDeclaration"; s] | BuiltinRegistration s -> ["BuiltinRegistration"; s] | CompilerExtension s -> ["CompilerExtension"; s] in
  let encodeStrings xs = `List (List.map string xs) in
  let candidateValue (c : candidate) = tuple [encodeStrings (qualifiedNameSegments c.visibleName); encodeStrings (identityKey c.identity); encodeStrings (provenanceKey c.provenance)] in
  let identities = [LocalValue "x"; ModuleValue (RootNamespace, "x"); PackageValue (PackageNamespace ("Owner", ["Module"]), "x"); BuiltinValue ("x", -1); ModuleFunction (ModuleNamespace (NonEmptyList.fromList ["A"; "B"]), "x", source); PackageFunction (RootNamespace, "x", "decl2"); BuiltinFunction ("x", 2); ConstructorSymbol ("First", "x"); ConstructorSymbol ("Second", "x"); UserType "X"; BuiltinType "X"] in
  let provenances = [LexicalBinding source; SourceDeclaration source; ModuleDeclaration source; PackageDeclaration source; BuiltinRegistration source; CompilerExtension source] in
  let spellings = ["x"; "A.x"; "A.B.x"; source; "Darklang.Stdlib.Option.Option"; "Darklang.Stdlib.Option.Option.Some"] in
  let all = List.concat_map (fun spelling -> List.concat_map (fun identity -> List.filter_map (fun provenance -> candidate spelling identity provenance) provenances) identities) spellings in
  let base = addCandidates all empty in
  let overlay = addCandidates (List.filter (fun (c : candidate) -> match c.provenance with SourceDeclaration _ | ModuleDeclaration _ -> true | LexicalBinding _ | PackageDeclaration _ | BuiltinRegistration _ | CompilerExtension _ -> false) all) empty in
  let environments = [empty; base; overlay; merge base overlay; filterCandidates (fun (c : candidate) -> c.identity = ConstructorSymbol ("First", "x")) base; filterCandidates (fun _ -> false) base; merge base empty] in
  let contexts = [Value; Callable; Constructor; Type] in
  let queries = [source; "x"; "A.x"; "A.B.x"; "Option"; "Option.Some"; "Result.Ok"; "Stdlib.Option.Option"; "missing"; "A..B"] in
  let scopes = [[]; ["A"]; ["A"; "B"]] in
  let resolutionValue = function
    | Error error -> union "FSharpResult" "Error" [string (errorToString error)]
    | Ok value -> union "FSharpResult" "Ok" [tuple [encodeStrings (qualifiedNameSegments value.originalName); string (contextToString value.context); encodeStrings (identityKey value.identity); encodeStrings (provenanceKey value.provenance); string (canonicalSpelling value.identity)]] in
  tuple [option (fun name -> encodeStrings (qualifiedNameSegments name)) (tryQualifiedName source);
    `List (List.map (fun environment -> tuple [`List (List.map candidateValue (candidates environment));
      `List (List.concat_map (fun context -> List.concat_map (fun scope -> List.map (fun query -> tuple [encodeStrings (candidateSpellings context scope query); resolutionValue (resolveInModule context scope query environment)]) queries) scopes) contexts)]) environments)]

let checkingDiagnostics source =
  let open! AST in
  let open CheckingDiagnostics in
  let types : AST.semanticType list = [TInt8; TInt16; TInt32; TInt64; TInt128; TInt; TUInt8; TUInt16; TUInt32; TUInt64; TUInt128; TBool; TFloat64; TString; TBlob; TChar; TDateTime; TUnit; TNever; TInternalRawPtr; TVar source; TInferenceVar (source, "#infer:id:fixed"); TFunction ([TInt64; TVar source], TDict (TString, TVar source)); TTuple [TUnit; TList TString]; TRecord (source, []); TRecord (source, [TDict (TChar, TStream TInt64)]); TSum (source, []); TSum (source, [TVar source]); TList (TDict (TInt64, TString)); TStream TFloat64; TDict (TBool, TInt64)] in
  let expressions : AST.expr list = [UnitLiteral; Int64Literal Int64.min_int; Int128Literal (Z.neg (Z.shift_left Z.one 127)); BigIntLiteral (Z.shift_left Z.one 256); Int8Literal (-128); Int16Literal (-32768); Int32Literal Int32.min_int; UInt8Literal 255; UInt16Literal 65535; UInt32Literal 4294967295L; UInt64Literal (-1L); UInt128Literal (Z.pred (Z.shift_left Z.one 128)); BoolLiteral true; BoolLiteral false; StringLiteral source; CharLiteral source; FloatLiteral (-0.); FloatLiteral infinity; FloatLiteral nan; FloatLiteral 1e16; FloatLiteral 1e-5; TupleLiteral [StringLiteral source; FloatLiteral 1.]; TupleLiteral [Var source]; ListLiteral []; ListLiteral [FloatLiteral 1.]; ListLiteral [Var source; TupleLiteral [Int64Literal 1L; StringLiteral source]]; Var source] in
  let patterns = [LPUnit; LPWildcard; LPVariable source; LPTuple (LPVariable source, LPUnit, [LPTuple (LPWildcard, LPVariable "nested", [])])] in
  let errorValues = List.concat_map (fun typ -> [TypeMismatch (TBool, typ, source); IfBranchTypeMismatch (TBool, typ); InvalidOperation (source, [typ; TString]); IncompatibleEqualityOperands (typ, TString); IncompatibleOrderingOperands (typ, TInt64)]) types @ [UndefinedVariable source; UndefinedCallTarget source; MissingTypeAnnotation source; PolymorphicRecursion source; ResolutionFailure (NameResolution.InvalidQualifiedName (source, NameResolution.Callable)); GenericError source] in
  let encodeStrings xs = `List (List.map string xs) in
  let bound = StringOrder.Map.singleton "message" (StringLiteral source) in
  let calls = [applyNamed "Builtin.unwrap" (NonEmptyList.singleton (Constructor (UnresolvedConstructor None, "Option.None", []))); applyNamed "Builtin.crash" (NonEmptyList.singleton (Var "message")); Let (LPVariable "message", StringLiteral source, applyNamed "Builtin.testRuntimeError" (NonEmptyList.singleton (Var "message"))); Var "absent"] in
  let names = [source; "Builtin.unwrap"; "Builtin.testRuntimeError"; "Builtin.crash"; "Builtin.testNan"; "Builtin.testInfinity"; "Builtin.blobEmpty"] in
  tuple [
    `List (List.map (fun typ -> tuple [string (typeToString typ); string (typeToHelperIdentityString typ); `Bool (isNeverType typ)]) types);
    encodeStrings (List.map typeErrorToString errorValues);
    `List (List.map (fun expr -> tuple [option string (tryFormatLiteralValue expr); option string (formatPatternMismatchValue expr); string (formatListLiteralForNoMatch [expr]);
      `List (List.map (fun typ -> encodeStrings [ifConditionTypeMismatchMessage expr typ; interpolationTypeMismatchMessage expr typ; formatPatternMismatchError expr typ TString None; formatPatternMismatchError expr typ TUnit (Some source); formatLegacyParamTypeError source 4 "param" TString typ expr]) types)]) expressions);
    `List (List.map (fun pattern -> tuple [string (formatLetDeconstructionPattern pattern); string (typeToHelperIdentityString (inferredLetPatternType source pattern));
      `List (List.map (fun typ -> option (fun bindings -> `List (List.map (fun (name, typ) -> tuple [string name; string (typeToHelperIdentityString typ)]) bindings)) (bindLetPatternTypes pattern typ)) types)]) patterns);
    `List (List.map (fun call -> tuple [`Bool (isKnownFailureConstructorExpr call); `Bool (isKnownUnwrapFailureExpr bound call); `Bool (isKnownTestRuntimeErrorExpr bound call); option string (tryExtractKnownTestRuntimeErrorMessage bound call)]) calls);
    `List (List.map (fun name -> tuple [string (withIndefiniteArticle name); `List (List.map (fun test -> `Bool (test name)) [isBuiltinUnwrapName; isBuiltinTestRuntimeErrorName; isRuntimeFailureName; isBuiltinTestNanName; isBuiltinTestInfinityName; isBuiltinBlobEmptyName])]) names);
    `List (List.map (fun scope ->
      let fresh, subst = freshenTypeParams scope [source; "a"; source] in
      let keys = List.map (fun key -> typeToString (inferenceVarForKey key)) fresh in
      tuple [encodeStrings keys; `Bool (List.sort_uniq String.compare fresh |> List.length = 3); `Bool (List.for_all (fun key -> let guid = String.sub key (String.length key - 32) 32 in String.length guid = 32 && guid.[12] = '4' && List.mem guid.[16] ['8';'9';'a';'b']) fresh);
        `List (List.map (fun (name, key) -> tuple [string name; string (typeToString (inferenceVarForKey key))]) (StringOrder.Map.bindings subst))]) [None; Some source])]

let freeVariables source =
  let open! AST in
  let x = Var source and y = Var "y" in
  let field = unresolvedRecordFieldReference "field" in
  let patterns = [PUnit; PWildcard; PVar source; PConstructor ("C", [PVar source; PVar "y"]); PResolvedConstructor ("M.T", "C", 3, [PVar source]); PInt64 1L; PBigInt Z.one; PInt128Literal Z.one; PInt8Literal 1; PInt16Literal 1; PInt32Literal 1l; PUInt8Literal 1; PUInt16Literal 1; PUInt32Literal 1L; PUInt64Literal 1L; PUInt128Literal Z.one; PBool true; PString source; PChar source; PFloat 1.; PTuple [PVar source; PVar "y"]; PList [PVar source]; PListCons ([PVar source], PVar "tail"); POr (NonEmptyList.fromList [PVar source; PVar "other"])] in
  let literalExpressions : AST.expr list = [UnitLiteral; Int64Literal 1L; Int128Literal Z.one; BigIntLiteral Z.one; Int8Literal 1; Int16Literal 1; Int32Literal 1l; UInt8Literal 1; UInt16Literal 1; UInt32Literal 1L; UInt64Literal 1L; UInt128Literal Z.one; BoolLiteral true; StringLiteral source; CharLiteral source; FloatLiteral 1.; RuntimeError source] in
  let expressions = literalExpressions @ [x; Var "Builtin.testNan"; Var "Builtin.testInfinity"; BoundaryRender (source, x); BinOp (Add, x, y); UnaryOp (Neg, x);
    Let (LPVariable source, x, TupleLiteral [x; y]); Let (LPTuple (LPVariable source, LPVariable "y", []), Var "value", TupleLiteral [x; y]);
    RecursiveLet (RecursiveBindingCandidate {sourceName = source; kind = NamedLocalFunctionMember}, Apply (x, [], NonEmptyList.singleton y), TupleLiteral [x; y]);
    If (x, y, Var "z"); Sequence (x, y); Apply (x, [], NonEmptyList.fromList [y; Var "z"]); TupleLiteral [x; y]; TupleAccess (x, 1);
    DictLiteral (TString, TString, [x, y]); RecordLiteral (unresolvedRecordReference "R" [], [field, x]); RecordUpdate (x, [field, y]); RecordAccess (x, field);
    Constructor (UnresolvedConstructor None, "C", [x; y]); ListLiteral [x; y]; Lambda (NonEmptyList.singleton (lambdaParameter (LPVariable source)), None, TupleLiteral [x; y]);
    Apply (TupleAccess (x, 0), [], NonEmptyList.singleton y); IndirectApply (x, NonEmptyList.singleton y); Closure (source, [x; y]);
    InterpolatedString [StringText source; StringExpr x; StringExpr y]] @ List.map (fun pattern -> Match (Var "scrutinee", [{patterns = NonEmptyList.singleton pattern; guard = Some (Var "guard"); body = TupleLiteral [x; y; Var "tail"]}])) patterns in
  let scopes = [StringOrder.Set.empty; StringOrder.Set.singleton source; StringOrder.Set.of_list [source; "y"; "guard"]] in
  let encodeSet values = `List (List.map string (StringOrder.Set.elements values)) in
  tuple [`List (List.map (fun pattern -> encodeSet (FreeVariables.collectPatternBindings pattern)) patterns);
    `List (List.map (fun expr -> `List (List.map (fun bound -> encodeSet (FreeVariables.collectFreeVars expr bound)) scopes)) expressions)]

let functionIdMap source =
  let ids = List.map AST.functionId [0L; 1L; Int64.max_int; Int64.min_int; -1L; 1L] in
  let entries = List.mapi (fun index id -> id, source ^ string_of_int index) ids in
  let table = FunctionIdMap.ofList entries in
  let overlay = FunctionIdMap.ofArray [|AST.functionId 1L, "overlay"; AST.functionId 2L, source|] in
  let tables = [FunctionIdMap.empty; table; FunctionIdMap.ofSeq (List.to_seq entries); FunctionIdMap.remove (AST.functionId 1L) table; FunctionIdMap.change (AST.functionId 0L) (fun _ -> None) table; FunctionIdMap.change (AST.functionId 3L) (fun previous -> Some (Option.value previous ~default:source)) table; FunctionIdMap.merge table overlay; FunctionIdMap.map (fun id value -> unsigned64 (AST.functionIdValue id) ^ value) table; FunctionIdMap.filter (fun id _ -> Int64.unsigned_compare (AST.functionIdValue id) Int64.min_int >= 0) table] in
  let encodeId id = scalar "uint64" (unsigned64 (AST.functionIdValue id)) in
  let encodeEntries entries = `List (List.map (fun (id, value) -> tuple [encodeId id; string value]) entries) in
  `List (List.map (fun table ->
    let iterated = ref [] in
    FunctionIdMap.iter (fun id value -> iterated := !iterated @ [id, value]) table;
    tuple [encodeEntries (FunctionIdMap.toList table); encodeEntries (List.of_seq (FunctionIdMap.toSeq table));
      `List (List.of_seq (Seq.map encodeId (FunctionIdMap.keys table))); `List (List.of_seq (Seq.map string (FunctionIdMap.values table)));
      int32 (FunctionIdMap.count table); `Bool (FunctionIdMap.isEmpty table);
      `List (List.map (fun id -> tuple [option string (FunctionIdMap.tryFind id table); `Bool (FunctionIdMap.containsKey id table)]) ids);
      encodeEntries (FunctionIdMap.fold (fun state id value -> state @ [id, value]) [] table); encodeEntries !iterated;
      `Bool (FunctionIdMap.exists (fun id _ -> AST.functionIdValue id = -1L) table); `Bool (FunctionIdMap.forall (fun _ value -> value <> "") table);
      option (fun (id, value) -> tuple [encodeId id; string value; string (FunctionIdMap.find id table)]) (if FunctionIdMap.isEmpty table then None else Some (FunctionIdMap.maxKeyValue table))]) tables)
