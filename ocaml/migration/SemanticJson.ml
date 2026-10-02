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
