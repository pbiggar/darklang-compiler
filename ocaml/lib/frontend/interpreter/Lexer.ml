(* Lexer.ml - Preserve the oracle's UTF-16 scans, recovery, and literal domains. *)
open Tokenizer
type triviaKind = LineComment | DocComment | BlockComment
type trivia = { kind : triviaKind; text : string; range : tokenRange }
type spannedToken = {
  token : token; text : string; range : tokenRange;
  docComment : string option; leadingTrivia : trivia list;
}
let units = HostText.utf16Units
let substring source start length = HostText.ofUtf16Units (Array.sub source start length)
let is source index character = source.(index) = Char.code character
let decodeEscape source index =
  let length = Array.length source in
  let hex start count =
    if start + count > length then None else
    let rec parse offset value =
      if offset = count then Some value else
      let character = source.(start + offset) in
      let digit =
        if character >= 48 && character <= 57 then character - 48
        else if character >= 65 && character <= 70 then character - 55
        else if character >= 97 && character <= 102 then character - 87
        else -1 in
      if digit < 0 then None else parse (offset + 1) ((value lsl 4) lor digit)
    in parse 0 0
  in
  let scalar count =
    match hex (index + 2) count with
    | Some value when value <= 0x10ffff && not (value >= 0xd800 && value <= 0xdfff) ->
        let buffer = Buffer.create 4 in
        Uutf.Buffer.add_utf_8 buffer (Uchar.of_int value);
        Some (count + 2, Buffer.contents buffer)
    | Some _ | None -> None
  in
  if index + 1 >= length then None else
  match source.(index + 1) with
  | 110 -> Some (2, "\n") | 116 -> Some (2, "\t") | 114 -> Some (2, "\r")
  | 97 -> Some (2, "\007") | 98 -> Some (2, "\008") | 118 -> Some (2, "\011")
  | 102 -> Some (2, "\012") | 92 -> Some (2, "\\") | 34 -> Some (2, "\"")
  | 39 -> Some (2, "'") | 47 -> Some (2, "/") | 48 -> Some (2, "\000")
  | 123 -> Some (2, "{") | 125 -> Some (2, "}")
  | 120 -> Option.map (fun value -> 4, substring [|value|] 0 1) (hex (index + 2) 2)
  | 88 | 117 -> scalar 4
  | 85 -> scalar 8
  | _ -> None
let unescape text =
  let source = units text in
  let buffer = Buffer.create (String.length text) in
  let rec append index =
    if index < Array.length source then
      match if is source index '\\' then decodeEscape source index else None with
      | Some (count, decoded) -> Buffer.add_string buffer decoded; append (index + count)
      | None ->
          let count = if source.(index) >= 0xd800 && source.(index) <= 0xdbff &&
                         index + 1 < Array.length source && source.(index + 1) >= 0xdc00 && source.(index + 1) <= 0xdfff then 2 else 1 in
          Buffer.add_string buffer (substring source index count); append (index + count)
  in append 0; HostText.normalize (Buffer.contents buffer)
let hasInvalidEscape text =
  let source = units text in
  let rec scan index =
    if index >= Array.length source then false
    else if is source index '\\' then
      match decodeEscape source index with None -> true | Some (count, _) -> scan (index + count)
    else scan (index + 1)
  in scan 0
let skipLineComment source limit index =
  let rec scan index = if index < limit && not (is source index '\n') then scan (index + 1) else index in
  scan (index + 2)
let skipBlockComment source limit index =
  let rec scan index depth =
    if index >= limit || depth = 0 then index
    else if index + 1 < limit && is source index '(' && is source (index + 1) '*' then scan (index + 2) (depth + 1)
    else if index + 1 < limit && is source index '*' && is source (index + 1) ')' then scan (index + 2) (depth - 1)
    else scan (index + 1) depth
  in scan (index + 2) 1
let skipRawString source limit index =
  let rec scan index =
    if index >= limit then index
    else if index + 2 < limit && is source index '"' && is source (index + 1) '"' && is source (index + 2) '"' then index + 3
    else scan (index + 1)
  in scan (index + 3)
let skipQuoted source limit index closing =
  let rec scan index =
    if index >= limit then index
    else if is source index '\\' then scan (index + 2)
    else if is source index closing then index + 1
    else scan (index + 1)
  in scan (index + 1)
let findClose source limit start =
  let rec scan index depth =
    if index >= limit then -1
    else if index + 1 < limit && is source index '/' && is source (index + 1) '/' then
      scan (skipLineComment source limit index) depth
    else if index + 1 < limit && is source index '(' && is source (index + 1) '*' then
      scan (skipBlockComment source limit index) depth
    else if index + 2 < limit && is source index '"' && is source (index + 1) '"' && is source (index + 2) '"' then
      scan (skipRawString source limit index) depth
    else if is source index '"' then scan (skipQuoted source limit index '"') depth
    else if index + 1 < limit && is source index '\'' && is source (index + 1) '\\' then
      scan (skipQuoted source limit index '\'') depth
    else if index + 2 < limit && is source index '\'' && is source (index + 2) '\'' then scan (index + 3) depth
    else if is source index '{' then scan (index + 1) (depth + 1)
    else if is source index '}' then if depth = 0 then index else scan (index + 1) (depth - 1)
    else scan (index + 1) depth
  in scan start 0
let findInterpExprClose text limit start = findClose (units text) limit start
let hasInvalidEscapeInterp text =
  let source = units text in
  let length = Array.length source in
  let rec scan index =
    if index >= length then false
    else if index + 1 < length &&
      ((is source index '{' && is source (index + 1) '{') || (is source index '}' && is source (index + 1) '}')) then scan (index + 2)
    else if is source index '{' then let close = findClose source length (index + 1) in if close < 0 then false else scan (close + 1)
    else if is source index '\\' then
      match decodeEscape source index with None -> true | Some (count, _) -> scan (index + count)
    else scan (index + 1)
  in scan 0
let hasSingleCloseBraceInterp text raw =
  let source = units text in
  let length = Array.length source in
  let rec scan index =
    if index >= length then false
    else if index + 1 < length &&
      ((is source index '{' && is source (index + 1) '{') || (is source index '}' && is source (index + 1) '}')) then scan (index + 2)
    else if is source index '{' then let close = findClose source length (index + 1) in if close < 0 then false else scan (close + 1)
    else if is source index '}' then true
    else if not raw && is source index '\\' && index + 1 < length then scan (index + 2)
    else scan (index + 1)
  in scan 0
let maxInterpNesting = 64
let operators = [
  "...", TDotDotDot; "**", TStarStar; "++", TPlusPlus; "->", TArrow;
  "==", TEqEq; "!=", TNeq; "<<", TShl; "<=", TLte; ">>", TShr; ">=", TGte;
  "&&", TAnd; "||", TOr; "|>", TPipe; "::", TCons;
  "+", TPlus; "-", TMinus; "*", TStar; "/", TSlash; "(", TLParen; ")", TRParen;
  "{", TLBrace; "}", TRBrace; "[", TLBracket; "]", TRBracket; ":", TColon;
  ",", TComma; ";", TSemicolon; ".", TDot; "=", TEquals; "!", TNot; "<", TLt;
  ">", TGt; "&", TBitAnd; "^", TBitXor; "~", TBitNot; "%", TPercent; "@", TAt; "|", TBar
]
let keyword = function
  | "let" -> TLet | "val" -> TVal | "in" -> TIn | "if" -> TIf | "elif" -> TElif
  | "then" -> TThen | "else" -> TElse | "type" -> TType | "of" -> TOf
  | "match" -> TMatch | "with" -> TWith | "fun" -> TFun | "when" -> TWhen
  | "true" -> TTrue | "false" -> TFalse | "_" -> TUnderscore | "___" -> TIdent ""
  | text -> TIdent text
let letter = HostText.isLetterUnit
let digit = HostText.isDigitUnit
let letterOrDigit value = letter value || digit value
let tokenize text =
  let source = units text in
  let length = Array.length source in
  let pendingDocComment = ref None and pendingTrivia = ref [] and diagnostics = ref [] in
  let diagnostic range message = diagnostics := (range, message) :: !diagnostics in
  let step position character = if character = 10 then {row = position.row + 1; column = 0} else {position with column = position.column + 1} in
  let advance position start stop =
    let rec loop position index = if index >= stop then position else loop (step position source.(index)) (index + 1) in
    loop position start in
  let rec skipTrivia index position =
    if index >= length then index, position else
    let character = source.(index) in
    if List.mem character [32; 9; 13; 10] then skipTrivia (index + 1) (step position character)
    else if index + 1 < length && is source index '/' && is source (index + 1) '/' then begin
      let stop = skipLineComment source length index in
      let doc = index + 2 < length && is source (index + 2) '/' && (index + 3 >= length || not (is source (index + 3) '/')) in
      if doc then begin
        let docText = HostText.trim (substring source (index + 3) (stop - index - 3)) in
        pendingDocComment := Some (match !pendingDocComment with None -> docText | Some previous -> previous ^ " " ^ docText)
      end;
      let endPosition = advance position index stop in
      pendingTrivia := ({kind = (if doc then DocComment else LineComment); text = substring source index (stop - index); range = {start = position; end_ = endPosition}} : trivia) :: !pendingTrivia;
      skipTrivia stop endPosition
    end else if index + 1 < length && is source index '(' && is source (index + 1) '*' &&
      not (index + 2 < length && is source (index + 2) ')') &&
      not (index + 3 < length && is source (index + 2) '*' && is source (index + 3) ')') then begin
      let rec skip index depth =
        if index + 1 < length && is source index '(' && is source (index + 1) '*' then skip (index + 2) (depth + 1)
        else if index + 1 < length && is source index '*' && is source (index + 1) ')' then
          if depth = 0 then index + 2, true else skip (index + 2) (depth - 1)
        else if index >= length then index, false else skip (index + 1) depth in
      let stop, closed = skip (index + 2) 0 in
      let endPosition = advance position index stop in
      let range = {start = position; end_ = endPosition} in
      pendingTrivia := ({kind = BlockComment; text = substring source index (stop - index); range} : trivia) :: !pendingTrivia;
      if not closed then diagnostic range "unterminated block comment";
      skipTrivia stop endPosition
    end else index, position
  in
  let matchesAt text index =
    let chars = units text in
    index + Array.length chars <= length &&
    let rec equal offset = offset >= Array.length chars || (source.(index + offset) = chars.(offset) && equal (offset + 1)) in equal 0
  in
  let emit token start stop position =
    let endPosition = advance position start stop in
    let spanned = {token; text = substring source start (stop - start); range = {start = position; end_ = endPosition}; docComment = !pendingDocComment; leadingTrivia = List.rev !pendingTrivia} in
    pendingDocComment := None; pendingTrivia := [];
    spanned, stop, endPosition
  in
  let suffixes = ["uy"; "us"; "ul"; "UL"; "L"; "Q"; "Z"; "y"; "s"; "l"] in
  let suffixAt index = List.find_opt (fun suffix -> matchesAt suffix index) suffixes in
  let intToken digits suffixIndex =
    let suffix = Option.value ~default:"" (suffixAt suffixIndex) in
    let invalid = Error ("Invalid integer literal: " ^ digits) in
    let ascii = String.for_all (fun char -> char >= '0' && char <= '9') digits in
    let parse bits signed constructor label =
      let error = Error ((if label = "Int64" then "Integer literal too large: " else "out of range for " ^ label ^ ": ") ^ digits) in
      if not ascii then error else
      let value = Z.of_string_base 10 digits in
      let bound = Z.shift_left Z.one (if signed then bits - 1 else bits) in
      if Z.lt value bound then Ok (constructor value, String.length suffix)
      else if signed && digits = Z.to_string bound then Ok (constructor (Z.neg bound), String.length suffix)
      else error
    in
    match suffix with
    | "uy" -> parse 8 false (fun value -> TUInt8 (Z.to_int value)) "UInt8"
    | "us" -> parse 16 false (fun value -> TUInt16 (Z.to_int value)) "UInt16"
    | "ul" -> parse 32 false (fun value -> TUInt32 (Z.to_int64 value)) "UInt32"
    | "UL" -> parse 64 false (fun value -> TUInt64 (Z.to_int64 (if Z.testbit value 63 then Z.sub value (Z.shift_left Z.one 64) else value))) "UInt64"
    | "L" -> parse 64 true (fun value -> TInt64 (Z.to_int64 value)) "Int64"
    | "Q" -> parse 128 true (fun value -> TInt128 value) "Int128"
    | "Z" -> parse 128 false (fun value -> TUInt128 value) "UInt128"
    | "y" -> parse 8 true (fun value -> TInt8 (Z.to_int value)) "Int8"
    | "s" -> parse 16 true (fun value -> TInt16 (Z.to_int value)) "Int16"
    | "l" -> parse 32 true (fun value -> TInt32 (Z.to_int32 value)) "Int32"
    | _ -> if ascii then Ok (TInt (Z.of_string_base 10 digits), 0) else invalid
  in
  let rec scanString index closing =
    if index >= length then Error "Unterminated string literal"
    else if is source index '\\' && index + 1 < length then scanString (index + 2) closing
    else if is source index closing then Ok (index + 1) else scanString (index + 1) closing in
  let rec lineEnd index = if index >= length || is source index '\n' then index else lineEnd (index + 1) in
  let scanInterp start =
    let triple = start + 3 < length && matchesAt "\"\"\"" (start + 1) in
    let rec scan index =
      if index >= length then Error "Unterminated interpolated string"
      else if (if triple then matchesAt "\"\"\"" index else is source index '"') then Ok (index + if triple then 3 else 1)
      else if not triple && index + 1 < length && is source index '\\' then scan (index + 2)
      else if index + 1 < length && ((is source index '{' && is source (index + 1) '{') || (is source index '}' && is source (index + 1) '}')) then scan (index + 2)
      else if is source index '{' then let close = findClose source length (index + 1) in if close = -1 then Error "Unterminated interpolated expression" else scan (close + 1)
      else scan (index + 1)
    in scan (start + if triple then 4 else 2)
  in
  let continue index = index < length && (letterOrDigit source.(index) || List.mem source.(index) [95; 39]) in
  let rec scanWhile predicate index = if index < length && predicate index then scanWhile predicate (index + 1) else index in
  let rec go index position tokens =
    let index, position = skipTrivia index position in
    let push token stop message =
      let spanned, next, nextPosition = emit token index stop position in
      Option.iter (diagnostic spanned.range) message;
      go next nextPosition (spanned :: tokens)
    in
    if index >= length then
      let eof, _, _ = emit TEOF index index position in Ok (List.rev (eof :: tokens))
    else if matchesAt "=>" index then push TEquals (index + 1) None
    else if matchesAt "``" index then
      let close = scanWhile (fun scan -> scan + 1 < length && not (matchesAt "``" scan) && not (is source scan '\n')) (index + 2) in
      if matchesAt "``" close then push (TIdent (substring source (index + 2) (close - index - 2))) (close + 2) None
      else let stop = lineEnd (index + 2) in push (TIdent (substring source (index + 2) (stop - index - 2))) stop (Some "unterminated backtick identifier")
    else if letter source.(index) || is source index '_' then
      let stop = scanWhile continue (index + 1) in push (keyword (substring source index (stop - index))) stop None
    else if digit source.(index) then begin
      let digitEnd = scanWhile (fun scan -> digit source.(scan)) (index + 1) in
      let fraction = digitEnd + 1 < length && is source digitEnd '.' && digit source.(digitEnd + 1) in
      let exponent = digitEnd < length && (is source digitEnd 'e' || is source digitEnd 'E') in
      if fraction || exponent then begin
        let stop = if fraction then scanWhile (fun scan -> digit source.(scan)) (digitEnd + 1) else digitEnd in
        let stop = if stop < length && (is source stop 'e' || is source stop 'E') then
          let start = stop + 1 in
          let start = if start < length && (is source start '+' || is source start '-') then start + 1 else start in
          scanWhile (fun scan -> digit source.(scan)) start else stop in
        let number = substring source index (stop - index) in
        (* The scanned grammar excludes OCaml's hex, underscore, and suffix syntax. *)
        let value = if String.for_all (fun char -> char < '\128') number then float_of_string_opt number else None in
        match value with
        | Some value when not (continue stop) -> push (TFloat value) stop None
        | Some _ -> let stop = scanWhile continue stop in push (TFloat 0.) stop (Some ("invalid number literal: " ^ substring source index (stop - index)))
        | None -> push (TFloat 0.) stop (Some ("malformed float literal: " ^ number))
      end else
        let digits = substring source index (digitEnd - index) in
        match intToken digits digitEnd with
        | Ok (token, suffixLength) when not (continue (digitEnd + suffixLength)) -> push token (digitEnd + suffixLength) None
        | Ok (_, suffixLength) -> let stop = scanWhile continue (digitEnd + suffixLength) in push (TInt64 0L) stop (Some ("invalid number literal: " ^ substring source index (stop - index)))
        | Error message -> let count = Option.fold ~none:0 ~some:String.length (suffixAt digitEnd) in push (TInt64 0L) (digitEnd + count) (Some message)
    end else if matchesAt "\"\"\"" index then begin
      let close = scanWhile (fun scan -> not (matchesAt "\"\"\"" scan)) (index + 3) in
      if close < length then push (TStringLit (HostText.normalize (substring source (index + 3) (close - index - 3)))) (close + 3) None
      else push (TStringLit (HostText.normalize (substring source (index + 3) (length - index - 3)))) length (Some "unterminated triple-quoted string literal")
    end else if is source index '"' then begin
      match scanString (index + 1) '"' with
      | Ok stop -> push (TStringLit (unescape (substring source (index + 1) (stop - index - 2)))) stop None
      | Error _ -> let stop = lineEnd (index + 1) in push (TStringLit (unescape (substring source (index + 1) (stop - index - 1)))) stop (Some "unterminated string literal")
    end else if is source index '\'' then begin
      let typeContext = match tokens with
        | previous :: _ -> List.mem previous.token [TLParen; TStar; TLt; TComma; TColon; TArrow; TEquals; TOf]
        | [] -> false in
      let quotedChar () = match scanString (index + 1) '\'' with
        | Ok stop -> push (TCharLit (unescape (substring source (index + 1) (stop - index - 2)))) stop None
        | Error _ -> let stop = lineEnd (index + 1) in push (TCharLit (unescape (substring source (index + 1) (stop - index - 1)))) stop (Some "unterminated char literal") in
      if typeContext && index + 1 < length && (letter source.(index + 1) || is source (index + 1) '_') then
        let stop = scanWhile (fun scan -> letterOrDigit source.(scan) || is source scan '_') (index + 1) in
        if stop < length && is source stop '\'' then quotedChar ()
        else push (TIdent (substring source (index + 1) (stop - index - 1))) stop None
      else if index + 1 < length && is source (index + 1) '\\' then quotedChar ()
      else
        let contentEnd =
          if index + 1 >= length then index + 1 else
          let remaining = substring source (index + 1) (length - index - 1) in
          let first = List.nth_opt (HostText.graphemeClusters remaining) 0 in
          index + 1 + Option.fold ~none:0 ~some:(fun text -> Array.length (units text)) first in
        if contentEnd < length && is source contentEnd '\'' then push (TCharLit (unescape (substring source (index + 1) (contentEnd - index - 1)))) (contentEnd + 1) None
        else let stop = min length (max (index + 1) contentEnd) in push (TCharLit (unescape (substring source (index + 1) (stop - index - 1)))) stop (Some "unterminated char literal")
    end else if matchesAt "$\"" index then begin
      match scanInterp index with
      | Ok stop -> push TInterpString stop None
      | Error _ -> push TInterpString (lineEnd (index + 2)) (Some "unterminated interpolated string")
    end else
      match List.find_opt (fun (operator, _) -> matchesAt operator index) operators with
      | Some (operator, token) -> push token (index + String.length operator) None
      | None ->
          let nextPosition = advance position index (index + 1) in
          let character = substring source index 1 in
          diagnostic {start = position; end_ = nextPosition} ("unexpected character: '" ^ character ^ "'");
          go (index + 1) nextPosition tokens
  in
  Result.map (fun tokens -> tokens, List.rev !diagnostics) (go 0 {row = 0; column = 0} [])
