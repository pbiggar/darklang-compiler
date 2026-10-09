(*
   Lexer for the Darklang syntax this repo uses. Produces `Tokenizer.Token`s
   with source ranges for the parser.
   Regular string/char escapes are processed (`unescape`); triple-quoted strings
   stay raw.
   Pos, TokenRange, Token
*)
(* Lexer.ml - Scan Unicode scalars with parser recovery and normalized literal domains. *)
open Tokenizer

(*
   `// …` (and `////…`)
   `/// …`
   `( * … * )`
*)
type triviaKind = LineComment | DocComment | BlockComment

(*
   A comment between the previous token and the one carrying it. Whitespace is
   not stored — blank lines are derivable from the token/trivia range gaps.
*)
type trivia = { kind : triviaKind; text : string; range : tokenRange }

(*
   accumulated `///` doc-comment text immediately preceding this token (the
   declaration it documents), if any. Used to recover fn/type descriptions.
   comments between the previous token and this one, in source order — kept
   so tooling (formatter, lossless round-trip) can reproduce the source.
   Trailing comments at EOF land on the TEOF token.
*)
type spannedToken = {
  token : token;
  text : string;
  range : tokenRange;
  docComment : string option;
  leadingTrivia : trivia list;
}

let units = Text.scalars

let substring source start length =
  Text.ofScalars (Array.sub source start length)

let is source index character = source.(index) = Char.code character

(*
   Decode the escape starting at `source[escapeStartIndex]`, which must be `\`.
   Returns `Some(charsConsumed, decodedText)`, or `None` for an invalid escape:
   unknown escape letter, short/non-hex Unicode escape, surrogate, or codepoint
   above `0x10FFFF`. `unescape` and the validators share this function so they
   cannot drift.
   a Unicode scalar value → its UTF-8 string
   trailing backslash
   bell
   backspace
   vertical tab
   form feed
   \xHH
   \XHHHH and \uHHHH both denote a Unicode scalar, so both reject surrogates /
   out-of-range codepoints via `scalar` (a lone `\XD800` is invalid, like `\uD800`)
   \XHHHH
   \uHHHH (BMP)
   \UHHHHHHHH
*)
let decodeEscape source index =
  let length = Array.length source in
  let hex start count =
    if start + count > length then None
    else
      let rec parse offset value =
        if offset = count then Some value
        else
          let character = source.(start + offset) in
          let digit =
            if character >= 48 && character <= 57 then character - 48
            else if character >= 65 && character <= 70 then character - 55
            else if character >= 97 && character <= 102 then character - 87
            else -1
          in
          if digit < 0 then None
          else parse (offset + 1) ((value lsl 4) lor digit)
      in
      parse 0 0
  in
  let scalar count =
    match hex (index + 2) count with
    | Some value
      when value <= 0x10ffff && not (value >= 0xd800 && value <= 0xdfff) ->
        let buffer = Buffer.create 4 in
        Uutf.Buffer.add_utf_8 buffer (Uchar.of_int value);
        Some (count + 2, Buffer.contents buffer)
    | Some _ | None -> None
  in
  if index + 1 >= length then None
  else
    match source.(index + 1) with
    | 110 -> Some (2, "\n")
    | 116 -> Some (2, "\t")
    | 114 -> Some (2, "\r")
    | 97 -> Some (2, "\007")
    | 98 -> Some (2, "\008")
    | 118 -> Some (2, "\011")
    | 102 -> Some (2, "\012")
    | 92 -> Some (2, "\\")
    | 34 -> Some (2, "\"")
    | 39 -> Some (2, "'")
    | 47 -> Some (2, "/")
    | 48 -> Some (2, "\000")
    | 123 -> Some (2, "{")
    | 125 -> Some (2, "}")
    | 120 ->
        Option.map
          (fun value -> (4, substring [| value |] 0 1))
          (hex (index + 2) 2)
    | 88 | 117 -> scalar 4
    | 85 -> scalar 8
    | _ -> None

(*
   Process escape sequences in regular string/char content. Triple-quoted
   strings stay raw. Invalid escapes keep the backslash as-is.
   Module-level so the parser can reuse it for interpolated-string literal parts.
   The result is NFC-normalized so decomposed and composed graphemes have the
   same stored form.
*)
let unescape text =
  let source = units text in
  let buffer = Buffer.create (String.length text) in
  let rec append index =
    if index < Array.length source then
      match
        if is source index '\\' then decodeEscape source index else None
      with
      | Some (count, decoded) ->
          Buffer.add_string buffer decoded;
          append (index + count)
      | None ->
          Buffer.add_string buffer (substring source index 1);
          append (index + 1)
  in
  append 0;
  Text.normalize (Buffer.contents buffer)

(*
   Does the raw inner text of a regular string/char contain an invalid escape?
*)
let hasInvalidEscape text =
  let source = units text in
  let rec scan index =
    if index >= Array.length source then false
    else if is source index '\\' then
      match decodeEscape source index with
      | None -> true
      | Some (count, _) -> scan (index + count)
    else scan (index + 1)
  in
  scan 0

let skipLineComment source limit index =
  let rec scan index =
    if index < limit && not (is source index '\n') then scan (index + 1)
    else index
  in
  scan (index + 2)

let skipBlockComment source limit index =
  let rec scan index depth =
    if index >= limit || depth = 0 then index
    else if
      index + 1 < limit && is source index '(' && is source (index + 1) '*'
    then scan (index + 2) (depth + 1)
    else if
      index + 1 < limit && is source index '*' && is source (index + 1) ')'
    then scan (index + 2) (depth - 1)
    else scan (index + 1) depth
  in
  scan (index + 2) 1

let skipRawString source limit index =
  let rec scan index =
    if index >= limit then index
    else if
      index + 2 < limit
      && is source index '"'
      && is source (index + 1) '"'
      && is source (index + 2) '"'
    then index + 3
    else scan (index + 1)
  in
  scan (index + 3)

let skipQuoted source limit index closing =
  let rec scan index =
    if index >= limit then index
    else if is source index '\\' then scan (index + 2)
    else if is source index closing then index + 1
    else scan (index + 1)
  in
  scan (index + 1)

let findClose source limit start =
  let rec scan index depth =
    if index >= limit then -1
    else if
      index + 1 < limit && is source index '/' && is source (index + 1) '/'
    then scan (skipLineComment source limit index) depth
    else if
      index + 1 < limit && is source index '(' && is source (index + 1) '*'
    then scan (skipBlockComment source limit index) depth
    else if
      index + 2 < limit
      && is source index '"'
      && is source (index + 1) '"'
      && is source (index + 2) '"'
    then scan (skipRawString source limit index) depth
    else if is source index '"' then
      scan (skipQuoted source limit index '"') depth
    else if
      index + 1 < limit && is source index '\'' && is source (index + 1) '\\'
    then scan (skipQuoted source limit index '\'') depth
    else if
      index + 2 < limit && is source index '\'' && is source (index + 2) '\''
    then scan (index + 3) depth
    else if is source index '{' then scan (index + 1) (depth + 1)
    else if is source index '}' then
      if depth = 0 then index else scan (index + 1) (depth - 1)
    else scan (index + 1) depth
  in
  scan start 0

(*
   Find the `}` that closes an interpolation expression region.
   `startIndex` is just past the opening `{`. The scan tracks nested braces and skips
   embedded string and char literals, so braces inside them do not desync the
   scan. Returns `-1` if unclosed. The tokenizer, escape validator, and parser
   all use this scanner so they agree on where interpolation regions end.
   Comments are code trivia, so braces inside them cannot close an
   interpolation. Block comments nest just like top-level lexer comments.
   A raw triple-quoted string may contain unescaped quotes and braces.
   Skip char literals so braces inside them do not affect interpolation
   depth. A leading `'` that is not a char literal, such as type var `'a` or
   tick-ident tail `x'`, falls through harmlessly.
   Start at the `\` so `'\''` skips the escape before looking for the
   closing quote.
*)
let findInterpExprClose text limit start = findClose (units text) limit start

(*
   Like `hasInvalidEscape`, but for regular `$"…"` interpolated strings.
   `{{`/`}}` are literal braces and `{ … }` regions are code, so only literal
   string text is escape-checked.
   skip the `{ … }` interpolation region
*)
let hasInvalidEscapeInterp text =
  let source = units text in
  let length = Array.length source in
  let rec scan index =
    if index >= length then false
    else if
      index + 1 < length
      && ((is source index '{' && is source (index + 1) '{')
         || (is source index '}' && is source (index + 1) '}'))
    then scan (index + 2)
    else if is source index '{' then
      let close = findClose source length (index + 1) in
      if close < 0 then false else scan (close + 1)
    else if is source index '\\' then
      match decodeEscape source index with
      | None -> true
      | Some (count, _) -> scan (index + count)
    else scan (index + 1)
  in
  scan 0

(*
   Does interpolated-string literal text contain a single unescaped `}`?
   Literal braces must be doubled (`}}`) or escaped (`\}` in regular strings).
   Braces inside `{ expression }` regions are code and are skipped.
*)
let hasSingleCloseBraceInterp text raw =
  let source = units text in
  let length = Array.length source in
  let rec scan index =
    if index >= length then false
    else if
      index + 1 < length
      && ((is source index '{' && is source (index + 1) '{')
         || (is source index '}' && is source (index + 1) '}'))
    then scan (index + 2)
    else if is source index '{' then
      let close = findClose source length (index + 1) in
      if close < 0 then false else scan (close + 1)
    else if is source index '}' then true
    else if (not raw) && is source index '\\' && index + 1 < length then
      scan (index + 2)
    else scan (index + 1)
  in
  scan 0

(*
   The parser parses each `{expr}` body recursively. Nested interpolated strings
   therefore increase recursion depth. Cap it so pathological nesting cannot
   overflow the process stack. The tokenizer itself does not recurse here;
   `scanInterp` skips `{expr}` regions iteratively.
*)
let maxInterpNesting = 64

let operators =
  [
    ("...", TDotDotDot);
    ("**", TStarStar);
    ("++", TPlusPlus);
    ("->", TArrow);
    ("==", TEqEq);
    ("!=", TNeq);
    ("<<", TShl);
    ("<=", TLte);
    (">>", TShr);
    (">=", TGte);
    ("&&", TAnd);
    ("||", TOr);
    ("|>", TPipe);
    ("::", TCons);
    ("+", TPlus);
    ("-", TMinus);
    ("*", TStar);
    ("/", TSlash);
    ("(", TLParen);
    (")", TRParen);
    ("{", TLBrace);
    ("}", TRBrace);
    ("[", TLBracket);
    ("]", TRBracket);
    (":", TColon);
    (",", TComma);
    (";", TSemicolon);
    (".", TDot);
    ("=", TEquals);
    ("!", TNot);
    ("<", TLt);
    (">", TGt);
    ("&", TBitAnd);
    ("^", TBitXor);
    ("~", TBitNot);
    ("%", TPercent);
    ("@", TAt);
    ("|", TBar);
  ]

let keyword = function
  | "let" -> TLet
  | "val" -> TVal
  | "in" -> TIn
  | "if" -> TIf
  | "elif" -> TElif
  | "then" -> TThen
  | "else" -> TElse
  | "type" -> TType
  | "of" -> TOf
  | "match" -> TMatch
  | "with" -> TWith
  | "fun" -> TFun
  | "when" -> TWhen
  | "true" -> TTrue
  | "false" -> TFalse
  | "_" -> TUnderscore
  | "___" -> TIdent ""
  | text -> TIdent text

let letter = Text.isLetter
let digit = Text.isDigit
let letterOrDigit value = letter value || digit value

(*
   `///` doc comments lex as trivia, but their text also lands on the next
   emitted token. `emit` consumes and clears this.
   Comments scanned since the last emitted token. `emit` drains them into
   `leadingTrivia`.
   Lexical diagnostics collected during recovery. Defined before trivia
   scanning so an unterminated block comment can report its own range.
   `/// …` is a doc comment for the next declaration. `////` and plain
   `//` are ordinary comments.
   Nestable block comment. `( * )` and `( ** )` are the multiply
   and exponentiation operator sections, so they are excluded here.
   longest-match operators (order matters)
   `val x = e` is a value declaration. `let` is reserved for functions and
   local/script bindings. The distinct token lets `parseItems` keep them apart.
   `def` is not a keyword in the interpreter dialect. It remains a valid
   identifier, for example `(def: Type)`.
   `___` is the blank-name placeholder: an identifier with an empty name.
   integer suffix → token; returns (token, charsConsumedAfterDigits)
   bare literal (no suffix) → arbitrary-precision `Int` (the default).
   Recovery for unterminated lexemes: scan to the current line end or EOF so a
   half-typed string/char does not swallow the rest of the document.
   Scan `$"text {expr} text"` or `$"""…"""`. The token carries no payload; the
   parser re-reads the source text and scans `{expr}` bodies itself. This only
   needs to find the token end while skipping escapes, literal braces, and
   interpolation regions with `findInterpExprClose`.
   triple-quoted `$'''…'''` (raw; a single `'` is literal text)
   `{{` / `}}` are escaped literal braces, not an interpolation
   Tolerate compiler-syntax `=>` by lexing `=` then `>`. The parser rejects
   it later, but tokenization can continue.
   backtick identifiers: ``name``
   unterminated ``…`` — best-effort ident to end of line
   identifiers / keywords
   numbers
   A number token must end at a non-identifier boundary. Glued suffix text
   like `123abc` or `12l3` is a typo, not two tokens.
   float?
   Float glued to identifier chars (`1.5abc`): consume the run and
   diagnose it as one malformed literal.
   Emit a placeholder so malformed floats still highlight as numbers.
   Int glued to identifier chars: consume the run and diagnose it as
   one malformed literal.
   Consume a recognized suffix even on range failure so it cannot
   reappear as a separate identifier in the recovery tree.
   triple-quoted string: """ ... """ (raw, may contain single/double quotes)
   Triple-quoted strings are raw, so normalize here to match the regular
   literal path through `unescape`.
   Unterminated `'''…`: take the rest as string content.
   strings / chars (escape processing deferred — raw content)
   Unterminated `'…`: take to end of line for mid-typing recovery.
   A leading `'` is a type variable (`'a`) in type context, decided by
   the previous token. Otherwise it starts a char literal. Type variables
   lex to the bare name as `TIdent`.
   a closing quote right after the name means it was a char literal
   Char literal: read one char, or one escape, then the closing quote.
   `'''` is the apostrophe char, not an empty literal.
   Escaped char: scan to the closing quote so multi-char escapes decode,
   such as `'\x41'` or `'\U0001F600'`.
   Unescaped char: one extended grapheme cluster, then the closing
   quote. A grapheme may span multiple Unicode scalars.
   Unterminated/half-typed char: recover through the grapheme end.
   Unterminated `$'…`: take to end of line.
   Unknown character: record it, skip it, and keep lexing.
*)
let tokenize text =
  let source = units text in
  let length = Array.length source in
  let pendingDocComment = ref None
  and pendingTrivia = ref []
  and diagnostics = ref [] in
  let diagnostic range message =
    diagnostics := (range, message) :: !diagnostics
  in
  let step position character =
    if character = 10 then { row = position.row + 1; column = 0 }
    else { position with column = position.column + 1 }
  in
  let advance position start stop =
    let rec loop position index =
      if index >= stop then position
      else loop (step position source.(index)) (index + 1)
    in
    loop position start
  in
  let rec skipTrivia index position =
    if index >= length then (index, position)
    else
      let character = source.(index) in
      if List.mem character [ 32; 9; 13; 10 ] then
        skipTrivia (index + 1) (step position character)
      else if
        index + 1 < length && is source index '/' && is source (index + 1) '/'
      then begin
        let stop = skipLineComment source length index in
        let doc =
          index + 2 < length
          && is source (index + 2) '/'
          && (index + 3 >= length || not (is source (index + 3) '/'))
        in
        if doc then begin
          let docText =
            Text.trim (substring source (index + 3) (stop - index - 3))
          in
          pendingDocComment :=
            Some
              (match !pendingDocComment with
              | None -> docText
              | Some previous -> previous ^ " " ^ docText)
        end;
        let endPosition = advance position index stop in
        pendingTrivia :=
          ({
             kind = (if doc then DocComment else LineComment);
             text = substring source index (stop - index);
             range = { start = position; end_ = endPosition };
           }
            : trivia)
          :: !pendingTrivia;
        skipTrivia stop endPosition
      end
      else if
        index + 1 < length
        && is source index '('
        && is source (index + 1) '*'
        && (not (index + 2 < length && is source (index + 2) ')'))
        && not
             (index + 3 < length
             && is source (index + 2) '*'
             && is source (index + 3) ')')
      then begin
        let rec skip index depth =
          if
            index + 1 < length
            && is source index '('
            && is source (index + 1) '*'
          then skip (index + 2) (depth + 1)
          else if
            index + 1 < length
            && is source index '*'
            && is source (index + 1) ')'
          then
            if depth = 0 then (index + 2, true) else skip (index + 2) (depth - 1)
          else if index >= length then (index, false)
          else skip (index + 1) depth
        in
        let stop, closed = skip (index + 2) 0 in
        let endPosition = advance position index stop in
        let range = { start = position; end_ = endPosition } in
        pendingTrivia :=
          ({
             kind = BlockComment;
             text = substring source index (stop - index);
             range;
           }
            : trivia)
          :: !pendingTrivia;
        if not closed then diagnostic range "unterminated block comment";
        skipTrivia stop endPosition
      end
      else (index, position)
  in
  (* These are ASCII grammar literals, not source names or Unicode text. *)
  let matchesAt text index =
    let count = String.length text in
    index + count <= length
    &&
    let rec equal offset =
      offset >= count
      || source.(index + offset) = Char.code text.[offset]
         && equal (offset + 1)
    in
    equal 0
  in
  let emit token start stop position =
    let endPosition = advance position start stop in
    let spanned =
      {
        token;
        text = substring source start (stop - start);
        range = { start = position; end_ = endPosition };
        docComment = !pendingDocComment;
        leadingTrivia = List.rev !pendingTrivia;
      }
    in
    pendingDocComment := None;
    pendingTrivia := [];
    (spanned, stop, endPosition)
  in
  let suffixes = [ "uy"; "us"; "ul"; "UL"; "L"; "Q"; "Z"; "y"; "s"; "l" ] in
  let suffixAt index =
    List.find_opt (fun suffix -> matchesAt suffix index) suffixes
  in
  let intToken digits suffixIndex =
    let suffix = Option.value ~default:"" (suffixAt suffixIndex) in
    let invalid = Error ("Invalid integer literal: " ^ digits) in
    let ascii =
      String.for_all (fun char -> char >= '0' && char <= '9') digits
    in
    let parse bits signed constructor label =
      let error =
        Error
          ((if label = "Int64" then "Integer literal too large: "
            else "out of range for " ^ label ^ ": ")
          ^ digits)
      in
      if not ascii then error
      else
        let value = Z.of_string_base 10 digits in
        let bound = Z.shift_left Z.one (if signed then bits - 1 else bits) in
        if Z.lt value bound then Ok (constructor value, String.length suffix)
        else if signed && digits = Z.to_string bound then
          Ok (constructor (Z.neg bound), String.length suffix)
        else error
    in
    match suffix with
    | "uy" -> parse 8 false (fun value -> TUInt8 (Z.to_int value)) "UInt8"
    | "us" -> parse 16 false (fun value -> TUInt16 (Z.to_int value)) "UInt16"
    | "ul" -> parse 32 false (fun value -> TUInt32 (Z.to_int64 value)) "UInt32"
    | "UL" ->
        parse 64 false
          (fun value ->
            TUInt64
              (Z.to_int64
                 (if Z.testbit value 63 then Z.sub value (Z.shift_left Z.one 64)
                  else value)))
          "UInt64"
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
    else if is source index '\\' && index + 1 < length then
      scanString (index + 2) closing
    else if is source index closing then Ok (index + 1)
    else scanString (index + 1) closing
  in
  let rec lineEnd index =
    if index >= length || is source index '\n' then index
    else lineEnd (index + 1)
  in
  let scanInterp start =
    let triple = start + 3 < length && matchesAt "\"\"\"" (start + 1) in
    let rec scan index =
      if index >= length then Error "Unterminated interpolated string"
      else if if triple then matchesAt "\"\"\"" index else is source index '"'
      then Ok (index + if triple then 3 else 1)
      else if (not triple) && index + 1 < length && is source index '\\' then
        scan (index + 2)
      else if
        index + 1 < length
        && ((is source index '{' && is source (index + 1) '{')
           || (is source index '}' && is source (index + 1) '}'))
      then scan (index + 2)
      else if is source index '{' then
        let close = findClose source length (index + 1) in
        if close = -1 then Error "Unterminated interpolated expression"
        else scan (close + 1)
      else scan (index + 1)
    in
    scan (start + if triple then 4 else 2)
  in
  let continue index =
    index < length
    && (letterOrDigit source.(index) || List.mem source.(index) [ 95; 39 ])
  in
  let rec scanWhile predicate index =
    if index < length && predicate index then scanWhile predicate (index + 1)
    else index
  in
  let rec go index position tokens =
    let index, position = skipTrivia index position in
    let push token stop message =
      let spanned, next, nextPosition = emit token index stop position in
      Option.iter (diagnostic spanned.range) message;
      go next nextPosition (spanned :: tokens)
    in
    if index >= length then
      let eof, _, _ = emit TEOF index index position in
      Ok (List.rev (eof :: tokens))
    else if matchesAt "=>" index then push TEquals (index + 1) None
    else if matchesAt "``" index then
      let close =
        scanWhile
          (fun scan ->
            scan + 1 < length
            && (not (matchesAt "``" scan))
            && not (is source scan '\n'))
          (index + 2)
      in
      if matchesAt "``" close then
        push
          (TIdent (substring source (index + 2) (close - index - 2)))
          (close + 2) None
      else
        let stop = lineEnd (index + 2) in
        push
          (TIdent (substring source (index + 2) (stop - index - 2)))
          stop (Some "unterminated backtick identifier")
    else if letter source.(index) || is source index '_' then
      let stop = scanWhile continue (index + 1) in
      push (keyword (substring source index (stop - index))) stop None
    else if digit source.(index) then begin
      let digitEnd = scanWhile (fun scan -> digit source.(scan)) (index + 1) in
      let fraction =
        digitEnd + 1 < length
        && is source digitEnd '.'
        && digit source.(digitEnd + 1)
      in
      let exponent =
        digitEnd < length && (is source digitEnd 'e' || is source digitEnd 'E')
      in
      if fraction || exponent then begin
        let stop =
          if fraction then
            scanWhile (fun scan -> digit source.(scan)) (digitEnd + 1)
          else digitEnd
        in
        let stop =
          if stop < length && (is source stop 'e' || is source stop 'E') then
            let start = stop + 1 in
            let start =
              if start < length && (is source start '+' || is source start '-')
              then start + 1
              else start
            in
            scanWhile (fun scan -> digit source.(scan)) start
          else stop
        in
        let number = substring source index (stop - index) in
        (* The scanned grammar excludes OCaml's hex, underscore, and suffix syntax. *)
        let value =
          if String.for_all (fun char -> char < '\128') number then
            float_of_string_opt number
          else None
        in
        match value with
        | Some value when not (continue stop) -> push (TFloat value) stop None
        | Some _ ->
            let stop = scanWhile continue stop in
            push (TFloat 0.) stop
              (Some
                 ("invalid number literal: "
                 ^ substring source index (stop - index)))
        | None ->
            push (TFloat 0.) stop (Some ("malformed float literal: " ^ number))
      end
      else
        let digits = substring source index (digitEnd - index) in
        match intToken digits digitEnd with
        | Ok (token, suffixLength) when not (continue (digitEnd + suffixLength))
          ->
            push token (digitEnd + suffixLength) None
        | Ok (_, suffixLength) ->
            let stop = scanWhile continue (digitEnd + suffixLength) in
            push (TInt64 0L) stop
              (Some
                 ("invalid number literal: "
                 ^ substring source index (stop - index)))
        | Error message ->
            let count =
              Option.fold ~none:0 ~some:String.length (suffixAt digitEnd)
            in
            push (TInt64 0L) (digitEnd + count) (Some message)
    end
    else if matchesAt "\"\"\"" index then begin
      let close =
        scanWhile (fun scan -> not (matchesAt "\"\"\"" scan)) (index + 3)
      in
      if close < length then
        push
          (TStringLit
             (Text.normalize (substring source (index + 3) (close - index - 3))))
          (close + 3) None
      else
        push
          (TStringLit
             (Text.normalize
                (substring source (index + 3) (length - index - 3))))
          length (Some "unterminated triple-quoted string literal")
    end
    else if is source index '"' then
      begin match scanString (index + 1) '"' with
      | Ok stop ->
          push
            (TStringLit
               (unescape (substring source (index + 1) (stop - index - 2))))
            stop None
      | Error _ ->
          let stop = lineEnd (index + 1) in
          push
            (TStringLit
               (unescape (substring source (index + 1) (stop - index - 1))))
            stop (Some "unterminated string literal")
      end
    else if is source index '\'' then begin
      let typeContext =
        match tokens with
        | previous :: _ ->
            List.mem previous.token
              [ TLParen; TStar; TLt; TComma; TColon; TArrow; TEquals; TOf ]
        | [] -> false
      in
      let quotedChar () =
        match scanString (index + 1) '\'' with
        | Ok stop ->
            push
              (TCharLit
                 (unescape (substring source (index + 1) (stop - index - 2))))
              stop None
        | Error _ ->
            let stop = lineEnd (index + 1) in
            push
              (TCharLit
                 (unescape (substring source (index + 1) (stop - index - 1))))
              stop (Some "unterminated char literal")
      in
      let contentEnd =
        if index + 1 >= length then index + 1
        else
          let remaining = substring source (index + 1) (length - index - 1) in
          let first = Text.firstGrapheme remaining in
          index + 1 + Option.fold ~none:0 ~some:Text.length first
      in
      if
        contentEnd < length && is source contentEnd '\''
        && not (is source (index + 1) '\\')
      then
        push
          (TCharLit
             (unescape (substring source (index + 1) (contentEnd - index - 1))))
          (contentEnd + 1) None
      else if
        typeContext
        && index + 1 < length
        && (letter source.(index + 1) || is source (index + 1) '_')
      then
        let stop =
          scanWhile
            (fun scan -> letterOrDigit source.(scan) || is source scan '_')
            (index + 1)
        in
        push
          (TIdent (substring source (index + 1) (stop - index - 1)))
          stop None
      else if index + 1 < length && is source (index + 1) '\\' then
        quotedChar ()
      else
        let stop = min length (max (index + 1) contentEnd) in
        push
          (TCharLit (unescape (substring source (index + 1) (stop - index - 1))))
          stop (Some "unterminated char literal")
    end
    else if matchesAt "$\"" index then
      begin match scanInterp index with
      | Ok stop -> push TInterpString stop None
      | Error _ ->
          push TInterpString
            (lineEnd (index + 2))
            (Some "unterminated interpolated string")
      end
    else
      match
        List.find_opt (fun (operator, _) -> matchesAt operator index) operators
      with
      | Some (operator, token) ->
          push token (index + String.length operator) None
      | None ->
          let nextPosition = advance position index (index + 1) in
          let character = substring source index 1 in
          diagnostic
            { start = position; end_ = nextPosition }
            ("unexpected character: '" ^ character ^ "'");
          go (index + 1) nextPosition tokens
  in
  Result.map
    (fun tokens -> (tokens, List.rev !diagnostics))
    (go 0 { row = 0; column = 0 } [])
