(* ExpressionInterpolation.ml - Unicode scalar string scans and nested expression diagnostics. *)
[@@@warning "-4"]
open Tokenizer
open ParserSupport
module WT = WrittenTypes
type parseTokensAt = int -> ItemScope.t -> Lexer.spannedToken array -> parseResult
let replaceDoubleBraces text =
  let replace from into text =
    let buffer = Buffer.create (String.length text) in
    let rec loop index =
      if index < String.length text then
        if index + 1 < String.length text && text.[index] = from && text.[index + 1] = from
        then begin Buffer.add_char buffer into; loop (index + 2) end
        else begin Buffer.add_char buffer text.[index]; loop (index + 1) end in
    loop 0; Buffer.contents buffer in
  replace '}' '}' (replace '{' '{' text)
let parseInterpString (parseTokensAt : parseTokensAt) state index =
  let spanned = state.toks.(index) in
  let fullText = spanned.Lexer.text and basePos = spanned.Lexer.range.start in
  let units = Text.scalars fullText in
  let length = Array.length units in
  let starts = RevBuffer.create () in
  RevBuffer.add starts 0;
  Array.iteri (fun index unit -> if unit = 10 then RevBuffer.add starts (index + 1)) units;
  let starts = Array.of_list (RevBuffer.toList starts) in
  let posAt offset =
    let offset = min offset length in
    let low = ref 0 and high = ref (Array.length starts - 1) in
    while !low < !high do
      let middle = (!low + !high + 1) / 2 in
      if starts.(middle) <= offset then low := middle else high := middle - 1
    done;
    let column = offset - starts.(!low) in
    if !low = 0 then {row = basePos.row; column = basePos.column + column}
    else {row = basePos.row + !low; column} in
  let rangeAt first last = {start = posAt first; end_ = posAt last} in
  let slice first last = Text.ofScalars (Array.sub units first (last - first)) in
  let triple = length >= 4 && units.(1) = 34 && units.(2) = 34 && units.(3) = 34 in
  let dollar = rangeAt 0 1 in
  let bodyStart = if triple then 4 else 2 and closeLength = if triple then 3 else 1 in
  let opening = rangeAt 1 bodyStart in
  let contents = RevBuffer.create () and textStart = ref bodyStart and stop = ref bodyStart in
  let more = ref true and foundClosing = ref false in
  let flushText ending =
    if ending > !textStart then
      let raw = replaceDoubleBraces (slice !textStart ending) in
      let text = if triple then Text.normalize raw else Lexer.unescape raw in
      RevBuffer.add contents (WT.StringText (rangeAt !textStart ending, text)) in
  let addDiagnostic code range message =
    state.diagnostics := {code; severity = DiagError; range; message; related = []; hint = None} :: !(state.diagnostics) in
  while !more && !stop < length do
    let atClose = if triple then !stop + 2 < length && units.(!stop) = 34 && units.(!stop + 1) = 34 && units.(!stop + 2) = 34 else units.(!stop) = 34 in
    if atClose then begin flushText !stop; foundClosing := true; more := false end
    else if units.(!stop) = 92 && not triple && !stop + 1 < length then stop := !stop + 2
    else if (units.(!stop) = 123 || units.(!stop) = 125) && !stop + 1 < length && units.(!stop + 1) = units.(!stop) then stop := !stop + 2
    else if units.(!stop) = 123 then begin
      flushText !stop;
      let braceOpen = rangeAt !stop (!stop + 1) in
      let found = Lexer.findInterpExprClose fullText length (!stop + 1) in
      if found < 0 then more := false else begin
        let exprText = slice (!stop + 1) found and exprStart = posAt (!stop + 1) in
        let braceClose = rangeAt found (found + 1) and bodyRange = rangeAt (!stop + 1) found in
        let inner = if state.interpDepth >= Lexer.maxInterpNesting then begin
          errFull state DiagnosticCode.tooDeep index (Printf.sprintf "string interpolation nested too deeply (over %d levels); parsing abandoned" Lexer.maxInterpNesting) [] None;
          state.abandoned <- true; WT.EUnit bodyRange
        end else
          let offset position = if position.row = 0 then {row = exprStart.row; column = exprStart.column + position.column}
            else {row = exprStart.row + position.row; column = position.column} in
          let offsetRange range = {start = offset range.start; end_ = offset range.end_} in
          match Lexer.tokenize exprText with
          | Error error -> err state DiagnosticCode.lex index error; WT.EUnit bodyRange
          | Ok (tokens, lexDiagnostics) ->
              List.iter (fun (range, message) -> if not state.abandoned then addDiagnostic DiagnosticCode.lex (offsetRange range) message) lexDiagnostics;
              let tokens = Array.of_list (List.map (fun (token : Lexer.spannedToken) -> {token with Lexer.range = offsetRange token.Lexer.range}) tokens) in
              let result = parseTokensAt (state.interpDepth + 1) ItemScope.Script tokens in
              List.iter (fun diagnostic -> state.diagnostics := diagnostic :: !(state.diagnostics)) result.diagnostics;
              (match result.parsed with
              | None -> WT.EUnit bodyRange
              | Some (WT.SourceFile file) ->
                  let interpolationError = addDiagnostic DiagnosticCode.interpolation bodyRange in
                  if file.WT.declarations <> [] then interpolationError "Interpolation body must be one expression, not a declaration";
                  match file.WT.declarations, file.WT.exprsToEval with
                  | [], [expression] -> expression
                  | [], expression :: _ -> interpolationError "Interpolation body must contain exactly one expression"; expression
                  | _ ->
                      if file.WT.declarations = [] then interpolationError "Interpolation body cannot be empty";
                      WT.EUnit bodyRange) in
        RevBuffer.add contents (WT.StringInterpolation (rangeAt !stop (found + 1), inner, braceOpen, braceClose));
        stop := found + 1; textStart := !stop
      end
    end else incr stop
  done;
  if not !foundClosing then flushText length;
  let closing = if !foundClosing then rangeAt (length - closeLength) length else rangeAt length length in
  WT.EString (spanned.Lexer.range, Some dollar, RevBuffer.toList contents, opening, closing), index + 1
