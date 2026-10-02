(* Parser.ml - Assemble the frozen grammar and enforce post-syntax validation. *)
[@@@warning "-4"]
open Tokenizer
open ParserSupport
module WT = WrittenTypes
let rec parseExpr state index =
  ExpressionControl.parseExpr {ExpressionControl.parseExpr; parseInfix} state index
and parseInfix state index =
  ExpressionPrecedence.parseInfix {ExpressionPrecedence.parseExpr; parsePrimary} state index
and parseApp state index =
  ExpressionPrecedence.parseApp {ExpressionPrecedence.parseExpr; parsePrimary} state index
and parseBlock state index =
  ExpressionControl.parseBlock {ExpressionControl.parseExpr; parseInfix} state index
and parsePrimary state index =
  ExpressionPrimary.parsePrimary {ExpressionPrimary.parseExpr; parseBlock; parseApp; parseCtorParenFields; parseInterpString} state index
and parseCtorParenFields state index sink =
  ExpressionPrecedence.parseCtorParenFields {ExpressionPrecedence.parseExpr; parsePrimary} state index sink
and parseInterpString state index = ExpressionInterpolation.parseInterpString parseTokensAt state index
and parseTokensAt depth scope tokens =
  let state = makeState depth tokens in
  FileParser.parseFile {FileParser.parseExpr; parseBlock} scope state
let parseTokens tokens = parseTokensAt 0 ItemScope.Script tokens
let lexical range message = {code = DiagnosticCode.lex; severity = DiagError; range; message; related = []; hint = None}
let zero = {start = {row = 0; column = 0}; end_ = {row = 0; column = 0}}
let parseSyntaxWithRootScope rootScope source =
  match Lexer.tokenize source with
  | Error message -> {parsed = None; diagnostics = [lexical zero message]}
  | Ok (tokens, lexDiagnostics) ->
      let result = parseTokensAt 0 rootScope (Array.of_list tokens) in
      {result with diagnostics = List.map (fun (range, message) -> lexical range message) lexDiagnostics @ result.diagnostics}
let parse source =
  let result = parseSyntaxWithRootScope ItemScope.Script source in
  let structural = match result.diagnostics, result.parsed with
    | [], Some (WT.SourceFile file) -> List.map diagnosticOfValidationIssue (Validation.validateStructure file)
    | _ -> [] in
  {result with diagnostics = result.diagnostics @ structural}
let parseFor mode source =
  let scope = match mode with Validation.Package -> ItemScope.Module | Validation.Script | Validation.Test -> ItemScope.Script in
  let result = parseSyntaxWithRootScope scope source in
  match result.diagnostics, result.parsed with
  | [], Some (WT.SourceFile file) ->
      (match Validation.validate mode file with Ok value -> Ok value
      | Error issues -> Error (List.map diagnosticOfValidationIssue (ParserDependencies.toList issues)))
  | (_ :: _ as diagnostics), _ -> Error diagnostics
  | [], None -> Error [{code = DiagnosticCode.unexpected; severity = DiagError; range = zero;
      message = "Parser did not produce a source tree"; related = []; hint = None}]
let parseTestFile = parse
let renderDiagnostic source (diagnostic : diagnostic) =
  let lines = Array.of_list (String.split_on_char '\n' source) in
  let snippet range =
    if range.start.row < 0 || range.start.row >= Array.length lines then [] else
    let original = HostText.utf16Units lines.(range.start.row) in
    let length = ref (Array.length original) in
    while !length > 0 && original.(!length - 1) = 13 do decr length done;
    let line = HostText.ofUtf16Units (Array.sub original 0 !length) in
    let number = string_of_int (range.start.row + 1) in
    let column = max 0 (min range.start.column !length) in
    let width = if range.start.row = range.end_.row then max 1 (min (range.end_.column - range.start.column) (max 1 (!length - column))) else 1 in
    ["  " ^ number ^ " | " ^ line; "  " ^ String.make (String.length number) ' ' ^ " | " ^ String.make column ' ' ^ String.make width '^'] in
  let first = Printf.sprintf "error[%s] at %d:%d: %s" diagnostic.code (diagnostic.range.start.row + 1) (diagnostic.range.start.column + 1) diagnostic.message in
  let related = List.concat_map (fun (range, note) ->
    Printf.sprintf "  note: %s (%d:%d)" note (range.start.row + 1) (range.start.column + 1) :: snippet range) diagnostic.related in
  let hint = match diagnostic.hint with None -> [] | Some text -> ["  hint: " ^ text] in
  String.concat "\n" (first :: snippet diagnostic.range @ related @ hint)
