(*
   TypeCheckingFormat.fs - Type checking test format parser
   Parses type checking test files in a simple line-based format.
   Format: expression : expected_type  // optional comment
   expression : error          // type error expected
   Example:
   // Integer literals
   42 : int
   2 + 3 : int
   // Type errors
   1 + true : error  // cannot add int and bool
*)
(* Parse the original line-based type checking test format. *)
open Dark_compiler
(*
   Type checking test expectation
*)
type typeExpectation = ExpectType of AST.semanticType | ExpectError
(*
   Type checking test specification
*)
type typeCheckingTest = {name : string; source : string; expectation : typeExpectation}
(*
   Parse type string to AST.SemanticType
*)
let parseType text = match HostText.lowerInvariant (HostText.trim text) with
 | "int" | "int64" -> Ok AST.TInt64
 | "bool" | "boolean" -> Ok AST.TBool
 | "float" | "float64" -> Ok AST.TFloat64
 | "string" -> Ok AST.TString
 | "unit" -> Ok AST.TUnit
 | "error" -> Error "Use 'error' keyword, not as a type"
 | other -> Error ("Unknown type: " ^ other)
let isEscapedQuote units index =
 let rec count index total = if index < 0 || units.(index) <> 0x5c then total else count (index - 1) (total + 1) in
 count (index - 1) 0 mod 2 = 1
(*
   Parse a single test line
   Format: source : expectation  // comment
   First, remove any comment
   Find the : that separates source from expectation
   It's the last : that's not inside quotes
   Parse expectation
*)
let parseTestLine line lineNumber =
 let units = HostText.scalars line in
 let substring units start length = HostText.ofScalars (Array.sub units start length) in
 let rec commentStart index inQuotes =
  if index >= Array.length units - 1 then None
  else if units.(index) = 0x22 && not (isEscapedQuote units index) then commentStart (index + 1) (not inQuotes)
  else if units.(index) = 0x2f && units.(index + 1) = 0x2f && not inQuotes then Some index
  else commentStart (index + 1) inQuotes in
 let text, comment = match commentStart 0 false with
 | None -> line, None
 | Some index -> HostText.trim (substring units 0 index), Some (HostText.trim (substring units (index + 2) (Array.length units - index - 2))) in
 let units = HostText.scalars text in
 let rec separator index inQuotes last =
  if index >= Array.length units then last
  else if units.(index) = 0x22 && not (isEscapedQuote units index) then separator (index + 1) (not inQuotes) last
  else if units.(index) = 0x3a && not inQuotes then separator (index + 1) inQuotes (Some index)
  else separator (index + 1) inQuotes last in
 let prefix = "Line " ^ string_of_int lineNumber ^ ": " in
 match separator 0 false None with
 | None -> Error (prefix ^ "Expected format 'source : expectation', got: " ^ line)
 | Some index ->
  let source = HostText.trim (substring units 0 index) and expectation = HostText.trim (substring units (index + 1) (Array.length units - index - 1)) in
  let expectation = if HostText.lowerInvariant expectation = "error" then Ok ExpectError else Result.map (fun typ -> ExpectType typ) (parseType expectation) in
  match expectation with Error message -> Error (prefix ^ message) | Ok expectation -> Ok {name = Option.value comment ~default:(prefix ^ source); source; expectation}
(*
   Parse all type checking tests from a single file
   Skip blank lines and comments
*)
let parseTypeCheckingTestFile path =
 if not (TestFileIO.exists path) then Error ("Test file not found: " ^ path) else
 let tests, errors = Array.fold_left (fun (tests, errors) (index, raw) -> let line = HostText.trim raw in
  if line = "" || String.starts_with ~prefix:"//" line then tests, errors else
  match parseTestLine line (index + 1) with Ok test -> test :: tests, errors | Error error -> tests, error :: errors) ([], []) (Array.mapi (fun index line -> index, line) (TestFileIO.readAllLines path)) in
 match errors with [] -> Ok (List.rev tests) | _ -> Error (String.concat "\n" (List.rev errors))
