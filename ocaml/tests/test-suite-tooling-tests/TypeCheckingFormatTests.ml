(*
   TypeCheckingFormatTests.fs - Unit tests for type checking test parsing
   Verifies parser behavior for the line-based type checking test format.
*)
(* Original type checking fixture parser tests. *)
[@@@warning "-4"]
open Dark_compiler
open TypeCheckingFormat
type testResult = (unit, string) result
let withTempFile content test =
 let path = Filename.temp_file "dark-typechecking-" ".typecheck" in
 Out_channel.with_open_bin path (fun channel -> output_string channel content);
 Fun.protect ~finally:(fun () -> if Sys.file_exists path then Sys.remove path) (fun () -> test path)
let testParsesSlashSlashInsideStringLiteral () =
 withTempFile "\"https://darklang.com\" : string  // string containing URL" (fun path -> match parseTypeCheckingTestFile path with
 | Ok [test] when test.source = "\"https://darklang.com\"" -> Ok ()
 | Ok [test] -> Error ("Expected source to preserve // inside string literal, got: " ^ test.source)
 | Ok tests -> Error ("Expected one parsed type checking test, got " ^ string_of_int (List.length tests))
 | Error message -> Error ("Expected type checking test with // inside string literal to parse, got: " ^ message))
let testParsesTypeKeywordsWithCultureInvariantCasing () =
 (* HostText invariant casing does not depend on the process locale. *)
 withTempFile "1 : INT" (fun path -> match parseTypeCheckingTestFile path with
 | Ok [{expectation = ExpectType AST.TInt64; _}] -> Ok ()
 | Ok [_] -> Error "Expected INT to parse as int64"
 | Ok tests -> Error ("Expected one parsed type checking test, got " ^ string_of_int (List.length tests))
 | Error message -> Error ("Expected uppercase INT to parse under Turkish culture, got: " ^ message))
let testParsesEscapedQuoteBeforeSlashSlashInsideStringLiteral () =
 withTempFile "\"say \\\"//\\\"\" : string  // string containing escaped quote and slashes" (fun path -> match parseTypeCheckingTestFile path with
 | Ok [test] when test.source = "\"say \\\"//\\\"\"" -> Ok ()
 | Ok [test] -> Error ("Expected source to preserve escaped quotes before //, got: " ^ test.source)
 | Ok tests -> Error ("Expected one parsed type checking test, got " ^ string_of_int (List.length tests))
 | Error message -> Error ("Expected type checking test with escaped quote before // to parse, got: " ^ message))
let tests = ["parse // inside type checking string literal", testParsesSlashSlashInsideStringLiteral; "parse type keywords with culture-invariant casing", testParsesTypeKeywordsWithCultureInvariantCasing; "parse escaped quote before // inside type checking string literal", testParsesEscapedQuoteBeforeSlashSlashInsideStringLiteral]
let runAll () =
 let rec run = function [] -> Ok () | (name, test) :: tail -> match test () with Ok () -> run tail | Error message -> Error (name ^ " test failed: " ^ message) in run tests
