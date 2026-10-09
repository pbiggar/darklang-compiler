(*
   OptimizationFormatTests.ml - Unit tests for optimization test parsing
   Verifies the optimization test file parser accepts repository test syntax
   across common line-ending formats.
*)
open Dark_compiler
open OptimizationFormat

type testResult = (unit, string) result

let withTempFile content test =
  let path = Filename.temp_file "dark-" ".opt" in
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel content);
  Fun.protect
    ~finally:(fun () -> if TestFileIO.exists path then Sys.remove path)
    (fun () -> test path)

let testParseCRLFOptimizationFile () =
  withTempFile
    "---NAME---\r\n\
     fold_add\r\n\
     ---INPUT---\r\n\
     1 + 2\r\n\
     ---EXPECTED---\r\n\
     return 3" (fun path ->
      match parseTestFile ANF path with
      | Ok [ test ]
        when test.name = "fold_add"
             && test.input = Source "1 + 2"
             && test.expectedIR = "return 3" ->
          Ok ()
      | Ok tests ->
          Error
            (Printf.sprintf "Expected one parsed CRLF optimization test, got %d"
               (List.length tests))
      | Error msg ->
          Error ("Expected CRLF optimization test file to parse, got: " ^ msg))

let testUnknownOptimizationSectionFails () =
  withTempFile
    {fixture|---NAME---
fold_add
---INPUT---
1 + 2
---EXPECTED---
return 3
---OUTPUT---
ignored|fixture}
    (fun path ->
      match parseTestFile ANF path with
      | Error msg when Text.contains msg "Unknown optimization section: OUTPUT"
        ->
          Ok ()
      | Ok _ -> Error "Expected unknown optimization section to fail"
      | Error msg -> Error ("Expected unknown section error, got: " ^ msg))

let testParseStdlibFunctionOptimization () =
  withTempFile
    {fixture|---NAME---
stdlib_strength_reduction
---STDLIB-FUNCTION---
Darklang.Stdlib.Int64.__powerLoop
---EXPECTED---
Function Darklang.Stdlib.Int64.__powerLoop:|fixture}
    (fun path ->
      match parseTestFile ANF path with
      | Ok [ test ]
        when test.input = StdlibFunction "Darklang.Stdlib.Int64.__powerLoop" ->
          Ok ()
      | Ok tests ->
          Error
            (Printf.sprintf
               "Expected one parsed stdlib optimization test, got %d"
               (List.length tests))
      | Error msg ->
          Error ("Expected stdlib optimization test to parse, got: " ^ msg))

let testStdlibFunctionRejectsNonANFStage () =
  withTempFile
    {fixture|---NAME---
invalid_stdlib_stage
---STDLIB-FUNCTION---
Darklang.Stdlib.Int64.__powerLoop
---EXPECTED---
unused|fixture}
    (fun path ->
      match parseTestFile MIR path with
      | Error msg when Text.contains msg "supported only for ANF" -> Ok ()
      | Ok _ -> Error "Expected a MIR stdlib-function fixture to fail"
      | Error msg -> Error ("Expected the stage diagnostic, got: " ^ msg))

let tests =
  [
    ("parse CRLF optimization file", testParseCRLFOptimizationFile);
    ("unknown optimization section fails", testUnknownOptimizationSectionFails);
    ("parse stdlib function optimization", testParseStdlibFunctionOptimization);
    ( "stdlib function optimization rejects non-ANF stage",
      testStdlibFunctionRejectsNonANFStage );
  ]

let runAll () =
  let rec run = function
    | [] -> Ok ()
    | (name, test) :: rest -> (
        match test () with
        | Ok () -> run rest
        | Error msg -> Error (name ^ " test failed: " ^ msg))
  in
  run tests
