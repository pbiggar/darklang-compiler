(* named_test_parser_regression.ml - Preserve package test syntax and purpose boundaries. *)
[@@@warning "-4-42"]

open Dark_compiler
module WT = WrittenTypes

let require condition message = if not condition then failwith message

let parse source =
  let result = Parser.parse source in
  require (result.ParserSupport.diagnostics = []) "Unexpected syntax diagnostic";
  match result.ParserSupport.parsed with
  | Some (WT.SourceFile file) -> file
  | None -> failwith "Missing parsed file"

let accepted mode source =
  match Parser.parseFor mode source with Ok _ -> true | Error _ -> false

let () =
  let source =
    "module Example\n\
     /// A package test.\n\
     test simple = 1\n\
     test failing = 2 => raises \"boom\"\n\
     module Nested =\n\
    \  test inner = 3\n"
  in
  (match (parse source).WT.declarations with
  | [ WT.DModule outer ] -> (
      match outer.WT.declarations with
      | [ WT.DTest simple; WT.DTest failing; WT.DModule nested ] ->
          require (simple.WT.name.WT.name = "simple") "Test name lost";
          require (simple.WT.description = "A package test.") "Doc comment lost";
          require (simple.WT.expectedError = None) "Invented expected error";
          require
            (simple.WT.keywordTest.Tokenizer.start.Tokenizer.row = 2)
            "Keyword range lost";
          require
            (simple.WT.range.Tokenizer.end_.Tokenizer.row = 2)
            "Test consumed next declaration";
          require
            (match failing.WT.expectedError with
            | Some (_, WT.RuntimeError "boom") -> true
            | _ -> false)
            "Expected runtime error lost";
          require (failing.WT.symbolOutcome <> None) "Outcome range lost";
          require (failing.WT.errorKindRange <> None) "Error kind range lost";
          require
            (match nested.WT.declarations with
            | [ WT.DTest inner ] -> inner.WT.name.WT.name = "inner"
            | _ -> false)
            "Nested test lost"
      | _ -> failwith "Named declarations have wrong shape")
  | _ -> failwith "Module header lost");
  require (accepted Validation.Package source) "Package test rejected";
  List.iter
    (fun mode ->
      require
        (not (accepted mode "test simple = 1"))
        "Named test accepted outside package")
    [ Validation.Script; Validation.Test ];
  (match (parse "1 = 1").WT.declarations with
  | [ WT.DAssertion _ ] -> ()
  | _ -> failwith "Legacy assertion changed shape");
  require (accepted Validation.Test "1 = 1") "Legacy assertion rejected";
  require
    (not (accepted Validation.Package "1 = 1"))
    "Legacy assertion accepted in package";
  require
    (accepted Validation.Script "test 1")
    "Contextual test identifier rejected";
  require
    (accepted Validation.Package "val test = 1")
    "Value named test rejected";
  require
    (accepted Validation.Package "test multiline =\n  let x = 1\n  x\n")
    "Multiline test rejected";
  List.iter
    (fun source ->
      require
        ((Parser.parse source).ParserSupport.diagnostics <> [])
        "Invalid outcome accepted")
    [
      "test t = 1 => raises 2";
      "test t = 1 => other \"boom\"";
      "test t : [] = 1";
      "test t raises \"boom\" = 1";
      "test t = 1 = > raises \"boom\"";
    ];
  print_endline "Named package test parser regressions passed"
