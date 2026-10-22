(* compiler_annotations_regression.ml - Verify item attachment and strict source metadata. *)
[@@@warning "-4-42"]

open Dark_compiler
module A = CompilerAnnotations

let reasons = [ "runtime-reflection"; "interpreter-intrinsic"; "test-counter" ]

let item row column isTest : A.item =
  { A.position = { Tokenizer.row; column }; isTest }

let read items source = A.read ~reasons ~items source
let require condition message = if not condition then failwith message

let accepts items source =
  match read items source with
  | Ok entries -> entries
  | Error message -> failwith message

let rejects items source =
  require
    (Result.is_error (read items source))
    ("Unexpectedly accepted: " ^ source)

let () =
  let entries =
    accepts
      [ item 1 0 false ]
      "// @compiler excluded: runtime-reflection\nlet lookup () : Int = 1"
  in
  require
    (A.at entries { Tokenizer.row = 1; column = 0 }
    = A.Excluded "runtime-reflection")
    "Exclusion not attached";
  require
    (A.at entries { Tokenizer.row = 4; column = 0 } = A.Run)
    "Unannotated item did not run";
  let entries =
    accepts
      [ item 2 2 true ]
      "module Tests =\n  // @compiler blocked: test-counter\n  test once = 1"
  in
  require
    (A.at entries { Tokenizer.row = 2; column = 2 } = A.Blocked "test-counter")
    "Nested test lost metadata";
  let entries =
    accepts
      [ item 1 0 false ]
      "// @compiler replace: compiler/base64.dark#decode \
       reason=interpreter-intrinsic\n\
       let decode () : Int = 1"
  in
  require
    (match A.at entries { Tokenizer.row = 1; column = 0 } with
    | A.Replace { target; reason } ->
        target = "compiler/base64.dark#decode"
        && reason = "interpreter-intrinsic"
    | _ -> false)
    "Replacement lost target or reason";
  let entries =
    accepts
      [ item 1 0 true ]
      "// @compiler xfail compile: \"Unknown function\" reason=test-counter\n\
       test once = 1"
  in
  require
    (match A.at entries { Tokenizer.row = 1; column = 0 } with
    | A.Xfail
        {
          phase = A.Compile;
          diagnostic = "Unknown function";
          reason = "test-counter";
        } ->
        true
    | _ -> false)
    "Expected failure contract changed";
  require
    (accepts [] "val text = \"// @compiler blocked: typo\"" = [])
    "String content treated as metadata";
  require
    (accepts [] "(* // @compiler blocked: typo *)" = [])
    "Block comment treated as metadata";
  rejects [ item 1 0 true ] "// @compiler blocked: typo\ntest once = 1";
  rejects
    [ item 1 0 true ]
    "// @compiler-file blocked: test-counter\ntest once = 1";
  rejects [] "// @compiler blocked: test-counter";
  rejects
    [ item 2 0 true ]
    "// @compiler blocked: test-counter\n\
     // @compiler excluded: runtime-reflection\n\
     test once = 1";
  rejects
    [ item 0 0 false ]
    "val seed = 1 // @compiler blocked: test-counter\nval next = 2";
  rejects
    [ item 1 0 false ]
    "// @compiler xfail runtime: \"boom\" reason=test-counter\n\
     let bad () : Int = 1";
  rejects
    [ item 1 0 true ]
    "// @compiler replace: compiler/a.dark#b reason=interpreter-intrinsic\n\
     test once = 1";
  rejects
    [ item 1 0 false ]
    "// @compiler replace: ../outside.dark#b reason=interpreter-intrinsic\n\
     let b () : Int = 1";
  rejects
    [ item 1 0 false ]
    "// @compiler replace: compiler/a.dark#b\nlet b () : Int = 1";
  rejects
    [ item 1 0 true ]
    "// @compiler xfail runtime: \"\" reason=test-counter\ntest once = 1";
  print_endline "Compiler annotation regressions passed"
