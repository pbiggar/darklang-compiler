(* named_test_partition_regression.ml - Preserve test scopes and package declarations separately. *)
[@@@warning "-4"]
open Dark_compiler
module WT = WrittenTypes
module NPT = NamedPackageTests

let require condition message = if not condition then failwith message

let package source =
  match Parser.parseFor Validation.Package source with
  | Ok validated -> validated
  | Error _ -> failwith "Package did not parse"

let partition source =
  match NPT.partition source with
  | Ok result -> result
  | Error message -> failwith message

let rec originalTests declarations =
  List.concat_map
    (function
      | WT.DTest test -> [ test ]
      | WT.DModule modul -> originalTests modul.WT.declarations
      | _ -> [])
    declarations

let () =
  let source =
    package
      "module Example\nval seed = 3L\nlet helper (value: Int64) : Int64 = value\n/// Preserved documentation.\ntest same = helper seed\nmodule Nested =\n  val nestedSeed = 4L\n  test same = nestedSeed => raises \"boom\"\ntest last = 5L\n"
  in
  let original = Validation.ValidatedSourceFile.toWrittenTypes source in
  let result = partition source in
  require
    (List.map (fun test -> test.NPT.qualifiedName) result.NPT.tests
     = [ "Example.same"; "Example.Nested.same"; "Example.last" ])
    "Qualified test order or scope changed";
  require
    (List.map (fun test -> test.NPT.modules) result.NPT.tests
     = [ [ "Example" ]; [ "Example"; "Nested" ]; [ "Example" ] ])
    "Test lexical scope changed";
  require
    (List.map (fun test -> test.NPT.declaration) result.NPT.tests
     = originalTests original.WT.declarations)
    "Test body, ranges, documentation or expected error changed";
  let stripped =
    Validation.ValidatedSourceFile.toWrittenTypes result.NPT.declarations
  in
  require (originalTests stripped.WT.declarations = [])
    "Named tests leaked into executable declarations";
  require (stripped.WT.range = original.WT.range) "File range changed";
  (match WrittenSource.items result.NPT.declarations with
  | Ok
      [ WrittenSource.Value ([ "Example" ], seed);
        WrittenSource.Function ([ "Example" ], helper);
        WrittenSource.Value ([ "Example"; "Nested" ], nestedSeed) ] ->
      require (seed.WT.name.WT.name = "seed") "Outer value changed";
      require (helper.WT.name.WT.name = "helper") "Helper function changed";
      require (nestedSeed.WT.name.WT.name = "nestedSeed") "Nested value changed"
  | _ -> failwith "Executable package declarations changed");
  require
    (match WrittenSource.items source with Error _ -> true | Ok _ -> false)
    "Ordinary compiler silently discarded package tests";
  require
    (match
       NPT.partition
         (package "module Left =\n  test same = 1\nmodule Right =\n  test same = 2\n")
     with
    | Error _ -> false
    | Ok _ -> true)
    "Distinct qualified test names rejected";
  require
    (match NPT.partition (package "test duplicate = 1\ntest duplicate = 2\n") with
    | Error "Duplicate named package test 'duplicate'" -> true
    | _ -> false)
    "Duplicate test identity accepted";
  let ordinary = package "module Ordinary\nval seed = 1\n" in
  let unchanged = partition ordinary in
  require (unchanged.NPT.tests = []) "Invented tests";
  require
    (Validation.ValidatedSourceFile.toWrittenTypes unchanged.NPT.declarations
     = Validation.ValidatedSourceFile.toWrittenTypes ordinary)
    "Test-free package changed";
  List.iter
    (fun selected ->
      match NPT.executable source selected.NPT.qualifiedName with
      | Error message -> failwith message
      | Ok (executable, test) ->
          require (test = selected) "Selected test metadata changed";
          require
            (Validation.ValidatedSourceFile.mode executable = Validation.Script)
            "Executable retained Package purpose";
          (match WrittenSource.items executable with
          | Error message -> failwith message
          | Ok items ->
              let entries =
                List.filter_map
                  (function
                    | WrittenSource.Expression (modules, body) ->
                        Some (modules, body)
                    | _ -> None)
                  items
              in
              require
                (entries = [ (selected.NPT.modules, selected.NPT.declaration.WT.body) ])
                "Entry body or lexical scope changed");
          (match WrittenChecking.checkSimpleProgram true executable with
          | Ok (AST.TInt64, _) -> ()
          | Ok _ -> failwith "Selected test result type changed"
          | Error message -> failwith message))
    result.NPT.tests;
  require
    (match NPT.executable source "same" with
    | Error "Unknown named package test 'same'" -> true
    | _ -> false)
    "Ambiguous short test name accepted";
  require
    (match NPT.executable ordinary "missing" with Error _ -> true | _ -> false)
    "Test-free source acquired an executable entry";
  let independent =
    package
      "module Example\nval seed = 3L\ntest selected = seed\nval later = 4L\ntest broken = missingFunction ()\n"
  in
  (match NPT.executable independent "Example.selected" with
  | Error message -> failwith message
  | Ok (executable, _) ->
      (match WrittenSource.items executable with
      | Ok
          [ WrittenSource.Value (_, _);
            WrittenSource.Expression ([ "Example" ], _);
            WrittenSource.Value (_, _) ] -> ()
      | _ -> failwith "Entry moved past ordinary declarations");
      (match WrittenChecking.checkSimpleProgram true executable with
      | Ok (AST.TInt64, _) -> ()
      | Ok _ -> failwith "Independent test result type changed"
      | Error message -> failwith message));
  require
    (match
       NPT.executable
         (package "test duplicate = 1\ntest duplicate = 2\n")
         "duplicate"
     with
    | Error "Duplicate named package test 'duplicate'" -> true
    | _ -> false)
    "Executable selected an arbitrary duplicate test";
  (match Parser.parseFor Validation.Script "1" with
  | Ok script ->
      require
        (match NPT.partition script with Error _ -> true | Ok _ -> false)
        "Script accepted as package";
      require
        (match NPT.executable script "missing" with Error _ -> true | Ok _ -> false)
        "Script accepted for package test execution"
  | Error _ -> failwith "Script did not parse");
  print_endline "Named package test partition regressions passed"
