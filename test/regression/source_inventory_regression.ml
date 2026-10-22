(* source_inventory_regression.ml - Count complete test groups and declarations without typing them. *)
open Dark_compiler
module I = SourceInventory
module A = CompilerAnnotations

let require condition message = if not condition then failwith message

let parse source =
  match I.parse ~reasons:[ "runtime-reflection" ] source with
  | Ok result -> result
  | Error message -> failwith message

let () =
  let result =
    parse
      "module Example\n\
       val seed = 1\n\
       let wrong () : String = 42\n\
       type Box = { value: Int }\n\
       trait Show<'a> =\n\
      \  let show (value: 'a) : String\n\
       impl Show for Int =\n\
      \  let show (value: Int) : String = \"int\"\n\
       module Tests =\n\
      \  // @compiler excluded: runtime-reflection\n\
      \  test unsupported = Unknown.evaluate seed\n\
      \  test grouped = Stdlib.Test.all [ Stdlib.Test.equal 1 1, \
       Stdlib.Test.equal 2 2 ]"
  in
  require
    (List.length result.I.entries = 9)
    "A declaration or named test was lost";
  require
    (List.length
       (List.filter
          (fun (entry : I.entry) -> I.isTest entry.I.kind)
          result.I.entries)
    = 2)
    "Test.all assertions counted as separate tests";
  let blocked =
    List.filter
      (fun (entry : I.entry) ->
        entry.I.disposition = A.Excluded "runtime-reflection")
      result.I.entries
  in
  require
    (List.map (fun (entry : I.entry) -> entry.I.name) blocked
    = [ "Example.Tests.unsupported" ])
    "Exclusion leaked into another item";
  let legacy =
    parse
      "module Old\n\
       val seed = 1\n\
       // @compiler excluded: runtime-reflection\n\
       Unknown.lookup seed = 1\n\
       (1 +\n\
      \ 2) = 3"
  in
  require
    (List.length
       (List.filter
          (fun (entry : I.entry) -> I.isTest entry.I.kind)
          legacy.I.entries)
    = 2)
    "Multiline legacy test was not one case";
  require
    (Result.is_error
       (I.parse ~reasons:[] "// @compiler blocked: typo\nval seed = 1"))
    "Invalid metadata escaped inventory validation";
  print_endline "Source inventory regressions passed"
