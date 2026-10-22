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
  let named = parse "test grouped = (\n  Stdlib.Test.pass ()\n)\n" in
  require
    (match named.I.tree.WrittenTypes.declarations with
    | [ WrittenTypes.DTest test ] ->
        test.WrittenTypes.range.Tokenizer.end_.Tokenizer.row = 2
        && test.WrittenTypes.range.Tokenizer.end_.Tokenizer.column = 1
    | []
    | (WrittenTypes.DFunction _ | WrittenTypes.DValue _ | WrittenTypes.DType _
      | WrittenTypes.DTrait _ | WrittenTypes.DImpl _ | WrittenTypes.DModule _
      | WrittenTypes.DAssertion _ | WrittenTypes.DExpr _) :: _
    | WrittenTypes.DTest _ :: _ :: _ -> false)
    "Named test range omitted its closing parenthesis";
  let grouped =
    parse
      "// @compiler excluded: runtime-reflection\n(\n  Unknown.value ()\n) = 1"
  in
  require
    (match grouped.I.entries with
    | [ entry ] ->
        entry.I.position.Tokenizer.row = 1
        && entry.I.position.Tokenizer.column = 0
        && entry.I.disposition = A.Excluded "runtime-reflection"
    | [] | _ :: _ -> false)
    "Grouped assertion lost its declaration start or annotation";
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
