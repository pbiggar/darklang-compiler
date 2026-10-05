(* Complete equality preparation, generated batch source and expectation results. *)
[@@@warning "-4-42"]
open Dark_compiler
module J=Semantic_observation.SemanticJson
module R=E2ETestRunner
let list f xs=`List (List.map f xs)
let result f=function Ok v->J.union "FSharpResult" "Ok" [f v]|Error e->J.union "FSharpResult" "Error" [J.string e]
let guarded f=try f () with Invalid_argument _|Failure _->`Assoc ["internalException",`Bool true]
let outcome test run=R.evaluateExpectations test run |> Result.map (fun _->()) |> Result.map_error (fun e->e.R.message) |> result (fun ()->`Null)
let observe source=
 let open Yojson.Basic.Util in
 let data=Yojson.Basic.from_file "scripts/ocaml/e2e_format_fixtures.json" in
 let files=data |> member "files" |> to_list |> List.map to_string in
 let fixtures=data |> member "fixtures" |> to_list |> List.map (fun f->f |> member "path" |> to_string) in
 let bucket=int_of_string source in let selected xs=List.filteri (fun i _->i mod 16=bucket) xs in
 let zero=HostTimeSpan.zero in
 let runs=[R.CompileFailed (1,"compile failure",zero);R.CompileFailed (0,"",zero)] @
   List.map (fun (code,stdout,stderr)->R.Ran (code,stdout,stderr,zero,zero)) [
    0,"true\n","";0,"false\n","";0,"true\r\n","";0,"true\n\n","";1,"","Uncaught exception: a";139,"","";
    0,"Result.Error(\"a\")\n","";0," true \n","";0,"true\n","leaks: 0\n";0,"true\n","leaks: 5\r\n";
    0,"true\n","note\nleaks: 3\n";0,"true\n","leaks: ٥\n";0,"é😀\r\n","err\r\n"] in
 let prepared p=J.option J.string (Option.map (fun p->p.R.equalitySource) p) in
 let tests xs=list (fun test->J.tuple [prepared (R.tryPrepareBatchTest test);list (outcome test) runs]) xs in
 let file path=J.tuple [J.string path;guarded (fun ()->result tests (E2EFormat.parseE2ETestFile path));guarded (fun ()->
   match E2EFormat.parseE2ETestFile path with Error e->result J.string (Error e)|Ok tests->
     let xs=List.filter_map R.tryPrepareBatchTest tests in result J.string (Ok (R.buildBatchSource (List.take (min 65 (List.length xs)) xs))))] in
 let counts=[0;1;3;31;32;33;63;64;65;8192;8193] in
 let outputs=["";"0\n";"5\n";"8\n";"-1\n";"4294967295\n";"4294967296\n";"+5\n";" 5 \r\n";"5\000\n";"(2147483649, 2, 1)\n";"(0,0,0)\n";"(0,0,2)\n";"ignored\n5\n";"5\n \n";"(0,0)\n";"５\n";"0x5\n"] in
 J.tuple [list file (selected (files@fixtures));list (fun count->J.tuple [J.int32 count;list (fun text->J.tuple [J.string text;J.option (list (fun b->`Bool b)) (R.tryParseBatchBoolResults count text)]) outputs]) (selected counts)]
