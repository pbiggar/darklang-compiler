(* Complete optimization fixture records and ordered section diagnostics. *)
[@@@warning "-42"]
open OptimizationFormat
module J=Semantic_observation.SemanticJson
let tuple=J.tuple
let list f xs=`List (List.map f xs)
let result encode=function Ok value->J.union "FSharpResult" "Ok" [encode value]|Error error->J.union "FSharpResult" "Error" [J.string error]
let stage value=J.union "IRStage" (match value with ANF->"ANF"|MIR->"MIR"|LIR->"LIR"|DirectLIR->"DirectLIR"|DirectARM64->"DirectARM64"|DirectLIR2X64->"DirectLIR2X64") []
let input=function Source value->J.union "OptimizationInput" "Source" [J.string value]|StdlibFunction value->J.union "OptimizationInput" "StdlibFunction" [J.string value]
let test (value:optimizationTest)=J.record "OptimizationTest" ["Name",J.string value.name;"Input",input value.input;"ExpectedIR",J.string value.expectedIR;"Stage",stage value.stage;"SourceFile",J.string value.sourceFile]
let tests values=list (fun (name,run)->let actual=run () in tuple [J.string name;result (fun ()->`Null) actual]) values
let observe source=
 let open Yojson.Basic.Util in
 let fixtures=Yojson.Basic.from_file "scripts/ocaml/optimization_format_fixtures.json" |> to_list |> List.map (fun value->value |> member "path" |> to_string) in
 let corpus=Sys.readdir "src/Tests/optimization" |> Array.to_list |> List.sort Dark_compiler.StringOrder.compare |> List.map (Filename.concat "src/Tests/optimization") in
 let row path=tuple [J.string path;list (fun value->result (list test) (parseTestFile value path)) [ANF;MIR;LIR;DirectLIR;DirectARM64;DirectLIR2X64]] in
 tuple [J.string source;list row (fixtures@corpus@["missing.opt";"src/Tests/optimization"]);tests OptimizationFormatTests.tests;result (fun ()->`Null) (OptimizationFormatTests.runAll ())]
