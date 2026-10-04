(* Compare whole typed snapshot inputs and full success/failure formatter descriptions. *)
open IRFormatSnapshotFormat
module J=Semantic_observation.SemanticJson
let tuple=J.tuple
let result encode=function Ok value->J.union "FSharpResult" "Ok" [encode value]|Error error->J.union "FSharpResult" "Error" [J.string error]
let input=function ANFInput value->J.union "IRFormatInput" "ANFInput" [Semantic_observation.ProductionANF.aNF_program value]|MIRInput value->J.union "IRFormatInput" "MIRInput" [Semantic_observation.ProductionMIR.program value]|LIRInput value->J.union "IRFormatInput" "LIRInput" [Semantic_observation.ProductionLIR.program value]
let test (value:irFormatSnapshotTest)=J.record "IRFormatSnapshotTest" ["Name",J.string value.name;"Input",input value.input;"Expected",J.string value.expected;"SourceFile",J.string value.sourceFile]
let outcome (value:TestOutcome.t)=J.record "PassTestResult" ["Success",`Bool value.TestOutcome.success;"Message",J.string value.TestOutcome.message;"Expected",J.option J.string value.TestOutcome.expected;"Actual",J.option J.string value.TestOutcome.actual]
let tests values=`List (List.map (fun (name,run)->let actual=run () in tuple [J.string name;result (fun ()->`Null) actual]) values)
let runs value=`List (List.map (fun value->tuple [test value;outcome (IRFormatSnapshotTestRunner.runIRFormatSnapshotTest value)]) [value;{value with expected="mismatch"};{value with expected=value.expected^"\n"}])
let parsed path content=result (fun values->`List (List.map (fun value->tuple [test value;runs value]) values)) (parseIRFormatSnapshotFileContent path content)
let observe source=
 let open Yojson.Basic.Util in
 let fixtures=Yojson.Basic.from_file "scripts/ocaml/ir_snapshot_fixtures.json" |> to_list in
 let file="src/Tests/formatting/ir/core.irformat" in
 let paths=["missing.irformat";"src/Tests/formatting/ir";file] in
 tuple [parsed source source;`List (List.map (fun value->parsed source (to_string value)) fixtures);parsed file (Dark_compiler.HostFile.readText file);`List (List.map (fun path->result (fun values->`List (List.map test values)) (IRFormatSnapshotTestRunner.loadIRFormatSnapshotTests path)) paths);tests (IRFormatSnapshotTestRunner.tests (Array.of_list (List.rev paths)));tests IRFormatSnapshotDSLTests.tests]
