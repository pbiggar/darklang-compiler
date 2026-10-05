(* Full parallel move parser, lowering and diagnostics parity. *)
open ParallelMoveFormat
module J=Semantic_observation.SemanticJson
module ISA=Semantic_observation.MachineISAObservation
let tuple=J.tuple
let list f xs=`List (List.map f xs)
let result encode=function Ok value->J.union "FSharpResult" "Ok" [encode value]|Error error->J.union "FSharpResult" "Error" [J.string error]
let test (value:parallelMoveTest)=J.record "ParallelMoveTest" ["Name",J.string value.name;"Moves",list (fun (reg,operand)->tuple [Semantic_observation.ProductionLIR.physReg reg;Semantic_observation.ProductionLIR.operand operand]) value.moves;"Expected",list ISA.symInstr value.expected;"SourceFile",J.string value.sourceFile]
let outcome (value:TestOutcome.t)=J.record "PassTestResult" ["Success",`Bool value.TestOutcome.success;"Message",J.string value.TestOutcome.message;"Expected",J.option J.string value.TestOutcome.expected;"Actual",J.option J.string value.TestOutcome.actual]
let tests values=list (fun (name,run)->let actual=run () in tuple [J.string name;result (fun ()->`Null) actual]) values
let runs value=list (fun value->tuple [test value;outcome (ParallelMoveTestRunner.runParallelMoveTest value)]) [value;{value with expected=[Dark_compiler.Symbolic.RET]};{value with expected=Dark_compiler.Symbolic.Label "included"::value.expected}]
let parsed path content=result (list (fun value->tuple [test value;runs value])) (parseParallelMoveFileContent path content)
let observe source=
 let fixtures=Yojson.Basic.from_file "scripts/ocaml/parallel_move_fixtures.json" |> Yojson.Basic.Util.to_list |> List.map Yojson.Basic.Util.to_string in
 let file="src/Tests/algorithms/parallel-moves/arm64.parallelmoves" in let paths=["missing.parallelmoves";"src/Tests/algorithms/parallel-moves";file] in
 tuple [parsed source source;list (parsed source) fixtures;parsed file (Dark_compiler.HostFile.readText file);list (fun path->result (list test) (ParallelMoveTestRunner.loadParallelMoveTests path)) paths;tests (ParallelMoveTestRunner.tests (Array.of_list (List.rev paths)));tests ParallelMoveDSLTests.tests]
[@@@warning "-42"]
