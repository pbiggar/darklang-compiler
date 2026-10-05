(* Whole executable LIR fixture and backend process observations. *)
open Dark_compiler
open LIRExecutionFormat
module J=Semantic_observation.SemanticJson
let tuple=J.tuple
let list f xs=`List (List.map f xs)
let result encode=function Ok value->J.union "FSharpResult" "Ok" [encode value]|Error error->J.union "FSharpResult" "Error" [J.string error]
let unitResult=result (fun ()->`Null)
let leakCheck=function LeakCheckDisabled->J.union "LeakCheckMode" "LeakCheckDisabled" []|LeakCheckEnabled->J.union "LeakCheckMode" "LeakCheckEnabled" []
let processExpectation=function ExpectedExitCode value->J.union "ProcessExpectation" "ExpectedExitCode" [J.int32 value]|ExpectedStdout value->J.union "ProcessExpectation" "ExpectedStdout" [J.string value]|ExpectedStderr value->J.union "ProcessExpectation" "ExpectedStderr" [J.string value]
let expectation=function ExpectedProcessResult values->J.union "LIRExecutionExpectation" "ExpectedProcessResult" [list processExpectation values]|ExpectedCodegenError value->J.union "LIRExecutionExpectation" "ExpectedCodegenError" [J.string value]
let test (value:lIRExecutionTest)=J.record "LIRExecutionTest" ["Name",J.string value.name;"Program",Semantic_observation.ProductionLIR.program value.program;"LeakCheck",leakCheck value.leakCheck;"Expectation",expectation value.expectation;"SourceFile",J.string value.sourceFile]
let tests values=list (fun (name,run)->let actual=run () in tuple [J.string name;unitResult actual]) values
let process (code,stdout,stderr)=tuple [J.int32 code;J.string stdout;J.string stderr]
let runs value=list (fun value->tuple [test value;unitResult (LIRExecutionTestRunner.runLIRExecutionTest value)]) [value;{value with expectation=ExpectedProcessResult [ExpectedExitCode 999;ExpectedStdout "mismatch";ExpectedStderr "mismatch"]};{value with expectation=ExpectedCodegenError "mismatch"}]
let parsed path content=result (list (fun value->tuple [test value;runs value])) (parseLIRExecutionFileContent path content)
let observe source=
 let fixtures=Yojson.Basic.from_file "scripts/ocaml/lir_execution_fixtures.json" |> Yojson.Basic.Util.to_list |> List.map Yojson.Basic.Util.to_string in
 let file="src/Tests/backend/x64/basic.lirexec" in let paths=["missing.lirexec";"src/Tests/backend/x64";file] in
 let cross=result (list (fun value->tuple [test value;result process (LIRExecutionTestRunner.executeProgram (Platform.ARM64Backend Platform.LinuxARM64) value.program value.leakCheck)])) (LIRExecutionTestRunner.loadLIRExecutionTests file) in
 tuple [parsed source source;list (parsed source) fixtures;parsed file (HostFile.readText file);list (fun path->result (list test) (LIRExecutionTestRunner.loadLIRExecutionTests path)) paths;tests (LIRExecutionTestRunner.tests (Array.of_list (List.rev paths)));tests LIRExecutionDSLTests.tests;cross]
[@@@warning "-42"]
