(* Full optimization runner outcomes, diagnostics, phase ordering and IR text. *)
[@@@warning "-4-42"]
open Dark_compiler
open OptimizationFormat
module J=Semantic_observation.SemanticJson
let tuple=J.tuple
let list f xs=`List (List.map f xs)
let result encode=function Ok value->J.union "FSharpResult" "Ok" [encode value]|Error error->J.union "FSharpResult" "Error" [J.string error]
let outcome (value:TestOutcome.t)=J.record "OptimizationTestResult" ["Success",`Bool value.TestOutcome.success;"Message",J.string value.TestOutcome.message;"Expected",J.option J.string value.TestOutcome.expected;"Actual",J.option J.string value.TestOutcome.actual]
let stdlib=lazy (StdlibCompilation.buildStdlib Platform.LinuxX86_64)
let preparedCache=ref None
let observe source=
 let open Yojson.Basic.Util in
 let normalization=Yojson.Basic.from_file "scripts/ocaml/optimization_runner_fixtures.json" |> member "normalization" |> to_list |> List.map (fun value->to_list value |> List.map to_int |> Array.of_list |> HostText.ofScalars) in
 let normalized=list (fun value->tuple [J.string value;J.string (OptimizationTestRunner.normalizeIR value)]) normalization in
 let prepare ()=result (fun stdlib->
  let sources=["1L + 2L";"let f (x: Int64) : Int64 = x + 1L";"\"t42 __closure_99\"";"let x = (1L, \"two\")\nx";"if true then 1L else 2L";"missing_name";"let =";""] in
  let compile source=list (fun stage->let phases=ref [] in let record (value:CompilerOptions.passTiming)=phases:=value.CompilerOptions.pass:: !phases in
   let output=(match stage with ANF->OptimizationTestRunner.getOptimizedANF|MIR->OptimizationTestRunner.getOptimizedMIR|LIR->OptimizationTestRunner.getOptimizedLIR|DirectLIR|DirectARM64|DirectLIR2X64->assert false) stdlib (Some record) source in
   tuple [result J.string output;list J.string (List.rev !phases)]) [ANF;MIR;LIR] in
  let run stage input expected=OptimizationTestRunner.runOptimizationTest stdlib None {name="boundary";input;expectedIR=expected;stage;sourceFile="boundary.opt"} |> outcome in
  let stages=[ANF;MIR;LIR;DirectLIR;DirectARM64;DirectLIR2X64] in
  let boundaries=list (fun stage->list (fun (input,expected)->run stage input expected) [Source "", "";Source "bad", "bad";Source "Ret", "bad";Source "bad", "Ret";Source "Ret", "Ret";Source "Ret", "MOV_reg(RAX, RDI)";Source "MOV_reg(X0, X1)", "MOV_reg(X0, X1)";StdlibFunction "missing_function", ""]) stages in
  let corpus=[ANF,"anf.opt";MIR,"mir.opt";LIR,"lir.opt";DirectLIR,"lir-peepholes.liropt";DirectARM64,"arm64-peepholes.arm64opt";DirectLIR2X64,"x64-selection.lir2x64"] in
  let rows=list (fun (stage,file)->let path="src/Tests/optimization/"^file in
   let run filter=result (list (fun (test,output)->tuple [OptimizationFormatObservation.test test;outcome output])) (OptimizationTestRunner.runTestFile stdlib None filter stage path) in
   let forced=result (list (fun test->let wrong={test with expectedIR="mismatch"} in tuple [OptimizationFormatObservation.test wrong;outcome (OptimizationTestRunner.runOptimizationTest stdlib None wrong)])) (parseTestFile stage path) in
   tuple [J.string path;run (fun _->true);run (fun _->false);run (fun test->String.length test.name mod 2=0);forced]) corpus in
  tuple [list (fun source->tuple [J.string source;compile source]) sources;boundaries;rows;list (fun stage->result (list (fun (test,output)->tuple [OptimizationFormatObservation.test test;outcome output])) (OptimizationTestRunner.runTestFile stdlib None (fun _->true) stage "missing.opt")) stages;list (fun name->result J.string (OptimizationTestRunner.getOptimizedStdlibANF stdlib name)) ["missing_function";"Darklang.Stdlib.String.__asciiWhitespace";"Darklang.Stdlib.Int64.__powerLoop"]]) (Lazy.force stdlib) in
 let prepared=match !preparedCache with Some value->value|None->let value=prepare () in preparedCache:=Some value;value in
 tuple [J.string (OptimizationTestRunner.normalizeIR source);normalized;prepared]
