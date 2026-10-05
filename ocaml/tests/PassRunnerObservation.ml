(* Full typed compiler-pass execution and diagnostics observations. *)
open Dark_compiler
module J=Semantic_observation.SemanticJson
module ISA=Semantic_observation.MachineISAObservation
let tuple=J.tuple
let list f xs=`List (List.map f xs)
let result encode=function Ok value->J.union "FSharpResult" "Ok" [encode value]|Error error->J.union "FSharpResult" "Error" [J.string error]
let outcome (value:TestOutcome.t)=J.record "PassTestResult" ["Success",`Bool value.TestOutcome.success;"Message",J.string value.TestOutcome.message;"Expected",J.option J.string value.TestOutcome.expected;"Actual",J.option J.string value.TestOutcome.actual]
let pair a b (x,y)=tuple [a x;b y]
let observe source=
 let open Yojson.Basic.Util in
 let paths=Yojson.Basic.from_file "scripts/ocaml/pass_runner_fixtures.json" |> to_list |> List.map to_string in
 let anf=Semantic_observation.ProductionANF.aNF_program in
 let mir=Semantic_observation.ProductionMIR.program in
 let lir=Semantic_observation.ProductionLIR.program in
 let row path=tuple [J.string path;result (pair anf mir) (PassTestRunner.loadANF2MIRTest path);result (pair mir lir) (PassTestRunner.loadMIR2LIRTest path);result (pair lir (list ISA.symInstr)) (PassTestRunner.loadLIR2ARM64Test path)] in
 let runs path=
  if Filename.check_suffix path ".anf2mir" then result (fun (input,expected)->tuple [outcome (PassTestRunner.runANF2MIRTest input expected);outcome (PassTestRunner.runANF2MIRTest input (MIR.Program ([],StringOrder.Map.empty,StringOrder.Map.empty)))]) (PassTestRunner.loadANF2MIRTest path)
  else if Filename.check_suffix path ".mir2lir" then result (fun (input,expected)->tuple [outcome (PassTestRunner.runMIR2LIRTest input expected);outcome (PassTestRunner.runMIR2LIRTest input (PassTestRunner.renameLIRFunctions "mismatch" expected))]) (PassTestRunner.loadMIR2LIRTest path)
  else result (fun (input,expected)->tuple [outcome (PassTestRunner.runLIR2ARM64Test input expected);outcome (PassTestRunner.runLIR2ARM64Test input []);outcome (PassTestRunner.runLIR2ARM64Test input (Symbolic.Label source::expected))]) (PassTestRunner.loadLIR2ARM64Test path) in
 let format instrs=tuple [list (fun instr->tuple [ISA.symInstr instr;J.string (PassTestRunner.prettyPrintARM64Instr instr)]) instrs;J.string (PassTestRunner.prettyPrintARM64 instrs)] in
 let machine=list (fun role->list (fun boundary->format (Symbolic.ofARM64List (ISA.armInstructions source role boundary))) [-32768;-1;0;1;255;4095;65535;Int32.to_int Int32.min_int;Int32.to_int Int32.max_int]) (List.init 32 Fun.id) in
 let refs=[Symbolic.CodeLabel source;Symbolic.DataLabel (Symbolic.Named source);Symbolic.DataLabel (Symbolic.StringLiteral (source^"\\\"\n"))]@List.map (fun value->Symbolic.DataLabel (Symbolic.FloatLiteral value)) [0.;-0.;infinity;neg_infinity;Int64.float_of_bits 0xfff8000000000000L;Int64.float_of_bits 1L;Float.max_float;1e-5;1e16] in
 let extras=format (List.concat_map (fun label->[Symbolic.ADRP (ARM64.X0,label);Symbolic.ADR (ARM64.X1,label);Symbolic.ADD_label (ARM64.X2,ARM64.X3,label)]) refs) in
 let tests=list (fun (name,run)->let actual=run () in tuple [J.string name;result (fun ()->`Null) actual]) PassTestRunnerTests.tests in
 tuple [list row (paths@["missing.pass";"src/Tests/passes";"scripts/ocaml/pass_runner_fixtures.json"]);list runs paths;machine;extras;tests;result (fun ()->`Null) (PassTestRunnerTests.runAll ())]
