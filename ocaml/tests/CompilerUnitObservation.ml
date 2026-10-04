(* Compare original compiler unit registration and complete result diagnostics. *)
open Dark_compiler
module J=Semantic_observation.SemanticJson
let tuple=J.tuple
let result=function Ok ()->J.union "FSharpResult" "Ok" [`Null]|Error error->J.union "FSharpResult" "Error" [J.string error]
let tests values=`List (List.map (fun (name,run)->let outcome=run () in tuple [J.string name;result outcome]) values)
let stdlib=lazy (Result.bind (Platform.detectHostTarget ()) StdlibCompilation.buildStdlib)
let observe source=
 let prepared=match Lazy.force stdlib with Error error->J.union "FSharpResult" "Error" [J.string error]|Ok stdlib->J.union "FSharpResult" "Ok" [tuple [tests (JsonPlanningTests.tests stdlib);tests (StdlibOptimizationTests.tests stdlib)]] in
 tuple [J.string source;prepared;tests RuntimeDataLayoutTests.tests;tests X86_64ResolveTests.tests;tests LambdaLiftingTests.tests;tests MonomorphizationTests.tests;tests IRPrinterTests.tests;result (IRPrinterTests.runAll ())]
