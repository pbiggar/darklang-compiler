[@@@warning "-42"]
(* Compare runner state transitions, timing tables and captured presentation. *)
open Dark_compiler
module F=TestFramework
module J=Semantic_observation.SemanticJson
let tuple=J.tuple
let str=J.string
let int n=`Int n
let ticks n=`String (Int64.to_string n)
let list f xs=`List (List.map f xs)
let time=HostTimeSpan.fromMilliseconds
let details (x:F.failedTestInfo)=tuple [str x.F.file;str x.F.name;str x.F.message;list str x.F.details]
let timing (x:F.testTiming)=tuple [str x.F.name;ticks x.F.totalTime;J.option ticks x.F.compileTime;J.option ticks x.F.runtimeTime]
let entries (x:F.passTimingSection)=tuple [str x.F.title;list (fun (entry:F.passTimingEntry)->tuple [J.option str entry.F.number;str entry.F.name;ticks entry.F.elapsed]) x.F.entries]
let mapValues f values=list (fun (key,value)->tuple [str key;f value]) (StringOrder.Map.bindings values)
let snapshot (state:F.testRunState) completed=tuple [int state.F.passed;int state.F.failed;list details (List.of_seq (Queue.to_seq state.F.failedTests));list timing (List.of_seq (Queue.to_seq state.F.timings));mapValues ticks state.F.passTimings;mapValues int state.F.passTimingCounts;list str (List.of_seq (Queue.to_seq state.F.passTimingOrder));list int completed]
let normalize output=
 let completed=Str.regexp "Completed in -?[0-9]+\\(\\.[0-9]+\\)?[ms]+" in
 let failed=Str.regexp "([0-9]+\\(\\.[0-9]+\\)?[ms]+)" in
 output |> Str.global_replace completed "Completed in <duration>" |> Str.global_replace failed "(<duration>)"
let observe source=
 let fixtures=Yojson.Basic.from_file "scripts/ocaml/framework_fixtures.json" in
 let open Yojson.Basic.Util in
 let names=fixtures |> member "names" |> to_list |> List.map to_string in
 let formatted=list (fun value->let value=Int64.of_string (to_string value) in tuple [ticks value;str (F.formatTime value)]) (fixtures |> member "ticks" |> to_list) in
 let stateRows=list (fun reporter->
 let completed=ref [] in let state=F.createStateWithProgressReporter (if reporter then Some (fun value->completed:=value:: !completed) else None) in
 let fail name={F.file="fixture";name;message="message";details=[source;"line"]} in
 F.recordResults state 2 1 [fail "first"];F.recordResults state 0 0 [];
 F.recordTiming state {F.name=source;totalTime=time 175.;compileTime=Some (time 100.);runtimeTime=Some (time 25.)};
 List.iteri (fun index name->F.recordPassTiming state {CompilerOptions.pass=name;elapsed=time (float (index mod 7)*.25.)}) (names@[source;"Parse";source]);
 F.recordResults state 1 2 [fail "second";fail "third"];
 let before=snapshot state (List.rev !completed) in
 let raised=try F.recordResults state (-1) 0 [];false with Failure _->true in
 tuple [`Bool reporter;before;`Bool raised;snapshot state (List.rev !completed)]) [false;true] in
 let columns=list (fun variant->
 let values=List.mapi (fun index name->name,time (if variant=0 then 0. else if variant=1 then 49.999 else if variant=2 then 50. else float (index mod 4)*.75.)) names |> List.fold_left (fun map (key,value)->StringOrder.Map.add key value map) StringOrder.Map.empty in
 let filtered=F.filterPassTimingsForOverhead values in
 let timings=List.to_seq [{F.name="first";totalTime=time 500.;compileTime=None;runtimeTime=Some (time 150.)};{F.name="second";totalTime=time 500.;compileTime=Some (time 300.);runtimeTime=None};{F.name="third";totalTime=time 300.;compileTime=None;runtimeTime=Some (time 75.)}] in
 let breakdown=F.calculateUnaccountedTimeBreakdown (time 12500.) values timings in
 tuple [mapValues ticks values;mapValues ticks filtered;ticks (F.calculatePassTimingsTotal values);ticks (F.calculatePassTimingsTotalForOverhead values);tuple [ticks breakdown.F.unaccounted;ticks breakdown.F.runtime;ticks breakdown.F.overhead];list (fun unknown->let columns=F.buildPassTimingColumns values (List.rev names) (time unknown) in tuple [list entries columns.F.ordered;list entries columns.F.byTime]) [-1.;0.;49.999;50.;1000.]]) [0;1;2;3] in
 let progress=list (fun total->list (fun count->list (fun failed->
 let state=ProgressBar.create source total in state.ProgressBar.completed<-count;state.ProgressBar.failed<-failed;
 let (),output,error=TestCapture.run (fun ()->ProgressBar.update state;ProgressBar.increment state false;ProgressBar.increment state true;ProgressBar.finish state) in
 tuple [int total;int count;int failed;str output;str error;int state.ProgressBar.completed;int state.ProgressBar.failed]) [-1;0;2]) [-2;0;1;4;21]) [-2;0;1;3;20] in
 let expected=list (fun expected->list (fun actual->let result,output,error=TestCapture.run (fun ()->F.addExpectedActualDetails expected actual) in tuple [list str result;str output;str error]) [None;Some "";Some "actual\n\nlast\n"]) [None;Some "";Some source;Some "expected\n\nlast\n"] in
 let symbols={F.pass="PASS";fail="FAIL";sectionPrefix="→"} in
 let unitSuites=list (fun enabled->
 let completed=ref [] in let state=F.createStateWithProgressReporter (Some (fun count->completed:=count:: !completed)) in
 let suites=if enabled then [|{F.name=source;tests=["works",(fun ()->Ok ());"fails",(fun ()->Error "failure\nsecond")]} ;{F.name="empty";tests=[]};{F.name="final";tests=["works",(fun ()->Ok ())]}|] else [||] in
 let (),output,error=TestCapture.run (fun ()->F.runUnitTestSuites state symbols "Unit tests" source suites) in
 let names=List.of_seq (Queue.to_seq state.F.timings) |> List.map (fun (timing:F.testTiming)->str timing.F.name) in
 tuple [int state.F.passed;int state.F.failed;list details (List.of_seq (Queue.to_seq state.F.failedTests));`List names;list int (List.rev !completed);str (normalize output);str error]) [false;true] in
 let fileSuites=list (fun enabled->
 let completed=ref [] in let state=F.createStateWithProgressReporter (Some (fun count->completed:=count:: !completed)) in
 let files=if enabled then [|"good";"bad";"good2"|] else [||] in
 let handle success progress path name _elapsed value=ProgressBar.increment progress success;{F.passed=(if success then 2 else 0);failed=(if success then 0 else 1);failedTests=(if success then [] else [{F.file=path;name;message=value;details=[source]}])} in
 let (),output,error=TestCapture.run (fun ()->F.runFileSuite state symbols "Files" source files (fun name->"Test "^name) (fun name->"File "^name) (fun path->if path="bad" then Error "bad input" else Ok "works") (handle true) (handle false)) in
 tuple [int state.F.passed;int state.F.failed;list details (List.of_seq (Queue.to_seq state.F.failedTests));list (fun (timing:F.testTiming)->str timing.F.name) (List.of_seq (Queue.to_seq state.F.timings));list int (List.rev !completed);str (normalize output);str error]) [false;true] in
 tuple [formatted;stateRows;columns;progress;expected;unitSuites;fileSuites;list str [TestRunnerColors.reset;TestRunnerColors.green;TestRunnerColors.red;TestRunnerColors.yellow;TestRunnerColors.white;TestRunnerColors.cyan;TestRunnerColors.gray;TestRunnerColors.bold]]
