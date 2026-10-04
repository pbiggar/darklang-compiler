[@@@warning "-42"]
(* TestFramework.fs - Generic test runner helpers.
   Provides shared types and utilities that are independent of any specific test suite. *)
open Dark_compiler
module Colors=TestRunnerColors
module M=StringOrder.Map
type unitTestCase=string * (unit -> (unit,string) result)
type unitTestSuite={name:string;tests:unitTestCase list}
(* Failed test info for summary at end *)
type failedTestInfo={file:string;name:string;message:string;details:string list} (* Additional details like expected/actual *)
(* Test timing info for slowest tests report *)
type testTiming={name:string;totalTime:HostTimeSpan.t;compileTime:HostTimeSpan.t option;runtimeTime:HostTimeSpan.t option}
type passTimingEntry={number:string option;name:string;elapsed:HostTimeSpan.t}
type passTimingSection={title:string;entries:passTimingEntry list}
type passTimingColumns={ordered:passTimingSection list;byTime:passTimingSection list}
type unaccountedTimeBreakdown={unaccounted:HostTimeSpan.t;runtime:HostTimeSpan.t;overhead:HostTimeSpan.t}
(* Summary of per-file test suite results *)
type fileSuiteSummary={passed:int;failed:int;failedTests:failedTestInfo list}
type testRunState={mutable passed:int;mutable failed:int;failedTests:failedTestInfo Queue.t;timings:testTiming Queue.t;mutable passTimings:HostTimeSpan.t M.t;mutable passTimingCounts:int M.t;passTimingOrder:string Queue.t;completedTestReporter:(int -> unit) option}
type outputSymbols={pass:string;fail:string;sectionPrefix:string}
let testRuntimeTimingName="Test Runtime"
let createStateWithProgressReporter completedTestReporter={passed=0;failed=0;failedTests=Queue.create ();timings=Queue.create ();passTimings=M.empty;passTimingCounts=M.empty;passTimingOrder=Queue.create ();completedTestReporter}
let createState ()=createStateWithProgressReporter None
let recordTiming state timing=Queue.add timing state.timings
let recordPassTiming state (timing:CompilerOptions.passTiming)=
 if not (M.mem timing.CompilerOptions.pass state.passTimings) then Queue.add timing.CompilerOptions.pass state.passTimingOrder;
 let existing=Option.value ~default:HostTimeSpan.zero (M.find_opt timing.CompilerOptions.pass state.passTimings) in
 state.passTimings<-M.add timing.CompilerOptions.pass (Int64.add existing timing.CompilerOptions.elapsed) state.passTimings;
 let count=Option.value ~default:0 (M.find_opt timing.CompilerOptions.pass state.passTimingCounts) in
 state.passTimingCounts<-M.add timing.CompilerOptions.pass (count+1) state.passTimingCounts
let recordResults (state:testRunState) passedDelta failedDelta failedTestsDelta=
 let previousCompleted=state.passed+state.failed in
 state.passed<-state.passed+passedDelta;state.failed<-state.failed+failedDelta;
 List.iter (fun test->Queue.add test state.failedTests) failedTestsDelta;
 match state.completedTestReporter with None->()|Some reportCompletedTest->
 let completedDelta=passedDelta+failedDelta in
 if completedDelta<0 then Crash.crash (Printf.sprintf "recordResults: completed test delta cannot be negative (%d)" completedDelta)
 else for completed=previousCompleted+1 to previousCompleted+completedDelta do reportCompletedTest completed done
(* Format elapsed time *)
let formatDecimal places value=
 let text=Printf.sprintf "%.15g" (Float.abs value) in
 let mantissa,exponent=match String.index_opt text 'e' with None->text,0|Some at->String.sub text 0 at,int_of_string (String.sub text (at+1) (String.length text-at-1)) in
 let fraction=match String.index_opt mantissa '.' with None->0|Some at->String.length mantissa-at-1 in
 let digits=String.concat "" (String.split_on_char '.' mantissa) |> Z.of_string in
 let shift=exponent-fraction+places in
 let rounded=if shift>=0 then Z.mul digits (Z.pow (Z.of_int 10) shift) else
 let divisor=Z.pow (Z.of_int 10) (-shift) in let quotient,remainder=Z.div_rem digits divisor in
 if Z.compare (Z.mul remainder (Z.of_int 2)) divisor>=0 then Z.succ quotient else quotient in
 let digits=Z.to_string rounded in
 let digits=String.make (max 0 (places+1-String.length digits)) '0'^digits in
 let formatted=if places=0 then digits else let split=String.length digits-places in String.sub digits 0 split^"."^String.sub digits split places in
 (if value<0. then "-" else "")^formatted
let formatTime elapsed=if HostTimeSpan.totalMilliseconds elapsed<1000. then formatDecimal 0 (HostTimeSpan.totalMilliseconds elapsed)^"ms" else formatDecimal 2 (HostTimeSpan.totalSeconds elapsed)^"s"
let calculatePassTimingsTotal passTimings=M.fold (fun _ elapsed acc->Int64.add acc elapsed) passTimings HostTimeSpan.zero
let filterPassTimingsForOverhead passTimings=
 let overlapTimingNames=["Unit Test Suite Execution";"Start Function Compilation";"E2E Suite Execution";"Verification Suite Execution";"JSON Planning";"ARM64 Codegen Metadata";"ARM64 Codegen Functions";"ARM64 Codegen Helpers";"ARM64 Codegen Assembly";"ARM64 Codegen Peephole"] in
 let starts=String.starts_with in
 M.filter (fun name _->not (List.mem name overlapTimingNames)
 && not (List.exists (fun prefix->starts ~prefix name) ["Ownership detail: ";"AST -> ANF detail: ";"AST -> ANF function: ";"Stdlib detail: ";"TypeCheck: ";"AST -> ANF Preparation: ";"AST -> ANF ";"ANF -> MIR ";"SSA: ";"MIR -> LIR ";"RegAlloc: "])
 && not (List.mem name ["Value Rendering";"ANF Higher-Order Specialization";"ANF Direct-Call Specialization"])
 && not (starts ~prefix:"Reference Count " name && name<>"Reference Count Insertion")
 && not (starts ~prefix:"MIR " name && name<>"MIR Optimizations" && name<>"MIR -> LIR")
 && not (starts ~prefix:"ARM64 " name && name<>"ARM64 Function Metadata Planning" && name<>"ARM64 Emit")) passTimings
let calculatePassTimingsTotalForOverhead timings=calculatePassTimingsTotal (filterPassTimingsForOverhead timings)
let calculateUnaccountedTimeBreakdown totalTime passTimings timings=
 let unaccounted=Int64.sub totalTime (calculatePassTimingsTotalForOverhead passTimings) in
 let runtimeTotal=Seq.fold_left (fun acc (timing:testTiming)->Int64.add acc (Option.value ~default:HostTimeSpan.zero timing.runtimeTime)) HostTimeSpan.zero timings in
 let runtimeAccounted=Option.value ~default:HostTimeSpan.zero (M.find_opt testRuntimeTimingName passTimings) in
 let runtime=Int64.sub runtimeTotal runtimeAccounted in
 {unaccounted;runtime;overhead=Int64.sub unaccounted runtime}
let buildPassTimingColumns passTimings _passTimingOrder unaccountedTime=
 let hiddenTimingNames=["Unit Test Suite Execution";"Start Function Compilation";"JSON Planning";"ARM64 Codegen Metadata";"ARM64 Codegen Functions";"ARM64 Codegen Helpers";"ARM64 Codegen Assembly";"ARM64 Codegen Peephole"] in
 let consolidated=M.filter (fun name _->not (List.mem name hiddenTimingNames) && not (String.starts_with ~prefix:"Ownership detail: " name)) (filterPassTimingsForOverhead passTimings) in
 let passDefinitions=[("Parse","1","Parser");("Type Checking","1.5","Type Checking");("AST -> ANF","2","AST to ANF");("Print Insertion","2.2","Print Insertion");("ANF Accumulator Lowering","2.3","Accumulator Helper Lowering");("SSA Optimizations","2.4.1","SSA Optimizations");("SSA Inlining","2.4.5","SSA Inlining");("SSA Higher-Order Specialization","2.4.6","Known Closure Specialization");("SSA Direct-Call Specialization","2.5","Direct-Call Specialization");("SSA Escape Analysis","2.6","Escape Analysis");("Reference Count Insertion","2.7","Ref Count Insertion");("Tail Call Detection","2.8","Tail Call Optimization");("ANF -> MIR","3.1","SSA to MIR");("MIR Optimizations","3.5","MIR Optimizations");("MIR -> LIR","4","MIR to LIR");("LIR Peephole","4.5","LIR Peephole");("Register Allocation","5","Register Allocation");("Function Tree Shaking","5.5","Function Tree Shaking");("Code Generation","6","Code Generation");("ARM64 Emit","7","ARM64 Emit")] in
 let orderedDefinitions=List.map (fun (key,number,name)->key,Some number,name) passDefinitions@[testRuntimeTimingName,None,testRuntimeTimingName] in
 let overheadDefinitions=[("Pass Test Suite Execution","Pass Test Suite Execution");("ANF to MIR Test Suite Execution","ANF to MIR Test Suite Execution");("MIR to LIR Test Suite Execution","MIR to LIR Test Suite Execution");("LIR to ARM64 Test Suite Execution","LIR to ARM64 Test Suite Execution");("ARM64 Encoding Test Suite Execution","ARM64 Encoding Test Suite Execution");("Type Checking Test Suite Execution","Type Checking Test Suite Execution");("Optimization Test Suite Execution","Optimization Test Suite Execution");("Stdlib Build Overhead","Stdlib Build Overhead");("E2E Test Parse","E2E Test Parse");("Suite Context Planning","Suite Context Planning");("Suite Context Stdlib Specialization Overhead","Suite Context Stdlib Specialization Overhead");("Suite Context Preamble Build Overhead","Suite Context Preamble Build Overhead");("E2E Suite Context Overhead","E2E Suite Context Overhead");("Verification Test Parse","Verification Test Parse");("Verification Suite Context Overhead","Verification Suite Context Overhead");("Compile Overhead","Compile Overhead")] in
 let shouldDisplay elapsed=elapsed>=HostTimeSpan.fromMilliseconds 50. in
 let orderedEntries=List.filter_map (fun (key,number,name)->match M.find_opt key consolidated with Some elapsed when shouldDisplay elapsed->Some {number;name;elapsed}|Some _|None->None) orderedDefinitions in
 let overheadEntries=List.filter_map (fun (key,name)->match M.find_opt key consolidated with Some elapsed when shouldDisplay elapsed->Some {number=None;name;elapsed}|Some _|None->None) overheadDefinitions in
 let knownTotal=calculatePassTimingsTotal consolidated in
 let displayedTotal=List.fold_left (fun acc (entry:passTimingEntry)->Int64.add acc entry.elapsed) HostTimeSpan.zero (orderedEntries@overheadEntries) in
 let otherKnown=Int64.sub knownTotal displayedTotal in
 if otherKnown<HostTimeSpan.zero then Crash.crash ("buildPassTimingColumns: known timings ("^Int64.to_string knownTotal^") smaller than displayed ("^Int64.to_string displayedTotal^")");
 let other name elapsed=if shouldDisplay elapsed then [{number=None;name;elapsed}] else [] in
 let overheadEntriesWithOther=overheadEntries@other "Other (known)" otherKnown@other "Other (unknown)" unaccountedTime in
 let overheadSection=if overheadEntriesWithOther=[] then [] else [{title="Overhead";entries=overheadEntriesWithOther}] in
 let combinedByTimeEntries=List.stable_sort (fun a b->Int64.compare b.elapsed a.elapsed) (orderedEntries@overheadEntriesWithOther) in
 {ordered={title="Passes";entries=orderedEntries}::overheadSection;byTime=[{title="By Time";entries=combinedByTimeEntries}]}
let addExpectedActualDetails expected actual=match expected,actual with
 |Some expected,Some actual->
 Output.println "    Expected:";
 let expectedDetails=String.split_on_char '\n' expected |> List.map (fun line->Output.println ("      "^line);"Expected: "^line) in
 Output.println "    Actual:";
 let actualDetails=String.split_on_char '\n' actual |> List.map (fun line->Output.println ("      "^line);"Actual: "^line) in
 expectedDetails@actualDetails
 |None,_|_,None->[]
let now ()=HostClock.milliseconds ()
let elapsed start=HostTimeSpan.fromMilliseconds (now ()-.start)
let printSummary symbols passed failed totalTime=
 if failed=0 then Output.println (Printf.sprintf "  %s%s %d passed%s" Colors.green symbols.pass passed Colors.reset)
 else Output.println (Printf.sprintf "  %s%s %d passed%s, %s%s %d failed%s" Colors.green symbols.pass passed Colors.reset Colors.red symbols.fail failed Colors.reset);
 Output.println (Printf.sprintf "  %s%s Completed in %s%s" Colors.gray symbols.sectionPrefix (formatTime totalTime) Colors.reset);
 Output.println ""
let runFileSuite state symbols suiteTitle progressLabel testFiles getTestName formatTimingName runFile handleSuccess handleError=
 if Array.length testFiles>0 then (
 let sectionTimer=now () in Output.println (Colors.cyan^suiteTitle^Colors.reset);
 let sectionPassed=ref 0 and sectionFailed=ref 0 in
 let progress=ProgressBar.create progressLabel (Array.length testFiles) in ProgressBar.update progress;
 Array.iter (fun testPath->
 let testName=getTestName testPath in let testTimer=now () in
 let result=runFile testPath in let totalTime=elapsed testTimer in
 let (summary:fileSuiteSummary)=match result with Ok result->handleSuccess progress testPath testName totalTime result|Error message->handleError progress testPath testName totalTime message in
 recordTiming state {name=formatTimingName testName;totalTime;compileTime=None;runtimeTime=None};
 sectionPassed:= !sectionPassed+summary.passed;sectionFailed:= !sectionFailed+summary.failed;
 recordResults state summary.passed summary.failed summary.failedTests) testFiles;
 ProgressBar.finish progress;let totalTime=elapsed sectionTimer in printSummary symbols !sectionPassed !sectionFailed totalTime)
let runUnitTestSuites state symbols suiteTitle progressLabel suites=
 let unitSectionTimer=now () in Output.println (Colors.cyan^suiteTitle^Colors.reset);
 let passed=ref 0 and failed=ref 0 in let failedTests=Queue.create () in
 let total=Array.fold_left (fun acc (suite:unitTestSuite)->acc+List.length suite.tests) 0 suites in
 let progress=ProgressBar.create progressLabel total in ProgressBar.update progress;
 Array.iter (fun (suite:unitTestSuite)->List.iter (fun (testName,runTest)->
 let timer=now () in let displayName=suite.name^": "^testName^": " in
 let result=runTest () in let totalTime=elapsed timer in
 match result with
 |Ok ()->recordTiming state {name="Unit: "^displayName;totalTime;compileTime=None;runtimeTime=None};incr passed;ProgressBar.increment progress true
 |Error message->
 recordTiming state {name="Unit: "^suite.name^": "^testName;totalTime;compileTime=None;runtimeTime=None};
 recordTiming state {name="Unit: "^displayName;totalTime;compileTime=None;runtimeTime=None};
 ProgressBar.increment progress false;ProgressBar.finish progress;
 Output.println (Printf.sprintf "  %s... %s%s FAIL%s %s(%s)%s" displayName Colors.red symbols.fail Colors.reset Colors.gray (formatTime totalTime) Colors.reset);
 Output.println ("    "^message);
 Queue.add {file="";name="Unit: "^displayName;message;details=[]} failedTests;incr failed;ProgressBar.update progress) suite.tests) suites;
 ProgressBar.finish progress;let totalTime=elapsed unitSectionTimer in printSummary symbols !passed !failed totalTime;
 recordResults state !passed !failed (List.of_seq (Queue.to_seq failedTests))
