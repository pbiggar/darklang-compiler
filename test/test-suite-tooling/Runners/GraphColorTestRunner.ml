(* GraphColorTestRunner.fs - Executes graph-coloring fixtures.
   Checks externally meaningful coloring, spill, preference, and MCS properties. *)
open Dark_compiler
open GraphColorFormat
module A=AllocationModel
let success:TestOutcome.t={TestOutcome.success=true;message="Test passed";expected=None;actual=None}
let failure message expected actual:TestOutcome.t={TestOutcome.success=false;message;expected=Some expected;actual=Some actual}
let countMatches expectation actual=match expectation with Exactly expected->actual=expected|AtMost expected->actual<=expected|AtLeast expected->actual>=expected
let formatCount=function Exactly value->string_of_int value|AtMost value->"<= "^string_of_int value|AtLeast value->">= "^string_of_int value
let formatOption=function None->""|Some value->"Some("^string_of_int value^")"
let runGraphColorTest (test:graphColorTest)=
 let graph=A.buildInterferenceGraphFromEdges test.vertices test.edges in
 let result=RegisterColoring.chordalGraphColor graph test.precolored test.availableColors test.preferencePairs test.movePairs in
 let checkCount label expectation actual=match expectation with Some expected when not (countMatches expected actual)->Some (failure (label^" did not match") (formatCount expected) (string_of_int actual))|Some _|None->None in
 let checkColors pairs relation label=List.find_map (fun (left,right)->match A.colorOf result left,A.colorOf result right with
 |Some leftColor,Some rightColor when relation leftColor rightColor->None
 |leftColor,rightColor->Some (failure label (Printf.sprintf "%d and %d" left right) (formatOption leftColor^" and "^formatOption rightColor))) pairs in
 let explicitColorFailure=List.find_map (fun (vertex,expected)->match A.colorOf result vertex with Some actual when actual=expected->None|actual->Some (failure "Vertex color did not match" (Printf.sprintf "%d=%d" vertex expected) (string_of_int vertex^"="^formatOption actual))) test.expectedColors in
 let mcsFailure=if test.expectMcsCoversAll then let ordering=RegisterCoalescing.maximumCardinalitySearch graph in
 if List.sort Int.compare ordering=List.sort Int.compare test.vertices && List.length ordering=List.length test.vertices then None else
 let render values=HostStructuralFormat.format (HostStructuralFormat.Sequence (List.map (fun value->HostStructuralFormat.Scalar (string_of_int value)) values)) in
 Some (failure "MCS ordering did not cover every vertex exactly once" (render (List.sort Int.compare test.vertices)) (render ordering)) else None in
 let selectionFailure=match test.expectedSelectionChecks with None->None|Some expected->let _,profile=RegisterCoalescing.maximumCardinalitySearchWithProfile graph in if profile.A.selectionChecks=expected then None else Some (failure "MCS selection checks did not match" (string_of_int expected) (string_of_int profile.A.selectionChecks)) in
 [checkCount "Chromatic number" test.expectedChromatic result.A.chromaticNumber;checkCount "Spill count" test.expectedSpills (A.spillCount result);checkCount "Colored count" test.expectedColored (A.coloredCount result);explicitColorFailure;checkColors test.expectedSame (=) "Expected vertices to have the same color";checkColors test.expectedDifferent (<>) "Expected vertices to have different colors";mcsFailure;selectionFailure] |> List.find_map Fun.id |> Option.value ~default:success
let loadGraphColorTests path=
 if not (TestFileIO.exists path) then Error ("Graph-color test file not found: "^path) else try GraphColorFormat.parseGraphColorFileContent path (HostFile.readText path) with exn->Error ("Failed to read graph-color test file "^path^": "^HostFile.errorMessage path exn)
let tests testFiles=
 let testsForFile path=match loadGraphColorTests path with
 |Error message->["parse "^Filename.basename path,(fun ()->Error message)]
 |Ok cases->List.map (fun test->test.name,(fun ()->let result=runGraphColorTest test in if result.TestOutcome.success then Ok () else Error (result.TestOutcome.message^"\nExpected: "^(match result.TestOutcome.expected with None->""|Some value->"Some("^value^")")^"\nActual: "^(match result.TestOutcome.actual with None->""|Some value->"Some("^value^")")))) cases in
 Array.to_list testFiles |> List.sort StringOrder.compare |> List.concat_map testsForFile
