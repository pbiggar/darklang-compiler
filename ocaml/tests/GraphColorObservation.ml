(* Compare complete parsed graph fixtures and every runner outcome field. *)
module G=GraphColorFormat
module R=GraphColorTestRunner
module J=Semantic_observation.SemanticJson
let tuple=J.tuple
let str=J.string
let integer=J.int32
let list f xs=`List (List.map f xs)
let count=function G.Exactly n->J.union "CountExpectation" "Exactly" [integer n]|G.AtMost n->J.union "CountExpectation" "AtMost" [integer n]|G.AtLeast n->J.union "CountExpectation" "AtLeast" [integer n]
let pairs=list (fun (a,b)->tuple [integer a;integer b])
let parsed (test:G.graphColorTest)=J.record "GraphColorTest" ["Name",str test.G.name;"Vertices",list integer test.G.vertices;"Edges",pairs test.G.edges;"AvailableColors",integer test.G.availableColors;"Precolored",pairs test.G.precolored;"PreferencePairs",pairs test.G.preferencePairs;"MovePairs",pairs test.G.movePairs;"ExpectedChromatic",J.option count test.G.expectedChromatic;"ExpectedSpills",J.option count test.G.expectedSpills;"ExpectedColored",J.option count test.G.expectedColored;"ExpectedColors",pairs test.G.expectedColors;"ExpectedSame",pairs test.G.expectedSame;"ExpectedDifferent",pairs test.G.expectedDifferent;"ExpectMcsCoversAll",`Bool test.G.expectMcsCoversAll;"ExpectedSelectionChecks",J.option integer test.G.expectedSelectionChecks;"SourceFile",str test.G.sourceFile]
let outcome (result:TestOutcome.t)=J.record "PassTestResult" ["Success",`Bool result.TestOutcome.success;"Message",str result.TestOutcome.message;"Expected",J.option str result.TestOutcome.expected;"Actual",J.option str result.TestOutcome.actual]
let result f=function Ok value->J.union "FSharpResult" "Ok" [f value]|Error error->J.union "FSharpResult" "Error" [str error]
let run test=
 let variants=[test;{test with G.expectedChromatic=Some (G.Exactly 999)};{test with G.expectedChromatic=None;expectedSpills=Some (G.Exactly 999)};{test with G.expectedChromatic=None;expectedSpills=None;expectedColored=Some (G.AtLeast 999)};{test with G.expectedChromatic=None;expectedSpills=None;expectedColored=None;expectedColors=[0,999]};{test with G.expectedChromatic=None;expectedSpills=None;expectedColored=None;expectedColors=[];expectedSame=[0,999]};{test with G.expectedChromatic=None;expectedSpills=None;expectedColored=None;expectedColors=[];expectedSame=[];expectedDifferent=[0,0]};{test with G.expectedChromatic=None;expectedSpills=None;expectedColored=None;expectedColors=[];expectedSame=[];expectedDifferent=[];expectMcsCoversAll=true;expectedSelectionChecks=Some 999}] in
 list (fun value->tuple [parsed value;outcome (R.runGraphColorTest value)]) variants
let observe source=
 let fixtures=Yojson.Basic.from_file "scripts/ocaml/graphcolor_fixtures.json" |> Yojson.Basic.Util.to_list |> List.map Yojson.Basic.Util.to_string in
 let observed=list (fun content->result (fun tests->list (fun test->tuple [parsed test;run test]) tests) (G.parseGraphColorFileContent source content)) fixtures in
 let corpus=R.loadGraphColorTests "src/Tests/algorithms/graph-color/coloring.graphcolor" |> result (fun tests->list (fun test->tuple [parsed test;run test]) tests) in
 let loads=list (fun path->result (list parsed) (R.loadGraphColorTests path)) ["missing.graphcolor";"src/Tests/algorithms/graph-color";"src/Tests/algorithms/graph-color/coloring.graphcolor"] in
 let tests=R.tests [|"missing.graphcolor";"src/Tests/algorithms/graph-color/coloring.graphcolor"|] |> list (fun (name,run)->let actual=run () in tuple [str name;result (fun ()->`Null) actual]) in
 tuple [observed;corpus;loads;tests]
