(* Original repository policy and ownership region-contract units. *)
module J=Semantic_observation.SemanticJson
let result=function Ok ()->J.union "FSharpResult" "Ok" [`Null]|Error error->J.union "FSharpResult" "Error" [J.string error]
let fixed=lazy (`List (List.map (fun (name,run)->J.tuple [J.string name;result (run ())]) (StdlibSourceTests.tests @ ScriptHelperTests.tests @ RegionContractTests.tests @ OwnershipCallFactsTests.tests)))
let observe source=J.tuple [J.string source;Lazy.force fixed]
