(* Complete managed graph fixture parsers, typed LIR, release results and registration. *)
[@@@warning "-4-42"]
open Dark_compiler
open RCReleaseFormat
module J=Semantic_observation.SemanticJson
module L=Semantic_observation.ProductionLIR
let list f xs=`List (List.map f xs)
let tuple=J.tuple
let result encode=function Ok value->J.union "FSharpResult" "Ok" [encode value]|Error error->J.union "FSharpResult" "Error" [J.string error]
let unitResult=result (fun ()->`Null)
let rec shape=function
 |Int64Value->J.union "ManagedShape" "Int64Value" []|EnumValue->J.union "ManagedShape" "EnumValue" []|DynamicString->J.union "ManagedShape" "DynamicString" []|LiteralString->J.union "ManagedShape" "LiteralString" []|DynamicBlob->J.union "ManagedShape" "DynamicBlob" []
 |ListValue v->J.union "ManagedShape" "ListValue" [shape v]|DictValue (k,v)->J.union "ManagedShape" "DictValue" [shape k;shape v]|SumValue v->J.union "ManagedShape" "SumValue" [shape v]
 |TupleValue vs->J.union "ManagedShape" "TupleValue" [list shape vs]|RecordValue vs->J.union "ManagedShape" "RecordValue" [list shape vs]|ClosureValue vs->J.union "ManagedShape" "ClosureValue" [list shape vs]
let preserved (v:preservedRegister)=J.record "PreservedRegister" ["Register",L.physReg v.register;"Value",`Assoc ["kind",`String "int64";"value",`String (Int64.to_string v.value)]]
let placement=function CanonicalRoot->J.union "RootPlacement" "CanonicalRoot" []|ExplicitRoot (reg,vs)->J.union "RootPlacement" "ExplicitRoot" [L.physReg reg;list preserved vs]
let test (v:rCReleaseTest)=J.record "RCReleaseTest" ["Name",J.string v.name;"Root",shape v.root;"Placement",placement v.placement;"SourceFile",J.string v.sourceFile]
let program=result (fun (program,values)->tuple [L.program program;list preserved values])
let tests values=list (fun (name,run)->let actual=run () in tuple [J.string name;unitResult actual]) values
let targets=[Platform.ARM64Backend Platform.LinuxARM64;Platform.LinuxX86_64]
let fixed=lazy (
 let open Yojson.Basic.Util in
 let fixtures=Yojson.Basic.from_file "scripts/ocaml/rc_release_fixtures.json" |> member "format" |> to_list |> List.map (fun value->to_list value |> List.map to_int |> Array.of_list |> HostText.ofScalars) in
 let row content=result (list (fun value->tuple [test value;program (RCReleaseTestRunner.buildProgram value)])) (parseRCReleaseFileContent "boundary.rcrelease" content) in
 let path="src/Tests/backend/reference-release/reference-count.rcrelease" in
 let corpus=result (list (fun value->tuple [test value;program (RCReleaseTestRunner.buildProgram value);list (fun target->unitResult (RCReleaseTestRunner.runRCReleaseTest target value)) targets])) (RCReleaseTestRunner.loadRCReleaseTests path) in
 let paths=["missing.rcrelease";"src/Tests/backend/reference-release";path] in
 tuple [list row fixtures;corpus;list (fun path->result (list test) (RCReleaseTestRunner.loadRCReleaseTests path)) paths;list (fun target->tuple [tests (RCReleaseDSLTests.tests target);tests (RCReleaseTestRunner.tests target [|"missing.rcrelease"|]);list (fun (name,_)->J.string name) (RCReleaseTestRunner.tests target (Array.of_list (List.rev paths)))]) targets])
let observe source=tuple [result (list test) (parseRCReleaseFileContent source source);Lazy.force fixed]
