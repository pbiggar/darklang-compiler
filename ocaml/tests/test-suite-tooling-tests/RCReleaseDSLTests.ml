(*
   RCReleaseDSLTests.fs - Tests for semantic reference-release fixture parsing.
   Covers typed shape parsing, invalid placement, and executable release behavior.
*)
[@@@warning "-4-42"]
open Dark_compiler
open RCReleaseFormat
type testResult=(unit,string) result
let rec shape=function
 |Int64Value->StructuralValue.Union ("Int64Value",[])|EnumValue->StructuralValue.Union ("EnumValue",[])|DynamicString->StructuralValue.Union ("DynamicString",[])|LiteralString->StructuralValue.Union ("LiteralString",[])|DynamicBlob->StructuralValue.Union ("DynamicBlob",[])
 |ListValue v->StructuralValue.Union ("ListValue",[shape v])|DictValue (k,v)->StructuralValue.Union ("DictValue",[shape k;shape v])|SumValue v->StructuralValue.Union ("SumValue",[shape v])
 |TupleValue vs->StructuralValue.Union ("TupleValue",[StructuralValue.Sequence (List.map shape vs)])|RecordValue vs->StructuralValue.Union ("RecordValue",[StructuralValue.Sequence (List.map shape vs)])|ClosureValue vs->StructuralValue.Union ("ClosureValue",[StructuralValue.Sequence (List.map shape vs)])
let display cases=
 let register reg=LIRTestFormatting.physReg reg in
 let one (test:rCReleaseTest)=let placement=match test.placement with CanonicalRoot->StructuralValue.Union ("CanonicalRoot",[])|ExplicitRoot (reg,values)->StructuralValue.Union ("ExplicitRoot",[register reg;StructuralValue.Sequence (List.map (fun value->StructuralValue.Record ["Register",register value.register;"Value",StructuralValue.Scalar (Int64.to_string value.value)]) values)]) in
 HostStructuralFormat.format (StructuralValue.Record ["Name",StructuralValue.Text test.name;"Root",shape test.root;"Placement",placement;"SourceFile",StructuralValue.Text test.sourceFile]) in
 let first=List.filteri (fun index _->index<3) cases |> List.map one in "["^String.concat "; " first^(if List.length cases>3 then "; ... " else "")^"]"
let testParsesNestedManagedShape ()=
 match parseRCReleaseFileContent "nested.rcrelease" "---NAME---\nnested graph\n---ROOT---\ntuple(string, list(i64), dict(string, record(blob)))\n" with
 |Ok [{root=TupleValue [DynamicString;ListValue Int64Value;DictValue (DynamicString,RecordValue [DynamicBlob])];_}]->Ok ()
 |Ok tests->Error ("Expected one nested release shape, got "^display tests)
 |Error msg->Error ("Expected nested release shape to parse: "^msg)
let testRejectsPreserveWithoutRootRegister ()=
 match parseRCReleaseFileContent "invalid.rcrelease" "---NAME---\ninvalid placement\n---ROOT---\ntuple(string)\n---PRESERVE---\nX0 = 42\n" with
 |Error msg when HostText.contains msg "requires ROOT-REGISTER"->Ok ()
 |Error msg->Error ("Expected placement error, got: "^msg)
 |Ok _->Error "Expected PRESERVE without ROOT-REGISTER to fail"
let testRejectsInvalidShapeArity ()=
 match parseRCReleaseFileContent "invalid.rcrelease" "---NAME---\ninvalid dict\n---ROOT---\ndict(string)\n" with
 |Error msg when HostText.contains msg "exactly two"->Ok ()
 |Error msg->Error ("Expected dict arity error, got: "^msg)
 |Ok _->Error "Expected one-argument dict shape to fail"
let testRunsNestedReleaseCase target ()=
 match parseRCReleaseFileContent "release.rcrelease" "---NAME---\nrelease nested graph\n---ROOT---\ntuple(string, list(i64), dict(i64, string))\n" with
 |Ok [test]->RCReleaseTestRunner.runRCReleaseTest target test
 |Ok tests->Error ("Expected one release case, got "^string_of_int (List.length tests))
 |Error msg->Error ("Expected release case to parse: "^msg)
let tests target=["Reference-release DSL parses nested managed shapes",testParsesNestedManagedShape;"Reference-release DSL rejects preservation without root placement",testRejectsPreserveWithoutRootRegister;"Reference-release DSL rejects invalid shape arity",testRejectsInvalidShapeArity;"Reference-release DSL executes nested release",testRunsNestedReleaseCase target]
