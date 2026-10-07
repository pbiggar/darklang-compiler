(*
   LIRExecutionFormat.fs - Parser for executable single-block LIR fixtures.
   Keeps successful process results and expected codegen failures typed.
*)
open Dark_compiler
module M=StringOrder.Map
type leakCheckMode=LeakCheckDisabled | LeakCheckEnabled
type processExpectation=ExpectedExitCode of int | ExpectedStdout of string | ExpectedStderr of string
type lIRExecutionExpectation=ExpectedProcessResult of processExpectation list | ExpectedCodegenError of string
type lIRExecutionTest={name:string;program:LIR.program;leakCheck:leakCheckMode;expectation:lIRExecutionExpectation;sourceFile:string}
let (let*)=Result.bind
let knownSections=["NAME";"INPUT-LIR";"LEAK-CHECK";"EXPECT-EXIT";"EXPECT-STDOUT";"EXPECT-STDERR";"EXPECT-CODEGEN-ERROR"]
let groupCases sections=
 let rec loop completed current=function
 |[]->Ok (List.rev (if current=[] then completed else List.rev current::completed))
 |(("NAME",_) as section)::rest->loop (if current=[] then completed else List.rev current::completed) [section] rest
 |section::rest->if current=[] then Error ("LIR-execution case must start with NAME, found "^fst section) else loop completed (section::current) rest in
 loop [] [] sections
let toSectionMap sections=
 match List.find_opt (fun (name,_)->not (List.mem name knownSections)) sections with
 |Some (name,_)->Error ("Unknown LIR-execution section: "^name)
 |None->match List.find_opt (fun (name,_)->List.length (List.filter (fun (other,_)->name=other) sections)>1) sections with
 |Some (name,_)->Error ("Duplicate LIR-execution section: "^name)
 |None->Ok (M.of_list sections)
let required name sections=match M.find_opt name sections with
 |Some value when Text.trim value<>""->Ok (Text.trim value)
 |Some _->Error ("LIR-execution section "^name^" cannot be empty")
 |None->Error ("Missing required LIR-execution section: "^name)
let parseLeakCheck sections=match Option.map (fun value->Text.lowerInvariant (Text.trim value)) (M.find_opt "LEAK-CHECK" sections) with
 |None|Some "false"->Ok LeakCheckDisabled|Some "true"->Ok LeakCheckEnabled|Some value->Error ("Invalid LEAK-CHECK value '"^value^"' (expected true or false)")
let parseExitExpectation sections=match M.find_opt "EXPECT-EXIT" sections with
 |None->Ok None|Some value->let trimmed=Text.trim value in match Text.tryParseInt32 trimmed with Some exitCode->Ok (Some (ExpectedExitCode (Int32.to_int exitCode)))|None->Error ("Invalid EXPECT-EXIT value '"^trimmed^"' (expected 32-bit integer)")
let outputExpectation section constructor sections=Option.map (fun value->constructor (Common.normalizeLineEndings (Text.trim value))) (M.find_opt section sections)
let parseCase path sections=
 let* values=toSectionMap sections in let* name=required "NAME" values in let* source=required "INPUT-LIR" values in let* leakCheck=parseLeakCheck values in let* exitExpectation=parseExitExpectation values in
 let processExpectations=List.filter_map Fun.id [exitExpectation;outputExpectation "EXPECT-STDOUT" (fun value->ExpectedStdout value) values;outputExpectation "EXPECT-STDERR" (fun value->ExpectedStderr value) values] in
 let codegenError=Option.map Text.trim (M.find_opt "EXPECT-CODEGEN-ERROR" values) in
 let* expectation=match codegenError,processExpectations with
 |Some "",_->Error "EXPECT-CODEGEN-ERROR cannot be empty"
 |Some _,_::_->Error "EXPECT-CODEGEN-ERROR cannot be combined with process expectations"
 |Some expected,[]->Ok (ExpectedCodegenError expected)
 |None,[]->Error "LIR-execution case requires at least one expectation"
 |None,expected->Ok (ExpectedProcessResult expected) in
 let* program=Result.map_error (fun msg->"Failed to parse INPUT-LIR: "^msg) (LIRParser.parseLIR source) in
 Ok {name;program;leakCheck;expectation;sourceFile=path}
let parseLIRExecutionFileContent path content=
 let sections=Common.parseSections (Common.normalizeLineEndings content) in
 if sections=[] then Error "LIR-execution fixture contains no sections" else let* cases=groupCases sections in ResultList.traverse (parseCase path) cases
