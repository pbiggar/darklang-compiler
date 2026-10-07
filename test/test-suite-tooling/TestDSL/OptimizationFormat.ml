(*
   OptimizationFormat.fs - Parser for optimization test files
   Parses test files that verify IR optimizations work correctly.
   Each test contains source code and expected IR output at a specific stage.
   Format:
   ---NAME---
   test_name
   ---INPUT---
   source code
   or:
   ---STDLIB-FUNCTION---
   fully qualified prebuilt stdlib function name
   ---EXPECTED---
   exact IR output
   Stage of IR to verify
   After ANF optimization
   After MIR optimization (SSA-based)
   After LIR peephole optimization
   Direct symbolic LIR before and after the peephole pass
   Direct symbolic ARM64 before and after target peepholes
   Allocated LIR lowered to selected x64 instructions
   Optimization test specification
   Parse a single test from sections
   Parse multiple tests from a single file
   Tests are separated by ---NAME--- sections
*)
[@@@warning "-4"]
open Dark_compiler
type irStage=ANF | MIR | LIR | DirectLIR | DirectARM64 | DirectLIR2X64
type optimizationInput=Source of string | StdlibFunction of string
type optimizationTest={name:string;input:optimizationInput;expectedIR:string;stage:irStage;sourceFile:string}
type sectionName=Name | Input | StdlibFunctionSection | Expected
module Sections=Map.Make(struct type t=sectionName let compare=Stdlib.compare end)
type parseState={tests:optimizationTest list;sections:string Sections.t;currentSection:sectionName option;currentContent:string list;errors:string list}
let tryParseSectionName=function "NAME"->Ok Name|"INPUT"->Ok Input|"STDLIB-FUNCTION"->Ok StdlibFunctionSection|"EXPECTED"->Ok Expected|unknown->Error ("Unknown optimization section: "^unknown)
let parseTest stage filePath sections=match Sections.find_opt Name sections,Sections.find_opt Input sections,Sections.find_opt StdlibFunctionSection sections,Sections.find_opt Expected sections with
 |Some name,Some input,None,Some expected->Ok {name=HostText.trim name;input=Source (HostText.trim input);expectedIR=HostText.trim expected;stage;sourceFile=filePath}
 |Some name,None,Some functionName,Some expected when stage=ANF->Ok {name=HostText.trim name;input=StdlibFunction (HostText.trim functionName);expectedIR=HostText.trim expected;stage;sourceFile=filePath}
 |None,_,_,_->Error "Missing NAME section"
 |_,Some _,Some _,_->Error "INPUT and STDLIB-FUNCTION cannot be combined"
 |_,None,Some _,_ when stage<>ANF->Error "STDLIB-FUNCTION is supported only for ANF optimization tests"
 |_,None,None,_->Error "Missing INPUT or STDLIB-FUNCTION section"
 |_,_,_,None->Error "Missing EXPECTED section"
 |_,_,_,_->Error "Invalid optimization test sections"
let parseContent stage path content=
 let saveCurrentSection state=match state.currentSection with None->state|Some sectionName->{state with sections=Sections.add sectionName (String.concat "\n" (List.rev state.currentContent)) state.sections;currentContent=[]} in
 let parseCompletedTest state=if Sections.is_empty state.sections then state else match parseTest stage path state.sections with Ok test->{state with tests=test::state.tests;sections=Sections.empty}|Error error->{state with sections=Sections.empty;errors=error::state.errors} in
 let startSection sectionName state=let state=saveCurrentSection state in let state=if sectionName=Name then parseCompletedTest state else state in {state with currentSection=Some sectionName;currentContent=[]} in
 let parseLine state line=if String.starts_with ~prefix:"---" line && String.ends_with ~suffix:"---" line && Array.length (HostText.scalars line)>6 then
 let name=String.sub line 3 (String.length line-6) in match tryParseSectionName name with Ok section->startSection section state|Error msg->let state=saveCurrentSection state in {state with currentSection=None;currentContent=[];errors=msg::state.errors}
 else {state with currentContent=line::state.currentContent} in
 let initial={tests=[];sections=Sections.empty;currentSection=None;currentContent=[];errors=[]} in
 let state=List.fold_left parseLine initial (String.split_on_char '\n' (Common.normalizeLineEndings content)) |> saveCurrentSection |> parseCompletedTest in
 if state.errors<>[] then Error (String.concat "; " (List.rev state.errors)) else Ok (List.rev state.tests)
let parseTestFile stage path=if not (TestFileIO.exists path) then Error ("Test file not found: "^path) else parseContent stage path (HostFile.readText path)
