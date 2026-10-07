(*
   IRFormatSnapshotFormat.ml - Parser for exact IR formatter snapshot fixtures.
   Parses the existing compact ANF, MIR, and LIR test syntaxes into typed formatter inputs.
*)
(* Preserve case grouping, ordered section validation and complete typed formatter input. *)
open Dark_compiler
module M=StringOrder.Map
type irFormatInput=ANFInput of ANF.program | MIRInput of MIR.program | LIRInput of LIR.program
type irFormatSnapshotTest={name:string;input:irFormatInput;expected:string;sourceFile:string}
let (let*)=Result.bind
let knownSections=["NAME";"IR";"INPUT";"EXPECTED"]
let groupCases sections=
 let rec loop completed current=function
 |[]->Ok (List.rev (if current=[] then completed else List.rev current::completed))
 |(("NAME",_) as section)::rest->loop (if current=[] then completed else List.rev current::completed) [section] rest
 |section::rest->if current=[] then Error ("IR-format case must start with NAME, found "^fst section) else loop completed (section::current) rest in
 loop [] [] sections
let toSectionMap sections=
 match List.find_opt (fun (name,_)->not (List.mem name knownSections)) sections with
 |Some (name,_)->Error ("Unknown IR-format section: "^name)
 |None->match List.find_opt (fun (name,_)->List.length (List.filter (fun (other,_)->name=other) sections)>1) sections with
 |Some (name,_)->Error ("Duplicate IR-format section: "^name)
 |None->Ok (M.of_list sections)
let required name sections=match M.find_opt name sections with
 |Some value when Text.trim value<>""->Ok (Text.trim value)
 |Some _->Error ("IR-format section "^name^" cannot be empty")
 |None->Error ("Missing required IR-format section: "^name)
let parseInput kind source=match Text.lowerInvariant (Text.trim kind) with
 |"anf"->Result.map (fun value->ANFInput value) (ANFParser.parseANF source)
 |"mir"->Result.map (fun value->MIRInput value) (MIRParser.parseMIR source)
 |"lir"->Result.map (fun value->LIRInput value) (LIRParser.parseLIR source)
 |value->Error ("Unknown IR kind '"^value^"' (expected anf, mir, or lir)")
let parseCase path sections=
 let* values=toSectionMap sections in
 let* name=required "NAME" values in let* kind=required "IR" values in let* source=required "INPUT" values in let* expected=required "EXPECTED" values in
 let* input=Result.map_error (fun msg->"Failed to parse "^Text.trim kind^" INPUT: "^msg) (parseInput kind source) in
 Ok {name;input;expected=Common.normalizeLineEndings expected;sourceFile=path}
let parseIRFormatSnapshotFileContent path content=
 let sections=Common.parseSections (Common.normalizeLineEndings content) in
 if sections=[] then Error "IR-format fixture contains no sections" else let* cases=groupCases sections in ResultList.traverse (parseCase path) cases
