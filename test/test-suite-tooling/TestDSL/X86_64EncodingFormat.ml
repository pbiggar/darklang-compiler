(*
   X86_64EncodingFormat.fs - Parser for multi-case x64 encoding and resolution fixtures.
   A successful case can assert final bytes, deferred fixup labels, or both.
*)
open Dark_compiler
module M=StringOrder.Map
type x64EncodingExpectation=ResolvesTo of bytes option * string list | ResolutionErrorContaining of string
type x64EncodingTest={name:string;instructions:X86_64.instr list;expectation:x64EncodingExpectation;sourceFile:string}
let (let*)=Result.bind
let knownSections=["NAME";"INPUT-X64";"OUTPUT-HEX";"EXPECT-FIXUPS";"EXPECT-ERROR"]
let groupCases sections=
 let rec loop completed current=function
 |[]->Ok (List.rev (if current=[] then completed else List.rev current::completed))
 |(("NAME",_) as section)::rest->loop (if current=[] then completed else List.rev current::completed) [section] rest
 |section::rest->if current=[] then Error ("x64 encoding case must start with NAME, found "^fst section) else loop completed (section::current) rest in
 loop [] [] sections
let toSectionMap sections=
 match List.find_opt (fun (name,_)->not (List.mem name knownSections)) sections with
 |Some (name,_)->Error ("Unknown x64 encoding section: "^name)
 |None->match List.find_opt (fun (name,_)->List.length (List.filter (fun (other,_)->name=other) sections)>1) sections with
 |Some (name,_)->Error ("Duplicate x64 encoding section: "^name)
 |None->Ok (M.of_list sections)
let required name sections=match M.find_opt name sections with
 |Some value when Text.trim value<>""->Ok (Text.trim value)
 |Some _->Error ("x64 encoding section "^name^" cannot be empty")
 |None->Error ("Missing required x64 encoding section: "^name)
let optional name sections=Option.map Text.trim (M.find_opt name sections)
let parseHexBytes text=
 let tokens=String.split_on_char ' ' (String.map (function '\t'|'\n'|'\r'|','->' '|c->c) text) |> List.filter ((<>) "") in
 let parseToken token=let trimmed=Text.trim token in let digits=if String.starts_with ~prefix:"0x" trimmed || String.starts_with ~prefix:"0X" trimmed then String.sub trimmed 2 (String.length trimmed-2) else trimmed in
 let rec withoutNuls length=if length>0 && digits.[length-1]='\000' then withoutNuls (length-1) else length in
 let digits=String.sub digits 0 (withoutNuls (String.length digits)) in
 if digits<>"" && String.for_all (function '0'..'9'|'a'..'f'|'A'..'F'->true|_->false) digits then
 let value=Z.of_string_base 16 digits in if Z.compare value (Z.of_int 255)<=0 then Ok (Char.chr (Z.to_int value)) else Error ("Invalid x64 hex byte '"^trimmed^"'")
 else Error ("Invalid x64 hex byte '"^trimmed^"'") in
 if tokens=[] then Error "OUTPUT-HEX requires at least one byte" else let* values=ResultList.traverse parseToken tokens in Ok (Bytes.of_string (String.of_seq (List.to_seq values)))
let parseFixups text=Common.normalizeLineEndings text |> String.split_on_char '\n' |> List.map Text.trim |> List.filter (fun line->line<>"" && not (Text.startsWith line "//"))
let parseCase path sections=
 let* values=toSectionMap sections in let* name=required "NAME" values in let* input=required "INPUT-X64" values in let* instructions=X86_64Parser.parseX64 input in
 let output=optional "OUTPUT-HEX" values and fixups=optional "EXPECT-FIXUPS" values and expectedError=optional "EXPECT-ERROR" values in
 let* expectation=match output,fixups,expectedError with
 |_,_,Some error when Text.trim error=""->Error "EXPECT-ERROR cannot be empty"
 |Some _,_,Some _|_,Some _,Some _->Error "EXPECT-ERROR cannot be combined with successful x64 expectations"
 |None,None,None->Error "x64 encoding test requires OUTPUT-HEX, EXPECT-FIXUPS, or EXPECT-ERROR"
 |None,None,Some error->Ok (ResolutionErrorContaining error)
 |output,fixups,None->let* expectedBytes=match output with Some text->Result.map Option.some (parseHexBytes text)|None->Ok None in
 let expectedFixups=match fixups with Some text->parseFixups text|None->[] in Ok (ResolvesTo (expectedBytes,expectedFixups)) in
 Ok {name;instructions;expectation;sourceFile=path}
let parseX64EncodingFileContent path content=let sections=Common.parseSections (Common.normalizeLineEndings content) in if sections=[] then Error "x64 encoding fixture contains no sections" else let* cases=groupCases sections in ResultList.traverse (parseCase path) cases
