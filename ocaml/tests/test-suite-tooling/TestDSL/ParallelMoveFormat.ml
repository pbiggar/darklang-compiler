(*
   ParallelMoveFormat.fs - Parser for parallel-move lowering fixtures.
   Converts compact destination/operand pairs and expected symbolic ARM64 into typed cases.
*)
open Dark_compiler
module M=StringOrder.Map
type parallelMoveTest={name:string;moves:(LIR.physReg*LIR.operand) list;expected:Symbolic.instr list;sourceFile:string}
let (let*)=Result.bind
let knownSections=["NAME";"INPUT-MOVES";"OUTPUT-ARM64"]
let groupCases sections=
 let rec loop completed current=function
 |[]->Ok (List.rev (if current=[] then completed else List.rev current::completed))
 |(("NAME",_) as section)::rest->loop (if current=[] then completed else List.rev current::completed) [section] rest
 |section::rest->if current=[] then Error ("Parallel-move case must start with NAME, found "^fst section) else loop completed (section::current) rest in
 loop [] [] sections
let toSectionMap sections=
 match List.find_opt (fun (name,_)->not (List.mem name knownSections)) sections with
 |Some (name,_)->Error ("Unknown parallel-move section: "^name)
 |None->match List.find_opt (fun (name,_)->List.length (List.filter (fun (other,_)->name=other) sections)>1) sections with
 |Some (name,_)->Error ("Duplicate parallel-move section: "^name)
 |None->Ok (M.of_list sections)
let required name sections=match M.find_opt name sections with
 |Some value when HostText.trim value<>""->Ok (HostText.trim value)
 |Some _->Error ("Parallel-move section "^name^" cannot be empty")
 |None->Error ("Missing required parallel-move section: "^name)
let parseMove lineNumber line=
 let open DSLPattern in
 match matched [Capture [Repeat(Dot,1,false)];space;Literal "<-";space;Capture [Repeat(Dot,1,true)]] (HostText.trim line) with
 |None->Error (Printf.sprintf "Line %d: invalid move '%s' (expected DEST <- OPERAND)" lineNumber line)
 |Some groups->let* destination=Result.map_error (fun msg->Printf.sprintf "Line %d: %s" lineNumber msg) (LIRParser.parsePhysReg groups.(1)) in
 let* operand=Result.map_error (fun msg->Printf.sprintf "Line %d: %s" lineNumber msg) (LIRParser.parseOperand groups.(2)) in Ok (destination,operand)
let parseMoves text=match Common.stripCommentsAndEmpty text with
 |[]->Error "INPUT-MOVES requires at least one move"
 |lines->ResultList.traverse (fun (number,line)->parseMove number line) (List.mapi (fun index line->index+1,line) lines)
let parseCase path sections=
 let* values=toSectionMap sections in let* name=required "NAME" values in let* input=required "INPUT-MOVES" values in let* output=required "OUTPUT-ARM64" values in
 let* moves=Result.map_error (fun msg->"Failed to parse INPUT-MOVES: "^msg) (parseMoves input) in
 let* expected=Result.map_error (fun msg->"Failed to parse OUTPUT-ARM64: "^msg) (if String.lowercase_ascii output="none" then Ok [] else ARM64SymbolicParser.parseARM64Symbolic output) in
 Ok {name;moves;expected;sourceFile=path}
let parseParallelMoveFileContent path content=
 let sections=Common.parseSections (Common.normalizeLineEndings content) in
 if sections=[] then Error "Parallel-move fixture contains no sections" else let* cases=groupCases sections in ResultList.traverse (parseCase path) cases
