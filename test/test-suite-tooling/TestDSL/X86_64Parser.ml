(*
   X86_64Parser.fs - Parser for the x64 instruction subset used by encoding fixtures.
   Uses constructor-style syntax matching the x64 instruction discriminated union.
*)
[@@@warning "-4"]
open Dark_compiler
open X86_64
open DSLPattern
let (let*)=Result.bind
external upperScalar : int -> int = "dark_runner_upper_scalar"
let uppercase text=let buffer=Buffer.create (String.length text) in Uutf.String.fold_utf_8 (fun () _->function `Uchar value->Uutf.Buffer.add_utf_8 buffer (Uchar.of_int (upperScalar (Uchar.to_int value)))|`Malformed _->invalid_arg "Malformed UTF-8 host text") () text;Buffer.contents buffer
let parseReg text=match uppercase (HostText.trim text) with
 |"RAX"->Ok RAX|"RBX"->Ok RBX|"RCX"->Ok RCX|"RDX"->Ok RDX|"RSI"->Ok RSI|"RDI"->Ok RDI|"RBP"->Ok RBP|"RSP"->Ok RSP|"R8"->Ok R8|"R9"->Ok R9|"R10"->Ok R10|"R11"->Ok R11|"R12"->Ok R12|"R13"->Ok R13|"R14"->Ok R14|"R15"->Ok R15
 |value->Error ("Invalid x64 register '"^value^"'")
let parseFReg text=match uppercase (HostText.trim text) with
 |"XMM0"->Ok XMM0|"XMM1"->Ok XMM1|"XMM2"->Ok XMM2|"XMM3"->Ok XMM3|"XMM4"->Ok XMM4|"XMM5"->Ok XMM5|"XMM6"->Ok XMM6|"XMM7"->Ok XMM7|"XMM8"->Ok XMM8|"XMM9"->Ok XMM9|"XMM10"->Ok XMM10|"XMM11"->Ok XMM11|"XMM12"->Ok XMM12|"XMM13"->Ok XMM13|"XMM14"->Ok XMM14|"XMM15"->Ok XMM15
 |value->Error ("Invalid x64 floating-point register '"^value^"'")
let parseCondition text=match uppercase (HostText.trim text) with
 |"EQ"->Ok EQ|"NE"->Ok NE|"LT"->Ok LT|"GT"->Ok GT|"LE"->Ok LE|"GE"->Ok GE|"B"->Ok B|"A"->Ok A|"BE"->Ok BE|"AE"->Ok AE|"P"->Ok P|"NP"->Ok NP|value->Error ("Invalid x64 condition '"^value^"'")
let parseInt32 description text=let text=HostText.trim text in match HostText.tryParseInt32 text with Some value->Ok value|None->Error ("Invalid "^description^" '"^text^"' (expected 32-bit integer)")
let parseShift text=let text=HostText.trim text in match HostText.tryParseInt32 text with Some value->Ok (Int32.to_int value)|None->Error ("Invalid shift '"^text^"' (expected integer)")
let arguments name fields line=
 let rec join=function []->[]|[field]->[Capture field]|field::rest->Capture field::Literal ","::space::join rest in
 matched (Literal (name^"(")::join fields@[Literal ")"]) line
let last=[Repeat (Except ")",1,true)]
let middle=[Repeat (Except ",",1,true)]
let oneArg name=arguments name [last]
let twoArgs name=arguments name [middle;last]
let threeArgs name=arguments name [middle;middle;last]
let parseRegReg constructor name line=match twoArgs name line with None->Ok None|Some groups->let* a=parseReg groups.(1) in let* b=parseReg groups.(2) in Ok (Some (constructor (a,b)))
let parseRegImmediate constructor name line=match twoArgs name line with None->Ok None|Some groups->let* reg=parseReg groups.(1) in let* imm=parseInt32 "immediate" groups.(2) in Ok (Some (constructor (reg,imm)))
let parseLine lineNumber source=
 let line=HostText.trim source in let withLine=Result.map_error (fun msg->Printf.sprintf "Line %d: %s" lineNumber msg) in
 let labelLike name constructor=Option.map (fun groups->constructor (HostText.trim groups.(1))) (oneArg name line) in
 match line with "RET"->Ok RET|"SYSCALL"->Ok SYSCALL|"CQO"->Ok CQO|_->
 match labelLike "Label" (fun s->Label s) with Some instruction->Ok instruction|None->
 match labelLike "JMP" (fun s->JMP s) with Some instruction->Ok instruction|None->
 match labelLike "CALL" (fun s->CALL s) with Some instruction->Ok instruction|None->
 let unaryReg name constructor ()=match oneArg name line with None->Ok None|Some groups->Result.map (fun reg->Some (constructor reg)) (parseReg groups.(1)) in
 match twoArgs "Jcc" line with
 |Some groups->withLine (let* condition=parseCondition groups.(1) in Ok (Jcc (condition,HostText.trim groups.(2))))
 |None->
 let memory name constructor ()=match threeArgs name line with None->Ok None|Some groups->let* a=parseReg groups.(1) in let* b=parseReg groups.(2) in let* offset=parseInt32 "memory offset" groups.(3) in Ok (Some (constructor (a,b,offset))) in
 let store name constructor ()=match threeArgs name line with None->Ok None|Some groups->let* base=parseReg groups.(1) in let* offset=parseInt32 "memory offset" groups.(2) in let* src=parseReg groups.(3) in Ok (Some (constructor (base,offset,src))) in
 let leaIndex=arguments "LEA_index" [middle;middle;middle;[digits];[Alternatives [[Literal "-";digits];[digits]]]] line in
 let parsers=[
 (fun ()->parseRegReg (fun (v0,v1)->MOV_reg (v0,v1)) "MOV_reg" line);
 (fun ()->parseRegImmediate (fun (v0,v1)->MOV_imm32 (v0,v1)) "MOV_imm32" line);
 memory "MOV_load" (fun (v0,v1,v2)->MOV_load (v0,v1,v2));store "MOV_store" (fun (v0,v1,v2)->MOV_store (v0,v1,v2));memory "LEA" (fun (v0,v1,v2)->LEA (v0,v1,v2));memory "ADD_load" (fun (v0,v1,v2)->ADD_load (v0,v1,v2));memory "SUB_load" (fun (v0,v1,v2)->SUB_load (v0,v1,v2));
 (fun ()->match leaIndex with None->Ok None|Some groups->let* dest=parseReg groups.(1) in let* base=parseReg groups.(2) in let* index=parseReg groups.(3) in let* scale=parseShift groups.(4) in let* offset=parseInt32 "LEA offset" groups.(5) in Ok (Some (LEA_index (dest,base,index,scale,offset))));
 (fun ()->parseRegImmediate (fun (v0,v1)->ADD_imm (v0,v1)) "ADD_imm" line);(fun ()->parseRegImmediate (fun (v0,v1)->SUB_imm (v0,v1)) "SUB_imm" line);(fun ()->parseRegReg (fun (v0,v1)->XOR_reg (v0,v1)) "XOR_reg" line);(fun ()->parseRegReg (fun (v0,v1)->ADD_reg (v0,v1)) "ADD_reg" line);(fun ()->parseRegImmediate (fun (v0,v1)->CMP_imm (v0,v1)) "CMP_imm" line);(fun ()->parseRegReg (fun (v0,v1)->IMUL_reg (v0,v1)) "IMUL_reg" line);
 (fun ()->match twoArgs "SHL_imm" line with None->Ok None|Some groups->let* reg=parseReg groups.(1) in let* shift=parseShift groups.(2) in Ok (Some (SHL_imm (reg,shift))));
 memory "MOV_load_byte" (fun (v0,v1,v2)->MOV_load_byte (v0,v1,v2));
 (fun ()->match threeArgs "MOVSD_store" line with None->Ok None|Some groups->let* base=parseReg groups.(1) in let* offset=parseInt32 "memory offset" groups.(2) in let* src=parseFReg groups.(3) in Ok (Some (MOVSD_store (base,offset,src))));
 unaryReg "PUSH" (fun reg->PUSH reg);unaryReg "POP" (fun reg->POP reg);unaryReg "NEG" (fun reg->NEG reg)] in
 let rec choose=function []->Error ("Invalid x64 instruction '"^line^"'")|parser::rest->match parser () with Error msg->Error msg|Ok (Some instruction)->Ok instruction|Ok None->choose rest in withLine (choose parsers)
let parseX64 text=
 let lines=Common.normalizeLineEndings text |> String.split_on_char '\n' |> List.mapi (fun index line->index+1,HostText.trim line) |> List.filter (fun (_,line)->line<>"" && not (HostText.startsWith line "//")) in
 if lines=[] then Error "INPUT-X64 contains no instructions" else ResultList.traverse (fun (number,line)->parseLine number line) lines
