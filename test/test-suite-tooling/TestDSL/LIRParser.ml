(*
   LIRParser.fs - Parser for symbolic LIR DSL
   Parses human-readable LIR text into LIR.Program data structures.
   Example LIR:
   X1 <- Mov(Imm 42)
   X2 <- Add(X1, Imm 5)
   Ret
*)
(* Preserve symbolic LIR grammar, numeric widths and ordered diagnostic selection. *)
[@@@warning "-4-42"]
open Dark_compiler
open! LIR
open DSLPattern
type instructionOrTerminator=Instruction of instr | Terminator of terminator
let (let*)=Result.bind
let parseInt32Field description text=
 let trimmed=Text.trim text in match Text.tryParseInt32 trimmed with
 |Some value->Ok (Int32.to_int value)|None->Error ("Invalid "^description^" '"^trimmed^"' (expected 32-bit integer)")
let parseInt64Field description text=
 let trimmed=Text.trim text in match int64 trimmed with
 |Some value->Ok value|None->Error ("Invalid "^description^" '"^trimmed^"' (expected 64-bit integer)")
(*
   Parse physical register from text like "X0", "X1", etc.
*)
let parsePhysReg text=match Text.trim text with
 |"X0"->Ok X0|"X1"->Ok X1|"X2"->Ok X2|"X3"->Ok X3|"X4"->Ok X4|"X5"->Ok X5|"X6"->Ok X6|"X7"->Ok X7
 |"X8"->Ok X8|"X9"->Ok X9|"X10"->Ok X10|"X11"->Ok X11|"X12"->Ok X12|"X13"->Ok X13|"X14"->Ok X14|"X15"->Ok X15
 |"X16"->Ok X16|"X17"->Ok X17|"X19"->Ok X19|"X20"->Ok X20|"X21"->Ok X21|"X22"->Ok X22|"X23"->Ok X23|"X24"->Ok X24
 |"X25"->Ok X25|"X26"->Ok X26|"X27"->Ok X27|"X29"->Ok X29|"X30"->Ok X30|"SP"->Ok SP
 |reg->Error ("Invalid physical register '"^reg^"' (expected X0-X17, X19-X27, X29, X30, or SP)")
(*
   Parse register (physical or virtual) from text
*)
let parseRegister text=
 let text=Text.trim text in
 if Text.startsWith text "v" then
 match matched [literal "v";Capture [digits]] text with
 |Some groups->Result.map (fun id->Virtual id) (parseInt32Field "virtual register" groups.(1))
 |None->Error ("Invalid virtual register '"^text^"' (expected 'v0', 'v1', etc.)")
 else Result.map (fun reg->Physical reg) (parsePhysReg text)
let signedDigits=Alternatives [[literal "-";digits];[digits]]
(*
   Parse operand from text like "Imm 42", "Reg X1", "Stack 0"
   Try immediate: "Imm 42"
   Try register: "Reg X1" or "Reg v0"
   Try stack slot: "Stack 0"
*)
let parseOperand text=
 let text=Text.trim text in
 match matched [literal "str[";Capture [Repeat (Dot,0,true)];literal "]"] text with
 |Some groups->Result.map (fun value->StringSymbol value) (Common.parseEscapedText groups.(1))
 |None->match matched [literal "Imm";spaces;Capture [signedDigits]] text with
 |Some groups->Result.map (fun value->Imm value) (parseInt64Field "immediate" groups.(1))
 |None->match matched [literal "Reg";spaces;Capture [any]] text with
 |Some groups->Result.map (fun reg->Reg reg) (parseRegister groups.(1))
 |None->match matched [literal "Stack";spaces;Capture [signedDigits]] text with
 |Some groups->Result.map (fun offset->StackSlot offset) (parseInt32Field "stack slot" groups.(1))
 |None->Error ("Invalid operand '"^text^"' (expected 'Imm N', 'Reg X', 'Stack N', or 'str[...]')")
let parseRcKind text=match Text.lowerInvariant (Text.trim text) with
 |"generic"->Ok GenericHeap|"list"->Ok TaggedList|"dict"->Ok DictHeap|"closure"->Ok ClosureHeap
 |value->Error ("Invalid reference-count kind '"^value^"' (expected generic, list, dict, or closure)")
let lazyText=Repeat (Dot,1,false)
let arguments name fields=
 let rec joined=function []->[]|[field]->[Capture field]|field::rest->Capture field::literal ","::space::joined rest in
 literal (name^"(")::joined fields@[literal ")"]
let assignment name fields=Capture [lazyText]::space::literal "<-"::space::arguments name fields
(*
   Parse a single LIR instruction or terminator
   Returns either an Instr or a Terminator
   Try terminators and zero-operand instructions.
   Try RandomInt64: "X1 <- RandomInt64"
   Try PrintInt64: "PrintInt64(X0)" or "PrintInt64(v0)"
   Try PrintUInt64: "PrintUInt64(X0)" or "PrintUInt64(v0)"
   Try PrintHeapStringNoNewline: "PrintHeapStringNoNewline(X0)"
   Try PrintBool: "PrintBool(X0)" or "PrintBool(v0)"
   Try HeapAlloc: "X2 <- HeapAlloc(16)"
   Try HeapLoad: "X1 <- HeapLoad(X2, 16)"
   Try HeapStore: "HeapStore(X2, 8, Imm 42)"
   Try RawAlloc: "X2 <- RawAlloc(X1)"
   Try RawFree: "RawFree(X2)"
   Try StringConcat: "X2 <- StringConcat(str[a], str[b])"
   Try RefCountInc/RefCountDec: "RefCountDec(X2, 16, generic)"
   Try RefCountIncString/RefCountDecString: "RefCountIncString(Reg X2)"
   Try FileReadBlob: "X0 <- FileReadBlob(str[path])"
   Try Mov: "X1 <- Mov(Imm 42)"
   Try Store: "Store(Stack -8, X11)"
   Try Add: "X3 <- Add(X1, Imm 5)"
   Try Sub: "X3 <- Sub(X1, Imm 5)"
   Try Mul: "X3 <- Mul(X1, Reg X2)" - note: Mul requires both operands to be registers
   Try Sdiv: "X3 <- Sdiv(X1, Reg X2)" - note: Sdiv requires both operands to be registers
   Try AndImm: "X0 <- AndImm(X1, 42)"
   Try Lsl_imm: "X0 <- Lsl_imm(X1, #3)"
   Try Madd/Msub: "X0 <- Madd(X1, X2, X3)"
*)
let parseInstructionOrTerminator lineNum line=
 let line=Text.trim line in
 let prefix error=Printf.sprintf "Line %d: %s" lineNum error in
 if line="Ret" then Ok (Terminator Ret) else if line="Exit" then Ok (Instruction Exit) else
 let unary name construct=arguments name [[any]],(fun groups->Result.map construct (parseRegister groups.(1))) in
 let binary name construct=assignment name [[lazyText];[any]],(fun g->let* dest=parseRegister g.(1) in let* left=parseRegister g.(2) in let* right=parseOperand g.(3) in Ok (construct dest left right)) in
 let multiply name construct=[Capture [lazyText];space;literal "<-";space;literal (name^"(");Capture [lazyText];literal ",";space;literal "Reg";spaces;Capture [any];literal ")"],(fun g->let* dest=parseRegister g.(1) in let* left=parseRegister g.(2) in let* right=parseRegister g.(3) in Ok (construct dest left right)) in
 let cases=[
  [Capture [lazyText];space;literal "<-";space;literal "RandomInt64"],(fun g->Result.map (fun reg->RandomInt64 reg) (parseRegister g.(1)));
  unary "PrintInt64" (fun reg->PrintInt64 reg);
  unary "PrintUInt64" (fun reg->PrintUInt64 reg);
  unary "PrintHeapStringNoNewline" (fun reg->PrintHeapStringNoNewline reg);
  unary "PrintBool" (fun reg->PrintBool reg);
  assignment "HeapAlloc" [[signedDigits]],(fun g->let* dest=parseRegister g.(1) in let* size=parseInt32Field "heap allocation size" g.(2) in Ok (HeapAlloc (dest,size)));
  assignment "HeapLoad" [[lazyText];[signedDigits]],(fun g->let* dest=parseRegister g.(1) in let* addr=parseRegister g.(2) in let* offset=parseInt32Field "heap load offset" g.(3) in Ok (HeapLoad (dest,addr,offset)));
  arguments "HeapStore" [[lazyText];[signedDigits];[any]],(fun g->let* addr=parseRegister g.(1) in let* offset=parseInt32Field "heap store offset" g.(2) in let* src=parseOperand g.(3) in Ok (HeapStore (addr,offset,src,None)));
  assignment "RawAlloc" [[any]],(fun g->let* dest=parseRegister g.(1) in let* bytes=parseRegister g.(2) in Ok (RawAlloc (dest,bytes)));
  unary "RawFree" (fun reg->RawFree reg);
  assignment "StringConcat" [[lazyText];[any]],(fun g->let* dest=parseRegister g.(1) in let* left=parseOperand g.(2) in let* right=parseOperand g.(3) in Ok (StringConcat (dest,left,right,[])));
  Capture [Alternatives [[literal "RefCountInc"];[literal "RefCountDec"]]]::literal "("::Capture [lazyText]::literal ","::space::Capture [signedDigits]::literal ","::space::Capture [any]::[literal ")"],(fun g->let* addr=parseRegister g.(2) in let* size=parseInt32Field "reference-count payload size" g.(3) in let* kind=parseRcKind g.(4) in Ok (if g.(1)="RefCountInc" then RefCountInc (addr,size,kind,None) else RefCountDec (addr,size,kind,None)));
  [Capture [Alternatives [[literal "RefCountIncString"];[literal "RefCountDecString"]]];literal "(";Capture [any];literal ")"],(fun g->let* operand=parseOperand g.(2) in Ok (if g.(1)="RefCountIncString" then RefCountIncString operand else RefCountDecString operand));
  assignment "FileReadBlob" [[any]],(fun g->let* dest=parseRegister g.(1) in let* path=parseOperand g.(2) in Ok (FileReadBlob (dest,path)));
  assignment "Mov" [[any]],(fun g->let* dest=parseRegister g.(1) in let* src=parseOperand g.(2) in Ok (Mov (dest,src)));
  arguments "Store" [[lazyText];[any]],(fun g->let* addr=parseOperand g.(1) in match addr with StackSlot offset->let* src=parseRegister g.(2) in Ok (Store (offset,src))|_->Error "Store expects a Stack slot as the first operand");
  binary "Add" (fun d l r->Add (d,l,r));binary "Sub" (fun d l r->Sub (d,l,r));
  multiply "Mul" (fun d l r->Mul (d,l,r));multiply "Sdiv" (fun d l r->Sdiv (d,l,r));
  assignment "AndImm" [[lazyText];[signedDigits]],(fun g->let* dest=parseRegister g.(1) in let* src=parseRegister g.(2) in let* imm=parseInt64Field "AND immediate" g.(3) in Ok (And_imm (dest,src,imm)));
  assignment "Lsl_imm" [[lazyText];[Alternatives [[literal "#"];[]];digits]],(fun g->let* dest=parseRegister g.(1) in let* src=parseRegister g.(2) in let text=g.(3) in let text=if String.starts_with ~prefix:"#" text then String.sub text 1 (String.length text-1) else text in let* shift=parseInt32Field "left shift immediate" text in Ok (Lsl_imm (dest,src,shift)));
  [Capture [lazyText];space;literal "<-";space;Capture [Alternatives [[literal "Madd"];[literal "Msub"]]];literal "(";Capture [lazyText];literal ",";space;Capture [lazyText];literal ",";space;Capture [any];literal ")"],(fun g->let* dest=parseRegister g.(1) in let* left=parseRegister g.(3) in let* right=parseRegister g.(4) in let* addend=parseRegister g.(5) in Ok (if g.(2)="Madd" then Madd (dest,left,right,addend) else Msub (dest,left,right,addend)))
 ] in
 let rec choose=function []->Error (Printf.sprintf "Line %d: Invalid instruction format '%s'" lineNum line)| (pattern,build)::rest->match matched pattern line with None->choose rest|Some groups->Result.map_error prefix (Result.map (fun instr->Instruction instr) (build groups)) in choose cases
(*
   Parse LIR program from text
   Parses flat instruction list and wraps in a single-block CFG
   Parse all instructions/terminators
   Build single-block CFG
*)
let parseLIR text=
 let lines=String.split_on_char '\n' text |> List.mapi (fun index line->index+1,Text.trim line) |> List.filter (fun (_,line)->line<>"" && not (String.starts_with ~prefix:"//" line)) in
 let rec parseLines acc=function []->Ok (List.rev acc)|(number,line)::rest->let* result=parseInstructionOrTerminator number line in parseLines (result::acc) rest in
 let* parsed=parseLines [] lines in
 let rec splitInstructions=function
 |[]->Error "Empty LIR program"
 |[Instruction _]->Error "LIR program must end with an explicit terminator"
 |[Terminator term]->Ok ([],term)
 |Instruction instr::rest->let* instrs,terminator=splitInstructions rest in Ok (instr::instrs,terminator)
 |Terminator _::_->Error "LIR terminator must be the final line" in
 let* instrs,terminator=splitInstructions parsed in
 let entryLabel=Label "entry" in
 let block={label=entryLabel;instrs;terminator} in
 let cfg={entry=entryLabel;blocks=LabelMap.singleton entryLabel block} in
 let func={id=TestIds.functionIdForName "_start";name="_start";typedParams=[];cfg;stackSize=0;usedCalleeSaved=[];codegenFacts=None} in
 Ok (Program ([func],StringOrder.Map.empty,StringOrder.Map.empty))
