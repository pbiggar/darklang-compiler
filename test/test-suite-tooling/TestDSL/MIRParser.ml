(*
   MIRParser.fs - Parser for MIR (Mid-level IR) DSL
   Parses human-readable MIR text into MIR.Program data structures.
   Example MIR:
   v0 <- 42
   v1 <- v0 + 3
   ret v1
*)
(* Preserve flat MIR instructions, terminator placement and source diagnostic precedence. *)
[@@@warning "-42"]
open Dark_compiler
open MIR
open DSLPattern
type instructionOrTerminator=Instruction of instr | Terminator of terminator
let (let*)=Result.bind
(*
   Parse virtual register from text like "v123"
*)
let parseVReg text=
 let invalid ()=Error ("Invalid register format '"^text^"' (expected 'v0', 'v1', etc.)") in
 match matched [literal "v";Capture [digits]] (Text.trim text) with
 |Some groups->(match Text.tryParseInt32 groups.(1) with Some id->Ok (VReg (Int32.to_int id))|None->invalid ())|None->invalid ()
(*
   Parse operand (either a number or a register)
   Try parsing as register first
   Try parsing as integer
*)
let parseOperand text=
 let text=Text.trim text in
 match parseVReg text with Ok reg->Ok (Register reg)|Error _->
 match int64 text with Some n->Ok (Int64Const n)|None->Error ("Invalid operand '"^text^"' (expected number or register)")
(*
   Parse binary operator
*)
let parseOp text=match Text.trim text with
 |"+"->Ok Add|"-"->Ok Sub|"*"->Ok Mul|"/"->Ok Div|"=="->Ok Eq|"!="->Ok Neq|"<"->Ok Lt|">"->Ok Gt|"<="->Ok Lte|">="->Ok Gte|"&&"->Ok And|"||"->Ok Or|op->Error ("Unknown operator '"^op^"'")
(*
   Parse a single MIR instruction or terminator
   Returns either an Instr or a Terminator
   Try ret pattern: "ret <operand>"
   Try binop pattern: "v0 <- v1 + 3"
   Try move pattern: "v0 <- 42" or "v0 <- v1"
*)
let parseInstructionOrTerminator lineNum line=
 let line=Text.trim line in
 let prefix e=Printf.sprintf "Line %d: %s" lineNum e in
 let operand=Alternatives [[literal "v";digits];[literal "-";digits];[digits]] in
 match matched [literal "ret";spaces;Capture [any]] line with
 |Some groups->Result.map_error prefix (Result.map (fun operand->Terminator (Ret operand)) (parseOperand groups.(1)))
 |None->match matched [Capture [literal "v";digits];space;literal "<-";space;Capture [operand];space;Capture [Alternatives (List.map (fun op->[literal op]) ["==";"!=";"<=";">=";"&&";"||";"+";"-";"*";"/";"<";">"])];space;Capture [operand]] line with
 |Some groups->Result.map_error prefix (let* dest=parseVReg groups.(1) in let* left=parseOperand groups.(2) in let* op=parseOp groups.(3) in let* right=parseOperand groups.(4) in Ok (Instruction (BinOp (dest,op,left,right,AST.TInt64))))
 |None->match matched [Capture [literal "v";digits];space;literal "<-";space;Capture [any]] line with
 |Some groups->Result.map_error prefix (let* dest=parseVReg groups.(1) in let* src=parseOperand groups.(2) in Ok (Instruction (Mov (dest,src,None))))
 |None->Error (Printf.sprintf "Line %d: Invalid instruction format '%s'" lineNum line)
(*
   Parse MIR program from text with a specific entry label
   Parses flat instruction list and wraps in a single-block CFG
   Parse all instructions/terminators
   Split into instructions and terminator
   The last item should be a terminator (Ret)
   Extract instructions from Choice1Of2
   Check that all non-last items are instructions
   Build single-block CFG
*)
let parseMIRWithEntryLabel entryLabelName text=
 let lines=String.split_on_char '\n' text |> List.map Text.trim |> List.filter (fun line->line<>"" && not (String.starts_with ~prefix:"//" line)) in
 let rec parseLines lineNum acc=function []->Ok (List.rev acc)|line::rest->let* result=parseInstructionOrTerminator lineNum line in parseLines (lineNum+1) (result::acc) rest in
 let* parsed=parseLines 1 [] lines in
 match List.rev parsed with
 |[]->Error "Empty MIR program"
 |Instruction _::_->Error "Last instruction must be a terminator (ret)"
 |Terminator terminator::reversed->
  let instructions=List.rev reversed in
  let instrs=List.filter_map (function Instruction instr->Some instr|Terminator _->None) instructions in
  if List.length instrs<>List.length instructions then Error "Only the last line can be a terminator (ret)" else
  let entryLabel=Label entryLabelName in
  let block={label=entryLabel;instrs;terminator} in
  let cfg={entry=entryLabel;blocks=LabelMap.singleton entryLabel block} in
  let func={id=TestIds.functionIdForName "_start";name="_start";typedParams=[];returnType=AST.TInt64;cfg;floatRegs=IntSet.empty} in
  Ok (Program ([func],StringOrder.Map.empty,StringOrder.Map.empty))
(*
   Parse MIR program from text
   Parses flat instruction list and wraps in a single-block CFG
*)
let parseMIR text=parseMIRWithEntryLabel "entry" text
