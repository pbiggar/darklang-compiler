(*
   ARM64Parser.fs - Parser for ARM64 instruction DSL
   Parses human-readable ARM64 text into ARM64.Instr data structures.
   Example ARM64:
   MOVZ(X1, 10, 0)
   SUB_imm(X1, X1, 3)
   MOV_reg(X0, X1)
   RET
*)
[@@@warning "-4"]
open Dark_compiler
open ARM64
(*
   Parse ARM64 register from text like "X0", "X1", etc.
*)
let parseReg text=match HostText.trim text with
 | "X0" -> Ok X0
 | "X1" -> Ok X1
 | "X2" -> Ok X2
 | "X3" -> Ok X3
 | "X4" -> Ok X4
 | "X5" -> Ok X5
 | "X6" -> Ok X6
 | "X7" -> Ok X7
 | "X8" -> Ok X8
 | "X9" -> Ok X9
 | "X10" -> Ok X10
 | "X11" -> Ok X11
 | "X12" -> Ok X12
 | "X13" -> Ok X13
 | "X14" -> Ok X14
 | "X15" -> Ok X15
 | "X16" -> Ok X16
 | "X17" -> Ok X17
 | "X19" -> Ok X19
 | "X20" -> Ok X20
 | "X21" -> Ok X21
 | "X22" -> Ok X22
 | "X23" -> Ok X23
 | "X24" -> Ok X24
 | "X25" -> Ok X25
 | "X26" -> Ok X26
 | "X27" -> Ok X27
 | "X28" -> Ok X28
 | "X29" -> Ok X29
 | "X30" -> Ok X30
 | "SP" -> Ok SP
 | reg -> Error ("Invalid ARM64 register '"^reg^"'")
(*
   Parse ARM64 condition from text like "EQ", "NE", etc.
*)
let parseCond text=match HostText.trim text with
 | "EQ" -> Ok EQ | "NE" -> Ok NE | "LT" -> Ok LT | "GT" -> Ok GT | "LE" -> Ok LE | "GE" -> Ok GE
 | cond -> Error ("Invalid ARM64 condition '"^cond^"'")
let invalid lineNum fieldName text=Error (Printf.sprintf "Line %d: Invalid %s '%s'" lineNum fieldName text)
let parseUInt16Operand lineNum fieldName text=match HostText.tryParseInt32 (HostText.trim text) with Some value when Int32.compare value 0l>=0 && Int32.compare value 65535l<=0 -> Ok (Int32.to_int value) | _ -> invalid lineNum fieldName text
let parseUInt12Operand lineNum fieldName text=match parseUInt16Operand lineNum fieldName text with Ok value when value<=4095 -> Ok value | Ok _ -> invalid lineNum fieldName text | Error e -> Error e
let parseInt16Operand lineNum fieldName text=match HostText.tryParseInt32 (HostText.trim text) with Some value when Int32.compare value (-32768l)>=0 && Int32.compare value 32767l<=0 -> Ok (Int32.to_int value) | _ -> invalid lineNum fieldName text
let parseIntOperand lineNum fieldName text=match HostText.tryParseInt32 (HostText.trim text) with Some value -> Ok (Int32.to_int value) | None -> invalid lineNum fieldName text
(* Interpret the reference's anchored instruction regexes with their original
   lazy dot captures, Unicode digit/space classes and greedy whitespace. *)
type capture = Any | Digits | SignedDigits
type patternToken = Literal of int | Space | Capture of capture
let isSpace unit=Uchar.is_valid unit && Uucp.White.is_white_space (Uchar.of_int unit)
let isDigit=HostText.isDigit
let matched prefix fields line =
 let text=HostText.scalars line in let length=Array.length text in
 let prefix=Array.to_list (HostText.scalars (prefix^"(")) |> List.map (fun c -> Literal c) in
 let rec operands=function [] -> [Literal 41] | [kind] -> [Capture kind;Literal 41] | kind::rest -> Capture kind::Literal 44::Space::operands rest in
 let tokens=prefix@operands fields in
 let rec matchTokens tokens at captures = match tokens with
 | [] -> if at=length || (at=length-1 && text.(at)=10) then Some (Array.of_list (line::List.rev captures)) else None
 | Literal c::rest -> if at<length && text.(at)=c then matchTokens rest (at+1) captures else None
 | Space::rest ->
 let finish=ref at in while !finish<length && isSpace text.(!finish) do incr finish done;
 let rec tryLength n=if n<at then None else match matchTokens rest n captures with Some _ as found -> found | None -> tryLength (n-1) in tryLength !finish
 | Capture kind::rest ->
 let start=if kind=SignedDigits && at<length && text.(at)=45 then at+1 else at in
 let finish=ref start in while !finish<length && (match kind with Any -> text.(!finish)<>10 | Digits | SignedDigits -> isDigit text.(!finish)) do incr finish done;
 let captureUntil n=HostText.ofScalars (Array.sub text at (n-at)) in
 let rec tryLazy n=if n> !finish then None else match matchTokens rest n (captureUntil n::captures) with Some _ as found -> found | None -> tryLazy (n+1) in
 let rec tryGreedy n=if n<=start then None else match matchTokens rest n (captureUntil n::captures) with Some _ as found -> found | None -> tryGreedy (n-1) in
 (match kind with Any -> tryLazy (at+1) | Digits | SignedDigits -> tryGreedy !finish) in
 matchTokens tokens 0 []
let movzMatch _allowInvalidEncodingValues lineNum groups =
(match parseReg groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok dest -> ((match parseUInt16Operand lineNum "MOVZ immediate" groups.(2), parseIntOperand lineNum "MOVZ shift" groups.(3) with
| Ok imm, Ok shift -> (Ok (MOVZ (dest, imm, shift)))
| Error e, _ -> (Error e)
| _, Error e -> (Error e))))
let movnMatch _allowInvalidEncodingValues lineNum groups =
(match parseReg groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok dest -> ((match parseUInt16Operand lineNum "MOVN immediate" groups.(2), parseIntOperand lineNum "MOVN shift" groups.(3) with
| Ok imm, Ok shift -> (Ok (MOVN (dest, imm, shift)))
| Error e, _ -> (Error e)
| _, Error e -> (Error e))))
let movkMatch _allowInvalidEncodingValues lineNum groups =
(match parseReg groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok dest -> ((match parseUInt16Operand lineNum "MOVK immediate" groups.(2), parseIntOperand lineNum "MOVK shift" groups.(3) with
| Ok imm, Ok shift -> (Ok (MOVK (dest, imm, shift)))
| Error e, _ -> (Error e)
| _, Error e -> (Error e))))
let addImmMatch allowInvalidEncodingValues lineNum groups =
(match parseReg groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok dest -> ((match parseReg groups.(2) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok src -> (let parseImmediate = (if allowInvalidEncodingValues then parseUInt16Operand else parseUInt12Operand) in
(match parseImmediate lineNum "ADD_imm immediate" groups.(3) with
| Error e -> (Error e)
| Ok imm -> (Ok (ADD_imm (dest, src, imm))))))))
let addRegMatch _allowInvalidEncodingValues lineNum groups =
(match parseReg groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok dest -> ((match parseReg groups.(2) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok src1 -> ((match parseReg groups.(3) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok src2 -> (Ok (ADD_reg (dest, src1, src2))))))))
let subImmMatch allowInvalidEncodingValues lineNum groups =
(match parseReg groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok dest -> ((match parseReg groups.(2) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok src -> (let parseImmediate = (if allowInvalidEncodingValues then parseUInt16Operand else parseUInt12Operand) in
(match parseImmediate lineNum "SUB_imm immediate" groups.(3) with
| Error e -> (Error e)
| Ok imm -> (Ok (SUB_imm (dest, src, imm))))))))
let subRegMatch _allowInvalidEncodingValues lineNum groups =
(match parseReg groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok dest -> ((match parseReg groups.(2) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok src1 -> ((match parseReg groups.(3) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok src2 -> (Ok (SUB_reg (dest, src1, src2))))))))
let mulMatch _allowInvalidEncodingValues lineNum groups =
(match parseReg groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok dest -> ((match parseReg groups.(2) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok src1 -> ((match parseReg groups.(3) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok src2 -> (Ok (MUL (dest, src1, src2))))))))
let sdivMatch _allowInvalidEncodingValues lineNum groups =
(match parseReg groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok dest -> ((match parseReg groups.(2) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok src1 -> ((match parseReg groups.(3) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok src2 -> (Ok (SDIV (dest, src1, src2))))))))
let udivMatch _allowInvalidEncodingValues lineNum groups =
(match parseReg groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok dest -> ((match parseReg groups.(2) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok src1 -> ((match parseReg groups.(3) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok src2 -> (Ok (UDIV (dest, src1, src2))))))))
let movMatch _allowInvalidEncodingValues lineNum groups =
(match parseReg groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok dest -> ((match parseReg groups.(2) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok src -> (Ok (MOV_reg (dest, src))))))
let svcMatch _allowInvalidEncodingValues lineNum groups =
(match parseUInt16Operand lineNum "SVC immediate" groups.(1) with
| Error e -> (Error e)
| Ok imm -> (Ok (SVC imm)))
let stpMatch _allowInvalidEncodingValues lineNum groups =
(match parseReg groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok reg1 -> ((match parseReg groups.(2) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok reg2 -> ((match parseReg groups.(3) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok addr -> ((match parseInt16Operand lineNum "STP offset" groups.(4) with
| Error e -> (Error e)
| Ok offset -> (Ok (STP (reg1, reg2, addr, offset))))))))))
let stpPreMatch _allowInvalidEncodingValues lineNum groups =
(match parseReg groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok reg1 -> ((match parseReg groups.(2) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok reg2 -> ((match parseReg groups.(3) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok addr -> ((match parseInt16Operand lineNum "STP_pre offset" groups.(4) with
| Error e -> (Error e)
| Ok offset -> (Ok (STP_pre (reg1, reg2, addr, offset))))))))))
let ldpMatch _allowInvalidEncodingValues lineNum groups =
(match parseReg groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok reg1 -> ((match parseReg groups.(2) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok reg2 -> ((match parseReg groups.(3) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok addr -> ((match parseInt16Operand lineNum "LDP offset" groups.(4) with
| Error e -> (Error e)
| Ok offset -> (Ok (LDP (reg1, reg2, addr, offset))))))))))
let ldpPostMatch _allowInvalidEncodingValues lineNum groups =
(match parseReg groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok reg1 -> ((match parseReg groups.(2) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok reg2 -> ((match parseReg groups.(3) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok addr -> ((match parseInt16Operand lineNum "LDP_post offset" groups.(4) with
| Error e -> (Error e)
| Ok offset -> (Ok (LDP_post (reg1, reg2, addr, offset))))))))))
let strMatch _allowInvalidEncodingValues lineNum groups =
(match parseReg groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok src -> ((match parseReg groups.(2) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok addr -> ((match parseInt16Operand lineNum "STR offset" groups.(3) with
| Error e -> (Error e)
| Ok offset -> (Ok (STR (src, addr, offset))))))))
let sturMatch _allowInvalidEncodingValues lineNum groups =
(match parseReg groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok src -> ((match parseReg groups.(2) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok addr -> ((match parseInt16Operand lineNum "STUR offset" groups.(3) with
| Error e -> (Error e)
| Ok offset -> (Ok (STUR (src, addr, offset))))))))
let ldrMatch _allowInvalidEncodingValues lineNum groups =
(match parseReg groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok dest -> ((match parseReg groups.(2) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok addr -> ((match parseInt16Operand lineNum "LDR offset" groups.(3) with
| Error e -> (Error e)
| Ok offset -> (Ok (LDR (dest, addr, offset))))))))
let ldurMatch _allowInvalidEncodingValues lineNum groups =
(match parseReg groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok dest -> ((match parseReg groups.(2) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok addr -> ((match parseInt16Operand lineNum "LDUR offset" groups.(3) with
| Error e -> (Error e)
| Ok offset -> (Ok (LDUR (dest, addr, offset))))))))
let bLabelMatch _allowInvalidEncodingValues _lineNum groups =
let label = (groups.(1)) in
Ok (B_label label)
let bCondLabelMatch _allowInvalidEncodingValues lineNum groups =
(match parseCond groups.(1) with
| Error e -> (Error (Printf.sprintf "Line %d: %s" (lineNum) (e)))
| Ok cond -> (let label = (groups.(2)) in
Ok (B_cond_label (cond, label))))
let blMatch _allowInvalidEncodingValues _lineNum groups =
let label = (groups.(1)) in
Ok (BL label)
(*
   Parse a single ARM64 instruction
   Try RET
   Try MOVZ: "MOVZ(X1, 10, 0)"
   Try MOVN: "MOVN(X1, 10, 0)"
   Try MOVK: "MOVK(X1, 10, 16)"
   Try ADD_imm: "ADD_imm(X1, X0, 5)"
   Try ADD_reg: "ADD_reg(X1, X0, X2)"
   Try SUB_imm: "SUB_imm(X1, X1, 3)"
   Try SUB_reg: "SUB_reg(X1, X0, X2)"
   Try MUL: "MUL(X1, X0, X2)"
   Try SDIV: "SDIV(X1, X0, X2)"
   Try UDIV: "UDIV(X1, X0, X2)"
   Try MOV_reg: "MOV_reg(X0, X1)"
   Try SVC: "SVC(128)"
   Try STP: "STP(X29, X30, SP, -16)"
   Try STP_pre: "STP_pre(X29, X30, SP, -16)"
   Try LDP: "LDP(X29, X30, SP, 16)"
   Try LDP_post: "LDP_post(X29, X30, SP, 16)"
   Try STR: "STR(X0, SP, 8)"
   Try STUR: "STUR(X0, X29, -8)"
   Try LDR: "LDR(X0, SP, 8)"
   Try LDUR: "LDUR(X0, X29, -8)"
   Try B_label: "B_label(_epilogue_test)"
   Try B_cond_label: "B_cond_label(EQ, label)"
   Try BL: "BL(label)"
*)
let parseInstructionWithMode allowInvalidEncodingValues lineNum line =
 let line=HostText.trim line in if line="RET" then Ok RET else
 let rec first=function [] -> Error (Printf.sprintf "Line %d: Invalid ARM64 instruction format '%s'" lineNum line) | matcher::rest -> match matcher () with None -> first rest | Some value -> value in
 first [(fun () -> match matched "MOVZ" [Any;Digits;Digits] line with None -> None | Some groups -> Some (movzMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "MOVN" [Any;Digits;Digits] line with None -> None | Some groups -> Some (movnMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "MOVK" [Any;Digits;Digits] line with None -> None | Some groups -> Some (movkMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "ADD_imm" [Any;Any;Digits] line with None -> None | Some groups -> Some (addImmMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "ADD_reg" [Any;Any;Any] line with None -> None | Some groups -> Some (addRegMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "SUB_imm" [Any;Any;Digits] line with None -> None | Some groups -> Some (subImmMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "SUB_reg" [Any;Any;Any] line with None -> None | Some groups -> Some (subRegMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "MUL" [Any;Any;Any] line with None -> None | Some groups -> Some (mulMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "SDIV" [Any;Any;Any] line with None -> None | Some groups -> Some (sdivMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "UDIV" [Any;Any;Any] line with None -> None | Some groups -> Some (udivMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "MOV_reg" [Any;Any] line with None -> None | Some groups -> Some (movMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "SVC" [Digits] line with None -> None | Some groups -> Some (svcMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "STP" [Any;Any;Any;SignedDigits] line with None -> None | Some groups -> Some (stpMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "STP_pre" [Any;Any;Any;SignedDigits] line with None -> None | Some groups -> Some (stpPreMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "LDP" [Any;Any;Any;SignedDigits] line with None -> None | Some groups -> Some (ldpMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "LDP_post" [Any;Any;Any;SignedDigits] line with None -> None | Some groups -> Some (ldpPostMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "STR" [Any;Any;SignedDigits] line with None -> None | Some groups -> Some (strMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "STUR" [Any;Any;SignedDigits] line with None -> None | Some groups -> Some (sturMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "LDR" [Any;Any;SignedDigits] line with None -> None | Some groups -> Some (ldrMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "LDUR" [Any;Any;SignedDigits] line with None -> None | Some groups -> Some (ldurMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "B_label" [Any] line with None -> None | Some groups -> Some (bLabelMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "B_cond_label" [Any;Any] line with None -> None | Some groups -> Some (bCondLabelMatch allowInvalidEncodingValues lineNum groups));
(fun () -> match matched "BL" [Any] line with None -> None | Some groups -> Some (blMatch allowInvalidEncodingValues lineNum groups))]
let parseInstruction lineNum line=parseInstructionWithMode false lineNum line
let parseARM64WithMode allowInvalidEncodingValues text=Common.stripCommentsAndEmpty text |> List.mapi (fun i line -> parseInstructionWithMode allowInvalidEncodingValues (i+1) line) |> ResultList.sequenceResults
(*
   Parse an ARM64 program, validating numeric constraints represented by the DSL.
*)
let parseARM64 text=parseARM64WithMode false text
(*
   Parse an ARM64 encoding input while retaining values the encoder must reject.
*)
let parseARM64ForEncodingError text=parseARM64WithMode true text
