(*
   ARM64SymbolicParser.ml - Parser for symbolic ARM64 instruction DSL
   Parses human-readable ARM64 text into ARM64Symbolic.Instr data structures.
   Example ARM64:
   MOVZ(X1, 10, 0)
   SUB_imm(X1, X1, 3)
   MOV_reg(X0, X1)
   RET
*)
(* Retain symbolic operands, literal-pool references and original field validation order. *)
[@@@warning "-4"]

open Dark_compiler
open Symbolic
open DSLPattern

let ( let* ) = Result.bind

(*
   Parse ARM64 register from text like "X0", "X1", etc.
*)
let parseReg = ARM64Parser.parseReg

(*
   Parse ARM64 condition from text like "EQ", "NE", etc.
*)
let parseCond = ARM64Parser.parseCond

let invalid lineNum fieldName text =
  Error (Printf.sprintf "Line %d: Invalid %s '%s'" lineNum fieldName text)

let parseUInt16Operand lineNum fieldName text =
  match integer 16 false (Text.trim text) with
  | Some value -> Ok (Z.to_int value)
  | None -> invalid lineNum fieldName text

let parseUInt12Operand lineNum fieldName text =
  let* value = parseUInt16Operand lineNum fieldName text in
  if value <= 4095 then Ok value else invalid lineNum fieldName text

let parseInt16Operand lineNum fieldName text =
  match integer 16 true (Text.trim text) with
  | Some value -> Ok (Z.to_int value)
  | None -> invalid lineNum fieldName text

let parseIntOperand lineNum fieldName text =
  match Text.tryParseInt32 (Text.trim text) with
  | Some value -> Ok (Int32.to_int value)
  | None -> invalid lineNum fieldName text

let replace old replacement text =
  let size = String.length old in
  let length = String.length text in
  let buffer = Buffer.create length in
  let rec loop index =
    if index < length then
      if index + size <= length && String.sub text index size = old then (
        Buffer.add_string buffer replacement;
        loop (index + size))
      else (
        Buffer.add_char buffer text.[index];
        loop (index + 1))
  in
  loop 0;
  Buffer.contents buffer

let parseFloat text =
  let special = Text.lowerInvariant (Text.trim text) in
  match special with
  | "nan" | "+nan" | "-nan" -> Some (Int64.float_of_bits 0xfff8000000000000L)
  | "infinity" | "+infinity" -> Some infinity
  | "-infinity" -> Some neg_infinity
  | _ -> (
      let length = String.length text in
      let rec withoutNuls n =
        if n > 0 && text.[n - 1] = '\000' then withoutNuls (n - 1) else n
      in
      let length = withoutNuls length in
      let white = function
        | ' ' | '\t' | '\n' | '\r' | '\011' | '\012' -> true
        | _ -> false
      in
      let rec spaces index =
        if index < length && white text.[index] then spaces (index + 1)
        else index
      in
      let sign index =
        if index < length && (text.[index] = '+' || text.[index] = '-') then
          index + 1
        else index
      in
      let rec digits index =
        if index < length && text.[index] >= '0' && text.[index] <= '9' then
          digits (index + 1)
        else index
      in
      let first = spaces 0 in
      let beginning = sign first in
      let afterWhole = digits beginning in
      let afterFraction =
        if afterWhole < length && text.[afterWhole] = '.' then
          digits (afterWhole + 1)
        else afterWhole
      in
      let hasDigits =
        afterWhole > beginning || afterFraction > afterWhole + 1
      in
      let afterExponent =
        if
          afterFraction < length
          && (text.[afterFraction] = 'e' || text.[afterFraction] = 'E')
        then
          let firstDigit = sign (afterFraction + 1) in
          let lastDigit = digits firstDigit in
          if lastDigit = firstDigit then None else Some lastDigit
        else Some afterFraction
      in
      match afterExponent with
      | Some ending when hasDigits && spaces ending = length ->
          float_of_string_opt (String.sub text first (ending - first))
      | _ -> None)

(*
   Parse symbolic label reference
*)
let parseLabelRef text =
  let trimmed = Text.trim text in
  if Text.startsWith trimmed "data:" then
    Ok (DataLabel (Named (String.sub trimmed 5 (String.length trimmed - 5))))
  else if
    Text.startsWith trimmed "str:\"" && String.ends_with ~suffix:"\"" trimmed
  then
    let inner = String.sub trimmed 5 (String.length trimmed - 6) in
    Ok
      (DataLabel
         (StringLiteral (replace "\\\"" "\"" (replace "\\\\" "\\" inner))))
  else if Text.startsWith trimmed "float:" then
    let valueText = String.sub trimmed 6 (String.length trimmed - 6) in
    match parseFloat valueText with
    | Some value -> Ok (DataLabel (FloatLiteral value))
    | None -> Error ("Invalid float literal '" ^ valueText ^ "'")
  else Ok (CodeLabel trimmed)

let lazyText = Repeat (Dot, 1, false)

let call name fields =
  let rec joined = function
    | [] -> []
    | [ field ] -> [ Capture field ]
    | field :: rest -> Capture field :: literal "," :: space :: joined rest
  in
  (literal (name ^ "(") :: joined fields) @ [ literal ")" ]

let signedDigits = Alternatives [ [ literal "-"; digits ]; [ digits ] ]

(*
   Parse a single ARM64 instruction
   Try RET
   Try Label: "Label(done)"
   Try MOVZ: "MOVZ(X1, 10, 0)"
   Try MOVN: "MOVN(X1, 0, 0)"
   Try MOVK: "MOVK(X1, 10, 16)"
   Try ADD_imm: "ADD_imm(X1, X0, 5)"
   Try ADD_reg: "ADD_reg(X1, X0, X2)"
   Try SUB_imm: "SUB_imm(X1, X1, 3)"
   Try SUBS_imm: "SUBS_imm(X1, X1, 3)"
   Try CMP_imm: "CMP_imm(X1, 0)"
   Try SUB_reg: "SUB_reg(X1, X0, X2)"
   Try the three-register logical instructions used by peephole fixtures.
   Try MUL: "MUL(X1, X0, X2)"
   Try SDIV: "SDIV(X1, X0, X2)"
   Try UDIV: "UDIV(X1, X0, X2)"
   Try MOV_reg: "MOV_reg(X0, X1)"
   Try ADRP: "ADRP(X0, label)"
   Try ADD_label: "ADD_label(X0, X1, label)"
   Try ADR: "ADR(X0, label)"
   Try SVC: "SVC(128)"
   Try STP: "STP(X29, X30, SP, -16)"
   Try STP_pre: "STP_pre(X29, X30, SP, -16)"
   Try LDP: "LDP(X29, X30, SP, 16)"
   Try LDP_post: "LDP_post(X29, X30, SP, 16)"
   Try STR: "STR(X0, SP, 8)"
   Try LDR: "LDR(X0, SP, 8)"
   Try STUR: "STUR(X0, X29, -8)"
   Try LDUR: "LDUR(X0, X29, -8)"
   Try CBZ: "CBZ(X0, label)"
   Try CBNZ: "CBNZ(X0, label)"
   Try B_label: "B_label(done)"
   Try B_cond_label: "B_cond_label(EQ, label)"
   Try B: "B(12)"
   Try B_cond: "B_cond(NE, -4)"
*)
let parseInstruction lineNum line =
  let line = Text.trim line in
  let prefix error = Printf.sprintf "Line %d: %s" lineNum error in
  let reg text = Result.map_error prefix (parseReg text) in
  let cond text = Result.map_error prefix (parseCond text) in
  let label text = Result.map_error prefix (parseLabelRef text) in
  let three name construct =
    ( call name [ [ lazyText ]; [ lazyText ]; [ lazyText ] ],
      fun g ->
        let* dest = reg g.(1) in
        let* left = reg g.(2) in
        let* right = reg g.(3) in
        Ok (construct dest left right) )
  in
  let immediate name parseRegister construct =
    ( call name [ [ lazyText ]; [ lazyText ]; [ digits ] ],
      fun g ->
        let* dest = parseRegister g.(1) in
        let* src = parseRegister g.(2) in
        let* imm = parseUInt12Operand lineNum (name ^ " immediate") g.(3) in
        Ok (construct dest src imm) )
  in
  let wideMove name parseDestination construct =
    ( call name [ [ lazyText ]; [ digits ]; [ digits ] ],
      fun g ->
        let* dest = parseDestination g.(1) in
        let* imm = parseUInt16Operand lineNum (name ^ " immediate") g.(2) in
        let* shift = parseIntOperand lineNum (name ^ " shift") g.(3) in
        Ok (construct dest imm shift) )
  in
  let pair name construct =
    ( call name [ [ lazyText ]; [ lazyText ]; [ lazyText ]; [ signedDigits ] ],
      fun g ->
        let* a = reg g.(1) in
        let* b = reg g.(2) in
        let* addr = reg g.(3) in
        let* offset = parseInt16Operand lineNum (name ^ " offset") g.(4) in
        Ok (construct a b addr offset) )
  in
  let memory name construct =
    ( call name [ [ lazyText ]; [ lazyText ]; [ signedDigits ] ],
      fun g ->
        let* a = reg g.(1) in
        let* addr = reg g.(2) in
        let* offset = parseInt16Operand lineNum (name ^ " offset") g.(3) in
        Ok (construct a addr offset) )
  in
  if line = "RET" then Ok RET
  else
    let cases =
      [
        (call "Label" [ [ lazyText ] ], fun g -> Ok (Label g.(1)));
        wideMove "MOVZ" reg (fun d i s -> MOVZ (d, i, s));
        wideMove "MOVN" parseReg (fun d i s -> MOVN (d, i, s));
        wideMove "MOVK" reg (fun d i s -> MOVK (d, i, s));
        immediate "ADD_imm" reg (fun d s i -> ADD_imm (d, s, i));
        three "ADD_reg" (fun d l r -> ADD_reg (d, l, r));
        immediate "SUB_imm" reg (fun d s i -> SUB_imm (d, s, i));
        immediate "SUBS_imm" parseReg (fun d s i -> SUBS_imm (d, s, i));
        ( call "CMP_imm" [ [ lazyText ]; [ digits ] ],
          fun g ->
            let* src = parseReg g.(1) in
            let* imm = parseUInt12Operand lineNum "CMP_imm immediate" g.(2) in
            Ok (CMP_imm (src, imm)) );
        three "SUB_reg" (fun d l r -> SUB_reg (d, l, r));
        three "AND_reg" (fun d l r -> AND_reg (d, l, r));
        three "ORR_reg" (fun d l r -> ORR_reg (d, l, r));
        three "EOR_reg" (fun d l r -> EOR_reg (d, l, r));
        three "BIC_reg" (fun d l r -> BIC_reg (d, l, r));
        three "MUL" (fun d l r -> MUL (d, l, r));
        three "SDIV" (fun d l r -> SDIV (d, l, r));
        three "UDIV" (fun d l r -> UDIV (d, l, r));
        ( call "MOV_reg" [ [ lazyText ]; [ lazyText ] ],
          fun g ->
            let* dest = reg g.(1) in
            let* src = reg g.(2) in
            Ok (MOV_reg (dest, src)) );
        ( call "ADRP" [ [ lazyText ]; [ any ] ],
          fun g ->
            let* dest = reg g.(1) in
            let* value = label g.(2) in
            Ok (ADRP (dest, value)) );
        ( call "ADD_label" [ [ lazyText ]; [ lazyText ]; [ any ] ],
          fun g ->
            let* dest = reg g.(1) in
            let* src = reg g.(2) in
            let* value = label g.(3) in
            Ok (ADD_label (dest, src, value)) );
        ( call "ADR" [ [ lazyText ]; [ any ] ],
          fun g ->
            let* dest = reg g.(1) in
            let* value = label g.(2) in
            Ok (ADR (dest, value)) );
        ( call "SVC" [ [ digits ] ],
          fun g ->
            Result.map
              (fun i -> SVC i)
              (parseUInt16Operand lineNum "SVC immediate" g.(1)) );
        pair "STP" (fun a b r o -> STP (a, b, r, o));
        pair "STP_pre" (fun a b r o -> STP_pre (a, b, r, o));
        pair "LDP" (fun a b r o -> LDP (a, b, r, o));
        pair "LDP_post" (fun a b r o -> LDP_post (a, b, r, o));
        memory "STR" (fun a r o -> STR (a, r, o));
        memory "LDR" (fun a r o -> LDR (a, r, o));
        memory "STUR" (fun a r o -> STUR (a, r, o));
        memory "LDUR" (fun a r o -> LDUR (a, r, o));
        ( call "CBZ" [ [ lazyText ]; [ lazyText ] ],
          fun g ->
            let* value = reg g.(1) in
            Ok (CBZ (value, g.(2))) );
        ( call "CBNZ" [ [ lazyText ]; [ lazyText ] ],
          fun g ->
            let* value = reg g.(1) in
            Ok (CBNZ (value, g.(2))) );
        (call "B_label" [ [ lazyText ] ], fun g -> Ok (B_label g.(1)));
        ( call "B_cond_label" [ [ lazyText ]; [ lazyText ] ],
          fun g ->
            let* condition = cond g.(1) in
            Ok (B_cond_label (condition, g.(2))) );
        ( call "B" [ [ signedDigits ] ],
          fun g ->
            Result.map
              (fun offset -> B offset)
              (parseIntOperand lineNum "B offset" g.(1)) );
        ( call "B_cond" [ [ lazyText ]; [ signedDigits ] ],
          fun g ->
            let* condition = cond g.(1) in
            let* offset = parseIntOperand lineNum "B_cond offset" g.(2) in
            Ok (B_cond (condition, offset)) );
      ]
    in
    let rec choose = function
      | [] ->
          Error
            (Printf.sprintf "Line %d: Invalid instruction format '%s'" lineNum
               line)
      | (pattern, build) :: rest -> (
          match matched pattern line with
          | Some g -> build g
          | None -> choose rest)
    in
    choose cases

(*
   Parse ARM64 program from text
*)
let parseARM64Symbolic text =
  let lines =
    String.split_on_char '\n' text
    |> List.mapi (fun i line -> (i + 1, Text.trim line))
    |> List.filter (fun (_, line) ->
        line <> "" && not (String.starts_with ~prefix:"//" line))
  in
  let rec parseLines acc = function
    | [] -> Ok (List.rev acc)
    | (lineNum, line) :: rest ->
        let* instr = parseInstruction lineNum line in
        parseLines (instr :: acc) rest
  in
  parseLines [] lines
