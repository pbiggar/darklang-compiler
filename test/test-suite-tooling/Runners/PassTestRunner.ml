(*
   PassTestRunner.ml - Test runner for compiler pass tests
   Loads pass test files (e.g., MIR→LIR tests), runs the compiler pass,
   and compares the output with expected results.
   Pass tests are active; MIR pretty-printing reflects CFG structure for diagnostics.
   Result of running a pass test
*)
(* Execute the frozen pass fixture boundaries and preserve whole-program comparison. *)
[@@@warning "-4-42"]

open Dark_compiler

type passTestResult = TestOutcome.t = {
  success : bool;
  message : string;
  expected : string option;
  actual : string option;
}

let ( let* ) = Result.bind

(*
   Pretty-print MIR program with shared formatter
*)
let prettyPrintMIR = MIRPrinter.formatMIR

(*
   Pretty-print LIR program with shared formatter
*)
let prettyPrintLIR = LIRPrinter.formatLIR

(*
   Pretty-print ANF program with shared formatter
*)
let prettyPrintANF = ANFPrinter.formatANF

(*
   Rename all functions in an LIR program
*)
let renameLIRFunctions name (LIR.Program (functions, variants, records)) =
  LIR.Program
    ( List.map (fun (f : LIR.functionDef) -> { f with LIR.name }) functions,
      variants,
      records )

let success =
  { success = true; message = "Test passed"; expected = None; actual = None }

let failure message expected actual =
  { success = false; message; expected = Some expected; actual }

let listEqual eq left right =
  List.length left = List.length right && List.for_all2 eq left right

let equalMIR (MIR.Program (left, lv, lr)) (MIR.Program (right, rv, rr)) =
  let equalFunction (a : MIR.functionDef) (b : MIR.functionDef) =
    a.MIR.id = b.MIR.id && a.MIR.name = b.MIR.name
    && a.MIR.typedParams = b.MIR.typedParams
    && a.MIR.returnType = b.MIR.returnType
    && a.MIR.cfg.MIR.entry = b.MIR.cfg.MIR.entry
    && MIR.LabelMap.equal ( = ) a.MIR.cfg.MIR.blocks b.MIR.cfg.MIR.blocks
    && MIR.IntSet.equal a.MIR.floatRegs b.MIR.floatRegs
  in
  listEqual equalFunction left right
  && StringOrder.Map.equal ( = ) lv rv
  && StringOrder.Map.equal ( = ) lr rr

let equalLIR (LIR.Program (left, lv, lr)) (LIR.Program (right, rv, rr)) =
  let equalFunction (a : LIR.functionDef) (b : LIR.functionDef) =
    a.LIR.id = b.LIR.id && a.LIR.name = b.LIR.name
    && a.LIR.typedParams = b.LIR.typedParams
    && a.LIR.cfg.LIR.entry = b.LIR.cfg.LIR.entry
    && LIR.LabelMap.equal ( = ) a.LIR.cfg.LIR.blocks b.LIR.cfg.LIR.blocks
    && a.LIR.stackSize = b.LIR.stackSize
    && a.LIR.usedCalleeSaved = b.LIR.usedCalleeSaved
    && a.LIR.codegenFacts = b.LIR.codegenFacts
  in
  listEqual equalFunction left right
  && StringOrder.Map.equal ( = ) lv rv
  && StringOrder.Map.equal ( = ) lr rr

let loadPair path inputSection parseInput outputSection parseOutput =
  if not (TestFileIO.exists path) then Error ("Test file not found: " ^ path)
  else
    let file = Common.parseTestFile (FileIO.readText path) in
    let* inputText = Common.getRequiredSection inputSection file in
    let* input =
      Result.map_error
        (fun e -> "Failed to parse " ^ inputSection ^ ": " ^ e)
        (parseInput inputText)
    in
    let* outputText = Common.getRequiredSection outputSection file in
    let* output =
      Result.map_error
        (fun e -> "Failed to parse " ^ outputSection ^ ": " ^ e)
        (parseOutput outputText)
    in
    Ok (input, output)

(*
   Load MIR→LIR test from file
*)
let loadMIR2LIRTest path =
  loadPair path "INPUT-MIR" MIRParser.parseMIR "OUTPUT-LIR" LIRParser.parseLIR

(*
   Run MIR→LIR test
*)
let runMIR2LIRTest input expected =
  match MIR_to_LIR.toLIR input with
  | Error err ->
      failure ("LIR conversion error: " ^ err) (prettyPrintLIR expected) None
  | Ok actual ->
      if equalLIR actual expected then success
      else
        failure "Output mismatch" (prettyPrintLIR expected)
          (Some (prettyPrintLIR actual))

(*
   Load ANF→MIR test from file
*)
let loadANF2MIRTest path =
  loadPair path "INPUT-ANF" ANFParser.parseANF "OUTPUT-MIR"
    (MIRParser.parseMIRWithEntryLabel "_start_body")

(*
   Run ANF→MIR test
   Pass-test ANF DSL is int-only, so map all TempIds to TInt64.
*)
let runANF2MIRTest input expected =
  let maxId = ANF_to_MIR.maxTempIdInProgram input in
  let typeMap =
    ANF.TypeMap.ofSeq
      (List.init (max 0 (maxId + 1)) (fun id -> (ANF.TempId id, AST.TInt64))
      |> List.to_seq)
  in
  let (ANF.Program (functions, _)) = input in
  let functionNames =
    List.map (fun (f : ANF.functionDef) -> (f.ANF.id, f.ANF.name)) functions
    |> FunctionIdMap.ofList
    |> FunctionIdMap.add (AST.functionId 0L) "_start"
  in
  match
    ANF_to_MIR.toMIR input typeMap StringOrder.Map.empty AST.TInt64
      StringOrder.Map.empty StringOrder.Map.empty false FunctionIdMap.empty
      functionNames
  with
  | Error err ->
      failure ("MIR conversion error: " ^ err) (prettyPrintMIR expected) None
  | Ok actual ->
      if equalMIR actual expected then success
      else
        failure "Output mismatch" (prettyPrintMIR expected)
          (Some (prettyPrintMIR actual))

(*
   Pretty-print ARM64 register
*)
let prettyPrintARM64Reg = function
  | ARM64.X0 -> "X0"
  | ARM64.X1 -> "X1"
  | ARM64.X2 -> "X2"
  | ARM64.X3 -> "X3"
  | ARM64.X4 -> "X4"
  | ARM64.X5 -> "X5"
  | ARM64.X6 -> "X6"
  | ARM64.X7 -> "X7"
  | ARM64.X8 -> "X8"
  | ARM64.X9 -> "X9"
  | ARM64.X10 -> "X10"
  | ARM64.X11 -> "X11"
  | ARM64.X12 -> "X12"
  | ARM64.X13 -> "X13"
  | ARM64.X14 -> "X14"
  | ARM64.X15 -> "X15"
  | ARM64.X16 -> "X16"
  | ARM64.X17 -> "X17"
  | ARM64.X18 -> "X18"
  | ARM64.X19 -> "X19"
  | ARM64.X20 -> "X20"
  | ARM64.X21 -> "X21"
  | ARM64.X22 -> "X22"
  | ARM64.X23 -> "X23"
  | ARM64.X24 -> "X24"
  | ARM64.X25 -> "X25"
  | ARM64.X26 -> "X26"
  | ARM64.X27 -> "X27"
  | ARM64.X28 -> "X28"
  | ARM64.X29 -> "X29"
  | ARM64.X30 -> "X30"
  | ARM64.SP -> "SP"

let prettyPrintFReg = function
  | ARM64.D0 -> "D0"
  | ARM64.D1 -> "D1"
  | ARM64.D2 -> "D2"
  | ARM64.D3 -> "D3"
  | ARM64.D4 -> "D4"
  | ARM64.D5 -> "D5"
  | ARM64.D6 -> "D6"
  | ARM64.D7 -> "D7"
  | ARM64.D8 -> "D8"
  | ARM64.D9 -> "D9"
  | ARM64.D10 -> "D10"
  | ARM64.D11 -> "D11"
  | ARM64.D12 -> "D12"
  | ARM64.D13 -> "D13"
  | ARM64.D14 -> "D14"
  | ARM64.D15 -> "D15"
  | ARM64.D16 -> "D16"
  | ARM64.D17 -> "D17"
  | ARM64.D18 -> "D18"
  | ARM64.D19 -> "D19"
  | ARM64.D20 -> "D20"
  | ARM64.D21 -> "D21"
  | ARM64.D22 -> "D22"
  | ARM64.D23 -> "D23"
  | ARM64.D24 -> "D24"
  | ARM64.D25 -> "D25"
  | ARM64.D26 -> "D26"
  | ARM64.D27 -> "D27"
  | ARM64.D28 -> "D28"
  | ARM64.D29 -> "D29"
  | ARM64.D30 -> "D30"
  | ARM64.D31 -> "D31"

let prettyPrintCondition = function
  | ARM64.EQ -> "EQ"
  | ARM64.NE -> "NE"
  | ARM64.LT -> "LT"
  | ARM64.GT -> "GT"
  | ARM64.LE -> "LE"
  | ARM64.GE -> "GE"
  | ARM64.LO -> "LO"
  | ARM64.HI -> "HI"
  | ARM64.LS -> "LS"
  | ARM64.HS -> "HS"

let prettyPrintExtend = function
  | ARM64.ExtendUXTB -> "ExtendUXTB"
  | ARM64.ExtendUXTH -> "ExtendUXTH"
  | ARM64.ExtendUXTW -> "ExtendUXTW"
  | ARM64.ExtendSXTB -> "ExtendSXTB"
  | ARM64.ExtendSXTH -> "ExtendSXTH"
  | ARM64.ExtendSXTW -> "ExtendSXTW"

let escapeLabel text =
  let buffer = Buffer.create (String.length text) in
  String.iter
    (fun c ->
      if Char.code c = 92 || Char.code c = 34 then
        Buffer.add_char buffer (Char.chr 92);
      Buffer.add_char buffer c)
    text;
  Buffer.contents buffer

(*
   Pretty-print label references (code/data)
*)
let prettyPrintLabelRef = function
  | Symbolic.CodeLabel name -> name
  | Symbolic.DataLabel (Symbolic.Named name) -> "data:" ^ name
  | Symbolic.DataLabel (Symbolic.StringLiteral value) ->
      "str:\"" ^ escapeLabel value ^ "\""
  | Symbolic.DataLabel (Symbolic.FloatLiteral value) ->
      "float:" ^ FloatFormat.roundTrip value

(*
   Pretty-print ARM64 instruction
   Floating-point instructions
*)
let prettyPrintARM64Instr = function
  | Symbolic.MOVZ (dest, imm, shift) ->
      String.concat ""
        [
          "MOVZ(";
          prettyPrintARM64Reg dest;
          ", ";
          string_of_int imm;
          ", ";
          string_of_int shift;
          ")";
        ]
  | Symbolic.MOVN (dest, imm, shift) ->
      String.concat ""
        [
          "MOVN(";
          prettyPrintARM64Reg dest;
          ", ";
          string_of_int imm;
          ", ";
          string_of_int shift;
          ")";
        ]
  | Symbolic.MOVK (dest, imm, shift) ->
      String.concat ""
        [
          "MOVK(";
          prettyPrintARM64Reg dest;
          ", ";
          string_of_int imm;
          ", ";
          string_of_int shift;
          ")";
        ]
  | Symbolic.ADD_imm (dest, src, imm) ->
      String.concat ""
        [
          "ADD_imm(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src;
          ", ";
          string_of_int imm;
          ")";
        ]
  | Symbolic.ADD_reg (dest, src1, src2) ->
      String.concat ""
        [
          "ADD_reg(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src1;
          ", ";
          prettyPrintARM64Reg src2;
          ")";
        ]
  | Symbolic.ADD_shifted (dest, src1, src2, shift) ->
      String.concat ""
        [
          "ADD_shifted(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src1;
          ", ";
          prettyPrintARM64Reg src2;
          ", LSL #";
          string_of_int shift;
          ")";
        ]
  | Symbolic.ADD_extended (dest, src1, src2, extend) ->
      String.concat ""
        [
          "ADD_extended(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src1;
          ", ";
          prettyPrintARM64Reg src2;
          ", ";
          prettyPrintExtend extend;
          ")";
        ]
  | Symbolic.SUB_imm (dest, src, imm) ->
      String.concat ""
        [
          "SUB_imm(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src;
          ", ";
          string_of_int imm;
          ")";
        ]
  | Symbolic.SUB_imm12 (dest, src, imm) ->
      String.concat ""
        [
          "SUB_imm12(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src;
          ", ";
          string_of_int imm;
          ")";
        ]
  | Symbolic.SUB_reg (dest, src1, src2) ->
      String.concat ""
        [
          "SUB_reg(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src1;
          ", ";
          prettyPrintARM64Reg src2;
          ")";
        ]
  | Symbolic.SUB_shifted (dest, src1, src2, shift) ->
      String.concat ""
        [
          "SUB_shifted(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src1;
          ", ";
          prettyPrintARM64Reg src2;
          ", LSL #";
          string_of_int shift;
          ")";
        ]
  | Symbolic.SUB_extended (dest, src1, src2, extend) ->
      String.concat ""
        [
          "SUB_extended(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src1;
          ", ";
          prettyPrintARM64Reg src2;
          ", ";
          prettyPrintExtend extend;
          ")";
        ]
  | Symbolic.SUBS_imm (dest, src, imm) ->
      String.concat ""
        [
          "SUBS_imm(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src;
          ", ";
          string_of_int imm;
          ")";
        ]
  | Symbolic.MUL (dest, src1, src2) ->
      String.concat ""
        [
          "MUL(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src1;
          ", ";
          prettyPrintARM64Reg src2;
          ")";
        ]
  | Symbolic.SDIV (dest, src1, src2) ->
      String.concat ""
        [
          "SDIV(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src1;
          ", ";
          prettyPrintARM64Reg src2;
          ")";
        ]
  | Symbolic.UDIV (dest, src1, src2) ->
      String.concat ""
        [
          "UDIV(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src1;
          ", ";
          prettyPrintARM64Reg src2;
          ")";
        ]
  | Symbolic.MSUB (dest, src1, src2, src3) ->
      String.concat ""
        [
          "MSUB(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src1;
          ", ";
          prettyPrintARM64Reg src2;
          ", ";
          prettyPrintARM64Reg src3;
          ")";
        ]
  | Symbolic.MADD (dest, src1, src2, src3) ->
      String.concat ""
        [
          "MADD(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src1;
          ", ";
          prettyPrintARM64Reg src2;
          ", ";
          prettyPrintARM64Reg src3;
          ")";
        ]
  | Symbolic.MOV_reg (dest, src) ->
      String.concat ""
        [
          "MOV_reg(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src;
          ")";
        ]
  | Symbolic.STRB (src, addr, offset) ->
      String.concat ""
        [
          "STRB(";
          prettyPrintARM64Reg src;
          ", ";
          prettyPrintARM64Reg addr;
          ", ";
          string_of_int offset;
          ")";
        ]
  | Symbolic.LDRB (dest, baseAddr, index) ->
      String.concat ""
        [
          "LDRB(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg baseAddr;
          ", ";
          prettyPrintARM64Reg index;
          ")";
        ]
  | Symbolic.LDRB_imm (dest, baseAddr, offset) ->
      String.concat ""
        [
          "LDRB_imm(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg baseAddr;
          ", ";
          string_of_int offset;
          ")";
        ]
  | Symbolic.STRB_reg (src, addr) ->
      String.concat ""
        [
          "STRB_reg(";
          prettyPrintARM64Reg src;
          ", ";
          prettyPrintARM64Reg addr;
          ")";
        ]
  | Symbolic.CMP_imm (src, imm) ->
      String.concat ""
        [ "CMP_imm("; prettyPrintARM64Reg src; ", "; string_of_int imm; ")" ]
  | Symbolic.CMP_reg (src1, src2) ->
      String.concat ""
        [
          "CMP_reg(";
          prettyPrintARM64Reg src1;
          ", ";
          prettyPrintARM64Reg src2;
          ")";
        ]
  | Symbolic.CSET (dest, cond) ->
      String.concat ""
        [
          "CSET(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintCondition cond;
          ")";
        ]
  | Symbolic.CSEL (dest, whenTrue, whenFalse, cond) ->
      String.concat ""
        [
          "CSEL(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg whenTrue;
          ", ";
          prettyPrintARM64Reg whenFalse;
          ", ";
          prettyPrintCondition cond;
          ")";
        ]
  | Symbolic.AND_reg (dest, src1, src2) ->
      String.concat ""
        [
          "AND_reg(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src1;
          ", ";
          prettyPrintARM64Reg src2;
          ")";
        ]
  | Symbolic.BIC_reg (dest, src1, src2) ->
      String.concat ""
        [
          "BIC_reg(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src1;
          ", ";
          prettyPrintARM64Reg src2;
          ")";
        ]
  | Symbolic.AND_imm (dest, src, imm) ->
      String.concat ""
        [
          "AND_imm(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src;
          ", #";
          Printf.sprintf "%Lu" imm;
          ")";
        ]
  | Symbolic.ORR_reg (dest, src1, src2) ->
      String.concat ""
        [
          "ORR_reg(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src1;
          ", ";
          prettyPrintARM64Reg src2;
          ")";
        ]
  | Symbolic.EOR_reg (dest, src1, src2) ->
      String.concat ""
        [
          "EOR_reg(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src1;
          ", ";
          prettyPrintARM64Reg src2;
          ")";
        ]
  | Symbolic.LSL_reg (dest, src, shift) ->
      String.concat ""
        [
          "LSL_reg(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src;
          ", ";
          prettyPrintARM64Reg shift;
          ")";
        ]
  | Symbolic.LSR_reg (dest, src, shift) ->
      String.concat ""
        [
          "LSR_reg(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src;
          ", ";
          prettyPrintARM64Reg shift;
          ")";
        ]
  | Symbolic.ASR_reg (dest, src, shift) ->
      String.concat ""
        [
          "ASR_reg(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src;
          ", ";
          prettyPrintARM64Reg shift;
          ")";
        ]
  | Symbolic.LSL_imm (dest, src, shift) ->
      String.concat ""
        [
          "LSL_imm(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src;
          ", #";
          string_of_int shift;
          ")";
        ]
  | Symbolic.LSR_imm (dest, src, shift) ->
      String.concat ""
        [
          "LSR_imm(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src;
          ", #";
          string_of_int shift;
          ")";
        ]
  | Symbolic.ASR_imm (dest, src, shift) ->
      String.concat ""
        [
          "ASR_imm(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src;
          ", #";
          string_of_int shift;
          ")";
        ]
  | Symbolic.MVN (dest, src) ->
      String.concat ""
        [ "MVN("; prettyPrintARM64Reg dest; ", "; prettyPrintARM64Reg src; ")" ]
  | Symbolic.SXTB (dest, src) ->
      String.concat ""
        [
          "SXTB("; prettyPrintARM64Reg dest; ", "; prettyPrintARM64Reg src; ")";
        ]
  | Symbolic.SXTH (dest, src) ->
      String.concat ""
        [
          "SXTH("; prettyPrintARM64Reg dest; ", "; prettyPrintARM64Reg src; ")";
        ]
  | Symbolic.SXTW (dest, src) ->
      String.concat ""
        [
          "SXTW("; prettyPrintARM64Reg dest; ", "; prettyPrintARM64Reg src; ")";
        ]
  | Symbolic.UXTB (dest, src) ->
      String.concat ""
        [
          "UXTB("; prettyPrintARM64Reg dest; ", "; prettyPrintARM64Reg src; ")";
        ]
  | Symbolic.UXTH (dest, src) ->
      String.concat ""
        [
          "UXTH("; prettyPrintARM64Reg dest; ", "; prettyPrintARM64Reg src; ")";
        ]
  | Symbolic.UXTW (dest, src) ->
      String.concat ""
        [
          "UXTW("; prettyPrintARM64Reg dest; ", "; prettyPrintARM64Reg src; ")";
        ]
  | Symbolic.CBZ (reg, label) ->
      String.concat "" [ "CBZ("; prettyPrintARM64Reg reg; ", "; label; ")" ]
  | Symbolic.CBZ_offset (reg, offset) ->
      String.concat ""
        [
          "CBZ_offset(";
          prettyPrintARM64Reg reg;
          ", ";
          string_of_int offset;
          ")";
        ]
  | Symbolic.CBNZ (reg, label) ->
      String.concat "" [ "CBNZ("; prettyPrintARM64Reg reg; ", "; label; ")" ]
  | Symbolic.CBNZ_offset (reg, offset) ->
      String.concat ""
        [
          "CBNZ_offset(";
          prettyPrintARM64Reg reg;
          ", ";
          string_of_int offset;
          ")";
        ]
  | Symbolic.TBZ (reg, bit, offset) ->
      String.concat ""
        [
          "TBZ(";
          prettyPrintARM64Reg reg;
          ", ";
          string_of_int bit;
          ", ";
          string_of_int offset;
          ")";
        ]
  | Symbolic.TBNZ (reg, bit, offset) ->
      String.concat ""
        [
          "TBNZ(";
          prettyPrintARM64Reg reg;
          ", ";
          string_of_int bit;
          ", ";
          string_of_int offset;
          ")";
        ]
  | Symbolic.TBZ_label (reg, bit, label) ->
      String.concat ""
        [
          "TBZ_label(";
          prettyPrintARM64Reg reg;
          ", ";
          string_of_int bit;
          ", ";
          label;
          ")";
        ]
  | Symbolic.TBNZ_label (reg, bit, label) ->
      String.concat ""
        [
          "TBNZ_label(";
          prettyPrintARM64Reg reg;
          ", ";
          string_of_int bit;
          ", ";
          label;
          ")";
        ]
  | Symbolic.B offset -> String.concat "" [ "B("; string_of_int offset; ")" ]
  | Symbolic.B_cond (cond, offset) ->
      String.concat ""
        [
          "B_cond("; prettyPrintCondition cond; ", "; string_of_int offset; ")";
        ]
  | Symbolic.B_label label -> String.concat "" [ "B_label("; label; ")" ]
  | Symbolic.B_cond_label (cond, label) ->
      String.concat ""
        [ "B_cond_label("; prettyPrintCondition cond; ", "; label; ")" ]
  | Symbolic.NEG (dest, src) ->
      String.concat ""
        [ "NEG("; prettyPrintARM64Reg dest; ", "; prettyPrintARM64Reg src; ")" ]
  | Symbolic.STP (reg1, reg2, addr, offset) ->
      String.concat ""
        [
          "STP(";
          prettyPrintARM64Reg reg1;
          ", ";
          prettyPrintARM64Reg reg2;
          ", ";
          prettyPrintARM64Reg addr;
          ", ";
          string_of_int offset;
          ")";
        ]
  | Symbolic.STP_pre (reg1, reg2, addr, offset) ->
      String.concat ""
        [
          "STP_pre(";
          prettyPrintARM64Reg reg1;
          ", ";
          prettyPrintARM64Reg reg2;
          ", ";
          prettyPrintARM64Reg addr;
          ", ";
          string_of_int offset;
          ")";
        ]
  | Symbolic.LDP (reg1, reg2, addr, offset) ->
      String.concat ""
        [
          "LDP(";
          prettyPrintARM64Reg reg1;
          ", ";
          prettyPrintARM64Reg reg2;
          ", ";
          prettyPrintARM64Reg addr;
          ", ";
          string_of_int offset;
          ")";
        ]
  | Symbolic.LDP_post (reg1, reg2, addr, offset) ->
      String.concat ""
        [
          "LDP_post(";
          prettyPrintARM64Reg reg1;
          ", ";
          prettyPrintARM64Reg reg2;
          ", ";
          prettyPrintARM64Reg addr;
          ", ";
          string_of_int offset;
          ")";
        ]
  | Symbolic.STR (src, addr, offset) ->
      String.concat ""
        [
          "STR(";
          prettyPrintARM64Reg src;
          ", ";
          prettyPrintARM64Reg addr;
          ", ";
          string_of_int offset;
          ")";
        ]
  | Symbolic.LDR (dest, addr, offset) ->
      String.concat ""
        [
          "LDR(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg addr;
          ", ";
          string_of_int offset;
          ")";
        ]
  | Symbolic.STUR (src, addr, offset) ->
      String.concat ""
        [
          "STUR(";
          prettyPrintARM64Reg src;
          ", ";
          prettyPrintARM64Reg addr;
          ", ";
          string_of_int offset;
          ")";
        ]
  | Symbolic.LDUR (dest, addr, offset) ->
      String.concat ""
        [
          "LDUR(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg addr;
          ", ";
          string_of_int offset;
          ")";
        ]
  | Symbolic.BL label -> String.concat "" [ "BL("; label; ")" ]
  | Symbolic.BLR reg ->
      String.concat "" [ "BLR("; prettyPrintARM64Reg reg; ")" ]
  | Symbolic.RET -> String.concat "" [ "RET" ]
  | Symbolic.SVC imm -> String.concat "" [ "SVC("; string_of_int imm; ")" ]
  | Symbolic.Label label -> String.concat "" [ "Label("; label; ")" ]
  | Symbolic.ADRP (dest, label) ->
      String.concat ""
        [
          "ADRP(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintLabelRef label;
          ")";
        ]
  | Symbolic.ADD_label (dest, src, label) ->
      String.concat ""
        [
          "ADD_label(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintARM64Reg src;
          ", ";
          prettyPrintLabelRef label;
          ")";
        ]
  | Symbolic.ADR (dest, label) ->
      String.concat ""
        [
          "ADR("; prettyPrintARM64Reg dest; ", "; prettyPrintLabelRef label; ")";
        ]
  | Symbolic.LDR_fp (dest, addr, offset) ->
      String.concat ""
        [
          "LDR_fp(";
          prettyPrintFReg dest;
          ", ";
          prettyPrintARM64Reg addr;
          ", ";
          string_of_int offset;
          ")";
        ]
  | Symbolic.STR_fp (src, addr, offset) ->
      String.concat ""
        [
          "STR_fp(";
          prettyPrintFReg src;
          ", ";
          prettyPrintARM64Reg addr;
          ", ";
          string_of_int offset;
          ")";
        ]
  | Symbolic.STP_fp (freg1, freg2, addr, offset) ->
      String.concat ""
        [
          "STP_fp(";
          prettyPrintFReg freg1;
          ", ";
          prettyPrintFReg freg2;
          ", ";
          prettyPrintARM64Reg addr;
          ", ";
          string_of_int offset;
          ")";
        ]
  | Symbolic.LDP_fp (freg1, freg2, addr, offset) ->
      String.concat ""
        [
          "LDP_fp(";
          prettyPrintFReg freg1;
          ", ";
          prettyPrintFReg freg2;
          ", ";
          prettyPrintARM64Reg addr;
          ", ";
          string_of_int offset;
          ")";
        ]
  | Symbolic.FADD (dest, src1, src2) ->
      String.concat ""
        [
          "FADD(";
          prettyPrintFReg dest;
          ", ";
          prettyPrintFReg src1;
          ", ";
          prettyPrintFReg src2;
          ")";
        ]
  | Symbolic.FSUB (dest, src1, src2) ->
      String.concat ""
        [
          "FSUB(";
          prettyPrintFReg dest;
          ", ";
          prettyPrintFReg src1;
          ", ";
          prettyPrintFReg src2;
          ")";
        ]
  | Symbolic.FMUL (dest, src1, src2) ->
      String.concat ""
        [
          "FMUL(";
          prettyPrintFReg dest;
          ", ";
          prettyPrintFReg src1;
          ", ";
          prettyPrintFReg src2;
          ")";
        ]
  | Symbolic.FMADD (dest, src1, src2, addend) ->
      String.concat ""
        [
          "FMADD(";
          prettyPrintFReg dest;
          ", ";
          prettyPrintFReg src1;
          ", ";
          prettyPrintFReg src2;
          ", ";
          prettyPrintFReg addend;
          ")";
        ]
  | Symbolic.FDIV (dest, src1, src2) ->
      String.concat ""
        [
          "FDIV(";
          prettyPrintFReg dest;
          ", ";
          prettyPrintFReg src1;
          ", ";
          prettyPrintFReg src2;
          ")";
        ]
  | Symbolic.FNEG (dest, src) ->
      String.concat ""
        [ "FNEG("; prettyPrintFReg dest; ", "; prettyPrintFReg src; ")" ]
  | Symbolic.FABS (dest, src) ->
      String.concat ""
        [ "FABS("; prettyPrintFReg dest; ", "; prettyPrintFReg src; ")" ]
  | Symbolic.FCMP (src1, src2) ->
      String.concat ""
        [ "FCMP("; prettyPrintFReg src1; ", "; prettyPrintFReg src2; ")" ]
  | Symbolic.FMOV_reg (dest, src) ->
      String.concat ""
        [ "FMOV_reg("; prettyPrintFReg dest; ", "; prettyPrintFReg src; ")" ]
  | Symbolic.FMOV_imm (dest, value) ->
      String.concat ""
        [
          "FMOV_imm(";
          prettyPrintFReg dest;
          ", ";
          FloatFormat.roundTrip value;
          ")";
        ]
  | Symbolic.FMOV_zero dest ->
      String.concat "" [ "FMOV_zero("; prettyPrintFReg dest; ")" ]
  | Symbolic.FMOV_to_gp (dest, src) ->
      String.concat ""
        [
          "FMOV_to_gp(";
          prettyPrintARM64Reg dest;
          ", ";
          prettyPrintFReg src;
          ")";
        ]
  | Symbolic.FMOV_from_gp (dest, src) ->
      String.concat ""
        [
          "FMOV_from_gp(";
          prettyPrintFReg dest;
          ", ";
          prettyPrintARM64Reg src;
          ")";
        ]
  | Symbolic.CNT_8B (dest, src) ->
      String.concat ""
        [ "CNT_8B("; prettyPrintFReg dest; ", "; prettyPrintFReg src; ")" ]
  | Symbolic.ADDV_8B (dest, src) ->
      String.concat ""
        [ "ADDV_8B("; prettyPrintFReg dest; ", "; prettyPrintFReg src; ")" ]
  | Symbolic.UMOV_byte (dest, src) ->
      String.concat ""
        [
          "UMOV_byte("; prettyPrintARM64Reg dest; ", "; prettyPrintFReg src; ")";
        ]
  | Symbolic.FSQRT (dest, src) ->
      String.concat ""
        [ "FSQRT("; prettyPrintFReg dest; ", "; prettyPrintFReg src; ")" ]
  | Symbolic.SCVTF (dest, src) ->
      String.concat ""
        [ "SCVTF("; prettyPrintFReg dest; ", "; prettyPrintARM64Reg src; ")" ]
  | Symbolic.FCVTZS (dest, src) ->
      String.concat ""
        [ "FCVTZS("; prettyPrintARM64Reg dest; ", "; prettyPrintFReg src; ")" ]
  | Symbolic.BR reg -> String.concat "" [ "BR("; prettyPrintARM64Reg reg; ")" ]

(*
   Pretty-print ARM64 program (filtering out Label pseudo-instructions)
*)
let prettyPrintARM64 instrs =
  List.filter (function Symbolic.Label _ -> false | _ -> true) instrs
  |> List.map prettyPrintARM64Instr
  |> String.concat "\n"

(*
   Load LIR→ARM64 test from file
*)
let loadLIR2ARM64Test path =
  Result.map
    (fun (input, expected) -> (renameLIRFunctions "test" input, expected))
    (loadPair path "INPUT-LIR" LIRParser.parseLIR "OUTPUT-ARM64"
       ARM64SymbolicParser.parseARM64Symbolic)

(*
   Run LIR→ARM64 test
   Filter out Label pseudo-instructions for comparison
*)
let runLIR2ARM64Test input expected =
  let target = ARM64.targetConfigFor Platform.LinuxARM64 in
  let prepared = ARM64PrepareFunctions.prepareARM64Program input in
  match Backend_Arm64_CodeGen.generateARM64 target prepared with
  | Error err ->
      failure
        ("Code generation failed: " ^ err)
        (prettyPrintARM64 expected)
        None
  | Ok program ->
      let actualRaw =
        Backend_Arm64_CodeGen.generatedProgramInstructions program
      in
      let filter =
        List.filter (function Symbolic.Label _ -> false | _ -> true)
      in
      if filter actualRaw = filter expected then success
      else
        failure "Output mismatch"
          (prettyPrintARM64 expected)
          (Some (prettyPrintARM64 actualRaw))
