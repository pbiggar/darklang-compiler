(* Closed machine instruction observations for both native backends. *)
[@@@warning "-4"]
open Dark_compiler
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let scalar kind value=`Assoc ["kind",`String kind;"value",`String value]
let int32 value=scalar "int32" (Int32.to_string value)
let uint32 value=scalar "uint32" (Printf.sprintf "%lu" value)
let int64 value=scalar "int64" (Int64.to_string value)
let uint64 value=scalar "uint64" (Printf.sprintf "%Lu" value)
let float64 value=scalar "float64" (Printf.sprintf "%016Lx" (Int64.bits_of_float value))
let uint16 value=scalar "uint16" (string_of_int value)
let int16 value=scalar "int16" (string_of_int value)
let union=SemanticJson.union
let option f=function None -> union "FSharpOption" "None" [] | Some x -> union "FSharpOption" "Some" [f x]
let armReg (value:ARM64.reg) = match value with
 | ARM64.X0 -> union "Reg" "X0" []
 | ARM64.X1 -> union "Reg" "X1" []
 | ARM64.X2 -> union "Reg" "X2" []
 | ARM64.X3 -> union "Reg" "X3" []
 | ARM64.X4 -> union "Reg" "X4" []
 | ARM64.X5 -> union "Reg" "X5" []
 | ARM64.X6 -> union "Reg" "X6" []
 | ARM64.X7 -> union "Reg" "X7" []
 | ARM64.X8 -> union "Reg" "X8" []
 | ARM64.X9 -> union "Reg" "X9" []
 | ARM64.X10 -> union "Reg" "X10" []
 | ARM64.X11 -> union "Reg" "X11" []
 | ARM64.X12 -> union "Reg" "X12" []
 | ARM64.X13 -> union "Reg" "X13" []
 | ARM64.X14 -> union "Reg" "X14" []
 | ARM64.X15 -> union "Reg" "X15" []
 | ARM64.X16 -> union "Reg" "X16" []
 | ARM64.X17 -> union "Reg" "X17" []
 | ARM64.X18 -> union "Reg" "X18" []
 | ARM64.X19 -> union "Reg" "X19" []
 | ARM64.X20 -> union "Reg" "X20" []
 | ARM64.X21 -> union "Reg" "X21" []
 | ARM64.X22 -> union "Reg" "X22" []
 | ARM64.X23 -> union "Reg" "X23" []
 | ARM64.X24 -> union "Reg" "X24" []
 | ARM64.X25 -> union "Reg" "X25" []
 | ARM64.X26 -> union "Reg" "X26" []
 | ARM64.X27 -> union "Reg" "X27" []
 | ARM64.X28 -> union "Reg" "X28" []
 | ARM64.X29 -> union "Reg" "X29" []
 | ARM64.X30 -> union "Reg" "X30" []
 | ARM64.SP -> union "Reg" "SP" []
let armFReg (value:ARM64.fReg) = match value with
 | ARM64.D0 -> union "FReg" "D0" []
 | ARM64.D1 -> union "FReg" "D1" []
 | ARM64.D2 -> union "FReg" "D2" []
 | ARM64.D3 -> union "FReg" "D3" []
 | ARM64.D4 -> union "FReg" "D4" []
 | ARM64.D5 -> union "FReg" "D5" []
 | ARM64.D6 -> union "FReg" "D6" []
 | ARM64.D7 -> union "FReg" "D7" []
 | ARM64.D8 -> union "FReg" "D8" []
 | ARM64.D9 -> union "FReg" "D9" []
 | ARM64.D10 -> union "FReg" "D10" []
 | ARM64.D11 -> union "FReg" "D11" []
 | ARM64.D12 -> union "FReg" "D12" []
 | ARM64.D13 -> union "FReg" "D13" []
 | ARM64.D14 -> union "FReg" "D14" []
 | ARM64.D15 -> union "FReg" "D15" []
 | ARM64.D16 -> union "FReg" "D16" []
 | ARM64.D17 -> union "FReg" "D17" []
 | ARM64.D18 -> union "FReg" "D18" []
 | ARM64.D19 -> union "FReg" "D19" []
 | ARM64.D20 -> union "FReg" "D20" []
 | ARM64.D21 -> union "FReg" "D21" []
 | ARM64.D22 -> union "FReg" "D22" []
 | ARM64.D23 -> union "FReg" "D23" []
 | ARM64.D24 -> union "FReg" "D24" []
 | ARM64.D25 -> union "FReg" "D25" []
 | ARM64.D26 -> union "FReg" "D26" []
 | ARM64.D27 -> union "FReg" "D27" []
 | ARM64.D28 -> union "FReg" "D28" []
 | ARM64.D29 -> union "FReg" "D29" []
 | ARM64.D30 -> union "FReg" "D30" []
 | ARM64.D31 -> union "FReg" "D31" []
let armCondition (value:ARM64.condition) = match value with
 | ARM64.EQ -> union "Condition" "EQ" []
 | ARM64.NE -> union "Condition" "NE" []
 | ARM64.LT -> union "Condition" "LT" []
 | ARM64.GT -> union "Condition" "GT" []
 | ARM64.LE -> union "Condition" "LE" []
 | ARM64.GE -> union "Condition" "GE" []
 | ARM64.LO -> union "Condition" "LO" []
 | ARM64.HI -> union "Condition" "HI" []
 | ARM64.LS -> union "Condition" "LS" []
 | ARM64.HS -> union "Condition" "HS" []
let armExtend (value:ARM64.extend) = match value with
 | ARM64.ExtendUXTB -> union "Extend" "ExtendUXTB" []
 | ARM64.ExtendUXTH -> union "Extend" "ExtendUXTH" []
 | ARM64.ExtendUXTW -> union "Extend" "ExtendUXTW" []
 | ARM64.ExtendSXTB -> union "Extend" "ExtendSXTB" []
 | ARM64.ExtendSXTH -> union "Extend" "ExtendSXTH" []
 | ARM64.ExtendSXTW -> union "Extend" "ExtendSXTW" []
let armInstr (value:ARM64.instr) = match value with
 | ARM64.MOVZ (v0,v1,v2) -> union "Instr" "MOVZ" [armReg v0;uint16 v1;SemanticJson.int32 v2]
 | ARM64.MOVN (v0,v1,v2) -> union "Instr" "MOVN" [armReg v0;uint16 v1;SemanticJson.int32 v2]
 | ARM64.MOVK (v0,v1,v2) -> union "Instr" "MOVK" [armReg v0;uint16 v1;SemanticJson.int32 v2]
 | ARM64.ADD_imm (v0,v1,v2) -> union "Instr" "ADD_imm" [armReg v0;armReg v1;uint16 v2]
 | ARM64.ADD_reg (v0,v1,v2) -> union "Instr" "ADD_reg" [armReg v0;armReg v1;armReg v2]
 | ARM64.ADD_shifted (v0,v1,v2,v3) -> union "Instr" "ADD_shifted" [armReg v0;armReg v1;armReg v2;SemanticJson.int32 v3]
 | ARM64.ADD_extended (v0,v1,v2,v3) -> union "Instr" "ADD_extended" [armReg v0;armReg v1;armReg v2;armExtend v3]
 | ARM64.SUB_imm (v0,v1,v2) -> union "Instr" "SUB_imm" [armReg v0;armReg v1;uint16 v2]
 | ARM64.SUB_imm12 (v0,v1,v2) -> union "Instr" "SUB_imm12" [armReg v0;armReg v1;uint16 v2]
 | ARM64.SUB_reg (v0,v1,v2) -> union "Instr" "SUB_reg" [armReg v0;armReg v1;armReg v2]
 | ARM64.SUB_shifted (v0,v1,v2,v3) -> union "Instr" "SUB_shifted" [armReg v0;armReg v1;armReg v2;SemanticJson.int32 v3]
 | ARM64.SUB_extended (v0,v1,v2,v3) -> union "Instr" "SUB_extended" [armReg v0;armReg v1;armReg v2;armExtend v3]
 | ARM64.SUBS_imm (v0,v1,v2) -> union "Instr" "SUBS_imm" [armReg v0;armReg v1;uint16 v2]
 | ARM64.MUL (v0,v1,v2) -> union "Instr" "MUL" [armReg v0;armReg v1;armReg v2]
 | ARM64.SDIV (v0,v1,v2) -> union "Instr" "SDIV" [armReg v0;armReg v1;armReg v2]
 | ARM64.UDIV (v0,v1,v2) -> union "Instr" "UDIV" [armReg v0;armReg v1;armReg v2]
 | ARM64.MSUB (v0,v1,v2,v3) -> union "Instr" "MSUB" [armReg v0;armReg v1;armReg v2;armReg v3]
 | ARM64.MADD (v0,v1,v2,v3) -> union "Instr" "MADD" [armReg v0;armReg v1;armReg v2;armReg v3]
 | ARM64.CMP_imm (v0,v1) -> union "Instr" "CMP_imm" [armReg v0;uint16 v1]
 | ARM64.CMP_reg (v0,v1) -> union "Instr" "CMP_reg" [armReg v0;armReg v1]
 | ARM64.CSET (v0,v1) -> union "Instr" "CSET" [armReg v0;armCondition v1]
 | ARM64.CSEL (v0,v1,v2,v3) -> union "Instr" "CSEL" [armReg v0;armReg v1;armReg v2;armCondition v3]
 | ARM64.AND_reg (v0,v1,v2) -> union "Instr" "AND_reg" [armReg v0;armReg v1;armReg v2]
 | ARM64.BIC_reg (v0,v1,v2) -> union "Instr" "BIC_reg" [armReg v0;armReg v1;armReg v2]
 | ARM64.AND_imm (v0,v1,v2) -> union "Instr" "AND_imm" [armReg v0;armReg v1;uint64 v2]
 | ARM64.ORR_reg (v0,v1,v2) -> union "Instr" "ORR_reg" [armReg v0;armReg v1;armReg v2]
 | ARM64.EOR_reg (v0,v1,v2) -> union "Instr" "EOR_reg" [armReg v0;armReg v1;armReg v2]
 | ARM64.LSL_reg (v0,v1,v2) -> union "Instr" "LSL_reg" [armReg v0;armReg v1;armReg v2]
 | ARM64.LSR_reg (v0,v1,v2) -> union "Instr" "LSR_reg" [armReg v0;armReg v1;armReg v2]
 | ARM64.ASR_reg (v0,v1,v2) -> union "Instr" "ASR_reg" [armReg v0;armReg v1;armReg v2]
 | ARM64.LSL_imm (v0,v1,v2) -> union "Instr" "LSL_imm" [armReg v0;armReg v1;SemanticJson.int32 v2]
 | ARM64.LSR_imm (v0,v1,v2) -> union "Instr" "LSR_imm" [armReg v0;armReg v1;SemanticJson.int32 v2]
 | ARM64.ASR_imm (v0,v1,v2) -> union "Instr" "ASR_imm" [armReg v0;armReg v1;SemanticJson.int32 v2]
 | ARM64.MVN (v0,v1) -> union "Instr" "MVN" [armReg v0;armReg v1]
 | ARM64.MOV_reg (v0,v1) -> union "Instr" "MOV_reg" [armReg v0;armReg v1]
 | ARM64.STRB (v0,v1,v2) -> union "Instr" "STRB" [armReg v0;armReg v1;SemanticJson.int32 v2]
 | ARM64.LDRB (v0,v1,v2) -> union "Instr" "LDRB" [armReg v0;armReg v1;armReg v2]
 | ARM64.LDRB_imm (v0,v1,v2) -> union "Instr" "LDRB_imm" [armReg v0;armReg v1;SemanticJson.int32 v2]
 | ARM64.STRB_reg (v0,v1) -> union "Instr" "STRB_reg" [armReg v0;armReg v1]
 | ARM64.STP (v0,v1,v2,v3) -> union "Instr" "STP" [armReg v0;armReg v1;armReg v2;int16 v3]
 | ARM64.STP_pre (v0,v1,v2,v3) -> union "Instr" "STP_pre" [armReg v0;armReg v1;armReg v2;int16 v3]
 | ARM64.LDP (v0,v1,v2,v3) -> union "Instr" "LDP" [armReg v0;armReg v1;armReg v2;int16 v3]
 | ARM64.LDP_post (v0,v1,v2,v3) -> union "Instr" "LDP_post" [armReg v0;armReg v1;armReg v2;int16 v3]
 | ARM64.STR (v0,v1,v2) -> union "Instr" "STR" [armReg v0;armReg v1;int16 v2]
 | ARM64.LDR (v0,v1,v2) -> union "Instr" "LDR" [armReg v0;armReg v1;int16 v2]
 | ARM64.STUR (v0,v1,v2) -> union "Instr" "STUR" [armReg v0;armReg v1;int16 v2]
 | ARM64.LDUR (v0,v1,v2) -> union "Instr" "LDUR" [armReg v0;armReg v1;int16 v2]
 | ARM64.BL v0 -> union "Instr" "BL" [SemanticJson.string v0]
 | ARM64.BLR v0 -> union "Instr" "BLR" [armReg v0]
 | ARM64.BR v0 -> union "Instr" "BR" [armReg v0]
 | ARM64.CBZ (v0,v1) -> union "Instr" "CBZ" [armReg v0;SemanticJson.string v1]
 | ARM64.CBNZ (v0,v1) -> union "Instr" "CBNZ" [armReg v0;SemanticJson.string v1]
 | ARM64.B_label v0 -> union "Instr" "B_label" [SemanticJson.string v0]
 | ARM64.B_cond_label (v0,v1) -> union "Instr" "B_cond_label" [armCondition v0;SemanticJson.string v1]
 | ARM64.CBZ_offset (v0,v1) -> union "Instr" "CBZ_offset" [armReg v0;SemanticJson.int32 v1]
 | ARM64.CBNZ_offset (v0,v1) -> union "Instr" "CBNZ_offset" [armReg v0;SemanticJson.int32 v1]
 | ARM64.TBZ (v0,v1,v2) -> union "Instr" "TBZ" [armReg v0;SemanticJson.int32 v1;SemanticJson.int32 v2]
 | ARM64.TBNZ (v0,v1,v2) -> union "Instr" "TBNZ" [armReg v0;SemanticJson.int32 v1;SemanticJson.int32 v2]
 | ARM64.TBZ_label (v0,v1,v2) -> union "Instr" "TBZ_label" [armReg v0;SemanticJson.int32 v1;SemanticJson.string v2]
 | ARM64.TBNZ_label (v0,v1,v2) -> union "Instr" "TBNZ_label" [armReg v0;SemanticJson.int32 v1;SemanticJson.string v2]
 | ARM64.B v0 -> union "Instr" "B" [SemanticJson.int32 v0]
 | ARM64.B_cond (v0,v1) -> union "Instr" "B_cond" [armCondition v0;SemanticJson.int32 v1]
 | ARM64.NEG (v0,v1) -> union "Instr" "NEG" [armReg v0;armReg v1]
 | ARM64.RET -> union "Instr" "RET" []
 | ARM64.SVC v0 -> union "Instr" "SVC" [uint16 v0]
 | ARM64.Label v0 -> union "Instr" "Label" [SemanticJson.string v0]
 | ARM64.ADRP (v0,v1) -> union "Instr" "ADRP" [armReg v0;SemanticJson.string v1]
 | ARM64.ADD_label (v0,v1,v2) -> union "Instr" "ADD_label" [armReg v0;armReg v1;SemanticJson.string v2]
 | ARM64.ADR (v0,v1) -> union "Instr" "ADR" [armReg v0;SemanticJson.string v1]
 | ARM64.LDR_fp (v0,v1,v2) -> union "Instr" "LDR_fp" [armFReg v0;armReg v1;int16 v2]
 | ARM64.STR_fp (v0,v1,v2) -> union "Instr" "STR_fp" [armFReg v0;armReg v1;int16 v2]
 | ARM64.STP_fp (v0,v1,v2,v3) -> union "Instr" "STP_fp" [armFReg v0;armFReg v1;armReg v2;int16 v3]
 | ARM64.LDP_fp (v0,v1,v2,v3) -> union "Instr" "LDP_fp" [armFReg v0;armFReg v1;armReg v2;int16 v3]
 | ARM64.FADD (v0,v1,v2) -> union "Instr" "FADD" [armFReg v0;armFReg v1;armFReg v2]
 | ARM64.FSUB (v0,v1,v2) -> union "Instr" "FSUB" [armFReg v0;armFReg v1;armFReg v2]
 | ARM64.FMUL (v0,v1,v2) -> union "Instr" "FMUL" [armFReg v0;armFReg v1;armFReg v2]
 | ARM64.FMADD (v0,v1,v2,v3) -> union "Instr" "FMADD" [armFReg v0;armFReg v1;armFReg v2;armFReg v3]
 | ARM64.FDIV (v0,v1,v2) -> union "Instr" "FDIV" [armFReg v0;armFReg v1;armFReg v2]
 | ARM64.FNEG (v0,v1) -> union "Instr" "FNEG" [armFReg v0;armFReg v1]
 | ARM64.FABS (v0,v1) -> union "Instr" "FABS" [armFReg v0;armFReg v1]
 | ARM64.FSQRT (v0,v1) -> union "Instr" "FSQRT" [armFReg v0;armFReg v1]
 | ARM64.FCMP (v0,v1) -> union "Instr" "FCMP" [armFReg v0;armFReg v1]
 | ARM64.FMOV_reg (v0,v1) -> union "Instr" "FMOV_reg" [armFReg v0;armFReg v1]
 | ARM64.FMOV_imm (v0,v1) -> union "Instr" "FMOV_imm" [armFReg v0;float64 v1]
 | ARM64.FMOV_to_gp (v0,v1) -> union "Instr" "FMOV_to_gp" [armReg v0;armFReg v1]
 | ARM64.FMOV_from_gp (v0,v1) -> union "Instr" "FMOV_from_gp" [armFReg v0;armReg v1]
 | ARM64.SCVTF (v0,v1) -> union "Instr" "SCVTF" [armFReg v0;armReg v1]
 | ARM64.FCVTZS (v0,v1) -> union "Instr" "FCVTZS" [armReg v0;armFReg v1]
 | ARM64.SXTB (v0,v1) -> union "Instr" "SXTB" [armReg v0;armReg v1]
 | ARM64.SXTH (v0,v1) -> union "Instr" "SXTH" [armReg v0;armReg v1]
 | ARM64.SXTW (v0,v1) -> union "Instr" "SXTW" [armReg v0;armReg v1]
 | ARM64.UXTB (v0,v1) -> union "Instr" "UXTB" [armReg v0;armReg v1]
 | ARM64.UXTH (v0,v1) -> union "Instr" "UXTH" [armReg v0;armReg v1]
 | ARM64.UXTW (v0,v1) -> union "Instr" "UXTW" [armReg v0;armReg v1]
let x64Reg (value:X86_64.reg) = match value with
 | X86_64.RAX -> union "Reg" "RAX" []
 | X86_64.RBX -> union "Reg" "RBX" []
 | X86_64.RCX -> union "Reg" "RCX" []
 | X86_64.RDX -> union "Reg" "RDX" []
 | X86_64.RSI -> union "Reg" "RSI" []
 | X86_64.RDI -> union "Reg" "RDI" []
 | X86_64.RBP -> union "Reg" "RBP" []
 | X86_64.RSP -> union "Reg" "RSP" []
 | X86_64.R8 -> union "Reg" "R8" []
 | X86_64.R9 -> union "Reg" "R9" []
 | X86_64.R10 -> union "Reg" "R10" []
 | X86_64.R11 -> union "Reg" "R11" []
 | X86_64.R12 -> union "Reg" "R12" []
 | X86_64.R13 -> union "Reg" "R13" []
 | X86_64.R14 -> union "Reg" "R14" []
 | X86_64.R15 -> union "Reg" "R15" []
let x64FReg (value:X86_64.fReg) = match value with
 | X86_64.XMM0 -> union "FReg" "XMM0" []
 | X86_64.XMM1 -> union "FReg" "XMM1" []
 | X86_64.XMM2 -> union "FReg" "XMM2" []
 | X86_64.XMM3 -> union "FReg" "XMM3" []
 | X86_64.XMM4 -> union "FReg" "XMM4" []
 | X86_64.XMM5 -> union "FReg" "XMM5" []
 | X86_64.XMM6 -> union "FReg" "XMM6" []
 | X86_64.XMM7 -> union "FReg" "XMM7" []
 | X86_64.XMM8 -> union "FReg" "XMM8" []
 | X86_64.XMM9 -> union "FReg" "XMM9" []
 | X86_64.XMM10 -> union "FReg" "XMM10" []
 | X86_64.XMM11 -> union "FReg" "XMM11" []
 | X86_64.XMM12 -> union "FReg" "XMM12" []
 | X86_64.XMM13 -> union "FReg" "XMM13" []
 | X86_64.XMM14 -> union "FReg" "XMM14" []
 | X86_64.XMM15 -> union "FReg" "XMM15" []
let x64Condition (value:X86_64.condition) = match value with
 | X86_64.EQ -> union "Condition" "EQ" []
 | X86_64.NE -> union "Condition" "NE" []
 | X86_64.LT -> union "Condition" "LT" []
 | X86_64.GT -> union "Condition" "GT" []
 | X86_64.LE -> union "Condition" "LE" []
 | X86_64.GE -> union "Condition" "GE" []
 | X86_64.B -> union "Condition" "B" []
 | X86_64.A -> union "Condition" "A" []
 | X86_64.BE -> union "Condition" "BE" []
 | X86_64.AE -> union "Condition" "AE" []
 | X86_64.P -> union "Condition" "P" []
 | X86_64.NP -> union "Condition" "NP" []
let x64Size (value:X86_64.size) = match value with
 | X86_64.Byte -> union "Size" "Byte" []
 | X86_64.Word -> union "Size" "Word" []
 | X86_64.DWord -> union "Size" "DWord" []
 | X86_64.QWord -> union "Size" "QWord" []
let x64Instr (value:X86_64.instr) = match value with
 | X86_64.MOV_imm (v0,v1) -> union "Instr" "MOV_imm" [x64Reg v0;int64 v1]
 | X86_64.MOV_imm32 (v0,v1) -> union "Instr" "MOV_imm32" [x64Reg v0;int32 v1]
 | X86_64.MOV_reg (v0,v1) -> union "Instr" "MOV_reg" [x64Reg v0;x64Reg v1]
 | X86_64.MOV_load (v0,v1,v2) -> union "Instr" "MOV_load" [x64Reg v0;x64Reg v1;int32 v2]
 | X86_64.MOV_store (v0,v1,v2) -> union "Instr" "MOV_store" [x64Reg v0;int32 v1;x64Reg v2]
 | X86_64.MOV_reg32 (v0,v1) -> union "Instr" "MOV_reg32" [x64Reg v0;x64Reg v1]
 | X86_64.MOVZX_byte (v0,v1) -> union "Instr" "MOVZX_byte" [x64Reg v0;x64Reg v1]
 | X86_64.MOVZX_word (v0,v1) -> union "Instr" "MOVZX_word" [x64Reg v0;x64Reg v1]
 | X86_64.MOVSX_byte (v0,v1) -> union "Instr" "MOVSX_byte" [x64Reg v0;x64Reg v1]
 | X86_64.MOVSX_word (v0,v1) -> union "Instr" "MOVSX_word" [x64Reg v0;x64Reg v1]
 | X86_64.MOVSXD (v0,v1) -> union "Instr" "MOVSXD" [x64Reg v0;x64Reg v1]
 | X86_64.LEA (v0,v1,v2) -> union "Instr" "LEA" [x64Reg v0;x64Reg v1;int32 v2]
 | X86_64.LEA_index (v0,v1,v2,v3,v4) -> union "Instr" "LEA_index" [x64Reg v0;x64Reg v1;x64Reg v2;SemanticJson.int32 v3;int32 v4]
 | X86_64.LEA_rip (v0,v1) -> union "Instr" "LEA_rip" [x64Reg v0;SemanticJson.string v1]
 | X86_64.PUSH v0 -> union "Instr" "PUSH" [x64Reg v0]
 | X86_64.POP v0 -> union "Instr" "POP" [x64Reg v0]
 | X86_64.ADD_imm (v0,v1) -> union "Instr" "ADD_imm" [x64Reg v0;int32 v1]
 | X86_64.ADD_reg (v0,v1) -> union "Instr" "ADD_reg" [x64Reg v0;x64Reg v1]
 | X86_64.ADD_load (v0,v1,v2) -> union "Instr" "ADD_load" [x64Reg v0;x64Reg v1;int32 v2]
 | X86_64.SUB_imm (v0,v1) -> union "Instr" "SUB_imm" [x64Reg v0;int32 v1]
 | X86_64.SUB_reg (v0,v1) -> union "Instr" "SUB_reg" [x64Reg v0;x64Reg v1]
 | X86_64.SUB_load (v0,v1,v2) -> union "Instr" "SUB_load" [x64Reg v0;x64Reg v1;int32 v2]
 | X86_64.IMUL_reg (v0,v1) -> union "Instr" "IMUL_reg" [x64Reg v0;x64Reg v1]
 | X86_64.IMUL_imm (v0,v1,v2) -> union "Instr" "IMUL_imm" [x64Reg v0;x64Reg v1;int32 v2]
 | X86_64.IDIV v0 -> union "Instr" "IDIV" [x64Reg v0]
 | X86_64.DIV v0 -> union "Instr" "DIV" [x64Reg v0]
 | X86_64.NEG v0 -> union "Instr" "NEG" [x64Reg v0]
 | X86_64.NOT v0 -> union "Instr" "NOT" [x64Reg v0]
 | X86_64.CQO -> union "Instr" "CQO" []
 | X86_64.XOR_reg (v0,v1) -> union "Instr" "XOR_reg" [x64Reg v0;x64Reg v1]
 | X86_64.CMP_imm (v0,v1) -> union "Instr" "CMP_imm" [x64Reg v0;int32 v1]
 | X86_64.CMP_reg (v0,v1) -> union "Instr" "CMP_reg" [x64Reg v0;x64Reg v1]
 | X86_64.TEST_reg (v0,v1) -> union "Instr" "TEST_reg" [x64Reg v0;x64Reg v1]
 | X86_64.SETcc (v0,v1) -> union "Instr" "SETcc" [x64Condition v0;x64Reg v1]
 | X86_64.CMOVcc (v0,v1,v2) -> union "Instr" "CMOVcc" [x64Condition v0;x64Reg v1;x64Reg v2]
 | X86_64.AND_imm (v0,v1) -> union "Instr" "AND_imm" [x64Reg v0;int32 v1]
 | X86_64.AND_reg (v0,v1) -> union "Instr" "AND_reg" [x64Reg v0;x64Reg v1]
 | X86_64.OR_reg (v0,v1) -> union "Instr" "OR_reg" [x64Reg v0;x64Reg v1]
 | X86_64.SHL_imm (v0,v1) -> union "Instr" "SHL_imm" [x64Reg v0;SemanticJson.int32 v1]
 | X86_64.SHR_imm (v0,v1) -> union "Instr" "SHR_imm" [x64Reg v0;SemanticJson.int32 v1]
 | X86_64.SAR_imm (v0,v1) -> union "Instr" "SAR_imm" [x64Reg v0;SemanticJson.int32 v1]
 | X86_64.SHL_cl v0 -> union "Instr" "SHL_cl" [x64Reg v0]
 | X86_64.SHR_cl v0 -> union "Instr" "SHR_cl" [x64Reg v0]
 | X86_64.SAR_cl v0 -> union "Instr" "SAR_cl" [x64Reg v0]
 | X86_64.MOV_store_byte (v0,v1,v2) -> union "Instr" "MOV_store_byte" [x64Reg v0;int32 v1;x64Reg v2]
 | X86_64.MOV_load_byte (v0,v1,v2) -> union "Instr" "MOV_load_byte" [x64Reg v0;x64Reg v1;int32 v2]
 | X86_64.CALL v0 -> union "Instr" "CALL" [SemanticJson.string v0]
 | X86_64.CALL_reg v0 -> union "Instr" "CALL_reg" [x64Reg v0]
 | X86_64.JMP v0 -> union "Instr" "JMP" [SemanticJson.string v0]
 | X86_64.JMP_reg v0 -> union "Instr" "JMP_reg" [x64Reg v0]
 | X86_64.Jcc (v0,v1) -> union "Instr" "Jcc" [x64Condition v0;SemanticJson.string v1]
 | X86_64.RET -> union "Instr" "RET" []
 | X86_64.SYSCALL -> union "Instr" "SYSCALL" []
 | X86_64.Label v0 -> union "Instr" "Label" [SemanticJson.string v0]
 | X86_64.MOVSD_load (v0,v1,v2) -> union "Instr" "MOVSD_load" [x64FReg v0;x64Reg v1;int32 v2]
 | X86_64.MOVSD_store (v0,v1,v2) -> union "Instr" "MOVSD_store" [x64Reg v0;int32 v1;x64FReg v2]
 | X86_64.MOVSD_reg (v0,v1) -> union "Instr" "MOVSD_reg" [x64FReg v0;x64FReg v1]
 | X86_64.ADDSD (v0,v1) -> union "Instr" "ADDSD" [x64FReg v0;x64FReg v1]
 | X86_64.SUBSD (v0,v1) -> union "Instr" "SUBSD" [x64FReg v0;x64FReg v1]
 | X86_64.MULSD (v0,v1) -> union "Instr" "MULSD" [x64FReg v0;x64FReg v1]
 | X86_64.DIVSD (v0,v1) -> union "Instr" "DIVSD" [x64FReg v0;x64FReg v1]
 | X86_64.XORPD (v0,v1) -> union "Instr" "XORPD" [x64FReg v0;x64FReg v1]
 | X86_64.SQRTSD (v0,v1) -> union "Instr" "SQRTSD" [x64FReg v0;x64FReg v1]
 | X86_64.UCOMISD (v0,v1) -> union "Instr" "UCOMISD" [x64FReg v0;x64FReg v1]
 | X86_64.CVTSI2SD (v0,v1) -> union "Instr" "CVTSI2SD" [x64FReg v0;x64Reg v1]
 | X86_64.CVTTSD2SI (v0,v1) -> union "Instr" "CVTTSD2SI" [x64Reg v0;x64FReg v1]
 | X86_64.MOVQ_to_gp (v0,v1) -> union "Instr" "MOVQ_to_gp" [x64Reg v0;x64FReg v1]
 | X86_64.MOVQ_from_gp (v0,v1) -> union "Instr" "MOVQ_from_gp" [x64FReg v0;x64Reg v1]
let symReg=armReg
let symFReg=armFReg
let symCondition=armCondition
let symDataRef (value:Symbolic.dataRef) = match value with
 | Symbolic.StringLiteral v0 -> union "DataRef" "StringLiteral" [SemanticJson.string v0]
 | Symbolic.FloatLiteral v0 -> union "DataRef" "FloatLiteral" [float64 v0]
 | Symbolic.Named v0 -> union "DataRef" "Named" [SemanticJson.string v0]
let symLabelRef (value:Symbolic.labelRef) = match value with
 | Symbolic.CodeLabel v0 -> union "LabelRef" "CodeLabel" [SemanticJson.string v0]
 | Symbolic.DataLabel v0 -> union "LabelRef" "DataLabel" [symDataRef v0]
let symInstr (value:Symbolic.instr) = match value with
 | Symbolic.MOVZ (v0,v1,v2) -> union "Instr" "MOVZ" [symReg v0;uint16 v1;SemanticJson.int32 v2]
 | Symbolic.MOVN (v0,v1,v2) -> union "Instr" "MOVN" [symReg v0;uint16 v1;SemanticJson.int32 v2]
 | Symbolic.MOVK (v0,v1,v2) -> union "Instr" "MOVK" [symReg v0;uint16 v1;SemanticJson.int32 v2]
 | Symbolic.ADD_imm (v0,v1,v2) -> union "Instr" "ADD_imm" [symReg v0;symReg v1;uint16 v2]
 | Symbolic.ADD_reg (v0,v1,v2) -> union "Instr" "ADD_reg" [symReg v0;symReg v1;symReg v2]
 | Symbolic.ADD_shifted (v0,v1,v2,v3) -> union "Instr" "ADD_shifted" [symReg v0;symReg v1;symReg v2;SemanticJson.int32 v3]
 | Symbolic.ADD_extended (v0,v1,v2,v3) -> union "Instr" "ADD_extended" [symReg v0;symReg v1;symReg v2;armExtend v3]
 | Symbolic.SUB_imm (v0,v1,v2) -> union "Instr" "SUB_imm" [symReg v0;symReg v1;uint16 v2]
 | Symbolic.SUB_imm12 (v0,v1,v2) -> union "Instr" "SUB_imm12" [symReg v0;symReg v1;uint16 v2]
 | Symbolic.SUB_reg (v0,v1,v2) -> union "Instr" "SUB_reg" [symReg v0;symReg v1;symReg v2]
 | Symbolic.SUB_shifted (v0,v1,v2,v3) -> union "Instr" "SUB_shifted" [symReg v0;symReg v1;symReg v2;SemanticJson.int32 v3]
 | Symbolic.SUB_extended (v0,v1,v2,v3) -> union "Instr" "SUB_extended" [symReg v0;symReg v1;symReg v2;armExtend v3]
 | Symbolic.SUBS_imm (v0,v1,v2) -> union "Instr" "SUBS_imm" [symReg v0;symReg v1;uint16 v2]
 | Symbolic.MUL (v0,v1,v2) -> union "Instr" "MUL" [symReg v0;symReg v1;symReg v2]
 | Symbolic.SDIV (v0,v1,v2) -> union "Instr" "SDIV" [symReg v0;symReg v1;symReg v2]
 | Symbolic.UDIV (v0,v1,v2) -> union "Instr" "UDIV" [symReg v0;symReg v1;symReg v2]
 | Symbolic.MSUB (v0,v1,v2,v3) -> union "Instr" "MSUB" [symReg v0;symReg v1;symReg v2;symReg v3]
 | Symbolic.MADD (v0,v1,v2,v3) -> union "Instr" "MADD" [symReg v0;symReg v1;symReg v2;symReg v3]
 | Symbolic.CMP_imm (v0,v1) -> union "Instr" "CMP_imm" [symReg v0;uint16 v1]
 | Symbolic.CMP_reg (v0,v1) -> union "Instr" "CMP_reg" [symReg v0;symReg v1]
 | Symbolic.CSET (v0,v1) -> union "Instr" "CSET" [symReg v0;symCondition v1]
 | Symbolic.CSEL (v0,v1,v2,v3) -> union "Instr" "CSEL" [symReg v0;symReg v1;symReg v2;symCondition v3]
 | Symbolic.AND_reg (v0,v1,v2) -> union "Instr" "AND_reg" [symReg v0;symReg v1;symReg v2]
 | Symbolic.BIC_reg (v0,v1,v2) -> union "Instr" "BIC_reg" [symReg v0;symReg v1;symReg v2]
 | Symbolic.AND_imm (v0,v1,v2) -> union "Instr" "AND_imm" [symReg v0;symReg v1;uint64 v2]
 | Symbolic.ORR_reg (v0,v1,v2) -> union "Instr" "ORR_reg" [symReg v0;symReg v1;symReg v2]
 | Symbolic.EOR_reg (v0,v1,v2) -> union "Instr" "EOR_reg" [symReg v0;symReg v1;symReg v2]
 | Symbolic.LSL_reg (v0,v1,v2) -> union "Instr" "LSL_reg" [symReg v0;symReg v1;symReg v2]
 | Symbolic.LSR_reg (v0,v1,v2) -> union "Instr" "LSR_reg" [symReg v0;symReg v1;symReg v2]
 | Symbolic.ASR_reg (v0,v1,v2) -> union "Instr" "ASR_reg" [symReg v0;symReg v1;symReg v2]
 | Symbolic.LSL_imm (v0,v1,v2) -> union "Instr" "LSL_imm" [symReg v0;symReg v1;SemanticJson.int32 v2]
 | Symbolic.LSR_imm (v0,v1,v2) -> union "Instr" "LSR_imm" [symReg v0;symReg v1;SemanticJson.int32 v2]
 | Symbolic.ASR_imm (v0,v1,v2) -> union "Instr" "ASR_imm" [symReg v0;symReg v1;SemanticJson.int32 v2]
 | Symbolic.MVN (v0,v1) -> union "Instr" "MVN" [symReg v0;symReg v1]
 | Symbolic.MOV_reg (v0,v1) -> union "Instr" "MOV_reg" [symReg v0;symReg v1]
 | Symbolic.STRB (v0,v1,v2) -> union "Instr" "STRB" [symReg v0;symReg v1;SemanticJson.int32 v2]
 | Symbolic.LDRB (v0,v1,v2) -> union "Instr" "LDRB" [symReg v0;symReg v1;symReg v2]
 | Symbolic.LDRB_imm (v0,v1,v2) -> union "Instr" "LDRB_imm" [symReg v0;symReg v1;SemanticJson.int32 v2]
 | Symbolic.STRB_reg (v0,v1) -> union "Instr" "STRB_reg" [symReg v0;symReg v1]
 | Symbolic.STP (v0,v1,v2,v3) -> union "Instr" "STP" [symReg v0;symReg v1;symReg v2;int16 v3]
 | Symbolic.STP_pre (v0,v1,v2,v3) -> union "Instr" "STP_pre" [symReg v0;symReg v1;symReg v2;int16 v3]
 | Symbolic.LDP (v0,v1,v2,v3) -> union "Instr" "LDP" [symReg v0;symReg v1;symReg v2;int16 v3]
 | Symbolic.LDP_post (v0,v1,v2,v3) -> union "Instr" "LDP_post" [symReg v0;symReg v1;symReg v2;int16 v3]
 | Symbolic.STR (v0,v1,v2) -> union "Instr" "STR" [symReg v0;symReg v1;int16 v2]
 | Symbolic.LDR (v0,v1,v2) -> union "Instr" "LDR" [symReg v0;symReg v1;int16 v2]
 | Symbolic.STUR (v0,v1,v2) -> union "Instr" "STUR" [symReg v0;symReg v1;int16 v2]
 | Symbolic.LDUR (v0,v1,v2) -> union "Instr" "LDUR" [symReg v0;symReg v1;int16 v2]
 | Symbolic.BL v0 -> union "Instr" "BL" [SemanticJson.string v0]
 | Symbolic.BLR v0 -> union "Instr" "BLR" [symReg v0]
 | Symbolic.BR v0 -> union "Instr" "BR" [symReg v0]
 | Symbolic.CBZ (v0,v1) -> union "Instr" "CBZ" [symReg v0;SemanticJson.string v1]
 | Symbolic.CBNZ (v0,v1) -> union "Instr" "CBNZ" [symReg v0;SemanticJson.string v1]
 | Symbolic.B_label v0 -> union "Instr" "B_label" [SemanticJson.string v0]
 | Symbolic.B_cond_label (v0,v1) -> union "Instr" "B_cond_label" [symCondition v0;SemanticJson.string v1]
 | Symbolic.CBZ_offset (v0,v1) -> union "Instr" "CBZ_offset" [symReg v0;SemanticJson.int32 v1]
 | Symbolic.CBNZ_offset (v0,v1) -> union "Instr" "CBNZ_offset" [symReg v0;SemanticJson.int32 v1]
 | Symbolic.TBZ (v0,v1,v2) -> union "Instr" "TBZ" [symReg v0;SemanticJson.int32 v1;SemanticJson.int32 v2]
 | Symbolic.TBNZ (v0,v1,v2) -> union "Instr" "TBNZ" [symReg v0;SemanticJson.int32 v1;SemanticJson.int32 v2]
 | Symbolic.TBZ_label (v0,v1,v2) -> union "Instr" "TBZ_label" [symReg v0;SemanticJson.int32 v1;SemanticJson.string v2]
 | Symbolic.TBNZ_label (v0,v1,v2) -> union "Instr" "TBNZ_label" [symReg v0;SemanticJson.int32 v1;SemanticJson.string v2]
 | Symbolic.B v0 -> union "Instr" "B" [SemanticJson.int32 v0]
 | Symbolic.B_cond (v0,v1) -> union "Instr" "B_cond" [symCondition v0;SemanticJson.int32 v1]
 | Symbolic.NEG (v0,v1) -> union "Instr" "NEG" [symReg v0;symReg v1]
 | Symbolic.RET -> union "Instr" "RET" []
 | Symbolic.SVC v0 -> union "Instr" "SVC" [uint16 v0]
 | Symbolic.Label v0 -> union "Instr" "Label" [SemanticJson.string v0]
 | Symbolic.ADRP (v0,v1) -> union "Instr" "ADRP" [symReg v0;symLabelRef v1]
 | Symbolic.ADD_label (v0,v1,v2) -> union "Instr" "ADD_label" [symReg v0;symReg v1;symLabelRef v2]
 | Symbolic.ADR (v0,v1) -> union "Instr" "ADR" [symReg v0;symLabelRef v1]
 | Symbolic.LDR_fp (v0,v1,v2) -> union "Instr" "LDR_fp" [symFReg v0;symReg v1;int16 v2]
 | Symbolic.STR_fp (v0,v1,v2) -> union "Instr" "STR_fp" [symFReg v0;symReg v1;int16 v2]
 | Symbolic.STP_fp (v0,v1,v2,v3) -> union "Instr" "STP_fp" [symFReg v0;symFReg v1;symReg v2;int16 v3]
 | Symbolic.LDP_fp (v0,v1,v2,v3) -> union "Instr" "LDP_fp" [symFReg v0;symFReg v1;symReg v2;int16 v3]
 | Symbolic.FADD (v0,v1,v2) -> union "Instr" "FADD" [symFReg v0;symFReg v1;symFReg v2]
 | Symbolic.FSUB (v0,v1,v2) -> union "Instr" "FSUB" [symFReg v0;symFReg v1;symFReg v2]
 | Symbolic.FMUL (v0,v1,v2) -> union "Instr" "FMUL" [symFReg v0;symFReg v1;symFReg v2]
 | Symbolic.FMADD (v0,v1,v2,v3) -> union "Instr" "FMADD" [symFReg v0;symFReg v1;symFReg v2;symFReg v3]
 | Symbolic.FDIV (v0,v1,v2) -> union "Instr" "FDIV" [symFReg v0;symFReg v1;symFReg v2]
 | Symbolic.FNEG (v0,v1) -> union "Instr" "FNEG" [symFReg v0;symFReg v1]
 | Symbolic.FABS (v0,v1) -> union "Instr" "FABS" [symFReg v0;symFReg v1]
 | Symbolic.FSQRT (v0,v1) -> union "Instr" "FSQRT" [symFReg v0;symFReg v1]
 | Symbolic.FCMP (v0,v1) -> union "Instr" "FCMP" [symFReg v0;symFReg v1]
 | Symbolic.FMOV_reg (v0,v1) -> union "Instr" "FMOV_reg" [symFReg v0;symFReg v1]
 | Symbolic.FMOV_imm (v0,v1) -> union "Instr" "FMOV_imm" [symFReg v0;float64 v1]
 | Symbolic.FMOV_zero v0 -> union "Instr" "FMOV_zero" [symFReg v0]
 | Symbolic.FMOV_to_gp (v0,v1) -> union "Instr" "FMOV_to_gp" [symReg v0;symFReg v1]
 | Symbolic.FMOV_from_gp (v0,v1) -> union "Instr" "FMOV_from_gp" [symFReg v0;symReg v1]
 | Symbolic.CNT_8B (v0,v1) -> union "Instr" "CNT_8B" [symFReg v0;symFReg v1]
 | Symbolic.ADDV_8B (v0,v1) -> union "Instr" "ADDV_8B" [symFReg v0;symFReg v1]
 | Symbolic.UMOV_byte (v0,v1) -> union "Instr" "UMOV_byte" [symReg v0;symFReg v1]
 | Symbolic.SCVTF (v0,v1) -> union "Instr" "SCVTF" [symFReg v0;symReg v1]
 | Symbolic.FCVTZS (v0,v1) -> union "Instr" "FCVTZS" [symReg v0;symFReg v1]
 | Symbolic.SXTB (v0,v1) -> union "Instr" "SXTB" [symReg v0;symReg v1]
 | Symbolic.SXTH (v0,v1) -> union "Instr" "SXTH" [symReg v0;symReg v1]
 | Symbolic.SXTW (v0,v1) -> union "Instr" "SXTW" [symReg v0;symReg v1]
 | Symbolic.UXTB (v0,v1) -> union "Instr" "UXTB" [symReg v0;symReg v1]
 | Symbolic.UXTH (v0,v1) -> union "Instr" "UXTH" [symReg v0;symReg v1]
 | Symbolic.UXTW (v0,v1) -> union "Instr" "UXTW" [symReg v0;symReg v1]
let armRegValues=[|ARM64.X0;ARM64.X1;ARM64.X2;ARM64.X3;ARM64.X4;ARM64.X5;ARM64.X6;ARM64.X7;ARM64.X8;ARM64.X9;ARM64.X10;ARM64.X11;ARM64.X12;ARM64.X13;ARM64.X14;ARM64.X15;ARM64.X16;ARM64.X17;ARM64.X18;ARM64.X19;ARM64.X20;ARM64.X21;ARM64.X22;ARM64.X23;ARM64.X24;ARM64.X25;ARM64.X26;ARM64.X27;ARM64.X28;ARM64.X29;ARM64.X30;ARM64.SP|]
let armFRegValues=[|ARM64.D0;ARM64.D1;ARM64.D2;ARM64.D3;ARM64.D4;ARM64.D5;ARM64.D6;ARM64.D7;ARM64.D8;ARM64.D9;ARM64.D10;ARM64.D11;ARM64.D12;ARM64.D13;ARM64.D14;ARM64.D15;ARM64.D16;ARM64.D17;ARM64.D18;ARM64.D19;ARM64.D20;ARM64.D21;ARM64.D22;ARM64.D23;ARM64.D24;ARM64.D25;ARM64.D26;ARM64.D27;ARM64.D28;ARM64.D29;ARM64.D30;ARM64.D31|]
let armConditionValues=[|ARM64.EQ;ARM64.NE;ARM64.LT;ARM64.GT;ARM64.LE;ARM64.GE;ARM64.LO;ARM64.HI;ARM64.LS;ARM64.HS|]
let armExtendValues=[|ARM64.ExtendUXTB;ARM64.ExtendUXTH;ARM64.ExtendUXTW;ARM64.ExtendSXTB;ARM64.ExtendSXTH;ARM64.ExtendSXTW|]
let armInstructions source role boundary =
 [ARM64.MOVZ ((armRegValues.((role+0) mod Array.length armRegValues)),(boundary land 65535),(boundary));
 ARM64.MOVN ((armRegValues.((role+0) mod Array.length armRegValues)),(boundary land 65535),(boundary));
 ARM64.MOVK ((armRegValues.((role+0) mod Array.length armRegValues)),(boundary land 65535),(boundary));
 ARM64.ADD_imm ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(boundary land 65535));
 ARM64.ADD_reg ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)));
 ARM64.ADD_shifted ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)),(boundary));
 ARM64.ADD_extended ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)),(armExtendValues.((role+3) mod Array.length armExtendValues)));
 ARM64.SUB_imm ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(boundary land 65535));
 ARM64.SUB_imm12 ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(boundary land 65535));
 ARM64.SUB_reg ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)));
 ARM64.SUB_shifted ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)),(boundary));
 ARM64.SUB_extended ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)),(armExtendValues.((role+3) mod Array.length armExtendValues)));
 ARM64.SUBS_imm ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(boundary land 65535));
 ARM64.MUL ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)));
 ARM64.SDIV ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)));
 ARM64.UDIV ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)));
 ARM64.MSUB ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)),(armRegValues.((role+3) mod Array.length armRegValues)));
 ARM64.MADD ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)),(armRegValues.((role+3) mod Array.length armRegValues)));
 ARM64.CMP_imm ((armRegValues.((role+0) mod Array.length armRegValues)),(boundary land 65535));
 ARM64.CMP_reg ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)));
 ARM64.CSET ((armRegValues.((role+0) mod Array.length armRegValues)),(armConditionValues.((role+1) mod Array.length armConditionValues)));
 ARM64.CSEL ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)),(armConditionValues.((role+3) mod Array.length armConditionValues)));
 ARM64.AND_reg ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)));
 ARM64.BIC_reg ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)));
 ARM64.AND_imm ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(Int64.of_int boundary));
 ARM64.ORR_reg ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)));
 ARM64.EOR_reg ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)));
 ARM64.LSL_reg ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)));
 ARM64.LSR_reg ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)));
 ARM64.ASR_reg ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)));
 ARM64.LSL_imm ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(boundary));
 ARM64.LSR_imm ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(boundary));
 ARM64.ASR_imm ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(boundary));
 ARM64.MVN ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)));
 ARM64.MOV_reg ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)));
 ARM64.STRB ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(boundary));
 ARM64.LDRB ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)));
 ARM64.LDRB_imm ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(boundary));
 ARM64.STRB_reg ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)));
 ARM64.STP ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)),(Int32.to_int (Int32.shift_right (Int32.shift_left (Int32.of_int boundary) 16) 16)));
 ARM64.STP_pre ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)),(Int32.to_int (Int32.shift_right (Int32.shift_left (Int32.of_int boundary) 16) 16)));
 ARM64.LDP ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)),(Int32.to_int (Int32.shift_right (Int32.shift_left (Int32.of_int boundary) 16) 16)));
 ARM64.LDP_post ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)),(Int32.to_int (Int32.shift_right (Int32.shift_left (Int32.of_int boundary) 16) 16)));
 ARM64.STR ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(Int32.to_int (Int32.shift_right (Int32.shift_left (Int32.of_int boundary) 16) 16)));
 ARM64.LDR ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(Int32.to_int (Int32.shift_right (Int32.shift_left (Int32.of_int boundary) 16) 16)));
 ARM64.STUR ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(Int32.to_int (Int32.shift_right (Int32.shift_left (Int32.of_int boundary) 16) 16)));
 ARM64.LDUR ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(Int32.to_int (Int32.shift_right (Int32.shift_left (Int32.of_int boundary) 16) 16)));
 ARM64.BL ((source));
 ARM64.BLR ((armRegValues.((role+0) mod Array.length armRegValues)));
 ARM64.BR ((armRegValues.((role+0) mod Array.length armRegValues)));
 ARM64.CBZ ((armRegValues.((role+0) mod Array.length armRegValues)),(source));
 ARM64.CBNZ ((armRegValues.((role+0) mod Array.length armRegValues)),(source));
 ARM64.B_label ((source));
 ARM64.B_cond_label ((armConditionValues.((role+0) mod Array.length armConditionValues)),(source));
 ARM64.CBZ_offset ((armRegValues.((role+0) mod Array.length armRegValues)),(boundary));
 ARM64.CBNZ_offset ((armRegValues.((role+0) mod Array.length armRegValues)),(boundary));
 ARM64.TBZ ((armRegValues.((role+0) mod Array.length armRegValues)),(boundary),(boundary));
 ARM64.TBNZ ((armRegValues.((role+0) mod Array.length armRegValues)),(boundary),(boundary));
 ARM64.TBZ_label ((armRegValues.((role+0) mod Array.length armRegValues)),(boundary),(source));
 ARM64.TBNZ_label ((armRegValues.((role+0) mod Array.length armRegValues)),(boundary),(source));
 ARM64.B ((boundary));
 ARM64.B_cond ((armConditionValues.((role+0) mod Array.length armConditionValues)),(boundary));
 ARM64.NEG ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)));
 ARM64.RET;
 ARM64.SVC ((boundary land 65535));
 ARM64.Label ((source));
 ARM64.ADRP ((armRegValues.((role+0) mod Array.length armRegValues)),(source));
 ARM64.ADD_label ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(source));
 ARM64.ADR ((armRegValues.((role+0) mod Array.length armRegValues)),(source));
 ARM64.LDR_fp ((armFRegValues.((role+0) mod Array.length armFRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(Int32.to_int (Int32.shift_right (Int32.shift_left (Int32.of_int boundary) 16) 16)));
 ARM64.STR_fp ((armFRegValues.((role+0) mod Array.length armFRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)),(Int32.to_int (Int32.shift_right (Int32.shift_left (Int32.of_int boundary) 16) 16)));
 ARM64.STP_fp ((armFRegValues.((role+0) mod Array.length armFRegValues)),(armFRegValues.((role+1) mod Array.length armFRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)),(Int32.to_int (Int32.shift_right (Int32.shift_left (Int32.of_int boundary) 16) 16)));
 ARM64.LDP_fp ((armFRegValues.((role+0) mod Array.length armFRegValues)),(armFRegValues.((role+1) mod Array.length armFRegValues)),(armRegValues.((role+2) mod Array.length armRegValues)),(Int32.to_int (Int32.shift_right (Int32.shift_left (Int32.of_int boundary) 16) 16)));
 ARM64.FADD ((armFRegValues.((role+0) mod Array.length armFRegValues)),(armFRegValues.((role+1) mod Array.length armFRegValues)),(armFRegValues.((role+2) mod Array.length armFRegValues)));
 ARM64.FSUB ((armFRegValues.((role+0) mod Array.length armFRegValues)),(armFRegValues.((role+1) mod Array.length armFRegValues)),(armFRegValues.((role+2) mod Array.length armFRegValues)));
 ARM64.FMUL ((armFRegValues.((role+0) mod Array.length armFRegValues)),(armFRegValues.((role+1) mod Array.length armFRegValues)),(armFRegValues.((role+2) mod Array.length armFRegValues)));
 ARM64.FMADD ((armFRegValues.((role+0) mod Array.length armFRegValues)),(armFRegValues.((role+1) mod Array.length armFRegValues)),(armFRegValues.((role+2) mod Array.length armFRegValues)),(armFRegValues.((role+3) mod Array.length armFRegValues)));
 ARM64.FDIV ((armFRegValues.((role+0) mod Array.length armFRegValues)),(armFRegValues.((role+1) mod Array.length armFRegValues)),(armFRegValues.((role+2) mod Array.length armFRegValues)));
 ARM64.FNEG ((armFRegValues.((role+0) mod Array.length armFRegValues)),(armFRegValues.((role+1) mod Array.length armFRegValues)));
 ARM64.FABS ((armFRegValues.((role+0) mod Array.length armFRegValues)),(armFRegValues.((role+1) mod Array.length armFRegValues)));
 ARM64.FSQRT ((armFRegValues.((role+0) mod Array.length armFRegValues)),(armFRegValues.((role+1) mod Array.length armFRegValues)));
 ARM64.FCMP ((armFRegValues.((role+0) mod Array.length armFRegValues)),(armFRegValues.((role+1) mod Array.length armFRegValues)));
 ARM64.FMOV_reg ((armFRegValues.((role+0) mod Array.length armFRegValues)),(armFRegValues.((role+1) mod Array.length armFRegValues)));
 ARM64.FMOV_imm ((armFRegValues.((role+0) mod Array.length armFRegValues)),(float_of_int boundary));
 ARM64.FMOV_to_gp ((armRegValues.((role+0) mod Array.length armRegValues)),(armFRegValues.((role+1) mod Array.length armFRegValues)));
 ARM64.FMOV_from_gp ((armFRegValues.((role+0) mod Array.length armFRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)));
 ARM64.SCVTF ((armFRegValues.((role+0) mod Array.length armFRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)));
 ARM64.FCVTZS ((armRegValues.((role+0) mod Array.length armRegValues)),(armFRegValues.((role+1) mod Array.length armFRegValues)));
 ARM64.SXTB ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)));
 ARM64.SXTH ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)));
 ARM64.SXTW ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)));
 ARM64.UXTB ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)));
 ARM64.UXTH ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)));
 ARM64.UXTW ((armRegValues.((role+0) mod Array.length armRegValues)),(armRegValues.((role+1) mod Array.length armRegValues)));
 ]
let x64RegValues=[|X86_64.RAX;X86_64.RBX;X86_64.RCX;X86_64.RDX;X86_64.RSI;X86_64.RDI;X86_64.RBP;X86_64.RSP;X86_64.R8;X86_64.R9;X86_64.R10;X86_64.R11;X86_64.R12;X86_64.R13;X86_64.R14;X86_64.R15|]
let x64FRegValues=[|X86_64.XMM0;X86_64.XMM1;X86_64.XMM2;X86_64.XMM3;X86_64.XMM4;X86_64.XMM5;X86_64.XMM6;X86_64.XMM7;X86_64.XMM8;X86_64.XMM9;X86_64.XMM10;X86_64.XMM11;X86_64.XMM12;X86_64.XMM13;X86_64.XMM14;X86_64.XMM15|]
let x64ConditionValues=[|X86_64.EQ;X86_64.NE;X86_64.LT;X86_64.GT;X86_64.LE;X86_64.GE;X86_64.B;X86_64.A;X86_64.BE;X86_64.AE;X86_64.P;X86_64.NP|]
let x64SizeValues=[|X86_64.Byte;X86_64.Word;X86_64.DWord;X86_64.QWord|]
let x64Instructions source role boundary =
 [X86_64.MOV_imm ((x64RegValues.((role+0) mod Array.length x64RegValues)),(Int64.of_int boundary));
 X86_64.MOV_imm32 ((x64RegValues.((role+0) mod Array.length x64RegValues)),(Int32.of_int boundary));
 X86_64.MOV_reg ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)));
 X86_64.MOV_load ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)),(Int32.of_int boundary));
 X86_64.MOV_store ((x64RegValues.((role+0) mod Array.length x64RegValues)),(Int32.of_int boundary),(x64RegValues.((role+2) mod Array.length x64RegValues)));
 X86_64.MOV_reg32 ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)));
 X86_64.MOVZX_byte ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)));
 X86_64.MOVZX_word ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)));
 X86_64.MOVSX_byte ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)));
 X86_64.MOVSX_word ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)));
 X86_64.MOVSXD ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)));
 X86_64.LEA ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)),(Int32.of_int boundary));
 X86_64.LEA_index ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)),(x64RegValues.((role+2) mod Array.length x64RegValues)),(boundary),(Int32.of_int boundary));
 X86_64.LEA_rip ((x64RegValues.((role+0) mod Array.length x64RegValues)),(source));
 X86_64.PUSH ((x64RegValues.((role+0) mod Array.length x64RegValues)));
 X86_64.POP ((x64RegValues.((role+0) mod Array.length x64RegValues)));
 X86_64.ADD_imm ((x64RegValues.((role+0) mod Array.length x64RegValues)),(Int32.of_int boundary));
 X86_64.ADD_reg ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)));
 X86_64.ADD_load ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)),(Int32.of_int boundary));
 X86_64.SUB_imm ((x64RegValues.((role+0) mod Array.length x64RegValues)),(Int32.of_int boundary));
 X86_64.SUB_reg ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)));
 X86_64.SUB_load ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)),(Int32.of_int boundary));
 X86_64.IMUL_reg ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)));
 X86_64.IMUL_imm ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)),(Int32.of_int boundary));
 X86_64.IDIV ((x64RegValues.((role+0) mod Array.length x64RegValues)));
 X86_64.DIV ((x64RegValues.((role+0) mod Array.length x64RegValues)));
 X86_64.NEG ((x64RegValues.((role+0) mod Array.length x64RegValues)));
 X86_64.NOT ((x64RegValues.((role+0) mod Array.length x64RegValues)));
 X86_64.CQO;
 X86_64.XOR_reg ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)));
 X86_64.CMP_imm ((x64RegValues.((role+0) mod Array.length x64RegValues)),(Int32.of_int boundary));
 X86_64.CMP_reg ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)));
 X86_64.TEST_reg ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)));
 X86_64.SETcc ((x64ConditionValues.((role+0) mod Array.length x64ConditionValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)));
 X86_64.CMOVcc ((x64ConditionValues.((role+0) mod Array.length x64ConditionValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)),(x64RegValues.((role+2) mod Array.length x64RegValues)));
 X86_64.AND_imm ((x64RegValues.((role+0) mod Array.length x64RegValues)),(Int32.of_int boundary));
 X86_64.AND_reg ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)));
 X86_64.OR_reg ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)));
 X86_64.SHL_imm ((x64RegValues.((role+0) mod Array.length x64RegValues)),(boundary));
 X86_64.SHR_imm ((x64RegValues.((role+0) mod Array.length x64RegValues)),(boundary));
 X86_64.SAR_imm ((x64RegValues.((role+0) mod Array.length x64RegValues)),(boundary));
 X86_64.SHL_cl ((x64RegValues.((role+0) mod Array.length x64RegValues)));
 X86_64.SHR_cl ((x64RegValues.((role+0) mod Array.length x64RegValues)));
 X86_64.SAR_cl ((x64RegValues.((role+0) mod Array.length x64RegValues)));
 X86_64.MOV_store_byte ((x64RegValues.((role+0) mod Array.length x64RegValues)),(Int32.of_int boundary),(x64RegValues.((role+2) mod Array.length x64RegValues)));
 X86_64.MOV_load_byte ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)),(Int32.of_int boundary));
 X86_64.CALL ((source));
 X86_64.CALL_reg ((x64RegValues.((role+0) mod Array.length x64RegValues)));
 X86_64.JMP ((source));
 X86_64.JMP_reg ((x64RegValues.((role+0) mod Array.length x64RegValues)));
 X86_64.Jcc ((x64ConditionValues.((role+0) mod Array.length x64ConditionValues)),(source));
 X86_64.RET;
 X86_64.SYSCALL;
 X86_64.Label ((source));
 X86_64.MOVSD_load ((x64FRegValues.((role+0) mod Array.length x64FRegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)),(Int32.of_int boundary));
 X86_64.MOVSD_store ((x64RegValues.((role+0) mod Array.length x64RegValues)),(Int32.of_int boundary),(x64FRegValues.((role+2) mod Array.length x64FRegValues)));
 X86_64.MOVSD_reg ((x64FRegValues.((role+0) mod Array.length x64FRegValues)),(x64FRegValues.((role+1) mod Array.length x64FRegValues)));
 X86_64.ADDSD ((x64FRegValues.((role+0) mod Array.length x64FRegValues)),(x64FRegValues.((role+1) mod Array.length x64FRegValues)));
 X86_64.SUBSD ((x64FRegValues.((role+0) mod Array.length x64FRegValues)),(x64FRegValues.((role+1) mod Array.length x64FRegValues)));
 X86_64.MULSD ((x64FRegValues.((role+0) mod Array.length x64FRegValues)),(x64FRegValues.((role+1) mod Array.length x64FRegValues)));
 X86_64.DIVSD ((x64FRegValues.((role+0) mod Array.length x64FRegValues)),(x64FRegValues.((role+1) mod Array.length x64FRegValues)));
 X86_64.XORPD ((x64FRegValues.((role+0) mod Array.length x64FRegValues)),(x64FRegValues.((role+1) mod Array.length x64FRegValues)));
 X86_64.SQRTSD ((x64FRegValues.((role+0) mod Array.length x64FRegValues)),(x64FRegValues.((role+1) mod Array.length x64FRegValues)));
 X86_64.UCOMISD ((x64FRegValues.((role+0) mod Array.length x64FRegValues)),(x64FRegValues.((role+1) mod Array.length x64FRegValues)));
 X86_64.CVTSI2SD ((x64FRegValues.((role+0) mod Array.length x64FRegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)));
 X86_64.CVTTSD2SI ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64FRegValues.((role+1) mod Array.length x64FRegValues)));
 X86_64.MOVQ_to_gp ((x64RegValues.((role+0) mod Array.length x64RegValues)),(x64FRegValues.((role+1) mod Array.length x64FRegValues)));
 X86_64.MOVQ_from_gp ((x64FRegValues.((role+0) mod Array.length x64FRegValues)),(x64RegValues.((role+1) mod Array.length x64RegValues)));
 ]
let syscalls (s:ARM64.syscallConfig)=tuple [list uint16 [s.ARM64.numbers.Platform.write;s.ARM64.numbers.Platform.exit;s.ARM64.numbers.Platform.mmap;s.ARM64.numbers.Platform.munmap;s.ARM64.numbers.Platform.open_;s.ARM64.numbers.Platform.read;s.ARM64.numbers.Platform.close;s.ARM64.numbers.Platform.fstat;s.ARM64.numbers.Platform.access;s.ARM64.numbers.Platform.unlink;s.ARM64.numbers.Platform.chmod;s.ARM64.numbers.Platform.getrandom;s.ARM64.numbers.Platform.gettimeofday;s.ARM64.numbers.Platform.nanosleep;s.ARM64.numbers.Platform.socket;s.ARM64.numbers.Platform.connect;s.ARM64.numbers.Platform.setSockOpt];uint16 s.ARM64.svcImmediate;armReg s.ARM64.syscallRegister]
let observe source =
 let labels=[source;"";Symbolic.coverageDataLabelName;Symbolic.leakCounterLabelName;"__dark_string_literal_data:"^source;"other"] in
 let boundaries=[-32768;-512;-1;0;1;63;255;4095;32760;65535;Int32.to_int Int32.min_int;Int32.to_int Int32.max_int] in
 let armCases=list (fun label -> list (fun boundary -> list (fun role -> let instrs=armInstructions label role boundary in tuple [list armInstr instrs;list symInstr (Symbolic.ofARM64List instrs);list (fun instr -> symInstr (Symbolic.ofARM64 instr)) instrs]) (List.init 32 Fun.id)) boundaries) labels in
 let x64Cases=list (fun label -> list (fun boundary -> list (fun role -> list x64Instr (x64Instructions label role boundary)) (List.init 16 Fun.id)) boundaries) labels in
 let candidates=List.init 256 (fun encoded -> let sign=if encoded land 128=0 then 1. else -1. in let exponent=(encoded lsr 4) land 7 in let exp=if exponent>=4 then exponent-7 else exponent+1 in sign*.(1.+.float_of_int (encoded land 15)/.16.)*.Float.ldexp 1. exp) in
 let floats=[0.;-0.;infinity;neg_infinity;Int64.float_of_bits 0x7ff8000000000001L;Int64.float_of_bits 1L;Float.max_float]@List.concat_map (fun candidate -> let bits=Int64.bits_of_float candidate in [candidate;Int64.float_of_bits (Int64.pred bits);Int64.float_of_bits (Int64.succ bits)]) candidates in
 let floatCases=list (fun value -> tuple [float64 value;option uint32 (ARM64.tryEncodeFmovFloatImmediate value)]) floats in
 let platformCases=list (fun target -> let config=ARM64.targetConfigFor target in tuple [SemanticJson.union "OS" (match ARM64.targetOS config with Platform.MacOS -> "MacOS" | Platform.Linux -> "Linux") [];syscalls (ARM64.targetSyscalls config);syscalls (ARM64.syscallConfigFor (ARM64.targetOS config))]) [Platform.MacOSARM64;Platform.LinuxARM64] in
 let labelCases=list (fun label -> tuple [SemanticJson.string (X86_64.stringLiteralLabel label);option SemanticJson.string (X86_64.tryStringLiteralValue label);option SemanticJson.string (X86_64.tryStringLiteralValue (X86_64.stringLiteralLabel label))]) labels in
 tuple [armCases;x64Cases;floatCases;platformCases;labelCases;list x64Size (Array.to_list x64SizeValues)]
