(* Complete instruction streams, register lifetime classifications and guarded rewrites. *)
open Dark_compiler
module P=InstrumentedARMPeephole
module S=Symbolic
module J=MachineISAObservation
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let symbolic=list J.symInstr
let regs=[|ARM64.X0;ARM64.X1;ARM64.X2;ARM64.X3;ARM64.X4;ARM64.X5;ARM64.X6;ARM64.X7;ARM64.X8;ARM64.X9;ARM64.X10;ARM64.X11;ARM64.X12;ARM64.X13;ARM64.X14;ARM64.X15;ARM64.X16;ARM64.X17;ARM64.X18;ARM64.X19;ARM64.X20;ARM64.X21;ARM64.X22;ARM64.X23;ARM64.X24;ARM64.X25;ARM64.X26;ARM64.X27;ARM64.X28;ARM64.X29;ARM64.X30;ARM64.SP|]
let fps=[|ARM64.D0;ARM64.D1;ARM64.D2;ARM64.D3;ARM64.D4;ARM64.D5;ARM64.D6;ARM64.D7;ARM64.D8;ARM64.D9;ARM64.D10;ARM64.D11;ARM64.D12;ARM64.D13;ARM64.D14;ARM64.D15;ARM64.D16;ARM64.D17;ARM64.D18;ARM64.D19;ARM64.D20;ARM64.D21;ARM64.D22;ARM64.D23;ARM64.D24;ARM64.D25;ARM64.D26;ARM64.D27;ARM64.D28;ARM64.D29;ARM64.D30;ARM64.D31|]
let conditions=[ARM64.EQ;ARM64.NE;ARM64.LT;ARM64.GT;ARM64.LE;ARM64.GE;ARM64.LO;ARM64.HI;ARM64.LS;ARM64.HS]
let fixtures source role boundary=
 S.ofARM64List (J.armInstructions source role boundary) @
 [S.FMOV_zero fps.(role);S.CNT_8B (fps.(role),fps.((role+1) mod 32));S.ADDV_8B (fps.(role),fps.((role+1) mod 32));S.UMOV_byte (regs.(role),fps.((role+1) mod 32))]
let step value=SemanticJson.union "RegisterLifetimeStep" (match value with P.Unrelated -> "Unrelated" | P.Overwritten -> "Overwritten" | P.ReadOrControlFlow -> "ReadOrControlFlow") []
let outcome xs=tuple [symbolic xs;symbolic (P.peepholeOptimize xs)]
let int16Cast n=let n=n land 65535 in if n>=32768 then n-65536 else n
let observe source=
 let roles=List.init 32 Fun.id in
 let classifications=list (fun role -> list (fun instr -> tuple [J.symInstr instr;list (fun target -> tuple [step (P.registerLifetimeStep target instr);`Bool (P.overwrittenBeforeReadOrEnd target [instr]);`Bool (P.overwrittenBeforeReadOrEnd target [instr;S.MOVZ (target,0,0)]);`Bool (P.overwrittenBeforeReadOrEnd target [S.FMOV_zero ARM64.D0;instr;S.MOV_reg (ARM64.X0,target)])]) (Array.to_list regs)]) (fixtures source role 7)) roles in
 let suffixes target=[[];[S.MOVZ (target,0,0)];[S.MOV_reg (ARM64.X0,target)];[S.Label source];[S.FMOV_zero ARM64.D0];[S.MOVK (target,1,16)];[S.CMP_imm (target,0)]] in
 let rewrites=list (fun role ->
  let a=regs.(role) and b=regs.((role+1) mod 32) and c=regs.((role+2) mod 32) and d=regs.((role+3) mod 32) in
  let basic=list outcome [[S.MOV_reg (a,a)];[S.FMOV_reg (fps.(role),fps.(role))];[S.ADD_imm (a,a,0)];[S.SUB_imm (a,a,0)];[S.AND_reg (a,a,a)];[S.AND_reg (b,a,a)];[S.ORR_reg (a,a,a)];[S.ORR_reg (b,a,a)];[S.B_label source;S.Label source];[S.B_label (source^"x");S.Label source]] in
  let subtract=list (fun imm -> list (fun cmp -> outcome [S.SUB_imm (a,b,imm);S.CMP_imm (cmp,0)]) [a;b;c]) [0;1;4095;65535] in
  let branches=list (fun condition -> list (fun label -> tuple [outcome [S.CMP_imm (a,0);S.B_cond_label (condition,label)];outcome [S.CMP_imm (a,1);S.B_cond_label (condition,label)];outcome [S.B_cond_label (condition,source);S.B_label label;S.Label source];outcome [S.B_cond_label (condition,source);S.B_label label;S.Label (source^"x")]]) [source;source^"x"]) conditions in
  let bic=list (fun inverted -> list (fun left -> list (fun value -> list (fun suffix -> tuple [outcome ([S.MOVN (a,0,0);S.EOR_reg (inverted,value,a);S.AND_reg (inverted,left,inverted)]@suffix);outcome ([S.MOVN (a,0,0);S.EOR_reg (inverted,a,value);S.AND_reg (inverted,left,inverted)]@suffix)]) (suffixes a)) [a;b;c;d]) [a;b;c;d]) [a;b;c;d] in
  let shifts=list (fun dest -> list (fun shift -> list (fun suffix -> list outcome [ [S.LSL_imm (b,c,shift);S.ADD_reg (dest,a,b)]@suffix;[S.LSL_imm (b,c,shift);S.ADD_reg (dest,b,a)]@suffix;[S.LSL_imm (b,c,shift);S.SUB_reg (dest,a,b)]@suffix;[S.LSL_imm (b,c,shift);S.ADD_reg (dest,c,b)]@suffix;[S.LSL_imm (b,c,shift);S.ADD_reg (dest,b,c)]@suffix;[S.LSL_imm (b,c,shift);S.ADD_reg (dest,b,b)]@suffix]) (suffixes b)) [-1;0;1;63;64]) [a;b;c] in
  let extensions=list (fun extension -> list (fun dest -> list (fun suffix -> tuple [outcome ([extension;S.ADD_reg (dest,a,b)]@suffix);outcome ([extension;S.ADD_reg (dest,b,a)]@suffix);outcome ([extension;S.ADD_reg (dest,b,b)]@suffix)]) (suffixes b)) [a;b;c]) [S.UXTB (b,c);S.UXTH (b,c);S.UXTW (b,c);S.SXTB (b,c);S.SXTH (b,c);S.SXTW (b,c)] in
  let stores=list (fun offset -> list (fun next -> list (fun addr -> tuple [outcome [S.STR (a,addr,offset);S.STR (b,addr,next)];outcome [S.STR_fp (fps.(role),addr,offset);S.STR_fp (fps.((role+1) mod 32),addr,next)]]) [ARM64.SP;a]) [int16Cast (offset+7);int16Cast (offset+8);int16Cast (offset+9)]) [-32768;-16;-1;0;1;8;16;496;504;512;32767] in
  let guarded=list (fun instr -> tuple [outcome [S.MOVN (a,0,0);S.EOR_reg (b,c,a);S.AND_reg (b,d,b);instr];outcome [S.LSL_imm (b,c,1);S.ADD_reg (d,a,b);instr];outcome [S.SXTB (b,c);S.ADD_reg (d,a,b);instr]]) (fixtures source role 7) in
  let combined=outcome [S.SUB_imm (a,b,0);S.CMP_imm (a,0);S.B_cond_label (ARM64.EQ,source);S.B_label (source^"x");S.Label source;S.MOV_reg (a,a);S.ADD_imm (a,a,0);S.LSL_imm (b,c,1);S.ADD_reg (a,c,b);S.SXTB (b,c);S.ADD_reg (a,a,b);S.RET] in
  tuple [basic;subtract;branches;bic;shifts;extensions;stores;guarded;combined]) roles in
 let untouched=list (fun role -> outcome (fixtures source role 7)) roles in
 tuple [classifications;rewrites;untouched;list (fun condition -> J.symInstr (S.B_cond_label (P.invertCondition condition,source))) conditions]
