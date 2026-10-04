(* Full ARM64 integer lowering results, allocation, argument moves and effects. *)
open Dark_compiler
module E=ARM64EmitInteger
module J=MachineISAObservation
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let call f=try tuple [`Bool false; (match f () with Ok xs -> SemanticJson.union "FSharpResult" "Ok" [`List (List.map J.symInstr xs)] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string error])] with Failure _ | Invalid_argument _ -> tuple [`Bool true]
let physical=[LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X8;LIR.X9;LIR.X10;LIR.X11;LIR.X12;LIR.X13;LIR.X14;LIR.X15;LIR.X16;LIR.X17;LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26;LIR.X27;LIR.X29;LIR.X30;LIR.SP]
let fpPhysical=[LIR.D0;LIR.D1;LIR.D2;LIR.D3;LIR.D4;LIR.D5;LIR.D6;LIR.D7;LIR.D8;LIR.D9;LIR.D10;LIR.D11;LIR.D12;LIR.D13;LIR.D14;LIR.D15]
let observe source=
 let ctx={ (ARMPrintingObservation.context source (ARM64.targetConfigFor Platform.LinuxARM64) false) with ARM64CodeGenTypes.functionNames=FunctionIdMap.ofList [AST.functionId 0L,source;AST.functionId (-1L),"largest"] } in
 let gps=List.map (fun p -> LIR.Physical p) physical@List.map (fun n -> LIR.Virtual n) [-1;0;1;2147483647] in
 let fps=List.map (fun p -> LIR.FPhysical p) fpPhysical@List.map (fun n -> LIR.FVirtual n) [-2001;-2000;-1001;-1000;-1;0;1;2147483647] in
 let values=[Int64.min_int;Int64.max_int;-65536L;-4096L;-4L;-1L;0L;1L;4095L;4096L;65535L;65536L;0x123456789abcdef0L] in
 let offsets=[-2147483648;-65536;-4096;-4095;-257;-256;-1;0;255;256;4095;4096;65535;2147483647] in
 let operands=List.map (fun value -> LIR.Imm value) values@List.map (fun reg -> LIR.Reg reg) gps@List.map (fun offset -> LIR.StackSlot offset) offsets@[LIR.FloatImm 0.;LIR.FloatImm (-0.);LIR.FloatImm infinity;LIR.FloatSymbol nan;LIR.StringSymbol source;LIR.FuncAddr (AST.functionId 0L);LIR.FuncAddr (AST.functionId 1L);LIR.FuncAddr (AST.functionId (-1L))] in
 let unary=list (fun dest -> list (fun src -> list (fun f -> call (fun () -> f ctx dest src)) [E.emitNeg;E.emitMvn;E.emitSxtb;E.emitSxth;E.emitSxtw;E.emitUxtb;E.emitUxth;E.emitUxtw]) gps) gps in
 let moves=list (fun dest -> list (fun src -> call (fun () -> E.emitMov ctx dest src)) operands) gps in
 let arithmetic=list (fun dest -> list (fun left -> list (fun right -> tuple [call (fun () -> E.emitAdd ctx dest left right);call (fun () -> E.emitSub ctx dest left right)]) operands) gps) gps in
 let comparisons=list (fun left -> list (fun right -> call (fun () -> E.emitCmp ctx left right)) operands) gps in
 let binary=list (fun dest -> list (fun left -> list (fun right -> list (fun f -> call (fun () -> f ctx dest left right)) [E.emitMul;E.emitSdiv;E.emitUdiv;E.emitAnd;E.emitOrr;E.emitEor;E.emitLsl;E.emitLsr;E.emitAsr]) [dest;left;LIR.Physical LIR.X0;LIR.Physical LIR.X9;LIR.Physical LIR.X27;LIR.Physical LIR.SP;LIR.Virtual (-1)]) gps) gps in
 let fused=list (fun dest -> list (fun a -> list (fun b -> list (fun c -> tuple [call (fun () -> E.emitMadd ctx dest a b c);call (fun () -> E.emitMsub ctx dest a b c)]) [dest;a;b;LIR.Physical LIR.X9;LIR.Virtual (-1)]) [a;LIR.Physical LIR.X0;LIR.Physical LIR.X27;LIR.Virtual (-1)]) gps) gps in
 let immediate=list (fun dest -> list (fun src -> tuple [list (fun shift -> list (fun f -> call (fun () -> f ctx dest src shift)) [E.emitLsl_imm;E.emitLsr_imm;E.emitAsr_imm]) [-2147483648;-1;0;1;31;32;63;64;2147483647];list (fun value -> call (fun () -> E.emitAnd_imm ctx dest src value)) values]) gps) gps in
 let conditions=[LIR.EQ;LIR.NE;LIR.LT;LIR.GT;LIR.LE;LIR.GE;LIR.ULT;LIR.UGT;LIR.ULE;LIR.UGE] in
 let selection=list (fun dest -> list (fun cond -> tuple [call (fun () -> E.emitCset ctx dest cond);list (fun a -> list (fun b -> call (fun () -> E.emitSelect ctx dest a b cond)) [dest;a;LIR.Physical LIR.X0;LIR.Physical LIR.X9;LIR.Virtual (-1)]) gps]) conditions) gps in
 let stores=list (fun src -> list (fun offset -> call (fun () -> E.emitStore ctx offset src)) offsets) gps in
 let captures=[[];[LIR.Imm 0L];[LIR.Imm Int64.min_int;LIR.Imm Int64.max_int;LIR.Reg (LIR.Physical LIR.X0);LIR.Reg (LIR.Physical LIR.X15);LIR.FuncAddr (AST.functionId 0L)]]@List.map (fun operand -> [operand]) operands@[List.init 4100 (fun _ -> LIR.Imm 1L)] in
 let allocations=list (fun enabled -> let ctx={ctx with ARM64CodeGenTypes.options={ARM64CodeGenTypes.defaultOptions with ARM64CodeGenTypes.enableLeakCheck=enabled}} in list (fun dest -> list (fun id -> list (fun captured -> call (fun () -> E.emitClosureAlloc ctx dest (AST.functionId id) captured)) captures) [0L;1L;-1L]) gps) [false;true] in
 let parallelMoves=List.concat_map (fun dest -> List.map (fun src -> [dest,src]) operands) physical @ List.concat_map (fun count -> List.map (fun shift -> List.init count (fun n -> List.nth physical (n mod 30),LIR.Reg (LIR.Physical (List.nth physical ((n+shift) mod 30))))) [0;1;2;7;29]) [0;1;2;3;4;8;16;30;31]@[[LIR.X0,LIR.Reg (LIR.Physical LIR.X1);LIR.X0,LIR.Reg (LIR.Physical LIR.X2)];[LIR.X9,LIR.Imm Int64.max_int;LIR.X0,LIR.Reg (LIR.Physical LIR.X9)]] in
 let arguments=list (fun moved -> tuple [call (fun () -> E.emitArgMoves ctx moved);call (fun () -> E.emitTailArgMoves ctx moved)]) parallelMoves in
 let effects=list (fun target -> list (fun enabled -> let ctx={ctx with ARM64CodeGenTypes.target=target;ARM64CodeGenTypes.options={ARM64CodeGenTypes.defaultOptions with ARM64CodeGenTypes.enableLeakCheck=enabled}} in tuple [call (fun () -> E.emitExit ctx);list (fun effectId -> tuple [list (fun value -> list (fun newline -> call (fun () -> E.emitStdoutWrite ctx effectId value newline)) [false;true]) operands;list (fun reg -> call (fun () -> E.emitStdinReadLine ctx effectId reg)) gps]) [-2147483648;-1;0;2147483647]]) [false;true]) [ARM64.targetConfigFor Platform.MacOSARM64;ARM64.targetConfigFor Platform.LinuxARM64] in
 let errors=tuple [call (fun () -> E.emitRuntimeError ctx source);list (fun reg -> call (fun () -> E.emitRuntimeErrorString ctx reg)) gps] in
 let conversions=list (fun dest -> list (fun src -> tuple [call (fun () -> E.emitInt64ToFloat ctx dest src);call (fun () -> E.emitGpToFp ctx dest src)]) gps) fps in
 tuple [call (fun () -> E.emitPhi ctx);unary;moves;arithmetic;comparisons;binary;fused;immediate;selection;stores;allocations;arguments;effects;errors;conversions]
