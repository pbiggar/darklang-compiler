(* Complete basic ARM64 lowering results, parallel moves and save layouts. *)
open Dark_compiler
module F=ARM64EmitFloatingPoint
module C=ARM64EmitCalls
module J=MachineISAObservation
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let symbolic=list J.symInstr
let result=function Ok xs -> SemanticJson.union "FSharpResult" "Ok" [symbolic xs] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string error]
let call f=try tuple [`Bool false;result (f ())] with Failure _ | Invalid_argument _ -> tuple [`Bool true]
let physical=[LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X8;LIR.X9;LIR.X10;LIR.X11;LIR.X12;LIR.X13;LIR.X14;LIR.X15;LIR.X16;LIR.X17;LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26;LIR.X27;LIR.X29;LIR.X30;LIR.SP]
let fpPhysical=[LIR.D0;LIR.D1;LIR.D2;LIR.D3;LIR.D4;LIR.D5;LIR.D6;LIR.D7;LIR.D8;LIR.D9;LIR.D10;LIR.D11;LIR.D12;LIR.D13;LIR.D14;LIR.D15]
let virtualIds=[-2147483648;-2001;-2000;-1003;-1002;-1001;-1000;-14;-9;-8;-2;-1;0;1;7;8;9;20;9999;10000;10001;12001;2147483647]
let observe source=
 let target=ARM64.targetConfigFor Platform.LinuxARM64 in
 let ctx={ (ARMPrintingObservation.context source target false) with ARM64CodeGenTypes.functionNames=FunctionIdMap.ofList [AST.functionId 0L,source;AST.functionId (-1L),"largest"] } in
 let gps=List.map (fun p -> LIR.Physical p) physical@List.map (fun n -> LIR.Virtual n) [-1;0;1;2147483647] in
 let fps=List.map (fun p -> LIR.FPhysical p) fpPhysical@List.map (fun n -> LIR.FVirtual n) virtualIds in
 let unary=list (fun dest -> list (fun src -> list (fun f -> call (fun () -> f ctx dest src)) [F.emitFMov;F.emitFNeg;F.emitFAbs;F.emitFSqrt]) fps) fps in
 let binary=list (fun dest -> list (fun left -> list (fun right -> list (fun f -> call (fun () -> f ctx dest left right)) [F.emitFAdd;F.emitFSub;F.emitFMul;F.emitFDiv]) fps) fps) fps in
 let physicalFPs=List.map (fun p -> LIR.FPhysical p) fpPhysical in
 let fused=list (fun dest -> list (fun left -> list (fun right -> list (fun addend -> call (fun () -> F.emitFMadd ctx dest left right addend)) physicalFPs) physicalFPs) physicalFPs) physicalFPs in
 let fusedVirtual=list (fun reg -> tuple [call (fun () -> F.emitFMadd ctx reg (LIR.FPhysical LIR.D0) (LIR.FPhysical LIR.D1) (LIR.FPhysical LIR.D2));call (fun () -> F.emitFMadd ctx (LIR.FPhysical LIR.D0) reg reg reg)]) fps in
 let comparisons=list (fun left -> list (fun right -> call (fun () -> F.emitFCmp ctx left right)) fps) fps in
 let values=[0.;-0.;0.1;Int64.float_of_bits 1L;Float.max_float;infinity;neg_infinity;Int64.float_of_bits 0x7ff8000000000001L;Int64.float_of_bits 0xfff8000000000011L]@List.init 256 (fun encoded -> let sign=if encoded land 128=0 then 1. else -1. in let exponent=(encoded lsr 4) land 7 in sign*.(1.+.float_of_int (encoded land 15)/.16.)*.Float.ldexp 1. (if exponent>=4 then exponent-7 else exponent+1)) in
 let loads=list (fun dest -> list (fun value -> call (fun () -> F.emitFLoad ctx dest value)) values) fps in
 let spills=list (fun reg -> list (fun offset -> tuple [call (fun () -> F.emitFSpillLoad ctx reg offset);call (fun () -> F.emitFSpillStore ctx offset reg)]) [-2147483648;-4130;-4096;-4095;-257;-256;-1;0;255;256;4095;4096;65535;2147483647]) fps in
 let conversions=list (fun enabled -> let ctx={ctx with ARM64CodeGenTypes.options={ARM64CodeGenTypes.defaultOptions with ARM64CodeGenTypes.enableLeakCheck=enabled}} in list (fun dest -> list (fun src -> list (fun f -> call (fun () -> f ctx dest src)) [F.emitFloatToInt64;F.emitFpToGp;F.emitFloatToBits;F.emitFloatToString]) fps) gps) [false;true] in
 let moves=List.concat_map (fun dest -> List.map (fun src -> [dest,src]) fps) fpPhysical @ List.concat_map (fun count -> List.map (fun shift -> List.init count (fun n -> List.nth fpPhysical (n mod 16),LIR.FPhysical (List.nth fpPhysical ((n+shift) mod 16)))) [0;1;2;3;7]) [0;1;2;3;4;8;16;17] @ [[LIR.D0,LIR.FPhysical LIR.D1;LIR.D0,LIR.FPhysical LIR.D2];[LIR.D0,LIR.FVirtual (-2000);LIR.D1,LIR.FVirtual (-1000)]] in
 let argumentMoves=list (fun moves -> call (fun () -> F.emitFArgMoves ctx moves)) moves in
 let operands=[[];[LIR.Imm Int64.min_int;LIR.Reg (LIR.Virtual (-1));LIR.StackSlot (-4096);LIR.FloatImm (-0.);LIR.StringSymbol source;LIR.FuncAddr (AST.functionId (-1L))]] in
 let contexts=[ctx;{ctx with ARM64CodeGenTypes.stackSize=16;ARM64CodeGenTypes.usedCalleeSaved=[LIR.X19;LIR.X20;LIR.X21];ARM64CodeGenTypes.usedCalleeSavedF=[LIR.D8;LIR.D9;LIR.D10]};{ctx with ARM64CodeGenTypes.stackSize=65535;ARM64CodeGenTypes.usedCalleeSaved=physical;ARM64CodeGenTypes.usedCalleeSavedF=fpPhysical};{ctx with ARM64CodeGenTypes.stackSize=(-2147483648);ARM64CodeGenTypes.usedCalleeSaved=[LIR.X19;LIR.X19];ARM64CodeGenTypes.usedCalleeSavedF=[LIR.D8]}] in
 let calls=list (fun ctx -> list (fun reg -> tuple [list (fun id -> tuple [list (fun args -> tuple [call (fun () -> C.emitCall ctx reg (AST.functionId id) args);call (fun () -> C.emitTailCall ctx (AST.functionId id) args)]) operands;call (fun () -> C.emitLoadFuncAddr ctx reg (AST.functionId id))]) [0L;1L;-1L];list (fun args -> tuple [call (fun () -> C.emitIndirectCall ctx (LIR.Virtual (-1)) reg args);call (fun () -> C.emitIndirectTailCall ctx reg args);call (fun () -> C.emitClosureCall ctx (LIR.Virtual (-1)) reg args);call (fun () -> C.emitClosureTailCall ctx reg args)]) operands]) gps) contexts in
 let gpSaved=[]::List.map (fun reg -> [reg]) physical@List.concat_map (fun a -> List.map (fun b -> [a;b]) physical) physical@[physical;List.rev physical;[LIR.X19;LIR.X19;LIR.X20]] in
 let fpSaved=[]::List.map (fun reg -> [reg]) fpPhysical@[fpPhysical;List.rev fpPhysical;[LIR.D8;LIR.D8;LIR.D9];[LIR.D0;LIR.D15];[LIR.D15;LIR.D0]] in
 let saves=list (fun gp -> list (fun fp -> tuple [call (fun () -> C.emitSaveRegs ctx gp fp);call (fun () -> C.emitRestoreRegs ctx gp fp)]) fpSaved) gpSaved in
 let fpPairs=list (fun a -> list (fun b -> list (fun gp -> tuple [call (fun () -> C.emitSaveRegs ctx gp [a;b]);call (fun () -> C.emitRestoreRegs ctx gp [a;b])]) [[];[LIR.X0];[LIR.X19;LIR.X20;LIR.X21]]) fpPhysical) fpPhysical in
 let types=[AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TInt128;AST.TInt;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TUInt128;AST.TBool;AST.TFloat64;AST.TString;AST.TBlob;AST.TChar;AST.TDateTime;AST.TUnit;AST.TNever;AST.TInternalRawPtr;AST.TVar source;AST.TInferenceVar (source,source);AST.TList AST.TString;AST.TStream AST.TString;AST.TDict (AST.TString,AST.TInt64);AST.TFunction ([AST.TInt64],AST.TBool);AST.TRecord (source,[]);AST.TSum (source,[]);AST.TTuple [];AST.TTuple [AST.TInt64];AST.TTuple [AST.TInt64;AST.TUInt64;AST.TBool;AST.TFloat64;AST.TString;AST.TChar;AST.TBlob;AST.TTuple [AST.TInt64]]] in
 let printing=list (fun target -> let ctx=ARMPrintingObservation.context source target false in list (fun role -> let reg=List.nth (Array.to_list ARMRuntimeObservation.regs) role in list (fun typ -> list (fun newline -> symbolic (ARM64InstructionContext.generatePrintListInstrs ctx reg typ newline)) [false;true]) types) (List.init 32 Fun.id)) [ARM64.targetConfigFor Platform.MacOSARM64;target] in
 let largeTuple=list (fun target -> let ctx=ARMPrintingObservation.context source target false in list (fun newline -> symbolic (ARM64InstructionContext.generatePrintListInstrs ctx ARM64.X19 (AST.TTuple (List.init 4097 (fun _ -> AST.TUnit))) newline)) [false;true]) [ARM64.targetConfigFor Platform.MacOSARM64;target] in
 tuple [call (fun () -> F.emitFPhi ctx);unary;binary;fused;fusedVirtual;comparisons;loads;spills;conversions;argumentMoves;calls;saves;fpPairs;printing;largeTuple]
