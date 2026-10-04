(* Complete native-effect lowering, arity diagnostics, operand aliases and coverage overflow. *)
open Dark_compiler
module E=ARM64EmitNativeEffects
module J=MachineISAObservation
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let call f=try tuple [`Bool false; (match f () with Ok xs -> SemanticJson.union "FSharpResult" "Ok" [`List (List.map J.symInstr xs)] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string error])] with Failure _ | Invalid_argument _ -> tuple [`Bool true]
let physical=[LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X8;LIR.X9;LIR.X10;LIR.X11;LIR.X12;LIR.X13;LIR.X14;LIR.X15;LIR.X16;LIR.X17;LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26;LIR.X27;LIR.X29;LIR.X30;LIR.SP]
let fpPhysical=[LIR.D0;LIR.D1;LIR.D2;LIR.D3;LIR.D4;LIR.D5;LIR.D6;LIR.D7;LIR.D8;LIR.D9;LIR.D10;LIR.D11;LIR.D12;LIR.D13;LIR.D14;LIR.D15]
let operations=[LIR.Execute;LIR.RunProcess;LIR.HostOS;LIR.HostArchitecture;LIR.Hostname;LIR.GetEnv;LIR.GetEnvironmentPacked;LIR.SetEnv;LIR.UnsetEnv;LIR.DirectoryCurrent;LIR.DirectoryListPacked;LIR.FileIsDirectory;LIR.FileCreateExclusive;LIR.GetArgv;LIR.Kill;LIR.GetPid;LIR.GetUid;LIR.CpuCount;LIR.SpawnProcess;LIR.ProcessIO;LIR.TerminateProcess;LIR.SocketTcp4;LIR.SocketTcp6;LIR.SocketUdp4;LIR.SocketUdp6;LIR.SocketConnect4;LIR.SocketConnect6;LIR.SocketSend;LIR.SocketReceive;LIR.SocketReceiveTimeout;LIR.SocketSendTimeout;LIR.SocketClose;LIR.SecureRandomFill]
let observe source=
 let gps=List.map (fun p -> LIR.Physical p) physical@List.map (fun n -> LIR.Virtual n) [-1;0;2147483647] in
 let selected=List.map (fun p -> LIR.Physical p) [LIR.X0;LIR.X1;LIR.X2;LIR.X14;LIR.X15;LIR.X19;LIR.SP]@[LIR.Virtual (-1)] in
 let fps=List.map (fun p -> LIR.FPhysical p) fpPhysical@List.map (fun n -> LIR.FVirtual n) [-2147483648;-2001;-2000;-1003;-1002;-1001;-1000;-2;-1;0;7;8;9999;10000;2147483647] in
 let values=[LIR.Imm Int64.min_int;LIR.Imm Int64.max_int;LIR.Imm (-1L);LIR.Imm 0L;LIR.Imm 4096L;LIR.FloatImm (-0.);LIR.FloatSymbol (Int64.float_of_bits 0xfff8000000000000L);LIR.FuncAddr (AST.functionId (-1L));LIR.StringSymbol source;LIR.StringSymbol "hé😀";LIR.StringSymbol "a\000b";LIR.StringSymbol (HostText.ofUtf16Units [|0xd800;97;0xdc00|])]@List.map (fun n -> LIR.StackSlot n) [-2147483648;-4096;-4095;-257;-256;-1;0;255;256;4095;4096;2147483647]@List.map (fun reg -> LIR.Reg reg) gps in
 list (fun target -> list (fun enabled -> let ctx=ARMPrintingObservation.context source target enabled in tuple [
  list (fun reg -> tuple [call (fun () -> E.emitRandomInt64 ctx reg);call (fun () -> E.emitDateTimeNow ctx reg)]) gps;
  list (fun freg -> list (fun effectId -> call (fun () -> E.emitSleep ctx effectId freg)) [-2147483648;-1;0;2147483647]) fps;
  list (fun operation -> list (fun dest -> list (fun args -> call (fun () -> E.emitCliNative ctx dest operation args)) [[];[LIR.StringSymbol source];[LIR.StringSymbol source;LIR.Imm 1L];[LIR.StringSymbol source;LIR.Reg (LIR.Physical LIR.X15);LIR.Imm 8L];[LIR.Imm 0L;LIR.Imm 1L;LIR.Imm 2L;LIR.Imm 3L]]) gps) operations;
  list (fun operation -> list (fun operand -> list (fun args -> call (fun () -> E.emitCliNative ctx (LIR.Physical LIR.X19) operation args)) [[operand];[operand;LIR.Imm 1L];[LIR.StringSymbol source;operand];[operand;LIR.Imm 0L;LIR.Imm 1L];[LIR.Imm 0L;operand;LIR.Imm 1L];[LIR.Imm 0L;LIR.Imm 1L;operand]]) values) operations;
  list (fun operation -> list (fun dest -> list (fun operand -> list (fun args -> call (fun () -> E.emitCliNative ctx dest operation args)) [[operand];[operand;LIR.Reg dest];[LIR.Reg dest;operand];[operand;operand;LIR.Reg dest]]) [LIR.Reg dest;LIR.Reg (LIR.Physical LIR.X0);LIR.Reg (LIR.Physical LIR.X1);LIR.Reg (LIR.Physical LIR.X14);LIR.Reg (LIR.Physical LIR.X15)]) selected) [LIR.GetEnv;LIR.SetEnv;LIR.Kill;LIR.SecureRandomFill;LIR.SocketConnect4;LIR.SocketSend;LIR.SocketReceive;LIR.ProcessIO];
  list (fun id -> call (fun () -> E.emitCoverageHit ctx id)) [-2147483648;-65536;-513;-512;-511;-1;0;1;511;512;513;8191;8192;268435455;268435456;536870912;2147483647]
 ]) [false;true]) [ARM64.targetConfigFor Platform.LinuxARM64;ARM64.targetConfigFor Platform.MacOSARM64]
