(* Native effects and every CLI operation, argument shape and register alias. *)
open Dark_compiler
module E=X64EmitNativeEffects
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let call f=try tuple [`Bool false;(match f () with Ok xs->SemanticJson.union "FSharpResult" "Ok" [`List (List.map MachineISAObservation.x64Instr xs)] | Error e->SemanticJson.union "FSharpResult" "Error" [SemanticJson.string e])] with Failure e | Invalid_argument e->tuple [`Bool true;SemanticJson.string e]
let observe source=
 let physical=[LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X8;LIR.X9;LIR.X10;LIR.X11;LIR.X12;LIR.X13;LIR.X14;LIR.X15;LIR.X16;LIR.X17;LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26;LIR.X27;LIR.X29;LIR.X30;LIR.SP] in
 let gps=List.map (fun p->LIR.Physical p) physical@[LIR.Virtual (-1);LIR.Virtual 0;LIR.Virtual 2147483647] in
 let operations=[LIR.Execute;LIR.RunProcess;LIR.HostOS;LIR.HostArchitecture;LIR.Hostname;LIR.GetEnv;LIR.GetEnvironmentPacked;LIR.SetEnv;LIR.UnsetEnv;LIR.DirectoryCurrent;LIR.DirectoryListPacked;LIR.FileIsDirectory;LIR.FileCreateExclusive;LIR.GetArgv;LIR.Kill;LIR.GetPid;LIR.GetUid;LIR.CpuCount;LIR.SpawnProcess;LIR.ProcessIO;LIR.TerminateProcess;LIR.SocketTcp4;LIR.SocketTcp6;LIR.SocketUdp4;LIR.SocketUdp6;LIR.SocketConnect4;LIR.SocketConnect6;LIR.SocketSend;LIR.SocketReceive;LIR.SocketReceiveTimeout;LIR.SocketSendTimeout;LIR.SocketClose;LIR.SecureRandomFill] in
 let selected=List.map (fun p->LIR.Physical p) [LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X6;LIR.X8;LIR.X19]@[LIR.Virtual (-1)] in
 let ctx enabled={X64CodeGenTypes.functionName=source;stackSize=32;usedCalleeSaved=[LIR.X19;LIR.X20];enableLeakCheck=enabled;recordRegistry=StringOrder.Map.empty;sumShapeRegistry=StringOrder.Map.empty;functionNames=FunctionIdMap.empty} in
 let c=ctx false in
 let operands=List.map (fun reg->LIR.Reg reg) gps@List.map (fun n->LIR.StackSlot n) [-2147483648;-1;0;8;2147483647]@[LIR.Imm Int64.min_int;LIR.Imm (-1L);LIR.Imm 0L;LIR.Imm 1L;LIR.Imm Int64.max_int;LIR.FloatImm (-0.);LIR.FloatImm nan;LIR.FloatSymbol 0.1;LIR.StringSymbol source;LIR.StringSymbol "hé😀";LIR.FuncAddr (AST.functionId (-1L))] in
 let main=list (fun enabled->let c=ctx enabled in list (fun dest->list (fun operation->list (fun args->call (fun ()->E.emitCliNative c dest operation args)) [[];[LIR.Imm 0L];[LIR.Imm 0L;LIR.Imm 1L];[LIR.Imm 0L;LIR.Imm 1L;LIR.Imm 2L];[LIR.Reg dest];[LIR.Reg dest;LIR.Reg (LIR.Physical LIR.X3)];[LIR.Reg dest;LIR.Reg (LIR.Physical LIR.X3);LIR.Reg (LIR.Physical LIR.X8)];[LIR.Imm 0L;LIR.Imm 1L;LIR.Imm 2L;LIR.Imm 3L]]) operations) gps) [false;true] in
 let argOperations=List.filter (fun operation->not (List.mem operation [LIR.HostOS;LIR.HostArchitecture;LIR.Hostname;LIR.GetPid;LIR.GetUid;LIR.CpuCount;LIR.GetEnvironmentPacked;LIR.DirectoryCurrent;LIR.SocketTcp4;LIR.SocketTcp6;LIR.SocketUdp4;LIR.SocketUdp6])) operations in
 let arguments=list (fun dest->list (fun operation->list (fun operand->list (fun args->call (fun ()->E.emitCliNative c dest operation args)) [[operand];[operand;LIR.Reg (LIR.Physical LIR.X1)];[LIR.Reg (LIR.Physical LIR.X2);operand];[LIR.Reg (LIR.Physical LIR.X1);LIR.Reg (LIR.Physical LIR.X2);operand]]) operands) argOperations) selected in
 let system=list (fun reg->list call [(fun ()->E.emitRandomInt64 c reg);(fun ()->E.emitDateTimeNow c reg)]) gps in
 let floats=List.map (fun p->LIR.FPhysical p) [LIR.D0;LIR.D1;LIR.D2;LIR.D3;LIR.D4;LIR.D5;LIR.D6;LIR.D7;LIR.D8;LIR.D9;LIR.D10;LIR.D11;LIR.D12;LIR.D13;LIR.D14;LIR.D15]@[LIR.FVirtual (-1);LIR.FVirtual 0;LIR.FVirtual 2147483647] in
 let sleeps=list (fun id->list (fun delay->call (fun ()->E.emitSleep c id delay)) floats) [-2147483648;-1;0;1;8;2147483647] in
 let coverage=call (fun ()->E.emitCoverageHit c) in
 tuple [main;arguments;system;sleeps;coverage]
