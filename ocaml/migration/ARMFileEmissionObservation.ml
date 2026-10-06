(* Complete file lowering results with physical aliases, path forms and syscall targets. *)
open Dark_compiler
module E=ARM64EmitFiles
module J=MachineISAObservation
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let call f=try tuple [`Bool false; (match f () with Ok xs -> SemanticJson.union "FSharpResult" "Ok" [`List (List.map J.symInstr xs)] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string error])] with Failure _ | Invalid_argument _ -> tuple [`Bool true]
let physical=[LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X8;LIR.X9;LIR.X10;LIR.X11;LIR.X12;LIR.X13;LIR.X14;LIR.X15;LIR.X16;LIR.X17;LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26;LIR.X27;LIR.X29;LIR.X30;LIR.SP]
let observe source=
 let gps=List.map (fun p -> LIR.Physical p) physical@List.map (fun n -> LIR.Virtual n) [-1;0;2147483647] in
 let offsets=[-2147483648;-4096;-4095;-257;-256;-1;0;255;256;4095;4096;2147483647] in
 let operands=List.map (fun reg -> LIR.Reg reg) gps@List.map (fun n -> LIR.StackSlot n) offsets@[LIR.Imm Int64.min_int;LIR.FloatImm (-0.);LIR.FloatSymbol nan;LIR.FuncAddr (AST.functionId (-1L));LIR.StringSymbol source;LIR.StringSymbol "";LIR.StringSymbol "hé😀";LIR.StringSymbol "a\000b";LIR.StringSymbol (HostText.ofScalars [|0xfffd;0x61;0xfffd|])] in
 let selected=List.map (fun p -> LIR.Physical p) [LIR.X0;LIR.X9;LIR.X14;LIR.X15;LIR.X19;LIR.SP]@[LIR.Virtual (-1)] in
 let observeContext target enabled=
  let ctx=ARMPrintingObservation.context source target enabled in
  let fullPaths=list (fun dest -> list (fun path -> list (fun f -> call (fun () -> f ctx dest path)) [E.emitFileReadBlob;E.emitFileExists;E.emitFileDelete;E.emitFileCreateDirectory;E.emitFileSetExecutable]) operands) selected in
  let fullDestinations=list (fun dest -> list (fun path -> list (fun f -> call (fun () -> f ctx dest path)) [E.emitFileReadBlob;E.emitFileExists;E.emitFileDelete;E.emitFileCreateDirectory;E.emitFileSetExecutable]) [LIR.Reg dest;LIR.Reg (LIR.Physical LIR.X15);LIR.StringSymbol source;LIR.StackSlot 0]) gps in
  let writing=list (fun dest -> list (fun path -> list (fun content -> tuple [call (fun () -> E.emitFileWriteBlob ctx dest path content);call (fun () -> E.emitFileAppendText ctx dest path content)]) [LIR.Reg dest;path;LIR.Reg (LIR.Physical LIR.X15);LIR.Reg (LIR.Physical LIR.X14);LIR.Reg (LIR.Physical LIR.X0);LIR.StringSymbol source;LIR.StackSlot 4096;LIR.FloatImm 0.]) [LIR.Reg dest;LIR.Reg (LIR.Physical LIR.X15);LIR.Reg (LIR.Physical LIR.X14);LIR.StringSymbol source;LIR.StackSlot (-4095);LIR.Imm 0L]) selected in
  let writingPaths=list (fun operand -> tuple [call (fun () -> E.emitFileWriteBlob ctx (LIR.Physical LIR.X19) operand (LIR.StringSymbol source));call (fun () -> E.emitFileAppendText ctx (LIR.Physical LIR.X19) (LIR.StringSymbol source) operand)]) operands in
  let pointers=list (fun dest -> list (fun ptr -> list (fun length -> list (fun path -> call (fun () -> E.emitFileWriteFromPtr ctx dest path ptr length)) [LIR.Reg dest;LIR.Reg ptr;LIR.Reg length;LIR.Reg (LIR.Physical LIR.X15);LIR.StringSymbol source;LIR.StackSlot 0;LIR.Imm 1L]) [dest;ptr;LIR.Physical LIR.X14;LIR.Virtual 0]) selected) selected in
  let pointerRoles=list (fun reg -> tuple [call (fun () -> E.emitFileWriteFromPtr ctx reg (LIR.Reg reg) reg reg);call (fun () -> E.emitFileWriteFromPtr ctx (LIR.Physical LIR.X19) (LIR.StringSymbol source) reg (LIR.Physical LIR.X20));call (fun () -> E.emitFileWriteFromPtr ctx (LIR.Physical LIR.X19) (LIR.StringSymbol source) (LIR.Physical LIR.X20) reg)]) gps in
  tuple [fullPaths;fullDestinations;writing;writingPaths;pointers;pointerRoles]
 in list (fun target -> list (fun enabled -> observeContext target enabled) [false;true]) [ARM64.targetConfigFor Platform.LinuxARM64;ARM64.targetConfigFor Platform.MacOSARM64]
