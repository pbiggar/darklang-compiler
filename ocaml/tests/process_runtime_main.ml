(* Execute real ARM64 argument retrieval and presentation effects. *)
open Dark_compiler
module S=Symbolic
let image instructions=
 let sp,fp=ARM64_Resolve.collectPools instructions in
 let words=ARM64_Encoding.encodeSymbolicWithPools instructions sp fp Platform.Linux false in
 Backend_Arm64_Binary_Generation_ELF.createExecutableWithPools words sp fp false
let binary index=
 let target=ARM64.targetConfigFor Platform.LinuxARM64 in
 let ctx=Semantic_observation.ARMPrintingObservation.context "argv-check" target false in
 image ([S.STP_pre (S.X29,S.X30,S.SP,-16);S.MOV_reg (S.X29,S.SP);S.MOVZ (S.X9,0,0);S.STR (S.X9,S.X29,0)]@
 ProcessLifecycle.generateHeapInit target @ ARM64Operands.loadImmediate S.X0 (Int64.of_int index) @
 [S.BL "argv-check";S.CBZ (S.X0,"missing");S.LDR (S.X10,S.X0,8);S.ADD_imm (S.X9,S.X0,16)]@
 S.ofARM64List (PrintValues.generatePrintStringNoNewline target) @ S.ofARM64List (PrintAndExit.generateExit target) @
 [S.Label "missing"] @ S.ofARM64List (PrintValues.generatePrintChars target [78]) @ S.ofARM64List (PrintAndExit.generateExit target) @
 ProcessLifecycle.generateCliArgvHelper ctx "argv-check")
let runImage bytes arguments input=
 let path=Filename.temp_file "port-process-" ".elf" in
 let inputPath=Filename.temp_file "port-input-" ".txt" in
 Fun.protect ~finally:(fun () -> Sys.remove path;Sys.remove inputPath) (fun () ->
  let channel=open_out_bin path in output_bytes channel bytes;close_out channel;Unix.chmod path 0o700;
  let channel=open_out_bin inputPath in output_string channel input;close_out channel;
  let inputFd=Unix.openfile inputPath [Unix.O_RDONLY] 0 in
  let outRead,outWrite=Unix.pipe () and errRead,errWrite=Unix.pipe () in
  let args=Array.of_list ("/opt/dcb/qemu/qemu-aarch64"::path::arguments) in
  let pid=Unix.create_process args.(0) args inputFd outWrite errWrite in
  Unix.close inputFd;Unix.close outWrite;Unix.close errWrite;
  let read descriptor=let channel=Unix.in_channel_of_descr descriptor in let output=In_channel.input_all channel in close_in channel;output in
  let output=read outRead in let errors=read errRead in
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED 0 when errors="" -> output
  | (Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _) as status -> failwith (Printf.sprintf "ARM64 process execution failed (%s): %s" (match status with Unix.WEXITED code -> string_of_int code | Unix.WSIGNALED signal -> "signal "^string_of_int signal | Unix.WSTOPPED signal -> "stop "^string_of_int signal) errors))
let instructions=function Ok xs -> xs | Error message -> failwith message
let presentationBinary literal newline reads=
 let target=ARM64.targetConfigFor Platform.LinuxARM64 in
 let ctx=Semantic_observation.ARMPrintingObservation.context "presentation-check" target false in
 image (ProcessLifecycle.generateHeapInit target @
 (match literal with None -> [] | Some text -> instructions (ARM64EmitInteger.emitStdoutWrite ctx 0 (LIR.StringSymbol text) newline)) @
 List.concat_map (fun index -> instructions (ARM64EmitInteger.emitStdinReadLine ctx index (LIR.Physical LIR.X19)) @ instructions (ARM64EmitInteger.emitStdoutWrite ctx (index+100) (LIR.Reg (LIR.Physical LIR.X19)) newline)) (List.init reads Fun.id) @
 instructions (ARM64EmitInteger.emitExit ctx))
let ()=
 let argvCases=[0,[],"N";(-1),["first"],"N";0,["first";"second"],"first";1,["first";"second"],"second";2,["first";"second"],"N";0,[""],"";0,["hé😀"],"hé😀";2147483647,["first"],"N"] in
 List.iter (fun (index,args,expected) -> let actual=runImage (binary index) args "" in if actual<>expected then failwith (Printf.sprintf "argv[%d]: expected %S, got %S" index expected actual)) argvCases;
 let effects=[Some "hé😀",false,0,"","hé😀";Some "",true,0,"","\n";None,false,1,"hello\nignored","hello";None,true,1,"hé😀\r\n","hé😀\n";None,false,1,"tail","tail";None,true,1,"","\n";None,true,2,"first\nsecond\n","first\nsecond\n";None,false,1,"a\000b\n","a\000b";None,false,1,"\n","";None,false,1,"a\rb\r\n","a\rb"] in
 List.iter (fun (literal,newline,reads,input,expected) -> let actual=runImage (presentationBinary literal newline reads) [] input in if actual<>expected then failwith (Printf.sprintf "presentation input %S: expected %S, got %S" input expected actual)) effects;
 let count=List.length argvCases+List.length effects in
 Printf.printf "%d/%d native ARM64 process executions passed\n" count count
