(* Execute real ARM64 argument retrieval from the initial process stack. *)
open Dark_compiler
module S=Symbolic
let binary index=
 let target=ARM64.targetConfigFor Platform.LinuxARM64 in
 let ctx=Semantic_observation.ARMPrintingObservation.context "argv-check" target false in
 let instructions=[S.STP_pre (S.X29,S.X30,S.SP,-16);S.MOV_reg (S.X29,S.SP);S.MOVZ (S.X9,0,0);S.STR (S.X9,S.X29,0)]@
 ProcessLifecycle.generateHeapInit target @ ARM64Operands.loadImmediate S.X0 (Int64.of_int index) @
 [S.BL "argv-check";S.CBZ (S.X0,"missing");S.LDR (S.X10,S.X0,8);S.ADD_imm (S.X9,S.X0,16)]@
 S.ofARM64List (PrintValues.generatePrintStringNoNewline target) @ S.ofARM64List (PrintAndExit.generateExit target) @
 [S.Label "missing"] @ S.ofARM64List (PrintValues.generatePrintChars target [78]) @ S.ofARM64List (PrintAndExit.generateExit target) @
 ProcessLifecycle.generateCliArgvHelper ctx "argv-check" in
 let sp,fp=ARM64_Resolve.collectPools instructions in
 let words=ARM64_Encoding.encodeSymbolicWithPools instructions sp fp Platform.Linux false in
 Backend_Arm64_Binary_Generation_ELF.createExecutableWithPools words sp fp false
let run index arguments=
 let path=Filename.temp_file "port-argv-" ".elf" in
 Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
  let channel=open_out_bin path in output_bytes channel (binary index);close_out channel;Unix.chmod path 0o700;
  let outRead,outWrite=Unix.pipe () and errRead,errWrite=Unix.pipe () in
  let args=Array.of_list ("/opt/dcb/qemu/qemu-aarch64"::path::arguments) in
  let pid=Unix.create_process args.(0) args Unix.stdin outWrite errWrite in
  Unix.close outWrite;Unix.close errWrite;
  let read descriptor=let channel=Unix.in_channel_of_descr descriptor in let output=In_channel.input_all channel in close_in channel;output in
  let output=read outRead in let errors=read errRead in
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED 0 when errors="" -> output
  | (Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _) as status -> failwith (Printf.sprintf "ARM64 argv execution failed (%s): %s" (match status with Unix.WEXITED code -> string_of_int code | Unix.WSIGNALED signal -> "signal "^string_of_int signal | Unix.WSTOPPED signal -> "stop "^string_of_int signal) errors))
let ()=
 let cases=[0,[],"N";(-1),["first"],"N";0,["first";"second"],"first";1,["first";"second"],"second";2,["first";"second"],"N";0,[""],"";0,["hé😀"],"hé😀";2147483647,["first"],"N"] in
 List.iter (fun (index,args,expected) -> let actual=run index args in if actual<>expected then failwith (Printf.sprintf "argv[%d]: expected %S, got %S" index expected actual)) cases;
 Printf.printf "%d/%d native ARM64 argv executions passed\n" (List.length cases) (List.length cases)
