(* Execute real ARM64 argument, presentation, file and buffer effects. *)
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
let runImageOn emulator bytes arguments input=
 let path=Filename.temp_file "port-process-" ".elf" in
 let inputPath=Filename.temp_file "port-input-" ".txt" in
 Fun.protect ~finally:(fun () -> Sys.remove path;Sys.remove inputPath) (fun () ->
  let channel=open_out_bin path in output_bytes channel bytes;close_out channel;Unix.chmod path 0o700;
  let channel=open_out_bin inputPath in output_string channel input;close_out channel;
  let inputFd=Unix.openfile inputPath [Unix.O_RDONLY] 0 in
  let outRead,outWrite=Unix.pipe () and errRead,errWrite=Unix.pipe () in
  let args=Array.of_list (emulator::path::arguments) in
  let pid=Unix.create_process args.(0) args inputFd outWrite errWrite in
  Unix.close inputFd;Unix.close outWrite;Unix.close errWrite;
  let read descriptor=let channel=Unix.in_channel_of_descr descriptor in let output=In_channel.input_all channel in close_in channel;output in
  let output=read outRead in let errors=read errRead in
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED 0 when errors="" -> output
  | (Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _) as status -> failwith (Printf.sprintf "Process execution failed (%s): %s" (match status with Unix.WEXITED code -> string_of_int code | Unix.WSIGNALED signal -> "signal "^string_of_int signal | Unix.WSTOPPED signal -> "stop "^string_of_int signal) errors))
let runImage bytes arguments input=runImageOn "/opt/dcb/qemu/qemu-aarch64" bytes arguments input
let instructions=function Ok xs -> xs | Error message -> failwith message
let presentationBinary literal newline reads=
 let target=ARM64.targetConfigFor Platform.LinuxARM64 in
 let ctx=Semantic_observation.ARMPrintingObservation.context "presentation-check" target false in
 image (ProcessLifecycle.generateHeapInit target @
 (match literal with None -> [] | Some text -> instructions (ARM64EmitInteger.emitStdoutWrite ctx 0 (LIR.StringSymbol text) newline)) @
 List.concat_map (fun index -> instructions (ARM64EmitInteger.emitStdinReadLine ctx index (LIR.Physical LIR.X19)) @ instructions (ARM64EmitInteger.emitStdoutWrite ctx (index+100) (LIR.Reg (LIR.Physical LIR.X19)) newline)) (List.init reads Fun.id) @
 instructions (ARM64EmitInteger.emitExit ctx))
let fileBinary operation boxed printPayload=
 let target=ARM64.targetConfigFor Platform.LinuxARM64 in
 let ctx=Semantic_observation.ARMPrintingObservation.context "file-check" target false in
 image (ProcessLifecycle.generateHeapInit target @ operation ctx @
 (if boxed then [S.LDR (S.X0,S.X19,0)] else [S.MOV_reg (S.X0,S.X19)]) @
 S.ofARM64List (PrintValues.generatePrintInt64NoNewline target) @
 (if printPayload then [S.LDR (S.X20,S.X19,8)] @ instructions (ARM64EmitInteger.emitStdoutWrite ctx 0 (LIR.Reg (LIR.Physical LIR.X20)) false) else []) @
 instructions (ARM64EmitInteger.emitExit ctx))
let filesystemChecks ()=
 let path=Filename.temp_file "port-files-" ".bin" in
 let directory=path^".dir" in
 let missing=path^".missing" in
 let dest=LIR.Physical LIR.X19 in
 let execute expected boxed payload operation=
  let actual=runImage (fileBinary operation boxed payload) [] "" in
  if actual<>expected then failwith (Printf.sprintf "file execution: expected %S, got %S" expected actual)
 in
 let readFile ()=let channel=open_in_bin path in let bytes=In_channel.input_all channel in close_in channel;bytes in
 Fun.protect ~finally:(fun () -> if Sys.file_exists path then Sys.remove path;if Sys.file_exists directory then Unix.rmdir directory) (fun () ->
  execute "1" false false (fun ctx -> instructions (ARM64EmitFiles.emitFileExists ctx dest (LIR.StringSymbol path)));
  execute "0" false false (fun ctx -> instructions (ARM64EmitFiles.emitFileExists ctx dest (LIR.StringSymbol missing)));
  let contents="hé😀\000blob" in
  execute "0" true false (fun ctx -> instructions (ARM64EmitFiles.emitFileWriteBlob ctx dest (LIR.StringSymbol path) (LIR.StringSymbol contents)));
  if readFile ()<>contents then failwith "FileWriteBlob contents differ";
  execute "0" true false (fun ctx -> instructions (ARM64EmitFiles.emitFileAppendText ctx dest (LIR.StringSymbol path) (LIR.StringSymbol "tail")));
  if readFile ()<>contents^"tail" then failwith "FileAppendText contents differ";
  execute ("0"^contents^"tail") true true (fun ctx -> instructions (ARM64EmitFiles.emitFileReadBlob ctx dest (LIR.StringSymbol path)));
  execute "0" true false (fun ctx -> instructions (ARM64EmitFiles.emitFileCreateDirectory ctx dest (LIR.StringSymbol directory)));
  if (Unix.stat directory).Unix.st_kind<>Unix.S_DIR then failwith "FileCreateDirectory did not create directory";
  execute "1" true false (fun ctx -> instructions (ARM64EmitFiles.emitFileCreateDirectory ctx dest (LIR.StringSymbol directory)));
  Unix.chmod path 0o600;
  execute "0" true false (fun ctx -> instructions (ARM64EmitFiles.emitFileSetExecutable ctx dest (LIR.StringSymbol path)));
  if (Unix.stat path).Unix.st_perm land 0o111=0 then failwith "FileSetExecutable did not set execution permission";
  let raw="raw\000hé😀" in
  execute "1" false false (fun ctx -> HeapAllocation.loadStringLiteralPointer S.X20 raw @ [S.ADD_imm (S.X20,S.X20,16)] @ ARM64Operands.loadImmediate S.X21 (Int64.of_int (String.length raw)) @ instructions (ARM64EmitFiles.emitFileWriteFromPtr ctx dest (LIR.StringSymbol path) (LIR.Physical LIR.X20) (LIR.Physical LIR.X21)));
  if readFile ()<>raw then failwith "FileWriteFromPtr contents differ";
  execute "0" true false (fun ctx -> instructions (ARM64EmitFiles.emitFileDelete ctx dest (LIR.StringSymbol path)));
  if Sys.file_exists path then failwith "FileDelete did not remove file";
  execute "1" true false (fun ctx -> instructions (ARM64EmitFiles.emitFileReadBlob ctx dest (LIR.StringSymbol missing)));
  11)
let bufferChecks ()=
 let target=ARM64.targetConfigFor Platform.LinuxARM64 in
 let ctx=Semantic_observation.ARMPrintingObservation.context "buffer-check" target false in
 let expect expected body=
  let actual=runImage (image (ProcessLifecycle.generateHeapInit target @ body @ instructions (ARM64EmitInteger.emitExit ctx))) [] "" in
  if actual<>expected then failwith (Printf.sprintf "buffer execution: expected %S, got %S" expected actual)
 in
 let counter=ref 0 in
 let check expected body=incr counter;expect expected body in
 let kinds=[MemoryModel.Utf8String;MemoryModel.NullableUtf8String;MemoryModel.GraphemeCluster;MemoryModel.NullableGraphemeCluster] in
 List.iter (fun kind ->
  List.iter (fun (left,right,equal) ->
   let copies=instructions (ARM64EmitBuffers.emitStringConcat ctx (LIR.Physical LIR.X19) (LIR.StringSymbol left) (LIR.StringSymbol "") []) @ instructions (ARM64EmitBuffers.emitStringConcat ctx (LIR.Physical LIR.X20) (LIR.StringSymbol right) (LIR.StringSymbol "") []) in
   check (if equal then "1" else "0") (copies @ instructions (ARM64EmitBuffers.emitCanonicalBufferEq ctx kind (LIR.Physical LIR.X21) (LIR.Reg (LIR.Physical LIR.X19)) (LIR.Reg (LIR.Physical LIR.X20))) @ [S.MOV_reg (S.X0,S.X21)] @ S.ofARM64List (PrintValues.generatePrintInt64NoNewline target)))
   (List.concat_map (fun count -> let text=String.make count 'a' in [text,text,true;text,text^"b",false;text,"b"^text,false]) [0;1;7;8;9;16;17] @ ["hé😀","hé😀",true;"a\000b","a\000b",true;"é","é",false])) kinds;
 List.iter (fun kind -> List.iter (fun (left,right,equal) ->
  check (if equal then "1" else "0") (ARM64Operands.loadImmediate S.X19 left @ ARM64Operands.loadImmediate S.X20 right @ instructions (ARM64EmitBuffers.emitCanonicalBufferEq ctx kind (LIR.Physical LIR.X21) (LIR.Reg (LIR.Physical LIR.X19)) (LIR.Reg (LIR.Physical LIR.X20))) @ [S.MOV_reg (S.X0,S.X21)] @ S.ofARM64List (PrintValues.generatePrintInt64NoNewline target))) [0L,0L,true;0L,1L,false;1L,0L,false]) [MemoryModel.NullableUtf8String;MemoryModel.NullableGraphemeCluster];
 List.iter (fun parts -> match parts with
 | first::second::remaining ->
  List.iter (fun dynamic ->
   let setup,left,right=if dynamic then HeapAllocation.loadStringLiteralPointer S.X19 first @ HeapAllocation.loadStringLiteralPointer S.X20 second,LIR.Reg (LIR.Physical LIR.X19),LIR.Reg (LIR.Physical LIR.X20) else [],LIR.StringSymbol first,LIR.StringSymbol second in
   check (String.concat "" parts) (setup @ instructions (ARM64EmitBuffers.emitStringConcat ctx (LIR.Physical LIR.X21) left right (List.map (fun part -> LIR.StringSymbol part) remaining)) @ instructions (ARM64EmitInteger.emitStdoutWrite ctx 0 (LIR.Reg (LIR.Physical LIR.X21)) false))) [false;true]
 | [] | [_] -> failwith "concat execution fixture requires two parts") [["";""];["hé";"😀"];["a\000";"b"];["";"a";"";"é";"😀"];List.init 20 (fun n -> string_of_int n)];
 !counter
let memoryChecks ()=
 let target=ARM64.targetConfigFor Platform.LinuxARM64 in
 let initial=Semantic_observation.ARMPrintingObservation.context "memory-check" target false in
 let print=S.ofARM64List (PrintValues.generatePrintInt64NoNewline target) in
 let count=ref 0 in
 let check disabled expected generate=
  let ctx={initial with ARM64CodeGenTypes.options={ARM64CodeGenTypes.defaultOptions with ARM64CodeGenTypes.disableFreeList=disabled}} in
  let body=ProcessLifecycle.generateHeapInit target @ generate ctx @ instructions (ARM64EmitInteger.emitExit ctx) @ HeapAllocation.generateHeapOverflowTrapBlock (HeapAllocation.preparedHeapOverflowTrapBody target) ctx.ARM64CodeGenTypes.heapOverflowLabel @ HeapAllocation.generateRuntimeErrorHelper target in
  let actual=runImage (image body) [] "" in
  if actual<>expected then failwith (Printf.sprintf "memory execution: expected %S, got %S" expected actual);
  incr count
 in
 List.iter (fun disabled -> List.iter (fun size ->
  check disabled "421" (fun ctx -> instructions (ARM64EmitMemory.emitHeapAlloc ctx (LIR.Physical LIR.X19) size) @ instructions (ARM64EmitMemory.emitHeapStore ctx (LIR.Physical LIR.X19) 0 (LIR.Imm 42L) None) @ instructions (ARM64EmitMemory.emitHeapLoad ctx (LIR.Physical LIR.X0) (LIR.Physical LIR.X19) 0) @ print @ instructions (ARM64EmitMemory.emitHeapLoad ctx (LIR.Physical LIR.X0) (LIR.Physical LIR.X19) size) @ print)) [8;16;24;248]) [false;true];
 List.iter (fun disabled ->
  check disabled "9223372036854775807255" (fun ctx -> ARM64Operands.loadImmediate S.X20 8L @ instructions (ARM64EmitMemory.emitRawAlloc ctx (LIR.Physical LIR.X19) (LIR.Physical LIR.X20)) @ ARM64Operands.loadImmediate S.X21 0L @ ARM64Operands.loadImmediate S.X22 Int64.max_int @ instructions (ARM64EmitMemory.emitRawWriteWord ctx (LIR.Physical LIR.X19) (LIR.Physical LIR.X21) (LIR.Physical LIR.X22)) @ instructions (ARM64EmitMemory.emitRawGet ctx (LIR.Physical LIR.X0) (LIR.Physical LIR.X19) (LIR.Physical LIR.X21)) @ print @ ARM64Operands.loadImmediate S.X21 7L @ ARM64Operands.loadImmediate S.X22 511L @ instructions (ARM64EmitMemory.emitRawWriteByte ctx (LIR.Physical LIR.X19) (LIR.Physical LIR.X21) (LIR.Physical LIR.X22)) @ instructions (ARM64EmitMemory.emitRawGetByte ctx (LIR.Physical LIR.X0) (LIR.Physical LIR.X19) (LIR.Physical LIR.X21)) @ print)) [false;true];
 check false "1" (fun ctx -> ARM64Operands.loadImmediate S.X20 8L @ instructions (ARM64EmitMemory.emitRawAlloc ctx (LIR.Physical LIR.X19) (LIR.Physical LIR.X20)) @ instructions (ARM64EmitMemory.emitRawFree ctx (LIR.Physical LIR.X19)) @ instructions (ARM64EmitMemory.emitRawAlloc ctx (LIR.Physical LIR.X21) (LIR.Physical LIR.X20)) @ [S.CMP_reg (S.X19,S.X21);S.CSET (S.X0,S.EQ)] @ print);
 List.iter (fun size -> check false "-42" (fun ctx -> ARM64Operands.loadImmediate S.X20 (Int64.of_int size) @ instructions (ARM64EmitMemory.emitMappedAlloc ctx (LIR.Physical LIR.X19) (LIR.Physical LIR.X20)) @ ARM64Operands.loadImmediate S.X21 0L @ ARM64Operands.loadImmediate S.X22 (-42L) @ instructions (ARM64EmitMemory.emitRawWriteWord ctx (LIR.Physical LIR.X19) (LIR.Physical LIR.X21) (LIR.Physical LIR.X22)) @ instructions (ARM64EmitMemory.emitRawGet ctx (LIR.Physical LIR.X0) (LIR.Physical LIR.X19) (LIR.Physical LIR.X21)) @ print @ instructions (ARM64EmitMemory.emitMappedFree ctx (LIR.Physical LIR.X19)))) [8;4096;65536];
 check false "2" (fun ctx -> instructions (ARM64EmitMemory.emitHeapAlloc ctx (LIR.Physical LIR.X19) 8) @ instructions (ARM64EmitBuffers.emitStringConcat ctx (LIR.Physical LIR.X21) (LIR.StringSymbol "a") (LIR.StringSymbol "b") []) @ ARM64Operands.loadImmediate S.X20 0L @ instructions (ARM64EmitMemory.emitRawSlotInit ctx (LIR.Physical LIR.X19) (LIR.Physical LIR.X20) (LIR.Physical LIR.X21) AST.TString) @ [S.LDR (S.X0,S.X21,0)] @ print);
 check false "2" (fun ctx -> instructions (ARM64EmitMemory.emitHeapAlloc ctx (LIR.Physical LIR.X19) 8) @ instructions (ARM64EmitMemory.emitHeapAlloc ctx (LIR.Physical LIR.X21) 8) @ ARM64Operands.loadImmediate S.X20 0L @ instructions (ARM64EmitMemory.emitRawSlotInit ctx (LIR.Physical LIR.X19) (LIR.Physical LIR.X20) (LIR.Physical LIR.X21) (AST.TTuple [AST.TInt64])) @ [S.LDR (S.X0,S.X21,8)] @ print);
 check false "9223372036854775807" (fun ctx -> instructions (ARM64EmitMemory.emitHeapAlloc ctx (LIR.Physical LIR.X19) 8) @ HeapAllocation.loadStringLiteralPointer S.X21 "static" @ ARM64Operands.loadImmediate S.X20 0L @ instructions (ARM64EmitMemory.emitRawSlotInit ctx (LIR.Physical LIR.X19) (LIR.Physical LIR.X20) (LIR.Physical LIR.X21) AST.TString) @ [S.LDR (S.X0,S.X21,0)] @ print);
 List.iter (fun typ -> check false "1" (fun ctx -> instructions (ARM64EmitMemory.emitHeapAlloc ctx (LIR.Physical LIR.X19) 8) @ ARM64Operands.loadImmediate S.X21 1L @ ARM64Operands.loadImmediate S.X20 0L @ instructions (ARM64EmitMemory.emitRawSlotInit ctx (LIR.Physical LIR.X19) (LIR.Physical LIR.X20) (LIR.Physical LIR.X21) typ) @ [S.LDR (S.X0,S.X19,0)] @ print)) [AST.TInt;AST.TUnit];
 check false "1" (fun ctx -> let ctx={ctx with ARM64CodeGenTypes.rawSlotInitRetainTargets=Some (LIR.SemanticTypeMap.singleton AST.TString None)} in instructions (ARM64EmitMemory.emitHeapAlloc ctx (LIR.Physical LIR.X19) 8) @ instructions (ARM64EmitBuffers.emitStringConcat ctx (LIR.Physical LIR.X21) (LIR.StringSymbol "a") (LIR.StringSymbol "b") []) @ ARM64Operands.loadImmediate S.X20 0L @ instructions (ARM64EmitMemory.emitRawSlotInit ctx (LIR.Physical LIR.X19) (LIR.Physical LIR.X20) (LIR.Physical LIR.X21) AST.TString) @ [S.LDR (S.X0,S.X21,0)] @ print);
 !count
(* The reference signed nonterminating printer overwrites its newline byte.
   Preserve that source behavior; unsigned and aggregate printers retain theirs. *)
let printingChecks ()=
 let target=ARM64.targetConfigFor Platform.LinuxARM64 in
 let ctx=Semantic_observation.ARMPrintingObservation.context "printing-check" target false in
 let total=ref 0 in
 let check expected body=
  let actual=runImage (image (ProcessLifecycle.generateHeapInit target @ body @ instructions (ARM64EmitInteger.emitExit ctx))) [] "" in
  if actual<>expected then failwith (Printf.sprintf "printing execution: expected %S, got %S" expected actual);
  incr total
 in
 List.iter (fun p -> List.iter (fun (value,expected) -> check expected (ARM64Operands.loadImmediate (ARM64Operands.lirPhysRegToARM64Reg p) value @ instructions (ARM64EmitPrinting.emitPrintInt64 ctx (LIR.Physical p)))) [Int64.min_int,"-9223372036854775808";0L,"0";42L,"42"]) [LIR.X0;LIR.X19];
 check "18446744073709551615\n" (ARM64Operands.loadImmediate S.X19 (-1L) @ instructions (ARM64EmitPrinting.emitPrintUInt64 ctx (LIR.Physical LIR.X19)));
 List.iter (fun (value,expected) -> check expected (ARM64Operands.loadImmediate S.X19 value @ instructions (ARM64EmitPrinting.emitPrintBool ctx (LIR.Physical LIR.X19)))) [0L,"false\n";1L,"true\n"];
 List.iter (fun p -> check "hé😀\n" (HeapAllocation.loadStringLiteralPointer (ARM64Operands.lirPhysRegToARM64Reg p) "hé😀" @ instructions (ARM64EmitPrinting.emitPrintHeapString ctx (LIR.Physical p)))) [LIR.X0;LIR.X1;LIR.X2;LIR.X8;LIR.X9;LIR.X19];
 check "hé😀\n" (instructions (ARM64EmitPrinting.emitPrintString ctx "hé😀"));
 check "a\000b" (instructions (ARM64EmitPrinting.emitPrintChars ctx [97;0;98]));
 check "1.5" (instructions (ARM64EmitFloatingPoint.emitFLoad ctx (LIR.FPhysical LIR.D1) 1.5) @ instructions (ARM64EmitPrinting.emitPrintFloatNoNewline ctx (LIR.FPhysical LIR.D1)));
 check "1.5\n" (instructions (ARM64EmitFloatingPoint.emitFLoad ctx (LIR.FPhysical LIR.D1) 1.5) @ instructions (ARM64EmitPrinting.emitPrintFloat ctx (LIR.FPhysical LIR.D1)));
 check "[]\n" (ARM64Operands.loadImmediate S.X19 0L @ instructions (ARM64EmitPrinting.emitPrintList ctx (LIR.Physical LIR.X19) AST.TInt64));
 let convert _ _=Error "Unexpected display release callback in scalar fixture" in
 check "None\n" (ARM64Operands.loadImmediate S.X19 0L @ instructions (ARM64EmitPrinting.emitPrintSum ctx convert (LIR.Physical LIR.X19) ["None",0,None;"Some",1,Some AST.TString] false));
 check "Some(hé😀)\n" (HeapAllocation.loadStringLiteralPointer S.X19 "hé😀" @ instructions (ARM64EmitPrinting.emitPrintSum ctx convert (LIR.Physical LIR.X19) ["None",0,None;"Some",1,Some AST.TString] false));
 check "Wrapped(42)\n" (ARM64Operands.loadImmediate S.X19 42L @ instructions (ARM64EmitPrinting.emitPrintSum ctx convert (LIR.Physical LIR.X19) ["Wrapped",0,Some AST.TInt64] true));
 check "Nullary\n" (ARM64Operands.loadImmediate S.X19 7L @ instructions (ARM64EmitPrinting.emitPrintSum ctx convert (LIR.Physical LIR.X19) ["Other",0,None;"Nullary",7,None] false));
 check "Point { x = 42, name = hé😀 }\n" (instructions (ARM64EmitMemory.emitHeapAlloc ctx (LIR.Physical LIR.X19) 16) @ instructions (ARM64EmitMemory.emitHeapStore ctx (LIR.Physical LIR.X19) 0 (LIR.Imm 42L) None) @ HeapAllocation.loadStringLiteralPointer S.X20 "hé😀" @ instructions (ARM64EmitMemory.emitHeapStore ctx (LIR.Physical LIR.X19) 8 (LIR.Reg (LIR.Physical LIR.X20)) (Some AST.TString)) @ instructions (ARM64EmitPrinting.emitPrintRecord ctx (LIR.Physical LIR.X19) "Point" ["x",AST.TInt64;"name",AST.TString]) @ instructions (ARM64EmitInteger.emitExit ctx) @ HeapAllocation.generateHeapOverflowTrapBlock (HeapAllocation.preparedHeapOverflowTrapBody target) ctx.ARM64CodeGenTypes.heapOverflowLabel @ HeapAllocation.generateRuntimeErrorHelper target);
 !total
let nativeEffectChecks ()=
 let target=ARM64.targetConfigFor Platform.LinuxARM64 in
 let ctx=Semantic_observation.ARMPrintingObservation.context "effect-check" target false in
 let number=ref 0 and total=ref 0 in
 let emit operation args=
  incr number;
  let ctx={ctx with ARM64CodeGenTypes.instructionSite=string_of_int !number} in
  instructions (ARM64EmitNativeEffects.emitCliNative ctx (LIR.Physical LIR.X19) operation args)
 in
 let print=[S.MOV_reg (S.X0,S.X19)] @ S.ofARM64List (PrintValues.generatePrintInt64NoNewline target) in
 let printPositive=[S.CMP_imm (S.X19,0);S.CSET (S.X0,S.GT)] @ S.ofARM64List (PrintValues.generatePrintInt64NoNewline target) in
 let text= instructions (ARM64EmitInteger.emitStdoutWrite ctx 0 (LIR.Reg (LIR.Physical LIR.X19)) false) in
 let check expected body=
  let root=[S.STP_pre (S.X29,S.X30,S.SP,-16);S.MOV_reg (S.X29,S.SP);S.MOVZ (S.X9,0,0);S.STR (S.X9,S.X29,0)] in
  let helperLabel="__dark_cli_argv_effect-check" in
  let program=root @ ProcessLifecycle.generateHeapInit target @ body @ instructions (ARM64EmitInteger.emitExit ctx) @ ProcessLifecycle.generateCliArgvHelper ctx helperLabel @ HeapAllocation.generateHeapOverflowTrapBlock (HeapAllocation.preparedHeapOverflowTrapBody target) ctx.ARM64CodeGenTypes.heapOverflowLabel @ HeapAllocation.generateRuntimeErrorHelper target in
  let actual=runImage (image program) ["argument"] "" in
  if actual<>expected then failwith (Printf.sprintf "native effect execution: expected %S, got %S" expected actual);
  incr total
 in
 check "1" (emit LIR.HostOS [] @ print);
 check "2" (emit LIR.HostArchitecture [] @ print);
 check "1" (emit LIR.GetPid [] @ printPositive);
 check (string_of_int (Unix.getuid ())) (emit LIR.GetUid [] @ print);
 check "1" (emit LIR.CpuCount [] @ printPositive);
 check (Sys.getcwd ()) (emit LIR.DirectoryCurrent [] @ text);
 check (Unix.gethostname ()) (emit LIR.Hostname [] @ [S.LDR (S.X19,S.X19,8)] @ text);
 check "1" (instructions (ARM64EmitNativeEffects.emitDateTimeNow ctx (LIR.Physical LIR.X19)) @ printPositive);
 List.iter (fun delay -> check "slept" (instructions (ARM64EmitFloatingPoint.emitFLoad ctx (LIR.FPhysical LIR.D0) delay) @ instructions (ARM64EmitNativeEffects.emitSleep ctx 0 (LIR.FPhysical LIR.D0)) @ instructions (ARM64EmitInteger.emitStdoutWrite ctx 0 (LIR.StringSymbol "slept") false))) [-1.;0.;0.25];
 check "argument" (emit LIR.GetArgv [LIR.Imm 0L] @ text);
 Unix.putenv "PORT_NATIVE_EFFECT_FIXTURE" "hé😀";
 check "hé😀" (emit LIR.GetEnv [LIR.StringSymbol "PORT_NATIVE_EFFECT_FIXTURE"] @ text);
 check "changed" (emit LIR.SetEnv [LIR.StringSymbol "PORT_NATIVE_EFFECT_FIXTURE";LIR.StringSymbol "changed"] @ emit LIR.GetEnv [LIR.StringSymbol "PORT_NATIVE_EFFECT_FIXTURE"] @ text);
 check "0" (emit LIR.UnsetEnv [LIR.StringSymbol "PORT_NATIVE_EFFECT_FIXTURE"] @ emit LIR.GetEnv [LIR.StringSymbol "PORT_NATIVE_EFFECT_FIXTURE"] @ print);
 check "1" (emit LIR.FileIsDirectory [LIR.StringSymbol (Sys.getcwd ())] @ print);
 let path=Filename.temp_file "port-exclusive-" ".file" in
 Sys.remove path;
 Fun.protect ~finally:(fun () -> if Sys.file_exists path then Sys.remove path) (fun () ->
  check "0" (emit LIR.FileCreateExclusive [LIR.StringSymbol path] @ print);
  if not (Sys.file_exists path) then failwith "FileCreateExclusive did not create file";
  check "17" (emit LIR.FileCreateExclusive [LIR.StringSymbol path] @ print));
 check "8" (ARM64Operands.loadImmediate S.X20 8L @ instructions (ARM64EmitMemory.emitRawAlloc ctx (LIR.Physical LIR.X21) (LIR.Physical LIR.X20)) @ emit LIR.SecureRandomFill [LIR.Reg (LIR.Physical LIR.X21);LIR.Imm 8L] @ print);
 List.iter (fun operation -> check "0" (emit operation [] @ emit LIR.SocketClose [LIR.Reg (LIR.Physical LIR.X19)] @ print)) [LIR.SocketTcp4;LIR.SocketUdp4];
 !total
let listReferenceChecks ()=
 let target=ARM64.targetConfigFor Platform.LinuxARM64 in
 let ctx=Semantic_observation.ARMPrintingObservation.context "list-rc-check" target false in
 let total=ref 0 in
 let allocate reg size=instructions (ARM64EmitMemory.emitHeapAlloc ctx (LIR.Physical reg) size) in
 let literal reg value=ARM64Operands.loadImmediate reg value in
 let print=S.ofARM64List (PrintValues.generatePrintInt64NoNewline target) in
 let check label expected body=
  let helpers=ARM64ListReferenceCounts.generateListRefCountIncHelper () @ ARM64ListReferenceCounts.generateNeededListRefCountDecHelpers ctx (StringOrder.Set.singleton label) StringOrder.Map.empty in
  let program=ProcessLifecycle.generateHeapInit target @ body @ print @ instructions (ARM64EmitInteger.emitExit ctx) @ helpers @ HeapAllocation.generateHeapOverflowTrapBlock (HeapAllocation.preparedHeapOverflowTrapBody target) ctx.ARM64CodeGenTypes.heapOverflowLabel @ HeapAllocation.generateRuntimeErrorHelper target in
  let actual=runImage (image program) [] "" in
  if actual<>expected then failwith (Printf.sprintf "list lifetime: expected %S, got %S" expected actual);
  incr total
 in
 let dec=ARM64CodeGenTypes.listRefCountDecHelperLabel in
 List.iter (fun (size,tag) ->
  let base=allocate LIR.X19 size @ literal S.X9 42L @ [S.STR (S.X9,S.X19,0)] in
  check dec "2" (base @ [S.ADD_imm (S.X0,S.X19,tag);S.BL ARM64CodeGenTypes.listRefCountIncHelperLabel;S.LDR (S.X0,S.X19,size)]);
  check dec "1" (base @ [S.ADD_imm (S.X0,S.X19,tag);S.BL ARM64CodeGenTypes.listRefCountIncHelperLabel;S.ADD_imm (S.X0,S.X19,tag);S.BL dec;S.LDR (S.X0,S.X19,size)]);
  check dec "1" (base @ [S.ADD_imm (S.X0,S.X19,tag);S.BL dec;S.LDR (S.X0,S.X27,size);S.CMP_reg (S.X0,S.X19);S.CSET (S.X0,S.EQ)]);
  check dec "0" (base @ [S.ADD_imm (S.X0,S.X19,tag);S.BL dec;S.LDR (S.X0,S.X19,size)])) [8,2;24,3;32,1];
 List.iter (fun tag -> check dec "1" (allocate LIR.X19 8 @ literal S.X9 42L @ [S.STR (S.X9,S.X19,0)] @ (if tag=0 then [S.MOV_reg (S.X0,S.X19)] else [S.ADD_imm (S.X0,S.X19,tag)]) @ [S.BL ARM64CodeGenTypes.listRefCountIncHelperLabel;S.LDR (S.X0,S.X19,8)])) [0;4;5;6;7];
 check dec "0" (literal S.X0 0L @ [S.BL dec;S.LDR (S.X0,S.X27,8)]);
 check dec "0" (literal S.X0 2L @ [S.BL dec;S.LDR (S.X0,S.X27,8)]);
 let children=allocate LIR.X20 8 @ allocate LIR.X21 8 @ literal S.X9 42L @ [S.STR (S.X9,S.X20,0);S.STR (S.X9,S.X21,0)] in
 List.iter (fun (size,tag,left,right) ->
  let body=allocate LIR.X19 size @ children @ [S.ADD_imm (S.X9,S.X20,2);S.STR (S.X9,S.X19,left);S.ADD_imm (S.X9,S.X21,2);S.STR (S.X9,S.X19,right);S.ADD_imm (S.X0,S.X19,tag);S.BL dec] in
  check dec "0" (body @ [S.LDR (S.X0,S.X19,size);S.LDR (S.X9,S.X20,8);S.ADD_reg (S.X0,S.X0,S.X9);S.LDR (S.X9,S.X21,8);S.ADD_reg (S.X0,S.X0,S.X9)]);
  check dec "1" (body @ [S.LDR (S.X0,S.X27,size);S.CMP_reg (S.X0,S.X19);S.CSET (S.X0,S.EQ)])) [24,3,8,16;32,1,16,24];
 List.iter (fun label ->
  let body=allocate LIR.X19 8 @ allocate LIR.X20 16 @ literal S.X9 1L @ [S.STR (S.X9,S.X20,0);S.STR (S.X20,S.X19,0);S.ADD_imm (S.X0,S.X19,2);S.BL label] in
  check label "0" (body @ [S.LDR (S.X0,S.X20,0)]);
  check label "1" (body @ [S.LDR (S.X0,S.X27,8);S.CMP_reg (S.X0,S.X19);S.CSET (S.X0,S.EQ)])) [ARM64CodeGenTypes.listRefCountDecStringHelperLabel;ARM64CodeGenTypes.listRefCountDecBlobHelperLabel];
 !total
let closureReferenceChecks ()=
 let target=ARM64.targetConfigFor Platform.LinuxARM64 in
 let baseCtx=Semantic_observation.ARMPrintingObservation.context "closure-rc-check" target false in
 let total=ref 0 in
 let allocate reg size=instructions (ARM64EmitMemory.emitHeapAlloc baseCtx (LIR.Physical reg) size) in
 let literal reg value=ARM64Operands.loadImmediate reg value in
 let fn reg name=[S.ADR (S.X9,HeapAllocation.codeLabel name);S.STR (S.X9,reg,0)] in
 let print=S.ofARM64List (PrintValues.generatePrintInt64NoNewline target) in
 let dec=ARM64CodeGenTypes.closureRefCountDecHelperLabel in
 let inc=ARM64CodeGenTypes.closureRefCountIncHelperLabel in
 let check ctx expected body extra=
  let dictHelper=ARM64CodeGenTypes.plannedDictDecHelperLabelForReleasePlan in
  let helpers=ARM64ClosureReferenceCounts.generateClosureRefCountIncHelper ctx @ ARM64ClosureReferenceCounts.generateClosureRefCountDecHelper dictHelper ctx @ ARM64ClosureReferenceCounts.generateStreamRefCountDecHelper ctx @ ARM64ListReferenceCounts.generateListRefCountIncHelper () @ ARM64ListReferenceCounts.generateNeededListRefCountDecHelpers ctx (StringOrder.Set.of_list [ARM64CodeGenTypes.listRefCountDecHelperLabel;ARM64CodeGenTypes.listRefCountDecStringHelperLabel;ARM64CodeGenTypes.listRefCountDecBlobHelperLabel]) StringOrder.Map.empty in
  let program=ProcessLifecycle.generateHeapInit target @ body @ print @ instructions (ARM64EmitInteger.emitExit ctx) @ helpers @ extra @ [S.Label "closure_fn";S.RET;S.Label "close_fn";S.ADD_imm (S.X22,S.X22,1);S.RET] @ HeapAllocation.generateHeapOverflowTrapBlock (HeapAllocation.preparedHeapOverflowTrapBody target) ctx.ARM64CodeGenTypes.heapOverflowLabel @ HeapAllocation.generateRuntimeErrorHelper target in
  let actual=runImage (image program) [] "" in
  if actual<>expected then failwith (Printf.sprintf "closure lifetime: expected %S, got %S" expected actual);
  incr total
 in
 List.iter (fun size ->
  let ctx={baseCtx with ARM64CodeGenTypes.closurePayloadSizes=StringOrder.Map.singleton "closure_fn" size} in
  let base=allocate LIR.X19 size @ fn S.X19 "closure_fn" in
  check ctx "2" (base @ [S.MOV_reg (S.X0,S.X19);S.BL inc;S.LDR (S.X0,S.X19,size)]) [];
  check ctx "1" (base @ [S.MOV_reg (S.X0,S.X19);S.BL inc;S.MOV_reg (S.X0,S.X19);S.BL dec;S.LDR (S.X0,S.X19,size)]) [];
  check ctx "0" (base @ [S.MOV_reg (S.X0,S.X19);S.BL dec;S.LDR (S.X0,S.X19,size)]) [];
  check ctx (if size<256 then "1" else "0") (base @ [S.MOV_reg (S.X0,S.X19);S.BL dec;S.LDR (S.X0,S.X27,if size<256 then size else 248)] @ (if size<256 then [S.CMP_reg (S.X0,S.X19);S.CSET (S.X0,S.EQ)] else [])) []) [8;16;24;248;256;264];
 check baseCtx "0" (literal S.X0 0L @ [S.BL inc;S.BL dec]) [];
 List.iter (fun typ ->
  let ctx={baseCtx with ARM64CodeGenTypes.closurePayloadSizes=StringOrder.Map.singleton "closure_fn" 16;closureCaptureTypes=StringOrder.Map.singleton "closure_fn" [typ]} in
  let dynamic=allocate LIR.X19 16 @ fn S.X19 "closure_fn" @ allocate LIR.X20 16 @ literal S.X9 1L @ [S.STR (S.X9,S.X20,0);S.STR (S.X20,S.X19,8);S.MOV_reg (S.X0,S.X19);S.BL dec;S.LDR (S.X0,S.X20,0)] in
  check ctx "0" dynamic []) [AST.TString;AST.TBlob;AST.TChar;AST.TInt];
 let taggedCtx={baseCtx with ARM64CodeGenTypes.closurePayloadSizes=StringOrder.Map.singleton "closure_fn" 16;closureCaptureTypes=StringOrder.Map.singleton "closure_fn" [AST.TInt]} in
 check taggedCtx "0" (allocate LIR.X19 16 @ fn S.X19 "closure_fn" @ literal S.X9 85L @ [S.STR (S.X9,S.X19,8);S.MOV_reg (S.X0,S.X19);S.BL dec;S.LDR (S.X0,S.X19,16)]) [];
 let listCtx={taggedCtx with ARM64CodeGenTypes.closureCaptureTypes=StringOrder.Map.singleton "closure_fn" [AST.TList AST.TString]} in
 check listCtx "0" (allocate LIR.X19 16 @ fn S.X19 "closure_fn" @ allocate LIR.X20 8 @ literal S.X9 42L @ [S.STR (S.X9,S.X20,0);S.ADD_imm (S.X9,S.X20,2);S.STR (S.X9,S.X19,8);S.MOV_reg (S.X0,S.X19);S.BL dec;S.LDR (S.X0,S.X20,8)]) [];
 let tupleCtx={taggedCtx with ARM64CodeGenTypes.closureCaptureTypes=StringOrder.Map.singleton "closure_fn" [AST.TTuple [AST.TString]]} in
 check tupleCtx "0" (allocate LIR.X19 16 @ fn S.X19 "closure_fn" @ allocate LIR.X20 8 @ allocate LIR.X21 16 @ literal S.X9 1L @ [S.STR (S.X9,S.X21,0);S.STR (S.X21,S.X20,0);S.STR (S.X20,S.X19,8);S.MOV_reg (S.X0,S.X19);S.BL dec;S.LDR (S.X0,S.X21,0)]) [];
 let recursiveType=AST.TRecord ("Node",[]) in
 let recursiveCtx={baseCtx with ARM64CodeGenTypes.recordRegistry=StringOrder.Map.singleton "Node" ["next",recursiveType]} in
 let helper=ARM64CodeGenTypes.recursiveNominalRefCountDecHelperLabel recursiveType in
 let recursiveHelper=ARM64ClosureReferenceCounts.generateRecursiveNominalRefCountDecHelper ARM64CodeGenTypes.plannedDictDecHelperLabelForReleasePlan recursiveCtx recursiveType in
 check recursiveCtx "0" (allocate LIR.X19 8 @ allocate LIR.X20 8 @ [S.STR (S.X20,S.X19,0);S.MOV_reg (S.X0,S.X19);S.BL helper;S.LDR (S.X0,S.X19,8);S.LDR (S.X9,S.X20,8);S.ADD_reg (S.X0,S.X0,S.X9)]) recursiveHelper;
 List.iter (fun (state,shared,expected) ->
  let body=allocate LIR.X19 24 @ allocate LIR.X20 8 @ fn S.X20 "closure_fn" @ allocate LIR.X21 8 @ fn S.X21 "close_fn" @ literal S.X9 (Int64.of_int state) @ [S.STR (S.X9,S.X19,0);S.STR (S.X20,S.X19,8);S.STR (S.X21,S.X19,16)] @ (if shared then literal S.X9 2L @ [S.STR (S.X9,S.X19,24)] else []) @ literal S.X22 0L @ [S.MOV_reg (S.X0,S.X19);S.BL ARM64CodeGenTypes.streamRefCountDecHelperLabel] in
  check baseCtx expected (body @ [S.MOV_reg (S.X0,S.X22)]) [];
  check baseCtx (if shared then "3" else "0") (body @ [S.LDR (S.X0,S.X19,24);S.LDR (S.X9,S.X20,8);S.ADD_reg (S.X0,S.X0,S.X9);S.LDR (S.X9,S.X21,8);S.ADD_reg (S.X0,S.X0,S.X9)]) []) [0,false,"1";5,false,"0";0,true,"0"];
 !total
[@@warning "-42"]
let dictReferenceChecks ()=
 let target=ARM64.targetConfigFor Platform.LinuxARM64 in
 let ctx=Semantic_observation.ARMPrintingObservation.context "dict-rc-check" target false in
 let total=ref 0 in
 let allocate reg size=instructions (ARM64EmitMemory.emitHeapAlloc ctx (LIR.Physical reg) size) in
 let literal=ARM64Operands.loadImmediate in
 let dec="dict-release-case" in
 let baseline label=ARM64DictReferenceCounts.generateDictRefCountDecHelper label MemoryModel.NoReleasePlan false false None false false None false false false ctx in
 let check helper expected body=
  let dictHelper=ARM64CodeGenTypes.plannedDictDecHelperLabelForReleasePlan in
  let helpers=ARM64DictReferenceCounts.generateDictRefCountIncHelper () @ helper @ baseline ARM64CodeGenTypes.dictRefCountDecHelperLabel @ ARM64ClosureReferenceCounts.generateClosureRefCountIncHelper ctx @ ARM64ClosureReferenceCounts.generateClosureRefCountDecHelper dictHelper ctx @ ARM64ClosureReferenceCounts.generateStreamRefCountDecHelper ctx @ ARM64ListReferenceCounts.generateNeededListRefCountDecHelpers ctx (StringOrder.Set.singleton ARM64CodeGenTypes.listRefCountDecHelperLabel) StringOrder.Map.empty in
  let program=ProcessLifecycle.generateHeapInit target @ body @ S.ofARM64List (PrintValues.generatePrintInt64NoNewline target) @ instructions (ARM64EmitInteger.emitExit ctx) @ helpers @ [S.Label "closure_fn";S.RET] @ HeapAllocation.generateHeapOverflowTrapBlock (HeapAllocation.preparedHeapOverflowTrapBody target) ctx.ARM64CodeGenTypes.heapOverflowLabel @ HeapAllocation.generateRuntimeErrorHelper target in
  let actual=runImage (image program) [] "" in
  if actual<>expected then failwith (Printf.sprintf "dict lifetime: expected %S, got %S (case %d)" expected actual !total);
  incr total
 in
 let helper=baseline dec in
 List.iter (fun (size,tag,header) ->
  let base=allocate LIR.X19 size @ literal S.X9 header @ [S.STR (S.X9,S.X19,0)] in
  check helper "2" (base @ [S.ADD_imm (S.X0,S.X19,tag);S.BL ARM64CodeGenTypes.dictRefCountIncHelperLabel;S.LDR (S.X0,S.X19,size)]);
  check helper "1" (base @ [S.ADD_imm (S.X0,S.X19,tag);S.BL ARM64CodeGenTypes.dictRefCountIncHelperLabel;S.ADD_imm (S.X0,S.X19,tag);S.BL dec;S.LDR (S.X0,S.X19,size)]);
  check helper "0" (base @ [S.ADD_imm (S.X0,S.X19,tag);S.BL dec;S.LDR (S.X0,S.X19,size)]);
  check helper (if size<256 then "1" else "0") (base @ [S.ADD_imm (S.X0,S.X19,tag);S.BL dec;S.LDR (S.X0,S.X27,if size<256 then size else 248)] @ (if size<256 then [S.CMP_reg (S.X0,S.X19);S.CSET (S.X0,S.EQ)] else []))) [16,2,42L;8,1,0L;8,3,0L;24,3,1L;40,3,2L;248,3,15L;264,3,16L];
 List.iter (fun tag -> check helper "1" (allocate LIR.X19 16 @ [S.ADD_imm (S.X0,S.X19,tag);S.BL ARM64CodeGenTypes.dictRefCountIncHelperLabel;S.ADD_imm (S.X0,S.X19,tag);S.BL dec;S.LDR (S.X0,S.X19,16)])) [0;4];
 List.iter (fun value -> check helper "0" (literal S.X0 value @ [S.BL ARM64CodeGenTypes.dictRefCountIncHelperLabel] @ literal S.X0 value @ [S.BL dec;S.LDR (S.X0,S.X27,16)])) [0L;2L];
 check helper "0" ([S.ADD_imm (S.X0,S.X28,2);S.BL ARM64CodeGenTypes.dictRefCountIncHelperLabel;S.ADD_imm (S.X0,S.X28,2);S.BL dec;S.LDR (S.X0,S.X27,16)]);
 let internal=allocate LIR.X19 32 @ allocate LIR.X20 16 @ allocate LIR.X21 16 @ allocate LIR.X22 16 @ literal S.X9 7L @ [S.STR (S.X9,S.X19,0);S.ADD_imm (S.X9,S.X20,2);S.STR (S.X9,S.X19,8);S.ADD_imm (S.X9,S.X21,2);S.STR (S.X9,S.X19,16);S.ADD_imm (S.X9,S.X22,2);S.STR (S.X9,S.X19,24);S.ADD_imm (S.X0,S.X19,1);S.BL dec] in
 check helper "0" (internal @ [S.LDR (S.X0,S.X19,32);S.LDR (S.X9,S.X20,16);S.ADD_reg (S.X0,S.X0,S.X9);S.LDR (S.X9,S.X21,16);S.ADD_reg (S.X0,S.X0,S.X9);S.LDR (S.X9,S.X22,16);S.ADD_reg (S.X0,S.X0,S.X9)]);
 check helper "1" (internal @ [S.LDR (S.X0,S.X27,32);S.CMP_reg (S.X0,S.X19);S.CSET (S.X0,S.EQ)]);
 let dynamic=MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer in
 let planned key value=ARM64DictReferenceCounts.generatePlannedDictRefCountDecHelper dec (MemoryModel.RootRelease (16,MemoryModel.DictHeap,MemoryModel.DictPayloadRelease (key,value))) ctx in
 List.iter (fun count ->
  let body=allocate LIR.X19 16 @ allocate LIR.X20 16 @ allocate LIR.X21 16 @ literal S.X9 count @ [S.STR (S.X9,S.X20,0);S.STR (S.X9,S.X21,0);S.STR (S.X20,S.X19,0);S.STR (S.X21,S.X19,8);S.ADD_imm (S.X0,S.X19,2);S.BL dec] in
  check (planned dynamic dynamic) (Int64.to_string (if count=Int64.max_int then count else Int64.pred count)) (body @ [S.LDR (S.X0,S.X20,0)]);
  check (planned dynamic dynamic) (Int64.to_string (if count=Int64.max_int then count else Int64.pred count)) (body @ [S.LDR (S.X0,S.X21,0)])) [1L;2L;Int64.max_int];
 let collision=allocate LIR.X19 40 @ allocate LIR.X20 16 @ allocate LIR.X21 16 @ literal S.X9 2L @ [S.STR (S.X9,S.X19,0);S.STR (S.X9,S.X20,0);S.STR (S.X9,S.X21,0);S.STR (S.X20,S.X19,8);S.STR (S.X21,S.X19,16);S.STR (S.X20,S.X19,24);S.STR (S.X21,S.X19,32);S.ADD_imm (S.X0,S.X19,3);S.BL dec] in
 check (planned dynamic dynamic) "0" (collision @ [S.LDR (S.X0,S.X20,0);S.LDR (S.X9,S.X21,0);S.ADD_reg (S.X0,S.X0,S.X9)]);
 let generic=MemoryModel.RootRelease (8,MemoryModel.GenericHeap,MemoryModel.FixedBlockPayloadRelease (8,[MemoryModel.FieldRelease (0,dynamic)])) in
 let genericBody=allocate LIR.X19 16 @ allocate LIR.X20 8 @ allocate LIR.X21 16 @ literal S.X9 1L @ [S.STR (S.X9,S.X21,0);S.STR (S.X21,S.X20,0);S.STR (S.X20,S.X19,8);S.ADD_imm (S.X0,S.X19,2);S.BL dec] in
 check (planned MemoryModel.NoReleasePlan generic) "0" (genericBody @ [S.LDR (S.X0,S.X21,0);S.LDR (S.X9,S.X20,8);S.ADD_reg (S.X0,S.X0,S.X9)]);
 List.iter (fun (kind,size,setup,plan) ->
  let body=allocate LIR.X19 16 @ allocate LIR.X20 size @ setup @ (if kind=MemoryModel.TaggedList || kind=MemoryModel.DictHeap then [S.ADD_imm (S.X9,S.X20,2)] else [S.MOV_reg (S.X9,S.X20)]) @ [S.STR (S.X9,S.X19,8);S.ADD_imm (S.X0,S.X19,2);S.BL dec;S.LDR (S.X0,S.X20,size)] in
  check (planned MemoryModel.NoReleasePlan plan) "0" body) [MemoryModel.TaggedList,8,literal S.X9 42L @ [S.STR (S.X9,S.X20,0)],MemoryModel.RootRelease (8,MemoryModel.TaggedList,MemoryModel.TaggedListPayloadRelease MemoryModel.NoReleasePlan);MemoryModel.DictHeap,16,[],MemoryModel.RootRelease (16,MemoryModel.DictHeap,MemoryModel.DictPayloadRelease (MemoryModel.NoReleasePlan,MemoryModel.NoReleasePlan));MemoryModel.ClosureHeap,8,[S.ADR (S.X9,HeapAllocation.codeLabel "closure_fn");S.STR (S.X9,S.X20,0)],MemoryModel.RootRelease (8,MemoryModel.ClosureHeap,MemoryModel.ClosurePayloadRelease []);MemoryModel.StreamHeap,24,literal S.X9 5L @ [S.STR (S.X9,S.X20,0)],MemoryModel.RootRelease (24,MemoryModel.StreamHeap,MemoryModel.NoPayloadRelease)];
 List.iter (fun tupleSize ->
  let helper=ARM64DictReferenceCounts.generateDictRefCountDecHelper dec MemoryModel.NoReleasePlan false false None false false None (tupleSize=16) (tupleSize=24) false ctx in
  let body=allocate LIR.X19 16 @ allocate LIR.X20 tupleSize @ allocate LIR.X21 16 @ allocate LIR.X22 8 @ literal S.X9 1L @ [S.STR (S.X9,S.X21,0);S.STR (S.X21,S.X20,0)] @ literal S.X9 42L @ [S.STR (S.X9,S.X22,0);S.ADD_imm (S.X9,S.X22,2);S.STR (S.X9,S.X20,8)] @ (if tupleSize=24 then allocate LIR.X23 16 @ [S.ADD_imm (S.X9,S.X23,2);S.STR (S.X9,S.X20,16)] else []) @ [S.STR (S.X20,S.X19,8);S.ADD_imm (S.X0,S.X19,2);S.BL dec] in
  check helper "0" (body @ [S.LDR (S.X0,S.X21,0);S.LDR (S.X9,S.X22,8);S.ADD_reg (S.X0,S.X0,S.X9);S.LDR (S.X9,S.X20,tupleSize);S.ADD_reg (S.X0,S.X0,S.X9)]);
  if tupleSize=24 then check helper "0" (body @ [S.LDR (S.X0,S.X23,16)])) [16;24];
 let sumHelper=ARM64DictReferenceCounts.generateDictRefCountDecHelper dec MemoryModel.NoReleasePlan false false None false false None false false true ctx in
 let sumBody=allocate LIR.X19 16 @ allocate LIR.X20 16 @ allocate LIR.X21 16 @ literal S.X9 1L @ [S.STR (S.X9,S.X21,0);S.STR (S.X21,S.X20,8);S.STR (S.X20,S.X19,8);S.ADD_imm (S.X0,S.X19,2);S.BL dec] in
 check sumHelper "0" (sumBody @ [S.LDR (S.X0,S.X21,0);S.LDR (S.X9,S.X20,16);S.ADD_reg (S.X0,S.X0,S.X9)]);
 !total
let rcEmissionChecks ()=
 let target=ARM64.targetConfigFor Platform.LinuxARM64 in
 let baseCtx=Semantic_observation.ARMPrintingObservation.context "rc-emission-check" target false in
 let total=ref 0 in
 let allocate reg size=instructions (ARM64EmitMemory.emitHeapAlloc baseCtx (LIR.Physical reg) size) in
 let literal=ARM64Operands.loadImmediate in
 let metadata plan=Some {MemoryModel.releasePlanCacheKey=None;releasePlan=Some plan;sourceType=None} in
 let check ?(extra=[]) ctx expected body=
  let dictHelper=ARM64CodeGenTypes.plannedDictDecHelperLabelForReleasePlan in
  let helpers=ARM64DictReferenceCounts.generateDictRefCountIncHelper () @ ARM64DictReferenceCounts.generateDictRefCountDecHelper ARM64CodeGenTypes.dictRefCountDecHelperLabel MemoryModel.NoReleasePlan false false None false false None false false false ctx @ ARM64ClosureReferenceCounts.generateClosureRefCountIncHelper ctx @ ARM64ClosureReferenceCounts.generateClosureRefCountDecHelper dictHelper ctx @ ARM64ClosureReferenceCounts.generateStreamRefCountDecHelper ctx @ ARM64ListReferenceCounts.generateListRefCountIncHelper () @ ARM64ListReferenceCounts.generateNeededListRefCountDecHelpers ctx (StringOrder.Set.singleton ARM64CodeGenTypes.listRefCountDecHelperLabel) StringOrder.Map.empty in
  let program=ProcessLifecycle.generateHeapInit target @ body @ S.ofARM64List (PrintValues.generatePrintInt64NoNewline target) @ instructions (ARM64EmitInteger.emitExit ctx) @ helpers @ extra @ [S.Label "closure_fn";S.RET] @ HeapAllocation.generateHeapOverflowTrapBlock (HeapAllocation.preparedHeapOverflowTrapBody target) ctx.ARM64CodeGenTypes.heapOverflowLabel @ HeapAllocation.generateRuntimeErrorHelper target in
  let actual=runImage (image program) [] "" in
  if actual<>expected then failwith (Printf.sprintf "RC emission: expected %S, got %S (case %d)" expected actual !total);
  incr total
 in
 let emitInc ctx addr size kind=instructions (ARM64Instructions.convertInstr ctx (LIR.RefCountInc (addr,size,kind,None))) in
 let emitDec ctx addr size kind metadata=instructions (ARM64Instructions.convertInstr ctx (LIR.RefCountDec (addr,size,kind,metadata))) in
 List.iter (fun size ->
  let base=allocate LIR.X19 size in
  check baseCtx "2" (base @ emitInc baseCtx (LIR.Physical LIR.X19) size LIR.GenericHeap @ [S.LDR (S.X0,S.X19,size)]);
  check baseCtx "1" (base @ emitInc baseCtx (LIR.Physical LIR.X19) size LIR.GenericHeap @ emitDec baseCtx (LIR.Physical LIR.X19) size LIR.GenericHeap None @ [S.LDR (S.X0,S.X19,size)]);
  check baseCtx "0" (base @ emitDec baseCtx (LIR.Physical LIR.X19) size LIR.GenericHeap None @ [S.LDR (S.X0,S.X19,size)]);
  check baseCtx (if size<256 then "1" else "0") (base @ emitDec baseCtx (LIR.Physical LIR.X19) size LIR.GenericHeap None @ [S.LDR (S.X0,S.X27,if size<256 then size else 248)] @ (if size<256 then [S.CMP_reg (S.X0,S.X19);S.CSET (S.X0,S.EQ)] else []))) [8;16;248;256;264];
 check baseCtx "0" (literal S.X19 0L @ emitInc baseCtx (LIR.Physical LIR.X19) 8 LIR.GenericHeap @ emitDec baseCtx (LIR.Physical LIR.X19) 8 LIR.GenericHeap None @ [S.LDR (S.X0,S.X27,8)]);
 let dynamic=MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer in
 let fixed=MemoryModel.RootRelease (8,MemoryModel.GenericHeap,MemoryModel.FixedBlockPayloadRelease (8,[MemoryModel.FieldRelease (0,dynamic)])) in
 List.iter (fun (physical,symbolic) ->
  let body=allocate LIR.X19 8 @ allocate LIR.X20 16 @ literal S.X9 1L @ [S.STR (S.X9,S.X20,0);S.STR (S.X20,S.X19,0);S.MOV_reg (symbolic,S.X19)] @ emitDec baseCtx (LIR.Physical physical) 8 LIR.GenericHeap (metadata fixed) in
  check baseCtx "0" (body @ [S.LDR (S.X0,S.X20,0)]);
  check baseCtx "1" (body @ [S.LDR (S.X0,S.X27,8);S.CMP_reg (S.X0,S.X19);S.CSET (S.X0,S.EQ)])) [LIR.X0,S.X0;LIR.X10,S.X10;LIR.X11,S.X11;LIR.X12,S.X12;LIR.X13,S.X13;LIR.X19,S.X19];
 let nested=MemoryModel.RootRelease (8,MemoryModel.GenericHeap,MemoryModel.FixedBlockPayloadRelease (8,[MemoryModel.FieldRelease (0,fixed)])) in
 check baseCtx "0" (allocate LIR.X19 8 @ allocate LIR.X20 8 @ allocate LIR.X21 16 @ literal S.X9 1L @ [S.STR (S.X9,S.X21,0);S.STR (S.X21,S.X20,0);S.STR (S.X20,S.X19,0)] @ emitDec baseCtx (LIR.Physical LIR.X19) 8 LIR.GenericHeap (metadata nested) @ [S.LDR (S.X0,S.X21,0);S.LDR (S.X9,S.X20,8);S.ADD_reg (S.X0,S.X0,S.X9)]);
 let boxed=MemoryModel.RootRelease (16,MemoryModel.GenericHeap,MemoryModel.BoxedSumPayloadRelease (16,[MemoryModel.FieldRelease (8,dynamic)],[{MemoryModel.tag=1;fieldReleases=[MemoryModel.FieldRelease (8,dynamic)]}])) in
 List.iter (fun (name,expected) ->
  let ctx={baseCtx with ARM64CodeGenTypes.functionName=name} in
  check ctx expected (allocate LIR.X19 16 @ allocate LIR.X20 16 @ literal S.X9 1L @ [S.STR (S.X9,S.X19,0);S.STR (S.X9,S.X20,0);S.STR (S.X20,S.X19,8)] @ emitDec ctx (LIR.Physical LIR.X19) 16 LIR.GenericHeap (metadata boxed) @ [S.LDR (S.X0,S.X20,0)])) ["caller","0";"Darklang.Stdlib.List.foo","1";"Darklang.Stdlib.Dict.foo","0"];
 List.iter (fun (physical,symbolic) -> List.iter (fun count ->
  let base=allocate LIR.X20 16 @ literal S.X9 count @ [S.STR (S.X9,S.X20,0);S.MOV_reg (symbolic,S.X20)] in
  check baseCtx (Int64.to_string (if count=Int64.max_int then count else Int64.succ count)) (base @ instructions (ARM64Instructions.convertInstr baseCtx (LIR.RefCountIncString (LIR.Reg (LIR.Physical physical)))) @ [S.LDR (S.X0,S.X20,0)]);
  check baseCtx (Int64.to_string (if count=Int64.max_int then count else Int64.pred count)) (base @ instructions (ARM64Instructions.convertInstr baseCtx (LIR.RefCountDecString (LIR.Reg (LIR.Physical physical)))) @ [S.LDR (S.X0,S.X20,0)])) [1L;2L;Int64.max_int]) [LIR.X0,S.X0;LIR.X12,S.X12;LIR.X13,S.X13;LIR.X14,S.X14;LIR.X15,S.X15;LIR.X19,S.X19];
 List.iter (fun value ->
  let body=literal S.X13 value @ instructions (ARM64Instructions.convertInstr baseCtx (LIR.RefCountIncInt (LIR.Reg (LIR.Physical LIR.X13)))) @ literal S.X15 value @ instructions (ARM64Instructions.convertInstr baseCtx (LIR.RefCountDecInt (LIR.Reg (LIR.Physical LIR.X15)))) @ literal S.X0 42L in
  check baseCtx "42" body) [0L;1L;85L];
 List.iter (fun (kind,size,tag,setup,plan) ->
  let body=allocate LIR.X19 size @ setup @ [S.ADD_imm (S.X19,S.X19,tag)] in
  check baseCtx "1" (body @ emitInc baseCtx (LIR.Physical LIR.X19) size kind @ emitDec baseCtx (LIR.Physical LIR.X19) size kind (metadata plan) @ [S.SUB_imm (S.X19,S.X19,tag);S.LDR (S.X0,S.X19,size)]);
  check baseCtx "0" (body @ emitDec baseCtx (LIR.Physical LIR.X19) size kind (metadata plan) @ [S.SUB_imm (S.X19,S.X19,tag);S.LDR (S.X0,S.X19,size)])) [LIR.TaggedList,8,2,literal S.X9 42L @ [S.STR (S.X9,S.X19,0)],MemoryModel.RootRelease (8,MemoryModel.TaggedList,MemoryModel.TaggedListPayloadRelease MemoryModel.NoReleasePlan);LIR.DictHeap,16,2,[],MemoryModel.RootRelease (16,MemoryModel.DictHeap,MemoryModel.DictPayloadRelease (MemoryModel.NoReleasePlan,MemoryModel.NoReleasePlan));LIR.ClosureHeap,8,0,[S.ADR (S.X9,HeapAllocation.codeLabel "closure_fn");S.STR (S.X9,S.X19,0)],MemoryModel.RootRelease (8,MemoryModel.ClosureHeap,MemoryModel.ClosurePayloadRelease []);LIR.StreamHeap,24,0,literal S.X9 5L @ [S.STR (S.X9,S.X19,0)],MemoryModel.RootRelease (24,MemoryModel.StreamHeap,MemoryModel.NoPayloadRelease)];
 let nestedPlan=MemoryModel.RootRelease (8,MemoryModel.GenericHeap,MemoryModel.FixedBlockPayloadRelease (8,[MemoryModel.FieldRelease (0,fixed)])) in
 List.iter (fun (size,plan,owns,isNested,isBoxed) ->
  let label="outlined-release" in
  let spec={LIR.releasePlanMemoKeys=LIR.RcReleasePlanMemoKeySet.empty;payloadSize=size;releasePlan=plan;ownsSinglePayloadSum=owns} in
  let extra=GenericReferenceCounts.generatePlannedGenericRefCountDecHelper label spec baseCtx in
  let body=allocate LIR.X19 size @ allocate LIR.X20 16 @ literal S.X9 1L @ [S.STR (S.X9,S.X20,0)] @ (if isNested then allocate LIR.X21 8 @ [S.STR (S.X20,S.X21,0);S.STR (S.X21,S.X19,0)] else if isBoxed then [S.STR (S.X9,S.X19,0);S.STR (S.X20,S.X19,8)] else [S.STR (S.X20,S.X19,0)]) @ [S.MOV_reg (S.X0,S.X19);S.BL label] in
  check ~extra baseCtx "1" (body @ [S.CMP_reg (S.X0,S.X19);S.CSET (S.X0,S.EQ)]);
  check ~extra baseCtx (if isBoxed && not owns then "1" else "0") (body @ [S.LDR (S.X0,S.X20,0)])) [8,fixed,true,false,false;8,nestedPlan,true,true,false;16,boxed,true,false,true;16,boxed,false,false,true];
 !total
let functionLoweringChecks ()=
 let target=ARM64.targetConfigFor Platform.LinuxARM64 in
 let ctx={ (Semantic_observation.ARMPrintingObservation.context "_start" target false) with ARM64CodeGenTypes.functionNames=FunctionIdMap.ofList [AST.functionId 0L,"_start";AST.functionId 1L,"fn";AST.functionId 2L,"tail_target"]} in
 let trap=HeapAllocation.preparedHeapOverflowTrapBody target in
 let total=ref 0 in
 let label name=LIR.Label name in
 let block name instrs terminator={LIR.label=label name;instrs;terminator} in
 let func id name stack saved blocks=
  let scoped (LIR.Label value)=label (name^"_"^value) in
  let terminator = function LIR.Ret->LIR.Ret|LIR.Jump target->LIR.Jump (scoped target)|LIR.Branch (reg,a,b)->LIR.Branch (reg,scoped a,scoped b)|LIR.BranchZero (reg,a,b)->LIR.BranchZero (reg,scoped a,scoped b)|LIR.BranchBitZero (reg,bit,a,b)->LIR.BranchBitZero (reg,bit,scoped a,scoped b)|LIR.BranchBitNonZero (reg,bit,a,b)->LIR.BranchBitNonZero (reg,bit,scoped a,scoped b)|LIR.CondBranch (condition,a,b)->LIR.CondBranch (condition,scoped a,scoped b) in
  let blocks=List.map (fun (b:LIR.basicBlock) -> {b with LIR.label=scoped b.LIR.label;terminator=terminator b.LIR.terminator}) blocks in
  {LIR.id=AST.functionId id;LIR.name=name;typedParams=[];cfg={LIR.entry=scoped (label "entry");blocks=LIR.LabelMap.of_seq (List.to_seq (List.map (fun (b:LIR.basicBlock) -> b.LIR.label,b) blocks))};stackSize=stack;usedCalleeSaved=saved;codegenFacts=None} in
 let check expected functions=
  let program=List.concat_map (fun f -> instructions (ARM64Functions.convertFunction trap ctx f)) functions @ HeapAllocation.generateRuntimeErrorHelper target in
  let actual=runImage (image program) [] "" in
  if actual<>expected then failwith (Printf.sprintf "function lowering: expected %S, got %S (case %d)" expected actual !total);
  incr total
 in
 let reg=LIR.Physical LIR.X19 in
 let print value=block "value" [LIR.Mov (reg,LIR.Imm value);LIR.PrintInt64NoNewline reg] LIR.Ret in
 List.iter (fun stack -> List.iter (fun saved -> check "42" [func 0L "_start" stack saved [block "entry" [LIR.Mov (reg,LIR.Imm 42L);LIR.PrintInt64NoNewline reg] LIR.Ret]]) [[];[LIR.X19;LIR.X20]]) [0;16;32;128];
 let branch initial terminator expected=
  check expected [func 0L "_start" 16 [LIR.X19] [block "entry" [LIR.Mov (reg,LIR.Imm initial);LIR.Cmp (reg,LIR.Imm 0L)] terminator;{(print 42L) with LIR.label=label "yes"};{(print 7L) with LIR.label=label "no"}]]
 in
 List.iter (fun initial ->
  branch initial (LIR.Branch (reg,label "yes",label "no")) (if initial=0L then "7" else "42");
  branch initial (LIR.BranchZero (reg,label "yes",label "no")) (if initial=0L then "42" else "7")) [0L;1L;-1L];
 List.iter (fun bit -> List.iter (fun initial ->
  let isSet=Int64.logand initial (Int64.shift_left 1L bit)<>0L in
  branch initial (LIR.BranchBitZero (reg,bit,label "yes",label "no")) (if isSet then "7" else "42");
  branch initial (LIR.BranchBitNonZero (reg,bit,label "yes",label "no")) (if isSet then "42" else "7")) [0L;1L;Int64.min_int;-1L]) [0;31;32;63];
 List.iter (fun initial -> List.iter (fun (condition,taken) -> branch initial (LIR.CondBranch (condition,label "yes",label "no")) (if taken then "42" else "7")) [LIR.EQ,initial=0L;LIR.NE,initial<>0L;LIR.LT,initial<0L;LIR.GT,initial>0L;LIR.LE,initial<=0L;LIR.GE,initial>=0L;LIR.ULT,false;LIR.UGT,initial<>0L;LIR.ULE,initial=0L;LIR.UGE,true]) [-1L;0L;1L];
 let root instrs=func 0L "_start" 32 [LIR.X19] [block "entry" instrs LIR.Ret] in
 check "42" [root [LIR.HeapAlloc (reg,8);LIR.HeapStore (reg,0,LIR.Imm 42L,Some AST.TInt64);LIR.HeapLoad (LIR.Physical LIR.X20,reg,0);LIR.PrintInt64NoNewline (LIR.Physical LIR.X20)]];
 let returnBlock=block "entry" [LIR.Mov (LIR.Physical LIR.X0,LIR.Imm 42L)] LIR.Ret in
 check "42" [root [LIR.Call (reg,AST.functionId 1L,[]);LIR.Mov (reg,LIR.Reg (LIR.Physical LIR.X0));LIR.PrintInt64NoNewline reg];func 1L "fn" 16 [LIR.X19] [returnBlock]];
 check "42" [root [LIR.Call (reg,AST.functionId 1L,[]);LIR.Mov (reg,LIR.Reg (LIR.Physical LIR.X0));LIR.PrintInt64NoNewline reg];func 1L "fn" 16 [LIR.X19] [block "entry" [LIR.TailCall (AST.functionId 2L,[])] LIR.Ret];func 2L "tail_target" 0 [] [returnBlock]];
 !total
[@@warning "-42"]
let programPipelineChecks ()=
 let target=ARM64.targetConfigFor Platform.LinuxARM64 in
 let reg n=LIR.Virtual n in
 let make instrs=
  let label=LIR.Label "pipeline-entry" in
  let block={LIR.label;instrs;terminator=LIR.Ret} in
  {LIR.id=AST.functionId 0L;name="_start";typedParams=[];cfg={LIR.entry=label;blocks=LIR.LabelMap.singleton label block};stackSize=0;usedCalleeSaved=[];codegenFacts=None} in
 let dynamic=MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer in
 let fields=List.init 25 (fun index -> MemoryModel.FieldRelease (index*8,dynamic)) in
 let plan=MemoryModel.RootRelease (200,MemoryModel.GenericHeap,MemoryModel.FixedBlockPayloadRelease (200,fields)) in
 let metadata plan=Some {MemoryModel.releasePlanCacheKey=None;releasePlan=Some plan;sourceType=Some AST.TString} in
 let cases=[
  "42",[LIR.Mov (reg 0,LIR.Imm 42L);LIR.PrintInt64NoNewline (reg 0)];
  "42",[LIR.Mov (reg 0,LIR.Imm 20L);LIR.Mov (reg 1,LIR.Imm 22L);LIR.Add (reg 2,reg 0,LIR.Reg (reg 1));LIR.PrintInt64NoNewline (reg 2)];
  "hé😀1.5",[LIR.StdoutWrite (0,LIR.StringSymbol "hé😀",false);LIR.FLoad (LIR.FVirtual 0,1.5);LIR.PrintFloatNoNewline (LIR.FVirtual 0)];
  "42",[LIR.HeapAlloc (reg 0,8);LIR.HeapStore (reg 0,0,LIR.Imm 42L,Some AST.TInt64);LIR.HeapLoad (reg 1,reg 0,0);LIR.PrintInt64NoNewline (reg 1)];
  "hé😀tail",[LIR.StringConcat (reg 0,LIR.StringSymbol "hé😀",LIR.StringSymbol "tail",[]);LIR.PrintHeapStringNoNewline (reg 0)];
  "0",[LIR.HeapAlloc (reg 0,200)]@List.init 25 (fun index -> LIR.HeapStore (reg 0,index*8,LIR.Imm 0L,Some AST.TString))@[LIR.RefCountDec (reg 0,200,LIR.GenericHeap,metadata plan);LIR.HeapLoad (reg 1,reg 0,200);LIR.PrintInt64NoNewline (reg 1)];
  "42",[LIR.Mov (reg 0,LIR.Imm 0L);LIR.RefCountInc (reg 0,8,LIR.TaggedList,None);LIR.RefCountDec (reg 0,8,LIR.TaggedList,metadata (MemoryModel.RootRelease (8,MemoryModel.TaggedList,MemoryModel.TaggedListPayloadRelease MemoryModel.NoReleasePlan)));LIR.Mov (reg 1,LIR.Imm 42L);LIR.PrintInt64NoNewline (reg 1)];
  "42",[LIR.Mov (reg 0,LIR.Imm 0L);LIR.RefCountInc (reg 0,16,LIR.DictHeap,None);LIR.RefCountDec (reg 0,16,LIR.DictHeap,metadata (MemoryModel.RootRelease (16,MemoryModel.DictHeap,MemoryModel.DictPayloadRelease (MemoryModel.NoReleasePlan,MemoryModel.NoReleasePlan))));LIR.Mov (reg 1,LIR.Imm 42L);LIR.PrintInt64NoNewline (reg 1)]
 ] in
 let total=ref 0 in
 List.iter (fun mode -> List.iter (fun (expected,body) ->
  let LIR.Program (functions,variants,records)=ARM64PrepareFunctions.prepareARM64Program (LIR.Program ([make body],StringOrder.Map.empty,StringOrder.Map.empty)) in
  let functions=List.map (RegisterAllocation.allocateRegisters Platform.ARM64) functions in
  let options={ARM64CodeGenTypes.defaultOptions with ARM64CodeGenTypes.disableFreeList=mode=2} in
  let functionCache _ generate=generate () and helperCache _ generate=generate () and groupCache _ _ generate=generate () in
  let groups=if mode=2 then [{Backend_Arm64_CodeGen.contextIdentity=Obj.repr (ref ());reusableAcrossCompilations=true;functions}] else [] in
  let generated=match Backend_Arm64_CodeGen.generateARM64WithOptionsAndCaches target options None None (if mode=0 then None else Some functionCache) None (if mode=2 then Some groupCache else None) groups None (if mode=0 then None else Some helperCache) [] None None (LIR.Program (functions,variants,records)) with Ok generated->generated|Error error->failwith error in
  let preparePart _ generate=generate () and prepareGroup _ generate=generate () in
  let emitted=Emit.emitBinary generated Platform.Linux false (Some preparePart) (Some prepareGroup) None in
  let actual=runImage emitted.Emit.binary [] "" in
  if actual<>expected then failwith (Printf.sprintf "program pipeline: expected %S, got %S (case %d)" expected actual !total);
  incr total) cases) [0;1;2];
 !total
[@@warning "-42"]
let x64CallFloatChecks ()=
 let module X=X86_64 in
 let module F=X64EmitFloatingPoint in
 let ctx={X64CodeGenTypes.functionName="x64-execution";stackSize=32;usedCalleeSaved=[LIR.X19];enableLeakCheck=false;recordRegistry=StringOrder.Map.empty;sumShapeRegistry=StringOrder.Map.empty;functionNames=FunctionIdMap.ofList [AST.functionId 1L,"callee"]} in
 let checked=function Ok value->value|Error error->failwith error in
 let total=ref 0 in
 let check body resultReg expected extra=
  let comparison=X64Operands.loadImm64 X.RCX expected@[X.CMP_reg (resultReg,X.RCX);X.Jcc (X.NE,"failed")] in
  let code=[X.Label "_start"]@X64Frames.genPrologue 32 [LIR.X19]@body@comparison@X64Operands.genPrintChars ['P']@[X.MOV_imm32 (X.RDI,0l)]@X64Operands.genExitSyscall@[X.Label "failed"]@X64Operands.genPrintChars ['F']@[X.MOV_imm32 (X.RDI,0l)]@X64Operands.genExitSyscall@extra in
  let resolved=checked (X86_64_Resolve.resolveAndEncode code) in
  let binary=Binary_Generation_ELF_X86_64.createExecutableWithPools resolved.X86_64_Resolve.machineCode (LiteralPool.createStringPool Seq.empty) (LiteralPool.createFloatPool Seq.empty) false 0 in
  let actual=runImageOn "/opt/dcb/qemu/qemu-x86_64" binary [] "" in
  if actual<>"P" then failwith (Printf.sprintf "x64 call/float execution failed at case %d" !total);
  incr total in
 let fps=[LIR.D0;LIR.D1;LIR.D2;LIR.D3;LIR.D4;LIR.D5;LIR.D6;LIR.D7;LIR.D8;LIR.D9;LIR.D10;LIR.D11;LIR.D12;LIR.D13;LIR.D14;LIR.D15] in
 let load reg value=checked (F.emitFLoad ctx (LIR.FPhysical reg) value) in
 List.iter (fun dest ->
  List.iter (fun (emit,expected) ->
   let body=load LIR.D0 8. @ load LIR.D15 2. @ checked (emit ctx (LIR.FPhysical dest) (LIR.FPhysical LIR.D0) (LIR.FPhysical LIR.D15)) @ [X.MOVQ_to_gp (X.RAX,X64Operands.lirFRegToX86 dest)] in
   check body X.RAX (Int64.bits_of_float expected) []) [F.emitFAdd,10.;F.emitFSub,6.;F.emitFMul,16.;F.emitFDiv,4.];
  List.iter (fun src -> List.iter (fun (emit,input,expected) ->
   let body=load src input @ checked (emit ctx (LIR.FPhysical dest) (LIR.FPhysical src)) @ [X.MOVQ_to_gp (X.RAX,X64Operands.lirFRegToX86 dest)] in
   check body X.RAX (Int64.bits_of_float expected) []) [F.emitFNeg,-3.,3.;F.emitFAbs,-3.,3.;F.emitFSqrt,9.,3.]) [dest;LIR.D0;LIR.D15]) fps;
 List.iter (fun dest ->
  let physical=LIR.Physical dest in let result=X64Operands.lirRegToX86 dest in
  let leaf=[X.Label "callee"]@X64Operands.loadImm64 X.RAX 42L@[X.RET] in
  check (checked (X64EmitCalls.emitCall ctx physical (AST.functionId 1L) [])) result 42L leaf;
  List.iter (fun pointer -> let source=LIR.Physical pointer in
   let setup=checked (X64EmitCalls.emitLoadFuncAddr ctx source (AST.functionId 1L)) in
   check (setup@checked (X64EmitCalls.emitIndirectCall ctx physical source [])) result 42L leaf;
   check (setup@checked (X64EmitCalls.emitClosureCall ctx physical source [])) result 42L leaf) [LIR.X6;LIR.X7;LIR.X19]) [LIR.X0;LIR.X6;LIR.X7;LIR.X19];
 List.iter (fun moves ->
  let body=load LIR.D0 1. @ load LIR.D1 2. @ load LIR.D2 3. @ checked (F.emitFArgMoves ctx moves) @ [X.MOVQ_to_gp (X.RAX,X.XMM0)] in
  check body X.RAX (Int64.bits_of_float 2.) []) [[LIR.D0,LIR.FPhysical LIR.D1;LIR.D1,LIR.FPhysical LIR.D0];[LIR.D0,LIR.FPhysical LIR.D1;LIR.D1,LIR.FPhysical LIR.D2;LIR.D2,LIR.FPhysical LIR.D0]];
 !total
[@@warning "-42"]
let x64PrintingChecks ()=
 let module X=X86_64 in
 let ctx={X64CodeGenTypes.functionName="x64-printing-execution";stackSize=0;usedCalleeSaved=[];enableLeakCheck=false;recordRegistry=StringOrder.Map.empty;sumShapeRegistry=StringOrder.Map.empty;functionNames=FunctionIdMap.empty} in
 let checked=function Ok value->value|Error error->failwith error in
 let total=ref 0 in
 let check expected body=
  let code=[X.Label "_start"]@body@[X.MOV_imm32 (X.RDI,0l)]@X64Operands.genExitSyscall in
  let resolved=checked (X86_64_Resolve.resolveAndEncode code) in
  let binary=Binary_Generation_ELF_X86_64.createExecutableWithPools resolved.X86_64_Resolve.machineCode LiteralPool.emptyStringPool LiteralPool.emptyFloatPool false 0 in
  let actual=runImageOn "/opt/dcb/qemu/qemu-x86_64" binary [] "" in
  if actual<>expected then failwith (Printf.sprintf "x64 printing: expected %S, got %S (case %d)" expected actual !total);
  incr total in
 List.iter (fun reg -> List.iter (fun value -> List.iter (fun newline ->
  let setup=X64Operands.loadImm64 (X64Operands.lirRegToX86 reg) value in
  let signed=if newline then X64EmitPrinting.emitPrintInt64 else X64EmitPrinting.emitPrintInt64NoNewline in
  let unsigned=if newline then X64EmitPrinting.emitPrintUInt64 else X64EmitPrinting.emitPrintUInt64NoNewline in
  check (Int64.to_string value^(if newline then "\n" else "")) (setup@checked (signed ctx (LIR.Physical reg)));
  check (Printf.sprintf "%Lu" value^(if newline then "\n" else "")) (setup@checked (unsigned ctx (LIR.Physical reg)))) [false;true]) [0L;1L;-1L;10L;Int64.min_int;Int64.max_int]) [LIR.X0;LIR.X2;LIR.X1;LIR.X4;LIR.X6;LIR.X19];
 List.iter (fun value ->
  let setup=X64Operands.loadImm64 X.RAX value in
  check (if value=0L then "false\n" else "true\n") (setup@checked (X64EmitPrinting.emitPrintBool ctx (LIR.Physical LIR.X0)));
  check "" (setup@checked (X64EmitPrinting.emitPrintBoolNoNewline ctx (LIR.Physical LIR.X0)))) [0L;1L;-1L];
 List.iter (fun text -> check (text^"\n") (checked (X64EmitPrinting.emitPrintString ctx text))) ["";"hé😀";String.make 7 'a';String.make 8 'a';String.make 9 'a'];
 List.iter (fun length -> let text=String.init length (fun index -> Char.chr ((index*73+255) land 255)) in check text (checked (X64EmitPrinting.emitPrintChars ctx (List.of_seq (String.to_seq text))))) [0;1;7;8;9;31;32;33;65];
 List.iter (fun reg -> List.iter (fun newline ->
  let text="hé😀" in let data=Bytes.make 8 '\000' in Bytes.blit_string text 0 data 0 (String.length text);
  let actualReg=X64Operands.lirRegToX86 reg in
  let setup=[X.SUB_imm (X.RSP,32l)]@X64Operands.loadImm64 X.RAX 1L@[X.MOV_store (X.RSP,0l,X.RAX)]@X64Operands.loadImm64 X.RAX (Int64.of_int (String.length text))@[X.MOV_store (X.RSP,8l,X.RAX)]@X64Operands.loadImm64 X.RAX (Bytes.get_int64_le data 0)@[X.MOV_store (X.RSP,16l,X.RAX);X.MOV_reg (actualReg,X.RSP)] in
  let emit=if newline then X64EmitPrinting.emitPrintHeapString else X64EmitPrinting.emitPrintHeapStringNoNewline in
  check (text^(if newline then "\n" else "")) (setup@checked (emit ctx (LIR.Physical reg))@[X.ADD_imm (X.RSP,32l)])) [false;true]) [LIR.X0;LIR.X2;LIR.X6;LIR.X19;LIR.X7];
 check "heap" (X64Printing.genHeapInit ()@X64Operands.genPrintChars ['h';'e';'a';'p']);
 !total
[@@warning "-42"]
let ()=
 let argvCases=[0,[],"N";(-1),["first"],"N";0,["first";"second"],"first";1,["first";"second"],"second";2,["first";"second"],"N";0,[""],"";0,["hé😀"],"hé😀";2147483647,["first"],"N"] in
 List.iter (fun (index,args,expected) -> let actual=runImage (binary index) args "" in if actual<>expected then failwith (Printf.sprintf "argv[%d]: expected %S, got %S" index expected actual)) argvCases;
 let effects=[Some "hé😀",false,0,"","hé😀";Some "",true,0,"","\n";None,false,1,"hello\nignored","hello";None,true,1,"hé😀\r\n","hé😀\n";None,false,1,"tail","tail";None,true,1,"","\n";None,true,2,"first\nsecond\n","first\nsecond\n";None,false,1,"a\000b\n","a\000b";None,false,1,"\n","";None,false,1,"a\rb\r\n","a\rb"] in
 List.iter (fun (literal,newline,reads,input,expected) -> let actual=runImage (presentationBinary literal newline reads) [] input in if actual<>expected then failwith (Printf.sprintf "presentation input %S: expected %S, got %S" input expected actual)) effects;
 let count=List.length argvCases+List.length effects+filesystemChecks ()+bufferChecks ()+memoryChecks ()+printingChecks ()+nativeEffectChecks ()+listReferenceChecks ()+closureReferenceChecks ()+dictReferenceChecks ()+rcEmissionChecks ()+functionLoweringChecks ()+programPipelineChecks () in
 Printf.printf "%d/%d native ARM64 process executions passed\n" count count;
 let x64Count=x64CallFloatChecks ()+x64PrintingChecks () in Printf.printf "%d/%d native x64 process executions passed\n" x64Count x64Count
