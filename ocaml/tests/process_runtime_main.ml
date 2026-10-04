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
let ()=
 let argvCases=[0,[],"N";(-1),["first"],"N";0,["first";"second"],"first";1,["first";"second"],"second";2,["first";"second"],"N";0,[""],"";0,["hé😀"],"hé😀";2147483647,["first"],"N"] in
 List.iter (fun (index,args,expected) -> let actual=runImage (binary index) args "" in if actual<>expected then failwith (Printf.sprintf "argv[%d]: expected %S, got %S" index expected actual)) argvCases;
 let effects=[Some "hé😀",false,0,"","hé😀";Some "",true,0,"","\n";None,false,1,"hello\nignored","hello";None,true,1,"hé😀\r\n","hé😀\n";None,false,1,"tail","tail";None,true,1,"","\n";None,true,2,"first\nsecond\n","first\nsecond\n";None,false,1,"a\000b\n","a\000b";None,false,1,"\n","";None,false,1,"a\rb\r\n","a\rb"] in
 List.iter (fun (literal,newline,reads,input,expected) -> let actual=runImage (presentationBinary literal newline reads) [] input in if actual<>expected then failwith (Printf.sprintf "presentation input %S: expected %S, got %S" input expected actual)) effects;
 let count=List.length argvCases+List.length effects+filesystemChecks ()+bufferChecks ()+memoryChecks ()+printingChecks ()+nativeEffectChecks ()+listReferenceChecks () in
 Printf.printf "%d/%d native ARM64 process executions passed\n" count count
