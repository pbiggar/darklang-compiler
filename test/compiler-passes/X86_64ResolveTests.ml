[@@@warning "-4-42"]
(* X86_64ResolveTests.ml - Tests for x86-64 label resolution
   Verifies that CALL/JMP/Jcc labels are resolved to correct relative offsets. *)
open Dark_compiler
open X86_64
type testResult=(unit,string) result
(* Test: entry labels should be required explicitly, not replaced with offset 0. *)
let testRequireLabelPositionRejectsMissingStart ()=match X86_64_Resolve.requireLabelPosition "_start" StringOrder.Map.empty with
 |Error message when Text.contains message "Missing required label: _start"->Ok ()
 |Error message->Error ("Expected missing _start label error, got: "^message)
 |Ok offset->Error (Printf.sprintf "Expected missing _start label to fail, got offset %d" offset)
(* Test: generate and execute a program with a forward call *)
let testCallAndExecute ()=
 (* main: call func; mov rax,60; syscall
    func: mov rdi,42; ret
    Result: exit(42) *)
 let instructions=[CALL "func"; (* func sets RDI to 42, then returns here *)
 MOV_imm32 (RAX,60l); (* sys_exit *)
 SYSCALL;Label "func";MOV_imm32 (RDI,42l); (* exit code *) RET] in
 match X86_64_Resolve.resolveAndEncode instructions with Error error->Error ("Resolution failed: "^error)|Ok result->
 let binary=Binary_Generation_ELF_X86_64.createExecutableWithPools result.X86_64_Resolve.machineCode LiteralPool.emptyStringPool LiteralPool.emptyFloatPool false 0 in
 match X86_64BinaryTests.runElfBinary binary with Error error->Error error|Ok exitCode->if exitCode=42 then Ok () else Error (Printf.sprintf "Expected exit code 42, got %d" exitCode)
(* Reusing unpatched templates must preserve code/data relocations when the
   next executable has different label names and instruction positions. *)
let testCachedEncodingRelocations ()=
 let session=new CompilationSession.compilationSession () in
 Fun.protect ~finally:(fun ()->session#dispose) (fun ()->
  let encode=session#encodeX64Instruction in
  let instructions name padding=padding @ [LEA_rip (R11,"_leak_count");CALL name;
   Jcc (EQ,name);JMP name;Label name;RET] in
  let first=instructions "first" [] in
  let second=instructions "second" [MOV_imm32 (RDI,42l)] in
  let resolve encode instructions=Result.bind (X86_64_Resolve.resolveAndEncodeWith encode instructions) (fun resolved->
   let pool=X86_64_Resolve.collectStringPool instructions in
   let labels=X86_64_Resolve.dataLabelOffsets 120 (Bytes.length resolved.X86_64_Resolve.machineCode) pool in
   X86_64_Resolve.patchDataLabels resolved labels 120) in
  match resolve encode first with Error error->Error error|Ok firstResolved->
  let original=Bytes.copy firstResolved.X86_64_Resolve.machineCode in
  match resolve encode second,resolve X86_64_Encoding.encodeInstruction second,
    resolve X86_64_Encoding.encodeInstruction first with
  | Ok cached,Ok uncached,Ok freshFirst when cached=uncached && firstResolved=freshFirst
     && original=firstResolved.X86_64_Resolve.machineCode->Ok ()
  | Error error,_,_|_,Error error,_|_,_,Error error->Error error
  | _->Error "Cached instruction templates changed code/data relocations or an earlier executable")
let tests=["Require label position rejects missing _start",testRequireLabelPositionRejectsMissingStart;
 "CALL + execute",testCallAndExecute;"cached encoding preserves code and data relocations",testCachedEncodingRelocations]
