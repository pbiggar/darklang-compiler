[@@@warning "-4-42"]
(* X86_64ResolveTests.fs - Tests for x86-64 label resolution
   Verifies that CALL/JMP/Jcc labels are resolved to correct relative offsets. *)
open Dark_compiler
open X86_64
type testResult=(unit,string) result
(* Test: entry labels should be required explicitly, not replaced with offset 0. *)
let testRequireLabelPositionRejectsMissingStart ()=match X86_64_Resolve.requireLabelPosition "_start" StringOrder.Map.empty with
 |Error message when HostText.contains message "Missing required label: _start"->Ok ()
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
let tests=["Require label position rejects missing _start",testRequireLabelPositionRejectsMissingStart;"CALL + execute",testCallAndExecute]
