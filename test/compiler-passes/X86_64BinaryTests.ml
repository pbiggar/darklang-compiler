(*
   X86_64BinaryTests.ml - End-to-end test for x86-64 binary generation
   Generates a minimal x86-64 ELF binary and verifies it can be executed.
   This proves the encoder + ELF generation pipeline works end-to-end.
*)
[@@@warning "-4-42"]

open Dark_compiler
open X86_64

(*
   Generate machine code for "exit(42)" on x86-64 Linux:
   MOV RAX, 60    (syscall number for exit)
   MOV RDI, 42    (exit code)
   SYSCALL
   sys_exit = 60
   exit code
*)
let exitProgram exitCode =
  Bytes.concat Bytes.empty
    (List.map X86_64_Encoding.encodeInstruction
       [ MOV_imm32 (RAX, 60l); MOV_imm32 (RDI, Int32.of_int exitCode); SYSCALL ])

(*
   Test that we can generate a valid x86-64 ELF binary
   Verify ELF magic
   Verify 64-bit
   Verify little-endian
   Verify machine type is x86-64 (0x3E = 62 at offset 18-19, little-endian)
*)
let testGenerateElf () =
  let machineCode = exitProgram 42 in
  let binary =
    Binary_Generation_ELF_X86_64.createExecutableWithPools machineCode
      LiteralPool.emptyStringPool LiteralPool.emptyFloatPool false 0
  in
  if Bytes.sub_string binary 0 4 <> "\127ELF" then
    Error "Missing ELF magic bytes"
  else if Bytes.get binary 4 <> '\002' then Error "Not ELF64"
  else if Bytes.get binary 5 <> '\001' then Error "Not little-endian"
  else if Bytes.get binary 18 <> '\062' || Bytes.get binary 19 <> '\000' then
    Error
      (Printf.sprintf
         "Wrong machine type: expected 0x3E 0x00, got 0x%02X 0x%02X"
         (Char.code (Bytes.get binary 18))
         (Char.code (Bytes.get binary 19)))
  else Ok ()

let testElfIdentHelper () =
  let ident = ELF.createIdent () in
  let expected =
    Bytes.init 16 (fun n ->
        match n with
        | 0 -> ELF.ei_MAG0
        | 1 -> ELF.ei_MAG1
        | 2 -> ELF.ei_MAG2
        | 3 -> ELF.ei_MAG3
        | 4 -> ELF.elfclass64
        | 5 -> ELF.elfdata2lsb
        | 6 -> ELF.ev_CURRENT
        | 7 -> ELF.elfosabi_NONE
        | _ -> '\000')
  in
  if ident = expected then Ok ()
  else Error "ELF ident helper produced unexpected bytes"

let testCombinedInstructionEncodings () =
  let cases =
    [
      ( LEA_index (RAX, RBX, RCX, 4, 8l),
        Bytes.of_string "\x48\x8d\x44\x8b\x08",
        "indexed LEA" );
      ( ADD_load (RAX, RBP, -8l),
        Bytes.of_string "\x48\x03\x45\xf8",
        "memory ADD" );
      ( SUB_load (R9, R12, 16l),
        Bytes.of_string "\x4d\x2b\x4c\x24\x10",
        "memory SUB" );
      ( CMOVcc (NE, R10, R11),
        Bytes.of_string "\x4d\x0f\x45\xd3",
        "conditional move" );
    ]
  in
  let rec check = function
    | [] -> Ok ()
    | (instr, expected, name) :: rest ->
        let actual = X86_64_Encoding.encodeInstruction instr in
        if actual = expected then check rest
        else Error (name ^ ": encoding mismatch")
  in
  check cases

let unixSignalNumber signal =
  Option.value ~default:signal
    (List.assoc_opt signal
       [
         (Sys.sighup, 1);
         (Sys.sigint, 2);
         (Sys.sigquit, 3);
         (Sys.sigill, 4);
         (Sys.sigabrt, 6);
         (Sys.sigfpe, 8);
         (Sys.sigkill, 9);
         (Sys.sigusr1, 10);
         (Sys.sigsegv, 11);
         (Sys.sigusr2, 12);
         (Sys.sigpipe, 13);
         (Sys.sigalrm, 14);
         (Sys.sigterm, 15);
         (Sys.sigchld, 17);
         (Sys.sigcont, 18);
         (Sys.sigstop, 19);
         (Sys.sigtstp, 20);
         (Sys.sigttin, 21);
         (Sys.sigttou, 22);
         (Sys.sigurg, 23);
         (Sys.sigxcpu, 24);
         (Sys.sigxfsz, 25);
         (Sys.sigvtalrm, 26);
         (Sys.sigprof, 27);
       ])

(*
   Run an ELF binary, using the pinned QEMU when on a different architecture.
   Returns the exit code.
   On non-x86_64 hosts, use the image's pinned QEMU build.
*)
let runElfBinary binary =
  let tempPath = Filename.temp_file "dark-x64-" "" in
  let outcome =
    try
      Out_channel.with_open_bin tempPath (fun stream ->
          Out_channel.output_bytes stream binary;
          Out_channel.flush stream;
          Unix.fsync (Unix.descr_of_out_channel stream));
      let permissions = (Unix.stat tempPath).Unix.st_perm in
      Unix.chmod tempPath (permissions lor 0o100);
      let command, args =
        match Platform.detectArch () with
        | Ok Platform.X86_64 -> (tempPath, [| tempPath |])
        | _ ->
            ( "/opt/dcb/qemu/qemu-x86_64",
              [| "/opt/dcb/qemu/qemu-x86_64"; tempPath |] )
      in
      let stdoutRead, stdoutWrite = Unix.pipe ~cloexec:true () in
      let stderrRead, stderrWrite = Unix.pipe ~cloexec:true () in
      let process =
        try Unix.create_process command args Unix.stdin stdoutWrite stderrWrite
        with ex ->
          List.iter Unix.close
            [ stdoutRead; stdoutWrite; stderrRead; stderrWrite ];
          raise ex
      in
      Unix.close stdoutWrite;
      Unix.close stderrWrite;
      Fun.protect
        ~finally:(fun () ->
          Unix.close stdoutRead;
          Unix.close stderrRead)
        (fun () ->
          let deadline = Unix.gettimeofday () +. 10. in
          let rec wait () =
            match Unix.waitpid [ Unix.WNOHANG ] process with
            | 0, _ when Unix.gettimeofday () < deadline ->
                ignore (Unix.select [] [] [] 0.01);
                wait ()
            | 0, _ ->
                Unix.kill process Sys.sigkill;
                ignore (Unix.waitpid [] process);
                Error "Timed out executing x86-64 ELF binary"
            | _, Unix.WEXITED code -> Ok code
            | _, Unix.WSIGNALED signal | _, Unix.WSTOPPED signal ->
                Ok (128 + unixSignalNumber signal)
          in
          wait ())
    with ex ->
      Error
        ("Failed to execute binary: "
        ^
        match ex with
        | Unix.Unix_error (error, _, _) -> Unix.error_message error
        | Sys_error message -> message
        | _ -> Printexc.to_string ex)
  in
  (try Sys.remove tempPath with Sys_error _ -> ());
  outcome

(*
   Test that the generated binary executes correctly
*)
let testExecuteElf () =
  let machineCode = exitProgram 42 in
  let binary =
    Binary_Generation_ELF_X86_64.createExecutableWithPools machineCode
      LiteralPool.emptyStringPool LiteralPool.emptyFloatPool false 0
  in
  match runElfBinary binary with
  | Error err -> Error err
  | Ok exitCode ->
      if exitCode = 42 then Ok ()
      else Error (Printf.sprintf "Expected exit code 42, got %d" exitCode)

let tests =
  NativeRegressionTests.printingTests
  @ [
      ("ELF ident helper", testElfIdentHelper);
      ("x64 combined instruction encodings", testCombinedInstructionEncodings);
      ("Generate x86-64 ELF", testGenerateElf);
      ("Execute x86-64 ELF", testExecuteElf);
    ]
