(*
   ProcessLifecycle.ml - Generate process lifecycle and argument-vector runtime helpers.
*)
open ARM64CodeGenTypes
open HeapAllocation
open LeakAccounting
open ARM64Operands

(*
   Generate heap initialization code for _start function
   Uses mmap to allocate 512MB of heap space and initializes X27/X28
   Memory layout:
   X27 -> [free list heads: 256 bytes (32 entries × 8 bytes)]
   X28 -> [heap allocation area: mapped heap after free list heads]
   Free list heads are indexed by (totalSize / 8), where totalSize includes
   the 8-byte ref count. Size class 0 and 1 are unused (too small).
   Size class 2 = 16 bytes, class 3 = 24 bytes, etc.
   X27 is the base for free list heads (constant after init)
   X28 is the bump pointer for new allocations
   MAP_PRIVATE | MAP_ANON
   MAP_PRIVATE | MAP_ANONYMOUS
   mmap(NULL, 512MB, PROT_READ|PROT_WRITE, flags, -1, 0)
   addr = NULL
   PROT_READ | PROT_WRITE
   flags
   X4 = 0
   X4 = ~0 = -1 (fd)
   offset = 0
   Check for mmap failure (returns -1 on error)
   X15 = 0
   X15 = -1
   Compare X0 with -1
   Skip exit if not error (+3 instructions)
   exit code = 1
   X0 now contains mmap result (valid address)
   X27 = free list heads base
   X28 = heap start
   No need to zero free list - MAP_ANONYMOUS provides zeroed pages
*)
let generateHeapInit (target: ARM64.targetConfig) =
    let freeListSize = 256
    in
    let os = ARM64.targetOS target
    in
    let syscalls = ARM64.targetSyscalls target
    in
    let mmapFlags =
        match os with
        | Platform.MacOS -> 0x1002
        | Platform.Linux -> 0x22
    in
    let heapSizeForMmap = loadImmediate Symbolic.X1 heapMmapSizeBytes
    in
    [

        Symbolic.MOVZ (Symbolic.X0, 0, 0);
    ]
    @ heapSizeForMmap
    @ [
        Symbolic.MOVZ (Symbolic.X2, 3, 0);
        Symbolic.MOVZ (Symbolic.X3, mmapFlags, 0);
        Symbolic.MOVZ (Symbolic.X4, 0, 0);
        Symbolic.MVN (Symbolic.X4, Symbolic.X4);
        Symbolic.MOVZ (Symbolic.X5, 0, 0);
        Symbolic.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.mmap, 0);
        Symbolic.SVC syscalls.ARM64.svcImmediate;

        Symbolic.MOVZ (Symbolic.X15, 0, 0);
        Symbolic.MVN (Symbolic.X15, Symbolic.X15);
        Symbolic.CMP_reg (Symbolic.X0, Symbolic.X15);
        Symbolic.B_cond (Symbolic.NE, 3);
        Symbolic.MOVZ (Symbolic.X0, 1, 0);
        Symbolic.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.exit, 0);
        Symbolic.SVC syscalls.ARM64.svcImmediate;

        Symbolic.MOV_reg (Symbolic.X27, Symbolic.X0);
        Symbolic.ADD_imm (Symbolic.X28, Symbolic.X27, freeListSize);

    ]

(*
   Linux AArch64 shell runner. Generated binaries remain libc-free, and both
   redirected streams are made nonblocking and drained on every wait probe.
   Return argv[index + 1] as a nullable String pointer. Native argv entries are
   zero-terminated bytes, so present values are copied into managed Dark strings.
   The root _start frame terminates the normal frame-pointer chain. Its initial
   stack layout keeps argc at +16, argv[0] at +24, and the first positional
   argument at +32, so no register is reserved
   for CLI state between calls.
*)
let generateCliArgvHelper (_ctx: ARM64CodeGenTypes.codeGenContext) (label: string) =
    let missingLabel = (label ^ "_missing")
    in
    let lengthLabel = (label ^ "_length")
    in
    let lengthDoneLabel = (label ^ "_length_done")
    in
    let copyLabel = (label ^ "_copy")
    in
    let copyDoneLabel = (label ^ "_copy_done")
    in
    [ Symbolic.Label label;
      Symbolic.CMP_imm (Symbolic.X0, 0);
      Symbolic.B_cond_label (Symbolic.LT, missingLabel);
      Symbolic.MOV_reg (Symbolic.X1, Symbolic.X29);
      Symbolic.Label (label ^ "_find_root");
      Symbolic.LDR (Symbolic.X2, Symbolic.X1, 0);
      Symbolic.CBZ (Symbolic.X2, (label ^ "_root_found"));
      Symbolic.MOV_reg (Symbolic.X1, Symbolic.X2);
      Symbolic.B_label (label ^ "_find_root");
      Symbolic.Label (label ^ "_root_found");
      Symbolic.LDR (Symbolic.X2, Symbolic.X1, 16);
      Symbolic.SUB_imm (Symbolic.X2, Symbolic.X2, 1);
      Symbolic.CMP_reg (Symbolic.X0, Symbolic.X2);
      Symbolic.B_cond_label (Symbolic.GE, missingLabel);
      Symbolic.LSL_imm (Symbolic.X2, Symbolic.X0, 3);
      Symbolic.ADD_reg (Symbolic.X1, Symbolic.X1, Symbolic.X2);
      Symbolic.ADD_imm (Symbolic.X1, Symbolic.X1, 32);
      Symbolic.LDR (Symbolic.X3, Symbolic.X1, 0);
      Symbolic.MOVZ (Symbolic.X4, 0, 0);
      Symbolic.MOV_reg (Symbolic.X5, Symbolic.X3);
      Symbolic.Label lengthLabel;
      Symbolic.LDRB_imm (Symbolic.X6, Symbolic.X5, 0);
      Symbolic.CBZ (Symbolic.X6, lengthDoneLabel);
      Symbolic.ADD_imm (Symbolic.X4, Symbolic.X4, 1);
      Symbolic.ADD_imm (Symbolic.X5, Symbolic.X5, 1);
      Symbolic.B_label lengthLabel;
      Symbolic.Label lengthDoneLabel;
      Symbolic.MOV_reg (Symbolic.X7, Symbolic.X28);
      Symbolic.MOVZ (Symbolic.X1, 1, 0);
      Symbolic.STR (Symbolic.X1, Symbolic.X7, 0);
      Symbolic.STR (Symbolic.X4, Symbolic.X7, 8);
      Symbolic.ADD_imm (Symbolic.X9, Symbolic.X4, 7);
      Symbolic.MOVZ (Symbolic.X10, 3, 0);
      Symbolic.LSR_reg (Symbolic.X9, Symbolic.X9, Symbolic.X10);
      Symbolic.LSL_reg (Symbolic.X9, Symbolic.X9, Symbolic.X10);
      Symbolic.ADD_imm (Symbolic.X10, Symbolic.X9, 16);
      Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X10);
      Symbolic.MOV_reg (Symbolic.X5, Symbolic.X3);
      Symbolic.ADD_imm (Symbolic.X10, Symbolic.X7, 16);
      Symbolic.MOV_reg (Symbolic.X2, Symbolic.X4);
      Symbolic.Label copyLabel;
      Symbolic.CBZ (Symbolic.X2, copyDoneLabel);
      Symbolic.LDRB_imm (Symbolic.X1, Symbolic.X5, 0);
      Symbolic.STRB_reg (Symbolic.X1, Symbolic.X10);
      Symbolic.ADD_imm (Symbolic.X5, Symbolic.X5, 1);
      Symbolic.ADD_imm (Symbolic.X10, Symbolic.X10, 1);
      Symbolic.SUB_imm (Symbolic.X2, Symbolic.X2, 1);
      Symbolic.B_label copyLabel;
      Symbolic.Label copyDoneLabel;
      Symbolic.MOV_reg (Symbolic.X0, Symbolic.X7);
      Symbolic.RET;
      Symbolic.Label missingLabel;
      Symbolic.MOVZ (Symbolic.X0, 0, 0);
      Symbolic.RET ]



(*
   Start a shell-language command and retain its pid and descriptors in the
   fixed process table rooted at X25.
   Recover inherited envp from the root frame.
   Keep raw accumulated output outside the managed object graph. Each
   communicate call copies only its newly-read suffix into immutable
   managed strings; terminate copies the complete buffers.
*)
let generateLinuxCliSpawnProcessHelper () =
    let syscall number =
        [ Symbolic.MOVZ (Symbolic.X8, number, 0);
          Symbolic.SVC 0 ]
    in
    let zero reg = Symbolic.MOVZ (reg, 0, 0)
    in
    let pairFd slot shift =
        [ Symbolic.LDR (Symbolic.X0, Symbolic.SP, slot);
          Symbolic.LSR_imm (Symbolic.X0, Symbolic.X0, shift);
          Symbolic.AND_imm (Symbolic.X0, Symbolic.X0, 0xffffffffL) ]
    in
    let closeFd slot shift = pairFd slot shift @ syscall 57
    in
    let setNonblocking slot =
        pairFd slot 0
        @ [ Symbolic.MOVZ (Symbolic.X1, 4, 0);
            Symbolic.MOVZ (Symbolic.X2, 2048, 0) ]
        @ syscall 25
    in
    [ Symbolic.Label "__dark_cli_spawn_process";
      Symbolic.STP_pre (Symbolic.X29, Symbolic.X30, Symbolic.SP, -16);
      Symbolic.MOV_reg (Symbolic.X29, Symbolic.SP);
      Symbolic.STP_pre (Symbolic.X19, Symbolic.X20, Symbolic.SP, -16);
      Symbolic.STP_pre (Symbolic.X21, Symbolic.X22, Symbolic.SP, -16);
      Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 80);
      Symbolic.CBNZ (Symbolic.X25, "__dark_spawn_table_ready");
      Symbolic.MOV_reg (Symbolic.X25, Symbolic.X28);
      Symbolic.MOVZ (Symbolic.X9, 4096, 0);
      Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X9);
      Symbolic.Label "__dark_spawn_table_ready";
      Symbolic.MOV_reg (Symbolic.X19, Symbolic.X28);
      Symbolic.LDR (Symbolic.X10, Symbolic.X0, 8);
      Symbolic.ADD_imm (Symbolic.X11, Symbolic.X0, 16);
      zero Symbolic.X12;
      Symbolic.Label "__dark_spawn_command_copy";
      Symbolic.CMP_reg (Symbolic.X12, Symbolic.X10);
      Symbolic.B_cond_label (Symbolic.GE, "__dark_spawn_command_copied");
      Symbolic.LDRB (Symbolic.X13, Symbolic.X11, Symbolic.X12);
      Symbolic.ADD_reg (Symbolic.X14, Symbolic.X19, Symbolic.X12);
      Symbolic.STRB_reg (Symbolic.X13, Symbolic.X14);
      Symbolic.ADD_imm (Symbolic.X12, Symbolic.X12, 1);
      Symbolic.B_label "__dark_spawn_command_copy";
      Symbolic.Label "__dark_spawn_command_copied";
      Symbolic.ADD_reg (Symbolic.X14, Symbolic.X19, Symbolic.X10);
      zero Symbolic.X13;
      Symbolic.STRB_reg (Symbolic.X13, Symbolic.X14);
      Symbolic.ADD_imm (Symbolic.X10, Symbolic.X10, 8);
      Symbolic.LSR_imm (Symbolic.X10, Symbolic.X10, 3);
      Symbolic.LSL_imm (Symbolic.X10, Symbolic.X10, 3);
      Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X10);

      Symbolic.MOV_reg (Symbolic.X14, Symbolic.X29);
      Symbolic.Label "__dark_spawn_find_root";
      Symbolic.LDR (Symbolic.X13, Symbolic.X14, 0);
      Symbolic.CBZ (Symbolic.X13, "__dark_spawn_root_found");
      Symbolic.MOV_reg (Symbolic.X14, Symbolic.X13);
      Symbolic.B_label "__dark_spawn_find_root";
      Symbolic.Label "__dark_spawn_root_found";
      Symbolic.ADD_imm (Symbolic.X14, Symbolic.X14, 24);
      Symbolic.Label "__dark_spawn_find_envp";
      Symbolic.LDR (Symbolic.X13, Symbolic.X14, 0);
      Symbolic.ADD_imm (Symbolic.X14, Symbolic.X14, 8);
      Symbolic.CBNZ (Symbolic.X13, "__dark_spawn_find_envp");
      Symbolic.MOV_reg (Symbolic.X22, Symbolic.X14);
      Symbolic.ADD_imm (Symbolic.X0, Symbolic.SP, 0);
      zero Symbolic.X1 ]
    @ syscall 59
    @ [ Symbolic.CMP_imm (Symbolic.X0, 0);
        Symbolic.B_cond_label (Symbolic.LT, "__dark_spawn_failed");
        Symbolic.ADD_imm (Symbolic.X0, Symbolic.SP, 8);
        zero Symbolic.X1 ]
    @ syscall 59
    @ [ Symbolic.CMP_imm (Symbolic.X0, 0);
        Symbolic.B_cond_label (Symbolic.LT, "__dark_spawn_failed_close_stdin");
        Symbolic.ADD_imm (Symbolic.X0, Symbolic.SP, 16);
        zero Symbolic.X1 ]
    @ syscall 59
    @ [ Symbolic.CMP_imm (Symbolic.X0, 0);
        Symbolic.B_cond_label (Symbolic.LT, "__dark_spawn_failed_close_output");
        Symbolic.MOVZ (Symbolic.X0, 17, 0);
        zero Symbolic.X1; zero Symbolic.X2; zero Symbolic.X3; zero Symbolic.X4 ]
    @ syscall 220
    @ [ Symbolic.CBZ (Symbolic.X0, "__dark_spawn_child");
        Symbolic.CMP_imm (Symbolic.X0, 0);
        Symbolic.B_cond_label (Symbolic.LT, "__dark_spawn_failed_close_all");
        Symbolic.MOV_reg (Symbolic.X21, Symbolic.X0) ]
    @ closeFd 0 0 @ closeFd 8 32 @ closeFd 16 32
    @ setNonblocking 8 @ setNonblocking 16
    @ [ Symbolic.MOVZ (Symbolic.X0, 1, 0);
        Symbolic.Label "__dark_spawn_find_slot";
        Symbolic.CMP_imm (Symbolic.X0, 63);
        Symbolic.B_cond_label (Symbolic.GT, "__dark_spawn_no_slot");
        Symbolic.LSL_imm (Symbolic.X10, Symbolic.X0, 6);
        Symbolic.ADD_reg (Symbolic.X10, Symbolic.X25, Symbolic.X10);
        Symbolic.LDR (Symbolic.X9, Symbolic.X10, 0);
        Symbolic.CBZ (Symbolic.X9, "__dark_spawn_slot_found");
        Symbolic.ADD_imm (Symbolic.X0, Symbolic.X0, 1);
        Symbolic.B_label "__dark_spawn_find_slot";
        Symbolic.Label "__dark_spawn_slot_found";
        Symbolic.MOVZ (Symbolic.X9, 1, 0);
        Symbolic.STR (Symbolic.X9, Symbolic.X10, 0);
        Symbolic.STR (Symbolic.X21, Symbolic.X10, 8);
        Symbolic.LDR (Symbolic.X9, Symbolic.SP, 0);
        Symbolic.LSR_imm (Symbolic.X9, Symbolic.X9, 32);
        Symbolic.STR (Symbolic.X9, Symbolic.X10, 16);
        Symbolic.LDR (Symbolic.X9, Symbolic.SP, 8);
        Symbolic.AND_imm (Symbolic.X9, Symbolic.X9, 0xffffffffL);
        Symbolic.STR (Symbolic.X9, Symbolic.X10, 24);
        Symbolic.LDR (Symbolic.X9, Symbolic.SP, 16);
        Symbolic.AND_imm (Symbolic.X9, Symbolic.X9, 0xffffffffL);
        Symbolic.STR (Symbolic.X9, Symbolic.X10, 32);



        Symbolic.STR (Symbolic.X28, Symbolic.X10, 48);
        zero Symbolic.X9;
        Symbolic.STR (Symbolic.X9, Symbolic.X28, 0) ]
    @ loadImmediate Symbolic.X9 1048584L
    @ [ Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X9);
        Symbolic.STR (Symbolic.X28, Symbolic.X10, 56);
        zero Symbolic.X9;
        Symbolic.STR (Symbolic.X9, Symbolic.X28, 0) ]
    @ loadImmediate Symbolic.X9 1048584L
    @ [ Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X9);
        Symbolic.B_label "__dark_spawn_return";
        Symbolic.Label "__dark_spawn_no_slot";
        Symbolic.MOV_reg (Symbolic.X0, Symbolic.X21);
        Symbolic.MOVZ (Symbolic.X1, 9, 0) ]
    @ syscall 129
    @ [ Symbolic.MOVZ (Symbolic.X0, 0, 0);
        Symbolic.MVN (Symbolic.X0, Symbolic.X0);
        Symbolic.B_label "__dark_spawn_return";
        Symbolic.Label "__dark_spawn_failed_close_all" ]
    @ closeFd 16 0 @ closeFd 16 32
    @ [ Symbolic.Label "__dark_spawn_failed_close_output" ]
    @ closeFd 8 0 @ closeFd 8 32
    @ [ Symbolic.Label "__dark_spawn_failed_close_stdin" ]
    @ closeFd 0 0 @ closeFd 0 32
    @ [ Symbolic.Label "__dark_spawn_failed";
        Symbolic.MOVZ (Symbolic.X0, 0, 0);
        Symbolic.MVN (Symbolic.X0, Symbolic.X0);
        Symbolic.Label "__dark_spawn_return";
        Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 80);
        Symbolic.LDP_post (Symbolic.X21, Symbolic.X22, Symbolic.SP, 16);
        Symbolic.LDP_post (Symbolic.X19, Symbolic.X20, Symbolic.SP, 16);
        Symbolic.LDP_post (Symbolic.X29, Symbolic.X30, Symbolic.SP, 16);
        Symbolic.RET;
        Symbolic.Label "__dark_spawn_child" ]
    @ pairFd 0 0
    @ [ zero Symbolic.X1; zero Symbolic.X2 ]
    @ syscall 24
    @ pairFd 8 32
    @ [ Symbolic.MOVZ (Symbolic.X1, 1, 0); zero Symbolic.X2 ]
    @ syscall 24
    @ pairFd 16 32
    @ [ Symbolic.MOVZ (Symbolic.X1, 2, 0); zero Symbolic.X2 ]
    @ syscall 24
    @ closeFd 0 0 @ closeFd 0 32 @ closeFd 8 0 @ closeFd 8 32 @ closeFd 16 0 @ closeFd 16 32
    @ loadStringLiteralPointer Symbolic.X20 "/bin/bash"
    @ loadStringLiteralPointer Symbolic.X9 "-c"
    @ [ Symbolic.ADD_imm (Symbolic.X20, Symbolic.X20, 16);
        Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 16);
        Symbolic.STR (Symbolic.X20, Symbolic.SP, 32);
        Symbolic.STR (Symbolic.X9, Symbolic.SP, 40);
        Symbolic.STR (Symbolic.X19, Symbolic.SP, 48);
        zero Symbolic.X9;
        Symbolic.STR (Symbolic.X9, Symbolic.SP, 56);
        Symbolic.MOV_reg (Symbolic.X0, Symbolic.X20);
        Symbolic.ADD_imm (Symbolic.X1, Symbolic.SP, 32);
        Symbolic.MOV_reg (Symbolic.X2, Symbolic.X22) ]
    @ syscall 221
    @ [ Symbolic.MOVZ (Symbolic.X0, 127, 0) ]
    @ syscall 93


(*
   Communicate with and terminate children tracked by the X25 process table.
   Write input plus the interpreter's WriteLine newline.
   wait4 writes a 32-bit status. Clear the full stack word so the
   64-bit decode below cannot observe stale upper bytes.
   A live child may await more input. Return after its response has
   been quiet for one poll, while still allowing chunked output.
   Copy the newly-read suffix (or the full accumulation for the
   terminate caller) into immutable managed strings.
   wait4 writes only the low 32 bits of the status slot.
   overwritten below; keep X0 defined
   Call communicate with empty input to drain and box the outcome.
   Recover handle from slot address rather than the allocation counter.
*)
let generateLinuxCliProcessLifecycleHelpers (ctx: ARM64CodeGenTypes.codeGenContext) =
    let syscall number =
        [ Symbolic.MOVZ (Symbolic.X8, number, 0);
          Symbolic.SVC 0 ]
    in
    let zero reg = Symbolic.MOVZ (reg, 0, 0)
    in
    let finalizeString buffer lengthReg =
        [ Symbolic.STR (lengthReg, buffer, 8);
          Symbolic.MOVZ (Symbolic.X10, 1, 0);
          Symbolic.STR (Symbolic.X10, buffer, 0);
        ]
    in
    let epilogue =
        [ Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 64);
          Symbolic.LDP_post (Symbolic.X23, Symbolic.X24, Symbolic.SP, 16);
          Symbolic.LDP_post (Symbolic.X21, Symbolic.X22, Symbolic.SP, 16);
          Symbolic.LDP_post (Symbolic.X19, Symbolic.X20, Symbolic.SP, 16);
          Symbolic.LDP_post (Symbolic.X29, Symbolic.X30, Symbolic.SP, 16);
          Symbolic.RET ]
    in
    let invalidOutcome label =
        loadStringLiteralPointer Symbolic.X8 ""
        @ loadStringLiteralPointer Symbolic.X9 "Process not found"
        @ [ Symbolic.MOV_reg (Symbolic.X0, Symbolic.X28);
            Symbolic.ADD_imm (Symbolic.X28, Symbolic.X28, 32);
            zero Symbolic.X10;
            Symbolic.MVN (Symbolic.X10, Symbolic.X10);
            Symbolic.STR (Symbolic.X10, Symbolic.X0, 0);
            Symbolic.STR (Symbolic.X8, Symbolic.X0, 8);
            Symbolic.STR (Symbolic.X9, Symbolic.X0, 16);
            Symbolic.MOVZ (Symbolic.X10, 1, 0);
            Symbolic.STR (Symbolic.X10, Symbolic.X0, 24) ]
        @ generateLeakCounterInc ctx
        @ [ Symbolic.B_label label ]
    in
    let communicate =
        [ Symbolic.Label "__dark_cli_process_io";
          Symbolic.STP_pre (Symbolic.X29, Symbolic.X30, Symbolic.SP, -16);
          Symbolic.MOV_reg (Symbolic.X29, Symbolic.SP);
          Symbolic.STP_pre (Symbolic.X19, Symbolic.X20, Symbolic.SP, -16);
          Symbolic.STP_pre (Symbolic.X21, Symbolic.X22, Symbolic.SP, -16);
          Symbolic.STP_pre (Symbolic.X23, Symbolic.X24, Symbolic.SP, -16);
          Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 64);
          Symbolic.MOV_reg (Symbolic.X19, Symbolic.X0);
          Symbolic.MOV_reg (Symbolic.X20, Symbolic.X1);
          Symbolic.STR (Symbolic.X2, Symbolic.SP, 24);
          Symbolic.CMP_imm (Symbolic.X19, 1);
          Symbolic.B_cond_label (Symbolic.LT, "__dark_process_io_invalid");
          Symbolic.CMP_imm (Symbolic.X19, 63);
          Symbolic.B_cond_label (Symbolic.GT, "__dark_process_io_invalid");
          Symbolic.LSL_imm (Symbolic.X9, Symbolic.X19, 6);
          Symbolic.ADD_reg (Symbolic.X21, Symbolic.X25, Symbolic.X9);
          Symbolic.LDR (Symbolic.X9, Symbolic.X21, 0);
          Symbolic.CBZ (Symbolic.X9, "__dark_process_io_invalid");
          Symbolic.LDR (Symbolic.X22, Symbolic.X21, 48);
          Symbolic.LDR (Symbolic.X23, Symbolic.X21, 56);
          Symbolic.LDR (Symbolic.X9, Symbolic.X22, 0);
          Symbolic.STR (Symbolic.X9, Symbolic.SP, 0);
          Symbolic.LDR (Symbolic.X9, Symbolic.X23, 0);
          Symbolic.STR (Symbolic.X9, Symbolic.SP, 8);
          Symbolic.LDR (Symbolic.X9, Symbolic.SP, 24);
          Symbolic.CBZ (Symbolic.X9, "__dark_process_io_suffix_ready");
          zero Symbolic.X9;
          Symbolic.STR (Symbolic.X9, Symbolic.SP, 0);
          Symbolic.STR (Symbolic.X9, Symbolic.SP, 8);
          Symbolic.Label "__dark_process_io_suffix_ready";
          zero Symbolic.X24;
          zero Symbolic.X9;
          Symbolic.STR (Symbolic.X9, Symbolic.SP, 56);

          Symbolic.LDR (Symbolic.X2, Symbolic.X20, 8);
          Symbolic.STR (Symbolic.X2, Symbolic.SP, 16);
          Symbolic.CBZ (Symbolic.X2, "__dark_process_io_read_stdout");
          Symbolic.LDR (Symbolic.X0, Symbolic.X21, 16);
          Symbolic.ADD_imm (Symbolic.X1, Symbolic.X20, 16) ]
        @ syscall 64
        @ [ Symbolic.MOVZ (Symbolic.X9, 10, 0);
            Symbolic.STRB (Symbolic.X9, Symbolic.SP, 56);
            Symbolic.LDR (Symbolic.X0, Symbolic.X21, 16);
            Symbolic.ADD_imm (Symbolic.X1, Symbolic.SP, 56);
            Symbolic.MOVZ (Symbolic.X2, 1, 0) ]
        @ syscall 64
        @ [ zero Symbolic.X9;
            Symbolic.STR (Symbolic.X9, Symbolic.SP, 56);
            Symbolic.Label "__dark_process_io_read_stdout";
            Symbolic.LDR (Symbolic.X19, Symbolic.X22, 0);
            Symbolic.ADD_imm (Symbolic.X1, Symbolic.X22, 8);
            Symbolic.ADD_reg (Symbolic.X1, Symbolic.X1, Symbolic.X19) ]
        @ loadImmediate Symbolic.X2 1048576L
        @ [ Symbolic.SUB_reg (Symbolic.X2, Symbolic.X2, Symbolic.X19);
            Symbolic.CBZ (Symbolic.X2, "__dark_process_io_read_stderr");
            Symbolic.LDR (Symbolic.X0, Symbolic.X21, 24) ]
        @ syscall 63
        @ [ Symbolic.CMP_imm (Symbolic.X0, 0);
            Symbolic.B_cond_label (Symbolic.LE, "__dark_process_io_read_stderr");
            Symbolic.ADD_reg (Symbolic.X19, Symbolic.X19, Symbolic.X0);
            Symbolic.STR (Symbolic.X19, Symbolic.X22, 0);
            Symbolic.B_label "__dark_process_io_read_stdout";
            Symbolic.Label "__dark_process_io_read_stderr";
            Symbolic.LDR (Symbolic.X20, Symbolic.X23, 0);
            Symbolic.ADD_imm (Symbolic.X1, Symbolic.X23, 8);
            Symbolic.ADD_reg (Symbolic.X1, Symbolic.X1, Symbolic.X20) ]
        @ loadImmediate Symbolic.X2 1048576L
        @ [ Symbolic.SUB_reg (Symbolic.X2, Symbolic.X2, Symbolic.X20);
            Symbolic.CBZ (Symbolic.X2, "__dark_process_io_status");
            Symbolic.LDR (Symbolic.X0, Symbolic.X21, 32) ]
        @ syscall 63
        @ [ Symbolic.CMP_imm (Symbolic.X0, 0);
            Symbolic.B_cond_label (Symbolic.LE, "__dark_process_io_status");
            Symbolic.ADD_reg (Symbolic.X20, Symbolic.X20, Symbolic.X0);
            Symbolic.STR (Symbolic.X20, Symbolic.X23, 0);
            Symbolic.B_label "__dark_process_io_read_stderr";
            Symbolic.Label "__dark_process_io_status";
            Symbolic.LDR (Symbolic.X9, Symbolic.X21, 0);
            Symbolic.CMP_imm (Symbolic.X9, 2);
            Symbolic.B_cond_label (Symbolic.EQ, "__dark_process_io_stored_status");
            Symbolic.LDR (Symbolic.X0, Symbolic.X21, 8);
            Symbolic.ADD_imm (Symbolic.X1, Symbolic.SP, 32);
            Symbolic.MOVZ (Symbolic.X2, 1, 0);
            zero Symbolic.X3;


            zero Symbolic.X9;
            Symbolic.STR (Symbolic.X9, Symbolic.SP, 32) ]
        @ syscall 260
        @ [ Symbolic.CMP_imm (Symbolic.X0, 0);
            Symbolic.B_cond_label (Symbolic.LT, "__dark_process_io_read_stdout");
            Symbolic.CBNZ (Symbolic.X0, "__dark_process_io_finished");
            Symbolic.LDR (Symbolic.X9, Symbolic.SP, 16);
            Symbolic.CBZ (Symbolic.X9, "__dark_process_io_running");


            Symbolic.LDR (Symbolic.X19, Symbolic.X22, 0);
            Symbolic.LDR (Symbolic.X20, Symbolic.X23, 0);
            Symbolic.ADD_reg (Symbolic.X9, Symbolic.X19, Symbolic.X20);
            Symbolic.LDR (Symbolic.X10, Symbolic.SP, 0);
            Symbolic.SUB_reg (Symbolic.X9, Symbolic.X9, Symbolic.X10);
            Symbolic.LDR (Symbolic.X10, Symbolic.SP, 8);
            Symbolic.SUB_reg (Symbolic.X9, Symbolic.X9, Symbolic.X10);
            Symbolic.LDR (Symbolic.X10, Symbolic.SP, 56);
            Symbolic.CMP_reg (Symbolic.X9, Symbolic.X10);
            Symbolic.B_cond_label (Symbolic.NE, "__dark_process_io_response_changed");
            Symbolic.CBNZ (Symbolic.X9, "__dark_process_io_running");
            Symbolic.B_label "__dark_process_io_poll";
            Symbolic.Label "__dark_process_io_response_changed";
            Symbolic.STR (Symbolic.X9, Symbolic.SP, 56);
            zero Symbolic.X24;
            Symbolic.Label "__dark_process_io_poll";
            Symbolic.ADD_imm (Symbolic.X24, Symbolic.X24, 1);
            Symbolic.CMP_imm (Symbolic.X24, 100);
            Symbolic.B_cond_label (Symbolic.GE, "__dark_process_io_running");
            zero Symbolic.X9;
            Symbolic.STR (Symbolic.X9, Symbolic.SP, 40) ]
        @ loadImmediate Symbolic.X9 100000000L
        @ [ Symbolic.STR (Symbolic.X9, Symbolic.SP, 48);
            Symbolic.ADD_imm (Symbolic.X0, Symbolic.SP, 40);
            zero Symbolic.X1 ]
        @ syscall 101
        @ [ Symbolic.B_label "__dark_process_io_read_stdout";
            Symbolic.Label "__dark_process_io_finished";
            Symbolic.LDR (Symbolic.X10, Symbolic.SP, 32);
            Symbolic.AND_imm (Symbolic.X11, Symbolic.X10, 0x7fL);
            Symbolic.CBNZ (Symbolic.X11, "__dark_process_io_signaled");
            Symbolic.LSR_imm (Symbolic.X24, Symbolic.X10, 8);
            Symbolic.B_label "__dark_process_io_store_status";
            Symbolic.Label "__dark_process_io_signaled";
            Symbolic.ADD_imm (Symbolic.X24, Symbolic.X11, 128);
            Symbolic.Label "__dark_process_io_store_status";
            Symbolic.MOVZ (Symbolic.X9, 2, 0);
            Symbolic.STR (Symbolic.X9, Symbolic.X21, 0);
            Symbolic.STR (Symbolic.X24, Symbolic.X21, 40);
            Symbolic.B_label "__dark_process_io_build";
            Symbolic.Label "__dark_process_io_stored_status";
            Symbolic.LDR (Symbolic.X24, Symbolic.X21, 40);
            Symbolic.B_label "__dark_process_io_build";
            Symbolic.Label "__dark_process_io_running";
            zero Symbolic.X24;
            Symbolic.Label "__dark_process_io_build";


            Symbolic.MOV_reg (Symbolic.X19, Symbolic.X28) ]
        @ loadImmediate Symbolic.X9 1048592L
        @ [ Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X9);
            Symbolic.LDR (Symbolic.X10, Symbolic.X22, 0);
            Symbolic.LDR (Symbolic.X11, Symbolic.SP, 0);
            Symbolic.SUB_reg (Symbolic.X10, Symbolic.X10, Symbolic.X11);
            zero Symbolic.X12;
            Symbolic.Label "__dark_process_io_copy_stdout";
            Symbolic.CMP_reg (Symbolic.X12, Symbolic.X10);
            Symbolic.B_cond_label (Symbolic.GE, "__dark_process_io_stdout_copied");
            Symbolic.ADD_imm (Symbolic.X13, Symbolic.X22, 8);
            Symbolic.ADD_reg (Symbolic.X13, Symbolic.X13, Symbolic.X11);
            Symbolic.LDRB (Symbolic.X14, Symbolic.X13, Symbolic.X12);
            Symbolic.ADD_imm (Symbolic.X15, Symbolic.X19, 16);
            Symbolic.ADD_reg (Symbolic.X15, Symbolic.X15, Symbolic.X12);
            Symbolic.STRB_reg (Symbolic.X14, Symbolic.X15);
            Symbolic.ADD_imm (Symbolic.X12, Symbolic.X12, 1);
            Symbolic.B_label "__dark_process_io_copy_stdout";
            Symbolic.Label "__dark_process_io_stdout_copied";
            Symbolic.MOV_reg (Symbolic.X20, Symbolic.X28);
            Symbolic.STR (Symbolic.X10, Symbolic.SP, 0) ]
        @ loadImmediate Symbolic.X9 1048592L
        @ [ Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X9);
            Symbolic.LDR (Symbolic.X10, Symbolic.X23, 0);
            Symbolic.LDR (Symbolic.X11, Symbolic.SP, 8);
            Symbolic.SUB_reg (Symbolic.X10, Symbolic.X10, Symbolic.X11);
            zero Symbolic.X12;
            Symbolic.Label "__dark_process_io_copy_stderr";
            Symbolic.CMP_reg (Symbolic.X12, Symbolic.X10);
            Symbolic.B_cond_label (Symbolic.GE, "__dark_process_io_stderr_copied");
            Symbolic.ADD_imm (Symbolic.X13, Symbolic.X23, 8);
            Symbolic.ADD_reg (Symbolic.X13, Symbolic.X13, Symbolic.X11);
            Symbolic.LDRB (Symbolic.X14, Symbolic.X13, Symbolic.X12);
            Symbolic.ADD_imm (Symbolic.X15, Symbolic.X20, 16);
            Symbolic.ADD_reg (Symbolic.X15, Symbolic.X15, Symbolic.X12);
            Symbolic.STRB_reg (Symbolic.X14, Symbolic.X15);
            Symbolic.ADD_imm (Symbolic.X12, Symbolic.X12, 1);
            Symbolic.B_label "__dark_process_io_copy_stderr";
            Symbolic.Label "__dark_process_io_stderr_copied";
            Symbolic.STR (Symbolic.X10, Symbolic.SP, 8) ]
        @ [ Symbolic.LDR (Symbolic.X10, Symbolic.SP, 0) ]
        @ finalizeString Symbolic.X19 Symbolic.X10
        @ [ Symbolic.LDR (Symbolic.X10, Symbolic.SP, 8) ]
        @ finalizeString Symbolic.X20 Symbolic.X10
        @ [ Symbolic.MOV_reg (Symbolic.X0, Symbolic.X28);
            Symbolic.ADD_imm (Symbolic.X28, Symbolic.X28, 32);
            Symbolic.STR (Symbolic.X24, Symbolic.X0, 0);
            Symbolic.STR (Symbolic.X19, Symbolic.X0, 8);
            Symbolic.STR (Symbolic.X20, Symbolic.X0, 16);
            Symbolic.MOVZ (Symbolic.X9, 1, 0);
            Symbolic.STR (Symbolic.X9, Symbolic.X0, 24) ]
        @ generateLeakCounterInc ctx
        @ generateLeakCounterInc ctx
        @ generateLeakCounterInc ctx
        @ [ Symbolic.B_label "__dark_process_io_return";
            Symbolic.Label "__dark_process_io_invalid" ]
        @ invalidOutcome "__dark_process_io_return"
        @ [ Symbolic.Label "__dark_process_io_return" ]
        @ epilogue
    in
    let terminate =
        [ Symbolic.Label "__dark_cli_terminate_process";
          Symbolic.STP_pre (Symbolic.X29, Symbolic.X30, Symbolic.SP, -16);
          Symbolic.MOV_reg (Symbolic.X29, Symbolic.SP);
          Symbolic.STP_pre (Symbolic.X19, Symbolic.X20, Symbolic.SP, -16);
          Symbolic.STP_pre (Symbolic.X21, Symbolic.X22, Symbolic.SP, -16);
          Symbolic.STP_pre (Symbolic.X23, Symbolic.X24, Symbolic.SP, -16);
          Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 64);
          Symbolic.CMP_imm (Symbolic.X0, 1);
          Symbolic.B_cond_label (Symbolic.LT, "__dark_terminate_invalid");
          Symbolic.CMP_imm (Symbolic.X0, 63);
          Symbolic.B_cond_label (Symbolic.GT, "__dark_terminate_invalid");
          Symbolic.LSL_imm (Symbolic.X9, Symbolic.X0, 6);
          Symbolic.ADD_reg (Symbolic.X21, Symbolic.X25, Symbolic.X9);
          Symbolic.LDR (Symbolic.X9, Symbolic.X21, 0);
          Symbolic.CBZ (Symbolic.X9, "__dark_terminate_invalid");
          Symbolic.CMP_imm (Symbolic.X9, 2);
          Symbolic.B_cond_label (Symbolic.EQ, "__dark_terminate_collect");
          Symbolic.LDR (Symbolic.X0, Symbolic.X21, 8);
          Symbolic.MOVZ (Symbolic.X1, 15, 0) ]
        @ syscall 129
        @ [ Symbolic.LDR (Symbolic.X0, Symbolic.X21, 8);
            Symbolic.ADD_imm (Symbolic.X1, Symbolic.SP, 32);
            zero Symbolic.X2;
            zero Symbolic.X3;

            zero Symbolic.X9;
            Symbolic.STR (Symbolic.X9, Symbolic.SP, 32) ]
        @ syscall 260
        @ [ Symbolic.LDR (Symbolic.X10, Symbolic.SP, 32);
            Symbolic.AND_imm (Symbolic.X11, Symbolic.X10, 0x7fL);
            Symbolic.CBNZ (Symbolic.X11, "__dark_terminate_signaled");
            Symbolic.LSR_imm (Symbolic.X9, Symbolic.X10, 8);
            Symbolic.B_label "__dark_terminate_store";
            Symbolic.Label "__dark_terminate_signaled";
            Symbolic.ADD_imm (Symbolic.X9, Symbolic.X11, 128);
            Symbolic.Label "__dark_terminate_store";
            Symbolic.STR (Symbolic.X9, Symbolic.X21, 40);
            Symbolic.MOVZ (Symbolic.X9, 2, 0);
            Symbolic.STR (Symbolic.X9, Symbolic.X21, 0);
            Symbolic.Label "__dark_terminate_collect";
            Symbolic.LSR_imm (Symbolic.X0, Symbolic.X9, 6);
            zero Symbolic.X1 ]
        @ [

            Symbolic.SUB_reg (Symbolic.X0, Symbolic.X21, Symbolic.X25);
            Symbolic.LSR_imm (Symbolic.X0, Symbolic.X0, 6) ]
        @ loadStringLiteralPointer Symbolic.X1 ""
        @ [ Symbolic.MOVZ (Symbolic.X2, 1, 0);
            Symbolic.BL "__dark_cli_process_io";
            Symbolic.MOV_reg (Symbolic.X19, Symbolic.X0);
            Symbolic.LDR (Symbolic.X0, Symbolic.X21, 16) ]
        @ syscall 57
        @ [ Symbolic.LDR (Symbolic.X0, Symbolic.X21, 24) ]
        @ syscall 57
        @ [ Symbolic.LDR (Symbolic.X0, Symbolic.X21, 32) ]
        @ syscall 57
        @ [ zero Symbolic.X9;
            Symbolic.STR (Symbolic.X9, Symbolic.X21, 0);
            Symbolic.MOV_reg (Symbolic.X0, Symbolic.X19);
            Symbolic.B_label "__dark_terminate_return";
            Symbolic.Label "__dark_terminate_invalid" ]
        @ invalidOutcome "__dark_terminate_return"
        @ [ Symbolic.Label "__dark_terminate_return" ]
        @ epilogue
    in
    let cleanup =
        [ Symbolic.Label "__dark_cli_cleanup_processes";
          Symbolic.STP_pre (Symbolic.X19, Symbolic.X30, Symbolic.SP, -16);
          Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 16);
          Symbolic.CBZ (Symbolic.X25, "__dark_cleanup_process_done");
          Symbolic.MOVZ (Symbolic.X19, 1, 0);
          Symbolic.Label "__dark_cleanup_process_next";
          Symbolic.CMP_imm (Symbolic.X19, 63);
          Symbolic.B_cond_label (Symbolic.GT, "__dark_cleanup_process_done");
          Symbolic.LSL_imm (Symbolic.X9, Symbolic.X19, 6);
          Symbolic.ADD_reg (Symbolic.X9, Symbolic.X25, Symbolic.X9);
          Symbolic.LDR (Symbolic.X10, Symbolic.X9, 0);
          Symbolic.CBZ (Symbolic.X10, "__dark_cleanup_process_advance");
          Symbolic.LDR (Symbolic.X0, Symbolic.X9, 8);
          Symbolic.MOVZ (Symbolic.X1, 9, 0) ]
        @ syscall 129
        @ [ Symbolic.LSL_imm (Symbolic.X9, Symbolic.X19, 6);
            Symbolic.ADD_reg (Symbolic.X9, Symbolic.X25, Symbolic.X9);
            Symbolic.LDR (Symbolic.X0, Symbolic.X9, 8);
            Symbolic.MOV_reg (Symbolic.X1, Symbolic.SP);
            zero Symbolic.X2;
            zero Symbolic.X3 ]
        @ syscall 260
        @ [ Symbolic.LSL_imm (Symbolic.X9, Symbolic.X19, 6);
            Symbolic.ADD_reg (Symbolic.X9, Symbolic.X25, Symbolic.X9);
            Symbolic.LDR (Symbolic.X0, Symbolic.X9, 16) ]
        @ syscall 57
        @ [ Symbolic.LDR (Symbolic.X0, Symbolic.X9, 24) ]
        @ syscall 57
        @ [ Symbolic.LDR (Symbolic.X0, Symbolic.X9, 32) ]
        @ syscall 57
        @ [ zero Symbolic.X10;
            Symbolic.STR (Symbolic.X10, Symbolic.X9, 0);
            Symbolic.Label "__dark_cleanup_process_advance";
            Symbolic.ADD_imm (Symbolic.X19, Symbolic.X19, 1);
            Symbolic.B_label "__dark_cleanup_process_next";
            Symbolic.Label "__dark_cleanup_process_done";
            Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 16);
            Symbolic.LDP_post (Symbolic.X19, Symbolic.X30, Symbolic.SP, 16);
            Symbolic.RET ]
    in
    communicate @ terminate @ cleanup
