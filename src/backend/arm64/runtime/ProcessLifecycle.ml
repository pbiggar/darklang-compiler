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
let generateHeapInit (target : ARM64.targetConfig) =
  let freeListSize = 256 in
  let os = ARM64.targetOS target in
  let syscalls = ARM64.targetSyscalls target in
  let mmapFlags =
    match os with Platform.MacOS -> 0x1002 | Platform.Linux -> 0x22
  in
  let heapSizeForMmap = loadImmediate Symbolic.X1 heapMmapSizeBytes in
  [ Symbolic.MOVZ (Symbolic.X0, 0, 0) ]
  @ heapSizeForMmap
  @ [
      Symbolic.MOVZ (Symbolic.X2, 3, 0);
      Symbolic.MOVZ (Symbolic.X3, mmapFlags, 0);
      Symbolic.MOVZ (Symbolic.X4, 0, 0);
      Symbolic.MVN (Symbolic.X4, Symbolic.X4);
      Symbolic.MOVZ (Symbolic.X5, 0, 0);
      Symbolic.MOVZ
        (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.mmap, 0);
      Symbolic.SVC syscalls.ARM64.svcImmediate;
      Symbolic.MOVZ (Symbolic.X15, 0, 0);
      Symbolic.MVN (Symbolic.X15, Symbolic.X15);
      Symbolic.CMP_reg (Symbolic.X0, Symbolic.X15);
      Symbolic.B_cond (Symbolic.NE, 3);
      Symbolic.MOVZ (Symbolic.X0, 1, 0);
      Symbolic.MOVZ
        (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.exit, 0);
      Symbolic.SVC syscalls.ARM64.svcImmediate;
      Symbolic.MOV_reg (Symbolic.X27, Symbolic.X0);
      Symbolic.ADD_imm (Symbolic.X28, Symbolic.X27, freeListSize + 512);
      Symbolic.MOVZ (Symbolic.X9, 8192 + 4096, 0);
      Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X9);
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
let generateCliArgvHelper (_ctx : ARM64CodeGenTypes.codeGenContext)
    (label : string) =
  let missingLabel = label ^ "_missing" in
  let lengthLabel = label ^ "_length" in
  let lengthDoneLabel = label ^ "_length_done" in
  let copyLabel = label ^ "_copy" in
  let copyDoneLabel = label ^ "_copy_done" in
  [
    Symbolic.Label label;
    Symbolic.CMP_imm (Symbolic.X0, 0);
    Symbolic.B_cond_label (Symbolic.LT, missingLabel);
    Symbolic.MOV_reg (Symbolic.X1, Symbolic.X29);
    Symbolic.Label (label ^ "_find_root");
    Symbolic.LDR (Symbolic.X2, Symbolic.X1, 0);
    Symbolic.CBZ (Symbolic.X2, label ^ "_root_found");
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
    Symbolic.RET;
  ]

(*
   Start a shell-language command and retain its pid and descriptors in the
   fixed process table rooted at X25.
   Recover inherited envp from the root frame.
   Keep raw accumulated output outside the managed object graph. Each
   communicate call copies only its newly-read suffix into immutable
   managed strings; terminate copies the complete buffers.
*)
(* One private launch unit. Only native instructions run in the forked child;
   argv/env policy, capture/draining, and deadlines belong to shared Dark. *)
let generateChildLaunchHelper target =
  let module S = Symbolic in
  let mac = ARM64.targetOS target = Platform.MacOS in
  let abi = ARM64.targetSyscalls target in
  let serial = ref 0 in
  let trap linux darwin =
    incr serial;
    let doneLabel = Printf.sprintf "__child_trap_%d" !serial in
    loadImmediate abi.ARM64.syscallRegister
      (Int64.of_int (if mac then darwin else linux))
    @ [ S.SVC abi.ARM64.svcImmediate ]
    @
    if not mac then []
    else
      [
        S.B_cond_label (S.LO, doneLabel); S.NEG (S.X0, S.X0); S.Label doneLabel;
      ]
  in
  let zero r = S.MOVZ (r, 0, 0) in
  let fd offset high =
    [ S.LDR (S.X0, S.SP, offset) ]
    @
    if high then [ S.LSR_imm (S.X0, S.X0, 32) ]
    else loadImmediate S.X9 4294967295L @ [ S.AND_reg (S.X0, S.X0, S.X9) ]
  in
  let close offset high = fd offset high @ trap 57 6 in
  let error =
    [ S.CMP_imm (S.X0, 0); S.B_cond_label (S.LT, "__child_launch_error") ]
  in
  let lift offset high =
    let doneLabel = Printf.sprintf "__child_lift_%d_%b" offset high in
    fd offset high
    @ [
        S.CMP_imm (S.X0, 3);
        S.B_cond_label (S.GE, doneLabel);
        S.STR (S.X0, S.SP, 88);
      ]
    @ loadImmediate S.X1 (if mac then 67L else 1030L)
    @ loadImmediate S.X2 3L @ trap 25 92 @ error
    @ [ S.STR (S.X0, S.SP, 80); S.LDR (S.X0, S.SP, 88) ]
    @ trap 57 6
    @ [ S.LDR (S.X10, S.SP, 80); S.LDR (S.X11, S.SP, offset) ]
    @ (if high then
         loadImmediate S.X9 4294967295L
         @ [ S.AND_reg (S.X11, S.X11, S.X9); S.LSL_imm (S.X10, S.X10, 32) ]
       else [ S.LSR_imm (S.X11, S.X11, 32); S.LSL_imm (S.X11, S.X11, 32) ])
    @ [
        S.ORR_reg (S.X11, S.X11, S.X10);
        S.STR (S.X11, S.SP, offset);
        S.Label doneLabel;
      ]
  in
  let pipe offset =
    (if not mac then
       [ S.ADD_imm (S.X0, S.SP, offset) ]
       @ loadImmediate S.X1 524288L @ trap 59 42
     else trap 59 42)
    @ error
    @ (if not mac then []
       else
         [
           S.LSL_imm (S.X1, S.X1, 32);
           S.ORR_reg (S.X9, S.X0, S.X1);
           S.STR (S.X9, S.SP, offset);
         ]
         @ fd offset false @ loadImmediate S.X1 2L @ loadImmediate S.X2 1L
         @ trap 25 92 @ error @ fd offset true @ loadImmediate S.X1 2L
         @ loadImmediate S.X2 1L @ trap 25 92 @ error)
    @ lift offset false @ lift offset true
  in
  let nonblocking offset high =
    fd offset high @ loadImmediate S.X1 3L
    @ [ zero S.X2 ]
    @ trap 25 92
    @ [
        S.CMP_imm (S.X0, 0);
        S.B_cond_label (S.LT, "__child_launch_kill_error");
        S.MOV_reg (S.X2, S.X0);
      ]
    @ loadImmediate S.X9 (if mac then 4L else 2048L)
    @ [ S.ORR_reg (S.X2, S.X2, S.X9) ]
    @ fd offset high @ loadImmediate S.X1 4L @ trap 25 92
    @ [
        S.CMP_imm (S.X0, 0); S.B_cond_label (S.LT, "__child_launch_kill_error");
      ]
  in
  let duplicate offset high target =
    fd offset high @ loadImmediate S.X1 target
    @ [ zero S.X2 ]
    @ trap 24 90
    @ [
        S.CMP_imm (S.X0, 0); S.B_cond_label (S.LT, "__child_launch_child_error");
      ]
  in
  let preserved =
    [
      (S.X0, S.X1);
      (S.X2, S.X3);
      (S.X4, S.X5);
      (S.X6, S.X7);
      (S.X8, S.X9);
      (S.X10, S.X11);
      (S.X12, S.X13);
      (S.X14, S.X15);
      (S.X16, S.X17);
      (S.X29, S.X30);
    ]
  in
  [ S.Label "__dark_child_launch" ]
  @ List.map (fun (a, b) -> S.STP_pre (a, b, S.SP, -16)) preserved
  @ [
      S.SUB_imm (S.SP, S.SP, 96);
      S.STR (S.X0, S.SP, 32);
      S.STR (S.X1, S.SP, 40);
      S.STR (S.X2, S.SP, 48);
      S.STR (S.X3, S.SP, 56);
    ]
  @ loadImmediate S.X9 (-1L)
  @ List.map (fun offset -> S.STR (S.X9, S.SP, offset)) [ 0; 8; 16; 24 ]
  @ [ S.CBNZ (S.X2, "__child_launch_pipes_ready") ]
  @ pipe 0 @ pipe 8 @ pipe 16
  @ [ S.Label "__child_launch_pipes_ready" ]
  @ pipe 24
  @ (if mac then trap 220 2
     else
       [ S.MOVZ (S.X0, 17, 0); zero S.X1; zero S.X2; zero S.X3; zero S.X4 ]
       @ trap 220 2)
  @ error
  @ (if mac then [ S.CBNZ (S.X1, "__child_launch_child") ]
     else [ S.CBZ (S.X0, "__child_launch_child") ])
  @ [ S.STR (S.X0, S.SP, 64) ]
  @ close 0 false @ close 8 true @ close 16 true @ close 24 true
  @ [ S.Label "__child_launch_handshake" ]
  @ fd 24 false
  @ [ S.ADD_imm (S.X1, S.SP, 72) ]
  @ loadImmediate S.X2 8L @ trap 63 3 @ loadImmediate S.X9 (-4L)
  @ [
      S.CMP_reg (S.X0, S.X9);
      S.B_cond_label (S.EQ, "__child_launch_handshake");
      S.CMP_imm (S.X0, 0);
      S.B_cond_label (S.LT, "__child_launch_kill_error");
      S.B_cond_label (S.GT, "__child_launch_exec_error");
    ]
  @ close 24 false
  @ [ S.LDR (S.X9, S.SP, 48); S.CBNZ (S.X9, "__child_launch_inherited") ]
  @ nonblocking 0 true @ nonblocking 8 false @ nonblocking 16 false
  @ [ S.LDR (S.X12, S.SP, 56) ]
  @ fd 0 true
  @ [ S.STR (S.X0, S.X12, 0) ]
  @ fd 8 false
  @ [ S.STR (S.X0, S.X12, 8) ]
  @ fd 16 false
  @ [
      S.STR (S.X0, S.X12, 16);
      S.Label "__child_launch_inherited";
      S.LDR (S.X0, S.SP, 64);
      S.B_label "__child_launch_return";
      S.Label "__child_launch_exec_error";
      S.LDR (S.X0, S.SP, 72);
      S.B_label "__child_launch_reap_error";
      S.Label "__child_launch_kill_error";
      S.STR (S.X0, S.SP, 72);
      S.LDR (S.X0, S.SP, 64);
    ]
  @ loadImmediate S.X1 9L @ trap 129 37
  @ [
      S.LDR (S.X0, S.SP, 72);
      S.Label "__child_launch_reap_error";
      S.STR (S.X0, S.SP, 72);
      S.Label "__child_launch_reap_retry";
      S.LDR (S.X0, S.SP, 64);
      S.ADD_imm (S.X1, S.SP, 80);
      zero S.X2;
      zero S.X3;
    ]
  @ trap 260 7 @ loadImmediate S.X9 (-4L)
  @ [
      S.CMP_reg (S.X0, S.X9);
      S.B_cond_label (S.EQ, "__child_launch_reap_retry");
      S.LDR (S.X0, S.SP, 72);
      S.Label "__child_launch_error";
      S.STR (S.X0, S.SP, 72);
    ]
  @ List.concat_map
      (fun offset -> close offset false @ close offset true)
      [ 0; 8; 16; 24 ]
  @ [
      S.LDR (S.X0, S.SP, 72);
      S.Label "__child_launch_return";
      S.STR (S.X0, S.SP, 240);
      S.ADD_imm (S.SP, S.SP, 96);
    ]
  @ List.map (fun (a, b) -> S.LDP_post (a, b, S.SP, 16)) (List.rev preserved)
  @ [
      S.RET;
      S.Label "__child_launch_child";
      S.LDR (S.X9, S.SP, 48);
      S.CBNZ (S.X9, "__child_launch_exec");
    ]
  @ duplicate 0 false 0L @ duplicate 8 true 1L @ duplicate 16 true 2L
  @ [ S.Label "__child_launch_exec" ]
  @ close 0 false @ close 0 true @ close 8 false @ close 8 true @ close 16 false
  @ close 16 true @ close 24 false
  @ [ S.LDR (S.X1, S.SP, 32); S.LDR (S.X0, S.X1, 0); S.LDR (S.X2, S.SP, 40) ]
  @ trap 221 59
  @ [ S.Label "__child_launch_child_error"; S.STR (S.X0, S.SP, 72) ]
  @ fd 24 true
  @ [ S.ADD_imm (S.X1, S.SP, 72) ]
  @ loadImmediate S.X2 8L @ trap 64 4 @ loadImmediate S.X0 127L @ trap 93 1

let generateChildCleanupHelper (ctx : codeGenContext) =
  let module S = Symbolic in
  let abi = ARM64.targetSyscalls ctx.target in
  let mac = ARM64.targetOS ctx.target = Platform.MacOS in
  let serial = ref 0 in
  let trap linux darwin =
    incr serial;
    let doneLabel = Printf.sprintf "__child_cleanup_trap_%d" !serial in
    loadImmediate abi.ARM64.syscallRegister
      (Int64.of_int (if mac then darwin else linux))
    @ [ S.SVC abi.ARM64.svcImmediate ]
    @
    if not mac then []
    else
      [
        S.B_cond_label (S.LO, doneLabel); S.NEG (S.X0, S.X0); S.Label doneLabel;
      ]
  in
  let close offset =
    let doneLabel = Printf.sprintf "__child_cleanup_closed_%d" offset in
    [
      S.LDR (S.X0, S.X20, offset);
      S.CMP_imm (S.X0, 0);
      S.B_cond_label (S.LT, doneLabel);
    ]
    @ loadImmediate S.X9 (-1L)
    @ [ S.STR (S.X9, S.X20, offset) ]
    @ trap 57 6 @ [ S.Label doneLabel ]
  in
  let free offset =
    let doneLabel = Printf.sprintf "__child_cleanup_freed_%d" offset in
    [
      S.LDR (S.X0, S.X20, offset);
      S.CBZ (S.X0, doneLabel);
      S.MOVZ (S.X9, 0, 0);
      S.STR (S.X9, S.X20, offset);
      S.SUB_imm (S.X0, S.X0, 8);
      S.LDR (S.X1, S.X0, 0);
    ]
    @ trap 215 73 @ generateLeakCounterDec ctx @ [ S.Label doneLabel ]
  in
  let preserve =
    [
      (S.X0, S.X1);
      (S.X2, S.X3);
      (S.X4, S.X5);
      (S.X6, S.X7);
      (S.X8, S.X9);
      (S.X10, S.X11);
      (S.X12, S.X13);
      (S.X14, S.X15);
      (S.X16, S.X17);
      (S.X19, S.X20);
      (S.X29, S.X30);
    ]
  in
  [ S.Label "__dark_cli_cleanup_processes" ]
  @ List.map (fun (a, b) -> S.STP_pre (a, b, S.SP, -16)) preserve
  @ [
      S.SUB_imm (S.SP, S.SP, 16);
      S.MOVZ (S.X19, 1, 0);
      S.Label "__child_cleanup_next";
      S.CMP_imm (S.X19, 63);
      S.B_cond_label (S.GT, "__child_cleanup_done");
    ]
  @ loadImmediate S.X20 8960L
  @ [
      S.ADD_reg (S.X20, S.X27, S.X20);
      S.LSL_imm (S.X9, S.X19, 6);
      S.ADD_reg (S.X20, S.X20, S.X9);
      S.LDR (S.X9, S.X20, 0);
      S.CBZ (S.X9, "__child_cleanup_advance");
      S.LDR (S.X9, S.X20, 40);
      S.CMP_imm (S.X9, 0);
      S.B_cond_label (S.GE, "__child_cleanup_pipes");
      S.LDR (S.X0, S.X20, 8);
      S.MOVZ (S.X1, 9, 0);
    ]
  @ trap 129 37
  @ [
      S.Label "__child_cleanup_reap";
      S.LDR (S.X0, S.X20, 8);
      S.MOV_reg (S.X1, S.SP);
      S.MOVZ (S.X2, 0, 0);
      S.MOVZ (S.X3, 0, 0);
    ]
  @ trap 260 7 @ loadImmediate S.X9 (-4L)
  @ [
      S.CMP_reg (S.X0, S.X9);
      S.B_cond_label (S.EQ, "__child_cleanup_reap");
      S.Label "__child_cleanup_pipes";
    ]
  @ close 16 @ close 24 @ close 32 @ free 48 @ free 56
  @ [
      S.MOVZ (S.X9, 0, 0);
      S.STR (S.X9, S.X20, 0);
      S.Label "__child_cleanup_advance";
      S.ADD_imm (S.X19, S.X19, 1);
      S.B_label "__child_cleanup_next";
      S.Label "__child_cleanup_done";
      S.ADD_imm (S.SP, S.SP, 16);
    ]
  @ List.map (fun (a, b) -> S.LDP_post (a, b, S.SP, 16)) (List.rev preserve)
  @ [ S.RET ]
