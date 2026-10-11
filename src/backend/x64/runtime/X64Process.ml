open X64Operands
(*  X64Process.ml - Generate x64 environment and process runtime helpers. *)

(*  Return argv[index + 1] as a nullable String pointer. RDI carries the zero-based *)
(*  positional index. Native argv entries are zero-terminated, so present *)
(*  values are copied into managed Dark strings. Following the frame-pointer *)
(*  chain reaches _start's root frame without reserving process-state registers. *)
let generateCliArgvHelper () =
  let label = "__dark_cli_argv" in
  let missingLabel = label ^ "_missing" in
  let rootLabel = label ^ "_find_root" in
  let rootFoundLabel = label ^ "_root_found" in
  let lengthLabel = label ^ "_length" in
  let lengthDoneLabel = label ^ "_length_done" in
  let copyLabel = label ^ "_copy" in
  let copyDoneLabel = label ^ "_copy_done" in
  let stringHeapOkLabel = label ^ "_string_heap_ok" in
  let savedRegs =
    [
      X86_64.RDI;
      X86_64.RSI;
      X86_64.RDX;
      X86_64.RCX;
      X86_64.R8;
      X86_64.R9;
      X86_64.R10;
      X86_64.R11;
    ]
  in
  let saves = savedRegs |> List.map (fun r -> X86_64.PUSH r) in
  let restores = savedRegs |> List.rev |> List.map (fun r -> X86_64.POP r) in
  let checkHeapBounds okLabel =
    [
      X86_64.MOV_reg (X86_64.R11, heapPtr);
      X86_64.SUB_reg (X86_64.R11, freeListBase);
      X86_64.CMP_imm (X86_64.R11, Int64.to_int32 heapMmapSizeBytes);
      X86_64.Jcc (X86_64.LE, okLabel);
      X86_64.JMP oomHandlerLabel;
      X86_64.Label okLabel;
    ]
  in
  [ X86_64.Label label ] @ saves
  @ [
      X86_64.CMP_imm (X86_64.RDI, 0l);
      X86_64.Jcc (X86_64.LT, missingLabel);
      X86_64.MOV_reg (X86_64.RAX, X86_64.RBP);
      X86_64.Label rootLabel;
      X86_64.MOV_load (X86_64.RDX, X86_64.RAX, 0l);
      X86_64.CMP_imm (X86_64.RDX, 0l);
      X86_64.Jcc (X86_64.EQ, rootFoundLabel);
      X86_64.MOV_reg (X86_64.RAX, X86_64.RDX);
      X86_64.JMP rootLabel;
      X86_64.Label rootFoundLabel;
      (*  _start's saved RBP is at +0, argc at +8, argv[0] at +16, and the *)
      (*  first positional argument at +24. *)
      X86_64.MOV_load (X86_64.RDX, X86_64.RAX, 8l);
      X86_64.SUB_imm (X86_64.RDX, 1l);
      X86_64.CMP_reg (X86_64.RDI, X86_64.RDX);
      X86_64.Jcc (X86_64.GE, missingLabel);
      X86_64.MOV_reg (X86_64.RDX, X86_64.RDI);
      X86_64.SHL_imm (X86_64.RDX, 3);
      X86_64.ADD_reg (X86_64.RAX, X86_64.RDX);
      X86_64.MOV_load (X86_64.R8, X86_64.RAX, 24l);
      (*  Measure the native zero-terminated string. *)
      X86_64.XOR_reg (X86_64.RCX, X86_64.RCX);
      X86_64.MOV_reg (X86_64.R9, X86_64.R8);
      X86_64.Label lengthLabel;
      X86_64.MOV_load_byte (X86_64.RDX, X86_64.R9, 0l);
      X86_64.TEST_reg (X86_64.RDX, X86_64.RDX);
      X86_64.Jcc (X86_64.EQ, lengthDoneLabel);
      X86_64.ADD_imm (X86_64.RCX, 1l);
      X86_64.ADD_imm (X86_64.R9, 1l);
      X86_64.JMP lengthLabel;
      X86_64.Label lengthDoneLabel;
      (*  Allocate [refcount][length][bytes][padding]. R10 retains the value *)
      (*  pointer while R11 holds the aligned byte count. *)
      X86_64.MOV_reg (X86_64.R10, heapPtr);
      X86_64.MOV_reg (X86_64.R11, X86_64.RCX);
      X86_64.ADD_imm (X86_64.R11, 7l);
      X86_64.AND_imm (X86_64.R11, -8l);
      X86_64.ADD_imm (X86_64.R11, 16l);
      X86_64.ADD_reg (heapPtr, X86_64.R11);
    ]
  @ checkHeapBounds stringHeapOkLabel
  @ loadImm64 X86_64.RDX 0x7FFFFFFFFFFFFFFFL
  @ [
      X86_64.MOV_store (X86_64.R10, 0l, X86_64.RDX);
      X86_64.MOV_store (X86_64.R10, 8l, X86_64.RCX);
      X86_64.LEA (X86_64.R9, X86_64.R10, 16l);
      X86_64.MOV_reg (X86_64.RAX, X86_64.RCX);
      X86_64.Label copyLabel;
      X86_64.CMP_imm (X86_64.RAX, 0l);
      X86_64.Jcc (X86_64.EQ, copyDoneLabel);
      X86_64.MOV_load_byte (X86_64.RDX, X86_64.R8, 0l);
      X86_64.MOV_store_byte (X86_64.R9, 0l, X86_64.RDX);
      X86_64.ADD_imm (X86_64.R8, 1l);
      X86_64.ADD_imm (X86_64.R9, 1l);
      X86_64.SUB_imm (X86_64.RAX, 1l);
      X86_64.JMP copyLabel;
      X86_64.Label copyDoneLabel;
      X86_64.MOV_reg (X86_64.RAX, X86_64.R10);
      X86_64.JMP (label ^ "_done");
      X86_64.Label missingLabel;
      X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
      X86_64.Label (label ^ "_done");
    ]
  @ restores @ [ X86_64.RET ]

(*  Return the original process environment as NUL-delimited UTF-8 entries. *)
let generateCliEnvironmentPackedHelper (enableLeakCheck : bool) =
  let label = "__dark_cli_environment_packed" in
  let findRoot = label ^ "_find_root" in
  let rootFound = label ^ "_root_found" in
  let findArgvEnd = label ^ "_find_argv_end" in
  let countEntry = label ^ "_count_entry" in
  let countByte = label ^ "_count_byte" in
  let countNext = label ^ "_count_next" in
  let countDone = label ^ "_count_done" in
  let copyEntry = label ^ "_copy_entry" in
  let copyByte = label ^ "_copy_byte" in
  let copyNext = label ^ "_copy_next" in
  let copyDone = label ^ "_copy_done" in
  let leakInc =
    if enableLeakCheck then
      [
        X86_64.LEA_rip (X86_64.R11, "_leak_count");
        X86_64.MOV_load (X86_64.RDX, X86_64.R11, 0l);
        X86_64.ADD_imm (X86_64.RDX, 1l);
        X86_64.MOV_store (X86_64.R11, 0l, X86_64.RDX);
      ]
    else []
  in
  [
    X86_64.Label label;
    X86_64.MOV_reg (X86_64.RAX, X86_64.RBP);
    X86_64.Label findRoot;
    X86_64.MOV_load (X86_64.RDX, X86_64.RAX, 0l);
    X86_64.CMP_imm (X86_64.RDX, 0l);
    X86_64.Jcc (X86_64.EQ, rootFound);
    X86_64.MOV_reg (X86_64.RAX, X86_64.RDX);
    X86_64.JMP findRoot;
    X86_64.Label rootFound;
    X86_64.ADD_imm (X86_64.RAX, 16l);
    X86_64.Label findArgvEnd;
    X86_64.MOV_load (X86_64.RDX, X86_64.RAX, 0l);
    X86_64.ADD_imm (X86_64.RAX, 8l);
    X86_64.CMP_imm (X86_64.RDX, 0l);
    X86_64.Jcc (X86_64.NE, findArgvEnd);
    X86_64.MOV_reg (X86_64.R8, X86_64.RAX);
    X86_64.XOR_reg (X86_64.RCX, X86_64.RCX);
    X86_64.Label countEntry;
    X86_64.MOV_load (X86_64.RDX, X86_64.RAX, 0l);
    X86_64.CMP_imm (X86_64.RDX, 0l);
    X86_64.Jcc (X86_64.EQ, countDone);
    X86_64.Label countByte;
    X86_64.MOV_load_byte (X86_64.R9, X86_64.RDX, 0l);
    X86_64.ADD_imm (X86_64.RCX, 1l);
    X86_64.ADD_imm (X86_64.RDX, 1l);
    X86_64.CMP_imm (X86_64.R9, 0l);
    X86_64.Jcc (X86_64.EQ, countNext);
    X86_64.JMP countByte;
    X86_64.Label countNext;
    X86_64.ADD_imm (X86_64.RAX, 8l);
    X86_64.JMP countEntry;
    X86_64.Label countDone;
    X86_64.MOV_reg (X86_64.R10, heapPtr);
    X86_64.MOV_reg (X86_64.R11, X86_64.RCX);
    X86_64.ADD_imm (X86_64.R11, 7l);
    X86_64.AND_imm (X86_64.R11, -8l);
    X86_64.ADD_imm (X86_64.R11, 16l);
    X86_64.ADD_reg (heapPtr, X86_64.R11);
    X86_64.MOV_imm32 (X86_64.RDX, 1l);
    X86_64.MOV_store (X86_64.R10, 0l, X86_64.RDX);
    X86_64.MOV_store (X86_64.R10, 8l, X86_64.RCX);
    X86_64.LEA (X86_64.R9, X86_64.R10, 16l);
    X86_64.MOV_reg (X86_64.RAX, X86_64.R8);
    X86_64.Label copyEntry;
    X86_64.MOV_load (X86_64.RDX, X86_64.RAX, 0l);
    X86_64.CMP_imm (X86_64.RDX, 0l);
    X86_64.Jcc (X86_64.EQ, copyDone);
    X86_64.Label copyByte;
    X86_64.MOV_load_byte (X86_64.R8, X86_64.RDX, 0l);
    X86_64.MOV_store_byte (X86_64.R9, 0l, X86_64.R8);
    X86_64.ADD_imm (X86_64.RDX, 1l);
    X86_64.ADD_imm (X86_64.R9, 1l);
    X86_64.CMP_imm (X86_64.R8, 0l);
    X86_64.Jcc (X86_64.EQ, copyNext);
    X86_64.JMP copyByte;
    X86_64.Label copyNext;
    X86_64.ADD_imm (X86_64.RAX, 8l);
    X86_64.JMP copyEntry;
    X86_64.Label copyDone;
    X86_64.MOV_reg (X86_64.RAX, X86_64.R10);
  ]
  @ leakInc @ [ X86_64.RET ]

(*  Return getcwd(2) as a managed UTF-8 string. *)
let generateCliGetEnvHelper (enableLeakCheck : bool) =
  let label = "__dark_cli_getenv" in
  let rootLabel = label ^ "_find_root" in
  let rootFoundLabel = label ^ "_root_found" in
  let argvEndLabel = label ^ "_find_argv_end" in
  let nextEntryLabel = label ^ "_next_entry" in
  let compareLabel = label ^ "_compare" in
  let nameMatchedLabel = label ^ "_name_matched" in
  let lengthLabel = label ^ "_length" in
  let lengthDoneLabel = label ^ "_length_done" in
  let copyLabel = label ^ "_copy" in
  let copyDoneLabel = label ^ "_copy_done" in
  let stringHeapOkLabel = label ^ "_string_heap_ok" in
  let missingLabel = label ^ "_missing" in
  let leakInc =
    if enableLeakCheck then
      [
        X86_64.LEA_rip (X86_64.R11, "_leak_count");
        X86_64.MOV_load (X86_64.RDX, X86_64.R11, 0l);
        X86_64.ADD_imm (X86_64.RDX, 1l);
        X86_64.MOV_store (X86_64.R11, 0l, X86_64.RDX);
      ]
    else []
  in
  let checkHeapBounds okLabel =
    [
      X86_64.MOV_reg (X86_64.R11, heapPtr);
      X86_64.SUB_reg (X86_64.R11, freeListBase);
      X86_64.CMP_imm (X86_64.R11, Int64.to_int32 heapMmapSizeBytes);
      X86_64.Jcc (X86_64.LE, okLabel);
      X86_64.JMP oomHandlerLabel;
      X86_64.Label okLabel;
    ]
  in
  [
    X86_64.Label label;
    X86_64.MOV_reg (X86_64.RAX, X86_64.RBP);
    X86_64.Label rootLabel;
    X86_64.MOV_load (X86_64.RDX, X86_64.RAX, 0l);
    X86_64.CMP_imm (X86_64.RDX, 0l);
    X86_64.Jcc (X86_64.EQ, rootFoundLabel);
    X86_64.MOV_reg (X86_64.RAX, X86_64.RDX);
    X86_64.JMP rootLabel;
    X86_64.Label rootFoundLabel;
    X86_64.ADD_imm (X86_64.RAX, 16l);
    X86_64.Label argvEndLabel;
    X86_64.MOV_load (X86_64.RDX, X86_64.RAX, 0l);
    X86_64.ADD_imm (X86_64.RAX, 8l);
    X86_64.CMP_imm (X86_64.RDX, 0l);
    X86_64.Jcc (X86_64.NE, argvEndLabel);
    X86_64.MOV_load (X86_64.RCX, X86_64.RDI, 8l);
    X86_64.Label nextEntryLabel;
    X86_64.MOV_load (X86_64.R8, X86_64.RAX, 0l);
    X86_64.ADD_imm (X86_64.RAX, 8l);
    X86_64.CMP_imm (X86_64.R8, 0l);
    X86_64.Jcc (X86_64.EQ, missingLabel);
    X86_64.XOR_reg (X86_64.R9, X86_64.R9);
    X86_64.Label compareLabel;
    X86_64.CMP_reg (X86_64.R9, X86_64.RCX);
    X86_64.Jcc (X86_64.GE, nameMatchedLabel);
    X86_64.MOV_reg (X86_64.R10, X86_64.RDI);
    X86_64.ADD_imm (X86_64.R10, 16l);
    X86_64.ADD_reg (X86_64.R10, X86_64.R9);
    X86_64.MOV_load_byte (X86_64.RDX, X86_64.R10, 0l);
    X86_64.MOV_reg (X86_64.R10, X86_64.R8);
    X86_64.ADD_reg (X86_64.R10, X86_64.R9);
    X86_64.MOV_load_byte (X86_64.R11, X86_64.R10, 0l);
    X86_64.CMP_reg (X86_64.RDX, X86_64.R11);
    X86_64.Jcc (X86_64.NE, nextEntryLabel);
    X86_64.ADD_imm (X86_64.R9, 1l);
    X86_64.JMP compareLabel;
    X86_64.Label nameMatchedLabel;
    X86_64.MOV_reg (X86_64.R10, X86_64.R8);
    X86_64.ADD_reg (X86_64.R10, X86_64.RCX);
    X86_64.MOV_load_byte (X86_64.RDX, X86_64.R10, 0l);
    X86_64.CMP_imm (X86_64.RDX, 61l);
    X86_64.Jcc (X86_64.NE, nextEntryLabel);
    X86_64.ADD_imm (X86_64.R10, 1l);
    X86_64.MOV_reg (X86_64.R8, X86_64.R10);
    X86_64.XOR_reg (X86_64.RCX, X86_64.RCX);
    X86_64.Label lengthLabel;
    X86_64.MOV_load_byte (X86_64.RDX, X86_64.R10, 0l);
    X86_64.CMP_imm (X86_64.RDX, 0l);
    X86_64.Jcc (X86_64.EQ, lengthDoneLabel);
    X86_64.ADD_imm (X86_64.RCX, 1l);
    X86_64.ADD_imm (X86_64.R10, 1l);
    X86_64.JMP lengthLabel;
    X86_64.Label lengthDoneLabel;
    X86_64.MOV_reg (X86_64.R10, heapPtr);
    X86_64.MOV_reg (X86_64.R11, X86_64.RCX);
    X86_64.ADD_imm (X86_64.R11, 7l);
    X86_64.AND_imm (X86_64.R11, -8l);
    X86_64.ADD_imm (X86_64.R11, 16l);
    X86_64.ADD_reg (heapPtr, X86_64.R11);
  ]
  @ checkHeapBounds stringHeapOkLabel
  @ [
      X86_64.MOV_imm32 (X86_64.RDX, 1l);
      X86_64.MOV_store (X86_64.R10, 0l, X86_64.RDX);
      X86_64.MOV_store (X86_64.R10, 8l, X86_64.RCX);
      X86_64.LEA (X86_64.R9, X86_64.R10, 16l);
      X86_64.MOV_reg (X86_64.RAX, X86_64.RCX);
      X86_64.Label copyLabel;
      X86_64.CMP_imm (X86_64.RAX, 0l);
      X86_64.Jcc (X86_64.EQ, copyDoneLabel);
      X86_64.MOV_load_byte (X86_64.RDX, X86_64.R8, 0l);
      X86_64.MOV_store_byte (X86_64.R9, 0l, X86_64.RDX);
      X86_64.ADD_imm (X86_64.R8, 1l);
      X86_64.ADD_imm (X86_64.R9, 1l);
      X86_64.SUB_imm (X86_64.RAX, 1l);
      X86_64.JMP copyLabel;
      X86_64.Label copyDoneLabel;
    ]
  @ leakInc
  @ [
      X86_64.MOV_reg (X86_64.RAX, X86_64.R10);
      X86_64.JMP (label ^ "_done");
      X86_64.Label missingLabel;
      X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
      X86_64.Label (label ^ "_done");
      X86_64.RET;
    ]

(*  Start a shell command and retain its pid, descriptors, and raw output in a *)
(*  fixed process table. The otherwise-unused first free-list slot owns the *)
(*  table pointer without consuming an allocatable register. *)
let generateChildLaunchHelper () =
  let module X = X86_64 in
  let trap n = loadImm64 X.RAX n @ [ X.SYSCALL ] in
  let fd offset high =
    [ X.MOV_load (X.RDI, X.RSP, offset) ]
    @ if high then [ X.SHR_imm (X.RDI, 32) ] else [ X.MOV_reg32 (X.RDI, X.RDI) ]
  in
  let close offset high = fd offset high @ trap 3L in
  let closeAll =
    List.concat_map
      (fun offset -> close offset false @ close offset true)
      [ 0l; 8l; 16l; 24l ]
  in
  (* Keep pipe endpoints away from stdio before dup2. Otherwise a closed
     parent stdio slot can be reused, remain CLOEXEC, then be closed twice. *)
  let lift offset high =
    let doneLabel = Printf.sprintf "__child_lift_%ld_%b" offset high in
    fd offset high
    @ [ X.CMP_imm (X.RDI, 3l); X.Jcc (X.GE, doneLabel) ]
    @ loadImm64 X.RSI 1030L @ loadImm64 X.RDX 3L @ trap 72L
    @ [
        X.CMP_imm (X.RAX, 0l);
        X.Jcc (X.LT, "__child_launch_error");
        X.MOV_reg (X.R8, X.RAX);
      ]
    @ trap 3L
    @ [ X.MOV_load (X.RDX, X.RSP, offset) ]
    @ (if high then [ X.MOV_reg32 (X.RDX, X.RDX); X.SHL_imm (X.R8, 32) ]
       else [ X.SHR_imm (X.RDX, 32); X.SHL_imm (X.RDX, 32) ])
    @ [
        X.OR_reg (X.RDX, X.R8);
        X.MOV_store (X.RSP, offset, X.RDX);
        X.Label doneLabel;
      ]
  in
  let pipe offset =
    [ X.LEA (X.RDI, X.RSP, offset) ]
    @ loadImm64 X.RSI 524288L @ trap 293L
    @ [ X.CMP_imm (X.RAX, 0l); X.Jcc (X.LT, "__child_launch_error") ]
    @ lift offset false @ lift offset true
  in
  let nonblocking offset high =
    fd offset high @ loadImm64 X.RSI 3L @ loadImm64 X.RDX 0L @ trap 72L
    @ [
        X.CMP_imm (X.RAX, 0l);
        X.Jcc (X.LT, "__child_launch_kill_error");
        X.MOV_reg (X.RDX, X.RAX);
      ]
    @ loadImm64 X.R11 2048L
    @ [ X.OR_reg (X.RDX, X.R11) ]
    @ fd offset high @ loadImm64 X.RSI 4L @ trap 72L
    @ [ X.CMP_imm (X.RAX, 0l); X.Jcc (X.LT, "__child_launch_kill_error") ]
  in
  let duplicate offset high target =
    fd offset high @ loadImm64 X.RSI target @ trap 33L
    @ [ X.CMP_imm (X.RAX, 0l); X.Jcc (X.LT, "__child_launch_child_error") ]
  in
  let preserved =
    [
      X.RBP;
      X.RBX;
      X.R12;
      X.R13;
      X.RDI;
      X.RSI;
      X.RDX;
      X.RCX;
      X.R8;
      X.R9;
      X.R10;
      X.R11;
    ]
  in
  [ X.Label "__dark_child_launch" ]
  @ List.map (fun r -> X.PUSH r) preserved
  @ [
      X.SUB_imm (X.RSP, 96l);
      X.MOV_store (X.RSP, 32l, X.RDI);
      X.MOV_store (X.RSP, 40l, X.RSI);
      X.MOV_store (X.RSP, 48l, X.RDX);
      X.MOV_store (X.RSP, 56l, X.RCX);
    ]
  @ loadImm64 X.RAX (-1L)
  @ List.map
      (fun offset -> X.MOV_store (X.RSP, offset, X.RAX))
      [ 0l; 8l; 16l; 24l ]
  @ [ X.CMP_imm (X.RDX, 0l); X.Jcc (X.NE, "__child_launch_pipes_ready") ]
  @ pipe 0l @ pipe 8l @ pipe 16l
  @ [ X.Label "__child_launch_pipes_ready" ]
  @ pipe 24l @ trap 57L
  @ [
      X.CMP_imm (X.RAX, 0l);
      X.Jcc (X.LT, "__child_launch_error");
      X.Jcc (X.EQ, "__child_launch_child");
      X.MOV_store (X.RSP, 64l, X.RAX);
    ]
  @ close 0l false @ close 8l true @ close 16l true @ close 24l true
  @ [ X.Label "__child_launch_handshake" ]
  @ fd 24l false
  @ [ X.LEA (X.RSI, X.RSP, 72l) ]
  @ loadImm64 X.RDX 8L @ trap 0L
  @ [
      X.CMP_imm (X.RAX, -4l);
      X.Jcc (X.EQ, "__child_launch_handshake");
      X.CMP_imm (X.RAX, 0l);
      X.Jcc (X.LT, "__child_launch_kill_error");
      X.Jcc (X.GT, "__child_launch_exec_error");
    ]
  @ close 24l false
  @ [
      X.MOV_load (X.RAX, X.RSP, 48l);
      X.CMP_imm (X.RAX, 0l);
      X.Jcc (X.NE, "__child_launch_inherited");
    ]
  @ nonblocking 0l true @ nonblocking 8l false @ nonblocking 16l false
  @ [ X.MOV_load (X.RBX, X.RSP, 56l) ]
  @ fd 0l true
  @ [ X.MOV_store (X.RBX, 0l, X.RDI) ]
  @ fd 8l false
  @ [ X.MOV_store (X.RBX, 8l, X.RDI) ]
  @ fd 16l false
  @ [
      X.MOV_store (X.RBX, 16l, X.RDI);
      X.Label "__child_launch_inherited";
      X.MOV_load (X.RAX, X.RSP, 64l);
      X.JMP "__child_launch_return";
      X.Label "__child_launch_exec_error";
      X.MOV_load (X.RAX, X.RSP, 72l);
      X.JMP "__child_launch_reap_error";
      X.Label "__child_launch_kill_error";
      X.MOV_store (X.RSP, 72l, X.RAX);
      X.MOV_load (X.RDI, X.RSP, 64l);
    ]
  @ loadImm64 X.RSI 9L @ trap 62L
  @ [
      X.MOV_load (X.RAX, X.RSP, 72l);
      X.Label "__child_launch_reap_error";
      X.MOV_store (X.RSP, 72l, X.RAX);
      X.Label "__child_launch_reap_retry";
      X.MOV_load (X.RDI, X.RSP, 64l);
      X.LEA (X.RSI, X.RSP, 80l);
    ]
  @ loadImm64 X.RDX 0L @ loadImm64 X.R10 0L @ trap 61L
  @ [
      X.CMP_imm (X.RAX, -4l);
      X.Jcc (X.EQ, "__child_launch_reap_retry");
      X.MOV_load (X.RAX, X.RSP, 72l);
      X.Label "__child_launch_error";
      X.MOV_store (X.RSP, 72l, X.RAX);
    ]
  @ closeAll
  @ [
      X.MOV_load (X.RAX, X.RSP, 72l);
      X.Label "__child_launch_return";
      X.ADD_imm (X.RSP, 96l);
    ]
  @ List.map (fun r -> X.POP r) (List.rev preserved)
  @ [
      X.RET;
      X.Label "__child_launch_child";
      X.MOV_load (X.RAX, X.RSP, 48l);
      X.CMP_imm (X.RAX, 0l);
      X.Jcc (X.NE, "__child_launch_exec");
    ]
  @ duplicate 0l false 0L @ duplicate 8l true 1L @ duplicate 16l true 2L
  @ [ X.Label "__child_launch_exec" ]
  @ close 0l false @ close 0l true @ close 8l false @ close 8l true
  @ close 16l false @ close 16l true @ close 24l false
  @ [
      X.MOV_load (X.RSI, X.RSP, 32l);
      X.MOV_load (X.RDI, X.RSI, 0l);
      X.MOV_load (X.RDX, X.RSP, 40l);
    ]
  @ trap 59L
  @ [ X.Label "__child_launch_child_error"; X.MOV_store (X.RSP, 72l, X.RAX) ]
  @ fd 24l true
  @ [ X.LEA (X.RSI, X.RSP, 72l) ]
  @ loadImm64 X.RDX 8L @ trap 1L @ loadImm64 X.RDI 127L @ trap 60L

let generateChildCleanupHelper enableLeakCheck =
  let module X = X86_64 in
  let trap n = loadImm64 X.RAX n @ [ X.SYSCALL ] in
  let preserve = [ X.RAX; X.RBX; X.R12; X.RDI; X.RSI; X.RDX; X.R10; X.R11 ] in
  let close offset =
    let doneLabel = Printf.sprintf "__child_cleanup_closed_%ld" offset in
    [
      X.MOV_load (X.RDI, X.RBX, offset);
      X.CMP_imm (X.RDI, 0l);
      X.Jcc (X.LT, doneLabel);
    ]
    @ loadImm64 X.R11 (-1L)
    @ [ X.MOV_store (X.RBX, offset, X.R11) ]
    @ trap 3L @ [ X.Label doneLabel ]
  in
  let free offset =
    let doneLabel = Printf.sprintf "__child_cleanup_freed_%ld" offset in
    [
      X.MOV_load (X.RDI, X.RBX, offset);
      X.CMP_imm (X.RDI, 0l);
      X.Jcc (X.EQ, doneLabel);
      X.XOR_reg (X.R11, X.R11);
      X.MOV_store (X.RBX, offset, X.R11);
      X.SUB_imm (X.RDI, 8l);
      X.MOV_load (X.RSI, X.RDI, 0l);
    ]
    @ trap 11L
    @ (if not enableLeakCheck then []
       else
         [
           X.LEA_rip (X.R11, "_leak_count");
           X.MOV_load (X.R10, X.R11, 0l);
           X.SUB_imm (X.R10, 1l);
           X.MOV_store (X.R11, 0l, X.R10);
         ])
    @ [ X.Label doneLabel ]
  in
  [ X.Label "__dark_cli_cleanup_processes" ]
  @ List.map (fun r -> X.PUSH r) preserve
  @ [
      X.SUB_imm (X.RSP, 16l);
      X.MOV_imm32 (X.R12, 1l);
      X.Label "__child_cleanup_next";
      X.CMP_imm (X.R12, 63l);
      X.Jcc (X.GT, "__child_cleanup_done");
      X.LEA (X.RBX, freeListBase, Int32.of_int processTableOffset);
      X.MOV_reg (X.R10, X.R12);
      X.SHL_imm (X.R10, 6);
      X.ADD_reg (X.RBX, X.R10);
      X.MOV_load (X.RAX, X.RBX, 0l);
      X.CMP_imm (X.RAX, 0l);
      X.Jcc (X.EQ, "__child_cleanup_advance");
      X.MOV_load (X.RAX, X.RBX, 40l);
      X.CMP_imm (X.RAX, 0l);
      X.Jcc (X.GE, "__child_cleanup_pipes");
      X.MOV_load (X.RDI, X.RBX, 8l);
    ]
  @ loadImm64 X.RSI 9L @ trap 62L
  @ [
      X.Label "__child_cleanup_reap";
      X.MOV_load (X.RDI, X.RBX, 8l);
      X.MOV_reg (X.RSI, X.RSP);
      X.XOR_reg (X.RDX, X.RDX);
      X.XOR_reg (X.R10, X.R10);
    ]
  @ trap 61L
  @ [
      X.CMP_imm (X.RAX, -4l);
      X.Jcc (X.EQ, "__child_cleanup_reap");
      X.Label "__child_cleanup_pipes";
    ]
  @ close 16l @ close 24l @ close 32l @ free 48l @ free 56l
  @ [
      X.XOR_reg (X.RAX, X.RAX);
      X.MOV_store (X.RBX, 0l, X.RAX);
      X.Label "__child_cleanup_advance";
      X.ADD_imm (X.R12, 1l);
      X.JMP "__child_cleanup_next";
      X.Label "__child_cleanup_done";
      X.ADD_imm (X.RSP, 16l);
    ]
  @ List.map (fun r -> X.POP r) (List.rev preserve)
  @ [ X.RET ]
