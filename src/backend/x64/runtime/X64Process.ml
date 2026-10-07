open X64Operands
(*  X64Process.ml - Generate x64 environment and process runtime helpers. *)



(*  Return argv[index + 1] as a nullable String pointer. RDI carries the zero-based *)
(*  positional index. Native argv entries are zero-terminated, so present *)
(*  values are copied into managed Dark strings. Following the frame-pointer *)
(*  chain reaches _start's root frame without reserving process-state registers. *)
let generateCliArgvHelper () =
    let label = "__dark_cli_argv"
    in
    let missingLabel = (label^"_missing")
    in
    let rootLabel = (label^"_find_root")
    in
    let rootFoundLabel = (label^"_root_found")
    in
    let lengthLabel = (label^"_length")
    in
    let lengthDoneLabel = (label^"_length_done")
    in
    let copyLabel = (label^"_copy")
    in
    let copyDoneLabel = (label^"_copy_done")
    in
    let stringHeapOkLabel = (label^"_string_heap_ok")
    in
    let savedRegs =
        [ X86_64.RDI;
          X86_64.RSI;
          X86_64.RDX;
          X86_64.RCX;
          X86_64.R8;
          X86_64.R9;
          X86_64.R10;
          X86_64.R11 ]
    in
    let saves = savedRegs |> List.map (fun r -> X86_64.PUSH r)
    in
    let restores = savedRegs |> List.rev |> List.map (fun r -> X86_64.POP r)
    in
    let checkHeapBounds okLabel =
        [ X86_64.MOV_reg (X86_64.R11, heapPtr);
          X86_64.SUB_reg (X86_64.R11, freeListBase);
          X86_64.CMP_imm (X86_64.R11, Int64.to_int32 heapMmapSizeBytes);
          X86_64.Jcc (X86_64.LE, okLabel);
          X86_64.JMP oomHandlerLabel;
          X86_64.Label okLabel ]

    in
    [ X86_64.Label label ]
    @ saves
    @ [ X86_64.CMP_imm (X86_64.RDI, 0l);
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
        X86_64.ADD_reg (heapPtr, X86_64.R11) ]
    @ checkHeapBounds stringHeapOkLabel
    @ loadImm64 X86_64.RDX 0x7FFFFFFFFFFFFFFFL
    @ [ X86_64.MOV_store (X86_64.R10, 0l, X86_64.RDX);
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
        X86_64.JMP (label^"_done");
        X86_64.Label missingLabel;
        X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
        X86_64.Label (label^"_done") ]
    @ restores
    @ [ X86_64.RET ]

(*  Return the original process environment as NUL-delimited UTF-8 entries. *)
let generateCliEnvironmentPackedHelper (enableLeakCheck:bool) =
    let label = "__dark_cli_environment_packed"
    in
    let findRoot = (label^"_find_root")
    in
    let rootFound = (label^"_root_found")
    in
    let findArgvEnd = (label^"_find_argv_end")
    in
    let countEntry = (label^"_count_entry")
    in
    let countByte = (label^"_count_byte")
    in
    let countNext = (label^"_count_next")
    in
    let countDone = (label^"_count_done")
    in
    let copyEntry = (label^"_copy_entry")
    in
    let copyByte = (label^"_copy_byte")
    in
    let copyNext = (label^"_copy_next")
    in
    let copyDone = (label^"_copy_done")
    in
    let leakInc =
        if enableLeakCheck then
            [ X86_64.LEA_rip (X86_64.R11, "_leak_count");
              X86_64.MOV_load (X86_64.RDX, X86_64.R11, 0l);
              X86_64.ADD_imm (X86_64.RDX, 1l);
              X86_64.MOV_store (X86_64.R11, 0l, X86_64.RDX) ]
        else []
    in
    [ X86_64.Label label;
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
      X86_64.MOV_reg (X86_64.RAX, X86_64.R10) ]
    @ leakInc
    @ [X86_64.RET]

(*  Return getcwd(2) as a managed UTF-8 string. *)
let generateCliDirectoryCurrentHelper (enableLeakCheck:bool) =
    let label = "__dark_cli_directory_current"
    in
    let lengthLoop = (label^"_length")
    in
    let lengthDone = (label^"_length_done")
    in
    let copyLoop = (label^"_copy")
    in
    let copyDone = (label^"_copy_done")
    in
    let failure = (label^"_failure")
    in
    let complete = (label^"_complete")
    in
    [ X86_64.Label label;
      X86_64.SUB_imm (X86_64.RSP, 4096l);
      X86_64.MOV_reg (X86_64.RDI, X86_64.RSP) ]
    @ loadImm64 X86_64.RSI 4096L
    @ loadImm64 X86_64.RAX 79L
    @ [ X86_64.SYSCALL;
        X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.LT, failure);
        X86_64.XOR_reg (X86_64.RCX, X86_64.RCX);
        X86_64.Label lengthLoop;
        X86_64.MOV_reg (X86_64.RDX, X86_64.RSP);
        X86_64.ADD_reg (X86_64.RDX, X86_64.RCX);
        X86_64.MOV_load_byte (X86_64.R8, X86_64.RDX, 0l);
        X86_64.CMP_imm (X86_64.R8, 0l);
        X86_64.Jcc (X86_64.EQ, lengthDone);
        X86_64.ADD_imm (X86_64.RCX, 1l);
        X86_64.JMP lengthLoop;
        X86_64.Label lengthDone;
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
        X86_64.XOR_reg (X86_64.R8, X86_64.R8);
        X86_64.Label copyLoop;
        X86_64.CMP_reg (X86_64.R8, X86_64.RCX);
        X86_64.Jcc (X86_64.GE, copyDone);
        X86_64.MOV_reg (X86_64.RDX, X86_64.RSP);
        X86_64.ADD_reg (X86_64.RDX, X86_64.R8);
        X86_64.MOV_load_byte (X86_64.RDI, X86_64.RDX, 0l);
        X86_64.MOV_store_byte (X86_64.R9, 0l, X86_64.RDI);
        X86_64.ADD_imm (X86_64.R9, 1l);
        X86_64.ADD_imm (X86_64.R8, 1l);
        X86_64.JMP copyLoop;
        X86_64.Label copyDone;
        X86_64.MOV_reg (X86_64.RAX, X86_64.R10);
        X86_64.ADD_imm (X86_64.RSP, 4096l) ]
    @ (if enableLeakCheck then
           [ X86_64.LEA_rip (X86_64.R11, "_leak_count");
             X86_64.MOV_load (X86_64.RDX, X86_64.R11, 0l);
             X86_64.ADD_imm (X86_64.RDX, 1l);
             X86_64.MOV_store (X86_64.R11, 0l, X86_64.RDX) ]
       else [])
    @ [ X86_64.JMP complete;
        X86_64.Label failure;
        X86_64.ADD_imm (X86_64.RSP, 4096l) ]
    @ emitStringLiteral X86_64.RAX ""
    @ [ X86_64.Label complete;
        X86_64.RET ]

(*  Mutate the process environment vector used by getenv and child execve calls. *)
let generateCliSetEnvHelper (enableLeakCheck:bool) =
    let label = "__dark_cli_setenv"
    in
    let findRoot = (label^"_find_root")
    in
    let rootFound = (label^"_root_found")
    in
    let findArgvEnd = (label^"_find_argv_end")
    in
    let nextEntry = (label^"_next_entry")
    in
    let compare = (label^"_compare")
    in
    let nameMatched = (label^"_name_matched")
    in
    let advance = (label^"_advance")
    in
    let useSlot = (label^"_use_slot")
    in
    let copyName = (label^"_copy_name")
    in
    let nameDone = (label^"_name_done")
    in
    let copyValue = (label^"_copy_value")
    in
    let valueDone = (label^"_value_done")
    in
    let stored = (label^"_stored")
    in
    [ X86_64.Label label;
      X86_64.PUSH X86_64.R12;
      X86_64.PUSH X86_64.R13;
      X86_64.PUSH X86_64.R15;
      X86_64.MOV_reg (X86_64.R12, X86_64.RDI);
      X86_64.MOV_reg (X86_64.R13, X86_64.RSI);
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
      X86_64.MOV_load (X86_64.RCX, X86_64.R12, 8l);
      X86_64.Label nextEntry;
      X86_64.MOV_load (X86_64.R8, X86_64.RAX, 0l);
      X86_64.CMP_imm (X86_64.R8, 0l);
      X86_64.Jcc (X86_64.EQ, useSlot);
      X86_64.XOR_reg (X86_64.R9, X86_64.R9);
      X86_64.Label compare;
      X86_64.CMP_reg (X86_64.R9, X86_64.RCX);
      X86_64.Jcc (X86_64.GE, nameMatched);
      X86_64.LEA (X86_64.R10, X86_64.R12, 16l);
      X86_64.ADD_reg (X86_64.R10, X86_64.R9);
      X86_64.MOV_load_byte (X86_64.RDX, X86_64.R10, 0l);
      X86_64.MOV_reg (X86_64.R10, X86_64.R8);
      X86_64.ADD_reg (X86_64.R10, X86_64.R9);
      X86_64.MOV_load_byte (X86_64.R11, X86_64.R10, 0l);
      X86_64.CMP_reg (X86_64.RDX, X86_64.R11);
      X86_64.Jcc (X86_64.NE, advance);
      X86_64.ADD_imm (X86_64.R9, 1l);
      X86_64.JMP compare;
      X86_64.Label nameMatched;
      X86_64.MOV_reg (X86_64.R10, X86_64.R8);
      X86_64.ADD_reg (X86_64.R10, X86_64.RCX);
      X86_64.MOV_load_byte (X86_64.RDX, X86_64.R10, 0l);
      X86_64.CMP_imm (X86_64.RDX, 61l);
      X86_64.Jcc (X86_64.EQ, useSlot);
      X86_64.Label advance;
      X86_64.ADD_imm (X86_64.RAX, 8l);
      X86_64.JMP nextEntry;
      X86_64.Label useSlot;
      X86_64.MOV_reg (X86_64.R15, X86_64.RAX);
      X86_64.MOV_reg (X86_64.R10, heapPtr);
      X86_64.MOV_load (X86_64.R11, X86_64.R13, 8l);
      X86_64.ADD_reg (X86_64.R11, X86_64.RCX);
      X86_64.ADD_imm (X86_64.R11, 9l);
      X86_64.AND_imm (X86_64.R11, -8l);
      X86_64.ADD_reg (heapPtr, X86_64.R11);
      X86_64.LEA (X86_64.R8, X86_64.R12, 16l);
      X86_64.MOV_reg (X86_64.R9, X86_64.R10);
      X86_64.MOV_reg (X86_64.R11, X86_64.RCX);
      X86_64.Label copyName;
      X86_64.CMP_imm (X86_64.R11, 0l);
      X86_64.Jcc (X86_64.EQ, nameDone);
      X86_64.MOV_load_byte (X86_64.RDX, X86_64.R8, 0l);
      X86_64.MOV_store_byte (X86_64.R9, 0l, X86_64.RDX);
      X86_64.ADD_imm (X86_64.R8, 1l);
      X86_64.ADD_imm (X86_64.R9, 1l);
      X86_64.SUB_imm (X86_64.R11, 1l);
      X86_64.JMP copyName;
      X86_64.Label nameDone;
      X86_64.MOV_imm32 (X86_64.RDX, 61l);
      X86_64.MOV_store_byte (X86_64.R9, 0l, X86_64.RDX);
      X86_64.ADD_imm (X86_64.R9, 1l);
      X86_64.LEA (X86_64.R8, X86_64.R13, 16l);
      X86_64.MOV_load (X86_64.R11, X86_64.R13, 8l);
      X86_64.Label copyValue;
      X86_64.CMP_imm (X86_64.R11, 0l);
      X86_64.Jcc (X86_64.EQ, valueDone);
      X86_64.MOV_load_byte (X86_64.RDX, X86_64.R8, 0l);
      X86_64.MOV_store_byte (X86_64.R9, 0l, X86_64.RDX);
      X86_64.ADD_imm (X86_64.R8, 1l);
      X86_64.ADD_imm (X86_64.R9, 1l);
      X86_64.SUB_imm (X86_64.R11, 1l);
      X86_64.JMP copyValue;
      X86_64.Label valueDone;
      X86_64.XOR_reg (X86_64.RDX, X86_64.RDX);
      X86_64.MOV_store_byte (X86_64.R9, 0l, X86_64.RDX);
      X86_64.MOV_load (X86_64.R8, X86_64.R15, 0l);
      X86_64.MOV_store (X86_64.R15, 0l, X86_64.R10);
      X86_64.CMP_imm (X86_64.R8, 0l);
      X86_64.Jcc (X86_64.NE, stored);
      X86_64.MOV_store (X86_64.R15, 8l, X86_64.R8);
      X86_64.Label stored;
      X86_64.MOV_reg (X86_64.RAX, heapPtr);
      X86_64.ADD_imm (heapPtr, 24l);
      X86_64.XOR_reg (X86_64.RDX, X86_64.RDX);
      X86_64.MOV_store (X86_64.RAX, 0l, X86_64.RDX);
      X86_64.MOV_store (X86_64.RAX, 8l, X86_64.RDX);
      X86_64.MOV_imm32 (X86_64.RDX, 1l);
      X86_64.MOV_store (X86_64.RAX, 16l, X86_64.RDX) ]
    @ (if enableLeakCheck then
           [ X86_64.LEA_rip (X86_64.R11, "_leak_count");
             X86_64.MOV_load (X86_64.RDX, X86_64.R11, 0l);
             X86_64.ADD_imm (X86_64.RDX, 1l);
             X86_64.MOV_store (X86_64.R11, 0l, X86_64.RDX) ]
       else [])
    @ [ X86_64.POP X86_64.R15;
        X86_64.POP X86_64.R13;
        X86_64.POP X86_64.R12;
        X86_64.RET ]

(*  Remove a matching entry from the original process environment vector. *)
let generateCliUnsetEnvHelper (enableLeakCheck:bool) =
    let label = "__dark_cli_unsetenv"
    in
    let findRoot = (label^"_find_root")
    in
    let rootFound = (label^"_root_found")
    in
    let findArgvEnd = (label^"_find_argv_end")
    in
    let nextEntry = (label^"_next_entry")
    in
    let compare = (label^"_compare")
    in
    let nameMatched = (label^"_name_matched")
    in
    let advance = (label^"_advance")
    in
    let shift = (label^"_shift")
    in
    let doneLabel = (label^"_done")
    in
    [ X86_64.Label label;
      X86_64.PUSH X86_64.R12;
      X86_64.MOV_reg (X86_64.R12, X86_64.RDI);
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
      X86_64.MOV_load (X86_64.RCX, X86_64.R12, 8l);
      X86_64.Label nextEntry;
      X86_64.MOV_load (X86_64.R8, X86_64.RAX, 0l);
      X86_64.CMP_imm (X86_64.R8, 0l);
      X86_64.Jcc (X86_64.EQ, doneLabel);
      X86_64.XOR_reg (X86_64.R9, X86_64.R9);
      X86_64.Label compare;
      X86_64.CMP_reg (X86_64.R9, X86_64.RCX);
      X86_64.Jcc (X86_64.GE, nameMatched);
      X86_64.LEA (X86_64.R10, X86_64.R12, 16l);
      X86_64.ADD_reg (X86_64.R10, X86_64.R9);
      X86_64.MOV_load_byte (X86_64.RDX, X86_64.R10, 0l);
      X86_64.MOV_reg (X86_64.R10, X86_64.R8);
      X86_64.ADD_reg (X86_64.R10, X86_64.R9);
      X86_64.MOV_load_byte (X86_64.R11, X86_64.R10, 0l);
      X86_64.CMP_reg (X86_64.RDX, X86_64.R11);
      X86_64.Jcc (X86_64.NE, advance);
      X86_64.ADD_imm (X86_64.R9, 1l);
      X86_64.JMP compare;
      X86_64.Label nameMatched;
      X86_64.MOV_reg (X86_64.R10, X86_64.R8);
      X86_64.ADD_reg (X86_64.R10, X86_64.RCX);
      X86_64.MOV_load_byte (X86_64.RDX, X86_64.R10, 0l);
      X86_64.CMP_imm (X86_64.RDX, 61l);
      X86_64.Jcc (X86_64.NE, advance);
      X86_64.MOV_reg (X86_64.R8, X86_64.RAX);
      X86_64.LEA (X86_64.R9, X86_64.RAX, 8l);
      X86_64.Label shift;
      X86_64.MOV_load (X86_64.R10, X86_64.R9, 0l);
      X86_64.MOV_store (X86_64.R8, 0l, X86_64.R10);
      X86_64.CMP_imm (X86_64.R10, 0l);
      X86_64.Jcc (X86_64.EQ, doneLabel);
      X86_64.ADD_imm (X86_64.R8, 8l);
      X86_64.ADD_imm (X86_64.R9, 8l);
      X86_64.JMP shift;
      X86_64.Label advance;
      X86_64.ADD_imm (X86_64.RAX, 8l);
      X86_64.JMP nextEntry;
      X86_64.Label doneLabel;
      X86_64.MOV_reg (X86_64.RAX, heapPtr);
      X86_64.ADD_imm (heapPtr, 24l);
      X86_64.XOR_reg (X86_64.RDX, X86_64.RDX);
      X86_64.MOV_store (X86_64.RAX, 0l, X86_64.RDX);
      X86_64.MOV_store (X86_64.RAX, 8l, X86_64.RDX);
      X86_64.MOV_imm32 (X86_64.RDX, 1l);
      X86_64.MOV_store (X86_64.RAX, 16l, X86_64.RDX) ]
    @ (if enableLeakCheck then
           [ X86_64.LEA_rip (X86_64.R11, "_leak_count");
             X86_64.MOV_load (X86_64.RDX, X86_64.R11, 0l);
             X86_64.ADD_imm (X86_64.RDX, 1l);
             X86_64.MOV_store (X86_64.R11, 0l, X86_64.RDX) ]
       else [])
    @ [ X86_64.POP X86_64.R12;
        X86_64.RET ]

(*  Return getdents64(2) entries as NUL-delimited full paths. *)
let generateCliDirectoryListHelper (enableLeakCheck:bool) =
    let label = "__dark_cli_directory_list"
    in
    let copyPath = (label^"_copy_path")
    in
    let pathDone = (label^"_path_done")
    in
    let readChunk = (label^"_read_chunk")
    in
    let readDone = (label^"_read_done")
    in
    let entryLoop = (label^"_entry_loop")
    in
    let entriesDone = (label^"_entries_done")
    in
    let skipEntry = (label^"_skip_entry")
    in
    let appendPath = (label^"_append_path")
    in
    let appendPathLoop = (label^"_append_path_loop")
    in
    let pathAppended = (label^"_path_appended")
    in
    let appendName = (label^"_append_name")
    in
    let nameDone = (label^"_name_done")
    in
    let openFailed = (label^"_open_failed")
    in
    let complete = (label^"_complete")
    in
    [ X86_64.Label label;
      X86_64.PUSH X86_64.R12;
      X86_64.PUSH X86_64.R13;
      X86_64.PUSH X86_64.R15;
      X86_64.SUB_imm (X86_64.RSP, 8208l);
      X86_64.MOV_store (X86_64.RSP, 8192l, X86_64.RDI);
      X86_64.MOV_load (X86_64.RCX, X86_64.RDI, 8l);
      X86_64.MOV_store (X86_64.RSP, 8200l, X86_64.RCX);
      X86_64.LEA (X86_64.RSI, X86_64.RDI, 16l);
      X86_64.MOV_reg (X86_64.RDI, X86_64.RSP);
      X86_64.XOR_reg (X86_64.R8, X86_64.R8);
      X86_64.Label copyPath;
      X86_64.CMP_reg (X86_64.R8, X86_64.RCX);
      X86_64.Jcc (X86_64.GE, pathDone);
      X86_64.MOV_reg (X86_64.R9, X86_64.RSI);
      X86_64.ADD_reg (X86_64.R9, X86_64.R8);
      X86_64.MOV_load_byte (X86_64.R10, X86_64.R9, 0l);
      X86_64.MOV_reg (X86_64.R9, X86_64.RDI);
      X86_64.ADD_reg (X86_64.R9, X86_64.R8);
      X86_64.MOV_store_byte (X86_64.R9, 0l, X86_64.R10);
      X86_64.ADD_imm (X86_64.R8, 1l);
      X86_64.JMP copyPath;
      X86_64.Label pathDone;
      X86_64.MOV_reg (X86_64.R9, X86_64.RDI);
      X86_64.ADD_reg (X86_64.R9, X86_64.RCX);
      X86_64.XOR_reg (X86_64.R10, X86_64.R10);
      X86_64.MOV_store_byte (X86_64.R9, 0l, X86_64.R10) ]
    @ loadImm64 X86_64.RSI 65536L
    @ loadImm64 X86_64.RDX 0L
    @ loadImm64 X86_64.RAX (Int64.of_int syscalls.Platform.open_)
    @ [ X86_64.SYSCALL;
        X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.LT, openFailed);
        X86_64.MOV_reg (X86_64.R15, X86_64.RAX);
        X86_64.MOV_reg (X86_64.R12, heapPtr);
        X86_64.XOR_reg (X86_64.R13, X86_64.R13);
        X86_64.Label readChunk;
        X86_64.MOV_reg (X86_64.RDI, X86_64.R15);
        X86_64.LEA (X86_64.RSI, X86_64.RSP, 4096l) ]
    @ loadImm64 X86_64.RDX 4096L
    @ loadImm64 X86_64.RAX 217L
    @ [ X86_64.SYSCALL;
        X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.LE, readDone);
        X86_64.MOV_reg (X86_64.RCX, X86_64.RAX);
        X86_64.XOR_reg (X86_64.R8, X86_64.R8);
        X86_64.Label entryLoop;
        X86_64.CMP_reg (X86_64.R8, X86_64.RCX);
        X86_64.Jcc (X86_64.GE, entriesDone);
        X86_64.LEA (X86_64.R9, X86_64.RSP, 4096l);
        X86_64.ADD_reg (X86_64.R9, X86_64.R8);
        X86_64.MOV_load_byte (X86_64.R10, X86_64.R9, 16l);
        X86_64.MOV_load_byte (X86_64.R11, X86_64.R9, 17l);
        X86_64.SHL_imm (X86_64.R11, 8);
        X86_64.OR_reg (X86_64.R10, X86_64.R11);
        X86_64.ADD_imm (X86_64.R9, 19l);
        X86_64.MOV_load_byte (X86_64.R11, X86_64.R9, 0l);
        X86_64.CMP_imm (X86_64.R11, 46l);
        X86_64.Jcc (X86_64.NE, appendPath);
        X86_64.MOV_load_byte (X86_64.R11, X86_64.R9, 1l);
        X86_64.CMP_imm (X86_64.R11, 0l);
        X86_64.Jcc (X86_64.EQ, skipEntry);
        X86_64.CMP_imm (X86_64.R11, 46l);
        X86_64.Jcc (X86_64.NE, appendPath);
        X86_64.MOV_load_byte (X86_64.R11, X86_64.R9, 2l);
        X86_64.CMP_imm (X86_64.R11, 0l);
        X86_64.Jcc (X86_64.EQ, skipEntry);
        X86_64.Label appendPath;
        X86_64.MOV_load (X86_64.RDI, X86_64.RSP, 8192l);
        X86_64.LEA (X86_64.RSI, X86_64.RDI, 16l);
        X86_64.MOV_load (X86_64.RDX, X86_64.RSP, 8200l);
        X86_64.LEA (X86_64.RDI, X86_64.R12, 16l);
        X86_64.ADD_reg (X86_64.RDI, X86_64.R13);
        X86_64.Label appendPathLoop;
        X86_64.CMP_imm (X86_64.RDX, 0l);
        X86_64.Jcc (X86_64.EQ, pathAppended);
        X86_64.MOV_load_byte (X86_64.R11, X86_64.RSI, 0l);
        X86_64.MOV_store_byte (X86_64.RDI, 0l, X86_64.R11);
        X86_64.ADD_imm (X86_64.RSI, 1l);
        X86_64.ADD_imm (X86_64.RDI, 1l);
        X86_64.ADD_imm (X86_64.R13, 1l);
        X86_64.SUB_imm (X86_64.RDX, 1l);
        X86_64.JMP appendPathLoop;
        X86_64.Label pathAppended;
        X86_64.MOV_load (X86_64.RDX, X86_64.RSP, 8200l);
        X86_64.CMP_imm (X86_64.RDX, 0l);
        X86_64.Jcc (X86_64.EQ, appendName);
        X86_64.MOV_load (X86_64.RSI, X86_64.RSP, 8192l);
        X86_64.ADD_imm (X86_64.RSI, 16l);
        X86_64.ADD_reg (X86_64.RSI, X86_64.RDX);
        X86_64.MOV_load_byte (X86_64.R11, X86_64.RSI, -1l);
        X86_64.CMP_imm (X86_64.R11, 47l);
        X86_64.Jcc (X86_64.EQ, appendName);
        X86_64.MOV_imm32 (X86_64.R11, 47l);
        X86_64.MOV_store_byte (X86_64.RDI, 0l, X86_64.R11);
        X86_64.ADD_imm (X86_64.RDI, 1l);
        X86_64.ADD_imm (X86_64.R13, 1l);
        X86_64.Label appendName;
        X86_64.MOV_load_byte (X86_64.R11, X86_64.R9, 0l);
        X86_64.CMP_imm (X86_64.R11, 0l);
        X86_64.Jcc (X86_64.EQ, nameDone);
        X86_64.MOV_store_byte (X86_64.RDI, 0l, X86_64.R11);
        X86_64.ADD_imm (X86_64.R9, 1l);
        X86_64.ADD_imm (X86_64.RDI, 1l);
        X86_64.ADD_imm (X86_64.R13, 1l);
        X86_64.JMP appendName;
        X86_64.Label nameDone;
        X86_64.XOR_reg (X86_64.R11, X86_64.R11);
        X86_64.MOV_store_byte (X86_64.RDI, 0l, X86_64.R11);
        X86_64.ADD_imm (X86_64.R13, 1l);
        X86_64.Label skipEntry;
        X86_64.ADD_reg (X86_64.R8, X86_64.R10);
        X86_64.JMP entryLoop;
        X86_64.Label entriesDone;
        X86_64.JMP readChunk;
        X86_64.Label readDone;
        X86_64.MOV_reg (X86_64.RDI, X86_64.R15) ]
    @ loadImm64 X86_64.RAX (Int64.of_int syscalls.Platform.close)
    @ [ X86_64.SYSCALL;
        X86_64.MOV_imm32 (X86_64.RDX, 1l);
        X86_64.MOV_store (X86_64.R12, 0l, X86_64.RDX);
        X86_64.MOV_store (X86_64.R12, 8l, X86_64.R13);
        X86_64.MOV_reg (X86_64.R11, X86_64.R13);
        X86_64.ADD_imm (X86_64.R11, 7l);
        X86_64.AND_imm (X86_64.R11, -8l);
        X86_64.ADD_imm (X86_64.R11, 16l);
        X86_64.ADD_reg (heapPtr, X86_64.R11);
        X86_64.MOV_reg (X86_64.RAX, X86_64.R12) ]
    @ (if enableLeakCheck then
           [ X86_64.LEA_rip (X86_64.R11, "_leak_count");
             X86_64.MOV_load (X86_64.RDX, X86_64.R11, 0l);
             X86_64.ADD_imm (X86_64.RDX, 1l);
             X86_64.MOV_store (X86_64.R11, 0l, X86_64.RDX) ]
       else [])
    @ [ X86_64.JMP complete;
        X86_64.Label openFailed ]
    @ emitStringLiteral X86_64.RAX ""
    @ [ X86_64.Label complete;
        X86_64.ADD_imm (X86_64.RSP, 8208l);
        X86_64.POP X86_64.R15;
        X86_64.POP X86_64.R13;
        X86_64.POP X86_64.R12;
        X86_64.RET ]

(*  Look up a managed name in the original process environment and return a *)
(*  nullable String pointer. This walks _start's native envp directly; no libc or *)
(*  child process is involved. *)
let generateCliGetEnvHelper (enableLeakCheck:bool) =
    let label = "__dark_cli_getenv"
    in
    let rootLabel = (label^"_find_root")
    in
    let rootFoundLabel = (label^"_root_found")
    in
    let argvEndLabel = (label^"_find_argv_end")
    in
    let nextEntryLabel = (label^"_next_entry")
    in
    let compareLabel = (label^"_compare")
    in
    let nameMatchedLabel = (label^"_name_matched")
    in
    let lengthLabel = (label^"_length")
    in
    let lengthDoneLabel = (label^"_length_done")
    in
    let copyLabel = (label^"_copy")
    in
    let copyDoneLabel = (label^"_copy_done")
    in
    let stringHeapOkLabel = (label^"_string_heap_ok")
    in
    let missingLabel = (label^"_missing")
    in
    let leakInc =
        if enableLeakCheck then
            [ X86_64.LEA_rip (X86_64.R11, "_leak_count");
              X86_64.MOV_load (X86_64.RDX, X86_64.R11, 0l);
              X86_64.ADD_imm (X86_64.RDX, 1l);
              X86_64.MOV_store (X86_64.R11, 0l, X86_64.RDX) ]
        else []
    in
    let checkHeapBounds okLabel =
        [ X86_64.MOV_reg (X86_64.R11, heapPtr);
          X86_64.SUB_reg (X86_64.R11, freeListBase);
          X86_64.CMP_imm (X86_64.R11, Int64.to_int32 heapMmapSizeBytes);
          X86_64.Jcc (X86_64.LE, okLabel);
          X86_64.JMP oomHandlerLabel;
          X86_64.Label okLabel ]

    in
    [ X86_64.Label label;
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
      X86_64.ADD_reg (heapPtr, X86_64.R11) ]
    @ checkHeapBounds stringHeapOkLabel
    @ [ X86_64.MOV_imm32 (X86_64.RDX, 1l);
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
        X86_64.Label copyDoneLabel ]
    @ leakInc
    @ [ X86_64.MOV_reg (X86_64.RAX, X86_64.R10);
        X86_64.JMP (label^"_done");
        X86_64.Label missingLabel;
        X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
        X86_64.Label (label^"_done");
        X86_64.RET ]

(*  Start a shell command and retain its pid, descriptors, and raw output in a *)
(*  fixed process table. The otherwise-unused first free-list slot owns the *)
(*  table pointer without consuming an allocatable register. *)
let generateLinuxCliSpawnProcessHelper () =
    let syscall number = loadImm64 X86_64.RAX number @ [X86_64.SYSCALL]
    in
    let loadFd stackOffset highHalf =
        [X86_64.MOV_load (X86_64.RDI, X86_64.RSP, stackOffset)]
        @ (if highHalf then [X86_64.SHR_imm (X86_64.RDI, 32)] else [X86_64.MOV_reg32 (X86_64.RDI, X86_64.RDI)])
    in
    let closeFd stackOffset highHalf = loadFd stackOffset highHalf @ syscall 3L
    in
    let setNonblocking stackOffset =
        loadFd stackOffset false
        @ loadImm64 X86_64.RSI 4L
        @ loadImm64 X86_64.RDX 2048L
        @ syscall 72L
    in
    [ X86_64.Label "__dark_cli_spawn_process";
      X86_64.PUSH X86_64.RBP;
      X86_64.MOV_reg (X86_64.RBP, X86_64.RSP);
      X86_64.PUSH X86_64.RBX;
      X86_64.PUSH X86_64.R12;
      X86_64.PUSH X86_64.R13;
      X86_64.PUSH X86_64.RDI;
      X86_64.PUSH X86_64.RSI;
      X86_64.PUSH X86_64.RCX;
      X86_64.PUSH X86_64.R8;
      X86_64.PUSH X86_64.R9;
      X86_64.PUSH X86_64.R10;
      X86_64.PUSH X86_64.RDX;
      X86_64.SUB_imm (X86_64.RSP, 80l);
      X86_64.LEA (X86_64.R13, freeListBase, Int32.of_int processTableOffset);
      X86_64.Label "__dark_spawn_table_ready";
(*  Copy the managed command to a native NUL-terminated buffer. *)
      X86_64.MOV_reg (X86_64.RBX, heapPtr);
      X86_64.MOV_load (X86_64.R10, X86_64.RDI, 8l);
      X86_64.LEA (X86_64.RSI, X86_64.RDI, 16l);
      X86_64.XOR_reg (X86_64.RCX, X86_64.RCX);
      X86_64.Label "__dark_spawn_command_copy";
      X86_64.CMP_reg (X86_64.RCX, X86_64.R10);
      X86_64.Jcc (X86_64.GE, "__dark_spawn_command_copied");
      X86_64.MOV_load_byte (X86_64.RAX, X86_64.RSI, 0l);
      X86_64.MOV_reg (X86_64.R11, X86_64.RBX);
      X86_64.ADD_reg (X86_64.R11, X86_64.RCX);
      X86_64.MOV_store_byte (X86_64.R11, 0l, X86_64.RAX);
      X86_64.ADD_imm (X86_64.RSI, 1l);
      X86_64.ADD_imm (X86_64.RCX, 1l);
      X86_64.JMP "__dark_spawn_command_copy";
      X86_64.Label "__dark_spawn_command_copied";
      X86_64.MOV_reg (X86_64.R11, X86_64.RBX);
      X86_64.ADD_reg (X86_64.R11, X86_64.R10);
      X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
      X86_64.MOV_store_byte (X86_64.R11, 0l, X86_64.RAX);
      X86_64.ADD_imm (X86_64.R10, 8l);
      X86_64.AND_imm (X86_64.R10, -8l);
      X86_64.ADD_reg (heapPtr, X86_64.R10);
(*  Recover inherited envp from the root frame. *)
      X86_64.MOV_reg (X86_64.RAX, X86_64.RBP);
      X86_64.Label "__dark_spawn_find_root";
      X86_64.MOV_load (X86_64.RCX, X86_64.RAX, 0l);
      X86_64.CMP_imm (X86_64.RCX, 0l);
      X86_64.Jcc (X86_64.EQ, "__dark_spawn_root_found");
      X86_64.MOV_reg (X86_64.RAX, X86_64.RCX);
      X86_64.JMP "__dark_spawn_find_root";
      X86_64.Label "__dark_spawn_root_found";
      X86_64.MOV_load (X86_64.RCX, X86_64.RAX, 8l);
      X86_64.SHL_imm (X86_64.RCX, 3);
      X86_64.ADD_reg (X86_64.RAX, X86_64.RCX);
      X86_64.ADD_imm (X86_64.RAX, 24l);
      X86_64.MOV_reg (X86_64.R12, X86_64.RAX);
      X86_64.LEA (X86_64.RDI, X86_64.RSP, 0l);
      X86_64.XOR_reg (X86_64.RSI, X86_64.RSI) ]
    @ syscall 293L
    @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.LT, "__dark_spawn_failed");
        X86_64.LEA (X86_64.RDI, X86_64.RSP, 8l);
        X86_64.XOR_reg (X86_64.RSI, X86_64.RSI) ]
    @ syscall 293L
    @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.LT, "__dark_spawn_failed_close_stdin");
        X86_64.LEA (X86_64.RDI, X86_64.RSP, 16l);
        X86_64.XOR_reg (X86_64.RSI, X86_64.RSI) ]
    @ syscall 293L
    @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.LT, "__dark_spawn_failed_close_output") ]
    @ syscall 57L
    @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.EQ, "__dark_spawn_child");
        X86_64.Jcc (X86_64.LT, "__dark_spawn_failed_close_all");
        X86_64.MOV_store (X86_64.RSP, 32l, X86_64.RAX) ]
    @ closeFd 0l false @ closeFd 8l true @ closeFd 16l true
    @ setNonblocking 8l @ setNonblocking 16l
    @ [ X86_64.MOV_imm32 (X86_64.RAX, 1l);
        X86_64.Label "__dark_spawn_find_slot";
        X86_64.CMP_imm (X86_64.RAX, 63l);
        X86_64.Jcc (X86_64.GT, "__dark_spawn_no_slot");
        X86_64.MOV_reg (X86_64.R10, X86_64.RAX);
        X86_64.SHL_imm (X86_64.R10, 6);
        X86_64.ADD_reg (X86_64.R10, X86_64.R13);
        X86_64.MOV_load (X86_64.R11, X86_64.R10, 0l);
        X86_64.CMP_imm (X86_64.R11, 0l);
        X86_64.Jcc (X86_64.EQ, "__dark_spawn_slot_found");
        X86_64.ADD_imm (X86_64.RAX, 1l);
        X86_64.JMP "__dark_spawn_find_slot";
        X86_64.Label "__dark_spawn_slot_found";
        X86_64.MOV_store (X86_64.RSP, 72l, X86_64.RAX);
        X86_64.MOV_imm32 (X86_64.R11, 1l);
        X86_64.MOV_store (X86_64.R10, 0l, X86_64.R11);
        X86_64.MOV_load (X86_64.R11, X86_64.RSP, 32l);
        X86_64.MOV_store (X86_64.R10, 8l, X86_64.R11);
        X86_64.MOV_load (X86_64.R11, X86_64.RSP, 0l);
        X86_64.SHR_imm (X86_64.R11, 32);
        X86_64.MOV_store (X86_64.R10, 16l, X86_64.R11);
        X86_64.MOV_load (X86_64.R11, X86_64.RSP, 8l);
        X86_64.MOV_reg32 (X86_64.R11, X86_64.R11);
        X86_64.MOV_store (X86_64.R10, 24l, X86_64.R11);
        X86_64.MOV_load (X86_64.R11, X86_64.RSP, 16l);
        X86_64.MOV_reg32 (X86_64.R11, X86_64.R11);
        X86_64.MOV_store (X86_64.R10, 32l, X86_64.R11);
        X86_64.MOV_reg (X86_64.R11, heapPtr);
        X86_64.MOV_store (X86_64.R10, 48l, X86_64.R11);
        X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
        X86_64.MOV_store (X86_64.R11, 0l, X86_64.RAX) ]
    @ loadImm64 X86_64.R11 1048584L
    @ [ X86_64.ADD_reg (heapPtr, X86_64.R11);
        X86_64.MOV_reg (X86_64.R11, heapPtr);
        X86_64.MOV_store (X86_64.R10, 56l, X86_64.R11);
        X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
        X86_64.MOV_store (X86_64.R11, 0l, X86_64.RAX) ]
    @ loadImm64 X86_64.R11 1048584L
    @ [ X86_64.ADD_reg (heapPtr, X86_64.R11);
        X86_64.MOV_load (X86_64.RAX, X86_64.RSP, 72l);
        X86_64.JMP "__dark_spawn_return";
        X86_64.Label "__dark_spawn_no_slot";
        X86_64.MOV_load (X86_64.RDI, X86_64.RSP, 32l);
        X86_64.MOV_imm32 (X86_64.RSI, 9l) ]
    @ syscall 62L
    @ loadImm64 X86_64.RAX (-1L)
    @ [ X86_64.JMP "__dark_spawn_return";
        X86_64.Label "__dark_spawn_failed_close_all" ]
    @ closeFd 16l false @ closeFd 16l true
    @ [ X86_64.Label "__dark_spawn_failed_close_output" ]
    @ closeFd 8l false @ closeFd 8l true
    @ [ X86_64.Label "__dark_spawn_failed_close_stdin" ]
    @ closeFd 0l false @ closeFd 0l true
    @ [ X86_64.Label "__dark_spawn_failed" ]
    @ loadImm64 X86_64.RAX (-1L)
    @ [ X86_64.Label "__dark_spawn_return";
        X86_64.ADD_imm (X86_64.RSP, 80l);
        X86_64.POP X86_64.RDX;
        X86_64.POP X86_64.R10;
        X86_64.POP X86_64.R9;
        X86_64.POP X86_64.R8;
        X86_64.POP X86_64.RCX;
        X86_64.POP X86_64.RSI;
        X86_64.POP X86_64.RDI;
        X86_64.POP X86_64.R13;
        X86_64.POP X86_64.R12;
        X86_64.POP X86_64.RBX;
        X86_64.POP X86_64.RBP;
        X86_64.RET;
        X86_64.Label "__dark_spawn_child" ]
    @ loadFd 0l false @ loadImm64 X86_64.RSI 0L @ syscall 33L
    @ loadFd 8l true @ loadImm64 X86_64.RSI 1L @ syscall 33L
    @ loadFd 16l true @ loadImm64 X86_64.RSI 2L @ syscall 33L
    @ closeFd 0l false @ closeFd 0l true @ closeFd 8l false @ closeFd 8l true @ closeFd 16l false @ closeFd 16l true
    @ emitStringLiteral X86_64.RDI "/bin/bash"
    @ emitStringLiteral X86_64.R11 "-c"
    @ [ X86_64.ADD_imm (X86_64.RDI, 16l);
        X86_64.ADD_imm (X86_64.R11, 16l);
        X86_64.MOV_store (X86_64.RSP, 40l, X86_64.RDI);
        X86_64.MOV_store (X86_64.RSP, 48l, X86_64.R11);
        X86_64.MOV_store (X86_64.RSP, 56l, X86_64.RBX);
        X86_64.XOR_reg (X86_64.R11, X86_64.R11);
        X86_64.MOV_store (X86_64.RSP, 64l, X86_64.R11);
        X86_64.LEA (X86_64.RSI, X86_64.RSP, 40l);
        X86_64.MOV_reg (X86_64.RDX, X86_64.R12) ]
    @ syscall 59L
    @ loadImm64 X86_64.RDI 127L
    @ syscall 60L

(*  Communicate with process-table children and collect their final output. *)
let generateLinuxCliProcessLifecycleHelpers (enableLeakCheck:bool) =
    let syscall number = loadImm64 X86_64.RAX number @ [X86_64.SYSCALL]
    in
    let leakInc count =
        if enableLeakCheck then
            [ X86_64.LEA_rip (X86_64.R11, "_leak_count");
              X86_64.MOV_load (X86_64.R10, X86_64.R11, 0l);
              X86_64.ADD_imm (X86_64.R10, count);
              X86_64.MOV_store (X86_64.R11, 0l, X86_64.R10) ]
        else []
    in
    let epilogue =
        [ X86_64.ADD_imm (X86_64.RSP, 80l);
          X86_64.POP X86_64.RDX; X86_64.POP X86_64.R10; X86_64.POP X86_64.R9;
          X86_64.POP X86_64.R8; X86_64.POP X86_64.RCX; X86_64.POP X86_64.RSI;
          X86_64.POP X86_64.RDI; X86_64.POP X86_64.R13; X86_64.POP X86_64.R12;
          X86_64.POP X86_64.RBX; X86_64.POP X86_64.RBP; X86_64.RET ]
    in
    let prologue label =
        [ X86_64.Label label;
          X86_64.PUSH X86_64.RBP; X86_64.MOV_reg (X86_64.RBP, X86_64.RSP);
          X86_64.PUSH X86_64.RBX; X86_64.PUSH X86_64.R12; X86_64.PUSH X86_64.R13;
          X86_64.PUSH X86_64.RDI; X86_64.PUSH X86_64.RSI; X86_64.PUSH X86_64.RCX;
          X86_64.PUSH X86_64.R8; X86_64.PUSH X86_64.R9; X86_64.PUSH X86_64.R10;
          X86_64.PUSH X86_64.RDX; X86_64.SUB_imm (X86_64.RSP, 80l) ]
    in
    let invalidOutcome returnLabel =
        emitStringLiteral X86_64.R8 ""
        @ emitStringLiteral X86_64.R9 "Process not found"
        @ [ X86_64.MOV_reg (X86_64.RAX, heapPtr); X86_64.ADD_imm (heapPtr, 32l) ]
        @ loadImm64 X86_64.R10 (-1L)
        @ [ X86_64.MOV_store (X86_64.RAX, 0l, X86_64.R10);
            X86_64.MOV_store (X86_64.RAX, 8l, X86_64.R8);
            X86_64.MOV_store (X86_64.RAX, 16l, X86_64.R9);
            X86_64.MOV_imm32 (X86_64.R10, 1l);
            X86_64.MOV_store (X86_64.RAX, 24l, X86_64.R10) ]
        @ leakInc 1l
        @ [X86_64.JMP returnLabel]
    in
    let copySuffix rawBuffer startOffset managedBuffer lengthOffset prefix =
        [ X86_64.MOV_load (X86_64.R10, rawBuffer, 0l);
          X86_64.MOV_load (X86_64.R11, X86_64.RSP, startOffset);
          X86_64.SUB_reg (X86_64.R10, X86_64.R11);
          X86_64.MOV_store (X86_64.RSP, lengthOffset, X86_64.R10);
          X86_64.XOR_reg (X86_64.RCX, X86_64.RCX);
          X86_64.Label ("__dark_process_io_copy_"^prefix);
          X86_64.CMP_reg (X86_64.RCX, X86_64.R10);
          X86_64.Jcc (X86_64.GE, ("__dark_process_io_"^prefix^"_copied"));
          X86_64.LEA (X86_64.RDX, rawBuffer, 8l);
          X86_64.ADD_reg (X86_64.RDX, X86_64.R11);
          X86_64.ADD_reg (X86_64.RDX, X86_64.RCX);
          X86_64.MOV_load_byte (X86_64.RAX, X86_64.RDX, 0l);
          X86_64.LEA (X86_64.RDX, managedBuffer, 16l);
          X86_64.ADD_reg (X86_64.RDX, X86_64.RCX);
          X86_64.MOV_store_byte (X86_64.RDX, 0l, X86_64.RAX);
          X86_64.ADD_imm (X86_64.RCX, 1l);
          X86_64.JMP ("__dark_process_io_copy_"^prefix);
          X86_64.Label ("__dark_process_io_"^prefix^"_copied") ]
    in
    let communicate =
        prologue "__dark_cli_process_io"
        @ [ X86_64.MOV_store (X86_64.RSP, 72l, X86_64.RSI);
            X86_64.MOV_store (X86_64.RSP, 24l, X86_64.RDX);
            X86_64.CMP_imm (X86_64.RDI, 1l);
            X86_64.Jcc (X86_64.LT, "__dark_process_io_invalid");
            X86_64.CMP_imm (X86_64.RDI, 63l);
            X86_64.Jcc (X86_64.GT, "__dark_process_io_invalid");
            X86_64.LEA (X86_64.RBX, freeListBase, Int32.of_int processTableOffset);
            X86_64.SHL_imm (X86_64.RDI, 6);
            X86_64.ADD_reg (X86_64.RBX, X86_64.RDI);
            X86_64.MOV_load (X86_64.RAX, X86_64.RBX, 0l);
            X86_64.CMP_imm (X86_64.RAX, 0l);
            X86_64.Jcc (X86_64.EQ, "__dark_process_io_invalid");
            X86_64.MOV_load (X86_64.R12, X86_64.RBX, 48l);
            X86_64.MOV_load (X86_64.R13, X86_64.RBX, 56l);
            X86_64.MOV_load (X86_64.RAX, X86_64.R12, 0l);
            X86_64.MOV_store (X86_64.RSP, 0l, X86_64.RAX);
            X86_64.MOV_load (X86_64.RAX, X86_64.R13, 0l);
            X86_64.MOV_store (X86_64.RSP, 8l, X86_64.RAX);
            X86_64.MOV_load (X86_64.RAX, X86_64.RSP, 24l);
            X86_64.CMP_imm (X86_64.RAX, 0l);
            X86_64.Jcc (X86_64.EQ, "__dark_process_io_suffix_ready");
            X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
            X86_64.MOV_store (X86_64.RSP, 0l, X86_64.RAX);
            X86_64.MOV_store (X86_64.RSP, 8l, X86_64.RAX);
            X86_64.Label "__dark_process_io_suffix_ready";
            X86_64.MOV_load (X86_64.RSI, X86_64.RSP, 72l);
            X86_64.MOV_load (X86_64.RDX, X86_64.RSI, 8l);
            X86_64.MOV_store (X86_64.RSP, 16l, X86_64.RDX);
            X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
            X86_64.MOV_store (X86_64.RSP, 64l, X86_64.RAX);
            X86_64.CMP_imm (X86_64.RDX, 0l);
            X86_64.Jcc (X86_64.EQ, "__dark_process_io_read_stdout");
            X86_64.MOV_load (X86_64.RDI, X86_64.RBX, 16l);
            X86_64.ADD_imm (X86_64.RSI, 16l) ]
        @ syscall 1L
        @ [ X86_64.MOV_imm32 (X86_64.RAX, 10l);
            X86_64.MOV_store_byte (X86_64.RSP, 64l, X86_64.RAX);
            X86_64.MOV_load (X86_64.RDI, X86_64.RBX, 16l);
            X86_64.LEA (X86_64.RSI, X86_64.RSP, 64l);
            X86_64.MOV_imm32 (X86_64.RDX, 1l) ]
        @ syscall 1L
        @ [ X86_64.XOR_reg (X86_64.RCX, X86_64.RCX);
            X86_64.MOV_store (X86_64.RSP, 40l, X86_64.RCX);
            X86_64.MOV_store (X86_64.RSP, 64l, X86_64.RCX);
            X86_64.Label "__dark_process_io_read_stdout";
            X86_64.MOV_load (X86_64.R10, X86_64.R12, 0l);
            X86_64.LEA (X86_64.RSI, X86_64.R12, 8l);
            X86_64.ADD_reg (X86_64.RSI, X86_64.R10) ]
        @ loadImm64 X86_64.RDX 1048576L
        @ [ X86_64.SUB_reg (X86_64.RDX, X86_64.R10);
            X86_64.CMP_imm (X86_64.RDX, 0l);
            X86_64.Jcc (X86_64.EQ, "__dark_process_io_read_stderr");
            X86_64.MOV_load (X86_64.RDI, X86_64.RBX, 24l) ]
        @ syscall 0L
        @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
            X86_64.Jcc (X86_64.LE, "__dark_process_io_read_stderr");
            X86_64.ADD_reg (X86_64.R10, X86_64.RAX);
            X86_64.MOV_store (X86_64.R12, 0l, X86_64.R10);
            X86_64.JMP "__dark_process_io_read_stdout";
            X86_64.Label "__dark_process_io_read_stderr";
            X86_64.MOV_load (X86_64.R10, X86_64.R13, 0l);
            X86_64.LEA (X86_64.RSI, X86_64.R13, 8l);
            X86_64.ADD_reg (X86_64.RSI, X86_64.R10) ]
        @ loadImm64 X86_64.RDX 1048576L
        @ [ X86_64.SUB_reg (X86_64.RDX, X86_64.R10);
            X86_64.CMP_imm (X86_64.RDX, 0l);
            X86_64.Jcc (X86_64.EQ, "__dark_process_io_status");
            X86_64.MOV_load (X86_64.RDI, X86_64.RBX, 32l) ]
        @ syscall 0L
        @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
            X86_64.Jcc (X86_64.LE, "__dark_process_io_status");
            X86_64.ADD_reg (X86_64.R10, X86_64.RAX);
            X86_64.MOV_store (X86_64.R13, 0l, X86_64.R10);
            X86_64.JMP "__dark_process_io_read_stderr";
            X86_64.Label "__dark_process_io_status";
            X86_64.MOV_load (X86_64.RAX, X86_64.RBX, 0l);
            X86_64.CMP_imm (X86_64.RAX, 2l);
            X86_64.Jcc (X86_64.EQ, "__dark_process_io_stored_status");
            X86_64.MOV_load (X86_64.RDI, X86_64.RBX, 8l);
            X86_64.LEA (X86_64.RSI, X86_64.RSP, 32l);
            X86_64.MOV_imm32 (X86_64.RDX, 1l);
            X86_64.XOR_reg (X86_64.R10, X86_64.R10);
(*  wait4 writes an int; clear the upper half before a 64-bit load. *)
            X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
            X86_64.MOV_store (X86_64.RSP, 32l, X86_64.RAX) ]
        @ syscall 61L
        @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
            X86_64.Jcc (X86_64.LT, "__dark_process_io_read_stdout");
            X86_64.Jcc (X86_64.NE, "__dark_process_io_finished");
            X86_64.MOV_load (X86_64.RAX, X86_64.RSP, 16l);
            X86_64.CMP_imm (X86_64.RAX, 0l);
            X86_64.Jcc (X86_64.EQ, "__dark_process_io_running");
            X86_64.MOV_load (X86_64.R10, X86_64.R12, 0l);
            X86_64.MOV_load (X86_64.R11, X86_64.R13, 0l);
            X86_64.ADD_reg (X86_64.R10, X86_64.R11);
            X86_64.MOV_load (X86_64.R11, X86_64.RSP, 0l);
            X86_64.SUB_reg (X86_64.R10, X86_64.R11);
            X86_64.MOV_load (X86_64.R11, X86_64.RSP, 8l);
            X86_64.SUB_reg (X86_64.R10, X86_64.R11);
            X86_64.MOV_load (X86_64.R11, X86_64.RSP, 64l);
            X86_64.CMP_reg (X86_64.R10, X86_64.R11);
            X86_64.Jcc (X86_64.NE, "__dark_process_io_response_changed");
            X86_64.CMP_imm (X86_64.R10, 0l);
            X86_64.Jcc (X86_64.NE, "__dark_process_io_running");
            X86_64.JMP "__dark_process_io_poll";
            X86_64.Label "__dark_process_io_response_changed";
            X86_64.MOV_store (X86_64.RSP, 64l, X86_64.R10);
            X86_64.XOR_reg (X86_64.R10, X86_64.R10);
            X86_64.MOV_store (X86_64.RSP, 40l, X86_64.R10);
            X86_64.Label "__dark_process_io_poll";
            X86_64.MOV_load (X86_64.R10, X86_64.RSP, 40l);
            X86_64.ADD_imm (X86_64.R10, 1l);
            X86_64.MOV_store (X86_64.RSP, 40l, X86_64.R10);
            X86_64.CMP_imm (X86_64.R10, 10l);
            X86_64.Jcc (X86_64.GE, "__dark_process_io_running");
            X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
            X86_64.MOV_store (X86_64.RSP, 48l, X86_64.RAX) ]
        @ loadImm64 X86_64.RAX 100000000L
        @ [ X86_64.MOV_store (X86_64.RSP, 56l, X86_64.RAX);
            X86_64.LEA (X86_64.RDI, X86_64.RSP, 48l);
            X86_64.XOR_reg (X86_64.RSI, X86_64.RSI) ]
        @ syscall 35L
        @ [ X86_64.JMP "__dark_process_io_read_stdout";
            X86_64.Label "__dark_process_io_finished";
            X86_64.MOV_load (X86_64.R10, X86_64.RSP, 32l);
            X86_64.MOV_reg32 (X86_64.R10, X86_64.R10);
            X86_64.MOV_reg (X86_64.R11, X86_64.R10);
            X86_64.AND_imm (X86_64.R11, 0x7fl);
            X86_64.CMP_imm (X86_64.R11, 0l);
            X86_64.Jcc (X86_64.NE, "__dark_process_io_signaled");
            X86_64.SHR_imm (X86_64.R10, 8);
            X86_64.JMP "__dark_process_io_store_status";
            X86_64.Label "__dark_process_io_signaled";
            X86_64.MOV_reg (X86_64.R10, X86_64.R11);
            X86_64.ADD_imm (X86_64.R10, 128l);
            X86_64.Label "__dark_process_io_store_status";
            X86_64.MOV_imm32 (X86_64.RAX, 2l);
            X86_64.MOV_store (X86_64.RBX, 0l, X86_64.RAX);
            X86_64.MOV_store (X86_64.RBX, 40l, X86_64.R10);
            X86_64.JMP "__dark_process_io_build";
            X86_64.Label "__dark_process_io_stored_status";
            X86_64.MOV_load (X86_64.R10, X86_64.RBX, 40l);
            X86_64.JMP "__dark_process_io_build";
            X86_64.Label "__dark_process_io_running";
            X86_64.XOR_reg (X86_64.R10, X86_64.R10);
            X86_64.Label "__dark_process_io_build";
            X86_64.MOV_store (X86_64.RSP, 64l, X86_64.R10);
            X86_64.MOV_reg (X86_64.R8, heapPtr) ]
        @ loadImm64 X86_64.R11 1048592L
        @ [ X86_64.ADD_reg (heapPtr, X86_64.R11);
            X86_64.MOV_reg (X86_64.R9, heapPtr) ]
        @ loadImm64 X86_64.R11 1048592L
        @ [ X86_64.ADD_reg (heapPtr, X86_64.R11) ]
        @ copySuffix X86_64.R12 0l X86_64.R8 0l "stdout"
        @ copySuffix X86_64.R13 8l X86_64.R9 8l "stderr"
        @ [ X86_64.MOV_imm32 (X86_64.R11, 1l);
            X86_64.MOV_store (X86_64.R8, 0l, X86_64.R11);
            X86_64.MOV_load (X86_64.R10, X86_64.RSP, 0l);
            X86_64.MOV_store (X86_64.R8, 8l, X86_64.R10);
            X86_64.MOV_store (X86_64.R9, 0l, X86_64.R11);
            X86_64.MOV_load (X86_64.R10, X86_64.RSP, 8l);
            X86_64.MOV_store (X86_64.R9, 8l, X86_64.R10);
            X86_64.MOV_reg (X86_64.RAX, heapPtr);
            X86_64.ADD_imm (heapPtr, 32l);
            X86_64.MOV_load (X86_64.R10, X86_64.RSP, 64l);
            X86_64.MOV_store (X86_64.RAX, 0l, X86_64.R10);
            X86_64.MOV_store (X86_64.RAX, 8l, X86_64.R8);
            X86_64.MOV_store (X86_64.RAX, 16l, X86_64.R9);
            X86_64.MOV_store (X86_64.RAX, 24l, X86_64.R11) ]
        @ leakInc 3l
        @ [ X86_64.JMP "__dark_process_io_return";
            X86_64.Label "__dark_process_io_invalid" ]
        @ invalidOutcome "__dark_process_io_return"
        @ [X86_64.Label "__dark_process_io_return"]
        @ epilogue
    in
    let terminate =
        prologue "__dark_cli_terminate_process"
        @ [ X86_64.CMP_imm (X86_64.RDI, 1l);
            X86_64.Jcc (X86_64.LT, "__dark_terminate_invalid");
            X86_64.CMP_imm (X86_64.RDI, 63l);
            X86_64.Jcc (X86_64.GT, "__dark_terminate_invalid");
            X86_64.LEA (X86_64.RBX, freeListBase, Int32.of_int processTableOffset);
            X86_64.MOV_reg (X86_64.R10, X86_64.RDI);
            X86_64.SHL_imm (X86_64.R10, 6);
            X86_64.ADD_reg (X86_64.RBX, X86_64.R10);
            X86_64.MOV_load (X86_64.RAX, X86_64.RBX, 0l);
            X86_64.CMP_imm (X86_64.RAX, 0l);
            X86_64.Jcc (X86_64.EQ, "__dark_terminate_invalid");
            X86_64.CMP_imm (X86_64.RAX, 2l);
            X86_64.Jcc (X86_64.EQ, "__dark_terminate_collect");
            X86_64.MOV_load (X86_64.RDI, X86_64.RBX, 8l);
            X86_64.MOV_imm32 (X86_64.RSI, 15l) ]
        @ syscall 62L
        @ [ X86_64.MOV_load (X86_64.RDI, X86_64.RBX, 8l);
            X86_64.LEA (X86_64.RSI, X86_64.RSP, 32l);
            X86_64.XOR_reg (X86_64.RDX, X86_64.RDX);
            X86_64.XOR_reg (X86_64.R10, X86_64.R10);
            X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
            X86_64.MOV_store (X86_64.RSP, 32l, X86_64.RAX) ]
        @ syscall 61L
        @ [ X86_64.MOV_load (X86_64.R10, X86_64.RSP, 32l);
            X86_64.MOV_reg32 (X86_64.R10, X86_64.R10);
            X86_64.MOV_reg (X86_64.R11, X86_64.R10);
            X86_64.AND_imm (X86_64.R11, 0x7fl);
            X86_64.CMP_imm (X86_64.R11, 0l);
            X86_64.Jcc (X86_64.NE, "__dark_terminate_signaled");
            X86_64.SHR_imm (X86_64.R10, 8);
            X86_64.JMP "__dark_terminate_store";
            X86_64.Label "__dark_terminate_signaled";
            X86_64.MOV_reg (X86_64.R10, X86_64.R11);
            X86_64.ADD_imm (X86_64.R10, 128l);
            X86_64.Label "__dark_terminate_store";
            X86_64.MOV_store (X86_64.RBX, 40l, X86_64.R10);
            X86_64.MOV_imm32 (X86_64.RAX, 2l);
            X86_64.MOV_store (X86_64.RBX, 0l, X86_64.RAX);
            X86_64.Label "__dark_terminate_collect";
            X86_64.MOV_reg (X86_64.RDI, X86_64.RBX);
            X86_64.LEA (X86_64.R10, freeListBase, Int32.of_int processTableOffset);
            X86_64.SUB_reg (X86_64.RDI, X86_64.R10);
            X86_64.SHR_imm (X86_64.RDI, 6) ]
        @ emitStringLiteral X86_64.RSI ""
        @ [ X86_64.MOV_imm32 (X86_64.RDX, 1l);
            X86_64.CALL "__dark_cli_process_io";
            X86_64.MOV_store (X86_64.RSP, 64l, X86_64.RAX);
            X86_64.MOV_load (X86_64.RDI, X86_64.RBX, 16l) ]
        @ syscall 3L
        @ [X86_64.MOV_load (X86_64.RDI, X86_64.RBX, 24l)] @ syscall 3L
        @ [X86_64.MOV_load (X86_64.RDI, X86_64.RBX, 32l)] @ syscall 3L
        @ [ X86_64.XOR_reg (X86_64.R10, X86_64.R10);
            X86_64.MOV_store (X86_64.RBX, 0l, X86_64.R10);
            X86_64.MOV_load (X86_64.RAX, X86_64.RSP, 64l);
            X86_64.JMP "__dark_terminate_return";
            X86_64.Label "__dark_terminate_invalid" ]
        @ invalidOutcome "__dark_terminate_return"
        @ [X86_64.Label "__dark_terminate_return"]
        @ epilogue
    in
    let cleanup =
        [ X86_64.Label "__dark_cli_cleanup_processes";
          X86_64.PUSH X86_64.RBX;
          X86_64.PUSH X86_64.R12;
          X86_64.SUB_imm (X86_64.RSP, 16l);
          X86_64.MOV_imm32 (X86_64.R12, 1l);
          X86_64.Label "__dark_cleanup_process_next";
          X86_64.CMP_imm (X86_64.R12, 63l);
          X86_64.Jcc (X86_64.GT, "__dark_cleanup_process_done");
          X86_64.LEA (X86_64.RBX, freeListBase, Int32.of_int processTableOffset);
          X86_64.MOV_reg (X86_64.R10, X86_64.R12);
          X86_64.SHL_imm (X86_64.R10, 6);
          X86_64.ADD_reg (X86_64.RBX, X86_64.R10);
          X86_64.MOV_load (X86_64.RAX, X86_64.RBX, 0l);
          X86_64.CMP_imm (X86_64.RAX, 0l);
          X86_64.Jcc (X86_64.EQ, "__dark_cleanup_process_advance");
          X86_64.MOV_load (X86_64.RDI, X86_64.RBX, 8l);
          X86_64.MOV_imm32 (X86_64.RSI, 9l) ]
        @ syscall 62L
        @ [ X86_64.MOV_load (X86_64.RDI, X86_64.RBX, 8l);
            X86_64.MOV_reg (X86_64.RSI, X86_64.RSP);
            X86_64.XOR_reg (X86_64.RDX, X86_64.RDX);
            X86_64.XOR_reg (X86_64.R10, X86_64.R10) ]
        @ syscall 61L
        @ [X86_64.MOV_load (X86_64.RDI, X86_64.RBX, 16l)] @ syscall 3L
        @ [X86_64.MOV_load (X86_64.RDI, X86_64.RBX, 24l)] @ syscall 3L
        @ [X86_64.MOV_load (X86_64.RDI, X86_64.RBX, 32l)] @ syscall 3L
        @ [ X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
            X86_64.MOV_store (X86_64.RBX, 0l, X86_64.RAX);
            X86_64.Label "__dark_cleanup_process_advance";
            X86_64.ADD_imm (X86_64.R12, 1l);
            X86_64.JMP "__dark_cleanup_process_next";
            X86_64.Label "__dark_cleanup_process_done";
            X86_64.ADD_imm (X86_64.RSP, 16l);
            X86_64.POP X86_64.R12;
            X86_64.POP X86_64.RBX;
            X86_64.RET ]
    in
    communicate @ terminate @ cleanup

(*  Execute a packed argv request directly through Linux process syscalls. The *)
(*  request layout is shared with the ARM64 backend and keeps shell policy in *)
(*  the Dark standard library rather than reinterpreting arguments here. *)
let generateLinuxCliRunProcessHelper (enableLeakCheck:bool) =
    let syscall number = loadImm64 X86_64.RAX number @ [X86_64.SYSCALL]
    in
    let loadFd stackOffset highHalf =
        [ X86_64.MOV_load (X86_64.RDI, X86_64.RSP, stackOffset) ]
        @ (if highHalf then [X86_64.SHR_imm (X86_64.RDI, 32)]
           else [X86_64.MOV_reg32 (X86_64.RDI, X86_64.RDI)])
    in
    let closeFd stackOffset highHalf = loadFd stackOffset highHalf @ syscall 3L
    in
    let setNonblocking stackOffset =
        loadFd stackOffset false
        @ loadImm64 X86_64.RSI 4L
        @ loadImm64 X86_64.RDX 2048L
        @ syscall 72L
    in
    let readPipe stackOffset buffer lengthOffset nextLabel =
        loadFd stackOffset false
        @ [ X86_64.LEA (X86_64.RSI, buffer, 16l);
            X86_64.MOV_load (X86_64.R10, X86_64.RSP, lengthOffset);
            X86_64.ADD_reg (X86_64.RSI, X86_64.R10) ]
        @ loadImm64 X86_64.RDX 1048576L
        @ [ X86_64.SUB_reg (X86_64.RDX, X86_64.R10) ]
        @ syscall 0L
        @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
            X86_64.Jcc (X86_64.LE, nextLabel);
            X86_64.ADD_reg (X86_64.R10, X86_64.RAX);
            X86_64.MOV_store (X86_64.RSP, lengthOffset, X86_64.R10) ]
    in
    let finalizeString buffer lengthOffset =
        [ X86_64.MOV_imm32 (X86_64.R11, 1l);
          X86_64.MOV_store (buffer, 0l, X86_64.R11);
          X86_64.MOV_load (X86_64.R10, X86_64.RSP, lengthOffset);
          X86_64.MOV_store (buffer, 8l, X86_64.R10) ]
    in
    let leakInc =
        if enableLeakCheck then
            [ X86_64.LEA_rip (X86_64.R11, "_leak_count");
              X86_64.MOV_load (X86_64.R10, X86_64.R11, 0l);
              X86_64.ADD_imm (X86_64.R10, 3l);
              X86_64.MOV_store (X86_64.R11, 0l, X86_64.R10) ]
        else
            []
    in
    let copyManagedString managedReg bufferReg lengthReg prefix =
        let loopLabel = ("__dark_run_"^prefix^"_copy")
        in
        let doneLabel = ("__dark_run_"^prefix^"_copied")
        in
        [ X86_64.MOV_load (lengthReg, managedReg, 8l);
          X86_64.MOV_reg (bufferReg, heapPtr);
          X86_64.LEA (X86_64.RDX, managedReg, 16l);
          X86_64.XOR_reg (X86_64.RCX, X86_64.RCX);
          X86_64.Label loopLabel;
          X86_64.CMP_reg (X86_64.RCX, lengthReg);
          X86_64.Jcc (X86_64.GE, doneLabel);
          X86_64.MOV_load_byte (X86_64.RAX, X86_64.RDX, 0l);
          X86_64.MOV_reg (X86_64.R11, bufferReg);
          X86_64.ADD_reg (X86_64.R11, X86_64.RCX);
          X86_64.MOV_store_byte (X86_64.R11, 0l, X86_64.RAX);
          X86_64.ADD_imm (X86_64.RDX, 1l);
          X86_64.ADD_imm (X86_64.RCX, 1l);
          X86_64.JMP loopLabel;
          X86_64.Label doneLabel;
          X86_64.MOV_reg (X86_64.R11, bufferReg);
          X86_64.ADD_reg (X86_64.R11, lengthReg);
          X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
          X86_64.MOV_store_byte (X86_64.R11, 0l, X86_64.RAX);
          X86_64.MOV_reg (X86_64.R11, lengthReg);
          X86_64.ADD_imm (X86_64.R11, 8l);
          X86_64.AND_imm (X86_64.R11, -8l);
          X86_64.ADD_reg (heapPtr, X86_64.R11) ]
    in
    let buildPointerVector bufferReg lengthReg vectorReg prefix =
        let loopLabel = ("__dark_run_"^prefix^"_scan")
        in
        let nextLabel = ("__dark_run_"^prefix^"_next")
        in
        let doneLabel = ("__dark_run_"^prefix^"_done")
        in
        [ X86_64.MOV_reg (vectorReg, heapPtr);
          X86_64.MOV_reg (X86_64.R11, lengthReg);
          X86_64.ADD_imm (X86_64.R11, 2l);
          X86_64.SHL_imm (X86_64.R11, 3);
          X86_64.ADD_reg (heapPtr, X86_64.R11);
          X86_64.XOR_reg (X86_64.RCX, X86_64.RCX);
          X86_64.XOR_reg (X86_64.RDX, X86_64.RDX);
          X86_64.MOV_store (vectorReg, 0l, bufferReg);
          X86_64.Label loopLabel;
          X86_64.CMP_reg (X86_64.RCX, lengthReg);
          X86_64.Jcc (X86_64.GE, doneLabel);
          X86_64.MOV_reg (X86_64.R11, bufferReg);
          X86_64.ADD_reg (X86_64.R11, X86_64.RCX);
          X86_64.MOV_load_byte (X86_64.RAX, X86_64.R11, 0l);
          X86_64.CMP_imm (X86_64.RAX, 0l);
          X86_64.Jcc (X86_64.NE, nextLabel);
          X86_64.ADD_imm (X86_64.RDX, 8l);
          X86_64.MOV_reg (X86_64.R11, bufferReg);
          X86_64.ADD_reg (X86_64.R11, X86_64.RCX);
          X86_64.ADD_imm (X86_64.R11, 1l);
          X86_64.MOV_reg (X86_64.RAX, vectorReg);
          X86_64.ADD_reg (X86_64.RAX, X86_64.RDX);
          X86_64.MOV_store (X86_64.RAX, 0l, X86_64.R11);
          X86_64.Label nextLabel;
          X86_64.ADD_imm (X86_64.RCX, 1l);
          X86_64.JMP loopLabel;
          X86_64.Label doneLabel;
          X86_64.ADD_imm (X86_64.RDX, 8l);
          X86_64.MOV_reg (X86_64.R11, vectorReg);
          X86_64.ADD_reg (X86_64.R11, X86_64.RDX);
          X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
          X86_64.MOV_store (X86_64.R11, 0l, X86_64.RAX) ]

    in
    [ X86_64.Label "__dark_cli_run_process";
      X86_64.PUSH X86_64.RBP;
      X86_64.MOV_reg (X86_64.RBP, X86_64.RSP);
      X86_64.PUSH X86_64.RBX;
      X86_64.PUSH X86_64.R12;
      X86_64.PUSH X86_64.R13;
      X86_64.PUSH X86_64.R15;
(*  CliNative has no explicit SaveRegs/RestoreRegs pair in LIR. *)
      X86_64.PUSH X86_64.RDI;
      X86_64.PUSH X86_64.RSI;
      X86_64.PUSH X86_64.RCX;
      X86_64.PUSH X86_64.R8;
      X86_64.PUSH X86_64.R9;
      X86_64.PUSH X86_64.R10;
      X86_64.PUSH X86_64.RDX;
      X86_64.SUB_imm (X86_64.RSP, 160l);
      X86_64.MOV_reg (X86_64.R15, X86_64.RDI);
(*  Remember the scratch boundary so an ENOENT candidate can be retried *)
(*  without retaining its argv, environment, cwd, and capture buffers. *)
      X86_64.MOV_store (X86_64.RSP, 80l, heapPtr);
      X86_64.MOV_load (X86_64.RAX, X86_64.R15, 8l) ]
    @ copyManagedString X86_64.RAX X86_64.RBX X86_64.R10 "argv"
    @ buildPointerVector X86_64.RBX X86_64.R10 X86_64.R12 "argv"
    @ [ X86_64.MOV_load (X86_64.RAX, X86_64.R15, 0l);
        X86_64.CMP_imm (X86_64.RAX, 4l);
        X86_64.Jcc (X86_64.NE, "__dark_run_pipeline_argv_prepared");
        X86_64.MOV_load (X86_64.RAX, X86_64.R15, 40l) ]
    @ copyManagedString X86_64.RAX X86_64.R8 X86_64.R10 "pipeline_argv"
    @ buildPointerVector X86_64.R8 X86_64.R10 X86_64.R9 "pipeline_argv"
    @ [ X86_64.MOV_store (X86_64.RSP, 88l, X86_64.R9);
        X86_64.Label "__dark_run_pipeline_argv_prepared";
        X86_64.MOV_reg (X86_64.RAX, X86_64.RBP);
        X86_64.Label "__dark_run_find_root";
        X86_64.MOV_load (X86_64.RCX, X86_64.RAX, 0l);
        X86_64.CMP_imm (X86_64.RCX, 0l);
        X86_64.Jcc (X86_64.EQ, "__dark_run_root_found");
        X86_64.MOV_reg (X86_64.RAX, X86_64.RCX);
        X86_64.JMP "__dark_run_find_root";
        X86_64.Label "__dark_run_root_found";
        X86_64.MOV_load (X86_64.RCX, X86_64.RAX, 8l);
        X86_64.SHL_imm (X86_64.RCX, 3);
        X86_64.ADD_reg (X86_64.RAX, X86_64.RCX);
        X86_64.ADD_imm (X86_64.RAX, 24l);
        X86_64.MOV_reg (X86_64.R13, X86_64.RAX);
        X86_64.MOV_store (X86_64.RSP, 64l, X86_64.R13);
        X86_64.MOV_load (X86_64.RAX, X86_64.R15, 24l);
        X86_64.MOV_load (X86_64.R10, X86_64.RAX, 8l);
        X86_64.CMP_imm (X86_64.R10, 0l);
        X86_64.Jcc (X86_64.EQ, "__dark_run_environment_done") ]
    @ copyManagedString X86_64.RAX X86_64.RBX X86_64.R10 "environment"
    @ [ X86_64.MOV_load (X86_64.R11, X86_64.RSP, 64l);
        X86_64.XOR_reg (X86_64.RCX, X86_64.RCX);
        X86_64.Label "__dark_run_environment_count";
        X86_64.MOV_load (X86_64.RAX, X86_64.R11, 0l);
        X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.EQ, "__dark_run_environment_counted");
        X86_64.ADD_imm (X86_64.RCX, 1l);
        X86_64.ADD_imm (X86_64.R11, 8l);
        X86_64.JMP "__dark_run_environment_count";
        X86_64.Label "__dark_run_environment_counted";
        X86_64.MOV_reg (X86_64.R13, heapPtr);
        X86_64.MOV_reg (X86_64.R11, X86_64.R10);
        X86_64.ADD_reg (X86_64.R11, X86_64.RCX);
        X86_64.ADD_imm (X86_64.R11, 2l);
        X86_64.SHL_imm (X86_64.R11, 3);
        X86_64.ADD_reg (heapPtr, X86_64.R11);
        X86_64.XOR_reg (X86_64.RCX, X86_64.RCX);
        X86_64.XOR_reg (X86_64.RDX, X86_64.RDX);
        X86_64.MOV_store (X86_64.R13, 0l, X86_64.RBX);
        X86_64.Label "__dark_run_environment_scan";
        X86_64.CMP_reg (X86_64.RCX, X86_64.R10);
        X86_64.Jcc (X86_64.GE, "__dark_run_environment_append_inherited");
        X86_64.MOV_reg (X86_64.R11, X86_64.RBX);
        X86_64.ADD_reg (X86_64.R11, X86_64.RCX);
        X86_64.MOV_load_byte (X86_64.RAX, X86_64.R11, 0l);
        X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.NE, "__dark_run_environment_next");
        X86_64.ADD_imm (X86_64.RDX, 8l);
        X86_64.ADD_imm (X86_64.R11, 1l);
        X86_64.MOV_reg (X86_64.RAX, X86_64.R13);
        X86_64.ADD_reg (X86_64.RAX, X86_64.RDX);
        X86_64.MOV_store (X86_64.RAX, 0l, X86_64.R11);
        X86_64.Label "__dark_run_environment_next";
        X86_64.ADD_imm (X86_64.RCX, 1l);
        X86_64.JMP "__dark_run_environment_scan";
        X86_64.Label "__dark_run_environment_append_inherited";
        X86_64.ADD_imm (X86_64.RDX, 8l);
        X86_64.MOV_load (X86_64.R11, X86_64.RSP, 64l);
        X86_64.Label "__dark_run_environment_append_next";
        X86_64.MOV_load (X86_64.RAX, X86_64.R11, 0l);
        X86_64.MOV_reg (X86_64.RCX, X86_64.R13);
        X86_64.ADD_reg (X86_64.RCX, X86_64.RDX);
        X86_64.MOV_store (X86_64.RCX, 0l, X86_64.RAX);
        X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.EQ, "__dark_run_environment_done");
        X86_64.ADD_imm (X86_64.RDX, 8l);
        X86_64.ADD_imm (X86_64.R11, 8l);
        X86_64.JMP "__dark_run_environment_append_next";
        X86_64.Label "__dark_run_environment_done";
        X86_64.MOV_load (X86_64.RAX, X86_64.R15, 16l) ]
    @ copyManagedString X86_64.RAX X86_64.RBX X86_64.R10 "cwd"
    @ [ X86_64.MOV_store (X86_64.RSP, 72l, X86_64.RBX);
        X86_64.MOV_reg (X86_64.R8, heapPtr) ]
    @ loadImm64 X86_64.R10 1048592L
    @ [ X86_64.ADD_reg (heapPtr, X86_64.R10);
        X86_64.MOV_reg (X86_64.R9, heapPtr);
        X86_64.ADD_reg (heapPtr, X86_64.R10);
        X86_64.XOR_reg (X86_64.R10, X86_64.R10);
        X86_64.MOV_store (X86_64.RSP, 24l, X86_64.R10);
        X86_64.MOV_store (X86_64.RSP, 40l, X86_64.R10);
        X86_64.MOV_store (X86_64.RSP, 48l, X86_64.R10);
        X86_64.MOV_store (X86_64.RSP, 56l, X86_64.R10);
        X86_64.MOV_store (X86_64.RSP, 104l, X86_64.R10);
        X86_64.MOV_store (X86_64.RSP, 120l, X86_64.R10);
        X86_64.MOV_store (X86_64.RSP, 128l, X86_64.R10);
        X86_64.LEA (X86_64.RDI, X86_64.RSP, 0l);
        X86_64.XOR_reg (X86_64.RSI, X86_64.RSI) ]
    @ syscall 293L
    @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.LT, "__dark_run_spawn_error");
        X86_64.LEA (X86_64.RDI, X86_64.RSP, 8l);
        X86_64.XOR_reg (X86_64.RSI, X86_64.RSI) ]
    @ syscall 293L
    @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.LT, "__dark_run_spawn_error_close_stdout");
        X86_64.LEA (X86_64.RDI, X86_64.RSP, 16l) ]
    @ loadImm64 X86_64.RSI 524288L
    @ syscall 293L
    @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.LT, "__dark_run_spawn_error_close_output");
        X86_64.LEA (X86_64.RDI, X86_64.RSP, 96l);
        X86_64.XOR_reg (X86_64.RSI, X86_64.RSI) ]
    @ syscall 293L
    @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.LT, "__dark_run_spawn_error_close_errno");
        X86_64.MOV_load (X86_64.RAX, X86_64.R15, 0l);
        X86_64.CMP_imm (X86_64.RAX, 4l);
        X86_64.Jcc (X86_64.NE, "__dark_run_fork_consumer") ]
    @ syscall 57L
    @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.EQ, "__dark_run_pipeline_producer");
        X86_64.Jcc (X86_64.LT, "__dark_run_spawn_error_close_pipeline");
        X86_64.MOV_store (X86_64.RSP, 104l, X86_64.RAX);
        X86_64.Label "__dark_run_fork_consumer" ]
    @ syscall 57L
    @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.EQ, "__dark_run_child");
        X86_64.Jcc (X86_64.LT, "__dark_run_spawn_error_close_all");
        X86_64.MOV_store (X86_64.RSP, 32l, X86_64.RAX) ]
    @ closeFd 0l true @ closeFd 8l true @ closeFd 16l true @ closeFd 96l false @ closeFd 96l true
    @ setNonblocking 0l @ setNonblocking 8l
    @ [ X86_64.Label "__dark_run_drain_wait" ]
    @ readPipe 0l X86_64.R8 120l "__dark_run_read_stderr"
    @ [ X86_64.Label "__dark_run_read_stderr" ]
    @ readPipe 8l X86_64.R9 128l "__dark_run_wait"
    @ [ X86_64.Label "__dark_run_wait";
        X86_64.MOV_load (X86_64.RDI, X86_64.RSP, 32l);
        X86_64.LEA (X86_64.RSI, X86_64.RSP, 24l);
        X86_64.MOV_imm32 (X86_64.RDX, 1l);
        X86_64.XOR_reg (X86_64.R10, X86_64.R10) ]
    @ syscall 61L
    @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.NE, "__dark_run_finished");
        X86_64.MOV_load (X86_64.RAX, X86_64.R15, 0l);
        X86_64.CMP_imm (X86_64.RAX, 3l);
        X86_64.Jcc (X86_64.NE, "__dark_run_sleep");
        X86_64.MOV_load (X86_64.R10, X86_64.RSP, 40l);
        X86_64.MOV_load (X86_64.R11, X86_64.R15, 32l);
        X86_64.CMP_reg (X86_64.R10, X86_64.R11);
        X86_64.Jcc (X86_64.LT, "__dark_run_timeout_next");
        X86_64.MOV_load (X86_64.RDI, X86_64.RSP, 32l);
        X86_64.MOV_imm32 (X86_64.RSI, 9l) ]
    @ syscall 62L
    @ [ X86_64.MOV_imm32 (X86_64.R10, 1l);
        X86_64.MOV_store (X86_64.RSP, 48l, X86_64.R10);
        X86_64.JMP "__dark_run_blocking_wait";
        X86_64.Label "__dark_run_timeout_next";
        X86_64.ADD_imm (X86_64.R10, 1l);
        X86_64.MOV_store (X86_64.RSP, 40l, X86_64.R10);
        X86_64.Label "__dark_run_sleep";
        X86_64.XOR_reg (X86_64.R10, X86_64.R10);
        X86_64.MOV_store (X86_64.RSP, 136l, X86_64.R10) ]
    @ loadImm64 X86_64.R10 1000000L
    @ [ X86_64.MOV_store (X86_64.RSP, 144l, X86_64.R10);
        X86_64.LEA (X86_64.RDI, X86_64.RSP, 136l);
        X86_64.XOR_reg (X86_64.RSI, X86_64.RSI) ]
    @ syscall 35L
    @ [ X86_64.JMP "__dark_run_drain_wait";
        X86_64.Label "__dark_run_blocking_wait";
        X86_64.MOV_load (X86_64.RDI, X86_64.RSP, 32l);
        X86_64.LEA (X86_64.RSI, X86_64.RSP, 24l);
        X86_64.XOR_reg (X86_64.RDX, X86_64.RDX);
        X86_64.XOR_reg (X86_64.R10, X86_64.R10) ]
    @ syscall 61L
    @ [ X86_64.Label "__dark_run_finished";
        X86_64.MOV_load (X86_64.RAX, X86_64.R15, 0l);
        X86_64.CMP_imm (X86_64.RAX, 4l);
        X86_64.Jcc (X86_64.NE, "__dark_run_all_children_finished");
        X86_64.MOV_load (X86_64.RDI, X86_64.RSP, 104l);
        X86_64.LEA (X86_64.RSI, X86_64.RSP, 112l);
        X86_64.XOR_reg (X86_64.RDX, X86_64.RDX);
        X86_64.XOR_reg (X86_64.R10, X86_64.R10) ]
    @ syscall 61L
    @ [ X86_64.Label "__dark_run_all_children_finished" ]
    @ readPipe 0l X86_64.R8 120l "__dark_run_final_stderr"
    @ [ X86_64.Label "__dark_run_final_stderr" ]
    @ readPipe 8l X86_64.R9 128l "__dark_run_final_close"
    @ [ X86_64.Label "__dark_run_final_close" ]
    @ closeFd 0l false @ closeFd 8l false
    @ loadFd 16l false
    @ [ X86_64.LEA (X86_64.RSI, X86_64.RSP, 56l);
        X86_64.MOV_imm32 (X86_64.RDX, 8l) ]
    @ syscall 0L
    @ closeFd 16l false
    @ [ X86_64.MOV_load (X86_64.R10, X86_64.RSP, 24l);
        X86_64.MOV_reg (X86_64.R11, X86_64.R10);
        X86_64.AND_imm (X86_64.R11, 0x7fl);
        X86_64.CMP_imm (X86_64.R11, 0l);
        X86_64.Jcc (X86_64.NE, "__dark_run_signaled");
        X86_64.SHR_imm (X86_64.R10, 8);
        X86_64.JMP "__dark_run_build_result";
        X86_64.Label "__dark_run_signaled";
        X86_64.MOV_reg (X86_64.R10, X86_64.R11);
        X86_64.ADD_imm (X86_64.R10, 128l);
        X86_64.JMP "__dark_run_build_result";
        X86_64.Label "__dark_run_spawn_error_close_all" ]
    @ closeFd 96l false @ closeFd 96l true
    @ [ X86_64.Label "__dark_run_spawn_error_close_pipeline";
        X86_64.Label "__dark_run_spawn_error_close_errno" ]
    @ closeFd 16l false @ closeFd 16l true
    @ [ X86_64.Label "__dark_run_spawn_error_close_output" ]
    @ closeFd 8l false @ closeFd 8l true
    @ [ X86_64.Label "__dark_run_spawn_error_close_stdout" ]
    @ closeFd 0l false @ closeFd 0l true
    @ [ X86_64.Label "__dark_run_spawn_error" ]
    @ loadImm64 X86_64.R10 127L
    @ [ X86_64.Label "__dark_run_build_result";
        X86_64.MOV_load (X86_64.R11, X86_64.RSP, 56l);
        X86_64.CMP_imm (X86_64.R11, 2l);
        X86_64.Jcc (X86_64.NE, "__dark_run_keep_capture_buffers");
        X86_64.MOV_load (heapPtr, X86_64.RSP, 80l);
        X86_64.MOV_reg (X86_64.R8, heapPtr);
        X86_64.ADD_imm (heapPtr, 16l);
        X86_64.MOV_reg (X86_64.R9, heapPtr);
        X86_64.ADD_imm (heapPtr, 16l);
        X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
        X86_64.MOV_store (X86_64.RSP, 120l, X86_64.RAX);
        X86_64.MOV_store (X86_64.RSP, 128l, X86_64.RAX);
        X86_64.Label "__dark_run_keep_capture_buffers";
        X86_64.MOV_store (X86_64.RSP, 152l, X86_64.R10) ]
    @ finalizeString X86_64.R8 120l
    @ finalizeString X86_64.R9 128l
    @ [ X86_64.MOV_reg (X86_64.RAX, heapPtr);
        X86_64.ADD_imm (heapPtr, 48l);
        X86_64.MOV_load (X86_64.R11, X86_64.RSP, 56l);
        X86_64.MOV_store (X86_64.RAX, 0l, X86_64.R11);
        X86_64.MOV_load (X86_64.R10, X86_64.RSP, 152l);
        X86_64.MOV_store (X86_64.RAX, 8l, X86_64.R10);
        X86_64.MOV_store (X86_64.RAX, 16l, X86_64.R8);
        X86_64.MOV_store (X86_64.RAX, 24l, X86_64.R9);
        X86_64.MOV_load (X86_64.R10, X86_64.RSP, 48l);
        X86_64.MOV_store (X86_64.RAX, 32l, X86_64.R10);
        X86_64.MOV_imm32 (X86_64.R10, 1l);
        X86_64.MOV_store (X86_64.RAX, 40l, X86_64.R10) ]
    @ leakInc
    @ [ X86_64.ADD_imm (X86_64.RSP, 160l);
        X86_64.POP X86_64.RDX;
        X86_64.POP X86_64.R10;
        X86_64.POP X86_64.R9;
        X86_64.POP X86_64.R8;
        X86_64.POP X86_64.RCX;
        X86_64.POP X86_64.RSI;
        X86_64.POP X86_64.RDI;
        X86_64.POP X86_64.R15;
        X86_64.POP X86_64.R13;
        X86_64.POP X86_64.R12;
        X86_64.POP X86_64.RBX;
        X86_64.POP X86_64.RBP;
        X86_64.RET;
        X86_64.Label "__dark_run_child" ]
    @ loadFd 0l true
    @ loadImm64 X86_64.RSI 1L
    @ syscall 33L
    @ loadFd 8l true
    @ loadImm64 X86_64.RSI 2L
    @ syscall 33L
    @ [ X86_64.MOV_load (X86_64.RAX, X86_64.R15, 0l);
        X86_64.CMP_imm (X86_64.RAX, 4l);
        X86_64.Jcc (X86_64.NE, "__dark_run_child_input_ready") ]
    @ loadFd 96l false
    @ loadImm64 X86_64.RSI 0L
    @ syscall 33L
    @ [ X86_64.Label "__dark_run_child_input_ready" ]
    @ closeFd 0l false @ closeFd 0l true @ closeFd 8l false @ closeFd 8l true @ closeFd 16l false @ closeFd 96l false @ closeFd 96l true
    @ [ X86_64.MOV_load (X86_64.RAX, X86_64.R15, 0l);
        X86_64.CMP_imm (X86_64.RAX, 1l);
        X86_64.Jcc (X86_64.NE, "__dark_run_child_exec");
        X86_64.MOV_load (X86_64.RDI, X86_64.RSP, 72l) ]
    @ syscall 80L
    @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.LT, "__dark_run_child_fail");
        X86_64.Label "__dark_run_child_exec";
        X86_64.MOV_load (X86_64.RAX, X86_64.R15, 0l);
        X86_64.CMP_imm (X86_64.RAX, 4l);
        X86_64.Jcc (X86_64.NE, "__dark_run_child_first_argv");
        X86_64.MOV_load (X86_64.RSI, X86_64.RSP, 88l);
        X86_64.MOV_load (X86_64.RDI, X86_64.RSI, 0l);
        X86_64.JMP "__dark_run_child_argv_ready";
        X86_64.Label "__dark_run_child_first_argv";
        X86_64.MOV_load (X86_64.RDI, X86_64.R12, 0l);
        X86_64.MOV_reg (X86_64.RSI, X86_64.R12);
        X86_64.Label "__dark_run_child_argv_ready";
        X86_64.MOV_reg (X86_64.RDX, X86_64.R13) ]
    @ syscall 59L
    @ [ X86_64.Label "__dark_run_child_fail";
        X86_64.NEG X86_64.RAX;
        X86_64.MOV_store (X86_64.RSP, 56l, X86_64.RAX) ]
    @ loadFd 16l true
    @ [ X86_64.LEA (X86_64.RSI, X86_64.RSP, 56l);
        X86_64.MOV_imm32 (X86_64.RDX, 8l) ]
    @ syscall 1L
    @ loadImm64 X86_64.RDI 127L
    @ syscall 60L
    @ [ X86_64.Label "__dark_run_pipeline_producer" ]
    @ loadFd 96l true
    @ loadImm64 X86_64.RSI 1L
    @ syscall 33L
    @ loadFd 8l true
    @ loadImm64 X86_64.RSI 2L
    @ syscall 33L
    @ closeFd 0l false @ closeFd 0l true @ closeFd 8l false @ closeFd 8l true @ closeFd 16l false @ closeFd 96l false @ closeFd 96l true
    @ [ X86_64.MOV_load (X86_64.RDI, X86_64.R12, 0l);
        X86_64.MOV_reg (X86_64.RSI, X86_64.R12);
        X86_64.MOV_reg (X86_64.RDX, X86_64.R13) ]
    @ syscall 59L
    @ [ X86_64.JMP "__dark_run_child_fail" ]

(*  Execute a managed command through /bin/bash and return NativeOutput. The *)
(*  helper drains stdout and stderr concurrently so a child cannot block on a *)
(*  full pipe. R14 remains the managed heap pointer across the native syscalls. *)
let generateLinuxCliExecuteHelper (enableLeakCheck:bool) =
    let label = "__dark_cli_execute"
    in
    let syscall number = loadImm64 X86_64.RAX number @ [X86_64.SYSCALL]
    in
    let loadFd stackOffset highHalf =
        [ X86_64.MOV_load (X86_64.RDI, X86_64.RSP, stackOffset) ]
        @ (if highHalf then [X86_64.SHR_imm (X86_64.RDI, 32)]
           else [X86_64.MOV_reg32 (X86_64.RDI, X86_64.RDI)])
    in
    let closeFd stackOffset highHalf = loadFd stackOffset highHalf @ syscall 3L
    in
    let setNonblocking stackOffset =
        loadFd stackOffset false
        @ loadImm64 X86_64.RSI 4L
        @ loadImm64 X86_64.RDX 2048L
        @ syscall 72L
    in
    let readPipe stackOffset buffer lengthOffset nextLabel =
        loadFd stackOffset false
        @ [ X86_64.LEA (X86_64.RSI, buffer, 16l);
            X86_64.MOV_load (X86_64.R10, X86_64.RSP, lengthOffset);
            X86_64.ADD_reg (X86_64.RSI, X86_64.R10) ]
        @ loadImm64 X86_64.RDX 1048576L
        @ [ X86_64.SUB_reg (X86_64.RDX, X86_64.R10) ]
        @ syscall 0L
        @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
            X86_64.Jcc (X86_64.LE, nextLabel);
            X86_64.ADD_reg (X86_64.R10, X86_64.RAX);
            X86_64.MOV_store (X86_64.RSP, lengthOffset, X86_64.R10) ]
    in
    let finalizeString buffer lengthOffset =
        [ X86_64.MOV_imm32 (X86_64.R11, 1l);
          X86_64.MOV_store (buffer, 0l, X86_64.R11);
          X86_64.MOV_load (X86_64.R10, X86_64.RSP, lengthOffset);
          X86_64.MOV_store (buffer, 8l, X86_64.R10) ]
    in
    let leakInc =
        if enableLeakCheck then
            [ X86_64.LEA_rip (X86_64.R11, "_leak_count");
              X86_64.MOV_load (X86_64.R10, X86_64.R11, 0l);
              X86_64.ADD_imm (X86_64.R10, 3l);
              X86_64.MOV_store (X86_64.R11, 0l, X86_64.R10) ]
        else
            []

    in
    [ X86_64.Label label;
      X86_64.PUSH X86_64.RBP;
      X86_64.MOV_reg (X86_64.RBP, X86_64.RSP);
      X86_64.PUSH X86_64.RBX;
      X86_64.PUSH X86_64.R12;
      X86_64.PUSH X86_64.R13;
(*  CliNative has no explicit SaveRegs/RestoreRegs pair in LIR. Preserve *)
(*  every allocatable x64 caller-saved register across this hidden call. *)
      X86_64.PUSH X86_64.RDI;
      X86_64.PUSH X86_64.RSI;
      X86_64.PUSH X86_64.RCX;
      X86_64.PUSH X86_64.R8;
      X86_64.PUSH X86_64.R9;
      X86_64.PUSH X86_64.R10;
      X86_64.PUSH X86_64.RDX;
      X86_64.SUB_imm (X86_64.RSP, 112l);
(*  Copy [refcount][length][bytes] to a native NUL-terminated command buffer. *)
      X86_64.MOV_reg (X86_64.RBX, heapPtr);
      X86_64.MOV_load (X86_64.RCX, X86_64.RDI, 8l);
      X86_64.LEA (X86_64.RSI, X86_64.RDI, 16l);
      X86_64.XOR_reg (X86_64.R8, X86_64.R8);
      X86_64.Label "__dark_cli_command_copy";
      X86_64.CMP_reg (X86_64.R8, X86_64.RCX);
      X86_64.Jcc (X86_64.GE, "__dark_cli_command_copied");
      X86_64.MOV_load_byte (X86_64.R9, X86_64.RSI, 0l);
      X86_64.MOV_reg (X86_64.R10, X86_64.RBX);
      X86_64.ADD_reg (X86_64.R10, X86_64.R8);
      X86_64.MOV_store_byte (X86_64.R10, 0l, X86_64.R9);
      X86_64.ADD_imm (X86_64.RSI, 1l);
      X86_64.ADD_imm (X86_64.R8, 1l);
      X86_64.JMP "__dark_cli_command_copy";
      X86_64.Label "__dark_cli_command_copied";
      X86_64.MOV_reg (X86_64.R10, X86_64.RBX);
      X86_64.ADD_reg (X86_64.R10, X86_64.RCX);
      X86_64.XOR_reg (X86_64.R9, X86_64.R9);
      X86_64.MOV_store_byte (X86_64.R10, 0l, X86_64.R9);
      X86_64.ADD_imm (X86_64.RCX, 8l);
      X86_64.AND_imm (X86_64.RCX, -8l);
      X86_64.ADD_reg (heapPtr, X86_64.RCX);
(*  Reserve bounded managed buffers for captured stdout and stderr. *)
      X86_64.MOV_reg (X86_64.R12, heapPtr) ]
    @ loadImm64 X86_64.R10 1048592L
    @ [ X86_64.ADD_reg (heapPtr, X86_64.R10);
        X86_64.MOV_reg (X86_64.R13, heapPtr);
        X86_64.ADD_reg (heapPtr, X86_64.R10);
        X86_64.XOR_reg (X86_64.R10, X86_64.R10);
        X86_64.MOV_store (X86_64.RSP, 32l, X86_64.R10);
        X86_64.MOV_store (X86_64.RSP, 40l, X86_64.R10);
(*  pipe2(stdout), pipe2(stderr) *)
        X86_64.LEA (X86_64.RDI, X86_64.RSP, 0l);
        X86_64.XOR_reg (X86_64.RSI, X86_64.RSI) ]
    @ syscall 293L
    @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.LT, "__dark_cli_spawn_error");
        X86_64.LEA (X86_64.RDI, X86_64.RSP, 8l);
        X86_64.XOR_reg (X86_64.RSI, X86_64.RSI) ]
    @ syscall 293L
    @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.LT, "__dark_cli_spawn_error_close_stdout") ]
    @ syscall 57L
    @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.EQ, "__dark_cli_child");
        X86_64.Jcc (X86_64.LT, "__dark_cli_spawn_error_close_all");
        X86_64.MOV_store (X86_64.RSP, 24l, X86_64.RAX);
        X86_64.XOR_reg (X86_64.R10, X86_64.R10);
        X86_64.MOV_store (X86_64.RSP, 16l, X86_64.R10) ]
    @ closeFd 0l true
    @ closeFd 8l true
    @ setNonblocking 0l
    @ setNonblocking 8l
    @ [ X86_64.Label "__dark_cli_drain_wait" ]
    @ readPipe 0l X86_64.R12 32l "__dark_cli_read_stderr"
    @ [ X86_64.Label "__dark_cli_read_stderr" ]
    @ readPipe 8l X86_64.R13 40l "__dark_cli_wait"
    @ [ X86_64.Label "__dark_cli_wait";
        X86_64.MOV_load (X86_64.RDI, X86_64.RSP, 24l);
        X86_64.LEA (X86_64.RSI, X86_64.RSP, 16l);
        X86_64.MOV_imm32 (X86_64.RDX, 1l);
        X86_64.XOR_reg (X86_64.R10, X86_64.R10) ]
    @ syscall 61L
    @ [ X86_64.CMP_imm (X86_64.RAX, 0l);
        X86_64.Jcc (X86_64.LT, "__dark_cli_drain_wait");
        X86_64.Jcc (X86_64.NE, "__dark_cli_finished");
        X86_64.XOR_reg (X86_64.R10, X86_64.R10);
        X86_64.MOV_store (X86_64.RSP, 48l, X86_64.R10) ]
    @ loadImm64 X86_64.R10 1000000L
    @ [ X86_64.MOV_store (X86_64.RSP, 56l, X86_64.R10);
        X86_64.LEA (X86_64.RDI, X86_64.RSP, 48l);
        X86_64.XOR_reg (X86_64.RSI, X86_64.RSI) ]
    @ syscall 35L
    @ [ X86_64.JMP "__dark_cli_drain_wait";
        X86_64.Label "__dark_cli_finished" ]
    @ readPipe 0l X86_64.R12 32l "__dark_cli_final_stderr"
    @ [ X86_64.Label "__dark_cli_final_stderr" ]
    @ readPipe 8l X86_64.R13 40l "__dark_cli_final_close"
    @ [ X86_64.Label "__dark_cli_final_close" ]
    @ closeFd 0l false
    @ closeFd 8l false
    @ [ X86_64.MOV_load (X86_64.R10, X86_64.RSP, 16l);
        X86_64.MOV_reg (X86_64.R11, X86_64.R10);
        X86_64.AND_imm (X86_64.R11, 0x7fl);
        X86_64.CMP_imm (X86_64.R11, 0l);
        X86_64.Jcc (X86_64.NE, "__dark_cli_signaled");
        X86_64.SHR_imm (X86_64.R10, 8);
        X86_64.JMP "__dark_cli_build_result";
        X86_64.Label "__dark_cli_signaled";
        X86_64.MOV_reg (X86_64.R10, X86_64.R11);
        X86_64.ADD_imm (X86_64.R10, 128l);
        X86_64.JMP "__dark_cli_build_result";
        X86_64.Label "__dark_cli_spawn_error_close_all" ]
    @ closeFd 8l false
    @ closeFd 8l true
    @ [ X86_64.Label "__dark_cli_spawn_error_close_stdout" ]
    @ closeFd 0l false
    @ closeFd 0l true
    @ [ X86_64.Label "__dark_cli_spawn_error" ]
    @ loadImm64 X86_64.R10 127L
    @ [ X86_64.Label "__dark_cli_build_result";
        X86_64.MOV_store (X86_64.RSP, 104l, X86_64.R10) ]
    @ finalizeString X86_64.R12 32l
    @ finalizeString X86_64.R13 40l
    @ [ X86_64.MOV_load (X86_64.R10, X86_64.RSP, 104l);
        X86_64.MOV_reg (X86_64.RAX, heapPtr);
        X86_64.ADD_imm (heapPtr, 32l);
        X86_64.MOV_store (X86_64.RAX, 0l, X86_64.R10);
        X86_64.MOV_store (X86_64.RAX, 8l, X86_64.R12);
        X86_64.MOV_store (X86_64.RAX, 16l, X86_64.R13);
        X86_64.MOV_imm32 (X86_64.R11, 1l);
        X86_64.MOV_store (X86_64.RAX, 24l, X86_64.R11) ]
    @ leakInc
    @ [ X86_64.ADD_imm (X86_64.RSP, 112l);
        X86_64.POP X86_64.RDX;
        X86_64.POP X86_64.R10;
        X86_64.POP X86_64.R9;
        X86_64.POP X86_64.R8;
        X86_64.POP X86_64.RCX;
        X86_64.POP X86_64.RSI;
        X86_64.POP X86_64.RDI;
        X86_64.POP X86_64.R13;
        X86_64.POP X86_64.R12;
        X86_64.POP X86_64.RBX;
        X86_64.POP X86_64.RBP;
        X86_64.RET;
        X86_64.Label "__dark_cli_child" ]
    @ loadFd 0l true
    @ loadImm64 X86_64.RSI 1L
    @ syscall 33L
    @ loadFd 8l true
    @ loadImm64 X86_64.RSI 2L
    @ syscall 33L
    @ closeFd 0l false
    @ closeFd 0l true
    @ closeFd 8l false
    @ closeFd 8l true
    @ emitStringLiteral X86_64.RDI "/bin/bash"
    @ [ X86_64.ADD_imm (X86_64.RDI, 16l);
(*  Recover envp from _start's root frame. *)
        X86_64.MOV_reg (X86_64.RAX, X86_64.RBP);
        X86_64.Label "__dark_cli_find_root_for_exec";
        X86_64.MOV_load (X86_64.RCX, X86_64.RAX, 0l);
        X86_64.CMP_imm (X86_64.RCX, 0l);
        X86_64.Jcc (X86_64.EQ, "__dark_cli_exec_root_found");
        X86_64.MOV_reg (X86_64.RAX, X86_64.RCX);
        X86_64.JMP "__dark_cli_find_root_for_exec";
        X86_64.Label "__dark_cli_exec_root_found";
        X86_64.MOV_load (X86_64.RCX, X86_64.RAX, 8l);
        X86_64.SHL_imm (X86_64.RCX, 3);
        X86_64.ADD_reg (X86_64.RAX, X86_64.RCX);
        X86_64.ADD_imm (X86_64.RAX, 24l);
        X86_64.MOV_store (X86_64.RSP, 96l, X86_64.RAX);
        X86_64.MOV_store (X86_64.RSP, 64l, X86_64.RDI) ]
    @ emitStringLiteral X86_64.R10 "-c"
    @ [ X86_64.ADD_imm (X86_64.R10, 16l);
        X86_64.MOV_store (X86_64.RSP, 72l, X86_64.R10);
        X86_64.MOV_store (X86_64.RSP, 80l, X86_64.RBX);
        X86_64.XOR_reg (X86_64.R10, X86_64.R10);
        X86_64.MOV_store (X86_64.RSP, 88l, X86_64.R10);
        X86_64.LEA (X86_64.RSI, X86_64.RSP, 64l);
        X86_64.MOV_load (X86_64.RDX, X86_64.RSP, 96l) ]
    @ syscall 59L
    @ loadImm64 X86_64.RDI 127L
    @ syscall 60L
