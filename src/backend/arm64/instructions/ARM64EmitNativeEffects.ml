(*
   ARM64EmitNativeEffects.ml - Emit arm64 instructions for nativeeffects operations.
*)
[@@@warning "-4"]

let int16 value =
  let low = value land 65535 in
  if low >= 32768 then low - 65536 else low

let bind f value = Result.bind value f
let mul a b = Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))

open ARM64CodeGenTypes
open HeapAllocation
open LeakAccounting
open ARM64Operands

(* Darwin exposes absolute ticks through gettimeofday's third argument. Query
   the Mach timebase rather than assuming the ARM timer frequency. Keep all
   values across traps on the stack, and divide before multiplying to avoid
   overflowing the intermediate tick/timebase product. *)
let emitMacOSMonotonicTime (ctx : codeGenContext) destReg time =
  let prefix =
    Printf.sprintf "__monotonic_%s_%s" ctx.functionName ctx.instructionSite
  in
  let failure = prefix ^ "_failure" in
  let timebaseFailure = prefix ^ "_timebase_failure" in
  let doneLabel = prefix ^ "_done" in
  let syscall = ARM64.targetSyscalls ctx.target in
  loadCliOperand Symbolic.X0 time
  |> Result.map (fun loads ->
      loads
      @ [
          Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 32);
          Symbolic.STR (Symbolic.X0, Symbolic.SP, 0);
          Symbolic.ADD_imm (Symbolic.X2, Symbolic.SP, 8);
          Symbolic.MOVZ (Symbolic.X0, 0, 0);
          Symbolic.MOVZ (Symbolic.X1, 0, 0);
          Symbolic.MOVZ
            ( syscall.ARM64.syscallRegister,
              syscall.ARM64.numbers.Platform.gettimeofday,
              0 );
          Symbolic.SVC syscall.ARM64.svcImmediate;
          Symbolic.B_cond_label (Symbolic.HS, failure);
          Symbolic.ADD_imm (Symbolic.X0, Symbolic.SP, 16);
        ]
      @ loadImmediate syscall.ARM64.syscallRegister
          Platform.macOSMachTimebaseInfoTrap
      @ [
          Symbolic.SVC syscall.ARM64.svcImmediate;
          Symbolic.CBNZ (Symbolic.X0, timebaseFailure);
          Symbolic.LDR (Symbolic.X9, Symbolic.SP, 16);
          Symbolic.LSR_imm (Symbolic.X10, Symbolic.X9, 32);
        ]
      @ loadImmediate Symbolic.X11 4294967295L
      @ [
          Symbolic.AND_reg (Symbolic.X9, Symbolic.X9, Symbolic.X11);
          Symbolic.CBZ (Symbolic.X10, timebaseFailure);
          Symbolic.LDR (Symbolic.X11, Symbolic.SP, 8);
          Symbolic.UDIV (Symbolic.X12, Symbolic.X11, Symbolic.X10);
          Symbolic.MSUB (Symbolic.X11, Symbolic.X12, Symbolic.X10, Symbolic.X11);
          Symbolic.MUL (Symbolic.X12, Symbolic.X12, Symbolic.X9);
          Symbolic.MUL (Symbolic.X11, Symbolic.X11, Symbolic.X9);
          Symbolic.UDIV (Symbolic.X11, Symbolic.X11, Symbolic.X10);
          Symbolic.ADD_reg (Symbolic.X11, Symbolic.X12, Symbolic.X11);
        ]
      @ loadImmediate Symbolic.X10 1000000000L
      @ [
          Symbolic.UDIV (Symbolic.X12, Symbolic.X11, Symbolic.X10);
          Symbolic.MSUB (Symbolic.X11, Symbolic.X12, Symbolic.X10, Symbolic.X11);
          Symbolic.LDR (Symbolic.X9, Symbolic.SP, 0);
          Symbolic.STR (Symbolic.X12, Symbolic.X9, 0);
          Symbolic.STR (Symbolic.X11, Symbolic.X9, 8);
          Symbolic.MOVZ (Symbolic.X0, 0, 0);
          Symbolic.B_label doneLabel;
          Symbolic.Label timebaseFailure;
          (* Mach return codes are not errno values; expose a failed query as EIO. *)
          Symbolic.MOVZ (Symbolic.X0, 5, 0);
          Symbolic.Label failure;
          Symbolic.NEG (Symbolic.X0, Symbolic.X0);
          Symbolic.Label doneLabel;
          Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 32);
          Symbolic.MOV_reg (destReg, Symbolic.X0);
        ])

let emitManagedStringFromStack (ctx : codeGenContext) (labelPrefix : string)
    (destReg : Symbolic.reg) (stackOffset : int)
    (releaseStack : Symbolic.instr list) =
  let lengthLoop = Printf.sprintf "%s_length" labelPrefix in
  let lengthDone = Printf.sprintf "%s_length_done" labelPrefix in
  let copyLoop = Printf.sprintf "%s_copy" labelPrefix in
  let copyDone = Printf.sprintf "%s_copy_done" labelPrefix in
  [
    Symbolic.ADD_imm (Symbolic.X9, Symbolic.SP, stackOffset);
    Symbolic.MOVZ (Symbolic.X10, 0, 0);
    Symbolic.Label lengthLoop;
    Symbolic.LDRB (Symbolic.X11, Symbolic.X9, Symbolic.X10);
    Symbolic.CBZ (Symbolic.X11, lengthDone);
    Symbolic.ADD_imm (Symbolic.X10, Symbolic.X10, 1);
    Symbolic.B_label lengthLoop;
    Symbolic.Label lengthDone;
    Symbolic.MOV_reg (destReg, Symbolic.X28);
    Symbolic.MOVZ (Symbolic.X11, 1, 0);
    Symbolic.STR (Symbolic.X11, destReg, 0);
    Symbolic.STR (Symbolic.X10, destReg, 8);
    Symbolic.ADD_imm (Symbolic.X12, Symbolic.X10, 7);
    Symbolic.LSR_imm (Symbolic.X12, Symbolic.X12, 3);
    Symbolic.LSL_imm (Symbolic.X12, Symbolic.X12, 3);
    Symbolic.ADD_imm (Symbolic.X12, Symbolic.X12, 16);
    Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X12);
    Symbolic.ADD_imm (Symbolic.X11, destReg, 16);
    Symbolic.MOV_reg (Symbolic.X12, Symbolic.X10);
    Symbolic.Label copyLoop;
    Symbolic.CBZ (Symbolic.X12, copyDone);
    Symbolic.LDRB_imm (Symbolic.X13, Symbolic.X9, 0);
    Symbolic.STRB_reg (Symbolic.X13, Symbolic.X11);
    Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 1);
    Symbolic.ADD_imm (Symbolic.X11, Symbolic.X11, 1);
    Symbolic.SUB_imm (Symbolic.X12, Symbolic.X12, 1);
    Symbolic.B_label copyLoop;
    Symbolic.Label copyDone;
  ]
  @ releaseStack @ generateLeakCounterInc ctx

let emitDirectoryCurrent (ctx : codeGenContext) (destReg : Symbolic.reg) =
  let labelPrefix =
    Printf.sprintf "__cwd_%s_%s" ctx.functionName ctx.instructionSite
  in
  let failure = Printf.sprintf "%s_failure" labelPrefix in
  let complete = Printf.sprintf "%s_complete" labelPrefix in
  let syscalls = ARM64.targetSyscalls ctx.target in
  let syscallNumber =
    match ARM64.targetOS ctx.target with
    | Platform.Linux -> 17
    | Platform.MacOS -> 326
  in
  let failureCheck =
    match ARM64.targetOS ctx.target with
    | Platform.Linux ->
        [
          Symbolic.CMP_imm (Symbolic.X0, 0);
          Symbolic.B_cond_label (Symbolic.LT, failure);
        ]
    | Platform.MacOS -> [ Symbolic.B_cond_label (Symbolic.HS, failure) ]
  in
  [
    Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 2048);
    Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 2048);
    Symbolic.MOV_reg (Symbolic.X0, Symbolic.SP);
  ]
  @ loadImmediate Symbolic.X1 4096L
  @ [
      Symbolic.MOVZ (syscalls.ARM64.syscallRegister, syscallNumber, 0);
      Symbolic.SVC syscalls.ARM64.svcImmediate;
    ]
  @ failureCheck
  @ emitManagedStringFromStack ctx labelPrefix destReg 0
      [
        Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048);
        Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048);
        Symbolic.B_label complete;
      ]
  @ [
      Symbolic.Label failure;
      Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048);
      Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048);
    ]
  @ loadStringLiteralPointer destReg ""
  @ [ Symbolic.Label complete ]

let emitEnvironmentPacked (ctx : codeGenContext) (destReg : Symbolic.reg) =
  let labelPrefix =
    Printf.sprintf "__env_all_%s_%s" ctx.functionName ctx.instructionSite
  in
  let findRoot = Printf.sprintf "%s_find_root" labelPrefix in
  let rootFound = Printf.sprintf "%s_root_found" labelPrefix in
  let findArgvEnd = Printf.sprintf "%s_find_argv_end" labelPrefix in
  let countEntry = Printf.sprintf "%s_count_entry" labelPrefix in
  let countByte = Printf.sprintf "%s_count_byte" labelPrefix in
  let countNext = Printf.sprintf "%s_count_next" labelPrefix in
  let countDone = Printf.sprintf "%s_count_done" labelPrefix in
  let copyEntry = Printf.sprintf "%s_copy_entry" labelPrefix in
  let copyByte = Printf.sprintf "%s_copy_byte" labelPrefix in
  let copyNext = Printf.sprintf "%s_copy_next" labelPrefix in
  let copyDone = Printf.sprintf "%s_copy_done" labelPrefix in
  [
    Symbolic.MOV_reg (Symbolic.X9, Symbolic.X29);
    Symbolic.Label findRoot;
    Symbolic.LDR (Symbolic.X10, Symbolic.X9, 0);
    Symbolic.CBZ (Symbolic.X10, rootFound);
    Symbolic.MOV_reg (Symbolic.X9, Symbolic.X10);
    Symbolic.B_label findRoot;
    Symbolic.Label rootFound;
    Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 24);
    Symbolic.Label findArgvEnd;
    Symbolic.LDR (Symbolic.X10, Symbolic.X9, 0);
    Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 8);
    Symbolic.CBNZ (Symbolic.X10, findArgvEnd);
    Symbolic.MOV_reg (Symbolic.X14, Symbolic.X9);
    Symbolic.MOVZ (Symbolic.X11, 0, 0);
    Symbolic.Label countEntry;
    Symbolic.LDR (Symbolic.X10, Symbolic.X9, 0);
    Symbolic.CBZ (Symbolic.X10, countDone);
    Symbolic.Label countByte;
    Symbolic.LDRB_imm (Symbolic.X12, Symbolic.X10, 0);
    Symbolic.ADD_imm (Symbolic.X11, Symbolic.X11, 1);
    Symbolic.ADD_imm (Symbolic.X10, Symbolic.X10, 1);
    Symbolic.CBZ (Symbolic.X12, countNext);
    Symbolic.B_label countByte;
    Symbolic.Label countNext;
    Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 8);
    Symbolic.B_label countEntry;
    Symbolic.Label countDone;
    Symbolic.MOV_reg (destReg, Symbolic.X28);
    Symbolic.MOVZ (Symbolic.X12, 1, 0);
    Symbolic.STR (Symbolic.X12, destReg, 0);
    Symbolic.STR (Symbolic.X11, destReg, 8);
    Symbolic.ADD_imm (Symbolic.X12, Symbolic.X11, 7);
    Symbolic.LSR_imm (Symbolic.X12, Symbolic.X12, 3);
    Symbolic.LSL_imm (Symbolic.X12, Symbolic.X12, 3);
    Symbolic.ADD_imm (Symbolic.X12, Symbolic.X12, 16);
    Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X12);
    Symbolic.ADD_imm (Symbolic.X13, destReg, 16);
    Symbolic.MOV_reg (Symbolic.X9, Symbolic.X14);
    Symbolic.Label copyEntry;
    Symbolic.LDR (Symbolic.X10, Symbolic.X9, 0);
    Symbolic.CBZ (Symbolic.X10, copyDone);
    Symbolic.Label copyByte;
    Symbolic.LDRB_imm (Symbolic.X12, Symbolic.X10, 0);
    Symbolic.STRB_reg (Symbolic.X12, Symbolic.X13);
    Symbolic.ADD_imm (Symbolic.X10, Symbolic.X10, 1);
    Symbolic.ADD_imm (Symbolic.X13, Symbolic.X13, 1);
    Symbolic.CBZ (Symbolic.X12, copyNext);
    Symbolic.B_label copyByte;
    Symbolic.Label copyNext;
    Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 8);
    Symbolic.B_label copyEntry;
    Symbolic.Label copyDone;
  ]
  @ generateLeakCounterInc ctx

let emitDirectoryListPacked (ctx : codeGenContext) (destReg : Symbolic.reg)
    (pathLoads : Symbolic.instr list) =
  let prefix =
    Printf.sprintf "__list_dir_%s_%s" ctx.functionName ctx.instructionSite
  in
  let pathCopy = Printf.sprintf "%s_path_copy" prefix in
  let pathDone = Printf.sprintf "%s_path_done" prefix in
  let openFailed = Printf.sprintf "%s_open_failed" prefix in
  let readChunk = Printf.sprintf "%s_read_chunk" prefix in
  let readDone = Printf.sprintf "%s_read_done" prefix in
  let entryLoop = Printf.sprintf "%s_entry_loop" prefix in
  let entriesDone = Printf.sprintf "%s_entries_done" prefix in
  let skipEntry = Printf.sprintf "%s_skip_entry" prefix in
  let appendPath = Printf.sprintf "%s_append_path" prefix in
  let appendPathDone = Printf.sprintf "%s_append_path_done" prefix in
  let appendName = Printf.sprintf "%s_append_name" prefix in
  let appendNameDone = Printf.sprintf "%s_append_name_done" prefix in
  let noSlash = Printf.sprintf "%s_no_slash" prefix in
  let complete = Printf.sprintf "%s_complete" prefix in
  let syscalls = ARM64.targetSyscalls ctx.target in
  let os = ARM64.targetOS ctx.target in
  let openCall, openFailureCheck, readCall, readFailureCheck, nameOffset =
    match os with
    | Platform.Linux ->
        ( loadImmediate Symbolic.X0 (-100L)
          @ [ Symbolic.MOV_reg (Symbolic.X1, Symbolic.SP) ]
          @ loadImmediate Symbolic.X2 16384L
          @ [
              Symbolic.MOVZ (Symbolic.X3, 0, 0);
              Symbolic.MOVZ
                ( syscalls.ARM64.syscallRegister,
                  syscalls.ARM64.numbers.Platform.open_,
                  0 );
              Symbolic.SVC syscalls.ARM64.svcImmediate;
            ],
          [
            Symbolic.CMP_imm (Symbolic.X0, 0);
            Symbolic.B_cond_label (Symbolic.LT, openFailed);
          ],
          [
            Symbolic.MOV_reg (Symbolic.X0, Symbolic.X9);
            Symbolic.MOV_reg (Symbolic.X1, Symbolic.X5);
          ]
          @ loadImmediate Symbolic.X2 4096L
          @ [
              Symbolic.MOVZ (syscalls.ARM64.syscallRegister, 61, 0);
              Symbolic.SVC syscalls.ARM64.svcImmediate;
            ],
          [
            Symbolic.CMP_imm (Symbolic.X0, 0);
            Symbolic.B_cond_label (Symbolic.LE, readDone);
          ],
          19 )
    | Platform.MacOS ->
        ( [ Symbolic.MOV_reg (Symbolic.X0, Symbolic.SP) ]
          @ loadImmediate Symbolic.X1 0x100000L
          @ [
              Symbolic.MOVZ (Symbolic.X2, 0, 0);
              Symbolic.MOVZ
                ( syscalls.ARM64.syscallRegister,
                  syscalls.ARM64.numbers.Platform.open_,
                  0 );
              Symbolic.SVC syscalls.ARM64.svcImmediate;
            ],
          [ Symbolic.B_cond_label (Symbolic.HS, openFailed) ],
          [
            Symbolic.MOV_reg (Symbolic.X0, Symbolic.X9);
            Symbolic.MOV_reg (Symbolic.X1, Symbolic.X5);
          ]
          @ loadImmediate Symbolic.X2 4096L
          @ [
              Symbolic.ADD_imm (Symbolic.X3, Symbolic.SP, 4080);
              Symbolic.MOVZ (syscalls.ARM64.syscallRegister, 344, 0);
              Symbolic.SVC syscalls.ARM64.svcImmediate;
            ],
          [
            Symbolic.B_cond_label (Symbolic.HS, readDone);
            Symbolic.CBZ (Symbolic.X0, readDone);
          ],
          21 )
  in
  pathLoads
  @ [
      Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 2048);
      Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 2048);
      Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 2048);
      Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 2048);
      Symbolic.MOV_reg (Symbolic.X12, Symbolic.X0);
      Symbolic.LDR (Symbolic.X13, Symbolic.X12, 8);
      Symbolic.ADD_imm (Symbolic.X4, Symbolic.X12, 16);
      Symbolic.MOV_reg (Symbolic.X6, Symbolic.SP);
      Symbolic.MOV_reg (Symbolic.X7, Symbolic.X13);
      Symbolic.Label pathCopy;
      Symbolic.CBZ (Symbolic.X7, pathDone);
      Symbolic.LDRB_imm (Symbolic.X8, Symbolic.X4, 0);
      Symbolic.STRB_reg (Symbolic.X8, Symbolic.X6);
      Symbolic.ADD_imm (Symbolic.X4, Symbolic.X4, 1);
      Symbolic.ADD_imm (Symbolic.X6, Symbolic.X6, 1);
      Symbolic.SUB_imm (Symbolic.X7, Symbolic.X7, 1);
      Symbolic.B_label pathCopy;
      Symbolic.Label pathDone;
      Symbolic.MOVZ (Symbolic.X8, 0, 0);
      Symbolic.STRB_reg (Symbolic.X8, Symbolic.X6);
    ]
  @ openCall @ openFailureCheck
  @ [
      Symbolic.MOV_reg (Symbolic.X9, Symbolic.X0);
      Symbolic.MOV_reg (Symbolic.X10, Symbolic.X28);
      Symbolic.MOVZ (Symbolic.X11, 0, 0);
      Symbolic.ADD_imm (Symbolic.X5, Symbolic.SP, 2048);
      Symbolic.ADD_imm (Symbolic.X5, Symbolic.X5, 2048);
      Symbolic.Label readChunk;
    ]
  @ readCall @ readFailureCheck
  @ [
      Symbolic.MOV_reg (Symbolic.X14, Symbolic.X0);
      Symbolic.MOVZ (Symbolic.X15, 0, 0);
      Symbolic.Label entryLoop;
      Symbolic.CMP_reg (Symbolic.X15, Symbolic.X14);
      Symbolic.B_cond_label (Symbolic.GE, entriesDone);
      Symbolic.ADD_reg (Symbolic.X4, Symbolic.X5, Symbolic.X15);
      Symbolic.LDRB_imm (Symbolic.X6, Symbolic.X4, 16);
      Symbolic.LDRB_imm (Symbolic.X7, Symbolic.X4, 17);
      Symbolic.LSL_imm (Symbolic.X7, Symbolic.X7, 8);
      Symbolic.ORR_reg (Symbolic.X6, Symbolic.X6, Symbolic.X7);
      Symbolic.ADD_imm (Symbolic.X8, Symbolic.X4, nameOffset);
      Symbolic.LDRB_imm (Symbolic.X7, Symbolic.X8, 0);
      Symbolic.CMP_imm (Symbolic.X7, 46);
      Symbolic.B_cond_label (Symbolic.NE, appendPath);
      Symbolic.LDRB_imm (Symbolic.X7, Symbolic.X8, 1);
      Symbolic.CBZ (Symbolic.X7, skipEntry);
      Symbolic.CMP_imm (Symbolic.X7, 46);
      Symbolic.B_cond_label (Symbolic.NE, appendPath);
      Symbolic.LDRB_imm (Symbolic.X7, Symbolic.X8, 2);
      Symbolic.CBZ (Symbolic.X7, skipEntry);
      Symbolic.Label appendPath;
      Symbolic.ADD_imm (Symbolic.X1, Symbolic.X12, 16);
      Symbolic.MOV_reg (Symbolic.X2, Symbolic.X13);
      Symbolic.ADD_imm (Symbolic.X3, Symbolic.X10, 16);
      Symbolic.ADD_reg (Symbolic.X3, Symbolic.X3, Symbolic.X11);
      Symbolic.Label appendPathDone;
      Symbolic.CBZ (Symbolic.X2, noSlash);
      Symbolic.LDRB_imm (Symbolic.X7, Symbolic.X1, 0);
      Symbolic.STRB_reg (Symbolic.X7, Symbolic.X3);
      Symbolic.ADD_imm (Symbolic.X1, Symbolic.X1, 1);
      Symbolic.ADD_imm (Symbolic.X3, Symbolic.X3, 1);
      Symbolic.ADD_imm (Symbolic.X11, Symbolic.X11, 1);
      Symbolic.SUB_imm (Symbolic.X2, Symbolic.X2, 1);
      Symbolic.B_label appendPathDone;
      Symbolic.Label noSlash;
      Symbolic.CBZ (Symbolic.X13, appendName);
      Symbolic.SUB_imm (Symbolic.X1, Symbolic.X13, 1);
      Symbolic.ADD_imm (Symbolic.X2, Symbolic.X12, 16);
      Symbolic.LDRB (Symbolic.X7, Symbolic.X2, Symbolic.X1);
      Symbolic.CMP_imm (Symbolic.X7, 47);
      Symbolic.B_cond_label (Symbolic.EQ, appendName);
      Symbolic.MOVZ (Symbolic.X7, 47, 0);
      Symbolic.STRB_reg (Symbolic.X7, Symbolic.X3);
      Symbolic.ADD_imm (Symbolic.X3, Symbolic.X3, 1);
      Symbolic.ADD_imm (Symbolic.X11, Symbolic.X11, 1);
      Symbolic.Label appendName;
      Symbolic.LDRB_imm (Symbolic.X7, Symbolic.X8, 0);
      Symbolic.CBZ (Symbolic.X7, appendNameDone);
      Symbolic.STRB_reg (Symbolic.X7, Symbolic.X3);
      Symbolic.ADD_imm (Symbolic.X8, Symbolic.X8, 1);
      Symbolic.ADD_imm (Symbolic.X3, Symbolic.X3, 1);
      Symbolic.ADD_imm (Symbolic.X11, Symbolic.X11, 1);
      Symbolic.B_label appendName;
      Symbolic.Label appendNameDone;
      Symbolic.MOVZ (Symbolic.X7, 0, 0);
      Symbolic.STRB_reg (Symbolic.X7, Symbolic.X3);
      Symbolic.ADD_imm (Symbolic.X11, Symbolic.X11, 1);
      Symbolic.Label skipEntry;
      Symbolic.ADD_reg (Symbolic.X15, Symbolic.X15, Symbolic.X6);
      Symbolic.B_label entryLoop;
      Symbolic.Label entriesDone;
      Symbolic.B_label readChunk;
      Symbolic.Label readDone;
      Symbolic.MOV_reg (Symbolic.X0, Symbolic.X9);
      Symbolic.MOVZ
        ( syscalls.ARM64.syscallRegister,
          syscalls.ARM64.numbers.Platform.close,
          0 );
      Symbolic.SVC syscalls.ARM64.svcImmediate;
      Symbolic.MOVZ (Symbolic.X7, 1, 0);
      Symbolic.STR (Symbolic.X7, Symbolic.X10, 0);
      Symbolic.STR (Symbolic.X11, Symbolic.X10, 8);
      Symbolic.ADD_imm (Symbolic.X7, Symbolic.X11, 7);
      Symbolic.LSR_imm (Symbolic.X7, Symbolic.X7, 3);
      Symbolic.LSL_imm (Symbolic.X7, Symbolic.X7, 3);
      Symbolic.ADD_imm (Symbolic.X7, Symbolic.X7, 16);
      Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X7);
      Symbolic.MOV_reg (destReg, Symbolic.X10);
      Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048);
      Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048);
      Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048);
      Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048);
    ]
  @ generateLeakCounterInc ctx
  @ [
      Symbolic.B_label complete;
      Symbolic.Label openFailed;
      Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048);
      Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048);
      Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048);
      Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048);
    ]
  @ loadStringLiteralPointer destReg ""
  @ [ Symbolic.Label complete ]

let emitUnitOk (ctx : codeGenContext) (destReg : Symbolic.reg) =
  [
    Symbolic.MOV_reg (destReg, Symbolic.X28);
    Symbolic.ADD_imm (Symbolic.X28, Symbolic.X28, 24);
    Symbolic.MOVZ (Symbolic.X9, 0, 0);
    Symbolic.STR (Symbolic.X9, destReg, 0);
    Symbolic.STR (Symbolic.X9, destReg, 8);
    Symbolic.MOVZ (Symbolic.X9, 1, 0);
    Symbolic.STR (Symbolic.X9, destReg, 16);
  ]
  @ generateLeakCounterInc ctx

let emitSetEnv (ctx : codeGenContext) (destReg : Symbolic.reg)
    (loads : Symbolic.instr list) =
  let prefix =
    Printf.sprintf "__setenv_%s_%s" ctx.functionName ctx.instructionSite
  in
  let findRoot = Printf.sprintf "%s_find_root" prefix in
  let rootFound = Printf.sprintf "%s_root_found" prefix in
  let findArgvEnd = Printf.sprintf "%s_find_argv_end" prefix in
  let nextEntry = Printf.sprintf "%s_next_entry" prefix in
  let compare = Printf.sprintf "%s_compare" prefix in
  let nameMatched = Printf.sprintf "%s_name_matched" prefix in
  let useSlot = Printf.sprintf "%s_use_slot" prefix in
  let copyName = Printf.sprintf "%s_copy_name" prefix in
  let nameDone = Printf.sprintf "%s_name_done" prefix in
  let copyValue = Printf.sprintf "%s_copy_value" prefix in
  let valueDone = Printf.sprintf "%s_value_done" prefix in
  let nextSlot = Printf.sprintf "%s_next_slot" prefix in
  let stored = Printf.sprintf "%s_stored" prefix in
  loads
  @ [
      Symbolic.MOV_reg (Symbolic.X9, Symbolic.X29);
      Symbolic.Label findRoot;
      Symbolic.LDR (Symbolic.X10, Symbolic.X9, 0);
      Symbolic.CBZ (Symbolic.X10, rootFound);
      Symbolic.MOV_reg (Symbolic.X9, Symbolic.X10);
      Symbolic.B_label findRoot;
      Symbolic.Label rootFound;
      Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 24);
      Symbolic.Label findArgvEnd;
      Symbolic.LDR (Symbolic.X10, Symbolic.X9, 0);
      Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 8);
      Symbolic.CBNZ (Symbolic.X10, findArgvEnd);
      Symbolic.LDR (Symbolic.X11, Symbolic.X14, 8);
      Symbolic.Label nextEntry;
      Symbolic.LDR (Symbolic.X10, Symbolic.X9, 0);
      Symbolic.CBZ (Symbolic.X10, useSlot);
      Symbolic.MOVZ (Symbolic.X12, 0, 0);
      Symbolic.Label compare;
      Symbolic.CMP_reg (Symbolic.X12, Symbolic.X11);
      Symbolic.B_cond_label (Symbolic.GE, nameMatched);
      Symbolic.ADD_imm (Symbolic.X13, Symbolic.X14, 16);
      Symbolic.LDRB (Symbolic.X13, Symbolic.X13, Symbolic.X12);
      Symbolic.LDRB (Symbolic.X15, Symbolic.X10, Symbolic.X12);
      Symbolic.CMP_reg (Symbolic.X13, Symbolic.X15);
      Symbolic.B_cond_label (Symbolic.NE, nextSlot);
      Symbolic.ADD_imm (Symbolic.X12, Symbolic.X12, 1);
      Symbolic.B_label compare;
      Symbolic.Label nameMatched;
      Symbolic.LDRB (Symbolic.X13, Symbolic.X10, Symbolic.X12);
      Symbolic.CMP_imm (Symbolic.X13, 61);
      Symbolic.B_cond_label (Symbolic.EQ, useSlot);
      Symbolic.Label nextSlot;
      Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 8);
      Symbolic.B_label nextEntry;
      Symbolic.Label useSlot;
      Symbolic.MOV_reg (Symbolic.X10, Symbolic.X28);
      Symbolic.LDR (Symbolic.X12, Symbolic.X1, 8);
      Symbolic.ADD_reg (Symbolic.X13, Symbolic.X11, Symbolic.X12);
      Symbolic.ADD_imm (Symbolic.X13, Symbolic.X13, 9);
      Symbolic.LSR_imm (Symbolic.X13, Symbolic.X13, 3);
      Symbolic.LSL_imm (Symbolic.X13, Symbolic.X13, 3);
      Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X13);
      Symbolic.ADD_imm (Symbolic.X13, Symbolic.X14, 16);
      Symbolic.MOV_reg (Symbolic.X15, Symbolic.X10);
      Symbolic.MOV_reg (Symbolic.X12, Symbolic.X11);
      Symbolic.Label copyName;
      Symbolic.CBZ (Symbolic.X12, nameDone);
      Symbolic.LDRB_imm (Symbolic.X11, Symbolic.X13, 0);
      Symbolic.STRB_reg (Symbolic.X11, Symbolic.X15);
      Symbolic.ADD_imm (Symbolic.X13, Symbolic.X13, 1);
      Symbolic.ADD_imm (Symbolic.X15, Symbolic.X15, 1);
      Symbolic.SUB_imm (Symbolic.X12, Symbolic.X12, 1);
      Symbolic.B_label copyName;
      Symbolic.Label nameDone;
      Symbolic.MOVZ (Symbolic.X11, 61, 0);
      Symbolic.STRB_reg (Symbolic.X11, Symbolic.X15);
      Symbolic.ADD_imm (Symbolic.X15, Symbolic.X15, 1);
      Symbolic.ADD_imm (Symbolic.X13, Symbolic.X1, 16);
      Symbolic.LDR (Symbolic.X12, Symbolic.X1, 8);
      Symbolic.Label copyValue;
      Symbolic.CBZ (Symbolic.X12, valueDone);
      Symbolic.LDRB_imm (Symbolic.X11, Symbolic.X13, 0);
      Symbolic.STRB_reg (Symbolic.X11, Symbolic.X15);
      Symbolic.ADD_imm (Symbolic.X13, Symbolic.X13, 1);
      Symbolic.ADD_imm (Symbolic.X15, Symbolic.X15, 1);
      Symbolic.SUB_imm (Symbolic.X12, Symbolic.X12, 1);
      Symbolic.B_label copyValue;
      Symbolic.Label valueDone;
      Symbolic.MOVZ (Symbolic.X11, 0, 0);
      Symbolic.STRB_reg (Symbolic.X11, Symbolic.X15);
      Symbolic.LDR (Symbolic.X11, Symbolic.X9, 0);
      Symbolic.STR (Symbolic.X10, Symbolic.X9, 0);
      Symbolic.CBNZ (Symbolic.X11, stored);
      Symbolic.STR (Symbolic.X11, Symbolic.X9, 8);
      Symbolic.Label stored;
    ]
  @ emitUnitOk ctx destReg

let emitUnsetEnv (ctx : codeGenContext) (destReg : Symbolic.reg)
    (loads : Symbolic.instr list) =
  let prefix =
    Printf.sprintf "__unsetenv_%s_%s" ctx.functionName ctx.instructionSite
  in
  let findRoot = Printf.sprintf "%s_find_root" prefix in
  let rootFound = Printf.sprintf "%s_root_found" prefix in
  let findArgvEnd = Printf.sprintf "%s_find_argv_end" prefix in
  let nextEntry = Printf.sprintf "%s_next_entry" prefix in
  let compare = Printf.sprintf "%s_compare" prefix in
  let nameMatched = Printf.sprintf "%s_name_matched" prefix in
  let advance = Printf.sprintf "%s_advance" prefix in
  let shift = Printf.sprintf "%s_shift" prefix in
  let doneLabel = Printf.sprintf "%s_done" prefix in
  loads
  @ [
      Symbolic.MOV_reg (Symbolic.X9, Symbolic.X29);
      Symbolic.Label findRoot;
      Symbolic.LDR (Symbolic.X10, Symbolic.X9, 0);
      Symbolic.CBZ (Symbolic.X10, rootFound);
      Symbolic.MOV_reg (Symbolic.X9, Symbolic.X10);
      Symbolic.B_label findRoot;
      Symbolic.Label rootFound;
      Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 24);
      Symbolic.Label findArgvEnd;
      Symbolic.LDR (Symbolic.X10, Symbolic.X9, 0);
      Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 8);
      Symbolic.CBNZ (Symbolic.X10, findArgvEnd);
      Symbolic.LDR (Symbolic.X11, Symbolic.X0, 8);
      Symbolic.Label nextEntry;
      Symbolic.LDR (Symbolic.X10, Symbolic.X9, 0);
      Symbolic.CBZ (Symbolic.X10, doneLabel);
      Symbolic.MOVZ (Symbolic.X12, 0, 0);
      Symbolic.Label compare;
      Symbolic.CMP_reg (Symbolic.X12, Symbolic.X11);
      Symbolic.B_cond_label (Symbolic.GE, nameMatched);
      Symbolic.ADD_imm (Symbolic.X13, Symbolic.X0, 16);
      Symbolic.LDRB (Symbolic.X13, Symbolic.X13, Symbolic.X12);
      Symbolic.LDRB (Symbolic.X14, Symbolic.X10, Symbolic.X12);
      Symbolic.CMP_reg (Symbolic.X13, Symbolic.X14);
      Symbolic.B_cond_label (Symbolic.NE, advance);
      Symbolic.ADD_imm (Symbolic.X12, Symbolic.X12, 1);
      Symbolic.B_label compare;
      Symbolic.Label nameMatched;
      Symbolic.LDRB (Symbolic.X13, Symbolic.X10, Symbolic.X12);
      Symbolic.CMP_imm (Symbolic.X13, 61);
      Symbolic.B_cond_label (Symbolic.NE, advance);
      Symbolic.MOV_reg (Symbolic.X13, Symbolic.X9);
      Symbolic.ADD_imm (Symbolic.X14, Symbolic.X9, 8);
      Symbolic.Label shift;
      Symbolic.LDR (Symbolic.X15, Symbolic.X14, 0);
      Symbolic.STR (Symbolic.X15, Symbolic.X13, 0);
      Symbolic.CBZ (Symbolic.X15, doneLabel);
      Symbolic.ADD_imm (Symbolic.X13, Symbolic.X13, 8);
      Symbolic.ADD_imm (Symbolic.X14, Symbolic.X14, 8);
      Symbolic.B_label shift;
      Symbolic.Label advance;
      Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 8);
      Symbolic.B_label nextEntry;
      Symbolic.Label doneLabel;
    ]
  @ emitUnitOk ctx destReg

(*
   Generate random 8 bytes as Int64
*)
let emitRandomInt64 (ctx : codeGenContext) (dest : LIR.reg) =
  lirRegToARM64Reg dest
  |> Result.map (fun destReg ->
      runtimeInstrs (ClockAndRandom.generateRandomInt64 ctx.target destReg))

(*
   Generate the current UTC instant as 100ns Unix ticks.
*)
let emitDateTimeNow (ctx : codeGenContext) (dest : LIR.reg) =
  lirRegToARM64Reg dest
  |> Result.map (fun destReg ->
      runtimeInstrs (ClockAndRandom.generateDateTimeNow ctx.target destReg))

let emitSleep (ctx : codeGenContext) (effectId : int) (delayMs : LIR.fReg) =
  lirFRegToARM64FReg delayMs
  |> Result.map (fun delayReg ->
      let syscalls = ARM64.targetSyscalls ctx.target in
      let label suffix =
        Printf.sprintf "__sleep_%s_%d_%s_%s" ctx.functionName effectId
          ctx.instructionSite suffix
      in
      let retryLabel = label "retry" in
      let interruptedLabel = label "interrupted" in
      let releaseLabel = label "release" in
      let completeLabel = label "complete" in
      let millionLabel = floatDataLabel 1000000.0 in
      let resultCheck =
        match ARM64.targetOS ctx.target with
        | Platform.Linux ->
            loadImmediate Symbolic.X12 (-4L)
            @ [
                Symbolic.CMP_reg (Symbolic.X0, Symbolic.X12);
                Symbolic.B_cond_label (Symbolic.EQ, interruptedLabel);
                Symbolic.B_label releaseLabel;
              ]
        | Platform.MacOS ->
            [
              Symbolic.B_cond_label (Symbolic.HS, interruptedLabel);
              Symbolic.B_label releaseLabel;
            ]
      in
      let interruptCheck =
        match ARM64.targetOS ctx.target with
        | Platform.Linux -> []
        | Platform.MacOS ->
            [
              Symbolic.CMP_imm (Symbolic.X0, 4);
              Symbolic.B_cond_label (Symbolic.NE, releaseLabel);
            ]
      in
      [
        Symbolic.ADRP (Symbolic.X9, millionLabel);
        Symbolic.ADD_label (Symbolic.X9, Symbolic.X9, millionLabel);
        Symbolic.LDR_fp (Symbolic.D16, Symbolic.X9, 0);
        Symbolic.FMUL (Symbolic.D16, delayReg, Symbolic.D16);
        Symbolic.FCVTZS (Symbolic.X9, Symbolic.D16);
        Symbolic.CMP_imm (Symbolic.X9, 0);
        Symbolic.B_cond_label (Symbolic.LE, completeLabel);
      ]
      @ loadImmediate Symbolic.X12 1000000000L
      @ [
          Symbolic.SDIV (Symbolic.X10, Symbolic.X9, Symbolic.X12);
          Symbolic.MSUB (Symbolic.X11, Symbolic.X10, Symbolic.X12, Symbolic.X9);
          Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 32);
          Symbolic.STP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 0);
          Symbolic.MOVZ (Symbolic.X12, 0, 0);
          Symbolic.STP (Symbolic.X12, Symbolic.X12, Symbolic.SP, 16);
          Symbolic.Label retryLabel;
          Symbolic.MOV_reg (Symbolic.X0, Symbolic.SP);
          Symbolic.ADD_imm (Symbolic.X1, Symbolic.SP, 16);
          Symbolic.MOVZ
            ( syscalls.ARM64.syscallRegister,
              syscalls.ARM64.numbers.Platform.nanosleep,
              0 );
          Symbolic.SVC syscalls.ARM64.svcImmediate;
        ]
      @ resultCheck
      @ [ Symbolic.Label interruptedLabel ]
      @ interruptCheck
      @ [
          Symbolic.LDP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 16);
          Symbolic.STP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 0);
          Symbolic.B_label retryLabel;
          Symbolic.Label releaseLabel;
          Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 32);
          Symbolic.Label completeLabel;
        ])

let emitCliNative (ctx : codeGenContext) (dest : LIR.reg)
    (operation : LIR.cliOperation) (args : LIR.operand list) =
  lirRegToARM64Reg dest
  |> bind (fun destReg ->
      match operation with
      | LIR.PosixOpenAt | LIR.PosixRead | LIR.PosixWrite | LIR.PosixClose
      | LIR.PosixSeek | LIR.PosixStatAt | LIR.PosixGetCwd | LIR.PosixChdir
      | LIR.PosixMkdirAt | LIR.PosixUnlinkAt | LIR.PosixRenameAt
      | LIR.PosixChmodAt | LIR.PosixChmodAt2 | LIR.PosixUtimesAt
      | LIR.PosixSetAttributesAt | LIR.PosixSymlinkAt | LIR.PosixReadlinkAt
      | LIR.PosixFlock | LIR.PosixGetDents | LIR.PosixIoctl -> (
          let number, arity =
            match operation with
            | LIR.PosixOpenAt ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> Some 56
                  | Platform.MacOS -> Some 463),
                  4 )
            | LIR.PosixRead ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> Some 63
                  | Platform.MacOS -> Some 3),
                  3 )
            | LIR.PosixWrite ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> Some 64
                  | Platform.MacOS -> Some 4),
                  3 )
            | LIR.PosixClose ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> Some 57
                  | Platform.MacOS -> Some 6),
                  1 )
            | LIR.PosixSeek ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> Some 62
                  | Platform.MacOS -> Some 199),
                  3 )
            | LIR.PosixStatAt ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> Some 79
                  | Platform.MacOS -> Some 470),
                  4 )
            | LIR.PosixGetCwd ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> Some 17
                  | Platform.MacOS -> Some 326),
                  2 )
            | LIR.PosixChdir ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> Some 50
                  | Platform.MacOS -> Some 13),
                  1 )
            | LIR.PosixMkdirAt ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> Some 34
                  | Platform.MacOS -> Some 475),
                  3 )
            | LIR.PosixUnlinkAt ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> Some 35
                  | Platform.MacOS -> Some 472),
                  3 )
            | LIR.PosixRenameAt ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> Some 38
                  | Platform.MacOS -> Some 465),
                  4 )
            | LIR.PosixChmodAt2 ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> Some 452
                  | Platform.MacOS -> None),
                  4 )
            | LIR.PosixChmodAt ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> Some 53
                  | Platform.MacOS -> Some 467),
                  4 )
            | LIR.PosixUtimesAt ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> Some 88
                  | Platform.MacOS -> None),
                  4 )
            | LIR.PosixSetAttributesAt ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> None
                  | Platform.MacOS -> Some 524),
                  6 )
            | LIR.PosixSymlinkAt ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> Some 36
                  | Platform.MacOS -> Some 474),
                  3 )
            | LIR.PosixReadlinkAt ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> Some 78
                  | Platform.MacOS -> Some 473),
                  4 )
            | LIR.PosixFlock ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> Some 32
                  | Platform.MacOS -> Some 131),
                  2 )
            | LIR.PosixIoctl ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> Some 29
                  | Platform.MacOS -> Some 54),
                  3 )
            | LIR.PosixGetDents ->
                ( (match ARM64.targetOS ctx.target with
                  | Platform.Linux -> Some 61
                  | Platform.MacOS -> Some 344),
                  4 )
            | _ -> Crash.crash "Non-POSIX operation in POSIX lowering"
          in
          if List.length args <> arity then
            Error "POSIX primitive: invalid argument count"
          else
            match number with
            | None -> Ok (loadImmediate destReg (-38L))
            | Some number ->
                args
                |> List.fold_left
                     (fun result operand ->
                       result
                       |> bind (fun code ->
                           loadCliOperand Symbolic.X9 operand
                           |> Result.map (fun next ->
                               code @ next
                               @ [
                                   Symbolic.SUB_imm
                                     (Symbolic.SP, Symbolic.SP, 16);
                                   Symbolic.STR (Symbolic.X9, Symbolic.SP, 0);
                                 ])))
                     (Ok [])
                |> Result.map (fun loads ->
                    let targets =
                      [
                        Symbolic.X0;
                        Symbolic.X1;
                        Symbolic.X2;
                        Symbolic.X3;
                        Symbolic.X4;
                        Symbolic.X5;
                      ]
                      |> List.filteri (fun i _ -> i < arity)
                    in
                    let syscall = ARM64.targetSyscalls ctx.target in
                    let doneLabel =
                      Printf.sprintf "__posix_%s_%s_done" ctx.functionName
                        ctx.instructionSite
                    in
                    let normalize =
                      match ARM64.targetOS ctx.target with
                      | Platform.Linux -> []
                      | Platform.MacOS ->
                          [
                            Symbolic.B_cond_label (Symbolic.LO, doneLabel);
                            Symbolic.NEG (Symbolic.X0, Symbolic.X0);
                            Symbolic.Label doneLabel;
                          ]
                    in
                    loads
                    @ (List.rev targets
                      |> List.concat_map (fun reg ->
                          [
                            Symbolic.LDR (reg, Symbolic.SP, 0);
                            Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 16);
                          ]))
                    @ [
                        Symbolic.MOVZ (syscall.ARM64.syscallRegister, number, 0);
                        Symbolic.SVC syscall.ARM64.svcImmediate;
                      ]
                    @ normalize
                    @ [ Symbolic.MOV_reg (destReg, Symbolic.X0) ]))
      | LIR.HostOS ->
          Ok
            [
              Symbolic.MOVZ
                ( destReg,
                  (if ARM64.targetOS ctx.target = Platform.MacOS then 2 else 1),
                  0 );
            ]
      | LIR.HostArchitecture ->
          Ok
            [
              Symbolic.MOVZ
                ( destReg,
                  (if ARM64.targetOS ctx.target = Platform.MacOS then 3 else 2),
                  0 );
            ]
      | LIR.Hostname ->
          let label suffix =
            Printf.sprintf "__hostname_%s_%s_%s" ctx.functionName
              ctx.instructionSite suffix
          in
          let failureLabel = label "failure" in
          let lengthLabel = label "length" in
          let lengthDoneLabel = label "length_done" in
          let copyLabel = label "copy" in
          let copyDoneLabel = label "copy_done" in
          let completeLabel = label "complete" in
          let os = ARM64.targetOS ctx.target in
          let stackSize, nodeOffset, syscallNumber =
            match os with
            | Platform.Linux -> (400, 65, 160)
            | Platform.MacOS -> (1280, 256, 164)
          in
          let failureCheck =
            match os with
            | Platform.Linux ->
                [
                  Symbolic.CMP_imm (Symbolic.X0, 0);
                  Symbolic.B_cond_label (Symbolic.LT, failureLabel);
                ]
            | Platform.MacOS ->
                [ Symbolic.B_cond_label (Symbolic.HS, failureLabel) ]
          in
          let normalizeErrno =
            match os with
            | Platform.Linux -> [ Symbolic.NEG (Symbolic.X2, Symbolic.X0) ]
            | Platform.MacOS -> [ Symbolic.MOV_reg (Symbolic.X2, Symbolic.X0) ]
          in
          Ok
            ([
               Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, stackSize);
               Symbolic.MOV_reg (Symbolic.X0, Symbolic.SP);
               Symbolic.MOVZ
                 ( (ARM64.targetSyscalls ctx.target).ARM64.syscallRegister,
                   syscallNumber,
                   0 );
               Symbolic.SVC (ARM64.targetSyscalls ctx.target).ARM64.svcImmediate;
             ]
            @ failureCheck
            @ [
                Symbolic.ADD_imm (Symbolic.X2, Symbolic.SP, nodeOffset);
                Symbolic.MOVZ (Symbolic.X3, 0, 0);
                Symbolic.Label lengthLabel;
                Symbolic.LDRB (Symbolic.X4, Symbolic.X2, Symbolic.X3);
                Symbolic.CBZ (Symbolic.X4, lengthDoneLabel);
                Symbolic.ADD_imm (Symbolic.X3, Symbolic.X3, 1);
                Symbolic.B_label lengthLabel;
                Symbolic.Label lengthDoneLabel;
                Symbolic.MOV_reg (Symbolic.X5, Symbolic.X28);
                Symbolic.MOVZ (Symbolic.X8, 1, 0);
                Symbolic.STR (Symbolic.X8, Symbolic.X5, 0);
                Symbolic.STR (Symbolic.X3, Symbolic.X5, 8);
                Symbolic.ADD_imm (Symbolic.X6, Symbolic.X3, 7);
                Symbolic.LSR_imm (Symbolic.X6, Symbolic.X6, 3);
                Symbolic.LSL_imm (Symbolic.X6, Symbolic.X6, 3);
                Symbolic.ADD_imm (Symbolic.X7, Symbolic.X6, 16);
                Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X7);
                Symbolic.ADD_imm (Symbolic.X7, Symbolic.X5, 16);
                Symbolic.MOV_reg (Symbolic.X8, Symbolic.X3);
                Symbolic.Label copyLabel;
                Symbolic.CBZ (Symbolic.X8, copyDoneLabel);
                Symbolic.LDRB_imm (Symbolic.X9, Symbolic.X2, 0);
                Symbolic.STRB_reg (Symbolic.X9, Symbolic.X7);
                Symbolic.ADD_imm (Symbolic.X2, Symbolic.X2, 1);
                Symbolic.ADD_imm (Symbolic.X7, Symbolic.X7, 1);
                Symbolic.SUB_imm (Symbolic.X8, Symbolic.X8, 1);
                Symbolic.B_label copyLabel;
                Symbolic.Label copyDoneLabel;
                Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, stackSize);
              ]
            @ generateLeakCounterInc ctx
            @ [
                Symbolic.MOV_reg (destReg, Symbolic.X28);
                Symbolic.ADD_imm (Symbolic.X28, Symbolic.X28, 24);
                Symbolic.MOVZ (Symbolic.X8, 0, 0);
                Symbolic.STR (Symbolic.X8, destReg, 0);
                Symbolic.STR (Symbolic.X5, destReg, 8);
                Symbolic.MOVZ (Symbolic.X8, 1, 0);
                Symbolic.STR (Symbolic.X8, destReg, 16);
              ]
            @ generateLeakCounterInc ctx
            @ [ Symbolic.B_label completeLabel; Symbolic.Label failureLabel ]
            @ normalizeErrno
            @ [ Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, stackSize) ]
            @ loadStringLiteralPointer Symbolic.X3 "POSIX error"
            @ [
                Symbolic.MOV_reg (Symbolic.X4, Symbolic.X28);
                Symbolic.ADD_imm (Symbolic.X28, Symbolic.X28, 24);
                Symbolic.STR (Symbolic.X2, Symbolic.X4, 0);
                Symbolic.STR (Symbolic.X3, Symbolic.X4, 8);
                Symbolic.MOVZ (Symbolic.X5, 1, 0);
                Symbolic.STR (Symbolic.X5, Symbolic.X4, 16);
              ]
            @ generateLeakCounterInc ctx
            @ [
                Symbolic.MOV_reg (destReg, Symbolic.X28);
                Symbolic.ADD_imm (Symbolic.X28, Symbolic.X28, 24);
                Symbolic.MOVZ (Symbolic.X5, 1, 0);
                Symbolic.STR (Symbolic.X5, destReg, 0);
                Symbolic.STR (Symbolic.X4, destReg, 8);
                Symbolic.STR (Symbolic.X5, destReg, 16);
              ]
            @ generateLeakCounterInc ctx
            @ [ Symbolic.Label completeLabel ])
      | LIR.Execute when ARM64.targetOS ctx.target = Platform.Linux -> (
          match args with
          | [ command ] ->
              loadCliOperand Symbolic.X0 command
              |> Result.map (fun loads ->
                  loads
                  @ [ Symbolic.BL "__dark_cli_execute" ]
                  @
                  if destReg = Symbolic.X0 then []
                  else [ Symbolic.MOV_reg (destReg, Symbolic.X0) ])
          | _ -> Error "CLI execute expects exactly one command")
      | LIR.GetPid | LIR.GetUid ->
          let os = ARM64.targetOS ctx.target in
          let number =
            match (operation, os) with
            | LIR.GetPid, Platform.Linux -> 172
            | LIR.GetPid, Platform.MacOS -> 20
            | LIR.GetUid, Platform.Linux -> 174
            | LIR.GetUid, Platform.MacOS -> 24
            | _ -> 0
          in
          let syscalls = ARM64.targetSyscalls ctx.target in
          Ok
            [
              Symbolic.MOVZ (syscalls.ARM64.syscallRegister, number, 0);
              Symbolic.SVC syscalls.ARM64.svcImmediate;
              Symbolic.MOV_reg (destReg, Symbolic.X0);
            ]
      | LIR.SecureRandomFill -> (
          match args with
          | [ buffer; length ] ->
              loadCliOperand Symbolic.X0 buffer
              |> bind (fun bufferLoads ->
                  loadCliOperand Symbolic.X1 length
                  |> Result.map (fun lengthLoads ->
                      let syscalls = ARM64.targetSyscalls ctx.target in
                      let result =
                        match ARM64.targetOS ctx.target with
                        | Platform.Linux -> []
                        | Platform.MacOS ->
                            let success =
                              Printf.sprintf "__entropy_%s_%s_success"
                                ctx.functionName ctx.instructionSite
                            in
                            let done_ =
                              Printf.sprintf "__entropy_%s_%s_done"
                                ctx.functionName ctx.instructionSite
                            in
                            [
                              Symbolic.B_cond_label (Symbolic.LO, success);
                              Symbolic.NEG (Symbolic.X0, Symbolic.X0);
                              Symbolic.B_label done_;
                              Symbolic.Label success;
                              Symbolic.MOV_reg (Symbolic.X0, Symbolic.X4);
                              Symbolic.Label done_;
                            ]
                      in
                      bufferLoads @ lengthLoads
                      @ [
                          Symbolic.MOV_reg (Symbolic.X4, Symbolic.X1);
                          Symbolic.MOVZ (Symbolic.X2, 0, 0);
                          Symbolic.MOVZ
                            ( syscalls.ARM64.syscallRegister,
                              syscalls.ARM64.numbers.Platform.getrandom,
                              0 );
                          Symbolic.SVC syscalls.ARM64.svcImmediate;
                        ]
                      @ result
                      @ [ Symbolic.MOV_reg (destReg, Symbolic.X0) ]))
          | _ -> Error "SecureRandomFill expects buffer and length")
      | LIR.SocketTcp4 | LIR.SocketTcp6 | LIR.SocketUdp4 | LIR.SocketUdp6 ->
          let syscalls = ARM64.targetSyscalls ctx.target in
          let constants =
            Platform.socketConstantsFor (ARM64.targetOS ctx.target)
          in
          let isUdp =
            operation = LIR.SocketUdp4 || operation = LIR.SocketUdp6
          in
          let socketType =
            if isUdp then constants.Platform.datagramType
            else constants.Platform.streamType
          in
          let protocol = if isUdp then 17 else 6 in
          let family =
            if operation = LIR.SocketTcp6 || operation = LIR.SocketUdp6 then
              constants.Platform.addressFamily6
            else constants.Platform.addressFamily4
          in
          let normalize =
            match ARM64.targetOS ctx.target with
            | Platform.Linux -> []
            | Platform.MacOS ->
                let prefix =
                  Printf.sprintf "__socket_open_%s_%s" ctx.functionName
                    ctx.instructionSite
                in
                let openFailed = prefix ^ "_open_failed" in
                let flagsFailed = prefix ^ "_flags_failed" in
                let doneLabel = prefix ^ "_done" in
                [
                  Symbolic.B_cond_label (Symbolic.HS, openFailed);
                  Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 16);
                  Symbolic.STR (Symbolic.X0, Symbolic.SP, 0);
                  Symbolic.MOVZ (Symbolic.X1, 2, 0);
                  Symbolic.MOVZ (Symbolic.X2, 1, 0);
                  Symbolic.MOVZ
                    ( syscalls.ARM64.syscallRegister,
                      syscalls.ARM64.numbers.Platform.fcntl,
                      0 );
                  Symbolic.SVC syscalls.ARM64.svcImmediate;
                  Symbolic.B_cond_label (Symbolic.HS, flagsFailed);
                  Symbolic.LDR (Symbolic.X0, Symbolic.SP, 0);
                  Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 16);
                  Symbolic.B_label doneLabel;
                  Symbolic.Label flagsFailed;
                  Symbolic.NEG (Symbolic.X0, Symbolic.X0);
                  Symbolic.STR (Symbolic.X0, Symbolic.SP, 8);
                  Symbolic.LDR (Symbolic.X0, Symbolic.SP, 0);
                  Symbolic.MOVZ
                    ( syscalls.ARM64.syscallRegister,
                      syscalls.ARM64.numbers.Platform.close,
                      0 );
                  Symbolic.SVC syscalls.ARM64.svcImmediate;
                  Symbolic.LDR (Symbolic.X0, Symbolic.SP, 8);
                  Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 16);
                  Symbolic.B_label doneLabel;
                  Symbolic.Label openFailed;
                  Symbolic.NEG (Symbolic.X0, Symbolic.X0);
                  Symbolic.Label doneLabel;
                ]
          in
          Ok
            ([ Symbolic.MOVZ (Symbolic.X0, family, 0) ]
            @ loadImmediate Symbolic.X1 socketType
            @ [
                Symbolic.MOVZ (Symbolic.X2, protocol, 0);
                Symbolic.MOVZ
                  ( syscalls.ARM64.syscallRegister,
                    syscalls.ARM64.numbers.Platform.socket,
                    0 );
                Symbolic.SVC syscalls.ARM64.svcImmediate;
              ]
            @ normalize
            @ [ Symbolic.MOV_reg (destReg, Symbolic.X0) ])
              | LIR.SocketConnect4 | LIR.SocketConnect6 | LIR.SocketSend | LIR.SocketSendTo | LIR.SocketReceive | LIR.SocketReceiveFrom | LIR.SocketReceiveTimeout | LIR.SocketSendTimeout -> (
          let syscall = ARM64.targetSyscalls ctx.target in
          let os = ARM64.targetOS ctx.target in
          let normalize =
            if os = Platform.Linux then []
            else
              let doneLabel =
                Printf.sprintf "__socket_io_%s_%s_done" ctx.functionName
                  ctx.instructionSite
              in
              [
                Symbolic.B_cond_label (Symbolic.LO, doneLabel);
                Symbolic.NEG (Symbolic.X0, Symbolic.X0);
                Symbolic.Label doneLabel;
              ]
          in
          let rec loadArguments operands registers = match operands, registers with
            | [], [] -> Ok []
            | operand :: remaining, register :: destinations ->
                loadCliOperand register operand
                |> bind (fun loads -> loadArguments remaining destinations |> Result.map (fun tail -> loads @ tail))
            | _ -> Error "Invalid socket argument registers" in
          match (operation, args) with
          | (LIR.SocketConnect4 | LIR.SocketConnect6), [ descriptor; address ]
            ->
              loadCliOperand Symbolic.X0 descriptor
              |> bind (fun fdLoads ->
                  loadCliOperand Symbolic.X1 address
                  |> Result.map (fun addressLoads ->
                      fdLoads @ addressLoads
                      @ [
                          Symbolic.MOVZ
                            ( Symbolic.X2,
                              (if operation = LIR.SocketConnect6 then 28 else 16),
                              0 );
                          Symbolic.MOVZ
                            ( syscall.ARM64.syscallRegister,
                              syscall.ARM64.numbers.Platform.connect,
                              0 );
                          Symbolic.SVC syscall.ARM64.svcImmediate;
                        ]
                      @ normalize
                      @ [ Symbolic.MOV_reg (destReg, Symbolic.X0) ]))
          | LIR.SocketSend, [ descriptor; blob ] ->
              loadCliOperand Symbolic.X0 descriptor
              |> bind (fun fdLoads ->
                  loadCliOperand Symbolic.X1 blob
                  |> Result.map (fun blobLoads ->
                      fdLoads @ blobLoads
                      @ [
                          Symbolic.LDR (Symbolic.X2, Symbolic.X1, 8);
                          Symbolic.ADD_imm (Symbolic.X1, Symbolic.X1, 16);
                          Symbolic.MOVZ (Symbolic.X4, 0, 0);
                          Symbolic.MOVZ (Symbolic.X5, 0, 0);
                        ]
                      @ loadImmediate Symbolic.X3
                          (Platform.socketConstantsFor os).Platform.noSignal
                      @ [
                          Symbolic.MOVZ
                            ( syscall.ARM64.syscallRegister,
                              syscall.ARM64.numbers.Platform.sendTo,
                              0 );
                          Symbolic.SVC syscall.ARM64.svcImmediate;
                        ]
                      @ normalize
                      @ [ Symbolic.MOV_reg (destReg, Symbolic.X0) ]))
                      | LIR.SocketSendTo, [descriptor; blob; address; length] ->
                loadArguments [descriptor; blob; address; length] [Symbolic.X0; Symbolic.X1; Symbolic.X4; Symbolic.X5]
                |> Result.map (fun loads ->
                    loads @ [Symbolic.LDR (Symbolic.X2, Symbolic.X1, 8);
                             Symbolic.ADD_imm (Symbolic.X1, Symbolic.X1, 16)] @
                    loadImmediate Symbolic.X3 (Platform.socketConstantsFor os).Platform.noSignal @
                    [Symbolic.MOVZ (syscall.ARM64.syscallRegister, syscall.ARM64.numbers.Platform.sendTo, 0);
                     Symbolic.SVC syscall.ARM64.svcImmediate] @ normalize @
                    [Symbolic.MOV_reg (destReg, Symbolic.X0)])
            | LIR.SocketReceiveFrom, [descriptor; buffer; length; peer] ->
                loadArguments [descriptor; buffer; length; peer] [Symbolic.X0; Symbolic.X1; Symbolic.X2; Symbolic.X4]
                |> Result.map (fun loads ->
                    loads @ [Symbolic.MOV_reg (Symbolic.X5, Symbolic.X4);
                             Symbolic.ADD_imm (Symbolic.X4, Symbolic.X4, 8);
                             Symbolic.MOVZ (Symbolic.X3, 0, 0);
                             Symbolic.MOVZ (syscall.ARM64.syscallRegister, syscall.ARM64.numbers.Platform.recvFrom, 0);
                             Symbolic.SVC syscall.ARM64.svcImmediate] @ normalize @
                    [Symbolic.MOV_reg (destReg, Symbolic.X0)])
| LIR.SocketReceive, [ descriptor; buffer; length ] ->
              loadCliOperand Symbolic.X0 descriptor
              |> bind (fun fdLoads ->
                  loadCliOperand Symbolic.X1 buffer
                  |> bind (fun bufferLoads ->
                      loadCliOperand Symbolic.X2 length
                      |> Result.map (fun lengthLoads ->
                          fdLoads @ bufferLoads @ lengthLoads
                          @ [
                              Symbolic.MOVZ
                                ( syscall.ARM64.syscallRegister,
                                  syscall.ARM64.numbers.Platform.read,
                                  0 );
                              Symbolic.SVC syscall.ARM64.svcImmediate;
                            ]
                          @ normalize
                          @ [ Symbolic.MOV_reg (destReg, Symbolic.X0) ])))
          | ( (LIR.SocketReceiveTimeout | LIR.SocketSendTimeout),
              [ descriptor; timeval ] ) ->
              loadCliOperand Symbolic.X0 descriptor
              |> bind (fun fdLoads ->
                  loadCliOperand Symbolic.X3 timeval
                  |> Result.map (fun timevalLoads ->
                      let constants = Platform.socketConstantsFor os in
                      let option_ =
                        if operation = LIR.SocketSendTimeout then
                          constants.Platform.sendTimeout
                        else constants.Platform.receiveTimeout
                      in
                      fdLoads @ timevalLoads
                      @ [
                          Symbolic.MOVZ
                            (Symbolic.X1, constants.Platform.socketLevel, 0);
                          Symbolic.MOVZ (Symbolic.X2, option_, 0);
                          Symbolic.MOVZ (Symbolic.X4, 16, 0);
                          Symbolic.MOVZ
                            ( syscall.ARM64.syscallRegister,
                              syscall.ARM64.numbers.Platform.setSockOpt,
                              0 );
                          Symbolic.SVC syscall.ARM64.svcImmediate;
                        ]
                      @ normalize
                      @ [ Symbolic.MOV_reg (destReg, Symbolic.X0) ]))
          | _ -> Error "Invalid socket operation arguments")
              | LIR.SocketBind4 | LIR.SocketBind6 | LIR.SocketListen | LIR.SocketAccept | LIR.SocketCloexec | LIR.SocketReuseAddress | LIR.SocketPoll
      | LIR.SignalBlock | LIR.SignalRestore | LIR.SignalPending | LIR.SignalWait
      | LIR.MonotonicTime -> (
          let syscall = ARM64.targetSyscalls ctx.target in
          let os = ARM64.targetOS ctx.target in
          let constants = Platform.socketConstantsFor os in
          let normalize =
            if os = Platform.Linux then []
            else
              let doneLabel =
                Printf.sprintf "__listener_%s_%s_done" ctx.functionName
                  ctx.instructionSite
              in
              [
                Symbolic.B_cond_label (Symbolic.LO, doneLabel);
                Symbolic.NEG (Symbolic.X0, Symbolic.X0);
                Symbolic.Label doneLabel;
              ]
          in
          let rec loadArguments operands registers =
            match (operands, registers) with
            | [], [] -> Ok []
            | operand :: remaining, register :: destinations ->
                loadCliOperand register operand
                |> bind (fun loads ->
                    loadArguments remaining destinations
                    |> Result.map (fun tail -> loads @ tail))
            | _ -> Error "Invalid listener argument registers"
          in
          let emit operands registers setup number =
            loadArguments operands registers
            |> Result.map (fun loads ->
                loads @ setup
                @ [
                    Symbolic.MOVZ (syscall.ARM64.syscallRegister, number, 0);
                    Symbolic.SVC syscall.ARM64.svcImmediate;
                  ]
                @ normalize
                @ [ Symbolic.MOV_reg (destReg, Symbolic.X0) ])
          in
          match (operation, args) with
                      | (LIR.SocketBind4 | LIR.SocketBind6),[descriptor;address] ->
                emit [descriptor;address] [Symbolic.X0;Symbolic.X1] (loadImmediate Symbolic.X2 (if operation = LIR.SocketBind6 then 28L else 16L)) syscall.ARM64.numbers.Platform.bind
          | LIR.SocketListen, [ descriptor ] ->
              emit [ descriptor ] [ Symbolic.X0 ]
                (loadImmediate Symbolic.X1 128L)
                syscall.ARM64.numbers.Platform.listen
          | LIR.SocketAccept, [ descriptor ] ->
              emit [ descriptor ] [ Symbolic.X0 ]
                (loadImmediate Symbolic.X1 0L @ loadImmediate Symbolic.X2 0L)
                syscall.ARM64.numbers.Platform.accept
          | LIR.SocketCloexec, [ descriptor ] ->
              emit [ descriptor ] [ Symbolic.X0 ]
                (loadImmediate Symbolic.X1 2L @ loadImmediate Symbolic.X2 1L)
                syscall.ARM64.numbers.Platform.fcntl
          | LIR.SocketReuseAddress, [ descriptor; enabled ] ->
              emit [ descriptor; enabled ]
                [ Symbolic.X0; Symbolic.X3 ]
                (loadImmediate Symbolic.X1
                   (Int64.of_int constants.Platform.socketLevel)
                @ loadImmediate Symbolic.X2
                    (Int64.of_int constants.Platform.reuseAddress)
                @ loadImmediate Symbolic.X4 4L)
                syscall.ARM64.numbers.Platform.setSockOpt
          | LIR.SocketPoll, [ pollfd; timeout ] ->
              let timeoutLoads =
                if os = Platform.Linux then loadCliOperand Symbolic.X2 timeout
                else
                  loadCliOperand Symbolic.X2 timeout
                  |> Result.map (fun loads ->
                      loads
                      @ [
                          Symbolic.LDR (Symbolic.X9, Symbolic.X2, 0);
                          Symbolic.LDR (Symbolic.X10, Symbolic.X2, 8);
                        ]
                      @ loadImmediate Symbolic.X11 1000L
                      @ [
                          Symbolic.MUL (Symbolic.X9, Symbolic.X9, Symbolic.X11);
                        ]
                      @ loadImmediate Symbolic.X11 999999L
                      @ [
                          Symbolic.ADD_reg
                            (Symbolic.X10, Symbolic.X10, Symbolic.X11);
                        ]
                      @ loadImmediate Symbolic.X11 1000000L
                      @ [
                          Symbolic.UDIV
                            (Symbolic.X10, Symbolic.X10, Symbolic.X11);
                          Symbolic.ADD_reg
                            (Symbolic.X2, Symbolic.X9, Symbolic.X10);
                        ])
              in
              timeoutLoads
              |> bind (fun loads ->
                  emit [ pollfd ] [ Symbolic.X0 ]
                    (loads
                    @ loadImmediate Symbolic.X1 1L
                    @ loadImmediate Symbolic.X3 0L
                    @ loadImmediate Symbolic.X4 8L)
                    syscall.ARM64.numbers.Platform.poll)
          | LIR.SignalBlock, [ mask; previous ] ->
              emit [ mask; previous ]
                [ Symbolic.X1; Symbolic.X2 ]
                (loadImmediate Symbolic.X0
                   (Int64.of_int constants.Platform.blockSignal)
                @ loadImmediate Symbolic.X3 8L)
                syscall.ARM64.numbers.Platform.signalMask
          | LIR.SignalRestore, [ previous ] ->
              emit [ previous ] [ Symbolic.X1 ]
                (loadImmediate Symbolic.X0
                   (Int64.of_int constants.Platform.restoreSignal)
                @ loadImmediate Symbolic.X2 0L
                @ loadImmediate Symbolic.X3 8L)
                syscall.ARM64.numbers.Platform.signalMask
          | LIR.SignalPending, [ mask ] ->
              emit [ mask ] [ Symbolic.X0 ]
                (loadImmediate Symbolic.X1 8L)
                syscall.ARM64.numbers.Platform.signalPending
          | LIR.SignalWait, [ mask; info ] ->
              emit [ mask; info ]
                [ Symbolic.X0; Symbolic.X1 ]
                (loadImmediate Symbolic.X2 0L @ loadImmediate Symbolic.X3 8L)
                syscall.ARM64.numbers.Platform.signalWait
          | LIR.MonotonicTime, [ time ] ->
              if os = Platform.MacOS then
                emitMacOSMonotonicTime ctx destReg time
              else
                emit [ time ] [ Symbolic.X1 ]
                  (loadImmediate Symbolic.X0 1L)
                  syscall.ARM64.numbers.Platform.gettimeofday
          | _ -> Error "Invalid listener or signal operation arguments")
      | LIR.SocketClose -> (
          match args with
          | [ descriptor ] ->
              loadCliOperand Symbolic.X0 descriptor
              |> Result.map (fun loads ->
                  let syscalls = ARM64.targetSyscalls ctx.target in
                  let normalize =
                    match ARM64.targetOS ctx.target with
                    | Platform.Linux -> []
                    | Platform.MacOS ->
                        let doneLabel =
                          Printf.sprintf "__socket_close_%s_%s_done"
                            ctx.functionName ctx.instructionSite
                        in
                        [
                          Symbolic.B_cond_label (Symbolic.LO, doneLabel);
                          Symbolic.NEG (Symbolic.X0, Symbolic.X0);
                          Symbolic.Label doneLabel;
                        ]
                  in
                  loads
                  @ [
                      Symbolic.MOVZ
                        ( syscalls.ARM64.syscallRegister,
                          syscalls.ARM64.numbers.Platform.close,
                          0 );
                      Symbolic.SVC syscalls.ARM64.svcImmediate;
                    ]
                  @ normalize
                  @ [ Symbolic.MOV_reg (destReg, Symbolic.X0) ])
          | _ -> Error "SocketClose expects one descriptor")
      | LIR.CpuCount -> (
          match ARM64.targetOS ctx.target with
          | Platform.MacOS ->
              let label suffix =
                Printf.sprintf "__cpu_count_%s_%s_%s" ctx.functionName
                  ctx.instructionSite suffix
              in
              let fallback = label "fallback" in
              let complete = label "complete" in
              Ok
                ([ Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 32) ]
                @ loadImmediate Symbolic.X9 0x1900000006L
                @ [
                    Symbolic.STR (Symbolic.X9, Symbolic.SP, 0);
                    Symbolic.MOVZ (Symbolic.X9, 0, 0);
                    Symbolic.STR (Symbolic.X9, Symbolic.SP, 8);
                    Symbolic.MOVZ (Symbolic.X9, 4, 0);
                    Symbolic.STR (Symbolic.X9, Symbolic.SP, 16);
                    Symbolic.MOV_reg (Symbolic.X0, Symbolic.SP);
                    Symbolic.MOVZ (Symbolic.X1, 2, 0);
                    Symbolic.ADD_imm (Symbolic.X2, Symbolic.SP, 8);
                    Symbolic.ADD_imm (Symbolic.X3, Symbolic.SP, 16);
                    Symbolic.MOVZ (Symbolic.X4, 0, 0);
                    Symbolic.MOVZ (Symbolic.X5, 0, 0);
                    Symbolic.MOVZ (Symbolic.X16, 202, 0);
                    Symbolic.SVC 0x80;
                    Symbolic.B_cond_label (Symbolic.HS, fallback);
                    Symbolic.LDR (Symbolic.X9, Symbolic.SP, 8);
                    Symbolic.CBZ (Symbolic.X9, fallback);
                    Symbolic.MOV_reg (destReg, Symbolic.X9);
                    Symbolic.B_label complete;
                    Symbolic.Label fallback;
                    Symbolic.MOVZ (destReg, 1, 0);
                    Symbolic.Label complete;
                    Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 32);
                  ])
          | Platform.Linux ->
              let label suffix =
                Printf.sprintf "__cpu_count_%s_%s_%s" ctx.functionName
                  ctx.instructionSite suffix
              in
              let byteLoop = label "byte_loop" in
              let bitLoop = label "bit_loop" in
              let nextByte = label "next_byte" in
              let doneLabel = label "done" in
              let fallback = label "fallback" in
              let complete = label "complete" in
              let zeroMask =
                List.init 16 Fun.id
                |> List.map (fun index ->
                    Symbolic.STR (Symbolic.X9, Symbolic.SP, int16 (mul index 8)))
              in
              Ok
                ([
                   Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 128);
                   Symbolic.MOVZ (Symbolic.X9, 0, 0);
                 ]
                @ zeroMask
                @ [
                    Symbolic.MOVZ (Symbolic.X0, 0, 0);
                    Symbolic.MOVZ (Symbolic.X1, 128, 0);
                    Symbolic.MOV_reg (Symbolic.X2, Symbolic.SP);
                    Symbolic.MOVZ (Symbolic.X8, 123, 0);
                    Symbolic.SVC 0;
                    Symbolic.CMP_imm (Symbolic.X0, 0);
                    Symbolic.B_cond_label (Symbolic.LT, fallback);
                    Symbolic.MOVZ (Symbolic.X9, 0, 0);
                    Symbolic.MOVZ (Symbolic.X10, 0, 0);
                    Symbolic.Label byteLoop;
                    Symbolic.CMP_imm (Symbolic.X9, 128);
                    Symbolic.B_cond_label (Symbolic.GE, doneLabel);
                    Symbolic.LDRB (Symbolic.X11, Symbolic.SP, Symbolic.X9);
                    Symbolic.Label bitLoop;
                    Symbolic.CBZ (Symbolic.X11, nextByte);
                    Symbolic.AND_imm (Symbolic.X12, Symbolic.X11, 1L);
                    Symbolic.ADD_reg (Symbolic.X10, Symbolic.X10, Symbolic.X12);
                    Symbolic.LSR_imm (Symbolic.X11, Symbolic.X11, 1);
                    Symbolic.B_label bitLoop;
                    Symbolic.Label nextByte;
                    Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 1);
                    Symbolic.B_label byteLoop;
                    Symbolic.Label doneLabel;
                    Symbolic.MOV_reg (destReg, Symbolic.X10);
                    Symbolic.B_label complete;
                    Symbolic.Label fallback;
                    Symbolic.MOVZ (destReg, 1, 0);
                    Symbolic.Label complete;
                    Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 128);
                  ]))
      | LIR.GetArgv -> (
          match args with
          | [ index ] ->
              loadCliOperand Symbolic.X0 index
              |> Result.map (fun loads ->
                  loads
                  @ [
                      Symbolic.BL
                        (Printf.sprintf "__dark_cli_argv_%s" ctx.functionName);
                    ]
                  @
                  if destReg = Symbolic.X0 then []
                  else [ Symbolic.MOV_reg (destReg, Symbolic.X0) ])
          | _ -> Error "CLI argv expects exactly one index")
      | LIR.GetEnv -> (
          match args with
          | [ name ] ->
              loadCliOperand Symbolic.X0 name
              |> Result.map (fun loads ->
                  let label suffix =
                    Printf.sprintf "__getenv_%s_%s_%s" ctx.functionName
                      ctx.instructionSite suffix
                  in
                  let findRoot = label "find_root" in
                  let rootFound = label "root_found" in
                  let findArgvEnd = label "find_argv_end" in
                  let nextEntry = label "next_entry" in
                  let compareName = label "compare_name" in
                  let nameMatched = label "name_matched" in
                  let findLength = label "find_length" in
                  let lengthFound = label "length_found" in
                  let copyValue = label "copy_value" in
                  let copyDone = label "copy_done" in
                  let missing = label "missing" in
                  let complete = label "complete" in
                  loads
                  @ [
                      Symbolic.MOV_reg (Symbolic.X1, Symbolic.X29);
                      Symbolic.Label findRoot;
                      Symbolic.LDR (Symbolic.X2, Symbolic.X1, 0);
                      Symbolic.CBZ (Symbolic.X2, rootFound);
                      Symbolic.MOV_reg (Symbolic.X1, Symbolic.X2);
                      Symbolic.B_label findRoot;
                      Symbolic.Label rootFound;
                      Symbolic.ADD_imm (Symbolic.X1, Symbolic.X1, 24);
                      Symbolic.Label findArgvEnd;
                      Symbolic.LDR (Symbolic.X2, Symbolic.X1, 0);
                      Symbolic.ADD_imm (Symbolic.X1, Symbolic.X1, 8);
                      Symbolic.CBNZ (Symbolic.X2, findArgvEnd);
                      Symbolic.LDR (Symbolic.X3, Symbolic.X0, 8);
                      Symbolic.Label nextEntry;
                      Symbolic.LDR (Symbolic.X2, Symbolic.X1, 0);
                      Symbolic.ADD_imm (Symbolic.X1, Symbolic.X1, 8);
                      Symbolic.CBZ (Symbolic.X2, missing);
                      Symbolic.MOVZ (Symbolic.X4, 0, 0);
                      Symbolic.Label compareName;
                      Symbolic.CMP_reg (Symbolic.X4, Symbolic.X3);
                      Symbolic.B_cond_label (Symbolic.GE, nameMatched);
                      Symbolic.ADD_imm (Symbolic.X5, Symbolic.X0, 16);
                      Symbolic.LDRB (Symbolic.X6, Symbolic.X5, Symbolic.X4);
                      Symbolic.LDRB (Symbolic.X5, Symbolic.X2, Symbolic.X4);
                      Symbolic.CMP_reg (Symbolic.X5, Symbolic.X6);
                      Symbolic.B_cond_label (Symbolic.NE, nextEntry);
                      Symbolic.ADD_imm (Symbolic.X4, Symbolic.X4, 1);
                      Symbolic.B_label compareName;
                      Symbolic.Label nameMatched;
                      Symbolic.LDRB (Symbolic.X5, Symbolic.X2, Symbolic.X4);
                      Symbolic.CMP_imm (Symbolic.X5, 61);
                      Symbolic.B_cond_label (Symbolic.NE, nextEntry);
                      Symbolic.ADD_imm (Symbolic.X8, Symbolic.X2, 1);
                      Symbolic.ADD_reg (Symbolic.X8, Symbolic.X8, Symbolic.X3);
                      Symbolic.MOVZ (Symbolic.X9, 0, 0);
                      Symbolic.Label findLength;
                      Symbolic.LDRB (Symbolic.X5, Symbolic.X8, Symbolic.X9);
                      Symbolic.CBZ (Symbolic.X5, lengthFound);
                      Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, 1);
                      Symbolic.B_label findLength;
                      Symbolic.Label lengthFound;
                      Symbolic.MOV_reg (Symbolic.X7, Symbolic.X28);
                      Symbolic.MOVZ (Symbolic.X11, 1, 0);
                      Symbolic.STR (Symbolic.X11, Symbolic.X7, 0);
                      Symbolic.STR (Symbolic.X9, Symbolic.X7, 8);
                      Symbolic.ADD_imm (Symbolic.X13, Symbolic.X9, 7);
                      Symbolic.LSR_imm (Symbolic.X13, Symbolic.X13, 3);
                      Symbolic.LSL_imm (Symbolic.X13, Symbolic.X13, 3);
                      Symbolic.ADD_imm (Symbolic.X14, Symbolic.X13, 16);
                      Symbolic.ADD_reg (Symbolic.X28, Symbolic.X28, Symbolic.X14);
                      Symbolic.ADD_imm (Symbolic.X10, Symbolic.X7, 16);
                      Symbolic.MOV_reg (Symbolic.X11, Symbolic.X9);
                      Symbolic.MOV_reg (Symbolic.X12, Symbolic.X8);
                      Symbolic.Label copyValue;
                      Symbolic.CBZ (Symbolic.X11, copyDone);
                      Symbolic.LDRB_imm (Symbolic.X5, Symbolic.X12, 0);
                      Symbolic.STRB_reg (Symbolic.X5, Symbolic.X10);
                      Symbolic.ADD_imm (Symbolic.X12, Symbolic.X12, 1);
                      Symbolic.ADD_imm (Symbolic.X10, Symbolic.X10, 1);
                      Symbolic.SUB_imm (Symbolic.X11, Symbolic.X11, 1);
                      Symbolic.B_label copyValue;
                      Symbolic.Label copyDone;
                    ]
                  @ generateLeakCounterInc ctx
                  @ [
                      Symbolic.MOV_reg (destReg, Symbolic.X7);
                      Symbolic.B_label complete;
                      Symbolic.Label missing;
                      Symbolic.MOVZ (destReg, 0, 0);
                      Symbolic.Label complete;
                    ])
          | _ -> Error "CLI getenv expects exactly one name")
      | LIR.GetEnvironmentPacked -> Ok (emitEnvironmentPacked ctx destReg)
      | LIR.DirectoryCurrent -> Ok (emitDirectoryCurrent ctx destReg)
      | LIR.DirectoryListPacked -> (
          match args with
          | [ path ] ->
              loadCliOperand Symbolic.X0 path
              |> Result.map (emitDirectoryListPacked ctx destReg)
          | _ -> Error "directoryList expects exactly one path")
      | LIR.FileIsDirectory -> (
          match args with
          | [ path ] ->
              loadCliOperand Symbolic.X0 path
              |> Result.map (fun loads ->
                  let labelPrefix =
                    Printf.sprintf "__is_dir_%s_%s" ctx.functionName
                      ctx.instructionSite
                  in
                  let copyLoop = Printf.sprintf "%s_copy" labelPrefix in
                  let copyDone = Printf.sprintf "%s_copy_done" labelPrefix in
                  let failure = Printf.sprintf "%s_failure" labelPrefix in
                  let complete = Printf.sprintf "%s_complete" labelPrefix in
                  let syscalls = ARM64.targetSyscalls ctx.target in
                  let openCall, failureCheck =
                    match ARM64.targetOS ctx.target with
                    | Platform.Linux ->
                        ( loadImmediate Symbolic.X0 (-100L)
                          @ [ Symbolic.MOV_reg (Symbolic.X1, Symbolic.SP) ]
                          @ loadImmediate Symbolic.X2 16384L
                          @ [
                              Symbolic.MOVZ (Symbolic.X3, 0, 0);
                              Symbolic.MOVZ
                                ( syscalls.ARM64.syscallRegister,
                                  syscalls.ARM64.numbers.Platform.open_,
                                  0 );
                              Symbolic.SVC syscalls.ARM64.svcImmediate;
                            ],
                          [
                            Symbolic.CMP_imm (Symbolic.X0, 0);
                            Symbolic.B_cond_label (Symbolic.LT, failure);
                          ] )
                    | Platform.MacOS ->
                        ( [ Symbolic.MOV_reg (Symbolic.X0, Symbolic.SP) ]
                          @ loadImmediate Symbolic.X1 0x100000L
                          @ [
                              Symbolic.MOVZ (Symbolic.X2, 0, 0);
                              Symbolic.MOVZ
                                ( syscalls.ARM64.syscallRegister,
                                  syscalls.ARM64.numbers.Platform.open_,
                                  0 );
                              Symbolic.SVC syscalls.ARM64.svcImmediate;
                            ],
                          [ Symbolic.B_cond_label (Symbolic.HS, failure) ] )
                  in
                  loads
                  @ [
                      Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 2048);
                      Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 2048);
                      Symbolic.LDR (Symbolic.X9, Symbolic.X0, 8);
                      Symbolic.ADD_imm (Symbolic.X10, Symbolic.X0, 16);
                      Symbolic.MOV_reg (Symbolic.X11, Symbolic.SP);
                      Symbolic.Label copyLoop;
                      Symbolic.CBZ (Symbolic.X9, copyDone);
                      Symbolic.LDRB_imm (Symbolic.X12, Symbolic.X10, 0);
                      Symbolic.STRB_reg (Symbolic.X12, Symbolic.X11);
                      Symbolic.ADD_imm (Symbolic.X10, Symbolic.X10, 1);
                      Symbolic.ADD_imm (Symbolic.X11, Symbolic.X11, 1);
                      Symbolic.SUB_imm (Symbolic.X9, Symbolic.X9, 1);
                      Symbolic.B_label copyLoop;
                      Symbolic.Label copyDone;
                      Symbolic.MOVZ (Symbolic.X12, 0, 0);
                      Symbolic.STRB_reg (Symbolic.X12, Symbolic.X11);
                    ]
                  @ openCall @ failureCheck
                  @ [
                      Symbolic.MOV_reg (Symbolic.X9, Symbolic.X0);
                      Symbolic.MOVZ
                        ( syscalls.ARM64.syscallRegister,
                          syscalls.ARM64.numbers.Platform.close,
                          0 );
                      Symbolic.SVC syscalls.ARM64.svcImmediate;
                      Symbolic.MOVZ (destReg, 1, 0);
                      Symbolic.B_label complete;
                      Symbolic.Label failure;
                      Symbolic.MOVZ (destReg, 0, 0);
                      Symbolic.Label complete;
                      Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048);
                      Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048);
                    ])
          | _ -> Error "fileIsDirectory expects exactly one path")
      | LIR.FileCreateExclusive -> (
          match args with
          | [ path ] ->
              loadCliOperand Symbolic.X0 path
              |> Result.map (fun loads ->
                  let labelPrefix =
                    Printf.sprintf "__create_exclusive_%s_%s" ctx.functionName
                      ctx.instructionSite
                  in
                  let copyLoop = Printf.sprintf "%s_copy" labelPrefix in
                  let copyDone = Printf.sprintf "%s_copy_done" labelPrefix in
                  let tooLong = Printf.sprintf "%s_too_long" labelPrefix in
                  let failure = Printf.sprintf "%s_failure" labelPrefix in
                  let complete = Printf.sprintf "%s_complete" labelPrefix in
                  let syscalls = ARM64.targetSyscalls ctx.target in
                  let openCall, failureCheck, normalizeError =
                    match ARM64.targetOS ctx.target with
                    | Platform.Linux ->
                        ( loadImmediate Symbolic.X0 (-100L)
                          @ [ Symbolic.MOV_reg (Symbolic.X1, Symbolic.SP) ]
                          @ loadImmediate Symbolic.X2 194L
                          @ loadImmediate Symbolic.X3 0o600L
                          @ [
                              Symbolic.MOVZ
                                ( syscalls.ARM64.syscallRegister,
                                  syscalls.ARM64.numbers.Platform.open_,
                                  0 );
                              Symbolic.SVC syscalls.ARM64.svcImmediate;
                            ],
                          [
                            Symbolic.CMP_imm (Symbolic.X0, 0);
                            Symbolic.B_cond_label (Symbolic.LT, failure);
                          ],
                          [ Symbolic.NEG (destReg, Symbolic.X0) ] )
                    | Platform.MacOS ->
                        ( [ Symbolic.MOV_reg (Symbolic.X0, Symbolic.SP) ]
                          @ loadImmediate Symbolic.X1 2562L
                          @ loadImmediate Symbolic.X2 0o600L
                          @ [
                              Symbolic.MOVZ
                                ( syscalls.ARM64.syscallRegister,
                                  syscalls.ARM64.numbers.Platform.open_,
                                  0 );
                              Symbolic.SVC syscalls.ARM64.svcImmediate;
                            ],
                          [ Symbolic.B_cond_label (Symbolic.HS, failure) ],
                          [ Symbolic.MOV_reg (destReg, Symbolic.X0) ] )
                  in
                  loads
                  @ [
                      Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 2048);
                      Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 2048);
                      Symbolic.LDR (Symbolic.X9, Symbolic.X0, 8);
                    ]
                  @ loadImmediate Symbolic.X12 4096L
                  @ [
                      Symbolic.CMP_reg (Symbolic.X9, Symbolic.X12);
                      Symbolic.B_cond_label (Symbolic.GE, tooLong);
                      Symbolic.ADD_imm (Symbolic.X10, Symbolic.X0, 16);
                      Symbolic.MOV_reg (Symbolic.X11, Symbolic.SP);
                      Symbolic.Label copyLoop;
                      Symbolic.CBZ (Symbolic.X9, copyDone);
                      Symbolic.LDRB_imm (Symbolic.X12, Symbolic.X10, 0);
                      Symbolic.STRB_reg (Symbolic.X12, Symbolic.X11);
                      Symbolic.ADD_imm (Symbolic.X10, Symbolic.X10, 1);
                      Symbolic.ADD_imm (Symbolic.X11, Symbolic.X11, 1);
                      Symbolic.SUB_imm (Symbolic.X9, Symbolic.X9, 1);
                      Symbolic.B_label copyLoop;
                      Symbolic.Label copyDone;
                      Symbolic.MOVZ (Symbolic.X12, 0, 0);
                      Symbolic.STRB_reg (Symbolic.X12, Symbolic.X11);
                    ]
                  @ openCall @ failureCheck
                  @ [
                      Symbolic.MOVZ
                        ( syscalls.ARM64.syscallRegister,
                          syscalls.ARM64.numbers.Platform.close,
                          0 );
                      Symbolic.SVC syscalls.ARM64.svcImmediate;
                      Symbolic.MOVZ (destReg, 0, 0);
                      Symbolic.B_label complete;
                      Symbolic.Label failure;
                    ]
                  @ normalizeError
                  @ [
                      Symbolic.B_label complete;
                      Symbolic.Label tooLong;
                      Symbolic.MOVZ (destReg, 36, 0);
                      Symbolic.Label complete;
                      Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048);
                      Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048);
                    ])
          | _ -> Error "fileCreateExclusive expects exactly one path")
      | LIR.SetEnv -> (
          match args with
          | [ name; value ] ->
              loadCliOperand Symbolic.X0 name
              |> bind (fun nameLoads ->
                  loadCliOperand Symbolic.X1 value
                  |> Result.map (fun valueLoads ->
                      emitSetEnv ctx destReg
                        (nameLoads
                        @ [ Symbolic.MOV_reg (Symbolic.X14, Symbolic.X0) ]
                        @ valueLoads)))
          | _ -> Error "setenv expects exactly a name and value")
      | LIR.UnsetEnv -> (
          match args with
          | [ name ] ->
              loadCliOperand Symbolic.X0 name
              |> Result.map (emitUnsetEnv ctx destReg)
          | _ -> Error "unsetenv expects exactly one name")
      | LIR.Kill -> (
          match args with
          | [ pid; signal ] ->
              loadCliOperand Symbolic.X0 pid
              |> bind (fun pidLoads ->
                  loadCliOperand Symbolic.X1 signal
                  |> Result.map (fun signalLoads ->
                      let success =
                        [
                          Symbolic.MOV_reg (destReg, Symbolic.X28);
                          Symbolic.ADD_imm (Symbolic.X28, Symbolic.X28, 24);
                          Symbolic.MOVZ (Symbolic.X2, 0, 0);
                          Symbolic.STR (Symbolic.X2, destReg, 0);
                          Symbolic.STR (Symbolic.X2, destReg, 8);
                          Symbolic.MOVZ (Symbolic.X2, 1, 0);
                          Symbolic.STR (Symbolic.X2, destReg, 16);
                        ]
                        @ generateLeakCounterInc ctx
                      in
                      let normalizeErrno =
                        match ARM64.targetOS ctx.target with
                        | Platform.Linux ->
                            [ Symbolic.NEG (Symbolic.X2, Symbolic.X0) ]
                        | Platform.MacOS ->
                            [ Symbolic.MOV_reg (Symbolic.X2, Symbolic.X0) ]
                      in
                      let failure =
                        normalizeErrno
                        @ loadStringLiteralPointer Symbolic.X3 "POSIX error"
                        @ [
                            Symbolic.MOV_reg (Symbolic.X4, Symbolic.X28);
                            Symbolic.ADD_imm (Symbolic.X28, Symbolic.X28, 24);
                            Symbolic.STR (Symbolic.X2, Symbolic.X4, 0);
                            Symbolic.STR (Symbolic.X3, Symbolic.X4, 8);
                            Symbolic.MOVZ (Symbolic.X5, 1, 0);
                            Symbolic.STR (Symbolic.X5, Symbolic.X4, 16);
                          ]
                        @ generateLeakCounterInc ctx
                        @ [
                            Symbolic.MOV_reg (destReg, Symbolic.X28);
                            Symbolic.ADD_imm (Symbolic.X28, Symbolic.X28, 24);
                            Symbolic.MOVZ (Symbolic.X5, 1, 0);
                            Symbolic.STR (Symbolic.X5, destReg, 0);
                            Symbolic.STR (Symbolic.X4, destReg, 8);
                            Symbolic.STR (Symbolic.X5, destReg, 16);
                          ]
                        @ generateLeakCounterInc ctx
                      in
                      let branchToFailure =
                        match ARM64.targetOS ctx.target with
                        | Platform.Linux ->
                            Symbolic.B_cond
                              (Symbolic.LT, List.length success + 2)
                        | Platform.MacOS ->
                            Symbolic.B_cond
                              (Symbolic.HS, List.length success + 2)
                      in
                      let prepareFailureCheck =
                        match ARM64.targetOS ctx.target with
                        | Platform.Linux ->
                            [ Symbolic.CMP_imm (Symbolic.X0, 0) ]
                        | Platform.MacOS -> []
                      in
                      let syscalls = ARM64.targetSyscalls ctx.target in
                      let killNumber =
                        match ARM64.targetOS ctx.target with
                        | Platform.Linux -> 129
                        | Platform.MacOS -> 37
                      in
                      pidLoads @ signalLoads
                      @ [
                          Symbolic.MOVZ
                            (syscalls.ARM64.syscallRegister, killNumber, 0);
                          Symbolic.SVC syscalls.ARM64.svcImmediate;
                        ]
                      @ prepareFailureCheck @ [ branchToFailure ] @ success
                      @ [ Symbolic.B (List.length failure + 1) ]
                      @ failure))
          | _ -> Error "CLI kill expects a pid and signal")
      | LIR.RunProcess when ARM64.targetOS ctx.target = Platform.Linux -> (
          match args with
          | [ request ] ->
              loadCliOperand Symbolic.X0 request
              |> Result.map (fun loads ->
                  loads
                  @ [ Symbolic.BL "__dark_cli_run_process" ]
                  @
                  if destReg = Symbolic.X0 then []
                  else [ Symbolic.MOV_reg (destReg, Symbolic.X0) ])
          | _ -> Error "CLI run process expects one request")
      | LIR.RunProcess ->
          Ok
            (loadStringLiteralPointer Symbolic.X8 ""
            @ loadStringLiteralPointer Symbolic.X9
                "native process execution unavailable"
            @ [
                Symbolic.MOV_reg (destReg, Symbolic.X28);
                Symbolic.ADD_imm (Symbolic.X28, Symbolic.X28, 48);
                Symbolic.MOVZ (Symbolic.X10, 38, 0);
                Symbolic.STR (Symbolic.X10, destReg, 0);
                Symbolic.MOVZ (Symbolic.X10, 0, 0);
                Symbolic.MVN (Symbolic.X10, Symbolic.X10);
                Symbolic.STR (Symbolic.X10, destReg, 8);
                Symbolic.STR (Symbolic.X8, destReg, 16);
                Symbolic.STR (Symbolic.X9, destReg, 24);
                Symbolic.MOVZ (Symbolic.X10, 0, 0);
                Symbolic.STR (Symbolic.X10, destReg, 32);
                Symbolic.MOVZ (Symbolic.X10, 1, 0);
                Symbolic.STR (Symbolic.X10, destReg, 40);
              ])
      | LIR.SpawnProcess when ARM64.targetOS ctx.target = Platform.Linux -> (
          match args with
          | [ command ] ->
              loadCliOperand Symbolic.X0 command
              |> Result.map (fun loads ->
                  loads
                  @ [
                      Symbolic.BL "__dark_cli_spawn_process";
                      Symbolic.MOV_reg (destReg, Symbolic.X0);
                    ])
          | _ -> Error "CLI spawn process expects one command")
      | LIR.ProcessIO when ARM64.targetOS ctx.target = Platform.Linux -> (
          match args with
          | [ handle; input ] ->
              loadCliOperand Symbolic.X0 handle
              |> bind (fun handleLoads ->
                  loadCliOperand Symbolic.X1 input
                  |> Result.map (fun inputLoads ->
                      handleLoads @ inputLoads
                      @ [
                          Symbolic.MOVZ (Symbolic.X2, 0, 0);
                          Symbolic.BL "__dark_cli_process_io";
                          Symbolic.MOV_reg (destReg, Symbolic.X0);
                        ]))
          | _ -> Error "CLI process IO expects a handle and input")
      | LIR.TerminateProcess when ARM64.targetOS ctx.target = Platform.Linux
        -> (
          match args with
          | [ handle ] ->
              loadCliOperand Symbolic.X0 handle
              |> Result.map (fun loads ->
                  loads
                  @ [
                      Symbolic.BL "__dark_cli_terminate_process";
                      Symbolic.MOV_reg (destReg, Symbolic.X0);
                    ])
          | _ -> Error "CLI terminate process expects one handle")
      | LIR.Execute | LIR.ProcessIO | LIR.TerminateProcess ->
          let errorMessage =
            match operation with
            | LIR.ProcessIO | LIR.TerminateProcess -> "Invalid process handle"
            | _ -> "native CLI operation unavailable"
          in
          Ok
            (loadStringLiteralPointer Symbolic.X8 ""
            @ loadStringLiteralPointer Symbolic.X9 errorMessage
            @ [
                Symbolic.MOV_reg (destReg, Symbolic.X28);
                Symbolic.ADD_imm (Symbolic.X28, Symbolic.X28, 32);
              ]
            @ [
                Symbolic.MOVZ (Symbolic.X10, 0, 0);
                Symbolic.MVN (Symbolic.X10, Symbolic.X10);
                Symbolic.STR (Symbolic.X10, destReg, 0);
                Symbolic.STR (Symbolic.X8, destReg, 8);
                Symbolic.STR (Symbolic.X9, destReg, 16);
                Symbolic.MOVZ (Symbolic.X10, 1, 0);
                Symbolic.STR (Symbolic.X10, destReg, 24);
              ])
      | LIR.SpawnProcess -> Ok (loadImmediate destReg (-1L)))

(*
   Increment coverage counter at _coverage_data[exprId * 8]
   Uses PC-relative addressing (ADRP+ADD) to get BSS buffer address
   Uses X9 and X10 as scratch registers
   Get address of coverage buffer using PC-relative addressing
   Add offset for this expression's counter
   X10 = coverage_buffer[exprId]
   X10++
   coverage_buffer[exprId] = X10
*)
let emitCoverageHit (_ctx : codeGenContext) (exprId : int) =
  let offset = mul exprId 8 in
  Ok
    ([
       Symbolic.ADRP (Symbolic.X9, dataLabel Symbolic.coverageDataLabelName);
       Symbolic.ADD_label
         (Symbolic.X9, Symbolic.X9, dataLabel Symbolic.coverageDataLabelName);
     ]
    @ (if offset = 0 then []
       else if offset < 4096 then
         [ Symbolic.ADD_imm (Symbolic.X9, Symbolic.X9, offset land 65535) ]
       else
         loadImmediate Symbolic.X10 (Int64.of_int offset)
         @ [ Symbolic.ADD_reg (Symbolic.X9, Symbolic.X9, Symbolic.X10) ])
    @ [
        Symbolic.LDR (Symbolic.X10, Symbolic.X9, 0);
        Symbolic.ADD_imm (Symbolic.X10, Symbolic.X10, 1);
        Symbolic.STR (Symbolic.X10, Symbolic.X9, 0);
      ])
