(* X64EmitFiles.ml - Emit x64 instructions for files operations. *)
[@@@warning "-4"]

open X64Operands
open X64CodeGenTypes
module X = X86_64

let preparePointer ctx target fallback = function
  | LIR.Reg reg ->
      Result.map
        (fun source ->
          if source = target then [] else [ X.MOV_reg (target, source) ])
        (resolveReg reg)
  | LIR.StackSlot offset ->
      Ok
        [
          X.MOV_load
            ( target,
              X.RBP,
              Int32.of_int (X64InstructionContext.adjustStackOffset ctx offset)
            );
        ]
  | LIR.StringSymbol value -> Ok (emitStringLiteralNoRefCount target value)
  | _ -> fallback ()

let saveRegs = [ X.RDI; X.RSI; X.RCX; X.R10; X.R8; X.R9 ]
let saves = List.map (fun r -> X.PUSH r) saveRegs
let restores = List.map (fun r -> X.POP r) (List.rev saveRegs)

(*
   File read: open → fstat → alloc → read → close → Result
   Save registers that syscalls will clobber
*)
let emitFileReadBlob ctx dest path =
  Result.bind (resolveReg dest) (fun destReg ->
      let resolvePathToR10 =
        preparePointer ctx X.R10
          (fun () ->
            Error
              "FileReadBlob path operand must be a string pointer or string \
               literal")
          path
      in
      let copyLabel = freshLabel "fr_copy" in
      let doneLabel = freshLabel "fr_done" in
      let errorLabel = freshLabel "fr_err" in
      let cleanupLabel = freshLabel "fr_clean" in
      let openSyscall = Int64.of_int syscalls.Platform.open_ in
      let fstatSyscall = Int64.of_int syscalls.Platform.fstat in
      let readSyscall = Int64.of_int syscalls.Platform.read in
      let closeSyscall = Int64.of_int syscalls.Platform.close in
      Result.map
        (fun pathSetup ->
          pathSetup
          @ saves
            (* Allocate stack: 4096 bytes for path (PATH_MAX) + 144 bytes for stat buf = 4240 *)
          @ [ X.SUB_imm (X.RSP, 4240l) ]
          (* Copy heap string to null-terminated C string on stack *)
          (* R10 points to [refcount][length][data]. *)
          @ [
              X.MOV_load (X.RCX, X.R10, 8l);
              X.LEA (X.RSI, X.R10, 16l);
              X.LEA (X.RDI, X.RSP, 144l);
            ]
            (* RDI = stack buf (after stat buf) *)
          @ loadImm64 X.R10 0L
          @ [
              X.Label copyLabel;
              X.CMP_reg (X.R10, X.RCX);
              X.Jcc (X.GE, doneLabel);
              X.MOV_reg (scratch, X.RSI);
              X.ADD_reg (scratch, X.R10);
              X.MOV_load_byte (scratch, scratch, 0l);
              X.MOV_reg (X.R8, X.RDI);
              X.ADD_reg (X.R8, X.R10);
              X.MOV_store_byte (X.R8, 0l, scratch);
              X.ADD_imm (X.R10, 1l);
              X.JMP copyLabel;
              X.Label doneLabel;
            ]
            (* Null-terminate *)
          @ [ X.MOV_reg (scratch, X.RDI); X.ADD_reg (scratch, X.RCX) ]
          @ loadImm64 X.R10 0L
          @ [ X.MOV_store_byte (scratch, 0l, X.R10) ]
            (* open(path, O_RDONLY=0, 0) → fd *)
          @ [ X.LEA (X.RDI, X.RSP, 144l) ] (* path on stack *)
          @ loadImm64 X.RSI 0L (* O_RDONLY *)
          @ loadImm64 X.RDX 0L (* mode *)
          @ loadImm64 X.RAX openSyscall
          @ [ X.SYSCALL ] (* Check if open failed (RAX < 0) *)
          @ [ X.CMP_imm (X.RAX, 0l); X.Jcc (X.LT, errorLabel) ]
            (* Save fd in R8 *)
          @ [ X.MOV_reg (X.R8, X.RAX) ]
            (* fstat(fd, stat_buf) → get file size *)
          @ [ X.MOV_reg (X.RDI, X.R8) ] (* fd *)
          @ [ X.MOV_reg (X.RSI, X.RSP) ] (* stat buf at RSP *)
          @ loadImm64 X.RAX fstatSyscall
          @ [ X.SYSCALL ]
            (* File size is at offset 48 in stat struct (x86_64 Linux) *)
          @ [ X.MOV_load (X.R9, X.RSP, 48l) ]
          (* R9 = file size *)
          (* Allocate [refcount:8][length:8][data:N]. *)
          @ [ X.MOV_reg (X.R10, heapPtr) ] (* R10 = string ptr *)
          @ [
              X.MOV_reg (scratch, X.R9);
              X.ADD_imm (scratch, 24l);
              (* size + 24 *)
              X.ADD_reg (heapPtr, scratch);
              X.ADD_imm (heapPtr, 7l);
              X.AND_imm (heapPtr, -8l);
            ]
          (* align *)
          (* Store fixed header *)
          @ loadImm64 X.RCX 1L
          @ [ X.MOV_store (X.R10, 0l, X.RCX); X.MOV_store (X.R10, 8l, X.R9) ]
          @ genLeakCounterInc ctx (* read(fd, buf, count) *)
          @ [ X.MOV_reg (X.RDI, X.R8) ] (* fd *)
          @ [ X.LEA (X.RSI, X.R10, 16l) ]
          @ [ X.MOV_reg (X.RDX, X.R9) ] (* count = file size *)
          @ loadImm64 X.RAX readSyscall
          @ [ X.SYSCALL ] (* close(fd) *)
          @ [ X.MOV_reg (X.RDI, X.R8) ]
          @ loadImm64 X.RAX closeSyscall
          @ [ X.SYSCALL ]
            (* Allocate Result Ok: [tag=0:8][payload=string_ptr:8][refcount=1:8] *)
          @ [ X.MOV_reg (scratch, heapPtr); X.ADD_imm (heapPtr, 24l) ]
          @ loadImm64 X.RCX 0L
          @ [
              X.MOV_store (scratch, 0l, X.RCX);
              (* tag = 0 (Ok) *)
              X.MOV_store (scratch, 8l, X.R10);
            ]
            (* payload = string ptr *)
          @ loadImm64 X.RCX 1L
          @ [
              X.MOV_store (scratch, 16l, X.RCX);
              (* refcount = 1 *)
              X.MOV_reg (X.RAX, scratch);
            ]
          @ genLeakCounterInc ctx
          @ [ X.JMP cleanupLabel ] (* === Error path === *)
          @ [ X.Label errorLabel ] (* Allocate error string "File not found". *)
          @ [ X.MOV_reg (X.R10, heapPtr); X.ADD_imm (heapPtr, 32l) ]
          @ loadImm64 scratch 1L
          @ [ X.MOV_store (X.R10, 0l, scratch) ]
          @ loadImm64 scratch 14L
          @ [ X.MOV_store (X.R10, 8l, scratch) ]
          @ loadImm64 scratch 0x746F6E20656C6946L
          @ [ X.MOV_store (X.R10, 16l, scratch) ]
          @ loadImm64 scratch 0x646E756F6620L
          @ [ X.MOV_store (X.R10, 24l, scratch) ]
            (* Allocate Result Error: [tag=1:8][payload=error_str:8][refcount=1:8] *)
          @ [ X.MOV_reg (scratch, heapPtr); X.ADD_imm (heapPtr, 24l) ]
          @ loadImm64 X.RCX 1L
          @ [
              X.MOV_store (scratch, 0l, X.RCX);
              (* tag = 1 (Error) *)
              X.MOV_store (scratch, 8l, X.R10);
            ]
            (* payload = error string *)
          @ loadImm64 X.RCX 1L
          @ [
              X.MOV_store (scratch, 16l, X.RCX);
              (* refcount *)
              X.MOV_reg (X.RAX, scratch);
            ]
          @ genLeakCounterInc ctx
          @ genLeakCounterInc ctx (* === Cleanup === *)
          @ [ X.Label cleanupLabel; X.ADD_imm (X.RSP, 4240l) ]
          @ restores
          @ [ X.MOV_reg (destReg, X.RAX) ])
        resolvePathToR10)

(*
   File write/append: open → write → close → Result
   O_WRONLY|O_CREAT|O_TRUNC = 577 for write, O_WRONLY|O_CREAT|O_APPEND = 1089 for append
*)
let emitFileWriteBlob ctx instr dest path content =
  let isAppend = match instr with LIR.FileAppendText _ -> true | _ -> false in
  Result.bind (resolveReg dest) (fun destReg ->
      let resolvePathToR10 =
        preparePointer ctx X.R10
          (fun () ->
            Error
              "FileWriteBlob/FileAppendText path operand must be a string \
               pointer or string literal")
          path
      in
      let resolveContentToR9 =
        preparePointer ctx X.R9
          (fun () ->
            Error
              "FileWriteBlob/FileAppendText content operand must be a string \
               pointer or string literal")
          content
      in
      let copyLabel = freshLabel "fw_copy" in
      let doneLabel = freshLabel "fw_done" in
      let errorLabel = freshLabel "fw_err" in
      let cleanupLabel = freshLabel "fw_clean" in
      let openSyscall = Int64.of_int syscalls.Platform.open_ in
      let writeSyscall = Int64.of_int syscalls.Platform.write in
      let closeSyscall = Int64.of_int syscalls.Platform.close in
      let openFlags = if isAppend then 1089L else 577L in
      Result.bind resolvePathToR10 (fun pathSetup ->
          Result.map
            (fun contentSetup ->
              pathSetup @ contentSetup
              @ saves (* Allocate 4096 bytes on stack for path (PATH_MAX) *)
              @ [ X.SUB_imm (X.RSP, 4096l) ]
                (* Copy path to null-terminated stack buffer *)
              @ [
                  X.MOV_load (X.RCX, X.R10, 8l);
                  X.LEA (X.RSI, X.R10, 16l);
                  X.MOV_reg (X.RDI, X.RSP);
                ]
              @ loadImm64 X.R10 0L
              @ [
                  X.Label copyLabel;
                  X.CMP_reg (X.R10, X.RCX);
                  X.Jcc (X.GE, doneLabel);
                  X.MOV_reg (scratch, X.RSI);
                  X.ADD_reg (scratch, X.R10);
                  X.MOV_load_byte (scratch, scratch, 0l);
                  X.MOV_reg (X.R8, X.RDI);
                  X.ADD_reg (X.R8, X.R10);
                  X.MOV_store_byte (X.R8, 0l, scratch);
                  X.ADD_imm (X.R10, 1l);
                  X.JMP copyLabel;
                  X.Label doneLabel;
                ]
                (* Null-terminate *)
              @ [ X.MOV_reg (scratch, X.RDI); X.ADD_reg (scratch, X.RCX) ]
              @ loadImm64 X.R10 0L
              @ [ X.MOV_store_byte (scratch, 0l, X.R10) ]
                (* open(path, flags, mode=0666) *)
              @ [ X.MOV_reg (X.RDI, X.RSP) ]
              @ loadImm64 X.RSI openFlags @ loadImm64 X.RDX 0o666L
              @ loadImm64 X.RAX openSyscall
              @ [ X.SYSCALL ]
              @ [ X.CMP_imm (X.RAX, 0l); X.Jcc (X.LT, errorLabel) ]
                (* Save fd in R8 *)
              @ [ X.MOV_reg (X.R8, X.RAX) ]
              (* write(fd, content_data, content_len) *)
              (* R9 = content heap string (saved by PUSH above, load from stack) *)
              (* R9 was pushed at position 5 (index from top after SUB): need to recalculate *)
              (* After pushes (6 * 8 = 48) + SUB 4096 = 4144 bytes below original RSP *)
              (* R9 was the last push, so at [RSP + 4096 + 0] = [RSP + 4096] *)
              @ [ X.MOV_load (X.R9, X.RSP, 4096l) ] (* reload R9 (content) *)
              @ [ X.MOV_reg (X.RDI, X.R8) ] (* fd *)
              @ [ X.LEA (X.RSI, X.R9, 16l) ]
              @ [ X.MOV_load (X.RDX, X.R9, 8l) ]
              @ loadImm64 X.RAX writeSyscall
              @ [ X.SYSCALL ] (* close(fd) *)
              @ [ X.MOV_reg (X.RDI, X.R8) ]
              @ loadImm64 X.RAX closeSyscall
              @ [ X.SYSCALL ]
                (* Allocate Result Ok: [tag=0:8][payload=0:8][refcount=1:8] *)
              @ [ X.MOV_reg (scratch, heapPtr); X.ADD_imm (heapPtr, 24l) ]
              @ loadImm64 X.RCX 0L
              @ [
                  X.MOV_store (scratch, 0l, X.RCX);
                  (* tag = 0 (Ok) *)
                  X.MOV_store (scratch, 8l, X.RCX);
                ]
                (* payload = 0 (Unit) *)
              @ loadImm64 X.RCX 1L
              @ [
                  X.MOV_store (scratch, 16l, X.RCX);
                  (* refcount *)
                  X.MOV_reg (X.RAX, scratch);
                ]
              @ genLeakCounterInc ctx
              @ [ X.JMP cleanupLabel ] (* === Error path === *)
              @ [ X.Label errorLabel ]
              @ [ X.MOV_reg (X.R10, heapPtr); X.ADD_imm (heapPtr, 24l) ]
              @ loadImm64 scratch 1L
              @ [ X.MOV_store (X.R10, 0l, scratch) ]
              @ loadImm64 scratch 5L
              @ [ X.MOV_store (X.R10, 8l, scratch) ]
              @ loadImm64 scratch 0x726F727245L (* "Error" *)
              @ [ X.MOV_store (X.R10, 16l, scratch) ]
              @ genLeakCounterInc ctx
              @ [ X.MOV_reg (scratch, heapPtr); X.ADD_imm (heapPtr, 24l) ]
              @ loadImm64 X.RCX 1L
              @ [
                  X.MOV_store (scratch, 0l, X.RCX);
                  X.MOV_store (scratch, 8l, X.R10);
                ]
              @ loadImm64 X.RCX 1L
              @ [
                  X.MOV_store (scratch, 16l, X.RCX); X.MOV_reg (X.RAX, scratch);
                ]
              @ genLeakCounterInc ctx (* === Cleanup === *)
              @ [ X.Label cleanupLabel; X.ADD_imm (X.RSP, 4096l) ]
              @ restores
              @ [ X.MOV_reg (destReg, X.RAX) ])
            resolveContentToR9))

(*
   Save clobbered registers (access syscall uses RDI, RSI, RAX + copy uses RCX, R10)
*)
let emitFileExists ctx dest path =
  Result.bind (resolveReg dest) (fun destReg ->
      let resolvePathToR10 =
        preparePointer ctx X.R10 (fun () -> Ok (loadImm64 X.R10 0L)) path
      in
      let copyLabel = freshLabel "fe_copy" in
      let doneLabel = freshLabel "fe_done" in
      Result.map
        (fun pathSetup ->
          let accessSyscall = Int64.of_int syscalls.Platform.access in
          let registers = [ X.RDI; X.RSI; X.RCX; X.R10 ] in
          let saves = List.map (fun r -> X.PUSH r) registers in
          let restores = List.map (fun r -> X.POP r) (List.rev registers) in
          pathSetup
          @ saves
            (* Allocate 4096 bytes on stack for null-terminated path (PATH_MAX) *)
          @ [ X.SUB_imm (X.RSP, 4096l) ]
          (* R10 points to [refcount][length][data]. *)
          (* RSI = string data addr, RDI = stack buf, RCX = length, R11 = counter *)
          @ [
              X.MOV_load (X.RCX, X.R10, 8l);
              X.LEA (X.RSI, X.R10, 16l);
              X.MOV_reg (X.RDI, X.RSP);
            ]
          (* RDI = stack buf *)
          (* Copy loop using R11 (scratch) as counter *)
          @ loadImm64 X.R10 0L
            (* R10 = counter (reuse R10 since string ptr no longer needed) *)
          @ [
              X.Label copyLabel;
              X.CMP_reg (X.R10, X.RCX);
              X.Jcc (X.GE, doneLabel);
              (* scratch = [RSI + R10] *)
              X.MOV_reg (scratch, X.RSI);
              X.ADD_reg (scratch, X.R10);
              X.MOV_load_byte (scratch, scratch, 0l);
              (* [RDI + R10] = byte *)
              X.PUSH X.R8;
              X.MOV_reg (X.R8, X.RDI);
              X.ADD_reg (X.R8, X.R10);
              X.MOV_store_byte (X.R8, 0l, scratch);
              X.POP X.R8;
              X.ADD_imm (X.R10, 1l);
              X.JMP copyLabel;
              X.Label doneLabel;
            ]
            (* Null-terminate: [RDI + RCX] = 0 *)
          @ [ X.MOV_reg (scratch, X.RDI); X.ADD_reg (scratch, X.RCX) ]
          @ loadImm64 X.R10 0L
          @ [ X.MOV_store_byte (scratch, 0l, X.R10) ]
            (* syscall: access(path=RSP, mode=F_OK=0) *)
          @ [ X.MOV_reg (X.RDI, X.RSP) ]
          @ loadImm64 X.RSI 0L
          @ loadImm64 X.RAX accessSyscall
          @ [
              X.SYSCALL;
              (* RAX = 0 if exists, negative otherwise *)
              (* Convert to boolean in R10 (safe temp, will be popped later but unused) *)
              X.CMP_imm (X.RAX, 0l);
              X.SETcc (X.EQ, X.RAX);
              X.MOVZX_byte (X.RAX, X.RAX);
              X.ADD_imm (X.RSP, 4096l);
            ]
          @ restores
            (* Move result to destReg after restoring saved registers *)
          @ [ X.MOV_reg (destReg, X.RAX) ])
        resolvePathToR10)

let emitPathUnitOperation ctx dest path createDirectory =
  Result.bind (resolveReg dest) (fun destReg ->
      let pathSetup =
        preparePointer ctx X.R10
          (fun () ->
            Error
              "FileDelete path operand must be a string pointer or string \
               literal")
          path
      in
      let copyLabel = freshLabel "fd_copy" in
      let copyDoneLabel = freshLabel "fd_copy_done" in
      let errorLabel = freshLabel "fd_error" in
      let cleanupLabel = freshLabel "fd_cleanup" in
      Result.map
        (fun setup ->
          setup
          @ [
              X.PUSH X.RDI;
              X.PUSH X.RSI;
              X.PUSH X.RCX;
              X.PUSH X.R10;
              X.SUB_imm (X.RSP, 4096l);
              X.MOV_load (X.RCX, X.R10, 8l);
              X.LEA (X.RSI, X.R10, 16l);
              X.MOV_reg (X.RDI, X.RSP);
              X.XOR_reg (X.R10, X.R10);
              X.Label copyLabel;
              X.CMP_reg (X.R10, X.RCX);
              X.Jcc (X.GE, copyDoneLabel);
              X.MOV_reg (scratch, X.RSI);
              X.ADD_reg (scratch, X.R10);
              X.MOV_load_byte (scratch, scratch, 0l);
              X.MOV_reg (X.RAX, X.RDI);
              X.ADD_reg (X.RAX, X.R10);
              X.MOV_store_byte (X.RAX, 0l, scratch);
              X.ADD_imm (X.R10, 1l);
              X.JMP copyLabel;
              X.Label copyDoneLabel;
              X.MOV_reg (scratch, X.RDI);
              X.ADD_reg (scratch, X.RCX);
              X.XOR_reg (X.R10, X.R10);
              X.MOV_store_byte (scratch, 0l, X.R10);
              X.MOV_reg (X.RDI, X.RSP);
            ]
          @ (if createDirectory then loadImm64 X.RSI 0o777L else [])
          @ loadImm64 X.RAX
              (if createDirectory then 83L
               else Int64.of_int syscalls.Platform.unlink)
          @ [
              X.SYSCALL;
              X.CMP_imm (X.RAX, 0l);
              X.Jcc (X.LT, errorLabel);
              X.MOV_reg (X.RAX, heapPtr);
              X.ADD_imm (heapPtr, 24l);
              X.XOR_reg (X.RCX, X.RCX);
              X.MOV_store (X.RAX, 0l, X.RCX);
              X.MOV_store (X.RAX, 8l, X.RCX);
              X.MOV_imm32 (X.RCX, 1l);
              X.MOV_store (X.RAX, 16l, X.RCX);
            ]
          @ genLeakCounterInc ctx
          @ [
              X.JMP cleanupLabel;
              X.Label errorLabel;
              X.MOV_reg (X.R10, heapPtr);
              X.ADD_imm (heapPtr, 24l);
            ]
          @ loadImm64 X.RCX 1L
          @ [ X.MOV_store (X.R10, 0l, X.RCX) ]
          @ loadImm64 X.RCX 5L
          @ [ X.MOV_store (X.R10, 8l, X.RCX) ]
          @ loadImm64 X.RCX 0x726F727245L
          @ [
              X.MOV_store (X.R10, 16l, X.RCX);
              X.MOV_reg (X.RAX, heapPtr);
              X.ADD_imm (heapPtr, 24l);
            ]
          @ loadImm64 X.RCX 1L
          @ [
              X.MOV_store (X.RAX, 0l, X.RCX);
              X.MOV_store (X.RAX, 8l, X.R10);
              X.MOV_store (X.RAX, 16l, X.RCX);
            ]
          @ genLeakCounterInc ctx @ genLeakCounterInc ctx
          @ [
              X.Label cleanupLabel;
              X.ADD_imm (X.RSP, 4096l);
              X.POP X.R10;
              X.POP X.RCX;
              X.POP X.RSI;
              X.POP X.RDI;
              X.MOV_reg (destReg, X.RAX);
            ])
        pathSetup)

let emitFileDelete ctx dest path = emitPathUnitOperation ctx dest path false

let emitFileCreateDirectory ctx dest path =
  emitPathUnitOperation ctx dest path true

let emitFileSetExecutable (_ctx : funcCtx) dest =
  Result.map (fun destReg -> loadImm64 destReg 0L) (resolveReg dest)

let emitFileWriteFromPtr (_ctx : funcCtx) dest =
  Result.map (fun destReg -> loadImm64 destReg 0L) (resolveReg dest)
