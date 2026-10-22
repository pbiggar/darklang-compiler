(* X64Fibers.ml - VM-backed stacks and stackful native task context transitions. *)
[@@@warning "-4"]

open X64Operands
open X64CodeGenTypes
module X = X86_64

(* Leave heapPtr/freeListBase shared. Restoring old allocator cursors from a
   suspended task would overwrite allocations performed by another task. *)
let saved = [ (X.RBP, 8); (X.RBX, 16); (X.R12, 24); (X.R13, 32) ]

let floats =
  [
    X.XMM0;
    X.XMM1;
    X.XMM2;
    X.XMM3;
    X.XMM4;
    X.XMM5;
    X.XMM6;
    X.XMM7;
    X.XMM8;
    X.XMM9;
    X.XMM10;
    X.XMM11;
    X.XMM12;
    X.XMM13;
    X.XMM14;
    X.XMM15;
  ]

let guard = 65536

(* RDI/RSI are the outgoing/incoming contexts. The saved SP includes a native
   continuation address. An ordinary return on the incoming stack resumes it.
   LIR's normal native-effect call barrier preserves caller-owned GP values. *)
let switch resume =
  let current = freshLabel "fiber_current_task" in
  let incomingTask = freshLabel "fiber_incoming_task" in
  [
    X.MOV_reg (X.R10, X.RSI);
    X.LEA_rip (X.RAX, resume);
    X.PUSH X.RAX;
    X.MOV_store (X.RDI, 0l, X.RSP);
    (* Scheduler contexts have no parent. Publish zero while completion
       callbacks run there, and the task identity before its stack resumes. *)
    X.MOV_load (X.RAX, X.R10, 56l);
    X.TEST_reg (X.RAX, X.RAX);
    X.Jcc (X.NE, incomingTask);
    (* Eager operation starts may enter a child directly. Record the actual
       returning task rather than assuming it was the scheduler's last pick.
       Slot 40 is an entry closure in task contexts and borrowed here. *)
    X.MOV_store (X.R10, 40l, X.RDI);
    X.JMP current;
    X.Label incomingTask;
    X.MOV_reg (X.RAX, X.R10);
    X.Label current;
    X.MOV_store (freeListBase, Int32.of_int currentTaskOffset, X.RAX);
  ]
  @ List.map
      (fun (reg, offset) -> X.MOV_store (X.RDI, Int32.of_int offset, reg))
      saved
  @ List.mapi
      (fun index reg ->
        X.MOVSD_store (X.RDI, Int32.of_int (72 + (index * 8)), reg))
      floats
  @ List.mapi
      (fun index reg ->
        X.MOVSD_load (reg, X.R10, Int32.of_int (72 + (index * 8))))
      floats
  @ [ X.MOV_reg (X.RDI, X.R10); X.MOV_load (X.RSP, X.R10, 0l) ]
  @ List.map
      (fun (reg, offset) -> X.MOV_load (reg, X.R10, Int32.of_int offset))
      saved
  @ [ X.XOR_reg (X.RAX, X.RAX); X.RET; X.Label resume ]

let emit ctx dest operation =
  let finish = if dest = X.RAX then [] else [ X.MOV_reg (dest, X.RAX) ] in
  match operation with
  | LIR.FiberCurrent ->
      Ok [ X.MOV_load (dest, freeListBase, Int32.of_int currentTaskOffset) ]
  | LIR.FiberSetCurrent ->
      Ok
        ([
           X.MOV_store (freeListBase, Int32.of_int currentTaskOffset, X.RDI);
           X.XOR_reg (X.RAX, X.RAX);
         ]
        @ finish)
  | LIR.FiberSwitch -> Ok (switch (freshLabel "fiber_resume") @ finish)
  | LIR.FiberPrepare ->
      let start = freshLabel "fiber_start" in
      let doneLabel = freshLabel "fiber_prepared" in
      let returned = freshLabel "fiber_completed_resumed" in
      let root = freshLabel "fiber_root_frame" in
      let rootFound = freshLabel "fiber_root_found" in
      Ok
        ([
           X.ADD_imm (X.RDX, Int32.of_int guard);
           X.ADD_reg (X.RDX, X.RSI);
           X.SUB_imm (X.RDX, 16l);
           X.MOV_store (X.RDI, 0l, X.RDX);
           X.LEA_rip (X.RAX, start);
           X.MOV_store (X.RDX, 0l, X.RAX);
           X.XOR_reg (X.RAX, X.RAX);
           X.MOV_store (X.RDX, 8l, X.RAX);
         ]
        @ List.map
            (fun (_, offset) -> X.MOV_store (X.RDI, Int32.of_int offset, X.RAX))
            saved
        @ List.mapi
            (fun index _ ->
              X.MOV_store (X.RDI, Int32.of_int (72 + (index * 8)), X.RAX))
            floats
        (* Preserve the original process root in the frame chain, so native
           argv/startup helpers still work from a newly entered fiber. *)
        @ [
            X.MOV_reg (X.R10, X.RBP);
            X.Label root;
            X.MOV_load (X.RAX, X.R10, 0l);
            X.TEST_reg (X.RAX, X.RAX);
            X.Jcc (X.EQ, rootFound);
            X.MOV_reg (X.R10, X.RAX);
            X.JMP root;
            X.Label rootFound;
            X.MOV_store (X.RDI, 8l, X.R10);
            X.JMP doneLabel;
            X.Label start;
            X.PUSH X.RBP;
            X.MOV_reg (X.RBP, X.RSP);
            X.MOV_reg (X.R12, X.RDI);
            (* Dark closures receive their environment in X0/RAX and the
               first explicit argument in X1/RDI. *)
            X.MOV_load (X.RAX, X.RDI, 40l);
            X.MOV_load (X.R11, X.RAX, 0l);
            X.CALL_reg X.R11;
            X.MOV_imm32 (X.RAX, 4l);
            X.MOV_store (X.R12, 64l, X.RAX);
            X.MOV_reg (X.RDI, X.R12);
            X.MOV_load (X.RSI, X.R12, 56l);
          ]
        @ switch returned @ loadImm64 X.RDI 70L @ loadImm64 X.RAX 60L
        @ [ X.SYSCALL; X.Label doneLabel; X.XOR_reg (X.RAX, X.RAX) ]
        @ finish)
  | LIR.FiberStackAlloc ->
      Ok
        ([
           X.CMP_imm (X.RSI, 65536l);
           X.Jcc (X.LT, oomHandlerLabel);
           X.CMP_imm (X.RSI, 1073741824l);
           X.Jcc (X.GT, oomHandlerLabel);
           X.MOV_reg (X.RAX, X.RSI);
           X.AND_imm (X.RAX, 65535l);
           X.Jcc (X.NE, oomHandlerLabel);
           X.PUSH X.RSI;
           X.ADD_imm (X.RSI, Int32.of_int (guard * 2));
         ]
        @ loadImm64 X.RDI 0L @ loadImm64 X.RDX 0L @ loadImm64 X.R10 0x22L
        @ loadImm64 X.R8 (-1L) @ loadImm64 X.R9 0L @ loadImm64 X.RAX 9L
        @ [
            X.SYSCALL;
            X.CMP_imm (X.RAX, 0l);
            X.Jcc (X.LT, oomHandlerLabel);
            X.POP X.RSI;
            X.PUSH X.RAX;
            X.LEA (X.RDI, X.RAX, Int32.of_int guard);
          ]
        @ loadImm64 X.RDX 3L @ loadImm64 X.RAX 10L
        @ [
            X.SYSCALL;
            X.TEST_reg (X.RAX, X.RAX);
            X.Jcc (X.NE, oomHandlerLabel);
            X.POP X.RAX;
          ]
        @ genLeakCounterInc ctx @ finish)
  | LIR.FiberStackFree ->
      Ok
        ([ X.ADD_imm (X.RSI, Int32.of_int (guard * 2)) ]
        @ loadImm64 X.RAX 11L
        @ [
            X.SYSCALL; X.TEST_reg (X.RAX, X.RAX); X.Jcc (X.NE, oomHandlerLabel);
          ]
        @ genLeakCounterDec ctx
        @ [ X.XOR_reg (X.RAX, X.RAX) ]
        @ finish)
  | _ -> Error "x64 fiber emitter received a non-fiber operation"
