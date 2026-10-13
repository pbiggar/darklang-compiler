(* X64EmitNativeEffects.ml - Emit x64 instructions for nativeeffects operations. *)
[@@@warning "-4"]

open X64Operands
open X64CodeGenTypes
module X = X86_64

let bind f value = Result.bind value f

let emitCoverageHit (_ctx : X64CodeGenTypes.funcCtx) =
  Ok [] (*  Coverage instrumentation not supported on x86_64 yet *)

let emitRandomInt64 (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) =
  (*  getrandom(buf, 8, 0) syscall *)
  resolveReg dest
  |> Result.map (fun destReg ->
      let clobbered = [ X.RAX; X.RDI; X.RSI; X.RDX; X.RCX; scratch ] in
      let preserved = List.filter (( <> ) destReg) clobbered in
      let saves = List.map (fun r -> X.PUSH r) preserved in
      let restores = preserved |> List.rev |> List.map (fun r -> X.POP r) in
      saves
      @ [ X.SUB_imm (X.RSP, 8l) ]
      @ [ X.MOV_reg (X.RDI, X.RSP) ] (*  buf *)
      @ loadImm64 X.RSI 8L (*  len = 8 *)
      @ loadImm64 X.RDX 0L (*  flags = 0 *)
      @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.getrandom)
      @ [ X.SYSCALL; X.MOV_load (destReg, X.RSP, 0l); X.ADD_imm (X.RSP, 8l) ]
      @ restores)

let emitDateTimeNow (_ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg) =
  (*  clock_gettime(CLOCK_REALTIME=0, &ts), converted to 100ns Unix ticks. *)
  resolveReg dest
  |> Result.map (fun destReg ->
      (* DateTimeNow is not a call barrier in register allocation. Preserve
           syscall and division registers just as RandomInt64 does. *)
      let preserved =
        List.filter (( <> ) destReg)
          [ X.RAX; X.RDI; X.RSI; X.RDX; X.RCX; scratch ]
      in
      List.map (fun reg -> X.PUSH reg) preserved
      @ [ X.SUB_imm (X.RSP, 16l) ] (*  timespec: tv_sec(8) + tv_nsec(8) *)
      @ loadImm64 X.RDI 0L (*  CLOCK_REALTIME *)
      @ [ X.MOV_reg (X.RSI, X.RSP) ]
      @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.gettimeofday)
      @ [
          X.SYSCALL;
          X.MOV_load (scratch, X.RSP, 0l);
          X.IMUL_imm (scratch, scratch, 10000000l);
          X.MOV_load (X.RAX, X.RSP, 8l);
          X.CQO;
        ]
      @ loadImm64 X.RDI 100L
      @ [
          X.IDIV X.RDI;
          X.ADD_reg (scratch, X.RAX);
          X.MOV_reg (destReg, scratch);
          X.ADD_imm (X.RSP, 16l);
        ]
      @ List.map (fun reg -> X.POP reg) (List.rev preserved))

let emitSleep (ctx : X64CodeGenTypes.funcCtx) (effectId : int)
    (delayMs : LIR.fReg) =
  match delayMs with
  | LIR.FPhysical physicalDelay ->
      let delayReg = lirFRegToX86 physicalDelay in
      let label suffix =
        Printf.sprintf "__sleep_%s_%d_%s" ctx.functionName effectId suffix
      in
      let retryLabel = label "retry" in
      let interruptedLabel = label "interrupted" in
      let releaseLabel = label "release" in
      let completeLabel = label "complete" in
      let millionBits = Int64.bits_of_float 1000000.0 in
      Ok
        (withPreservedFloatScratch [ delayReg ] (fun temp ->
             loadImm64 scratch millionBits
             @ [
                 X.MOVQ_from_gp (temp, scratch);
                 X.MULSD (temp, delayReg);
                 X.CVTTSD2SI (scratch, temp);
               ])
        @ [
            X.CMP_imm (scratch, 0l);
            X.Jcc (X.LE, completeLabel);
            X.SUB_imm (X.RSP, 32l);
            X.MOV_reg (X.RAX, scratch);
            X.CQO;
          ]
        @ loadImm64 X.R10 1000000000L
        @ [
            X.IDIV X.R10;
            X.MOV_store (X.RSP, 0l, X.RAX);
            X.MOV_store (X.RSP, 8l, X.RDX);
          ]
        @ loadImm64 X.R10 0L
        @ [
            X.MOV_store (X.RSP, 16l, X.R10);
            X.MOV_store (X.RSP, 24l, X.R10);
            X.Label retryLabel;
            X.LEA (X.RDI, X.RSP, 0l);
            X.LEA (X.RSI, X.RSP, 16l);
          ]
        @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.nanosleep)
        @ [
            X.SYSCALL;
            X.CMP_imm (X.RAX, -4l);
            X.Jcc (X.EQ, interruptedLabel);
            X.JMP releaseLabel;
            X.Label interruptedLabel;
            X.MOV_load (X.R10, X.RSP, 16l);
            X.MOV_store (X.RSP, 0l, X.R10);
            X.MOV_load (X.R10, X.RSP, 24l);
            X.MOV_store (X.RSP, 8l, X.R10);
            X.JMP retryLabel;
            X.Label releaseLabel;
            X.ADD_imm (X.RSP, 32l);
            X.Label completeLabel;
          ])
  | _ -> Error "Sleep with virtual float register"

let emitCliNative (ctx : X64CodeGenTypes.funcCtx) (dest : LIR.reg)
    (operation : LIR.cliOperation) (args : LIR.operand list) =
  resolveReg dest
  |> bind (fun destReg ->
      let loadCliOperand dest operand =
        match operand with
        | LIR.Imm value -> Ok (loadImm64 dest value)
        | LIR.Reg source ->
            resolveReg source
            |> Result.map (fun sourceReg ->
                if sourceReg = dest then [] else [ X.MOV_reg (dest, sourceReg) ])
        | LIR.StackSlot offset ->
            Ok
              [
                X.MOV_load
                  ( dest,
                    X.RBP,
                    Int32.of_int
                      (X64InstructionContext.adjustStackOffset ctx offset) );
              ]
        | LIR.StringSymbol value -> Ok (emitStringLiteral dest value)
        | _ -> Error "CLI native operation received a non-integer operand"
        (*  Capture every source before populating syscall registers: later *)
        (*  operands may currently live in an earlier destination register. *)
      in
      let loadSocketArgs operands targets =
        operands
        |> List.mapi (fun index operand -> (index, operand))
        |> List.fold_left
             (fun loaded (index, operand) ->
               loaded
               |> bind (fun code ->
                   (* Spill reloads can put an incoming operand in R11 itself.
                      Preserve its original value before collecting earlier
                      operands, or ppoll's timeout can become NULL. *)
                   let next =
                     match operand with
                     | LIR.Reg source ->
                         resolveReg source
                         |> bind (fun sourceReg ->
                             if sourceReg = X.R11 then
                               Ok
                                 [
                                   X.MOV_load
                                     (X.R11, X.RSP, Int32.of_int (index * 8));
                                 ]
                             else loadCliOperand X.R11 operand)
                     | _ -> loadCliOperand X.R11 operand
                   in
                   next
                   |> Result.map (fun next -> code @ next @ [ X.PUSH X.R11 ])))
             (Ok [ X.PUSH X.R11 ])
        |> Result.map (fun code ->
            code
            @ (targets |> List.rev |> List.map (fun r -> X.POP r))
            @ [ X.ADD_imm (X.RSP, 8l) ])
      in
      match operation with
      | LIR.PosixOpenAt | LIR.PosixRead | LIR.PosixWrite | LIR.PosixClose
      | LIR.PosixSeek | LIR.PosixStatAt | LIR.PosixGetCwd | LIR.PosixChdir
      | LIR.PosixMkdirAt | LIR.PosixUnlinkAt | LIR.PosixRenameAt
      | LIR.PosixChmodAt | LIR.PosixChmodAt2 | LIR.PosixUtimesAt
      | LIR.PosixSetAttributesAt | LIR.PosixSymlinkAt | LIR.PosixReadlinkAt
      | LIR.PosixAccessAt | LIR.PosixFlock | LIR.PosixGetDents | LIR.PosixIoctl
      | LIR.PosixProcInfo | LIR.PosixCopyFileRange | LIR.PosixFtruncate
      | LIR.PosixFchmod | LIR.PosixFstat | LIR.PosixPread
      | LIR.PosixPwrite | LIR.PosixFclonefileat | LIR.PosixGetentropy
      | LIR.PosixUname | LIR.PosixSysctl | LIR.PosixSchedGetaffinity
      | LIR.PosixFcntlGetPath | LIR.PosixClockGettime | LIR.PosixGettimeofday
      | LIR.PosixNanosleep | LIR.PosixMachTimebaseInfo | LIR.PosixInotifyInit1
      | LIR.PosixInotifyAddWatch | LIR.PosixPpoll | LIR.PosixPollDarwin
      | LIR.PosixKqueue | LIR.PosixKeventVnode | LIR.PosixWait4 -> (
          let number, arity =
            match operation with
            | LIR.PosixOpenAt -> (Some 257, 4)
            | LIR.PosixRead -> (Some 0, 3)
            | LIR.PosixWrite -> (Some 1, 3)
            | LIR.PosixClose -> (Some 3, 1)
            | LIR.PosixSeek -> (Some 8, 3)
            | LIR.PosixStatAt -> (Some 262, 4)
            | LIR.PosixGetCwd -> (Some 79, 2)
            | LIR.PosixChdir -> (Some 81, 1)
            | LIR.PosixMkdirAt -> (Some 258, 3)
            | LIR.PosixUnlinkAt -> (Some 263, 3)
            | LIR.PosixRenameAt -> (Some 264, 4)
            | LIR.PosixChmodAt -> (Some 268, 4)
            | LIR.PosixChmodAt2 -> (Some 452, 4)
            | LIR.PosixUtimesAt -> (Some 280, 4)
            | LIR.PosixSetAttributesAt -> (None, 6)
            | LIR.PosixSymlinkAt -> (Some 266, 3)
            | LIR.PosixReadlinkAt -> (Some 267, 4)
            | LIR.PosixAccessAt -> (Some 269, 4)
            | LIR.PosixFlock -> (Some 73, 2)
            | LIR.PosixGetDents -> (Some 217, 4)
            | LIR.PosixIoctl -> (Some 16, 3)
            | LIR.PosixProcInfo -> (None, 6)
            | LIR.PosixCopyFileRange -> (Some 326, 6)
            | LIR.PosixFtruncate -> (Some 77, 2)
            | LIR.PosixFchmod -> (Some 91, 2)
            | LIR.PosixFstat -> (Some 5, 2)
            | LIR.PosixPread -> (Some 17, 4)
            | LIR.PosixPwrite -> (Some 18, 4)
            | LIR.PosixFclonefileat -> (None, 4)
            | LIR.PosixGetentropy -> (Some 318, 3)
            | LIR.PosixUname -> (Some 63, 1)
            | LIR.PosixSysctl -> (None, 6)
            | LIR.PosixSchedGetaffinity -> (Some 204, 3)
            | LIR.PosixFcntlGetPath -> (None, 3)
            | LIR.PosixClockGettime -> (Some 228, 2)
            | LIR.PosixGettimeofday -> (Some 96, 3)
            | LIR.PosixNanosleep -> (Some 35, 2)
            | LIR.PosixMachTimebaseInfo -> (None, 1)
            | LIR.PosixInotifyInit1 -> (Some 294, 1)
            | LIR.PosixInotifyAddWatch -> (Some 254, 3)
            | LIR.PosixPpoll -> (Some 271, 5)
            | LIR.PosixPollDarwin -> (None, 3)
            | LIR.PosixKqueue -> (None, 0)
            | LIR.PosixWait4 -> (Some 61, 4)
            | LIR.PosixKeventVnode -> (None, 6)
            | _ -> Crash.crash "Non-POSIX operation in POSIX lowering"
          in
          if List.length args <> arity then
            Error "POSIX primitive: invalid argument count"
          else
            match number with
            | None -> Ok (loadImm64 destReg (-38L))
            | Some number ->
                let targets =
                  [ X.RDI; X.RSI; X.RDX; X.R10; X.R8; X.R9 ]
                  |> List.filteri (fun i _ -> i < arity)
                in
                loadSocketArgs args targets
                |> Result.map (fun loads ->
                    loads
                    @ loadImm64 X.RAX (Int64.of_int number)
                    @ [ X.SYSCALL ]
                    @
                    if destReg = X.RAX then []
                    else [ X.MOV_reg (destReg, X.RAX) ]))
      | LIR.HostOS -> Ok (loadImm64 destReg 1L)
      | LIR.HostArchitecture -> Ok (loadImm64 destReg 1L)
      | LIR.StartupStack ->
          let loop = freshLabel ("startup_stack_" ^ ctx.functionName) in
          let doneLabel = freshLabel "startup_stack_done" in
          Ok
            [
              X.MOV_reg (X.R10, X.RBP);
              X.Label loop;
              X.MOV_load (X.R11, X.R10, 0l);
              X.CMP_imm (X.R11, 0l);
              X.Jcc (X.EQ, doneLabel);
              X.MOV_reg (X.R10, X.R11);
              X.JMP loop;
              X.Label doneLabel;
              X.LEA (destReg, X.R10, 8l);
            ]
      | LIR.ExecutableState ->
          Ok
            [
              X.LEA
                ( destReg,
                  freeListBase,
                  Int32.of_int (freeListSize + processTableSize + 512) );
            ]
      | LIR.ChildState ->
          Ok [ X.LEA (destReg, freeListBase, Int32.of_int processTableOffset) ]
      | LIR.ChildLaunch ->
          loadSocketArgs args [ X.RDI; X.RSI; X.RDX; X.RCX ]
          |> Result.map (fun loads ->
              loads
              @ [ X.CALL "__dark_child_launch" ]
              @ if destReg = X.RAX then [] else [ X.MOV_reg (destReg, X.RAX) ])
      | LIR.GetPid ->
          Ok
            (loadImm64 X.RAX 39L @ [ X.SYSCALL ]
            @ if destReg = X.RAX then [] else [ X.MOV_reg (destReg, X.RAX) ])
      | LIR.SecureRandomFill -> (
          match args with
          | [ buffer; length ] ->
              loadSocketArgs [ buffer; length ] [ X.RDI; X.RSI ]
              |> Result.map (fun loads ->
                  loads @ loadImm64 X.RDX 0L
                  @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.getrandom)
                  @ [ X.SYSCALL ]
                  @
                  if destReg = X.RAX then [] else [ X.MOV_reg (destReg, X.RAX) ])
          | _ -> Error "SecureRandomFill expects buffer and length")
      | LIR.SocketTcp4 | LIR.SocketTcp6 | LIR.SocketUdp4 | LIR.SocketUdp6 ->
          let constants = Platform.socketConstantsFor Platform.Linux in
          let family =
            if operation = LIR.SocketTcp6 || operation = LIR.SocketUdp6 then
              constants.Platform.addressFamily6
            else constants.Platform.addressFamily4
          in
          let isUdp =
            operation = LIR.SocketUdp4 || operation = LIR.SocketUdp6
          in
          let kind =
            if isUdp then constants.Platform.datagramType
            else constants.Platform.streamType
          in
          let protocol = if isUdp then 17L else 6L in
          Ok
            (loadImm64 X.RDI (Int64.of_int family)
            @ loadImm64 X.RSI kind @ loadImm64 X.RDX protocol
            @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.socket)
            @ [ X.SYSCALL ]
            @ if destReg = X.RAX then [] else [ X.MOV_reg (destReg, X.RAX) ])
      | LIR.SocketConnect4 | LIR.SocketConnect6 | LIR.SocketSend
      | LIR.SocketSendTo | LIR.SocketReceive | LIR.SocketReceiveFrom
      | LIR.SocketReceiveTimeout | LIR.SocketSendTimeout -> (
          let finish =
            if destReg = X.RAX then [] else [ X.MOV_reg (destReg, X.RAX) ]
          in
          match (operation, args) with
          | (LIR.SocketConnect4 | LIR.SocketConnect6), [ descriptor; address ]
            ->
              loadSocketArgs [ descriptor; address ] [ X.RDI; X.RSI ]
              |> Result.map (fun loads ->
                  loads
                  @ loadImm64 X.RDX
                      (if operation = LIR.SocketConnect6 then 28L else 16L)
                  @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.connect)
                  @ [ X.SYSCALL ] @ finish)
          | LIR.SocketSend, [ descriptor; blob ] ->
              loadSocketArgs [ descriptor; blob ] [ X.RDI; X.RSI ]
              |> Result.map (fun loads ->
                  loads
                  @ [ X.MOV_load (X.RDX, X.RSI, 8l); X.ADD_imm (X.RSI, 16l) ]
                  @ loadImm64 X.R10
                      (Platform.socketConstantsFor Platform.Linux)
                        .Platform.noSignal
                  @ loadImm64 X.R8 0L @ loadImm64 X.R9 0L
                  @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.sendTo)
                  @ [ X.SYSCALL ] @ finish)
          | LIR.SocketSend, [ descriptor; blob; nonblocking ] ->
              loadSocketArgs
                [ descriptor; blob; nonblocking ]
                [ X.RDI; X.RSI; X.R10 ]
              |> Result.map (fun loads ->
                  let constants = Platform.socketConstantsFor Platform.Linux in
                  loads
                  @ [ X.MOV_load (X.RDX, X.RSI, 8l); X.ADD_imm (X.RSI, 16l) ]
                  @ loadImm64 X.R8 constants.Platform.dontWait
                  @ [ X.IMUL_reg (X.R10, X.R8) ]
                  @ loadImm64 X.R8 constants.Platform.noSignal
                  @ [ X.OR_reg (X.R10, X.R8) ]
                  @ loadImm64 X.R8 0L @ loadImm64 X.R9 0L
                  @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.sendTo)
                  @ [ X.SYSCALL ] @ finish)
          | LIR.SocketSendTo, [ descriptor; blob; address; length ] ->
              loadSocketArgs
                [ descriptor; blob; address; length ]
                [ X.RDI; X.RSI; X.R8; X.R9 ]
              |> Result.map (fun loads ->
                  loads
                  @ [ X.MOV_load (X.RDX, X.RSI, 8l); X.ADD_imm (X.RSI, 16l) ]
                  @ loadImm64 X.R10
                      (Platform.socketConstantsFor Platform.Linux)
                        .Platform.noSignal
                  @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.sendTo)
                  @ [ X.SYSCALL ] @ finish)
          | LIR.SocketReceiveFrom, [ descriptor; buffer; length; peer ] ->
              loadSocketArgs
                [ descriptor; buffer; length; peer ]
                [ X.RDI; X.RSI; X.RDX; X.R8 ]
              |> Result.map (fun loads ->
                  loads
                  @ [ X.MOV_reg (X.R9, X.R8); X.ADD_imm (X.R8, 8l) ]
                  @ loadImm64 X.R10 0L
                  @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.recvFrom)
                  @ [ X.SYSCALL ] @ finish)
          | LIR.SocketReceive, [ descriptor; buffer; length ] ->
              loadSocketArgs
                [ descriptor; buffer; length ]
                [ X.RDI; X.RSI; X.RDX ]
              |> Result.map (fun loads ->
                  loads
                  @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.read)
                  @ [ X.SYSCALL ] @ finish)
          | LIR.SocketReceive, [ descriptor; buffer; length; nonblocking ] ->
              loadSocketArgs
                [ descriptor; buffer; length; nonblocking ]
                [ X.RDI; X.RSI; X.RDX; X.R10 ]
              |> Result.map (fun loads ->
                  loads
                  @ loadImm64 X.R8
                      (Platform.socketConstantsFor Platform.Linux)
                        .Platform.dontWait
                  @ [ X.IMUL_reg (X.R10, X.R8) ]
                  @ loadImm64 X.R8 0L @ loadImm64 X.R9 0L
                  @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.recvFrom)
                  @ [ X.SYSCALL ] @ finish)
          | ( (LIR.SocketReceiveTimeout | LIR.SocketSendTimeout),
              [ descriptor; timeval ] ) ->
              loadSocketArgs [ descriptor; timeval ] [ X.RDI; X.R10 ]
              |> Result.map (fun loads ->
                  let constants = Platform.socketConstantsFor Platform.Linux in
                  let option_ =
                    if operation = LIR.SocketSendTimeout then
                      constants.Platform.sendTimeout
                    else constants.Platform.receiveTimeout
                  in
                  loads
                  @ loadImm64 X.RSI
                      (Int64.of_int constants.Platform.socketLevel)
                  @ loadImm64 X.RDX (Int64.of_int option_)
                  @ loadImm64 X.R8 16L
                  @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.setSockOpt)
                  @ [ X.SYSCALL ] @ finish)
          | _ -> Error "Invalid socket operation arguments")
      | LIR.SocketBind4 | LIR.SocketBind6 | LIR.SocketListen | LIR.SocketAccept
      | LIR.SocketCloexec | LIR.SocketReuseAddress | LIR.SocketPoll
      | LIR.SignalBlock | LIR.SignalRestore | LIR.SignalPending | LIR.SignalWait
      | LIR.MonotonicTime -> (
          let constants = Platform.socketConstantsFor Platform.Linux in
          let finish =
            if destReg = X.RAX then [] else [ X.MOV_reg (destReg, X.RAX) ]
          in
          let emit operands registers setup number =
            loadSocketArgs operands registers
            |> Result.map (fun loads ->
                loads @ setup
                @ loadImm64 X.RAX (Int64.of_int number)
                @ [ X.SYSCALL ] @ finish)
          in
          match (operation, args) with
          | (LIR.SocketBind4 | LIR.SocketBind6), [ descriptor; address ] ->
              emit [ descriptor; address ] [ X.RDI; X.RSI ]
                (loadImm64 X.RDX
                   (if operation = LIR.SocketBind6 then 28L else 16L))
                syscalls.Platform.bind
          | LIR.SocketListen, [ descriptor ] ->
              emit [ descriptor ] [ X.RDI ] (loadImm64 X.RSI 128L)
                syscalls.Platform.listen
          | LIR.SocketAccept, [ descriptor ] ->
              emit [ descriptor ] [ X.RDI ]
                (loadImm64 X.RSI 0L @ loadImm64 X.RDX 0L)
                syscalls.Platform.accept
          | LIR.SocketCloexec, [ descriptor ] ->
              emit [ descriptor ] [ X.RDI ]
                (loadImm64 X.RSI 2L @ loadImm64 X.RDX 1L)
                syscalls.Platform.fcntl
          | LIR.SocketReuseAddress, [ descriptor; enabled ] ->
              emit [ descriptor; enabled ] [ X.RDI; X.R10 ]
                (loadImm64 X.RSI (Int64.of_int constants.Platform.socketLevel)
                @ loadImm64 X.RDX (Int64.of_int constants.Platform.reuseAddress)
                @ loadImm64 X.R8 4L)
                syscalls.Platform.setSockOpt
          | LIR.SocketPoll, [ pollfd; timeout ] ->
              emit [ pollfd; timeout ] [ X.RDI; X.RDX ]
                (loadImm64 X.RSI 1L @ loadImm64 X.R10 0L @ loadImm64 X.R8 8L)
                syscalls.Platform.poll
          | LIR.SocketPoll, [ pollfds; count; timeout ] ->
              emit
                [ pollfds; count; timeout ]
                [ X.RDI; X.RSI; X.RDX ]
                (loadImm64 X.R10 0L @ loadImm64 X.R8 8L)
                syscalls.Platform.poll
          | LIR.SignalBlock, [ mask; previous ] ->
              emit [ mask; previous ] [ X.RSI; X.RDX ]
                (loadImm64 X.RDI (Int64.of_int constants.Platform.blockSignal)
                @ loadImm64 X.R10 8L)
                syscalls.Platform.signalMask
          | LIR.SignalRestore, [ previous ] ->
              emit [ previous ] [ X.RSI ]
                (loadImm64 X.RDI (Int64.of_int constants.Platform.restoreSignal)
                @ loadImm64 X.RDX 0L @ loadImm64 X.R10 8L)
                syscalls.Platform.signalMask
          | LIR.SignalPending, [ mask ] ->
              emit [ mask ] [ X.RDI ] (loadImm64 X.RSI 8L)
                syscalls.Platform.signalPending
          | LIR.SignalWait, [ mask; info ] ->
              emit [ mask; info ] [ X.RDI; X.RSI ]
                (loadImm64 X.RDX 0L @ loadImm64 X.R10 8L)
                syscalls.Platform.signalWait
          | LIR.MonotonicTime, [ time ] ->
              emit [ time ] [ X.RSI ] (loadImm64 X.RDI 1L)
                syscalls.Platform.gettimeofday
          | _ -> Error "Invalid listener or signal operation arguments")
      | LIR.SocketClose -> (
          match args with
          | [ descriptor ] ->
              loadCliOperand X.RDI descriptor
              |> Result.map (fun loads ->
                  loads
                  @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.close)
                  @ [ X.SYSCALL ]
                  @
                  if destReg = X.RAX then [] else [ X.MOV_reg (destReg, X.RAX) ])
          | _ -> Error "SocketClose expects one descriptor")
      | LIR.GetUid ->
          Ok
            (loadImm64 X.RAX 102L @ [ X.SYSCALL ]
            @ if destReg = X.RAX then [] else [ X.MOV_reg (destReg, X.RAX) ])
      | LIR.GetArgv -> (
          match args with
          | [ LIR.Imm index ] when index >= 0L ->
              Ok
                (loadImm64 X.RDI index
                @ [ X.CALL "__dark_cli_argv" ]
                @ if destReg = X.RAX then [] else [ X.MOV_reg (destReg, X.RAX) ]
                )
          | [ LIR.Reg index ] -> (
              match resolveReg index with
              | Ok indexReg ->
                  Ok
                    ([ X.MOV_reg (X.RDI, indexReg); X.CALL "__dark_cli_argv" ]
                    @
                    if destReg = X.RAX then []
                    else [ X.MOV_reg (destReg, X.RAX) ])
              | Error _ -> Ok (loadImm64 destReg 0L))
          | _ -> Ok (loadImm64 destReg 0L))
      | LIR.GetEnv ->
          let loadName =
            match args with
            | [ LIR.Reg name ] -> (
                match resolveReg name with
                | Ok nameReg when nameReg = X.RDI -> []
                | Ok nameReg -> [ X.MOV_reg (X.RDI, nameReg) ]
                | Error _ -> [])
            | [ LIR.StringSymbol name ] -> emitStringLiteral X.RDI name
            | [ LIR.StackSlot offset ] ->
                [
                  X.MOV_load
                    ( X.RDI,
                      X.RBP,
                      Int32.of_int
                        (X64InstructionContext.adjustStackOffset ctx offset) );
                ]
            | _ -> []
          in
          Ok
            (loadName
            @ [ X.CALL "__dark_cli_getenv" ]
            @ if destReg = X.RAX then [] else [ X.MOV_reg (destReg, X.RAX) ])
      | LIR.StdinState -> (
          match args with
          | [ LIR.Imm offset ] when offset = 0L || offset = 128L ->
              Ok
                [
                  X.LEA
                    ( destReg,
                      freeListBase,
                      Int32.of_int
                        (freeListSize + processTableSize + Int64.to_int offset)
                    );
                ]
          | _ -> Error "StdinState expects a fixed state or input-buffer offset"
          )
      | LIR.GetEnvironmentPacked ->
          Ok
            ([ X.CALL "__dark_cli_environment_packed" ]
            @ if destReg = X.RAX then [] else [ X.MOV_reg (destReg, X.RAX) ])
      | LIR.Kill -> (
          let successLabel =
            freshLabel ("kill_" ^ ctx.functionName ^ "_success")
          in
          let completeLabel =
            freshLabel ("kill_" ^ ctx.functionName ^ "_complete")
          in
          let pushOperand operand =
            match operand with
            | LIR.Imm value -> loadImm64 X.RAX value @ [ X.PUSH X.RAX ]
            | LIR.Reg reg -> (
                match resolveReg reg with
                | Ok source -> [ X.PUSH source ]
                | Error _ -> [])
            | LIR.StackSlot offset ->
                [
                  X.MOV_load
                    ( X.RAX,
                      X.RBP,
                      Int32.of_int
                        (X64InstructionContext.adjustStackOffset ctx offset) );
                  X.PUSH X.RAX;
                ]
            | _ -> []
          in
          match args with
          | [ pid; signal ] ->
              Ok
                (pushOperand pid @ pushOperand signal
                @ [ X.POP X.RSI; X.POP X.RDI ]
                @ loadImm64 X.RAX 62L
                @ [
                    X.SYSCALL;
                    X.CMP_imm (X.RAX, 0l);
                    X.Jcc (X.GE, successLabel);
                    X.NEG X.RAX;
                    X.MOV_reg (X.RDX, X.RAX);
                  ]
                @ emitStringLiteral X.R9 "POSIX error"
                @ [
                    X.MOV_reg (X.R8, heapPtr);
                    X.ADD_imm (heapPtr, 24l);
                    X.MOV_store (X.R8, 0l, X.RDX);
                    X.MOV_store (X.R8, 8l, X.R9);
                    X.MOV_imm32 (X.R10, 1l);
                    X.MOV_store (X.R8, 16l, X.R10);
                  ]
                @ genLeakCounterInc ctx
                @ [
                    X.MOV_reg (destReg, heapPtr);
                    X.ADD_imm (heapPtr, 24l);
                    X.MOV_imm32 (X.R10, 1l);
                    X.MOV_store (destReg, 0l, X.R10);
                    X.MOV_store (destReg, 8l, X.R8);
                    X.MOV_store (destReg, 16l, X.R10);
                  ]
                @ genLeakCounterInc ctx
                @ [
                    X.JMP completeLabel;
                    X.Label successLabel;
                    X.MOV_reg (destReg, heapPtr);
                    X.ADD_imm (heapPtr, 24l);
                    X.XOR_reg (X.R10, X.R10);
                    X.MOV_store (destReg, 0l, X.R10);
                    X.MOV_store (destReg, 8l, X.R10);
                    X.MOV_imm32 (X.R10, 1l);
                    X.MOV_store (destReg, 16l, X.R10);
                  ]
                @ genLeakCounterInc ctx @ [ X.Label completeLabel ])
          | _ -> Ok (loadImm64 destReg 0L)))
