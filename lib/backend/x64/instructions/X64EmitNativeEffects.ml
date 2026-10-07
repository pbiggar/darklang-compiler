(* NativeEffects.fs - Emit x64 instructions for nativeeffects operations. *)
[@@@warning "-4"]
open X64Operands
open X64CodeGenTypes
module X=X86_64
let bind f value=Result.bind value f
let emitCoverageHit (_ctx:X64CodeGenTypes.funcCtx) =
    Ok []   (*  Coverage instrumentation not supported on x86_64 yet *)
let emitRandomInt64 (_ctx:X64CodeGenTypes.funcCtx) (dest:LIR.reg) =
(*  getrandom(buf, 8, 0) syscall *)
    resolveReg dest
    |> Result.map (fun destReg ->
        let clobbered =
            [ X.RAX;
              X.RDI;
              X.RSI;
              X.RDX;
              X.RCX;
              scratch ]
        in
        let preserved = List.filter ((<>) destReg) clobbered
        in
        let saves = List.map (fun r->X.PUSH r) preserved
        in
        let restores = preserved |> List.rev |> List.map (fun r->X.POP r)
        in
        saves
        @ [X.SUB_imm (X.RSP, 8l)]
        @ [X.MOV_reg (X.RDI, X.RSP)]   (*  buf *)
        @ loadImm64 X.RSI 8L                       (*  len = 8 *)
        @ loadImm64 X.RDX 0L                       (*  flags = 0 *)
        @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.getrandom)
        @ [X.SYSCALL;
           X.MOV_load (destReg, X.RSP, 0l);
           X.ADD_imm (X.RSP, 8l)]
        @ restores)
let emitDateTimeNow (_ctx:X64CodeGenTypes.funcCtx) (dest:LIR.reg) =
(*  clock_gettime(CLOCK_REALTIME=0, &ts), converted to 100ns Unix ticks. *)
    resolveReg dest
    |> Result.map (fun destReg ->
        [X.SUB_imm (X.RSP, 16l)]   (*  timespec: tv_sec(8) + tv_nsec(8) *)
        @ loadImm64 X.RDI 0L            (*  CLOCK_REALTIME *)
        @ [X.MOV_reg (X.RSI, X.RSP)]
        @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.gettimeofday)
        @ [X.SYSCALL;
           X.MOV_load (scratch, X.RSP, 0l);
           X.IMUL_imm (scratch, scratch, 10000000l);
           X.MOV_load (X.RAX, X.RSP, 8l);
           X.CQO]
        @ loadImm64 X.RDI 100L
        @ [X.IDIV X.RDI;
           X.ADD_reg (scratch, X.RAX);
           X.MOV_reg (destReg, scratch);
           X.ADD_imm (X.RSP, 16l)])
let emitSleep (ctx:X64CodeGenTypes.funcCtx) (effectId:int) (delayMs:LIR.fReg) =
    match delayMs with
    | LIR.FPhysical physicalDelay ->
        let delayReg = lirFRegToX86 physicalDelay
        in
        let label suffix = Printf.sprintf "__sleep_%s_%d_%s" ctx.functionName effectId suffix
        in
        let retryLabel = label "retry"
        in
        let interruptedLabel = label "interrupted"
        in
        let releaseLabel = label "release"
        in
        let completeLabel = label "complete"
        in
        let millionBits = Int64.bits_of_float 1000000.0
        in
        Ok (
            withPreservedFloatScratch [delayReg] (fun temp ->
                loadImm64 scratch millionBits
                @ [ X.MOVQ_from_gp (temp, scratch);
                    X.MULSD (temp, delayReg);
                    X.CVTTSD2SI (scratch, temp) ])
            @ [ X.CMP_imm (scratch, 0l);
                X.Jcc (X.LE, completeLabel);
                X.SUB_imm (X.RSP, 32l);
                X.MOV_reg (X.RAX, scratch);
                X.CQO ]
            @ loadImm64 X.R10 1000000000L
            @ [ X.IDIV X.R10;
                X.MOV_store (X.RSP, 0l, X.RAX);
                X.MOV_store (X.RSP, 8l, X.RDX) ]
            @ loadImm64 X.R10 0L
            @ [ X.MOV_store (X.RSP, 16l, X.R10);
                X.MOV_store (X.RSP, 24l, X.R10);
                X.Label retryLabel;
                X.LEA (X.RDI, X.RSP, 0l);
                X.LEA (X.RSI, X.RSP, 16l) ]
            @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.nanosleep)
            @ [ X.SYSCALL;
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
                X.Label completeLabel ])
    | _ -> Error "Sleep with virtual float register"
let emitCliNative (ctx:X64CodeGenTypes.funcCtx) (dest:LIR.reg) (operation:LIR.cliOperation) (args:LIR.operand list) =
    resolveReg dest
    |> bind (fun destReg ->
        let loadCliOperand dest operand =
            match operand with
            | LIR.Imm value -> Ok (loadImm64 dest value)
            | LIR.Reg source ->
                resolveReg source
                |> Result.map (fun sourceReg ->
                    if sourceReg = dest then [] else [X.MOV_reg (dest, sourceReg)])
            | LIR.StackSlot offset ->
                Ok [X.MOV_load (dest, X.RBP, Int32.of_int (X64InstructionContext.adjustStackOffset ctx offset))]
            | LIR.StringSymbol value -> Ok (emitStringLiteral dest value)
            | _ -> Error "CLI native operation received a non-integer operand"
(*  Capture every source before populating syscall registers: later *)
(*  operands may currently live in an earlier destination register. *)
        in
        let loadSocketArgs operands targets =
            operands
            |> List.fold_left (fun loaded operand ->
                loaded
                |> bind (fun code ->
                    loadCliOperand X.R11 operand
                    |> Result.map (fun next -> code @ next @ [X.PUSH X.R11]))) (Ok [])
            |> Result.map (fun code -> code @ (targets |> List.rev |> List.map (fun r->X.POP r)))
        in
        match operation with
        | LIR.HostOS -> Ok (loadImm64 destReg 1L)
        | LIR.HostArchitecture -> Ok (loadImm64 destReg 1L)
        | LIR.Hostname ->
            let failureLabel = freshLabel ("hostname_"^ctx.functionName^"_failure")
            in
            let lengthLabel = freshLabel ("hostname_"^ctx.functionName^"_length")
            in
            let lengthDoneLabel = freshLabel ("hostname_"^ctx.functionName^"_length_done")
            in
            let copyLabel = freshLabel ("hostname_"^ctx.functionName^"_copy")
            in
            let copyDoneLabel = freshLabel ("hostname_"^ctx.functionName^"_copy_done")
            in
            let completeLabel = freshLabel ("hostname_"^ctx.functionName^"_complete")
            in
            Ok ([ X.SUB_imm (X.RSP, 400l);
                  X.MOV_reg (X.RDI, X.RSP) ]
            @ loadImm64 X.RAX 63L
            @ [ X.SYSCALL;
                X.CMP_imm (X.RAX, 0l);
                X.Jcc (X.LT, failureLabel);
                X.LEA (X.R8, X.RSP, 65l);
                X.XOR_reg (X.RCX, X.RCX);
                X.MOV_reg (X.R9, X.R8);
                X.Label lengthLabel;
                X.MOV_load_byte (X.RDX, X.R9, 0l);
                X.CMP_imm (X.RDX, 0l);
                X.Jcc (X.EQ, lengthDoneLabel);
                X.ADD_imm (X.RCX, 1l);
                X.ADD_imm (X.R9, 1l);
                X.JMP lengthLabel;
                X.Label lengthDoneLabel;
                X.MOV_reg (X.R10, heapPtr);
                X.MOV_reg (X.R11, X.RCX);
                X.ADD_imm (X.R11, 7l);
                X.AND_imm (X.R11, -8l);
                X.ADD_imm (X.R11, 16l);
                X.ADD_reg (heapPtr, X.R11);
                X.MOV_imm32 (X.RDX, 1l);
                X.MOV_store (X.R10, 0l, X.RDX);
                X.MOV_store (X.R10, 8l, X.RCX);
                X.LEA (X.R9, X.R10, 16l);
                X.MOV_reg (X.RAX, X.RCX);
                X.Label copyLabel;
                X.CMP_imm (X.RAX, 0l);
                X.Jcc (X.EQ, copyDoneLabel);
                X.MOV_load_byte (X.RDX, X.R8, 0l);
                X.MOV_store_byte (X.R9, 0l, X.RDX);
                X.ADD_imm (X.R8, 1l);
                X.ADD_imm (X.R9, 1l);
                X.SUB_imm (X.RAX, 1l);
                X.JMP copyLabel;
                X.Label copyDoneLabel;
                X.ADD_imm (X.RSP, 400l) ]
            @ genLeakCounterInc ctx
            @ [ X.MOV_reg (destReg, heapPtr);
                X.ADD_imm (heapPtr, 24l);
                X.XOR_reg (X.R8, X.R8);
                X.MOV_store (destReg, 0l, X.R8);
                X.MOV_store (destReg, 8l, X.R10);
                X.MOV_imm32 (X.R8, 1l);
                X.MOV_store (destReg, 16l, X.R8) ]
            @ genLeakCounterInc ctx
            @ [ X.JMP completeLabel;
                X.Label failureLabel;
                X.NEG X.RAX;
                X.MOV_reg (X.RDX, X.RAX);
                X.ADD_imm (X.RSP, 400l) ]
            @ emitStringLiteral X.R9 "POSIX error"
            @ [ X.MOV_reg (X.R8, heapPtr);
                X.ADD_imm (heapPtr, 24l);
                X.MOV_store (X.R8, 0l, X.RDX);
                X.MOV_store (X.R8, 8l, X.R9);
                X.MOV_imm32 (X.R10, 1l);
                X.MOV_store (X.R8, 16l, X.R10) ]
            @ genLeakCounterInc ctx
            @ [ X.MOV_reg (destReg, heapPtr);
                X.ADD_imm (heapPtr, 24l);
                X.MOV_imm32 (X.R10, 1l);
                X.MOV_store (destReg, 0l, X.R10);
                X.MOV_store (destReg, 8l, X.R8);
                X.MOV_store (destReg, 16l, X.R10) ]
            @ genLeakCounterInc ctx
            @ [X.Label completeLabel])
        | LIR.Execute ->
            (match args with
            | [command] ->
                loadCliOperand X.RDI command
                |> Result.map (fun loads ->
                    loads
                    @ [X.CALL "__dark_cli_execute"]
                    @ (if destReg = X.RAX then [] else [X.MOV_reg (destReg, X.RAX)]))
            | _ -> Error "CLI execute expects exactly one command")
        | LIR.GetPid ->
            Ok (loadImm64 X.RAX 39L
            @ [X.SYSCALL]
            @ (if destReg = X.RAX then [] else [X.MOV_reg (destReg, X.RAX)]))
        | LIR.SecureRandomFill ->
            (match args with
            | [buffer; length] ->
                loadSocketArgs [buffer; length] [X.RDI; X.RSI]
                |> Result.map (fun loads ->
                    loads
                    @ loadImm64 X.RDX 0L
                    @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.getrandom)
                    @ [X.SYSCALL]
                    @ (if destReg = X.RAX then []
                       else [X.MOV_reg (destReg, X.RAX)]))
            | _ -> Error "SecureRandomFill expects buffer and length")
        | LIR.SocketTcp4 | LIR.SocketTcp6 | LIR.SocketUdp4 | LIR.SocketUdp6 ->
            let constants = Platform.socketConstantsFor Platform.Linux
            in
            let family = if operation = LIR.SocketTcp6 || operation = LIR.SocketUdp6 then constants.Platform.addressFamily6 else constants.Platform.addressFamily4
            in
            let isUdp = operation = LIR.SocketUdp4 || operation = LIR.SocketUdp6
            in
            let kind = if isUdp then constants.Platform.datagramCloexec else constants.Platform.streamCloexec
            in
            let protocol = if isUdp then 17L else 6L
            in
            Ok (loadImm64 X.RDI (Int64.of_int family)
            @ loadImm64 X.RSI kind
            @ loadImm64 X.RDX protocol
            @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.socket)
            @ [X.SYSCALL]
            @ (if destReg = X.RAX then [] else [X.MOV_reg (destReg, X.RAX)]))
        | LIR.SocketConnect4 | LIR.SocketConnect6 | LIR.SocketSend | LIR.SocketReceive | LIR.SocketReceiveTimeout | LIR.SocketSendTimeout ->
            let finish = if destReg = X.RAX then [] else [X.MOV_reg (destReg, X.RAX)]
            in
            (match operation, args with
            | (LIR.SocketConnect4 | LIR.SocketConnect6), [descriptor; address] ->
                loadSocketArgs [descriptor; address] [X.RDI; X.RSI]
                |> Result.map (fun loads ->
                        loads @
                        loadImm64 X.RDX (if operation = LIR.SocketConnect6 then 28L else 16L) @
                        loadImm64 X.RAX (Int64.of_int syscalls.Platform.connect) @ [X.SYSCALL] @ finish)
            | LIR.SocketSend, [descriptor; blob] ->
                loadSocketArgs [descriptor; blob] [X.RDI; X.RSI]
                |> Result.map (fun loads ->
                        loads @
                        [X.MOV_load (X.RDX, X.RSI, 8l);
                         X.ADD_imm (X.RSI, 16l)] @
                        loadImm64 X.R10 (Platform.socketConstantsFor Platform.Linux).Platform.noSignal @
                        loadImm64 X.R8 0L @ loadImm64 X.R9 0L @
                        loadImm64 X.RAX (Int64.of_int syscalls.Platform.sendTo) @ [X.SYSCALL] @ finish)
            | LIR.SocketReceive, [descriptor; buffer; length] ->
                loadSocketArgs [descriptor; buffer; length] [X.RDI; X.RSI; X.RDX]
                |> Result.map (fun loads ->
                        loads @
                        loadImm64 X.RAX (Int64.of_int syscalls.Platform.read) @ [X.SYSCALL] @ finish)
            | (LIR.SocketReceiveTimeout | LIR.SocketSendTimeout), [descriptor; timeval] ->
                loadSocketArgs [descriptor; timeval] [X.RDI; X.R10]
                |> Result.map (fun loads ->
                        let constants = Platform.socketConstantsFor Platform.Linux
                        in
                        let option_ =
                            if operation = LIR.SocketSendTimeout then constants.Platform.sendTimeout
                            else constants.Platform.receiveTimeout
                        in
                        loads @ loadImm64 X.RSI (Int64.of_int constants.Platform.socketLevel) @
                        loadImm64 X.RDX (Int64.of_int option_) @ loadImm64 X.R8 16L @
                        loadImm64 X.RAX (Int64.of_int syscalls.Platform.setSockOpt) @
                        [X.SYSCALL] @ finish)
            | _ -> Error "Invalid socket operation arguments")
        | LIR.SocketBind4 | LIR.SocketListen | LIR.SocketAccept | LIR.SocketCloexec
        | LIR.SocketReuseAddress | LIR.SocketPoll | LIR.SignalBlock | LIR.SignalRestore
        | LIR.SignalPending | LIR.SignalWait | LIR.MonotonicTime ->
            let constants=Platform.socketConstantsFor Platform.Linux in
            let finish=if destReg=X.RAX then [] else [X.MOV_reg (destReg,X.RAX)] in
            let emit operands registers setup number=loadSocketArgs operands registers
                |> Result.map (fun loads -> loads @ setup @ loadImm64 X.RAX (Int64.of_int number) @ [X.SYSCALL] @ finish) in
            (match operation,args with
            | LIR.SocketBind4,[descriptor;address] ->
                emit [descriptor;address] [X.RDI;X.RSI] (loadImm64 X.RDX 16L) syscalls.Platform.bind
            | LIR.SocketListen,[descriptor] ->
                emit [descriptor] [X.RDI] (loadImm64 X.RSI 128L) syscalls.Platform.listen
            | LIR.SocketAccept,[descriptor] ->
                emit [descriptor] [X.RDI] (loadImm64 X.RSI 0L @ loadImm64 X.RDX 0L) syscalls.Platform.accept
            | LIR.SocketCloexec,[descriptor] ->
                emit [descriptor] [X.RDI] (loadImm64 X.RSI 2L @ loadImm64 X.RDX 1L) syscalls.Platform.fcntl
            | LIR.SocketReuseAddress,[descriptor;enabled] ->
                emit [descriptor;enabled] [X.RDI;X.R10]
                    (loadImm64 X.RSI (Int64.of_int constants.Platform.socketLevel) @ loadImm64 X.RDX (Int64.of_int constants.Platform.reuseAddress) @ loadImm64 X.R8 4L) syscalls.Platform.setSockOpt
            | LIR.SocketPoll,[pollfd;timeout] ->
                emit [pollfd;timeout] [X.RDI;X.RDX]
                    (loadImm64 X.RSI 1L @ loadImm64 X.R10 0L @ loadImm64 X.R8 8L) syscalls.Platform.poll
            | LIR.SignalBlock,[mask;previous] ->
                emit [mask;previous] [X.RSI;X.RDX]
                    (loadImm64 X.RDI (Int64.of_int constants.Platform.blockSignal) @ loadImm64 X.R10 8L) syscalls.Platform.signalMask
            | LIR.SignalRestore,[previous] ->
                emit [previous] [X.RSI]
                    (loadImm64 X.RDI (Int64.of_int constants.Platform.restoreSignal) @ loadImm64 X.RDX 0L @ loadImm64 X.R10 8L) syscalls.Platform.signalMask
            | LIR.SignalPending,[mask] ->
                emit [mask] [X.RDI] (loadImm64 X.RSI 8L) syscalls.Platform.signalPending
            | LIR.SignalWait,[mask;info] ->
                emit [mask;info] [X.RDI;X.RSI]
                    (loadImm64 X.RDX 0L @ loadImm64 X.R10 8L) syscalls.Platform.signalWait
            | LIR.MonotonicTime,[time] ->
                emit [time] [X.RSI] (loadImm64 X.RDI 1L) syscalls.Platform.gettimeofday
            | _ -> Error "Invalid listener or signal operation arguments")
        | LIR.SocketClose ->
            (match args with
            | [descriptor] ->
                loadCliOperand X.RDI descriptor
                |> Result.map (fun loads ->
                    loads
                    @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.close)
                    @ [X.SYSCALL]
                    @ (if destReg = X.RAX then [] else [X.MOV_reg (destReg, X.RAX)]))
            | _ -> Error "SocketClose expects one descriptor")
        | LIR.GetUid ->
            Ok (loadImm64 X.RAX 102L
            @ [X.SYSCALL]
            @ (if destReg = X.RAX then [] else [X.MOV_reg (destReg, X.RAX)]))
        | LIR.CpuCount ->
            let byteLoop = freshLabel ("cpu_count_"^ctx.functionName^"_byte")
            in
            let bitLoop = freshLabel ("cpu_count_"^ctx.functionName^"_bit")
            in
            let nextByte = freshLabel ("cpu_count_"^ctx.functionName^"_next")
            in
            let doneLabel = freshLabel ("cpu_count_"^ctx.functionName^"_done")
            in
            let fallbackLabel = freshLabel ("cpu_count_"^ctx.functionName^"_fallback")
            in
            let completeLabel = freshLabel ("cpu_count_"^ctx.functionName^"_complete")
            in
            let zeroMask =
                List.init 16 Fun.id
                |> List.map (fun index -> X.MOV_store (X.RSP, Int32.of_int (index * 8), X.R10))
            in
            Ok ([ X.SUB_imm (X.RSP, 128l);
                  X.XOR_reg (X.R10, X.R10) ]
            @ zeroMask
            @ loadImm64 X.RDI 0L
            @ loadImm64 X.RSI 128L
            @ [ X.MOV_reg (X.RDX, X.RSP) ]
            @ loadImm64 X.RAX 204L
            @ [ X.SYSCALL;
                X.CMP_imm (X.RAX, 0l);
                X.Jcc (X.LT, fallbackLabel);
                X.XOR_reg (X.R8, X.R8);
                X.XOR_reg (X.R9, X.R9);
                X.Label byteLoop;
                X.CMP_imm (X.R8, 128l);
                X.Jcc (X.GE, doneLabel);
                X.MOV_reg (X.R10, X.RSP);
                X.ADD_reg (X.R10, X.R8);
                X.MOV_load_byte (X.R10, X.R10, 0l);
                X.Label bitLoop;
                X.CMP_imm (X.R10, 0l);
                X.Jcc (X.EQ, nextByte);
                X.MOV_reg (X.R11, X.R10);
                X.AND_imm (X.R11, 1l);
                X.ADD_reg (X.R9, X.R11);
                X.SHR_imm (X.R10, 1);
                X.JMP bitLoop;
                X.Label nextByte;
                X.ADD_imm (X.R8, 1l);
                X.JMP byteLoop;
                X.Label doneLabel;
                X.MOV_reg (destReg, X.R9);
                X.JMP completeLabel;
                X.Label fallbackLabel;
                X.MOV_imm32 (destReg, 1l);
                X.Label completeLabel;
                X.ADD_imm (X.RSP, 128l) ])
        | LIR.GetArgv ->
            (match args with
            | [LIR.Imm index] when index >= 0L ->
                Ok (loadImm64 X.RDI index
                @ [X.CALL "__dark_cli_argv"]
                @ (if destReg = X.RAX then [] else [X.MOV_reg (destReg, X.RAX)]))
            | [LIR.Reg index] ->
                (match resolveReg index with
                | Ok indexReg ->
                    Ok ([X.MOV_reg (X.RDI, indexReg);
                         X.CALL "__dark_cli_argv"]
                        @ (if destReg = X.RAX then [] else [X.MOV_reg (destReg, X.RAX)]))
                | Error _ -> Ok (loadImm64 destReg 0L))
            | _ -> Ok (loadImm64 destReg 0L))
        | LIR.GetEnv ->
            let loadName =
                match args with
                | [LIR.Reg name] ->
                    (match resolveReg name with
                    | Ok nameReg when nameReg = X.RDI -> []
                    | Ok nameReg -> [X.MOV_reg (X.RDI, nameReg)]
                    | Error _ -> [])
                | [LIR.StringSymbol name] -> emitStringLiteral X.RDI name
                | [LIR.StackSlot offset] ->
                    [X.MOV_load (X.RDI, X.RBP, Int32.of_int (X64InstructionContext.adjustStackOffset ctx offset))]
                | _ -> []
            in
            Ok (loadName
            @ [X.CALL "__dark_cli_getenv"]
            @ (if destReg = X.RAX then [] else [X.MOV_reg (destReg, X.RAX)]))
        | LIR.GetEnvironmentPacked ->
            Ok ([X.CALL "__dark_cli_environment_packed"]
                @ (if destReg = X.RAX then [] else [X.MOV_reg (destReg, X.RAX)]))
        | LIR.DirectoryCurrent ->
            Ok ([X.CALL "__dark_cli_directory_current"]
                @ (if destReg = X.RAX then [] else [X.MOV_reg (destReg, X.RAX)]))
        | LIR.DirectoryListPacked ->
            (match args with
            | [path] ->
                loadCliOperand X.RDI path
                |> Result.map (fun loads ->
                    loads
                    @ [X.CALL "__dark_cli_directory_list"]
                    @ (if destReg = X.RAX then [] else [X.MOV_reg (destReg, X.RAX)]))
            | _ -> Error "directoryList expects exactly one path")
        | LIR.FileIsDirectory ->
            (match args with
            | [path] ->
                loadCliOperand X.R10 path
                |> Result.map (fun pathLoads ->
                    let copyLoop = freshLabel "is_dir_copy"
                    in
                    let copyDone = freshLabel "is_dir_copy_done"
                    in
                    let failure = freshLabel "is_dir_failure"
                    in
                    let complete = freshLabel "is_dir_complete"
                    in
                    pathLoads
                    @ [ X.PUSH X.RDI;
                        X.PUSH X.RSI;
                        X.PUSH X.RDX;
                        X.PUSH X.RCX;
                        X.PUSH X.R10;
                        X.SUB_imm (X.RSP, 4096l);
                        X.MOV_load (X.RCX, X.R10, 8l);
                        X.LEA (X.RSI, X.R10, 16l);
                        X.MOV_reg (X.RDI, X.RSP);
                        X.XOR_reg (X.R10, X.R10);
                        X.Label copyLoop;
                        X.CMP_reg (X.R10, X.RCX);
                        X.Jcc (X.GE, copyDone);
                        X.MOV_reg (scratch, X.RSI);
                        X.ADD_reg (scratch, X.R10);
                        X.MOV_load_byte (scratch, scratch, 0l);
                        X.MOV_reg (X.RDX, X.RDI);
                        X.ADD_reg (X.RDX, X.R10);
                        X.MOV_store_byte (X.RDX, 0l, scratch);
                        X.ADD_imm (X.R10, 1l);
                        X.JMP copyLoop;
                        X.Label copyDone;
                        X.MOV_reg (scratch, X.RDI);
                        X.ADD_reg (scratch, X.RCX);
                        X.XOR_reg (X.R10, X.R10);
                        X.MOV_store_byte (scratch, 0l, X.R10);
                        X.MOV_reg (X.RDI, X.RSP) ]
                    @ loadImm64 X.RSI 65536L
                    @ loadImm64 X.RDX 0L
                    @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.open_)
                    @ [ X.SYSCALL;
                        X.CMP_imm (X.RAX, 0l);
                        X.Jcc (X.LT, failure);
                        X.MOV_reg (X.RDI, X.RAX) ]
                    @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.close)
                    @ [ X.SYSCALL ]
                    @ loadImm64 X.RAX 1L
                    @ [ X.JMP complete;
                        X.Label failure ]
                    @ loadImm64 X.RAX 0L
                    @ [ X.Label complete;
                        X.ADD_imm (X.RSP, 4096l);
                        X.POP X.R10;
                        X.POP X.RCX;
                        X.POP X.RDX;
                        X.POP X.RSI;
                        X.POP X.RDI;
                        X.MOV_reg (destReg, X.RAX) ])
            | _ -> Error "fileIsDirectory expects exactly one path")
        | LIR.FileCreateExclusive ->
            (match args with
            | [path] ->
                loadCliOperand X.R10 path
                |> Result.map (fun pathLoads ->
                    let copyLoop = freshLabel "create_exclusive_copy"
                    in
                    let copyDone = freshLabel "create_exclusive_copy_done"
                    in
                    let tooLong = freshLabel "create_exclusive_too_long"
                    in
                    let failure = freshLabel "create_exclusive_failure"
                    in
                    let complete = freshLabel "create_exclusive_complete"
                    in
                    pathLoads
                    @ [ X.PUSH X.RDI;
                        X.PUSH X.RSI;
                        X.PUSH X.RDX;
                        X.PUSH X.RCX;
                        X.PUSH X.R10;
                        X.SUB_imm (X.RSP, 4096l);
                        X.MOV_load (X.RCX, X.R10, 8l);
                        X.CMP_imm (X.RCX, 4096l);
                        X.Jcc (X.GE, tooLong);
                        X.LEA (X.RSI, X.R10, 16l);
                        X.MOV_reg (X.RDI, X.RSP);
                        X.XOR_reg (X.R10, X.R10);
                        X.Label copyLoop;
                        X.CMP_reg (X.R10, X.RCX);
                        X.Jcc (X.GE, copyDone);
                        X.MOV_reg (scratch, X.RSI);
                        X.ADD_reg (scratch, X.R10);
                        X.MOV_load_byte (scratch, scratch, 0l);
                        X.MOV_reg (X.RDX, X.RDI);
                        X.ADD_reg (X.RDX, X.R10);
                        X.MOV_store_byte (X.RDX, 0l, scratch);
                        X.ADD_imm (X.R10, 1l);
                        X.JMP copyLoop;
                        X.Label copyDone;
                        X.MOV_reg (scratch, X.RDI);
                        X.ADD_reg (scratch, X.RCX);
                        X.XOR_reg (X.R10, X.R10);
                        X.MOV_store_byte (scratch, 0l, X.R10);
                        X.MOV_reg (X.RDI, X.RSP) ]
                    @ loadImm64 X.RSI 194L  (*  O_RDWR | O_CREAT | O_EXCL *)
                    @ loadImm64 X.RDX 0o600L
                    @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.open_)
                    @ [ X.SYSCALL;
                        X.CMP_imm (X.RAX, 0l);
                        X.Jcc (X.LT, failure);
                        X.MOV_reg (X.RDI, X.RAX) ]
                    @ loadImm64 X.RAX (Int64.of_int syscalls.Platform.close)
                    @ [ X.SYSCALL ]
                    @ loadImm64 X.RAX 0L
                    @ [ X.JMP complete;
                        X.Label failure;
                        X.NEG X.RAX;
                        X.JMP complete;
                        X.Label tooLong ]
                    @ loadImm64 X.RAX 36L
                    @ [ X.Label complete;
                        X.ADD_imm (X.RSP, 4096l);
                        X.POP X.R10;
                        X.POP X.RCX;
                        X.POP X.RDX;
                        X.POP X.RSI;
                        X.POP X.RDI;
                        X.MOV_reg (destReg, X.RAX) ])
            | _ -> Error "fileCreateExclusive expects exactly one path")
        | LIR.SetEnv ->
            (match args with
            | [name; value] ->
                loadSocketArgs [name; value] [X.RDI; X.RSI]
                |> Result.map (fun loads ->
                    loads
                    @ [X.CALL "__dark_cli_setenv"]
                    @ (if destReg = X.RAX then [] else [X.MOV_reg (destReg, X.RAX)]))
            | _ -> Error "setenv expects exactly a name and value")
        | LIR.UnsetEnv ->
            (match args with
            | [name] ->
                loadCliOperand X.RDI name
                |> Result.map (fun loads ->
                    loads
                    @ [X.CALL "__dark_cli_unsetenv"]
                    @ (if destReg = X.RAX then [] else [X.MOV_reg (destReg, X.RAX)]))
            | _ -> Error "unsetenv expects exactly one name")
        | LIR.Kill ->
            let successLabel = freshLabel ("kill_"^ctx.functionName^"_success")
            in
            let completeLabel = freshLabel ("kill_"^ctx.functionName^"_complete")
            in
            let pushOperand operand =
                match operand with
                | LIR.Imm value -> loadImm64 X.RAX value @ [X.PUSH X.RAX]
                | LIR.Reg reg ->
                    (match resolveReg reg with
                    | Ok source -> [X.PUSH source]
                    | Error _ -> [])
                | LIR.StackSlot offset ->
                    [ X.MOV_load (X.RAX, X.RBP, Int32.of_int (X64InstructionContext.adjustStackOffset ctx offset));
                      X.PUSH X.RAX ]
                | _ -> []
            in
            (match args with
            | [pid; signal] ->
                Ok (pushOperand pid
                @ pushOperand signal
                @ [ X.POP X.RSI;
                    X.POP X.RDI ]
                @ loadImm64 X.RAX 62L
                @ [ X.SYSCALL;
                    X.CMP_imm (X.RAX, 0l);
                    X.Jcc (X.GE, successLabel);
                    X.NEG X.RAX;
                    X.MOV_reg (X.RDX, X.RAX) ]
                @ emitStringLiteral X.R9 "POSIX error"
                @ [ X.MOV_reg (X.R8, heapPtr);
                    X.ADD_imm (heapPtr, 24l);
                    X.MOV_store (X.R8, 0l, X.RDX);
                    X.MOV_store (X.R8, 8l, X.R9);
                    X.MOV_imm32 (X.R10, 1l);
                    X.MOV_store (X.R8, 16l, X.R10) ]
                @ genLeakCounterInc ctx
                @ [ X.MOV_reg (destReg, heapPtr);
                    X.ADD_imm (heapPtr, 24l);
                    X.MOV_imm32 (X.R10, 1l);
                    X.MOV_store (destReg, 0l, X.R10);
                    X.MOV_store (destReg, 8l, X.R8);
                    X.MOV_store (destReg, 16l, X.R10) ]
                @ genLeakCounterInc ctx
                @ [ X.JMP completeLabel;
                    X.Label successLabel;
                    X.MOV_reg (destReg, heapPtr);
                    X.ADD_imm (heapPtr, 24l);
                    X.XOR_reg (X.R10, X.R10);
                    X.MOV_store (destReg, 0l, X.R10);
                    X.MOV_store (destReg, 8l, X.R10);
                    X.MOV_imm32 (X.R10, 1l);
                    X.MOV_store (destReg, 16l, X.R10) ]
                @ genLeakCounterInc ctx
                @ [X.Label completeLabel])
            | _ -> Ok (loadImm64 destReg 0L))
        | LIR.RunProcess ->
            (match args with
            | [request] ->
                loadCliOperand X.RDI request
                |> Result.map (fun loads ->
                    loads
                    @ [X.CALL "__dark_cli_run_process"]
                    @ (if destReg = X.RAX then [] else [X.MOV_reg (destReg, X.RAX)]))
            | _ -> Error "CLI run process expects one request")
        | LIR.SpawnProcess ->
            (match args with
            | [command] ->
                loadCliOperand X.RDI command
                |> Result.map (fun loads ->
                    loads
                    @ [X.CALL "__dark_cli_spawn_process"]
                    @ (if destReg = X.RAX then [] else [X.MOV_reg (destReg, X.RAX)]))
            | _ -> Error "CLI spawn process expects one command")
        | LIR.ProcessIO ->
            (match args with
            | [handle; input] ->
                loadSocketArgs [handle; input] [X.RDI; X.RSI]
                |> Result.map (fun loads ->
                    loads
                    @ [ X.XOR_reg (X.RDX, X.RDX);
                        X.CALL "__dark_cli_process_io" ]
                    @ (if destReg = X.RAX then [] else [X.MOV_reg (destReg, X.RAX)]))
            | _ -> Error "CLI process IO expects a handle and input")
        | LIR.TerminateProcess ->
            (match args with
            | [handle] ->
                loadCliOperand X.RDI handle
                |> Result.map (fun loads ->
                    loads
                    @ [X.CALL "__dark_cli_terminate_process"]
                    @ (if destReg = X.RAX then [] else [X.MOV_reg (destReg, X.RAX)]))
            | _ -> Error "CLI terminate process expects one handle"))
