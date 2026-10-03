// NativeEffects.fs - Emit x64 instructions for nativeeffects operations.

module X64EmitNativeEffects

open X64Operands
open X64CodeGenTypes
open X64FieldReferenceCounts
open X64InstructionContext

let internal emitCoverageHit (ctx: FuncCtx) : Result<X86_64.Instr list, string> =
    Ok []  // Coverage instrumentation not supported on x86_64 yet

let internal emitRandomInt64 (ctx: FuncCtx) (dest: LIR.Reg) : Result<X86_64.Instr list, string> =
    // getrandom(buf, 8, 0) syscall
    resolveReg dest
    |> Result.map (fun destReg ->
        let clobbered =
            [ X86_64.RAX
              X86_64.RDI
              X86_64.RSI
              X86_64.RDX
              X86_64.RCX
              scratch ]
        let preserved = List.filter ((<>) destReg) clobbered
        let saves = List.map X86_64.PUSH preserved
        let restores = preserved |> List.rev |> List.map X86_64.POP
        saves
        @ [X86_64.SUB_imm (X86_64.RSP, 8)]
        @ [X86_64.MOV_reg (X86_64.RDI, X86_64.RSP)]  // buf
        @ loadImm64 X86_64.RSI 8L                      // len = 8
        @ loadImm64 X86_64.RDX 0L                      // flags = 0
        @ loadImm64 X86_64.RAX (int64 syscalls.Getrandom)
        @ [X86_64.SYSCALL
           X86_64.MOV_load (destReg, X86_64.RSP, 0)
           X86_64.ADD_imm (X86_64.RSP, 8)]
        @ restores)

let internal emitDateTimeNow (ctx: FuncCtx) (dest: LIR.Reg) : Result<X86_64.Instr list, string> =
    // clock_gettime(CLOCK_REALTIME=0, &ts), converted to 100ns Unix ticks.
    resolveReg dest
    |> Result.map (fun destReg ->
        [X86_64.SUB_imm (X86_64.RSP, 16)]  // timespec: tv_sec(8) + tv_nsec(8)
        @ loadImm64 X86_64.RDI 0L           // CLOCK_REALTIME
        @ [X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)]
        @ loadImm64 X86_64.RAX (int64 syscalls.Gettimeofday)
        @ [X86_64.SYSCALL
           X86_64.MOV_load (scratch, X86_64.RSP, 0)
           X86_64.IMUL_imm (scratch, scratch, 10000000)
           X86_64.MOV_load (X86_64.RAX, X86_64.RSP, 8)
           X86_64.CQO]
        @ loadImm64 X86_64.RDI 100L
        @ [X86_64.IDIV X86_64.RDI
           X86_64.ADD_reg (scratch, X86_64.RAX)
           X86_64.MOV_reg (destReg, scratch)
           X86_64.ADD_imm (X86_64.RSP, 16)])

let internal emitSleep (ctx: FuncCtx) (effectId: int) (delayMs: LIR.FReg) : Result<X86_64.Instr list, string> =
    match delayMs with
    | LIR.FPhysical physicalDelay ->
        let delayReg = lirFRegToX86 physicalDelay
        let label suffix = $"__sleep_{ctx.FunctionName}_{effectId}_{suffix}"
        let retryLabel = label "retry"
        let interruptedLabel = label "interrupted"
        let releaseLabel = label "release"
        let completeLabel = label "complete"
        let millionBits = System.BitConverter.DoubleToInt64Bits 1000000.0
        Ok (
            withPreservedFloatScratch [delayReg] (fun temp ->
                loadImm64 scratch millionBits
                @ [ X86_64.MOVQ_from_gp (temp, scratch)
                    X86_64.MULSD (temp, delayReg)
                    X86_64.CVTTSD2SI (scratch, temp) ])
            @ [ X86_64.CMP_imm (scratch, 0)
                X86_64.Jcc (X86_64.LE, completeLabel)
                X86_64.SUB_imm (X86_64.RSP, 32)
                X86_64.MOV_reg (X86_64.RAX, scratch)
                X86_64.CQO ]
            @ loadImm64 X86_64.R10 1000000000L
            @ [ X86_64.IDIV X86_64.R10
                X86_64.MOV_store (X86_64.RSP, 0, X86_64.RAX)
                X86_64.MOV_store (X86_64.RSP, 8, X86_64.RDX) ]
            @ loadImm64 X86_64.R10 0L
            @ [ X86_64.MOV_store (X86_64.RSP, 16, X86_64.R10)
                X86_64.MOV_store (X86_64.RSP, 24, X86_64.R10)
                X86_64.Label retryLabel
                X86_64.LEA (X86_64.RDI, X86_64.RSP, 0)
                X86_64.LEA (X86_64.RSI, X86_64.RSP, 16) ]
            @ loadImm64 X86_64.RAX (int64 syscalls.Nanosleep)
            @ [ X86_64.SYSCALL
                X86_64.CMP_imm (X86_64.RAX, -4)
                X86_64.Jcc (X86_64.EQ, interruptedLabel)
                X86_64.JMP releaseLabel
                X86_64.Label interruptedLabel
                X86_64.MOV_load (X86_64.R10, X86_64.RSP, 16)
                X86_64.MOV_store (X86_64.RSP, 0, X86_64.R10)
                X86_64.MOV_load (X86_64.R10, X86_64.RSP, 24)
                X86_64.MOV_store (X86_64.RSP, 8, X86_64.R10)
                X86_64.JMP retryLabel
                X86_64.Label releaseLabel
                X86_64.ADD_imm (X86_64.RSP, 32)
                X86_64.Label completeLabel ])
    | _ -> Error "Sleep with virtual float register"

let internal emitCliNative (ctx: FuncCtx) (dest: LIR.Reg) (operation: LIR.CliOperation) (args: LIR.Operand list) : Result<X86_64.Instr list, string> =
    resolveReg dest
    |> Result.bind (fun destReg ->
        let loadCliOperand dest operand =
            match operand with
            | LIR.Imm value -> Ok (loadImm64 dest value)
            | LIR.Reg source ->
                resolveReg source
                |> Result.map (fun sourceReg ->
                    if sourceReg = dest then [] else [X86_64.MOV_reg (dest, sourceReg)])
            | LIR.StackSlot offset ->
                Ok [X86_64.MOV_load (dest, X86_64.RBP, int32 (adjustStackOffset ctx offset))]
            | LIR.StringSymbol value -> Ok (emitStringLiteral dest value)
            | _ -> Error "CLI native operation received a non-integer operand"
        // Capture every source before populating syscall registers: later
        // operands may currently live in an earlier destination register.
        let loadSocketArgs operands targets =
            operands
            |> List.fold (fun loaded operand ->
                loaded
                |> Result.bind (fun code ->
                    loadCliOperand X86_64.R11 operand
                    |> Result.map (fun next -> code @ next @ [X86_64.PUSH X86_64.R11]))) (Ok [])
            |> Result.map (fun code -> code @ (targets |> List.rev |> List.map X86_64.POP))
        match operation with
        | LIR.HostOS -> Ok (loadImm64 destReg 1L)
        | LIR.HostArchitecture -> Ok (loadImm64 destReg 1L)
        | LIR.Hostname ->
            let failureLabel = freshLabel $"hostname_{ctx.FunctionName}_failure"
            let lengthLabel = freshLabel $"hostname_{ctx.FunctionName}_length"
            let lengthDoneLabel = freshLabel $"hostname_{ctx.FunctionName}_length_done"
            let copyLabel = freshLabel $"hostname_{ctx.FunctionName}_copy"
            let copyDoneLabel = freshLabel $"hostname_{ctx.FunctionName}_copy_done"
            let completeLabel = freshLabel $"hostname_{ctx.FunctionName}_complete"
            Ok ([ X86_64.SUB_imm (X86_64.RSP, 400)
                  X86_64.MOV_reg (X86_64.RDI, X86_64.RSP) ]
            @ loadImm64 X86_64.RAX 63L
            @ [ X86_64.SYSCALL
                X86_64.CMP_imm (X86_64.RAX, 0)
                X86_64.Jcc (X86_64.LT, failureLabel)
                X86_64.LEA (X86_64.R8, X86_64.RSP, 65)
                X86_64.XOR_reg (X86_64.RCX, X86_64.RCX)
                X86_64.MOV_reg (X86_64.R9, X86_64.R8)
                X86_64.Label lengthLabel
                X86_64.MOV_load_byte (X86_64.RDX, X86_64.R9, 0)
                X86_64.CMP_imm (X86_64.RDX, 0)
                X86_64.Jcc (X86_64.EQ, lengthDoneLabel)
                X86_64.ADD_imm (X86_64.RCX, 1)
                X86_64.ADD_imm (X86_64.R9, 1)
                X86_64.JMP lengthLabel
                X86_64.Label lengthDoneLabel
                X86_64.MOV_reg (X86_64.R10, heapPtr)
                X86_64.MOV_reg (X86_64.R11, X86_64.RCX)
                X86_64.ADD_imm (X86_64.R11, 7)
                X86_64.AND_imm (X86_64.R11, -8)
                X86_64.ADD_imm (X86_64.R11, 16)
                X86_64.ADD_reg (heapPtr, X86_64.R11)
                X86_64.MOV_imm32 (X86_64.RDX, 1)
                X86_64.MOV_store (X86_64.R10, 0, X86_64.RDX)
                X86_64.MOV_store (X86_64.R10, 8, X86_64.RCX)
                X86_64.LEA (X86_64.R9, X86_64.R10, 16)
                X86_64.MOV_reg (X86_64.RAX, X86_64.RCX)
                X86_64.Label copyLabel
                X86_64.CMP_imm (X86_64.RAX, 0)
                X86_64.Jcc (X86_64.EQ, copyDoneLabel)
                X86_64.MOV_load_byte (X86_64.RDX, X86_64.R8, 0)
                X86_64.MOV_store_byte (X86_64.R9, 0, X86_64.RDX)
                X86_64.ADD_imm (X86_64.R8, 1)
                X86_64.ADD_imm (X86_64.R9, 1)
                X86_64.SUB_imm (X86_64.RAX, 1)
                X86_64.JMP copyLabel
                X86_64.Label copyDoneLabel
                X86_64.ADD_imm (X86_64.RSP, 400) ]
            @ genLeakCounterInc ctx
            @ [ X86_64.MOV_reg (destReg, heapPtr)
                X86_64.ADD_imm (heapPtr, 24)
                X86_64.XOR_reg (X86_64.R8, X86_64.R8)
                X86_64.MOV_store (destReg, 0, X86_64.R8)
                X86_64.MOV_store (destReg, 8, X86_64.R10)
                X86_64.MOV_imm32 (X86_64.R8, 1)
                X86_64.MOV_store (destReg, 16, X86_64.R8) ]
            @ genLeakCounterInc ctx
            @ [ X86_64.JMP completeLabel
                X86_64.Label failureLabel
                X86_64.NEG X86_64.RAX
                X86_64.MOV_reg (X86_64.RDX, X86_64.RAX)
                X86_64.ADD_imm (X86_64.RSP, 400) ]
            @ emitStringLiteral X86_64.R9 "POSIX error"
            @ [ X86_64.MOV_reg (X86_64.R8, heapPtr)
                X86_64.ADD_imm (heapPtr, 24)
                X86_64.MOV_store (X86_64.R8, 0, X86_64.RDX)
                X86_64.MOV_store (X86_64.R8, 8, X86_64.R9)
                X86_64.MOV_imm32 (X86_64.R10, 1)
                X86_64.MOV_store (X86_64.R8, 16, X86_64.R10) ]
            @ genLeakCounterInc ctx
            @ [ X86_64.MOV_reg (destReg, heapPtr)
                X86_64.ADD_imm (heapPtr, 24)
                X86_64.MOV_imm32 (X86_64.R10, 1)
                X86_64.MOV_store (destReg, 0, X86_64.R10)
                X86_64.MOV_store (destReg, 8, X86_64.R8)
                X86_64.MOV_store (destReg, 16, X86_64.R10) ]
            @ genLeakCounterInc ctx
            @ [X86_64.Label completeLabel])
        | LIR.Execute ->
            match args with
            | [command] ->
                loadCliOperand X86_64.RDI command
                |> Result.map (fun loads ->
                    loads
                    @ [X86_64.CALL "__dark_cli_execute"]
                    @ (if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)]))
            | _ -> Error "CLI execute expects exactly one command"
        | LIR.GetPid ->
            Ok (loadImm64 X86_64.RAX 39L
            @ [X86_64.SYSCALL]
            @ (if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)]))
        | LIR.SecureRandomFill ->
            match args with
            | [buffer; length] ->
                loadSocketArgs [buffer; length] [X86_64.RDI; X86_64.RSI]
                |> Result.map (fun loads ->
                    loads
                    @ loadImm64 X86_64.RDX 0L
                    @ loadImm64 X86_64.RAX (int64 syscalls.Getrandom)
                    @ [X86_64.SYSCALL]
                    @ (if destReg = X86_64.RAX then []
                       else [X86_64.MOV_reg (destReg, X86_64.RAX)]))
            | _ -> Error "SecureRandomFill expects buffer and length"
        | LIR.SocketTcp4 | LIR.SocketTcp6 | LIR.SocketUdp4 | LIR.SocketUdp6 ->
            let constants = Platform.socketConstantsFor Platform.Linux
            let family = if operation = LIR.SocketTcp6 || operation = LIR.SocketUdp6 then constants.AddressFamily6 else constants.AddressFamily4
            let isUdp = operation = LIR.SocketUdp4 || operation = LIR.SocketUdp6
            let kind = if isUdp then constants.DatagramCloexec else constants.StreamCloexec
            let protocol = if isUdp then 17L else 6L
            Ok (loadImm64 X86_64.RDI (int64 family)
            @ loadImm64 X86_64.RSI kind
            @ loadImm64 X86_64.RDX protocol
            @ loadImm64 X86_64.RAX (int64 syscalls.Socket)
            @ [X86_64.SYSCALL]
            @ (if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)]))
        | LIR.SocketConnect4 | LIR.SocketConnect6 | LIR.SocketSend | LIR.SocketReceive | LIR.SocketReceiveTimeout | LIR.SocketSendTimeout ->
            let finish = if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)] in
            match operation, args with
            | (LIR.SocketConnect4 | LIR.SocketConnect6), [descriptor; address] ->
                loadSocketArgs [descriptor; address] [X86_64.RDI; X86_64.RSI]
                |> Result.map (fun loads ->
                        loads @
                        loadImm64 X86_64.RDX (if operation = LIR.SocketConnect6 then 28L else 16L) @
                        loadImm64 X86_64.RAX (int64 syscalls.Connect) @ [X86_64.SYSCALL] @ finish)
            | LIR.SocketSend, [descriptor; blob] ->
                loadSocketArgs [descriptor; blob] [X86_64.RDI; X86_64.RSI]
                |> Result.map (fun loads ->
                        loads @
                        [X86_64.MOV_load (X86_64.RDX, X86_64.RSI, 8)
                         X86_64.ADD_imm (X86_64.RSI, 16)] @
                        loadImm64 X86_64.R10 (Platform.socketConstantsFor Platform.Linux).NoSignal @
                        loadImm64 X86_64.R8 0L @ loadImm64 X86_64.R9 0L @
                        loadImm64 X86_64.RAX (int64 syscalls.SendTo) @ [X86_64.SYSCALL] @ finish)
            | LIR.SocketReceive, [descriptor; buffer; length] ->
                loadSocketArgs [descriptor; buffer; length] [X86_64.RDI; X86_64.RSI; X86_64.RDX]
                |> Result.map (fun loads ->
                        loads @
                        loadImm64 X86_64.RAX (int64 syscalls.Read) @ [X86_64.SYSCALL] @ finish)
            | (LIR.SocketReceiveTimeout | LIR.SocketSendTimeout), [descriptor; timeval] ->
                loadSocketArgs [descriptor; timeval] [X86_64.RDI; X86_64.R10]
                |> Result.map (fun loads ->
                        let constants = Platform.socketConstantsFor Platform.Linux in
                        let option_ =
                            if operation = LIR.SocketSendTimeout then constants.SendTimeout
                            else constants.ReceiveTimeout in
                        loads @ loadImm64 X86_64.RSI (int64 constants.SocketLevel) @
                        loadImm64 X86_64.RDX (int64 option_) @ loadImm64 X86_64.R8 16L @
                        loadImm64 X86_64.RAX (int64 syscalls.SetSockOpt) @
                        [X86_64.SYSCALL] @ finish)
            | _ -> Error "Invalid socket operation arguments"
        | LIR.SocketBind4 | LIR.SocketListen | LIR.SocketAccept | LIR.SocketCloexec
        | LIR.SocketReuseAddress | LIR.SocketPoll | LIR.SignalBlock | LIR.SignalRestore
        | LIR.SignalPending | LIR.SignalWait | LIR.MonotonicTime ->
            let constants = Platform.socketConstantsFor Platform.Linux
            let finish = if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)]
            let emit operands registers setup number =
                loadSocketArgs operands registers
                |> Result.map (fun loads -> loads @ setup @ loadImm64 X86_64.RAX (int64 number) @ [X86_64.SYSCALL] @ finish)
            match operation, args with
            | LIR.SocketBind4, [descriptor; address] ->
                emit [descriptor; address] [X86_64.RDI; X86_64.RSI] (loadImm64 X86_64.RDX 16L) syscalls.Bind
            | LIR.SocketListen, [descriptor] ->
                emit [descriptor] [X86_64.RDI] (loadImm64 X86_64.RSI 128L) syscalls.Listen
            | LIR.SocketAccept, [descriptor] ->
                emit [descriptor] [X86_64.RDI] (loadImm64 X86_64.RSI 0L @ loadImm64 X86_64.RDX 0L) syscalls.Accept
            | LIR.SocketCloexec, [descriptor] ->
                emit [descriptor] [X86_64.RDI] (loadImm64 X86_64.RSI 2L @ loadImm64 X86_64.RDX 1L) syscalls.Fcntl
            | LIR.SocketReuseAddress, [descriptor; enabled] ->
                emit [descriptor; enabled] [X86_64.RDI; X86_64.R10]
                    (loadImm64 X86_64.RSI (int64 constants.SocketLevel) @ loadImm64 X86_64.RDX (int64 constants.ReuseAddress) @ loadImm64 X86_64.R8 4L) syscalls.SetSockOpt
            | LIR.SocketPoll, [pollfd; timeout] ->
                emit [pollfd; timeout] [X86_64.RDI; X86_64.RDX]
                    (loadImm64 X86_64.RSI 1L @ loadImm64 X86_64.R10 0L @ loadImm64 X86_64.R8 8L) syscalls.Poll
            | LIR.SignalBlock, [mask; previous] ->
                emit [mask; previous] [X86_64.RSI; X86_64.RDX]
                    (loadImm64 X86_64.RDI (int64 constants.BlockSignal) @ loadImm64 X86_64.R10 8L) syscalls.SignalMask
            | LIR.SignalRestore, [previous] ->
                emit [previous] [X86_64.RSI]
                    (loadImm64 X86_64.RDI (int64 constants.RestoreSignal) @ loadImm64 X86_64.RDX 0L @ loadImm64 X86_64.R10 8L) syscalls.SignalMask
            | LIR.SignalPending, [mask] ->
                emit [mask] [X86_64.RDI] (loadImm64 X86_64.RSI 8L) syscalls.SignalPending
            | LIR.SignalWait, [mask; info] ->
                emit [mask; info] [X86_64.RDI; X86_64.RSI]
                    (loadImm64 X86_64.RDX 0L @ loadImm64 X86_64.R10 8L) syscalls.SignalWait
            | LIR.MonotonicTime, [time] ->
                emit [time] [X86_64.RSI] (loadImm64 X86_64.RDI 1L) syscalls.Gettimeofday
            | _ -> Error "Invalid listener or signal operation arguments"
        | LIR.SocketClose ->
            match args with
            | [descriptor] ->
                loadCliOperand X86_64.RDI descriptor
                |> Result.map (fun loads ->
                    loads
                    @ loadImm64 X86_64.RAX (int64 syscalls.Close)
                    @ [X86_64.SYSCALL]
                    @ (if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)]))
            | _ -> Error "SocketClose expects one descriptor"
        | LIR.GetUid ->
            Ok (loadImm64 X86_64.RAX 102L
            @ [X86_64.SYSCALL]
            @ (if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)]))
        | LIR.CpuCount ->
            let byteLoop = freshLabel $"cpu_count_{ctx.FunctionName}_byte"
            let bitLoop = freshLabel $"cpu_count_{ctx.FunctionName}_bit"
            let nextByte = freshLabel $"cpu_count_{ctx.FunctionName}_next"
            let doneLabel = freshLabel $"cpu_count_{ctx.FunctionName}_done"
            let fallbackLabel = freshLabel $"cpu_count_{ctx.FunctionName}_fallback"
            let completeLabel = freshLabel $"cpu_count_{ctx.FunctionName}_complete"
            let zeroMask =
                [0 .. 15]
                |> List.map (fun index -> X86_64.MOV_store (X86_64.RSP, int32 (index * 8), X86_64.R10))
            Ok ([ X86_64.SUB_imm (X86_64.RSP, 128)
                  X86_64.XOR_reg (X86_64.R10, X86_64.R10) ]
            @ zeroMask
            @ loadImm64 X86_64.RDI 0L
            @ loadImm64 X86_64.RSI 128L
            @ [ X86_64.MOV_reg (X86_64.RDX, X86_64.RSP) ]
            @ loadImm64 X86_64.RAX 204L
            @ [ X86_64.SYSCALL
                X86_64.CMP_imm (X86_64.RAX, 0)
                X86_64.Jcc (X86_64.LT, fallbackLabel)
                X86_64.XOR_reg (X86_64.R8, X86_64.R8)
                X86_64.XOR_reg (X86_64.R9, X86_64.R9)
                X86_64.Label byteLoop
                X86_64.CMP_imm (X86_64.R8, 128)
                X86_64.Jcc (X86_64.GE, doneLabel)
                X86_64.MOV_reg (X86_64.R10, X86_64.RSP)
                X86_64.ADD_reg (X86_64.R10, X86_64.R8)
                X86_64.MOV_load_byte (X86_64.R10, X86_64.R10, 0)
                X86_64.Label bitLoop
                X86_64.CMP_imm (X86_64.R10, 0)
                X86_64.Jcc (X86_64.EQ, nextByte)
                X86_64.MOV_reg (X86_64.R11, X86_64.R10)
                X86_64.AND_imm (X86_64.R11, 1)
                X86_64.ADD_reg (X86_64.R9, X86_64.R11)
                X86_64.SHR_imm (X86_64.R10, 1)
                X86_64.JMP bitLoop
                X86_64.Label nextByte
                X86_64.ADD_imm (X86_64.R8, 1)
                X86_64.JMP byteLoop
                X86_64.Label doneLabel
                X86_64.MOV_reg (destReg, X86_64.R9)
                X86_64.JMP completeLabel
                X86_64.Label fallbackLabel
                X86_64.MOV_imm32 (destReg, 1)
                X86_64.Label completeLabel
                X86_64.ADD_imm (X86_64.RSP, 128) ])
        | LIR.GetArgv ->
            match args with
            | [LIR.Imm index] when index >= 0L ->
                Ok (loadImm64 X86_64.RDI index
                @ [X86_64.CALL "__dark_cli_argv"]
                @ (if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)]))
            | [LIR.Reg index] ->
                match resolveReg index with
                | Ok indexReg ->
                    Ok ([X86_64.MOV_reg (X86_64.RDI, indexReg)
                         X86_64.CALL "__dark_cli_argv"]
                        @ (if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)]))
                | Error _ -> Ok (loadImm64 destReg 0L)
            | _ -> Ok (loadImm64 destReg 0L)
        | LIR.GetEnv ->
            let loadName =
                match args with
                | [LIR.Reg name] ->
                    match resolveReg name with
                    | Ok nameReg when nameReg = X86_64.RDI -> []
                    | Ok nameReg -> [X86_64.MOV_reg (X86_64.RDI, nameReg)]
                    | Error _ -> []
                | [LIR.StringSymbol name] -> emitStringLiteral X86_64.RDI name
                | [LIR.StackSlot offset] ->
                    [X86_64.MOV_load (X86_64.RDI, X86_64.RBP, int32 (adjustStackOffset ctx offset))]
                | _ -> []
            Ok (loadName
            @ [X86_64.CALL "__dark_cli_getenv"]
            @ (if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)]))
        | LIR.GetEnvironmentPacked ->
            Ok ([X86_64.CALL "__dark_cli_environment_packed"]
                @ (if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)]))
        | LIR.DirectoryCurrent ->
            Ok ([X86_64.CALL "__dark_cli_directory_current"]
                @ (if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)]))
        | LIR.DirectoryListPacked ->
            match args with
            | [path] ->
                loadCliOperand X86_64.RDI path
                |> Result.map (fun loads ->
                    loads
                    @ [X86_64.CALL "__dark_cli_directory_list"]
                    @ (if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)]))
            | _ -> Error "directoryList expects exactly one path"
        | LIR.FileIsDirectory ->
            match args with
            | [path] ->
                loadCliOperand X86_64.R10 path
                |> Result.map (fun pathLoads ->
                    let copyLoop = freshLabel "is_dir_copy"
                    let copyDone = freshLabel "is_dir_copy_done"
                    let failure = freshLabel "is_dir_failure"
                    let complete = freshLabel "is_dir_complete"
                    pathLoads
                    @ [ X86_64.PUSH X86_64.RDI
                        X86_64.PUSH X86_64.RSI
                        X86_64.PUSH X86_64.RDX
                        X86_64.PUSH X86_64.RCX
                        X86_64.PUSH X86_64.R10
                        X86_64.SUB_imm (X86_64.RSP, 4096)
                        X86_64.MOV_load (X86_64.RCX, X86_64.R10, 8)
                        X86_64.LEA (X86_64.RSI, X86_64.R10, 16)
                        X86_64.MOV_reg (X86_64.RDI, X86_64.RSP)
                        X86_64.XOR_reg (X86_64.R10, X86_64.R10)
                        X86_64.Label copyLoop
                        X86_64.CMP_reg (X86_64.R10, X86_64.RCX)
                        X86_64.Jcc (X86_64.GE, copyDone)
                        X86_64.MOV_reg (scratch, X86_64.RSI)
                        X86_64.ADD_reg (scratch, X86_64.R10)
                        X86_64.MOV_load_byte (scratch, scratch, 0)
                        X86_64.MOV_reg (X86_64.RDX, X86_64.RDI)
                        X86_64.ADD_reg (X86_64.RDX, X86_64.R10)
                        X86_64.MOV_store_byte (X86_64.RDX, 0, scratch)
                        X86_64.ADD_imm (X86_64.R10, 1)
                        X86_64.JMP copyLoop
                        X86_64.Label copyDone
                        X86_64.MOV_reg (scratch, X86_64.RDI)
                        X86_64.ADD_reg (scratch, X86_64.RCX)
                        X86_64.XOR_reg (X86_64.R10, X86_64.R10)
                        X86_64.MOV_store_byte (scratch, 0, X86_64.R10)
                        X86_64.MOV_reg (X86_64.RDI, X86_64.RSP) ]
                    @ loadImm64 X86_64.RSI 65536L
                    @ loadImm64 X86_64.RDX 0L
                    @ loadImm64 X86_64.RAX (int64 syscalls.Open)
                    @ [ X86_64.SYSCALL
                        X86_64.CMP_imm (X86_64.RAX, 0)
                        X86_64.Jcc (X86_64.LT, failure)
                        X86_64.MOV_reg (X86_64.RDI, X86_64.RAX) ]
                    @ loadImm64 X86_64.RAX (int64 syscalls.Close)
                    @ [ X86_64.SYSCALL ]
                    @ loadImm64 X86_64.RAX 1L
                    @ [ X86_64.JMP complete
                        X86_64.Label failure ]
                    @ loadImm64 X86_64.RAX 0L
                    @ [ X86_64.Label complete
                        X86_64.ADD_imm (X86_64.RSP, 4096)
                        X86_64.POP X86_64.R10
                        X86_64.POP X86_64.RCX
                        X86_64.POP X86_64.RDX
                        X86_64.POP X86_64.RSI
                        X86_64.POP X86_64.RDI
                        X86_64.MOV_reg (destReg, X86_64.RAX) ])
            | _ -> Error "fileIsDirectory expects exactly one path"
        | LIR.FileCreateExclusive ->
            match args with
            | [path] ->
                loadCliOperand X86_64.R10 path
                |> Result.map (fun pathLoads ->
                    let copyLoop = freshLabel "create_exclusive_copy"
                    let copyDone = freshLabel "create_exclusive_copy_done"
                    let tooLong = freshLabel "create_exclusive_too_long"
                    let failure = freshLabel "create_exclusive_failure"
                    let complete = freshLabel "create_exclusive_complete"
                    pathLoads
                    @ [ X86_64.PUSH X86_64.RDI
                        X86_64.PUSH X86_64.RSI
                        X86_64.PUSH X86_64.RDX
                        X86_64.PUSH X86_64.RCX
                        X86_64.PUSH X86_64.R10
                        X86_64.SUB_imm (X86_64.RSP, 4096)
                        X86_64.MOV_load (X86_64.RCX, X86_64.R10, 8)
                        X86_64.CMP_imm (X86_64.RCX, 4096)
                        X86_64.Jcc (X86_64.GE, tooLong)
                        X86_64.LEA (X86_64.RSI, X86_64.R10, 16)
                        X86_64.MOV_reg (X86_64.RDI, X86_64.RSP)
                        X86_64.XOR_reg (X86_64.R10, X86_64.R10)
                        X86_64.Label copyLoop
                        X86_64.CMP_reg (X86_64.R10, X86_64.RCX)
                        X86_64.Jcc (X86_64.GE, copyDone)
                        X86_64.MOV_reg (scratch, X86_64.RSI)
                        X86_64.ADD_reg (scratch, X86_64.R10)
                        X86_64.MOV_load_byte (scratch, scratch, 0)
                        X86_64.MOV_reg (X86_64.RDX, X86_64.RDI)
                        X86_64.ADD_reg (X86_64.RDX, X86_64.R10)
                        X86_64.MOV_store_byte (X86_64.RDX, 0, scratch)
                        X86_64.ADD_imm (X86_64.R10, 1)
                        X86_64.JMP copyLoop
                        X86_64.Label copyDone
                        X86_64.MOV_reg (scratch, X86_64.RDI)
                        X86_64.ADD_reg (scratch, X86_64.RCX)
                        X86_64.XOR_reg (X86_64.R10, X86_64.R10)
                        X86_64.MOV_store_byte (scratch, 0, X86_64.R10)
                        X86_64.MOV_reg (X86_64.RDI, X86_64.RSP) ]
                    @ loadImm64 X86_64.RSI 194L // O_RDWR | O_CREAT | O_EXCL
                    @ loadImm64 X86_64.RDX 0o600L
                    @ loadImm64 X86_64.RAX (int64 syscalls.Open)
                    @ [ X86_64.SYSCALL
                        X86_64.CMP_imm (X86_64.RAX, 0)
                        X86_64.Jcc (X86_64.LT, failure)
                        X86_64.MOV_reg (X86_64.RDI, X86_64.RAX) ]
                    @ loadImm64 X86_64.RAX (int64 syscalls.Close)
                    @ [ X86_64.SYSCALL ]
                    @ loadImm64 X86_64.RAX 0L
                    @ [ X86_64.JMP complete
                        X86_64.Label failure
                        X86_64.NEG X86_64.RAX
                        X86_64.JMP complete
                        X86_64.Label tooLong ]
                    @ loadImm64 X86_64.RAX 36L
                    @ [ X86_64.Label complete
                        X86_64.ADD_imm (X86_64.RSP, 4096)
                        X86_64.POP X86_64.R10
                        X86_64.POP X86_64.RCX
                        X86_64.POP X86_64.RDX
                        X86_64.POP X86_64.RSI
                        X86_64.POP X86_64.RDI
                        X86_64.MOV_reg (destReg, X86_64.RAX) ])
            | _ -> Error "fileCreateExclusive expects exactly one path"
        | LIR.SetEnv ->
            match args with
            | [name; value] ->
                loadSocketArgs [name; value] [X86_64.RDI; X86_64.RSI]
                |> Result.map (fun loads ->
                    loads
                    @ [X86_64.CALL "__dark_cli_setenv"]
                    @ (if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)]))
            | _ -> Error "setenv expects exactly a name and value"
        | LIR.UnsetEnv ->
            match args with
            | [name] ->
                loadCliOperand X86_64.RDI name
                |> Result.map (fun loads ->
                    loads
                    @ [X86_64.CALL "__dark_cli_unsetenv"]
                    @ (if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)]))
            | _ -> Error "unsetenv expects exactly one name"
        | LIR.Kill ->
            let successLabel = freshLabel $"kill_{ctx.FunctionName}_success"
            let completeLabel = freshLabel $"kill_{ctx.FunctionName}_complete"
            let pushOperand operand =
                match operand with
                | LIR.Imm value -> loadImm64 X86_64.RAX value @ [X86_64.PUSH X86_64.RAX]
                | LIR.Reg reg ->
                    match resolveReg reg with
                    | Ok source -> [X86_64.PUSH source]
                    | Error _ -> []
                | LIR.StackSlot offset ->
                    [ X86_64.MOV_load (X86_64.RAX, X86_64.RBP, int32 (adjustStackOffset ctx offset))
                      X86_64.PUSH X86_64.RAX ]
                | _ -> []
            match args with
            | [pid; signal] ->
                Ok (pushOperand pid
                @ pushOperand signal
                @ [ X86_64.POP X86_64.RSI
                    X86_64.POP X86_64.RDI ]
                @ loadImm64 X86_64.RAX 62L
                @ [ X86_64.SYSCALL
                    X86_64.CMP_imm (X86_64.RAX, 0)
                    X86_64.Jcc (X86_64.GE, successLabel)
                    X86_64.NEG X86_64.RAX
                    X86_64.MOV_reg (X86_64.RDX, X86_64.RAX) ]
                @ emitStringLiteral X86_64.R9 "POSIX error"
                @ [ X86_64.MOV_reg (X86_64.R8, heapPtr)
                    X86_64.ADD_imm (heapPtr, 24)
                    X86_64.MOV_store (X86_64.R8, 0, X86_64.RDX)
                    X86_64.MOV_store (X86_64.R8, 8, X86_64.R9)
                    X86_64.MOV_imm32 (X86_64.R10, 1)
                    X86_64.MOV_store (X86_64.R8, 16, X86_64.R10) ]
                @ genLeakCounterInc ctx
                @ [ X86_64.MOV_reg (destReg, heapPtr)
                    X86_64.ADD_imm (heapPtr, 24)
                    X86_64.MOV_imm32 (X86_64.R10, 1)
                    X86_64.MOV_store (destReg, 0, X86_64.R10)
                    X86_64.MOV_store (destReg, 8, X86_64.R8)
                    X86_64.MOV_store (destReg, 16, X86_64.R10) ]
                @ genLeakCounterInc ctx
                @ [ X86_64.JMP completeLabel
                    X86_64.Label successLabel
                    X86_64.MOV_reg (destReg, heapPtr)
                    X86_64.ADD_imm (heapPtr, 24)
                    X86_64.XOR_reg (X86_64.R10, X86_64.R10)
                    X86_64.MOV_store (destReg, 0, X86_64.R10)
                    X86_64.MOV_store (destReg, 8, X86_64.R10)
                    X86_64.MOV_imm32 (X86_64.R10, 1)
                    X86_64.MOV_store (destReg, 16, X86_64.R10) ]
                @ genLeakCounterInc ctx
                @ [X86_64.Label completeLabel])
            | _ -> Ok (loadImm64 destReg 0L)
        | LIR.RunProcess ->
            match args with
            | [request] ->
                loadCliOperand X86_64.RDI request
                |> Result.map (fun loads ->
                    loads
                    @ [X86_64.CALL "__dark_cli_run_process"]
                    @ (if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)]))
            | _ -> Error "CLI run process expects one request"
        | LIR.SpawnProcess ->
            match args with
            | [command] ->
                loadCliOperand X86_64.RDI command
                |> Result.map (fun loads ->
                    loads
                    @ [X86_64.CALL "__dark_cli_spawn_process"]
                    @ (if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)]))
            | _ -> Error "CLI spawn process expects one command"
        | LIR.ProcessIO ->
            match args with
            | [handle; input] ->
                loadSocketArgs [handle; input] [X86_64.RDI; X86_64.RSI]
                |> Result.map (fun loads ->
                    loads
                    @ [ X86_64.XOR_reg (X86_64.RDX, X86_64.RDX)
                        X86_64.CALL "__dark_cli_process_io" ]
                    @ (if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)]))
            | _ -> Error "CLI process IO expects a handle and input"
        | LIR.TerminateProcess ->
            match args with
            | [handle] ->
                loadCliOperand X86_64.RDI handle
                |> Result.map (fun loads ->
                    loads
                    @ [X86_64.CALL "__dark_cli_terminate_process"]
                    @ (if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)]))
            | _ -> Error "CLI terminate process expects one handle")
