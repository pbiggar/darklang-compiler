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
        | LIR.SetEnv ->
            match args with
            | [name; value] ->
                loadCliOperand X86_64.RDI name
                |> Result.bind (fun nameLoads ->
                    loadCliOperand X86_64.RSI value
                    |> Result.map (fun valueLoads ->
                        nameLoads
                        @ [X86_64.PUSH X86_64.RDI]
                        @ valueLoads
                        @ [X86_64.POP X86_64.RDI]
                        @ [X86_64.CALL "__dark_cli_setenv"]
                        @ (if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)])))
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
                loadCliOperand X86_64.RDI handle
                |> Result.bind (fun handleLoads ->
                    loadCliOperand X86_64.RSI input
                    |> Result.map (fun inputLoads ->
                        handleLoads @ inputLoads
                        @ [ X86_64.XOR_reg (X86_64.RDX, X86_64.RDX)
                            X86_64.CALL "__dark_cli_process_io" ]
                        @ (if destReg = X86_64.RAX then [] else [X86_64.MOV_reg (destReg, X86_64.RAX)])))
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
