// NativeEffects.fs - Emit arm64 instructions for nativeeffects operations.

module ARM64EmitNativeEffects

open ARM64CodeGenTypes
open ARM64HeapAllocation
open ARM64LeakAccounting
open ARM64Operands

let private emitManagedStringFromStack
    (ctx: CodeGenContext)
    (labelPrefix: string)
    (destReg: ARM64Symbolic.Reg)
    (stackOffset: uint16)
    (releaseStack: ARM64Symbolic.Instr list)
    : ARM64Symbolic.Instr list =
    let lengthLoop = $"{labelPrefix}_length"
    let lengthDone = $"{labelPrefix}_length_done"
    let copyLoop = $"{labelPrefix}_copy"
    let copyDone = $"{labelPrefix}_copy_done"
    [ ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.SP, stackOffset)
      ARM64Symbolic.MOVZ (ARM64Symbolic.X10, 0us, 0)
      ARM64Symbolic.Label lengthLoop
      ARM64Symbolic.LDRB (ARM64Symbolic.X11, ARM64Symbolic.X9, ARM64Symbolic.X10)
      ARM64Symbolic.CBZ (ARM64Symbolic.X11, lengthDone)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X10, 1us)
      ARM64Symbolic.B_label lengthLoop
      ARM64Symbolic.Label lengthDone
      ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X28)
      ARM64Symbolic.MOVZ (ARM64Symbolic.X11, 1us, 0)
      ARM64Symbolic.STR (ARM64Symbolic.X11, destReg, 0s)
      ARM64Symbolic.STR (ARM64Symbolic.X10, destReg, 8s)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, ARM64Symbolic.X10, 7us)
      ARM64Symbolic.LSR_imm (ARM64Symbolic.X12, ARM64Symbolic.X12, 3)
      ARM64Symbolic.LSL_imm (ARM64Symbolic.X12, ARM64Symbolic.X12, 3)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, ARM64Symbolic.X12, 16us)
      ARM64Symbolic.ADD_reg (ARM64Symbolic.X28, ARM64Symbolic.X28, ARM64Symbolic.X12)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X11, destReg, 16us)
      ARM64Symbolic.MOV_reg (ARM64Symbolic.X12, ARM64Symbolic.X10)
      ARM64Symbolic.Label copyLoop
      ARM64Symbolic.CBZ (ARM64Symbolic.X12, copyDone)
      ARM64Symbolic.LDRB_imm (ARM64Symbolic.X13, ARM64Symbolic.X9, 0)
      ARM64Symbolic.STRB_reg (ARM64Symbolic.X13, ARM64Symbolic.X11)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 1us)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X11, ARM64Symbolic.X11, 1us)
      ARM64Symbolic.SUB_imm (ARM64Symbolic.X12, ARM64Symbolic.X12, 1us)
      ARM64Symbolic.B_label copyLoop
      ARM64Symbolic.Label copyDone ]
    @ releaseStack
    @ generateLeakCounterInc ctx

let private emitDirectoryCurrent (ctx: CodeGenContext) (destReg: ARM64Symbolic.Reg) : ARM64Symbolic.Instr list =
    let labelPrefix = $"__cwd_{ctx.FunctionName}_{ctx.InstructionSite}"
    let failure = $"{labelPrefix}_failure"
    let complete = $"{labelPrefix}_complete"
    let syscalls = ARM64.targetSyscalls ctx.Target
    let syscallNumber =
        match ARM64.targetOS ctx.Target with
        | Platform.Linux -> 17us
        | Platform.MacOS -> 326us
    let failureCheck =
        match ARM64.targetOS ctx.Target with
        | Platform.Linux ->
            [ ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 0us)
              ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, failure) ]
        | Platform.MacOS -> [ARM64Symbolic.B_cond_label (ARM64Symbolic.HS, failure)]
    [ ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
      ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
      ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.SP) ]
    @ loadImmediate ARM64Symbolic.X1 4096L
    @ [ ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscallNumber, 0)
        ARM64Symbolic.SVC syscalls.SvcImmediate ]
    @ failureCheck
    @ emitManagedStringFromStack
        ctx
        labelPrefix
        destReg
        0us
        [ ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
          ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
          ARM64Symbolic.B_label complete ]
    @ [ ARM64Symbolic.Label failure
        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us) ]
    @ loadStringLiteralPointer destReg ""
    @ [ARM64Symbolic.Label complete]

let private emitEnvironmentPacked (ctx: CodeGenContext) (destReg: ARM64Symbolic.Reg) : ARM64Symbolic.Instr list =
    let labelPrefix = $"__env_all_{ctx.FunctionName}_{ctx.InstructionSite}"
    let findRoot = $"{labelPrefix}_find_root"
    let rootFound = $"{labelPrefix}_root_found"
    let findArgvEnd = $"{labelPrefix}_find_argv_end"
    let countEntry = $"{labelPrefix}_count_entry"
    let countByte = $"{labelPrefix}_count_byte"
    let countNext = $"{labelPrefix}_count_next"
    let countDone = $"{labelPrefix}_count_done"
    let copyEntry = $"{labelPrefix}_copy_entry"
    let copyByte = $"{labelPrefix}_copy_byte"
    let copyNext = $"{labelPrefix}_copy_next"
    let copyDone = $"{labelPrefix}_copy_done"
    [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X9, ARM64Symbolic.X29)
      ARM64Symbolic.Label findRoot
      ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X9, 0s)
      ARM64Symbolic.CBZ (ARM64Symbolic.X10, rootFound)
      ARM64Symbolic.MOV_reg (ARM64Symbolic.X9, ARM64Symbolic.X10)
      ARM64Symbolic.B_label findRoot
      ARM64Symbolic.Label rootFound
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 24us)
      ARM64Symbolic.Label findArgvEnd
      ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X9, 0s)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 8us)
      ARM64Symbolic.CBNZ (ARM64Symbolic.X10, findArgvEnd)
      ARM64Symbolic.MOV_reg (ARM64Symbolic.X14, ARM64Symbolic.X9)
      ARM64Symbolic.MOVZ (ARM64Symbolic.X11, 0us, 0)
      ARM64Symbolic.Label countEntry
      ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X9, 0s)
      ARM64Symbolic.CBZ (ARM64Symbolic.X10, countDone)
      ARM64Symbolic.Label countByte
      ARM64Symbolic.LDRB_imm (ARM64Symbolic.X12, ARM64Symbolic.X10, 0)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X11, ARM64Symbolic.X11, 1us)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X10, 1us)
      ARM64Symbolic.CBZ (ARM64Symbolic.X12, countNext)
      ARM64Symbolic.B_label countByte
      ARM64Symbolic.Label countNext
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 8us)
      ARM64Symbolic.B_label countEntry
      ARM64Symbolic.Label countDone
      ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X28)
      ARM64Symbolic.MOVZ (ARM64Symbolic.X12, 1us, 0)
      ARM64Symbolic.STR (ARM64Symbolic.X12, destReg, 0s)
      ARM64Symbolic.STR (ARM64Symbolic.X11, destReg, 8s)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, ARM64Symbolic.X11, 7us)
      ARM64Symbolic.LSR_imm (ARM64Symbolic.X12, ARM64Symbolic.X12, 3)
      ARM64Symbolic.LSL_imm (ARM64Symbolic.X12, ARM64Symbolic.X12, 3)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, ARM64Symbolic.X12, 16us)
      ARM64Symbolic.ADD_reg (ARM64Symbolic.X28, ARM64Symbolic.X28, ARM64Symbolic.X12)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, destReg, 16us)
      ARM64Symbolic.MOV_reg (ARM64Symbolic.X9, ARM64Symbolic.X14)
      ARM64Symbolic.Label copyEntry
      ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X9, 0s)
      ARM64Symbolic.CBZ (ARM64Symbolic.X10, copyDone)
      ARM64Symbolic.Label copyByte
      ARM64Symbolic.LDRB_imm (ARM64Symbolic.X12, ARM64Symbolic.X10, 0)
      ARM64Symbolic.STRB_reg (ARM64Symbolic.X12, ARM64Symbolic.X13)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X10, 1us)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X13, 1us)
      ARM64Symbolic.CBZ (ARM64Symbolic.X12, copyNext)
      ARM64Symbolic.B_label copyByte
      ARM64Symbolic.Label copyNext
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 8us)
      ARM64Symbolic.B_label copyEntry
      ARM64Symbolic.Label copyDone ]
    @ generateLeakCounterInc ctx

let private emitDirectoryListPacked
    (ctx: CodeGenContext)
    (destReg: ARM64Symbolic.Reg)
    (pathLoads: ARM64Symbolic.Instr list)
    : ARM64Symbolic.Instr list =
    let prefix = $"__list_dir_{ctx.FunctionName}_{ctx.InstructionSite}"
    let pathCopy = $"{prefix}_path_copy"
    let pathDone = $"{prefix}_path_done"
    let openFailed = $"{prefix}_open_failed"
    let readChunk = $"{prefix}_read_chunk"
    let readDone = $"{prefix}_read_done"
    let entryLoop = $"{prefix}_entry_loop"
    let entriesDone = $"{prefix}_entries_done"
    let skipEntry = $"{prefix}_skip_entry"
    let appendPath = $"{prefix}_append_path"
    let appendPathDone = $"{prefix}_append_path_done"
    let appendName = $"{prefix}_append_name"
    let appendNameDone = $"{prefix}_append_name_done"
    let noSlash = $"{prefix}_no_slash"
    let complete = $"{prefix}_complete"
    let syscalls = ARM64.targetSyscalls ctx.Target
    let os = ARM64.targetOS ctx.Target
    let openCall, openFailureCheck, readCall, readFailureCheck, nameOffset =
        match os with
        | Platform.Linux ->
            (loadImmediate ARM64Symbolic.X0 -100L
             @ [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP) ]
             @ loadImmediate ARM64Symbolic.X2 16384L
             @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X3, 0us, 0)
                 ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Open, 0)
                 ARM64Symbolic.SVC syscalls.SvcImmediate ],
             [ ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 0us)
               ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, openFailed) ],
             [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.X9)
               ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.X5) ]
             @ loadImmediate ARM64Symbolic.X2 4096L
             @ [ ARM64Symbolic.MOVZ (syscalls.SyscallRegister, 61us, 0)
                 ARM64Symbolic.SVC syscalls.SvcImmediate ],
             [ ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 0us)
               ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, readDone) ],
             19us)
        | Platform.MacOS ->
            ([ ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.SP) ]
             @ loadImmediate ARM64Symbolic.X1 0x100000L
             @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 0us, 0)
                 ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Open, 0)
                 ARM64Symbolic.SVC syscalls.SvcImmediate ],
             [ARM64Symbolic.B_cond_label (ARM64Symbolic.HS, openFailed)],
             [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.X9)
               ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.X5) ]
             @ loadImmediate ARM64Symbolic.X2 4096L
             @ [ ARM64Symbolic.ADD_imm (ARM64Symbolic.X3, ARM64Symbolic.SP, 4080us)
                 ARM64Symbolic.MOVZ (syscalls.SyscallRegister, 344us, 0)
                 ARM64Symbolic.SVC syscalls.SvcImmediate ],
             [ARM64Symbolic.B_cond_label (ARM64Symbolic.HS, readDone)
              ARM64Symbolic.CBZ (ARM64Symbolic.X0, readDone)],
             21us)
    pathLoads
    @ [ ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
        ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
        ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
        ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X12, ARM64Symbolic.X0)
        ARM64Symbolic.LDR (ARM64Symbolic.X13, ARM64Symbolic.X12, 8s)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X4, ARM64Symbolic.X12, 16us)
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X6, ARM64Symbolic.SP)
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X7, ARM64Symbolic.X13)
        ARM64Symbolic.Label pathCopy
        ARM64Symbolic.CBZ (ARM64Symbolic.X7, pathDone)
        ARM64Symbolic.LDRB_imm (ARM64Symbolic.X8, ARM64Symbolic.X4, 0)
        ARM64Symbolic.STRB_reg (ARM64Symbolic.X8, ARM64Symbolic.X6)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X4, ARM64Symbolic.X4, 1us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X6, ARM64Symbolic.X6, 1us)
        ARM64Symbolic.SUB_imm (ARM64Symbolic.X7, ARM64Symbolic.X7, 1us)
        ARM64Symbolic.B_label pathCopy
        ARM64Symbolic.Label pathDone
        ARM64Symbolic.MOVZ (ARM64Symbolic.X8, 0us, 0)
        ARM64Symbolic.STRB_reg (ARM64Symbolic.X8, ARM64Symbolic.X6) ]
    @ openCall
    @ openFailureCheck
    @ [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X9, ARM64Symbolic.X0)
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X10, ARM64Symbolic.X28)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X11, 0us, 0)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X5, ARM64Symbolic.SP, 2048us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X5, ARM64Symbolic.X5, 2048us)
        ARM64Symbolic.Label readChunk ]
    @ readCall
    @ readFailureCheck
    @ [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X14, ARM64Symbolic.X0)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 0us, 0)
        ARM64Symbolic.Label entryLoop
        ARM64Symbolic.CMP_reg (ARM64Symbolic.X15, ARM64Symbolic.X14)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, entriesDone)
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X4, ARM64Symbolic.X5, ARM64Symbolic.X15)
        ARM64Symbolic.LDRB_imm (ARM64Symbolic.X6, ARM64Symbolic.X4, 16)
        ARM64Symbolic.LDRB_imm (ARM64Symbolic.X7, ARM64Symbolic.X4, 17)
        ARM64Symbolic.LSL_imm (ARM64Symbolic.X7, ARM64Symbolic.X7, 8)
        ARM64Symbolic.ORR_reg (ARM64Symbolic.X6, ARM64Symbolic.X6, ARM64Symbolic.X7)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X8, ARM64Symbolic.X4, nameOffset)
        ARM64Symbolic.LDRB_imm (ARM64Symbolic.X7, ARM64Symbolic.X8, 0)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X7, 46us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, appendPath)
        ARM64Symbolic.LDRB_imm (ARM64Symbolic.X7, ARM64Symbolic.X8, 1)
        ARM64Symbolic.CBZ (ARM64Symbolic.X7, skipEntry)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X7, 46us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, appendPath)
        ARM64Symbolic.LDRB_imm (ARM64Symbolic.X7, ARM64Symbolic.X8, 2)
        ARM64Symbolic.CBZ (ARM64Symbolic.X7, skipEntry)
        ARM64Symbolic.Label appendPath
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.X12, 16us)
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X2, ARM64Symbolic.X13)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X3, ARM64Symbolic.X10, 16us)
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X3, ARM64Symbolic.X3, ARM64Symbolic.X11)
        ARM64Symbolic.Label appendPathDone
        ARM64Symbolic.CBZ (ARM64Symbolic.X2, noSlash)
        ARM64Symbolic.LDRB_imm (ARM64Symbolic.X7, ARM64Symbolic.X1, 0)
        ARM64Symbolic.STRB_reg (ARM64Symbolic.X7, ARM64Symbolic.X3)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.X1, 1us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X3, ARM64Symbolic.X3, 1us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X11, ARM64Symbolic.X11, 1us)
        ARM64Symbolic.SUB_imm (ARM64Symbolic.X2, ARM64Symbolic.X2, 1us)
        ARM64Symbolic.B_label appendPathDone
        ARM64Symbolic.Label noSlash
        ARM64Symbolic.CBZ (ARM64Symbolic.X13, appendName)
        ARM64Symbolic.SUB_imm (ARM64Symbolic.X1, ARM64Symbolic.X13, 1us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X2, ARM64Symbolic.X12, 16us)
        ARM64Symbolic.LDRB (ARM64Symbolic.X7, ARM64Symbolic.X2, ARM64Symbolic.X1)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X7, 47us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, appendName)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X7, 47us, 0)
        ARM64Symbolic.STRB_reg (ARM64Symbolic.X7, ARM64Symbolic.X3)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X3, ARM64Symbolic.X3, 1us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X11, ARM64Symbolic.X11, 1us)
        ARM64Symbolic.Label appendName
        ARM64Symbolic.LDRB_imm (ARM64Symbolic.X7, ARM64Symbolic.X8, 0)
        ARM64Symbolic.CBZ (ARM64Symbolic.X7, appendNameDone)
        ARM64Symbolic.STRB_reg (ARM64Symbolic.X7, ARM64Symbolic.X3)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X8, ARM64Symbolic.X8, 1us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X3, ARM64Symbolic.X3, 1us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X11, ARM64Symbolic.X11, 1us)
        ARM64Symbolic.B_label appendName
        ARM64Symbolic.Label appendNameDone
        ARM64Symbolic.MOVZ (ARM64Symbolic.X7, 0us, 0)
        ARM64Symbolic.STRB_reg (ARM64Symbolic.X7, ARM64Symbolic.X3)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X11, ARM64Symbolic.X11, 1us)
        ARM64Symbolic.Label skipEntry
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X15, ARM64Symbolic.X15, ARM64Symbolic.X6)
        ARM64Symbolic.B_label entryLoop
        ARM64Symbolic.Label entriesDone
        ARM64Symbolic.B_label readChunk
        ARM64Symbolic.Label readDone
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.X9)
        ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Close, 0)
        ARM64Symbolic.SVC syscalls.SvcImmediate
        ARM64Symbolic.MOVZ (ARM64Symbolic.X7, 1us, 0)
        ARM64Symbolic.STR (ARM64Symbolic.X7, ARM64Symbolic.X10, 0s)
        ARM64Symbolic.STR (ARM64Symbolic.X11, ARM64Symbolic.X10, 8s)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X7, ARM64Symbolic.X11, 7us)
        ARM64Symbolic.LSR_imm (ARM64Symbolic.X7, ARM64Symbolic.X7, 3)
        ARM64Symbolic.LSL_imm (ARM64Symbolic.X7, ARM64Symbolic.X7, 3)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X7, ARM64Symbolic.X7, 16us)
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X28, ARM64Symbolic.X28, ARM64Symbolic.X7)
        ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X10)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us) ]
    @ generateLeakCounterInc ctx
    @ [ ARM64Symbolic.B_label complete
        ARM64Symbolic.Label openFailed
        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us) ]
    @ loadStringLiteralPointer destReg ""
    @ [ARM64Symbolic.Label complete]

let private emitUnitOk (ctx: CodeGenContext) (destReg: ARM64Symbolic.Reg) : ARM64Symbolic.Instr list =
    [ ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X28)
      ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, 24us)
      ARM64Symbolic.MOVZ (ARM64Symbolic.X9, 0us, 0)
      ARM64Symbolic.STR (ARM64Symbolic.X9, destReg, 0s)
      ARM64Symbolic.STR (ARM64Symbolic.X9, destReg, 8s)
      ARM64Symbolic.MOVZ (ARM64Symbolic.X9, 1us, 0)
      ARM64Symbolic.STR (ARM64Symbolic.X9, destReg, 16s) ]
    @ generateLeakCounterInc ctx

let private emitSetEnv
    (ctx: CodeGenContext)
    (destReg: ARM64Symbolic.Reg)
    (loads: ARM64Symbolic.Instr list)
    : ARM64Symbolic.Instr list =
    let prefix = $"__setenv_{ctx.FunctionName}_{ctx.InstructionSite}"
    let findRoot = $"{prefix}_find_root"
    let rootFound = $"{prefix}_root_found"
    let findArgvEnd = $"{prefix}_find_argv_end"
    let nextEntry = $"{prefix}_next_entry"
    let compare = $"{prefix}_compare"
    let nameMatched = $"{prefix}_name_matched"
    let useSlot = $"{prefix}_use_slot"
    let copyName = $"{prefix}_copy_name"
    let nameDone = $"{prefix}_name_done"
    let copyValue = $"{prefix}_copy_value"
    let valueDone = $"{prefix}_value_done"
    let nextSlot = $"{prefix}_next_slot"
    let stored = $"{prefix}_stored"
    loads
    @ [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X9, ARM64Symbolic.X29)
        ARM64Symbolic.Label findRoot
        ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X9, 0s)
        ARM64Symbolic.CBZ (ARM64Symbolic.X10, rootFound)
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X9, ARM64Symbolic.X10)
        ARM64Symbolic.B_label findRoot
        ARM64Symbolic.Label rootFound
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 24us)
        ARM64Symbolic.Label findArgvEnd
        ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X9, 0s)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 8us)
        ARM64Symbolic.CBNZ (ARM64Symbolic.X10, findArgvEnd)
        ARM64Symbolic.LDR (ARM64Symbolic.X11, ARM64Symbolic.X14, 8s)
        ARM64Symbolic.Label nextEntry
        ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X9, 0s)
        ARM64Symbolic.CBZ (ARM64Symbolic.X10, useSlot)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X12, 0us, 0)
        ARM64Symbolic.Label compare
        ARM64Symbolic.CMP_reg (ARM64Symbolic.X12, ARM64Symbolic.X11)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, nameMatched)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X14, 16us)
        ARM64Symbolic.LDRB (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X12)
        ARM64Symbolic.LDRB (ARM64Symbolic.X15, ARM64Symbolic.X10, ARM64Symbolic.X12)
        ARM64Symbolic.CMP_reg (ARM64Symbolic.X13, ARM64Symbolic.X15)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, nextSlot)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, ARM64Symbolic.X12, 1us)
        ARM64Symbolic.B_label compare
        ARM64Symbolic.Label nameMatched
        ARM64Symbolic.LDRB (ARM64Symbolic.X13, ARM64Symbolic.X10, ARM64Symbolic.X12)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X13, 61us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, useSlot)
        ARM64Symbolic.Label nextSlot
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 8us)
        ARM64Symbolic.B_label nextEntry
        ARM64Symbolic.Label useSlot
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X10, ARM64Symbolic.X28)
        ARM64Symbolic.LDR (ARM64Symbolic.X12, ARM64Symbolic.X1, 8s)
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X13, ARM64Symbolic.X11, ARM64Symbolic.X12)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X13, 9us)
        ARM64Symbolic.LSR_imm (ARM64Symbolic.X13, ARM64Symbolic.X13, 3)
        ARM64Symbolic.LSL_imm (ARM64Symbolic.X13, ARM64Symbolic.X13, 3)
        ARM64Symbolic.ADD_reg (ARM64Symbolic.X28, ARM64Symbolic.X28, ARM64Symbolic.X13)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X14, 16us)
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X15, ARM64Symbolic.X10)
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X12, ARM64Symbolic.X11)
        ARM64Symbolic.Label copyName
        ARM64Symbolic.CBZ (ARM64Symbolic.X12, nameDone)
        ARM64Symbolic.LDRB_imm (ARM64Symbolic.X11, ARM64Symbolic.X13, 0)
        ARM64Symbolic.STRB_reg (ARM64Symbolic.X11, ARM64Symbolic.X15)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X13, 1us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
        ARM64Symbolic.SUB_imm (ARM64Symbolic.X12, ARM64Symbolic.X12, 1us)
        ARM64Symbolic.B_label copyName
        ARM64Symbolic.Label nameDone
        ARM64Symbolic.MOVZ (ARM64Symbolic.X11, 61us, 0)
        ARM64Symbolic.STRB_reg (ARM64Symbolic.X11, ARM64Symbolic.X15)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X1, 16us)
        ARM64Symbolic.LDR (ARM64Symbolic.X12, ARM64Symbolic.X1, 8s)
        ARM64Symbolic.Label copyValue
        ARM64Symbolic.CBZ (ARM64Symbolic.X12, valueDone)
        ARM64Symbolic.LDRB_imm (ARM64Symbolic.X11, ARM64Symbolic.X13, 0)
        ARM64Symbolic.STRB_reg (ARM64Symbolic.X11, ARM64Symbolic.X15)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X13, 1us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X15, ARM64Symbolic.X15, 1us)
        ARM64Symbolic.SUB_imm (ARM64Symbolic.X12, ARM64Symbolic.X12, 1us)
        ARM64Symbolic.B_label copyValue
        ARM64Symbolic.Label valueDone
        ARM64Symbolic.MOVZ (ARM64Symbolic.X11, 0us, 0)
        ARM64Symbolic.STRB_reg (ARM64Symbolic.X11, ARM64Symbolic.X15)
        ARM64Symbolic.LDR (ARM64Symbolic.X11, ARM64Symbolic.X9, 0s)
        ARM64Symbolic.STR (ARM64Symbolic.X10, ARM64Symbolic.X9, 0s)
        ARM64Symbolic.CBNZ (ARM64Symbolic.X11, stored)
        ARM64Symbolic.STR (ARM64Symbolic.X11, ARM64Symbolic.X9, 8s)
        ARM64Symbolic.Label stored ]
    @ emitUnitOk ctx destReg

let private emitUnsetEnv
    (ctx: CodeGenContext)
    (destReg: ARM64Symbolic.Reg)
    (loads: ARM64Symbolic.Instr list)
    : ARM64Symbolic.Instr list =
    let prefix = $"__unsetenv_{ctx.FunctionName}_{ctx.InstructionSite}"
    let findRoot = $"{prefix}_find_root"
    let rootFound = $"{prefix}_root_found"
    let findArgvEnd = $"{prefix}_find_argv_end"
    let nextEntry = $"{prefix}_next_entry"
    let compare = $"{prefix}_compare"
    let nameMatched = $"{prefix}_name_matched"
    let advance = $"{prefix}_advance"
    let shift = $"{prefix}_shift"
    let doneLabel = $"{prefix}_done"
    loads
    @ [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X9, ARM64Symbolic.X29)
        ARM64Symbolic.Label findRoot
        ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X9, 0s)
        ARM64Symbolic.CBZ (ARM64Symbolic.X10, rootFound)
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X9, ARM64Symbolic.X10)
        ARM64Symbolic.B_label findRoot
        ARM64Symbolic.Label rootFound
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 24us)
        ARM64Symbolic.Label findArgvEnd
        ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X9, 0s)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 8us)
        ARM64Symbolic.CBNZ (ARM64Symbolic.X10, findArgvEnd)
        ARM64Symbolic.LDR (ARM64Symbolic.X11, ARM64Symbolic.X0, 8s)
        ARM64Symbolic.Label nextEntry
        ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X9, 0s)
        ARM64Symbolic.CBZ (ARM64Symbolic.X10, doneLabel)
        ARM64Symbolic.MOVZ (ARM64Symbolic.X12, 0us, 0)
        ARM64Symbolic.Label compare
        ARM64Symbolic.CMP_reg (ARM64Symbolic.X12, ARM64Symbolic.X11)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, nameMatched)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X0, 16us)
        ARM64Symbolic.LDRB (ARM64Symbolic.X13, ARM64Symbolic.X13, ARM64Symbolic.X12)
        ARM64Symbolic.LDRB (ARM64Symbolic.X14, ARM64Symbolic.X10, ARM64Symbolic.X12)
        ARM64Symbolic.CMP_reg (ARM64Symbolic.X13, ARM64Symbolic.X14)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, advance)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, ARM64Symbolic.X12, 1us)
        ARM64Symbolic.B_label compare
        ARM64Symbolic.Label nameMatched
        ARM64Symbolic.LDRB (ARM64Symbolic.X13, ARM64Symbolic.X10, ARM64Symbolic.X12)
        ARM64Symbolic.CMP_imm (ARM64Symbolic.X13, 61us)
        ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, advance)
        ARM64Symbolic.MOV_reg (ARM64Symbolic.X13, ARM64Symbolic.X9)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X14, ARM64Symbolic.X9, 8us)
        ARM64Symbolic.Label shift
        ARM64Symbolic.LDR (ARM64Symbolic.X15, ARM64Symbolic.X14, 0s)
        ARM64Symbolic.STR (ARM64Symbolic.X15, ARM64Symbolic.X13, 0s)
        ARM64Symbolic.CBZ (ARM64Symbolic.X15, doneLabel)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X13, 8us)
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X14, ARM64Symbolic.X14, 8us)
        ARM64Symbolic.B_label shift
        ARM64Symbolic.Label advance
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 8us)
        ARM64Symbolic.B_label nextEntry
        ARM64Symbolic.Label doneLabel ]
    @ emitUnitOk ctx destReg

let internal emitRandomInt64 (ctx: CodeGenContext) (dest: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // Generate random 8 bytes as Int64
    lirRegToARM64Reg dest
    |> Result.map (fun destReg ->
        runtimeInstrs (ARM64HostValues.generateRandomInt64 ctx.Target destReg))

let internal emitDateTimeNow (ctx: CodeGenContext) (dest: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // Generate the current UTC instant as 100ns Unix ticks.
    lirRegToARM64Reg dest
    |> Result.map (fun destReg ->
        runtimeInstrs (ARM64HostValues.generateDateTimeNow ctx.Target destReg))

let internal emitSleep (ctx: CodeGenContext) (effectId: int) (delayMs: LIR.FReg) : Result<ARM64Symbolic.Instr list, string> =
    lirFRegToARM64FReg delayMs
    |> Result.map (fun delayReg ->
        let syscalls = ARM64.targetSyscalls ctx.Target
        let label suffix = $"__sleep_{ctx.FunctionName}_{effectId}_{ctx.InstructionSite}_{suffix}"
        let retryLabel = label "retry"
        let interruptedLabel = label "interrupted"
        let releaseLabel = label "release"
        let completeLabel = label "complete"
        let millionLabel = floatDataLabel 1000000.0
        let resultCheck =
            match ARM64.targetOS ctx.Target with
            | Platform.Linux ->
                loadImmediate ARM64Symbolic.X12 -4L
                @ [ ARM64Symbolic.CMP_reg (ARM64Symbolic.X0, ARM64Symbolic.X12)
                    ARM64Symbolic.B_cond_label (ARM64Symbolic.EQ, interruptedLabel)
                    ARM64Symbolic.B_label releaseLabel ]
            | Platform.MacOS ->
                [ ARM64Symbolic.B_cond_label (ARM64Symbolic.HS, interruptedLabel)
                  ARM64Symbolic.B_label releaseLabel ]
        let interruptCheck =
            match ARM64.targetOS ctx.Target with
            | Platform.Linux -> []
            | Platform.MacOS ->
                [ ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 4us)
                  ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, releaseLabel) ]
        [ ARM64Symbolic.ADRP (ARM64Symbolic.X9, millionLabel)
          ARM64Symbolic.ADD_label (ARM64Symbolic.X9, ARM64Symbolic.X9, millionLabel)
          ARM64Symbolic.LDR_fp (ARM64Symbolic.D16, ARM64Symbolic.X9, 0s)
          ARM64Symbolic.FMUL (ARM64Symbolic.D16, delayReg, ARM64Symbolic.D16)
          ARM64Symbolic.FCVTZS (ARM64Symbolic.X9, ARM64Symbolic.D16)
          ARM64Symbolic.CMP_imm (ARM64Symbolic.X9, 0us)
          ARM64Symbolic.B_cond_label (ARM64Symbolic.LE, completeLabel) ]
        @ loadImmediate ARM64Symbolic.X12 1000000000L
        @ [ ARM64Symbolic.SDIV (ARM64Symbolic.X10, ARM64Symbolic.X9, ARM64Symbolic.X12)
            ARM64Symbolic.MSUB (ARM64Symbolic.X11, ARM64Symbolic.X10, ARM64Symbolic.X12, ARM64Symbolic.X9)
            ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 32us)
            ARM64Symbolic.STP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 0s)
            ARM64Symbolic.MOVZ (ARM64Symbolic.X12, 0us, 0)
            ARM64Symbolic.STP (ARM64Symbolic.X12, ARM64Symbolic.X12, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.Label retryLabel
            ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.SP)
            ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.SP, 16us)
            ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Nanosleep, 0)
            ARM64Symbolic.SVC syscalls.SvcImmediate ]
        @ resultCheck
        @ [ ARM64Symbolic.Label interruptedLabel ]
        @ interruptCheck
        @ [ ARM64Symbolic.LDP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 16s)
            ARM64Symbolic.STP (ARM64Symbolic.X10, ARM64Symbolic.X11, ARM64Symbolic.SP, 0s)
            ARM64Symbolic.B_label retryLabel
            ARM64Symbolic.Label releaseLabel
            ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 32us)
            ARM64Symbolic.Label completeLabel ])

let internal emitCliNative (ctx: CodeGenContext) (dest: LIR.Reg) (operation: LIR.CliOperation) (args: LIR.Operand list) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        match operation with
        | LIR.HostOS ->
            Ok [ARM64Symbolic.MOVZ (destReg, (if ARM64.targetOS ctx.Target = Platform.MacOS then 2us else 1us), 0)]
        | LIR.HostArchitecture ->
            Ok [ARM64Symbolic.MOVZ (destReg, (if ARM64.targetOS ctx.Target = Platform.MacOS then 3us else 2us), 0)]
        | LIR.Hostname ->
            let label suffix = $"__hostname_{ctx.FunctionName}_{ctx.InstructionSite}_{suffix}"
            let failureLabel = label "failure"
            let lengthLabel = label "length"
            let lengthDoneLabel = label "length_done"
            let copyLabel = label "copy"
            let copyDoneLabel = label "copy_done"
            let completeLabel = label "complete"
            let os = ARM64.targetOS ctx.Target
            let stackSize, nodeOffset, syscallNumber =
                match os with
                | Platform.Linux -> (400us, 65us, 160us)
                | Platform.MacOS -> (1280us, 256us, 164us)
            let failureCheck =
                match os with
                | Platform.Linux ->
                    [ ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 0us)
                      ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, failureLabel) ]
                | Platform.MacOS -> [ARM64Symbolic.B_cond_label (ARM64Symbolic.HS, failureLabel)]
            let normalizeErrno =
                match os with
                | Platform.Linux -> [ARM64Symbolic.NEG (ARM64Symbolic.X2, ARM64Symbolic.X0)]
                | Platform.MacOS -> [ARM64Symbolic.MOV_reg (ARM64Symbolic.X2, ARM64Symbolic.X0)]
            Ok (
                [ ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, stackSize)
                  ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.SP)
                  ARM64Symbolic.MOVZ ((ARM64.targetSyscalls ctx.Target).SyscallRegister, syscallNumber, 0)
                  ARM64Symbolic.SVC (ARM64.targetSyscalls ctx.Target).SvcImmediate ]
                @ failureCheck
                @ [ ARM64Symbolic.ADD_imm (ARM64Symbolic.X2, ARM64Symbolic.SP, nodeOffset)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X3, 0us, 0)
                    ARM64Symbolic.Label lengthLabel
                    ARM64Symbolic.LDRB (ARM64Symbolic.X4, ARM64Symbolic.X2, ARM64Symbolic.X3)
                    ARM64Symbolic.CBZ (ARM64Symbolic.X4, lengthDoneLabel)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X3, ARM64Symbolic.X3, 1us)
                    ARM64Symbolic.B_label lengthLabel
                    ARM64Symbolic.Label lengthDoneLabel
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X5, ARM64Symbolic.X28)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X8, 1us, 0)
                    ARM64Symbolic.STR (ARM64Symbolic.X8, ARM64Symbolic.X5, 0s)
                    ARM64Symbolic.STR (ARM64Symbolic.X3, ARM64Symbolic.X5, 8s)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X6, ARM64Symbolic.X3, 7us)
                    ARM64Symbolic.LSR_imm (ARM64Symbolic.X6, ARM64Symbolic.X6, 3)
                    ARM64Symbolic.LSL_imm (ARM64Symbolic.X6, ARM64Symbolic.X6, 3)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X7, ARM64Symbolic.X6, 16us)
                    ARM64Symbolic.ADD_reg (ARM64Symbolic.X28, ARM64Symbolic.X28, ARM64Symbolic.X7)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X7, ARM64Symbolic.X5, 16us)
                    ARM64Symbolic.MOV_reg (ARM64Symbolic.X8, ARM64Symbolic.X3)
                    ARM64Symbolic.Label copyLabel
                    ARM64Symbolic.CBZ (ARM64Symbolic.X8, copyDoneLabel)
                    ARM64Symbolic.LDRB_imm (ARM64Symbolic.X9, ARM64Symbolic.X2, 0)
                    ARM64Symbolic.STRB_reg (ARM64Symbolic.X9, ARM64Symbolic.X7)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X2, ARM64Symbolic.X2, 1us)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X7, ARM64Symbolic.X7, 1us)
                    ARM64Symbolic.SUB_imm (ARM64Symbolic.X8, ARM64Symbolic.X8, 1us)
                    ARM64Symbolic.B_label copyLabel
                    ARM64Symbolic.Label copyDoneLabel
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, stackSize) ]
                @ generateLeakCounterInc ctx
                @ [ ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X28)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, 24us)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X8, 0us, 0)
                    ARM64Symbolic.STR (ARM64Symbolic.X8, destReg, 0s)
                    ARM64Symbolic.STR (ARM64Symbolic.X5, destReg, 8s)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X8, 1us, 0)
                    ARM64Symbolic.STR (ARM64Symbolic.X8, destReg, 16s) ]
                @ generateLeakCounterInc ctx
                @ [ ARM64Symbolic.B_label completeLabel
                    ARM64Symbolic.Label failureLabel ]
                @ normalizeErrno
                @ [ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, stackSize)]
                @ loadStringLiteralPointer ARM64Symbolic.X3 "POSIX error"
                @ [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X4, ARM64Symbolic.X28)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, 24us)
                    ARM64Symbolic.STR (ARM64Symbolic.X2, ARM64Symbolic.X4, 0s)
                    ARM64Symbolic.STR (ARM64Symbolic.X3, ARM64Symbolic.X4, 8s)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X5, 1us, 0)
                    ARM64Symbolic.STR (ARM64Symbolic.X5, ARM64Symbolic.X4, 16s) ]
                @ generateLeakCounterInc ctx
                @ [ ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X28)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, 24us)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X5, 1us, 0)
                    ARM64Symbolic.STR (ARM64Symbolic.X5, destReg, 0s)
                    ARM64Symbolic.STR (ARM64Symbolic.X4, destReg, 8s)
                    ARM64Symbolic.STR (ARM64Symbolic.X5, destReg, 16s) ]
                @ generateLeakCounterInc ctx
                @ [ARM64Symbolic.Label completeLabel])
        | LIR.Execute when ARM64.targetOS ctx.Target = Platform.Linux ->
            match args with
            | [command] ->
                loadCliOperand ARM64Symbolic.X0 command
                |> Result.map (fun loads ->
                    loads
                    @ [ARM64Symbolic.BL "__dark_cli_execute"]
                    @ (if destReg = ARM64Symbolic.X0 then []
                       else [ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X0)]))
            | _ -> Error "CLI execute expects exactly one command"
        | LIR.GetPid | LIR.GetUid ->
            let os = ARM64.targetOS ctx.Target
            let number =
                match operation, os with
                | LIR.GetPid, Platform.Linux -> 172us
                | LIR.GetPid, Platform.MacOS -> 20us
                | LIR.GetUid, Platform.Linux -> 174us
                | LIR.GetUid, Platform.MacOS -> 24us
                | _ -> 0us
            let syscalls = ARM64.targetSyscalls ctx.Target
            Ok [ARM64Symbolic.MOVZ (syscalls.SyscallRegister, number, 0)
                ARM64Symbolic.SVC syscalls.SvcImmediate
                ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X0)]
        | LIR.CpuCount ->
            match ARM64.targetOS ctx.Target with
            | Platform.MacOS ->
                let label suffix = $"__cpu_count_{ctx.FunctionName}_{ctx.InstructionSite}_{suffix}"
                let fallback = label "fallback"
                let complete = label "complete"
                Ok (
                    [ ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 32us) ]
                    @ loadImmediate ARM64Symbolic.X9 0x1900000006L
                    @ [ ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.SP, 0s)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X9, 0us, 0)
                        ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.SP, 8s)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X9, 4us, 0)
                        ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.SP, 16s)
                        ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.SP)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X1, 2us, 0)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X2, ARM64Symbolic.SP, 8us)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X3, ARM64Symbolic.SP, 16us)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X4, 0us, 0)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X5, 0us, 0)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X16, 202us, 0)
                        ARM64Symbolic.SVC 0x80us
                        ARM64Symbolic.B_cond_label (ARM64Symbolic.HS, fallback)
                        ARM64Symbolic.LDR (ARM64Symbolic.X9, ARM64Symbolic.SP, 8s)
                        ARM64Symbolic.CBZ (ARM64Symbolic.X9, fallback)
                        ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X9)
                        ARM64Symbolic.B_label complete
                        ARM64Symbolic.Label fallback
                        ARM64Symbolic.MOVZ (destReg, 1us, 0)
                        ARM64Symbolic.Label complete
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 32us) ])
            | Platform.Linux ->
                let label suffix = $"__cpu_count_{ctx.FunctionName}_{ctx.InstructionSite}_{suffix}"
                let byteLoop = label "byte_loop"
                let bitLoop = label "bit_loop"
                let nextByte = label "next_byte"
                let doneLabel = label "done"
                let fallback = label "fallback"
                let complete = label "complete"
                let zeroMask =
                    [0 .. 15]
                    |> List.map (fun index -> ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.SP, int16 (index * 8)))
                Ok (
                    [ ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 128us)
                      ARM64Symbolic.MOVZ (ARM64Symbolic.X9, 0us, 0) ]
                    @ zeroMask
                    @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X0, 0us, 0)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X1, 128us, 0)
                        ARM64Symbolic.MOV_reg (ARM64Symbolic.X2, ARM64Symbolic.SP)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X8, 123us, 0)
                        ARM64Symbolic.SVC 0us
                        ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 0us)
                        ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, fallback)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X9, 0us, 0)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X10, 0us, 0)
                        ARM64Symbolic.Label byteLoop
                        ARM64Symbolic.CMP_imm (ARM64Symbolic.X9, 128us)
                        ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, doneLabel)
                        ARM64Symbolic.LDRB (ARM64Symbolic.X11, ARM64Symbolic.SP, ARM64Symbolic.X9)
                        ARM64Symbolic.Label bitLoop
                        ARM64Symbolic.CBZ (ARM64Symbolic.X11, nextByte)
                        ARM64Symbolic.AND_imm (ARM64Symbolic.X12, ARM64Symbolic.X11, 1UL)
                        ARM64Symbolic.ADD_reg (ARM64Symbolic.X10, ARM64Symbolic.X10, ARM64Symbolic.X12)
                        ARM64Symbolic.LSR_imm (ARM64Symbolic.X11, ARM64Symbolic.X11, 1)
                        ARM64Symbolic.B_label bitLoop
                        ARM64Symbolic.Label nextByte
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 1us)
                        ARM64Symbolic.B_label byteLoop
                        ARM64Symbolic.Label doneLabel
                        ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X10)
                        ARM64Symbolic.B_label complete
                        ARM64Symbolic.Label fallback
                        ARM64Symbolic.MOVZ (destReg, 1us, 0)
                        ARM64Symbolic.Label complete
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 128us) ])
        | LIR.GetArgv ->
            match args with
            | [index] ->
                loadCliOperand ARM64Symbolic.X0 index
                |> Result.map (fun loads ->
                    loads
                    @ [ARM64Symbolic.BL $"__dark_cli_argv_{ctx.FunctionName}"]
                    @ (if destReg = ARM64Symbolic.X0 then []
                       else [ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X0)]))
            | _ -> Error "CLI argv expects exactly one index"
        | LIR.GetEnv ->
            match args with
            | [name] ->
                loadCliOperand ARM64Symbolic.X0 name
                |> Result.map (fun loads ->
                    let label suffix = $"__getenv_{ctx.FunctionName}_{ctx.InstructionSite}_{suffix}"
                    let findRoot = label "find_root"
                    let rootFound = label "root_found"
                    let findArgvEnd = label "find_argv_end"
                    let nextEntry = label "next_entry"
                    let compareName = label "compare_name"
                    let nameMatched = label "name_matched"
                    let findLength = label "find_length"
                    let lengthFound = label "length_found"
                    let copyValue = label "copy_value"
                    let copyDone = label "copy_done"
                    let missing = label "missing"
                    let box = label "box"
                    loads
                    @ [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.X29)
                        ARM64Symbolic.Label findRoot
                        ARM64Symbolic.LDR (ARM64Symbolic.X2, ARM64Symbolic.X1, 0s)
                        ARM64Symbolic.CBZ (ARM64Symbolic.X2, rootFound)
                        ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.X2)
                        ARM64Symbolic.B_label findRoot
                        ARM64Symbolic.Label rootFound
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.X1, 24us)
                        ARM64Symbolic.Label findArgvEnd
                        ARM64Symbolic.LDR (ARM64Symbolic.X2, ARM64Symbolic.X1, 0s)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.X1, 8us)
                        ARM64Symbolic.CBNZ (ARM64Symbolic.X2, findArgvEnd)
                        ARM64Symbolic.LDR (ARM64Symbolic.X3, ARM64Symbolic.X0, 8s)
                        ARM64Symbolic.Label nextEntry
                        ARM64Symbolic.LDR (ARM64Symbolic.X2, ARM64Symbolic.X1, 0s)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X1, ARM64Symbolic.X1, 8us)
                        ARM64Symbolic.CBZ (ARM64Symbolic.X2, missing)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X4, 0us, 0)
                        ARM64Symbolic.Label compareName
                        ARM64Symbolic.CMP_reg (ARM64Symbolic.X4, ARM64Symbolic.X3)
                        ARM64Symbolic.B_cond_label (ARM64Symbolic.GE, nameMatched)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X5, ARM64Symbolic.X0, 16us)
                        ARM64Symbolic.LDRB (ARM64Symbolic.X6, ARM64Symbolic.X5, ARM64Symbolic.X4)
                        ARM64Symbolic.LDRB (ARM64Symbolic.X5, ARM64Symbolic.X2, ARM64Symbolic.X4)
                        ARM64Symbolic.CMP_reg (ARM64Symbolic.X5, ARM64Symbolic.X6)
                        ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, nextEntry)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X4, ARM64Symbolic.X4, 1us)
                        ARM64Symbolic.B_label compareName
                        ARM64Symbolic.Label nameMatched
                        ARM64Symbolic.LDRB (ARM64Symbolic.X5, ARM64Symbolic.X2, ARM64Symbolic.X4)
                        ARM64Symbolic.CMP_imm (ARM64Symbolic.X5, 61us)
                        ARM64Symbolic.B_cond_label (ARM64Symbolic.NE, nextEntry)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X8, ARM64Symbolic.X2, 1us)
                        ARM64Symbolic.ADD_reg (ARM64Symbolic.X8, ARM64Symbolic.X8, ARM64Symbolic.X3)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X9, 0us, 0)
                        ARM64Symbolic.Label findLength
                        ARM64Symbolic.LDRB (ARM64Symbolic.X5, ARM64Symbolic.X8, ARM64Symbolic.X9)
                        ARM64Symbolic.CBZ (ARM64Symbolic.X5, lengthFound)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 1us)
                        ARM64Symbolic.B_label findLength
                        ARM64Symbolic.Label lengthFound
                        ARM64Symbolic.MOV_reg (ARM64Symbolic.X7, ARM64Symbolic.X28)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X11, 1us, 0)
                        ARM64Symbolic.STR (ARM64Symbolic.X11, ARM64Symbolic.X7, 0s)
                        ARM64Symbolic.STR (ARM64Symbolic.X9, ARM64Symbolic.X7, 8s)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X13, ARM64Symbolic.X9, 7us)
                        ARM64Symbolic.LSR_imm (ARM64Symbolic.X13, ARM64Symbolic.X13, 3)
                        ARM64Symbolic.LSL_imm (ARM64Symbolic.X13, ARM64Symbolic.X13, 3)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X14, ARM64Symbolic.X13, 16us)
                        ARM64Symbolic.ADD_reg (ARM64Symbolic.X28, ARM64Symbolic.X28, ARM64Symbolic.X14)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X7, 16us)
                        ARM64Symbolic.MOV_reg (ARM64Symbolic.X11, ARM64Symbolic.X9)
                        ARM64Symbolic.MOV_reg (ARM64Symbolic.X12, ARM64Symbolic.X8)
                        ARM64Symbolic.Label copyValue
                        ARM64Symbolic.CBZ (ARM64Symbolic.X11, copyDone)
                        ARM64Symbolic.LDRB_imm (ARM64Symbolic.X5, ARM64Symbolic.X12, 0)
                        ARM64Symbolic.STRB_reg (ARM64Symbolic.X5, ARM64Symbolic.X10)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X12, ARM64Symbolic.X12, 1us)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X10, 1us)
                        ARM64Symbolic.SUB_imm (ARM64Symbolic.X11, ARM64Symbolic.X11, 1us)
                        ARM64Symbolic.B_label copyValue
                        ARM64Symbolic.Label copyDone ]
                    @ generateLeakCounterInc ctx
                    @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 0us, 0)
                        ARM64Symbolic.B_label box
                        ARM64Symbolic.Label missing
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X15, 1us, 0)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X7, 0us, 0)
                        ARM64Symbolic.Label box
                        ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X28)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, 24us)
                        ARM64Symbolic.STR (ARM64Symbolic.X15, destReg, 0s)
                        ARM64Symbolic.STR (ARM64Symbolic.X7, destReg, 8s)
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X11, 1us, 0)
                        ARM64Symbolic.STR (ARM64Symbolic.X11, destReg, 16s) ]
                    @ generateLeakCounterInc ctx)
            | _ -> Error "CLI getenv expects exactly one name"
        | LIR.GetEnvironmentPacked ->
            Ok (emitEnvironmentPacked ctx destReg)
        | LIR.DirectoryCurrent ->
            Ok (emitDirectoryCurrent ctx destReg)
        | LIR.DirectoryListPacked ->
            match args with
            | [path] ->
                loadCliOperand ARM64Symbolic.X0 path
                |> Result.map (emitDirectoryListPacked ctx destReg)
            | _ -> Error "directoryList expects exactly one path"
        | LIR.FileIsDirectory ->
            match args with
            | [path] ->
                loadCliOperand ARM64Symbolic.X0 path
                |> Result.map (fun loads ->
                    let labelPrefix = $"__is_dir_{ctx.FunctionName}_{ctx.InstructionSite}"
                    let copyLoop = $"{labelPrefix}_copy"
                    let copyDone = $"{labelPrefix}_copy_done"
                    let failure = $"{labelPrefix}_failure"
                    let complete = $"{labelPrefix}_complete"
                    let syscalls = ARM64.targetSyscalls ctx.Target
                    let openCall, failureCheck =
                        match ARM64.targetOS ctx.Target with
                        | Platform.Linux ->
                            (loadImmediate ARM64Symbolic.X0 -100L
                             @ [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP) ]
                             @ loadImmediate ARM64Symbolic.X2 16384L
                             @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X3, 0us, 0)
                                 ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Open, 0)
                                 ARM64Symbolic.SVC syscalls.SvcImmediate ],
                             [ ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 0us)
                               ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, failure) ])
                        | Platform.MacOS ->
                            ([ ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.SP) ]
                             @ loadImmediate ARM64Symbolic.X1 0x100000L
                             @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 0us, 0)
                                 ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Open, 0)
                                 ARM64Symbolic.SVC syscalls.SvcImmediate ],
                             [ARM64Symbolic.B_cond_label (ARM64Symbolic.HS, failure)])
                    loads
                    @ [ ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
                        ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
                        ARM64Symbolic.LDR (ARM64Symbolic.X9, ARM64Symbolic.X0, 8s)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X0, 16us)
                        ARM64Symbolic.MOV_reg (ARM64Symbolic.X11, ARM64Symbolic.SP)
                        ARM64Symbolic.Label copyLoop
                        ARM64Symbolic.CBZ (ARM64Symbolic.X9, copyDone)
                        ARM64Symbolic.LDRB_imm (ARM64Symbolic.X12, ARM64Symbolic.X10, 0)
                        ARM64Symbolic.STRB_reg (ARM64Symbolic.X12, ARM64Symbolic.X11)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X10, 1us)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.X11, ARM64Symbolic.X11, 1us)
                        ARM64Symbolic.SUB_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, 1us)
                        ARM64Symbolic.B_label copyLoop
                        ARM64Symbolic.Label copyDone
                        ARM64Symbolic.MOVZ (ARM64Symbolic.X12, 0us, 0)
                        ARM64Symbolic.STRB_reg (ARM64Symbolic.X12, ARM64Symbolic.X11) ]
                    @ openCall
                    @ failureCheck
                    @ [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X9, ARM64Symbolic.X0)
                        ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Close, 0)
                        ARM64Symbolic.SVC syscalls.SvcImmediate
                        ARM64Symbolic.MOVZ (destReg, 1us, 0)
                        ARM64Symbolic.B_label complete
                        ARM64Symbolic.Label failure
                        ARM64Symbolic.MOVZ (destReg, 0us, 0)
                        ARM64Symbolic.Label complete
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
                        ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us) ])
            | _ -> Error "fileIsDirectory expects exactly one path"
        | LIR.SetEnv ->
            match args with
            | [name; value] ->
                loadCliOperand ARM64Symbolic.X0 name
                |> Result.bind (fun nameLoads ->
                    loadCliOperand ARM64Symbolic.X1 value
                    |> Result.map (fun valueLoads ->
                        emitSetEnv
                            ctx
                            destReg
                            (nameLoads
                             @ [ARM64Symbolic.MOV_reg (ARM64Symbolic.X14, ARM64Symbolic.X0)]
                             @ valueLoads)))
            | _ -> Error "setenv expects exactly a name and value"
        | LIR.UnsetEnv ->
            match args with
            | [name] ->
                loadCliOperand ARM64Symbolic.X0 name
                |> Result.map (emitUnsetEnv ctx destReg)
            | _ -> Error "unsetenv expects exactly one name"
        | LIR.Kill ->
            match args with
            | [pid; signal] ->
                loadCliOperand ARM64Symbolic.X0 pid
                |> Result.bind (fun pidLoads ->
                    loadCliOperand ARM64Symbolic.X1 signal
                    |> Result.map (fun signalLoads ->
                        let success =
                            [ ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X28)
                              ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, 24us)
                              ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 0us, 0)
                              ARM64Symbolic.STR (ARM64Symbolic.X2, destReg, 0s)
                              ARM64Symbolic.STR (ARM64Symbolic.X2, destReg, 8s)
                              ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 1us, 0)
                              ARM64Symbolic.STR (ARM64Symbolic.X2, destReg, 16s) ]
                            @ generateLeakCounterInc ctx
                        let normalizeErrno =
                            match ARM64.targetOS ctx.Target with
                            | Platform.Linux -> [ARM64Symbolic.NEG (ARM64Symbolic.X2, ARM64Symbolic.X0)]
                            | Platform.MacOS -> [ARM64Symbolic.MOV_reg (ARM64Symbolic.X2, ARM64Symbolic.X0)]
                        let failure =
                            normalizeErrno
                            @ loadStringLiteralPointer ARM64Symbolic.X3 "POSIX error"
                            @ [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X4, ARM64Symbolic.X28)
                                ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, 24us)
                                ARM64Symbolic.STR (ARM64Symbolic.X2, ARM64Symbolic.X4, 0s)
                                ARM64Symbolic.STR (ARM64Symbolic.X3, ARM64Symbolic.X4, 8s)
                                ARM64Symbolic.MOVZ (ARM64Symbolic.X5, 1us, 0)
                                ARM64Symbolic.STR (ARM64Symbolic.X5, ARM64Symbolic.X4, 16s) ]
                            @ generateLeakCounterInc ctx
                            @ [ ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X28)
                                ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, 24us)
                                ARM64Symbolic.MOVZ (ARM64Symbolic.X5, 1us, 0)
                                ARM64Symbolic.STR (ARM64Symbolic.X5, destReg, 0s)
                                ARM64Symbolic.STR (ARM64Symbolic.X4, destReg, 8s)
                                ARM64Symbolic.STR (ARM64Symbolic.X5, destReg, 16s) ]
                            @ generateLeakCounterInc ctx
                        let branchToFailure =
                            match ARM64.targetOS ctx.Target with
                            | Platform.Linux -> ARM64Symbolic.B_cond (ARM64Symbolic.LT, List.length success + 2)
                            | Platform.MacOS -> ARM64Symbolic.B_cond (ARM64Symbolic.HS, List.length success + 2)
                        let prepareFailureCheck =
                            match ARM64.targetOS ctx.Target with
                            | Platform.Linux -> [ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 0us)]
                            | Platform.MacOS -> []
                        let syscalls = ARM64.targetSyscalls ctx.Target
                        let killNumber =
                            match ARM64.targetOS ctx.Target with
                            | Platform.Linux -> 129us
                            | Platform.MacOS -> 37us
                        pidLoads
                        @ signalLoads
                        @ [ ARM64Symbolic.MOVZ (syscalls.SyscallRegister, killNumber, 0)
                            ARM64Symbolic.SVC syscalls.SvcImmediate ]
                        @ prepareFailureCheck
                        @ [branchToFailure]
                        @ success
                        @ [ARM64Symbolic.B (List.length failure + 1)]
                        @ failure))
            | _ -> Error "CLI kill expects a pid and signal"
        | LIR.RunProcess when ARM64.targetOS ctx.Target = Platform.Linux ->
            match args with
            | [request] ->
                loadCliOperand ARM64Symbolic.X0 request
                |> Result.map (fun loads ->
                    loads
                    @ [ARM64Symbolic.BL "__dark_cli_run_process"]
                    @ (if destReg = ARM64Symbolic.X0 then [] else [ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X0)]))
            | _ -> Error "CLI run process expects one request"
        | LIR.RunProcess ->
            Ok (loadStringLiteralPointer ARM64Symbolic.X8 ""
                @ loadStringLiteralPointer ARM64Symbolic.X9 "native process execution unavailable"
                @ [ ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X28)
                    ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, 48us)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X10, 38us, 0)
                    ARM64Symbolic.STR (ARM64Symbolic.X10, destReg, 0s)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X10, 0us, 0)
                    ARM64Symbolic.MVN (ARM64Symbolic.X10, ARM64Symbolic.X10)
                    ARM64Symbolic.STR (ARM64Symbolic.X10, destReg, 8s)
                    ARM64Symbolic.STR (ARM64Symbolic.X8, destReg, 16s)
                    ARM64Symbolic.STR (ARM64Symbolic.X9, destReg, 24s)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X10, 0us, 0)
                    ARM64Symbolic.STR (ARM64Symbolic.X10, destReg, 32s)
                    ARM64Symbolic.MOVZ (ARM64Symbolic.X10, 1us, 0)
                    ARM64Symbolic.STR (ARM64Symbolic.X10, destReg, 40s) ])
        | LIR.SpawnProcess when ARM64.targetOS ctx.Target = Platform.Linux ->
            match args with
            | [command] ->
                loadCliOperand ARM64Symbolic.X0 command
                |> Result.map (fun loads ->
                    loads
                    @ [ ARM64Symbolic.BL "__dark_cli_spawn_process"
                        ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X0) ])
            | _ -> Error "CLI spawn process expects one command"
        | LIR.ProcessIO when ARM64.targetOS ctx.Target = Platform.Linux ->
            match args with
            | [handle; input] ->
                loadCliOperand ARM64Symbolic.X0 handle
                |> Result.bind (fun handleLoads ->
                    loadCliOperand ARM64Symbolic.X1 input
                    |> Result.map (fun inputLoads ->
                        handleLoads
                        @ inputLoads
                        @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 0us, 0)
                            ARM64Symbolic.BL "__dark_cli_process_io"
                            ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X0) ]))
            | _ -> Error "CLI process IO expects a handle and input"
        | LIR.TerminateProcess when ARM64.targetOS ctx.Target = Platform.Linux ->
            match args with
            | [handle] ->
                loadCliOperand ARM64Symbolic.X0 handle
                |> Result.map (fun loads ->
                    loads
                    @ [ ARM64Symbolic.BL "__dark_cli_terminate_process"
                        ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X0) ])
            | _ -> Error "CLI terminate process expects one handle"
        | LIR.Execute | LIR.ProcessIO | LIR.TerminateProcess ->
            let errorMessage =
                match operation with
                | LIR.ProcessIO | LIR.TerminateProcess -> "Invalid process handle"
                | _ -> "native CLI operation unavailable"
            Ok (loadStringLiteralPointer ARM64Symbolic.X8 ""
            @ loadStringLiteralPointer ARM64Symbolic.X9 errorMessage
            @ [ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X28)
               ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, 32us)]
            @ [ARM64Symbolic.MOVZ (ARM64Symbolic.X10, 0us, 0)
               ARM64Symbolic.MVN (ARM64Symbolic.X10, ARM64Symbolic.X10)
               ARM64Symbolic.STR (ARM64Symbolic.X10, destReg, 0s)
               ARM64Symbolic.STR (ARM64Symbolic.X8, destReg, 8s)
               ARM64Symbolic.STR (ARM64Symbolic.X9, destReg, 16s)
               ARM64Symbolic.MOVZ (ARM64Symbolic.X10, 1us, 0)
               ARM64Symbolic.STR (ARM64Symbolic.X10, destReg, 24s)])
        | LIR.SpawnProcess -> Ok (loadImmediate destReg -1L))

let internal emitCoverageHit (ctx: CodeGenContext) (exprId: int) : Result<ARM64Symbolic.Instr list, string> =
    // Increment coverage counter at _coverage_data[exprId * 8]
    // Uses PC-relative addressing (ADRP+ADD) to get BSS buffer address
    // Uses X9 and X10 as scratch registers
    let offset = exprId * 8
    Ok ([
        // Get address of coverage buffer using PC-relative addressing
        ARM64Symbolic.ADRP (ARM64Symbolic.X9, dataLabel ARM64Symbolic.coverageDataLabelName)
        ARM64Symbolic.ADD_label (ARM64Symbolic.X9, ARM64Symbolic.X9, dataLabel ARM64Symbolic.coverageDataLabelName)
    ] @
    // Add offset for this expression's counter
    (if offset = 0 then
        []
    elif offset < 4096 then
        [ARM64Symbolic.ADD_imm (ARM64Symbolic.X9, ARM64Symbolic.X9, uint16 offset)]
    else
        loadImmediate ARM64Symbolic.X10 (int64 offset) @ [ARM64Symbolic.ADD_reg (ARM64Symbolic.X9, ARM64Symbolic.X9, ARM64Symbolic.X10)]) @
    [
        ARM64Symbolic.LDR (ARM64Symbolic.X10, ARM64Symbolic.X9, 0s)        // X10 = coverage_buffer[exprId]
        ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X10, 1us)  // X10++
        ARM64Symbolic.STR (ARM64Symbolic.X10, ARM64Symbolic.X9, 0s)        // coverage_buffer[exprId] = X10
    ])
