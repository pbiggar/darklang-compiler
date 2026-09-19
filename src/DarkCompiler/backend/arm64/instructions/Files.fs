// Files.fs - Emit arm64 instructions for files operations.

module ARM64EmitFiles

open ARM64CodeGenTypes
open ARM64HeapAllocation
open ARM64LeakAccounting
open ARM64Operands

let internal emitFileReadBlob (ctx: CodeGenContext) (dest: LIR.Reg) (path: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
    // File reading: generates syscall sequence to read file contents
    // Returns Result<Blob, String>
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        match path with
        | LIR.Reg pathReg ->
            // Already a heap string pointer
            lirRegToARM64Reg pathReg
            |> Result.map (fun pathARM64 ->
                runtimeInstrs (ARM64FileRead.generateFileReadBlob ctx.Target destReg pathARM64)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterInc ctx)
        | LIR.StringSymbol value ->
            Ok (
                loadStringLiteralPointer ARM64Symbolic.X15 value
                @ runtimeInstrs (ARM64FileRead.generateFileReadBlob ctx.Target destReg ARM64Symbolic.X15)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterInc ctx)
        | LIR.StackSlot offset ->
            loadStackSlot ARM64Symbolic.X15 offset
            |> Result.map (fun loadInstrs ->
                loadInstrs
                @ runtimeInstrs (ARM64FileRead.generateFileReadBlob ctx.Target destReg ARM64Symbolic.X15)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterInc ctx)
        | _ -> Error "FileReadBlob requires string operand")

let internal emitFileExists (ctx: CodeGenContext) (dest: LIR.Reg) (path: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        let pathSetup =
            match path with
            | LIR.Reg pathReg ->
                lirRegToARM64Reg pathReg
                |> Result.map (fun source ->
                    if source = ARM64Symbolic.X15 then [] else [ARM64Symbolic.MOV_reg (ARM64Symbolic.X15, source)])
            | LIR.StringSymbol value -> Ok (loadStringLiteralPointer ARM64Symbolic.X15 value)
            | LIR.StackSlot offset -> loadStackSlot ARM64Symbolic.X15 offset
            | _ -> Error "FileExists requires string operand"
        pathSetup
        |> Result.map (fun setup ->
            let prefix = $"__file_exists_{ctx.FunctionName}_{ctx.InstructionSite}"
            let copyLoop = $"{prefix}_copy"
            let copyDone = $"{prefix}_copy_done"
            let failure = $"{prefix}_failure"
            let complete = $"{prefix}_complete"
            let syscalls = ARM64.targetSyscalls ctx.Target
            let call, failureCheck =
                match ARM64.targetOS ctx.Target with
                | Platform.Linux ->
                    (loadImmediate ARM64Symbolic.X0 -100L
                     @ [ ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP)
                         ARM64Symbolic.MOVZ (ARM64Symbolic.X2, 0us, 0)
                         ARM64Symbolic.MOVZ (ARM64Symbolic.X3, 0us, 0)
                         ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Access, 0)
                         ARM64Symbolic.SVC syscalls.SvcImmediate ],
                     [ ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 0us)
                       ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, failure) ])
                | Platform.MacOS ->
                    ([ ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.SP)
                       ARM64Symbolic.MOVZ (ARM64Symbolic.X1, 0us, 0)
                       ARM64Symbolic.MOVZ (syscalls.SyscallRegister, syscalls.Numbers.Access, 0)
                       ARM64Symbolic.SVC syscalls.SvcImmediate ],
                     [ARM64Symbolic.B_cond_label (ARM64Symbolic.HS, failure)])
            setup
            @ [ ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
                ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
                ARM64Symbolic.LDR (ARM64Symbolic.X9, ARM64Symbolic.X15, 8s)
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X15, 16us)
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
            @ call
            @ failureCheck
            @ [ ARM64Symbolic.MOVZ (destReg, 1us, 0)
                ARM64Symbolic.B_label complete
                ARM64Symbolic.Label failure
                ARM64Symbolic.MOVZ (destReg, 0us, 0)
                ARM64Symbolic.Label complete
                ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
                ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us) ]))

let internal emitFileWriteBlob (ctx: CodeGenContext) (dest: LIR.Reg) (path: LIR.Operand) (content: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
    // File write: writes content string to file at path
    // Returns Result<Unit, String>
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        // Helper to get operand into a register
        let getOperandReg operand tempReg =
            match operand with
            | LIR.Reg reg ->
                lirRegToARM64Reg reg |> Result.map (fun r -> ([], r))
            | LIR.StringSymbol value ->
                Ok (loadStringLiteralPointer tempReg value, tempReg)
            | LIR.StackSlot offset ->
                loadStackSlot tempReg offset |> Result.map (fun instrs -> (instrs, tempReg))
            | _ -> Error "FileWriteBlob requires string operands"

        getOperandReg path ARM64Symbolic.X15
        |> Result.bind (fun (pathInstrs, pathReg) ->
            getOperandReg content ARM64Symbolic.X14
            |> Result.map (fun (contentInstrs, contentReg) ->
                pathInstrs
                @ contentInstrs
                @ runtimeInstrs (ARM64FileWrite.generateFileWriteBlob ctx.Target destReg pathReg contentReg false)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterIncIfResultError ctx destReg)))

let internal emitFileAppendText (ctx: CodeGenContext) (dest: LIR.Reg) (path: LIR.Operand) (content: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
    // File append: appends content string to file at path
    // Returns Result<Unit, String>
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        // Same helper as FileWriteBlob
        let getOperandReg operand tempReg =
            match operand with
            | LIR.Reg reg ->
                lirRegToARM64Reg reg |> Result.map (fun r -> ([], r))
            | LIR.StringSymbol value ->
                Ok (loadStringLiteralPointer tempReg value, tempReg)
            | LIR.StackSlot offset ->
                loadStackSlot tempReg offset |> Result.map (fun instrs -> (instrs, tempReg))
            | _ -> Error "FileAppendText requires string operands"

        getOperandReg path ARM64Symbolic.X15
        |> Result.bind (fun (pathInstrs, pathReg) ->
            getOperandReg content ARM64Symbolic.X14
            |> Result.map (fun (contentInstrs, contentReg) ->
                pathInstrs
                @ contentInstrs
                @ runtimeInstrs (ARM64FileWrite.generateFileWriteBlob ctx.Target destReg pathReg contentReg true)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterIncIfResultError ctx destReg)))

let internal emitFileDelete (ctx: CodeGenContext) (dest: LIR.Reg) (path: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
    // File delete: deletes file at path
    // Uses unlink syscall to remove file
    // Returns Result<Unit, String>
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        match path with
        | LIR.Reg pathReg ->
            // Already a heap string pointer
            lirRegToARM64Reg pathReg
            |> Result.map (fun pathARM64 ->
                runtimeInstrs (ARM64FileMetadata.generateFileDelete ctx.Target destReg pathARM64)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterIncIfResultError ctx destReg)
        | LIR.StringSymbol value ->
            Ok (
                loadStringLiteralPointer ARM64Symbolic.X15 value
                @ runtimeInstrs (ARM64FileMetadata.generateFileDelete ctx.Target destReg ARM64Symbolic.X15)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterIncIfResultError ctx destReg)
        | LIR.StackSlot offset ->
            // Load heap string from stack slot
            loadStackSlot ARM64Symbolic.X15 offset
            |> Result.map (fun loadInstrs ->
                loadInstrs
                @ runtimeInstrs (ARM64FileMetadata.generateFileDelete ctx.Target destReg ARM64Symbolic.X15)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterIncIfResultError ctx destReg)
        | _ -> Error "FileDelete requires string operand")

let internal emitFileCreateDirectory (ctx: CodeGenContext) (dest: LIR.Reg) (path: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        let pathSetup =
            match path with
            | LIR.Reg pathReg ->
                lirRegToARM64Reg pathReg
                |> Result.map (fun source ->
                    if source = ARM64Symbolic.X15 then [] else [ARM64Symbolic.MOV_reg (ARM64Symbolic.X15, source)])
            | LIR.StringSymbol value -> Ok (loadStringLiteralPointer ARM64Symbolic.X15 value)
            | LIR.StackSlot offset -> loadStackSlot ARM64Symbolic.X15 offset
            | _ -> Error "FileCreateDirectory requires string operand"
        pathSetup
        |> Result.map (fun setup ->
            let prefix = $"__mkdir_{ctx.FunctionName}_{ctx.InstructionSite}"
            let copyLoop = $"{prefix}_copy"
            let copyDone = $"{prefix}_copy_done"
            let failure = $"{prefix}_failure"
            let box = $"{prefix}_box"
            let syscalls = ARM64.targetSyscalls ctx.Target
            let call, failureCheck =
                match ARM64.targetOS ctx.Target with
                | Platform.Linux ->
                    (loadImmediate ARM64Symbolic.X0 -100L
                     @ [ARM64Symbolic.MOV_reg (ARM64Symbolic.X1, ARM64Symbolic.SP)]
                     @ loadImmediate ARM64Symbolic.X2 0o777L
                     @ [ ARM64Symbolic.MOVZ (syscalls.SyscallRegister, 34us, 0)
                         ARM64Symbolic.SVC syscalls.SvcImmediate ],
                     [ ARM64Symbolic.CMP_imm (ARM64Symbolic.X0, 0us)
                       ARM64Symbolic.B_cond_label (ARM64Symbolic.LT, failure) ])
                | Platform.MacOS ->
                    ([ARM64Symbolic.MOV_reg (ARM64Symbolic.X0, ARM64Symbolic.SP)]
                     @ loadImmediate ARM64Symbolic.X1 0o777L
                     @ [ ARM64Symbolic.MOVZ (syscalls.SyscallRegister, 136us, 0)
                         ARM64Symbolic.SVC syscalls.SvcImmediate ],
                     [ARM64Symbolic.B_cond_label (ARM64Symbolic.HS, failure)])
            setup
            @ [ ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
                ARM64Symbolic.SUB_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
                ARM64Symbolic.LDR (ARM64Symbolic.X9, ARM64Symbolic.X15, 8s)
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X10, ARM64Symbolic.X15, 16us)
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
            @ call
            @ failureCheck
            @ [ ARM64Symbolic.MOVZ (ARM64Symbolic.X9, 0us, 0)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X10, 0us, 0)
                ARM64Symbolic.B_label box
                ARM64Symbolic.Label failure
                ARM64Symbolic.MOVZ (ARM64Symbolic.X9, 1us, 0) ]
            @ loadStringLiteralPointer ARM64Symbolic.X10 "Error"
            @ [ ARM64Symbolic.Label box
                ARM64Symbolic.MOV_reg (destReg, ARM64Symbolic.X28)
                ARM64Symbolic.ADD_imm (ARM64Symbolic.X28, ARM64Symbolic.X28, 24us)
                ARM64Symbolic.STR (ARM64Symbolic.X9, destReg, 0s)
                ARM64Symbolic.STR (ARM64Symbolic.X10, destReg, 8s)
                ARM64Symbolic.MOVZ (ARM64Symbolic.X9, 1us, 0)
                ARM64Symbolic.STR (ARM64Symbolic.X9, destReg, 16s)
                ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us)
                ARM64Symbolic.ADD_imm (ARM64Symbolic.SP, ARM64Symbolic.SP, 2048us) ]
            @ generateLeakCounterInc ctx))

let internal emitFileSetExecutable (ctx: CodeGenContext) (dest: LIR.Reg) (path: LIR.Operand) : Result<ARM64Symbolic.Instr list, string> =
    // File set executable: sets executable bit on file at path
    // Uses chmod syscall with executable permission
    // Returns Result<Unit, String>
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        match path with
        | LIR.Reg pathReg ->
            // Already a heap string pointer
            lirRegToARM64Reg pathReg
            |> Result.map (fun pathARM64 ->
                runtimeInstrs (ARM64FileMetadata.generateFileSetExecutable ctx.Target destReg pathARM64)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterIncIfResultError ctx destReg)
        | LIR.StringSymbol value ->
            Ok (
                loadStringLiteralPointer ARM64Symbolic.X15 value
                @ runtimeInstrs (ARM64FileMetadata.generateFileSetExecutable ctx.Target destReg ARM64Symbolic.X15)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterIncIfResultError ctx destReg)
        | LIR.StackSlot offset ->
            loadStackSlot ARM64Symbolic.X15 offset
            |> Result.map (fun loadInstrs ->
                loadInstrs
                @ runtimeInstrs (ARM64FileMetadata.generateFileSetExecutable ctx.Target destReg ARM64Symbolic.X15)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterIncIfResultError ctx destReg)
        | _ -> Error "FileSetExecutable requires string operand")

let internal emitFileWriteFromPtr (ctx: CodeGenContext) (dest: LIR.Reg) (path: LIR.Operand) (ptr: LIR.Reg) (length: LIR.Reg) : Result<ARM64Symbolic.Instr list, string> =
    // Write raw bytes from ptr to file at path
    // Returns 1 on success, 0 on failure
    lirRegToARM64Reg dest
    |> Result.bind (fun destReg ->
        lirRegToARM64Reg ptr
        |> Result.bind (fun ptrARM64 ->
            lirRegToARM64Reg length
            |> Result.bind (fun lengthARM64 ->
                match path with
                | LIR.Reg pathReg ->
                    // Already a heap string pointer
                    lirRegToARM64Reg pathReg
                    |> Result.map (fun pathARM64 ->
                        runtimeInstrs (ARM64WriteFromPointer.generateFileWriteFromPtr ctx.Target destReg pathARM64 ptrARM64 lengthARM64)
                        @ generateLeakCounterIncIfResultError ctx destReg)
                | LIR.StringSymbol value ->
                    Ok (
                        loadStringLiteralPointer ARM64Symbolic.X15 value
                        @ runtimeInstrs (ARM64WriteFromPointer.generateFileWriteFromPtr ctx.Target destReg ARM64Symbolic.X15 ptrARM64 lengthARM64)
                        @ generateLeakCounterIncIfResultError ctx destReg)
                | LIR.StackSlot offset ->
                    loadStackSlot ARM64Symbolic.X15 offset
                    |> Result.map (fun loadInstrs ->
                        loadInstrs
                        @ runtimeInstrs (ARM64WriteFromPointer.generateFileWriteFromPtr ctx.Target destReg ARM64Symbolic.X15 ptrARM64 lengthARM64)
                        @ generateLeakCounterIncIfResultError ctx destReg)
                | _ -> Error "FileWriteFromPtr requires string path operand")))
