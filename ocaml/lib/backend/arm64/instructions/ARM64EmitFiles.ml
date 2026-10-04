(*
   Files.fs - Emit arm64 instructions for files operations.
*)
[@@@warning "-4"]
let bind f value=Result.bind value f



open ARM64CodeGenTypes
open HeapAllocation
open LeakAccounting
open ARM64Operands

(*
   File reading: generates syscall sequence to read file contents
   Returns Result<Blob, String>
   Already a heap string pointer
*)
let emitFileReadBlob (ctx: codeGenContext) (dest: LIR.reg) (path: LIR.operand) =


    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        match path with
        | LIR.Reg pathReg ->

            lirRegToARM64Reg pathReg
            |> Result.map (fun pathARM64 ->
                runtimeInstrs (FileRead.generateFileReadBlob ctx.target destReg pathARM64)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterInc ctx)
        | LIR.StringSymbol value ->
            Ok (
                loadStringLiteralPointer Symbolic.X15 value
                @ runtimeInstrs (FileRead.generateFileReadBlob ctx.target destReg Symbolic.X15)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterInc ctx)
        | LIR.StackSlot offset ->
            loadStackSlot Symbolic.X15 offset
            |> Result.map (fun loadInstrs ->
                loadInstrs
                @ runtimeInstrs (FileRead.generateFileReadBlob ctx.target destReg Symbolic.X15)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterInc ctx)
        | _ -> Error "FileReadBlob requires string operand")

let emitFileExists (ctx: codeGenContext) (dest: LIR.reg) (path: LIR.operand) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        let pathSetup =
            match path with
            | LIR.Reg pathReg ->
                lirRegToARM64Reg pathReg
                |> Result.map (fun source ->
                    if source = Symbolic.X15 then [] else [Symbolic.MOV_reg (Symbolic.X15, source)])
            | LIR.StringSymbol value -> Ok (loadStringLiteralPointer Symbolic.X15 value)
            | LIR.StackSlot offset -> loadStackSlot Symbolic.X15 offset
            | _ -> Error "FileExists requires string operand"
        in
        pathSetup
        |> Result.map (fun setup ->
            let prefix = Printf.sprintf "__file_exists_%s_%s" ctx.functionName ctx.instructionSite
            in
            let copyLoop = (prefix ^ "_copy")
            in
            let copyDone = (prefix ^ "_copy_done")
            in
            let failure = (prefix ^ "_failure")
            in
            let complete = (prefix ^ "_complete")
            in
            let syscalls = ARM64.targetSyscalls ctx.target
            in
            let call, failureCheck =
                match ARM64.targetOS ctx.target with
                | Platform.Linux ->
                    (loadImmediate Symbolic.X0 (-100L)
                     @ [ Symbolic.MOV_reg (Symbolic.X1, Symbolic.SP);
                         Symbolic.MOVZ (Symbolic.X2, 0, 0);
                         Symbolic.MOVZ (Symbolic.X3, 0, 0);
                         Symbolic.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.access, 0);
                         Symbolic.SVC syscalls.ARM64.svcImmediate ],
                     [ Symbolic.CMP_imm (Symbolic.X0, 0);
                       Symbolic.B_cond_label (Symbolic.LT, failure) ])
                | Platform.MacOS ->
                    ([ Symbolic.MOV_reg (Symbolic.X0, Symbolic.SP);
                       Symbolic.MOVZ (Symbolic.X1, 0, 0);
                       Symbolic.MOVZ (syscalls.ARM64.syscallRegister, syscalls.ARM64.numbers.Platform.access, 0);
                       Symbolic.SVC syscalls.ARM64.svcImmediate ],
                     [Symbolic.B_cond_label (Symbolic.HS, failure)])
            in
            setup
            @ [ Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 2048);
                Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 2048);
                Symbolic.LDR (Symbolic.X9, Symbolic.X15, 8);
                Symbolic.ADD_imm (Symbolic.X10, Symbolic.X15, 16);
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
                Symbolic.STRB_reg (Symbolic.X12, Symbolic.X11) ]
            @ call
            @ failureCheck
            @ [ Symbolic.MOVZ (destReg, 1, 0);
                Symbolic.B_label complete;
                Symbolic.Label failure;
                Symbolic.MOVZ (destReg, 0, 0);
                Symbolic.Label complete;
                Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048);
                Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048) ]))

(*
   File write: writes content string to file at path
   Returns Result<Unit, String>
   Helper to get operand into a register
*)
let emitFileWriteBlob (ctx: codeGenContext) (dest: LIR.reg) (path: LIR.operand) (content: LIR.operand) =


    lirRegToARM64Reg dest
    |> bind (fun destReg ->

        let getOperandReg operand tempReg =
            match operand with
            | LIR.Reg reg ->
                lirRegToARM64Reg reg |> Result.map (fun r -> ([], r))
            | LIR.StringSymbol value ->
                Ok (loadStringLiteralPointer tempReg value, tempReg)
            | LIR.StackSlot offset ->
                loadStackSlot tempReg offset |> Result.map (fun instrs -> (instrs, tempReg))
            | _ -> Error "FileWriteBlob requires string operands"

        in
        getOperandReg path Symbolic.X15
        |> bind (fun (pathInstrs, pathReg) ->
            getOperandReg content Symbolic.X14
            |> Result.map (fun (contentInstrs, contentReg) ->
                pathInstrs
                @ contentInstrs
                @ runtimeInstrs (FileWrite.generateFileWriteBlob ctx.target destReg pathReg contentReg false)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterIncIfResultError ctx destReg)))

(*
   File append: appends content string to file at path
   Returns Result<Unit, String>
   Same helper as FileWriteBlob
*)
let emitFileAppendText (ctx: codeGenContext) (dest: LIR.reg) (path: LIR.operand) (content: LIR.operand) =


    lirRegToARM64Reg dest
    |> bind (fun destReg ->

        let getOperandReg operand tempReg =
            match operand with
            | LIR.Reg reg ->
                lirRegToARM64Reg reg |> Result.map (fun r -> ([], r))
            | LIR.StringSymbol value ->
                Ok (loadStringLiteralPointer tempReg value, tempReg)
            | LIR.StackSlot offset ->
                loadStackSlot tempReg offset |> Result.map (fun instrs -> (instrs, tempReg))
            | _ -> Error "FileAppendText requires string operands"

        in
        getOperandReg path Symbolic.X15
        |> bind (fun (pathInstrs, pathReg) ->
            getOperandReg content Symbolic.X14
            |> Result.map (fun (contentInstrs, contentReg) ->
                pathInstrs
                @ contentInstrs
                @ runtimeInstrs (FileWrite.generateFileWriteBlob ctx.target destReg pathReg contentReg true)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterIncIfResultError ctx destReg)))

(*
   File delete: deletes file at path
   Uses unlink syscall to remove file
   Returns Result<Unit, String>
   Already a heap string pointer
   Load heap string from stack slot
*)
let emitFileDelete (ctx: codeGenContext) (dest: LIR.reg) (path: LIR.operand) =



    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        match path with
        | LIR.Reg pathReg ->

            lirRegToARM64Reg pathReg
            |> Result.map (fun pathARM64 ->
                runtimeInstrs (FileMetadata.generateFileDelete ctx.target destReg pathARM64)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterIncIfResultError ctx destReg)
        | LIR.StringSymbol value ->
            Ok (
                loadStringLiteralPointer Symbolic.X15 value
                @ runtimeInstrs (FileMetadata.generateFileDelete ctx.target destReg Symbolic.X15)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterIncIfResultError ctx destReg)
        | LIR.StackSlot offset ->

            loadStackSlot Symbolic.X15 offset
            |> Result.map (fun loadInstrs ->
                loadInstrs
                @ runtimeInstrs (FileMetadata.generateFileDelete ctx.target destReg Symbolic.X15)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterIncIfResultError ctx destReg)
        | _ -> Error "FileDelete requires string operand")

let emitFileCreateDirectory (ctx: codeGenContext) (dest: LIR.reg) (path: LIR.operand) =
    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        let pathSetup =
            match path with
            | LIR.Reg pathReg ->
                lirRegToARM64Reg pathReg
                |> Result.map (fun source ->
                    if source = Symbolic.X15 then [] else [Symbolic.MOV_reg (Symbolic.X15, source)])
            | LIR.StringSymbol value -> Ok (loadStringLiteralPointer Symbolic.X15 value)
            | LIR.StackSlot offset -> loadStackSlot Symbolic.X15 offset
            | _ -> Error "FileCreateDirectory requires string operand"
        in
        pathSetup
        |> Result.map (fun setup ->
            let prefix = Printf.sprintf "__mkdir_%s_%s" ctx.functionName ctx.instructionSite
            in
            let copyLoop = (prefix ^ "_copy")
            in
            let copyDone = (prefix ^ "_copy_done")
            in
            let failure = (prefix ^ "_failure")
            in
            let box = (prefix ^ "_box")
            in
            let syscalls = ARM64.targetSyscalls ctx.target
            in
            let call, failureCheck =
                match ARM64.targetOS ctx.target with
                | Platform.Linux ->
                    (loadImmediate Symbolic.X0 (-100L)
                     @ [Symbolic.MOV_reg (Symbolic.X1, Symbolic.SP)]
                     @ loadImmediate Symbolic.X2 0o777L
                     @ [ Symbolic.MOVZ (syscalls.ARM64.syscallRegister, 34, 0);
                         Symbolic.SVC syscalls.ARM64.svcImmediate ],
                     [ Symbolic.CMP_imm (Symbolic.X0, 0);
                       Symbolic.B_cond_label (Symbolic.LT, failure) ])
                | Platform.MacOS ->
                    ([Symbolic.MOV_reg (Symbolic.X0, Symbolic.SP)]
                     @ loadImmediate Symbolic.X1 0o777L
                     @ [ Symbolic.MOVZ (syscalls.ARM64.syscallRegister, 136, 0);
                         Symbolic.SVC syscalls.ARM64.svcImmediate ],
                     [Symbolic.B_cond_label (Symbolic.HS, failure)])
            in
            setup
            @ [ Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 2048);
                Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 2048);
                Symbolic.LDR (Symbolic.X9, Symbolic.X15, 8);
                Symbolic.ADD_imm (Symbolic.X10, Symbolic.X15, 16);
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
                Symbolic.STRB_reg (Symbolic.X12, Symbolic.X11) ]
            @ call
            @ failureCheck
            @ [ Symbolic.MOVZ (Symbolic.X9, 0, 0);
                Symbolic.MOVZ (Symbolic.X10, 0, 0);
                Symbolic.B_label box;
                Symbolic.Label failure;
                Symbolic.MOVZ (Symbolic.X9, 1, 0) ]
            @ loadStringLiteralPointer Symbolic.X10 "Error"
            @ [ Symbolic.Label box;
                Symbolic.MOV_reg (destReg, Symbolic.X28);
                Symbolic.ADD_imm (Symbolic.X28, Symbolic.X28, 24);
                Symbolic.STR (Symbolic.X9, destReg, 0);
                Symbolic.STR (Symbolic.X10, destReg, 8);
                Symbolic.MOVZ (Symbolic.X9, 1, 0);
                Symbolic.STR (Symbolic.X9, destReg, 16);
                Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048);
                Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 2048) ]
            @ generateLeakCounterInc ctx))

(*
   File set executable: sets executable bit on file at path
   Uses chmod syscall with executable permission
   Returns Result<Unit, String>
   Already a heap string pointer
*)
let emitFileSetExecutable (ctx: codeGenContext) (dest: LIR.reg) (path: LIR.operand) =



    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        match path with
        | LIR.Reg pathReg ->

            lirRegToARM64Reg pathReg
            |> Result.map (fun pathARM64 ->
                runtimeInstrs (FileMetadata.generateFileSetExecutable ctx.target destReg pathARM64)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterIncIfResultError ctx destReg)
        | LIR.StringSymbol value ->
            Ok (
                loadStringLiteralPointer Symbolic.X15 value
                @ runtimeInstrs (FileMetadata.generateFileSetExecutable ctx.target destReg Symbolic.X15)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterIncIfResultError ctx destReg)
        | LIR.StackSlot offset ->
            loadStackSlot Symbolic.X15 offset
            |> Result.map (fun loadInstrs ->
                loadInstrs
                @ runtimeInstrs (FileMetadata.generateFileSetExecutable ctx.target destReg Symbolic.X15)
                @ generateLeakCounterInc ctx
                @ generateLeakCounterIncIfResultError ctx destReg)
        | _ -> Error "FileSetExecutable requires string operand")

(*
   Write raw bytes from ptr to file at path
   Returns 1 on success, 0 on failure
   Already a heap string pointer
*)
let emitFileWriteFromPtr (ctx: codeGenContext) (dest: LIR.reg) (path: LIR.operand) (ptr: LIR.reg) (length: LIR.reg) =


    lirRegToARM64Reg dest
    |> bind (fun destReg ->
        lirRegToARM64Reg ptr
        |> bind (fun ptrARM64 ->
            lirRegToARM64Reg length
            |> bind (fun lengthARM64 ->
                match path with
                | LIR.Reg pathReg ->

                    lirRegToARM64Reg pathReg
                    |> Result.map (fun pathARM64 ->
                        runtimeInstrs (WriteFromPointer.generateFileWriteFromPtr ctx.target destReg pathARM64 ptrARM64 lengthARM64)
                        @ generateLeakCounterIncIfResultError ctx destReg)
                | LIR.StringSymbol value ->
                    Ok (
                        loadStringLiteralPointer Symbolic.X15 value
                        @ runtimeInstrs (WriteFromPointer.generateFileWriteFromPtr ctx.target destReg Symbolic.X15 ptrARM64 lengthARM64)
                        @ generateLeakCounterIncIfResultError ctx destReg)
                | LIR.StackSlot offset ->
                    loadStackSlot Symbolic.X15 offset
                    |> Result.map (fun loadInstrs ->
                        loadInstrs
                        @ runtimeInstrs (WriteFromPointer.generateFileWriteFromPtr ctx.target destReg Symbolic.X15 ptrARM64 lengthARM64)
                        @ generateLeakCounterIncIfResultError ctx destReg)
                | _ -> Error "FileWriteFromPtr requires string path operand")))
