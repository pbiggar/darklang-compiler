// Printing.fs - Emit x64 instructions for printing operations.

module X64EmitPrinting

open X64Operands
open X64Printing
open X64CodeGenTypes
open X64FieldReferenceCounts

let internal emitPrintChars (ctx: FuncCtx) (bytes: byte list) : Result<X86_64.Instr list, string> =
    Ok (genPrintChars bytes)

let internal emitPrintInt64 (ctx: FuncCtx) (reg: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg reg
    |> Result.map (fun srcReg -> genPrintInt64 srcReg true)

let internal emitPrintUInt64 (ctx: FuncCtx) (reg: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg reg
    |> Result.map (fun srcReg -> genPrintUInt64 srcReg true)

let internal emitPrintInt64NoNewline (ctx: FuncCtx) (reg: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg reg
    |> Result.map (fun srcReg -> genPrintInt64 srcReg false)

let internal emitPrintUInt64NoNewline (ctx: FuncCtx) (reg: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg reg
    |> Result.map (fun srcReg -> genPrintUInt64 srcReg false)

let internal emitPrintBool (ctx: FuncCtx) (reg: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg reg
    |> Result.map (fun srcReg ->
        // Print "true\n" or "false\n" without exiting (Ret handles exit)
        let trueLabel = freshLabel "bool_true"
        let writeLabel = freshLabel "bool_write"
        [X86_64.TEST_reg (srcReg, srcReg)
         X86_64.Jcc (X86_64.NE, trueLabel)
         X86_64.SUB_imm (X86_64.RSP, 8)]
        @ loadImm64 scratch 0x0A65736C6166L  // "false\n"
        @ [X86_64.MOV_store (X86_64.RSP, 0, scratch)
           X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)
           X86_64.MOV_imm32 (X86_64.RDX, 6)
           X86_64.JMP writeLabel
           X86_64.Label trueLabel
           X86_64.SUB_imm (X86_64.RSP, 8)]
        @ loadImm64 scratch 0x0A65757274L  // "true\n"
        @ [X86_64.MOV_store (X86_64.RSP, 0, scratch)
           X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)
           X86_64.MOV_imm32 (X86_64.RDX, 5)
           X86_64.Label writeLabel
           X86_64.MOV_imm32 (X86_64.RDI, 1)]
        @ genWriteSyscall
        @ [X86_64.ADD_imm (X86_64.RSP, 8)])

let internal emitPrintBoolNoNewline (ctx: FuncCtx) (reg: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg reg
    |> Result.map (fun srcReg ->
        // TODO: implement bool printing without exit
        [])

let internal emitPrintHeapString (ctx: FuncCtx) (reg: LIR.Reg) : Result<X86_64.Instr list, string> =
    // Dynamic string format: [refcount:8][length:8][data:N]
    // Print data + newline (exit handled by subsequent Ret → epilogue)
    resolveReg reg
    |> Result.map (fun srcReg ->
        [X86_64.PUSH srcReg]
        @ (if srcReg = X86_64.RDX then
             [ X86_64.MOV_reg (X86_64.R10, srcReg)
               X86_64.MOV_load (X86_64.RDX, X86_64.R10, 8)
               X86_64.LEA (X86_64.RSI, X86_64.R10, 16) ]
         else
             [ X86_64.MOV_load (X86_64.RDX, srcReg, 8)
               X86_64.LEA (X86_64.RSI, srcReg, 16) ])
        @ [X86_64.MOV_imm32 (X86_64.RDI, 1)]
        @ genWriteSyscall
        @ [X86_64.SUB_imm (X86_64.RSP, 8)]
        @ loadImm64 scratch 10L
        @ [X86_64.MOV_store (X86_64.RSP, 0, scratch)
           X86_64.MOV_imm32 (X86_64.RDI, 1)
           X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)
           X86_64.MOV_imm32 (X86_64.RDX, 1)]
        @ genWriteSyscall
        @ [X86_64.ADD_imm (X86_64.RSP, 8)
           X86_64.POP srcReg])

let internal emitPrintHeapStringNoNewline (ctx: FuncCtx) (reg: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg reg
    |> Result.map (fun srcReg ->
        [X86_64.PUSH srcReg]
        @ (if srcReg = X86_64.RDX then
             [ X86_64.MOV_reg (X86_64.R10, srcReg)
               X86_64.MOV_load (X86_64.RDX, X86_64.R10, 8)
               X86_64.LEA (X86_64.RSI, X86_64.R10, 16) ]
           else
             [ X86_64.MOV_load (X86_64.RDX, srcReg, 8)
               X86_64.LEA (X86_64.RSI, srcReg, 16) ])
        @ [X86_64.MOV_imm32 (X86_64.RDI, 1)]
        @ genWriteSyscall
        @ [X86_64.POP srcReg])

let internal emitPrintString (ctx: FuncCtx) (str: string) : Result<X86_64.Instr list, string> =
    // Write a literal string to stdout and exit(0)
    let bytes = System.Text.Encoding.UTF8.GetBytes(str + "\n")
    let len = bytes.Length
    let padded = ((len + 7) / 8) * 8
    let paddedBytes = (bytes |> Array.toList) @ List.replicate (padded - len) 0uy
    let pushInstrs =
        paddedBytes
        |> List.chunkBySize 8
        |> List.rev
        |> List.collect (fun chunk ->
            let value = chunk |> List.mapi (fun i b -> int64 b <<< (i * 8)) |> List.fold (|||) 0L
            loadImm64 scratch value @ [X86_64.PUSH scratch])
    Ok (
        pushInstrs
        @ [X86_64.MOV_imm32 (X86_64.RDI, 1)]  // fd = stdout
        @ [X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)]
        @ loadImm64 X86_64.RDX (int64 len)
        @ genWriteSyscall
        @ [X86_64.ADD_imm (X86_64.RSP, int32 padded)]
        @ loadImm64 X86_64.RDI 0L
        @ genExitSyscall
    )

let internal emitPrintFloat (ctx: FuncCtx) (freg: LIR.FReg) : Result<X86_64.Instr list, string> =
    // Call Stdlib.Float.toString(D0), print result as heap string
    match freg with
    | LIR.FPhysical fp ->
        let xmm = lirFRegToX86 fp
        Ok ((if xmm <> X86_64.XMM0 then [X86_64.MOVSD_reg (X86_64.XMM0, xmm)] else [])
            @ [X86_64.CALL "Darklang.Stdlib.Float.toString"]
            @ [X86_64.MOV_load (X86_64.RDX, X86_64.RAX, 8)
               X86_64.LEA (X86_64.RSI, X86_64.RAX, 16)
               X86_64.MOV_imm32 (X86_64.RDI, 1)]
            @ genWriteSyscall
            @ [X86_64.SUB_imm (X86_64.RSP, 8)]
            @ loadImm64 scratch 10L
            @ [X86_64.MOV_store (X86_64.RSP, 0, scratch)
               X86_64.MOV_imm32 (X86_64.RDI, 1)
               X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)
               X86_64.MOV_imm32 (X86_64.RDX, 1)]
            @ genWriteSyscall
            @ [X86_64.ADD_imm (X86_64.RSP, 8)])
    | _ -> Error "PrintFloat with virtual FP register"

let internal emitPrintFloatNoNewline (ctx: FuncCtx) (freg: LIR.FReg) : Result<X86_64.Instr list, string> =
    // Call Stdlib.Float.toString(D0), print result without newline
    match freg with
    | LIR.FPhysical fp ->
        let xmm = lirFRegToX86 fp
        Ok ((if xmm <> X86_64.XMM0 then [X86_64.MOVSD_reg (X86_64.XMM0, xmm)] else [])
            @ [X86_64.CALL "Darklang.Stdlib.Float.toString"]
            @ [X86_64.MOV_load (X86_64.RDX, X86_64.RAX, 8)
               X86_64.LEA (X86_64.RSI, X86_64.RAX, 16)
               X86_64.MOV_imm32 (X86_64.RDI, 1)]
            @ genWriteSyscall)
    | _ -> Error "PrintFloatNoNewline with virtual FP register"

let internal emitPrintList (ctx: FuncCtx) (listPtr: LIR.Reg) (_elemType: AST.Type) : Result<X86_64.Instr list, string> =
    // TODO: implement list printing
    resolveReg listPtr
    |> Result.map (fun _ -> loadImm64 X86_64.RDI 0L @ genExitSyscall)

let internal emitPrintSum (ctx: FuncCtx) (sumPtr: LIR.Reg) (_variants: (string * int * AST.Type option) list) : Result<X86_64.Instr list, string> =
    resolveReg sumPtr
    |> Result.map (fun _ -> loadImm64 X86_64.RDI 0L @ genExitSyscall)

let internal emitPrintRecord (ctx: FuncCtx) (recordPtr: LIR.Reg) (_typeName: string) (_fields: (string * AST.Type) list) : Result<X86_64.Instr list, string> =
    resolveReg recordPtr
    |> Result.map (fun _ -> loadImm64 X86_64.RDI 0L @ genExitSyscall)

let internal emitPrintBlob (ctx: FuncCtx) (reg: LIR.Reg) : Result<X86_64.Instr list, string> =
    resolveReg reg
    |> Result.map (fun _ -> loadImm64 X86_64.RDI 0L @ genExitSyscall)
