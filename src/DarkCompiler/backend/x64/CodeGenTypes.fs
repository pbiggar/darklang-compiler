// CodeGenTypes.fs - Define target code generation options, contexts, and planning facts.

module X64CodeGenTypes

open X64Operands

/// Function context for instructions that need stack frame info (TailCall, etc.)
type internal FuncCtx = {
    FunctionName: string
    StackSize: int
    UsedCalleeSaved: LIR.PhysReg list
    EnableLeakCheck: bool
    RecordRegistry: LIR.RecordRegistry
    SumShapeRegistry: MemoryModel.RcSumShapeRegistry
    FunctionNames: Map<AST.FunctionId, string>
}

let internal functionName (ctx: FuncCtx) (functionId: AST.FunctionId) : string =
    match Map.tryFind functionId ctx.FunctionNames with
    | Some name -> name
    | None -> Crash.crash $"x64 code generation: missing function name for identity {AST.functionIdValue functionId}"

let internal rcSumShapeRegistryFromVariantRegistry (variantRegistry: LIR.VariantRegistry) : MemoryModel.RcSumShapeRegistry =
    variantRegistry
    |> Map.map (fun _typeName typeVariants ->
        { MemoryModel.TypeParams = typeVariants.TypeParams
          MemoryModel.Payloads =
            typeVariants.Variants
            |> List.sortBy (fun variant -> variant.Tag)
            |> List.map (fun variant -> variant.Tag, variant.Payload) })

// ============================================================================
// Leak Counter (data label _leak_count in ELF data section)
// ============================================================================

/// Increment the leak counter (called on every heap allocation)
let internal genLeakCounterInc (ctx: FuncCtx) : X86_64.Instr list =
    if not ctx.EnableLeakCheck then []
    else
        // LEA R11, [RIP + _leak_count]; INC [R11]
        [X86_64.PUSH scratch
         X86_64.PUSH X86_64.RCX
         X86_64.LEA_rip (scratch, "_leak_count")
         X86_64.MOV_load (X86_64.RCX, scratch, 0)
         X86_64.ADD_imm (X86_64.RCX, 1)
         X86_64.MOV_store (scratch, 0, X86_64.RCX)
         X86_64.POP X86_64.RCX
         X86_64.POP scratch]

/// Decrement the leak counter (called when refcount hits zero and block is freed)
let internal genLeakCounterDec (ctx: FuncCtx) : X86_64.Instr list =
    if not ctx.EnableLeakCheck then []
    else
        [X86_64.PUSH scratch
         X86_64.PUSH X86_64.RCX
         X86_64.LEA_rip (scratch, "_leak_count")
         X86_64.MOV_load (X86_64.RCX, scratch, 0)
         X86_64.SUB_imm (X86_64.RCX, 1)
         X86_64.MOV_store (scratch, 0, X86_64.RCX)
         X86_64.POP X86_64.RCX
         X86_64.POP scratch]

/// Reverse bytes at [RSP..RSP+RCX-1] in place
let private genReverseBytes () : X86_64.Instr list =
    let loopLabel = freshLabel "rev_loop"
    let doneLabel = freshLabel "rev_done"
    [X86_64.MOV_reg (X86_64.RDI, X86_64.RSP)
     X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)
     X86_64.ADD_reg (X86_64.RSI, X86_64.RCX)
     X86_64.SUB_imm (X86_64.RSI, 1)
     X86_64.Label loopLabel
     X86_64.CMP_reg (X86_64.RDI, X86_64.RSI)
     X86_64.Jcc (X86_64.GE, doneLabel)
     X86_64.MOV_load_byte (X86_64.RAX, X86_64.RDI, 0)
     X86_64.MOV_load_byte (X86_64.RDX, X86_64.RSI, 0)
     X86_64.MOV_store_byte (X86_64.RDI, 0, X86_64.RDX)
     X86_64.MOV_store_byte (X86_64.RSI, 0, X86_64.RAX)
     X86_64.ADD_imm (X86_64.RDI, 1)
     X86_64.SUB_imm (X86_64.RSI, 1)
     X86_64.JMP loopLabel
     X86_64.Label doneLabel]

/// Print an int64 value to stderr followed by newline
let private genPrintInt64ToStderr (srcReg: X86_64.Reg) : X86_64.Instr list =
    let loopLabel = freshLabel "print_i64_stderr_loop"
    let writeLabel = freshLabel "print_i64_stderr_write"
    [X86_64.SUB_imm (X86_64.RSP, 24)
     X86_64.MOV_imm32 (X86_64.RCX, 0)
     X86_64.TEST_reg (srcReg, srcReg)
     X86_64.Jcc (X86_64.NE, loopLabel)
     X86_64.MOV_imm32 (scratch, 48)
     X86_64.MOV_store_byte (X86_64.RSP, 0, scratch)
     X86_64.MOV_imm32 (X86_64.RCX, 1)
     X86_64.JMP writeLabel
     X86_64.Label loopLabel
     X86_64.TEST_reg (srcReg, srcReg)
     X86_64.Jcc (X86_64.EQ, writeLabel)]
    @ loadImm64 scratch 10L
    @ [X86_64.PUSH X86_64.RDX
       X86_64.MOV_reg (X86_64.RAX, srcReg)
       X86_64.XOR_reg (X86_64.RDX, X86_64.RDX)
       X86_64.IDIV scratch
       X86_64.ADD_imm (X86_64.RDX, 48)
       X86_64.MOV_reg (scratch, X86_64.RSP)
       X86_64.ADD_imm (scratch, 8)
       X86_64.ADD_reg (scratch, X86_64.RCX)
       X86_64.MOV_store_byte (scratch, 0, X86_64.RDX)
       X86_64.ADD_imm (X86_64.RCX, 1)
       X86_64.MOV_reg (srcReg, X86_64.RAX)
       X86_64.POP X86_64.RDX
       X86_64.JMP loopLabel
       X86_64.Label writeLabel]
    @ genReverseBytes ()
    @ [X86_64.MOV_imm32 (X86_64.RDI, 2)
       X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)
       X86_64.MOV_reg (scratch, X86_64.RSP)
       X86_64.ADD_reg (scratch, X86_64.RCX)
       X86_64.MOV_imm32 (X86_64.RAX, 10)
       X86_64.MOV_store_byte (scratch, 0, X86_64.RAX)
       X86_64.LEA (X86_64.RDX, X86_64.RCX, 1)]
    @ loadImm64 X86_64.RAX (int64 syscalls.Write)
    @ [X86_64.SYSCALL
       X86_64.ADD_imm (X86_64.RSP, 24)]

/// Generate leak check report at exit: if _leak_count > 0, print "leaks: N\n" to stderr
let internal genLeakCheckReport () : X86_64.Instr list =
    let noLeaksLabel = freshLabel "no_leaks"
    [X86_64.LEA_rip (scratch, "_leak_count")
     X86_64.MOV_load (X86_64.RAX, scratch, 0)
     X86_64.TEST_reg (X86_64.RAX, X86_64.RAX)
     X86_64.Jcc (X86_64.EQ, noLeaksLabel)
     X86_64.PUSH X86_64.RAX
     X86_64.SUB_imm (X86_64.RSP, 8)]
    @ loadImm64 scratch 0x203A736B61656CL   // "leaks: " little-endian
    @ [X86_64.MOV_store (X86_64.RSP, 0, scratch)
       X86_64.MOV_imm32 (X86_64.RDI, 2)
       X86_64.MOV_reg (X86_64.RSI, X86_64.RSP)
       X86_64.MOV_imm32 (X86_64.RDX, 7)]
    @ loadImm64 X86_64.RAX (int64 syscalls.Write)
    @ [X86_64.SYSCALL
       X86_64.ADD_imm (X86_64.RSP, 8)
       X86_64.POP X86_64.RAX]
    @ genPrintInt64ToStderr X86_64.RAX
    @ [X86_64.Label noLeaksLabel]
