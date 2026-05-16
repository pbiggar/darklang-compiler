// X86_64CodeGenTests.fs - Tests for x86-64 code generation from LIR
//
// Verifies that LIR programs translate to working x86-64 executables.

module X86_64CodeGenTests

/// Build and run a LIR program, returning exit code and stdout
let private runLIRProgramFull (program: LIR.Program) : Result<int * string, string> =
    match CodeGen_X86_64.translateProgram program false with
    | Error e -> Error $"Codegen error: {e}"
    | Ok instrs ->
        match X86_64_Resolve.resolveAndEncode instrs with
        | Error e -> Error $"Resolve error: {e}"
        | Ok resolveResult ->
            let binary =
                Binary_Generation_ELF_X86_64.createExecutableWithPools
                    resolveResult.MachineCode LiteralPool.emptyStringPool LiteralPool.emptyFloatPool false 0
            let tempPath = System.IO.Path.Combine(System.IO.Path.GetTempPath(), System.Guid.NewGuid().ToString("N"))
            try
                do
                    use stream = new System.IO.FileStream(tempPath, System.IO.FileMode.Create, System.IO.FileAccess.Write, System.IO.FileShare.None)
                    stream.Write(binary, 0, binary.Length)
                    stream.Flush(true)
                let permissions = System.IO.File.GetUnixFileMode(tempPath)
                System.IO.File.SetUnixFileMode(tempPath, permissions ||| System.IO.UnixFileMode.UserExecute)
                let psi =
                    match Platform.detectArch () with
                    | Ok Platform.X86_64 -> System.Diagnostics.ProcessStartInfo(tempPath)
                    | _ -> System.Diagnostics.ProcessStartInfo("qemu-x86_64-static", tempPath)
                psi.UseShellExecute <- false
                psi.RedirectStandardOutput <- true
                psi.RedirectStandardError <- true
                use proc = System.Diagnostics.Process.Start(psi)
                let stdout = proc.StandardOutput.ReadToEnd()
                proc.WaitForExit(10000) |> ignore
                Ok (proc.ExitCode, stdout)
            with ex -> Error $"Execution failed: {ex.Message}"
            |> fun result ->
                try System.IO.File.Delete(tempPath) with _ -> ()
                result

/// Build and run a LIR program, returning the exit code
let private runLIRProgram (program: LIR.Program) : Result<int, string> =
    match CodeGen_X86_64.translateProgram program false with
    | Error e -> Error $"Codegen error: {e}"
    | Ok instrs ->
        match X86_64_Resolve.resolveAndEncode instrs with
        | Error e -> Error $"Resolve error: {e}"
        | Ok resolveResult ->
            let binary =
                Binary_Generation_ELF_X86_64.createExecutableWithPools
                    resolveResult.MachineCode LiteralPool.emptyStringPool LiteralPool.emptyFloatPool false 0
            X86_64BinaryTests.runElfBinary binary

/// Create a minimal LIR function with a single basic block
let private makeSimpleProgram (instrs: LIR.Instr list) (term: LIR.Terminator) : LIR.Program =
    let entryLabel = LIR.Label "_start_entry"
    let bodyLabel = LIR.Label "_start_body"
    let entryBlock : LIR.BasicBlock = {
        Label = entryLabel
        Instrs = []
        Terminator = LIR.Jump bodyLabel
    }
    let bodyBlock : LIR.BasicBlock = {
        Label = bodyLabel
        Instrs = instrs
        Terminator = term
    }
    let func : LIR.Function = {
        Name = "_start"
        TypedParams = []
        CFG = {
            Entry = entryLabel
            Blocks = Map.ofList [(entryLabel, entryBlock); (bodyLabel, bodyBlock)]
        }
        StackSize = 0
        UsedCalleeSaved = []
    }
    LIR.Program [func]

/// Test: MOV immediate + exit
let testMovAndExit () : Result<unit, string> =
    // exit(42): X0 <- 42; X1 <- X0; Exit
    // On x86_64: X0→RAX, X1→RDI, Exit = mov rax,60; syscall
    // But Exit expects exit code in RDI already, so: X1 <- 42; Exit
    let program = makeSimpleProgram
                    [LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 42L)]
                    LIR.Ret  // We'll use Ret but need exit syscall

    // Actually, let's just set RDI to 42 and call exit directly
    // The LIR.Exit instruction generates the syscall
    let program = makeSimpleProgram
                    [LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 42L)
                     LIR.Exit]
                    LIR.Ret

    match runLIRProgram program with
    | Error e -> Error e
    | Ok exitCode ->
        if exitCode = 42 then Ok ()
        else Error $"Expected exit code 42, got {exitCode}"

/// Test: ADD immediate
let testAddImm () : Result<unit, string> =
    let program = makeSimpleProgram
                    [LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 40L)
                     LIR.Add (LIR.Physical LIR.X1, LIR.Physical LIR.X1, LIR.Imm 2L)
                     LIR.Exit]
                    LIR.Ret
    match runLIRProgram program with
    | Error e -> Error e
    | Ok exitCode ->
        if exitCode = 42 then Ok ()
        else Error $"Expected exit code 42, got {exitCode}"

/// Test: SUB
let testSub () : Result<unit, string> =
    let program = makeSimpleProgram
                    [LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 50L)
                     LIR.Sub (LIR.Physical LIR.X1, LIR.Physical LIR.X1, LIR.Imm 8L)
                     LIR.Exit]
                    LIR.Ret
    match runLIRProgram program with
    | Error e -> Error e
    | Ok exitCode ->
        if exitCode = 42 then Ok ()
        else Error $"Expected exit code 42, got {exitCode}"

/// Test: MUL
let testMul () : Result<unit, string> =
    let program = makeSimpleProgram
                    [LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 6L)
                     LIR.Mov (LIR.Physical LIR.X2, LIR.Imm 7L)
                     LIR.Mul (LIR.Physical LIR.X1, LIR.Physical LIR.X1, LIR.Physical LIR.X2)
                     LIR.Exit]
                    LIR.Ret
    match runLIRProgram program with
    | Error e -> Error e
    | Ok exitCode ->
        if exitCode = 42 then Ok ()
        else Error $"Expected exit code 42, got {exitCode}"

/// Test: conditional branch
let testBranch () : Result<unit, string> =
    let entryLabel = LIR.Label "_start_entry"
    let testLabel = LIR.Label "_start_test"
    let trueLabel = LIR.Label "_start_true"
    let falseLabel = LIR.Label "_start_false"

    let entryBlock : LIR.BasicBlock = {
        Label = entryLabel
        Instrs = []
        Terminator = LIR.Jump testLabel
    }
    let testBlock : LIR.BasicBlock = {
        Label = testLabel
        Instrs = [
            LIR.Mov (LIR.Physical LIR.X2, LIR.Imm 10L)
            LIR.Cmp (LIR.Physical LIR.X2, LIR.Imm 5L)
        ]
        Terminator = LIR.CondBranch (LIR.GT, trueLabel, falseLabel)
    }
    let trueBlock : LIR.BasicBlock = {
        Label = trueLabel
        Instrs = [
            LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 42L)
            LIR.Exit
        ]
        Terminator = LIR.Ret
    }
    let falseBlock : LIR.BasicBlock = {
        Label = falseLabel
        Instrs = [
            LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 0L)
            LIR.Exit
        ]
        Terminator = LIR.Ret
    }
    let func : LIR.Function = {
        Name = "_start"
        TypedParams = []
        CFG = {
            Entry = entryLabel
            Blocks = Map.ofList [
                (entryLabel, entryBlock)
                (testLabel, testBlock)
                (trueLabel, trueBlock)
                (falseLabel, falseBlock)
            ]
        }
        StackSize = 0
        UsedCalleeSaved = []
    }
    let program = LIR.Program [func]
    match runLIRProgram program with
    | Error e -> Error e
    | Ok exitCode ->
        if exitCode = 42 then Ok ()
        else Error $"Expected exit code 42, got {exitCode}"

/// Test: PrintInt64 outputs correct string
let testPrintInt64 () : Result<unit, string> =
    let program = makeSimpleProgram
                    [LIR.Mov (LIR.Physical LIR.X0, LIR.Imm 42L)
                     LIR.PrintInt64 (LIR.Physical LIR.X0)]
                    LIR.Ret
    match runLIRProgramFull program with
    | Error e -> Error e
    | Ok (exitCode, stdout) ->
        if exitCode <> 0 then Error $"Expected exit code 0, got {exitCode}"
        elif stdout.Trim() <> "42" then Error $"Expected stdout '42', got '{stdout.Trim()}'"
        else Ok ()

/// Test: PrintInt64 with negative number
let testPrintInt64Negative () : Result<unit, string> =
    let program = makeSimpleProgram
                    [LIR.Mov (LIR.Physical LIR.X0, LIR.Imm -123L)
                     LIR.PrintInt64 (LIR.Physical LIR.X0)]
                    LIR.Ret
    match runLIRProgramFull program with
    | Error e -> Error e
    | Ok (exitCode, stdout) ->
        if exitCode <> 0 then Error $"Expected exit code 0, got {exitCode}"
        elif stdout.Trim() <> "-123" then Error $"Expected stdout '-123', got '{stdout.Trim()}'"
        else Ok ()

/// Test: PrintInt64 with zero
let testPrintInt64Zero () : Result<unit, string> =
    let program = makeSimpleProgram
                    [LIR.Mov (LIR.Physical LIR.X0, LIR.Imm 0L)
                     LIR.PrintInt64 (LIR.Physical LIR.X0)]
                    LIR.Ret
    match runLIRProgramFull program with
    | Error e -> Error e
    | Ok (exitCode, stdout) ->
        if exitCode <> 0 then Error $"Expected exit code 0, got {exitCode}"
        elif stdout.Trim() <> "0" then Error $"Expected stdout '0', got '{stdout.Trim()}'"
        else Ok ()

/// Test: HeapAlloc initializes the trailing fixed-block refcount.
let testHeapAllocInitializesRefcount () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 16)
                LIR.HeapLoad (LIR.Physical LIR.X1, LIR.Physical LIR.X2, 16)
                LIR.Exit
            ]
            LIR.Ret

    match runLIRProgram program with
    | Error e -> Error e
    | Ok exitCode ->
        if exitCode = 1 then Ok ()
        else Error $"Expected initialized refcount exit code 1, got {exitCode}"

let tests : (string * (unit -> Result<unit, string>)) list = [
    ("LIR MOV + Exit", testMovAndExit)
    ("LIR ADD immediate", testAddImm)
    ("LIR SUB", testSub)
    ("LIR MUL", testMul)
    ("LIR conditional branch", testBranch)
    ("LIR PrintInt64", testPrintInt64)
    ("LIR PrintInt64 negative", testPrintInt64Negative)
    ("LIR PrintInt64 zero", testPrintInt64Zero)
    ("LIR HeapAlloc initializes refcount", testHeapAllocInitializesRefcount)
]
