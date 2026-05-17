// X86_64CodeGenTests.fs - Tests for x86-64 code generation from LIR
//
// Verifies that LIR programs translate to working x86-64 executables.

module X86_64CodeGenTests

/// Build and run a LIR program, returning exit code, stdout, and stderr.
let private runLIRProgramFullWithOptions (program: LIR.Program) (enableLeakCheck: bool) : Result<int * string * string, string> =
    match CodeGen_X86_64.translateProgram program enableLeakCheck with
    | Error e -> Error $"Codegen error: {e}"
    | Ok instrs ->
        match X86_64_Resolve.resolveAndEncode instrs with
        | Error e -> Error $"Resolve error: {e}"
        | Ok unresolvedResult ->
            let patchedResult =
                if List.isEmpty unresolvedResult.DeferredFixups then
                    Ok unresolvedResult
                else
                    let elfHeaderSize = 64
                    let programHeaderSize = 56
                    let codeFileOffset = elfHeaderSize + programHeaderSize
                    let codeSize = unresolvedResult.MachineCode.Length
                    let alignedDataStart = (codeFileOffset + codeSize + 7) &&& (~~~7)
                    let dataLabels = Map.ofList [("_leak_count", alignedDataStart)]
                    X86_64_Resolve.patchDataLabels unresolvedResult dataLabels codeFileOffset

            match patchedResult with
            | Error e -> Error $"Data label error: {e}"
            | Ok resolveResult ->
            let binary =
                Binary_Generation_ELF_X86_64.createExecutableWithPools
                    resolveResult.MachineCode LiteralPool.emptyStringPool LiteralPool.emptyFloatPool enableLeakCheck 0
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
                let stderr = proc.StandardError.ReadToEnd()
                proc.WaitForExit(10000) |> ignore
                Ok (proc.ExitCode, stdout, stderr)
            with ex -> Error $"Execution failed: {ex.Message}"
            |> fun result ->
                try System.IO.File.Delete(tempPath) with _ -> ()
                result

/// Build and run a LIR program, returning exit code and stdout.
let private runLIRProgramFull (program: LIR.Program) : Result<int * string, string> =
    runLIRProgramFullWithOptions program false
    |> Result.map (fun (exitCode, stdout, _) -> exitCode, stdout)

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
let private makeSimpleProgramWithRecords (instrs: LIR.Instr list) (term: LIR.Terminator) (records: LIR.RecordRegistry) : LIR.Program =
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
    LIR.Program ([func], records)

let private makeSimpleProgram (instrs: LIR.Instr list) (term: LIR.Terminator) : LIR.Program =
    makeSimpleProgramWithRecords instrs term Map.empty

let private makeEmptyFunction (name: string) (typedParams: LIR.TypedLIRParam list) : LIR.Function =
    let label = LIR.Label $"{name}_entry"
    {
        Name = name
        TypedParams = typedParams
        CFG = {
            Entry = label
            Blocks = Map.ofList [(label, { Label = label; Instrs = []; Terminator = LIR.Ret })]
        }
        StackSize = 0
        UsedCalleeSaved = []
    }

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
    let program = LIR.Program ([func], Map.empty)
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

/// Test: fixed-block bump allocation increments x64 leak accounting.
let testHeapAllocIncrementsLeakCounter () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 16)
                LIR.HeapLoad (LIR.Physical LIR.X1, LIR.Physical LIR.X2, 0)
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "leaks: 1" then Ok ()
        else Error $"Expected leak checker to report one x64 fixed-block allocation, got stderr '{stderr.Trim()}'"

/// Test: generic fixed-block RefCountInc increments the trailing refcount.
let testGenericRefCountInc () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 16)
                LIR.RefCountInc (LIR.Physical LIR.X2, 16, LIR.GenericHeap, None)
                LIR.HeapLoad (LIR.Physical LIR.X1, LIR.Physical LIR.X2, 16)
                LIR.Exit
            ]
            LIR.Ret

    match runLIRProgram program with
    | Error e -> Error e
    | Ok exitCode ->
        if exitCode = 2 then Ok ()
        else Error $"Expected RefCountInc to raise refcount exit code to 2, got {exitCode}"

/// Test: generic fixed-block RefCountDec reclaims 16-byte payload blocks.
let testGenericRefCountDecTuple2 () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 16)
                LIR.RefCountDec (LIR.Physical LIR.X2, 16, LIR.GenericHeap, None)
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected tuple2-sized RefCountDec to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: generic fixed-block RefCountDec handles other fixed payload sizes.
let testGenericRefCountDecFixedSizes () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 8)
                LIR.RefCountDec (LIR.Physical LIR.X2, 8, LIR.GenericHeap, None)
                LIR.HeapAlloc (LIR.Physical LIR.X3, 24)
                LIR.RefCountDec (LIR.Physical LIR.X3, 24, LIR.GenericHeap, None)
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected generic fixed-size RefCountDec to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 dynamic string RefCountDec balances StringConcat allocation.
let testDynamicStringRefCountDec () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "a", LIR.StringSymbol "b")
                LIR.RefCountDecString (LIR.Reg (LIR.Physical LIR.X2))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected dynamic string RefCountDec to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases a dynamic string field.
let testGenericRefCountDecStringField () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "a", LIR.StringSymbol "b")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.RefCountDec (LIR.Physical LIR.X3, 8, LIR.GenericHeap, Some (AST.TTuple [AST.TString]))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected fixed-block string field release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases a dynamic bytes field.
let testGenericRefCountDecBytesField () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "a", LIR.StringSymbol "b")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TBytes)
                LIR.RefCountDec (LIR.Physical LIR.X3, 8, LIR.GenericHeap, Some (AST.TTuple [AST.TBytes]))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected fixed-block bytes field release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases nested tuple fields.
let testGenericRefCountDecNestedStringTupleField () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "a", LIR.StringSymbol "b")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X3), Some (AST.TTuple [AST.TString]))
                LIR.RefCountDec (LIR.Physical LIR.X4, 8, LIR.GenericHeap, Some (AST.TTuple [AST.TTuple [AST.TString]]))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected nested tuple field release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases a record string field.
let testGenericRefCountDecRecordStringField () : Result<unit, string> =
    let recordType = AST.TRecord ("X64RcRecord", [])
    let records = Map.ofList [("X64RcRecord", [("value", AST.TString)])]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "a", LIR.StringSymbol "b")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.RefCountDec (LIR.Physical LIR.X3, 8, LIR.GenericHeap, Some recordType)
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected record string field release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases a record dict root field.
let testGenericRefCountDecRecordDictField () : Result<unit, string> =
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let recordType = AST.TRecord ("X64RcRecordDict", [])
    let records = Map.ofList [("X64RcRecordDict", [("value", dictType)])]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 16)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X2, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X3, LIR.Physical LIR.X2, LIR.Physical LIR.X3)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X3), Some dictType)
                LIR.RefCountDec (LIR.Physical LIR.X4, 8, LIR.GenericHeap, Some recordType)
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected record dict field release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases a record closure root field.
let testGenericRefCountDecRecordClosureField () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let recordType = AST.TRecord ("X64RcRecordClosure", [])
    let records = Map.ofList [("X64RcRecordClosure", [("value", closureType)])]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.RefCountDec (LIR.Physical LIR.X3, 8, LIR.GenericHeap, Some recordType)
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected record closure field release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases a boxed sum string payload.
let testGenericRefCountDecSumStringPayload () : Result<unit, string> =
    let sumType = AST.TSum ("X64RcSum", [AST.TString])
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "a", LIR.StringSymbol "b")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 16)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.RefCountDec (LIR.Physical LIR.X3, 16, LIR.GenericHeap, Some sumType)
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected boxed sum string payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases a boxed sum bytes payload.
let testGenericRefCountDecSumBytesPayload () : Result<unit, string> =
    let sumType = AST.TSum ("X64RcSumBytes", [AST.TBytes])
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "a", LIR.StringSymbol "b")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 16)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Reg (LIR.Physical LIR.X2), Some AST.TBytes)
                LIR.RefCountDec (LIR.Physical LIR.X3, 16, LIR.GenericHeap, Some sumType)
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected boxed sum bytes payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases nested boxed sum fields.
let testGenericRefCountDecNestedSumStringField () : Result<unit, string> =
    let sumType = AST.TSum ("X64RcSum", [AST.TString])
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "a", LIR.StringSymbol "b")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 16)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X3), Some sumType)
                LIR.RefCountDec (LIR.Physical LIR.X4, 8, LIR.GenericHeap, Some (AST.TTuple [sumType]))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected nested boxed sum field release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases boxed sum list payloads.
let testGenericRefCountDecSumListPayload () : Result<unit, string> =
    let sumType = AST.TSum ("ListPayloadSum", [AST.TList AST.TInt64])
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 8)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X3, LIR.Physical LIR.X2, LIR.Physical LIR.X3)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some (AST.TList AST.TInt64))
                LIR.RefCountDec (LIR.Physical LIR.X4, 16, LIR.GenericHeap, Some sumType)
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected boxed sum list payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases boxed sum dict payloads.
let testGenericRefCountDecSumDictPayload () : Result<unit, string> =
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let sumType = AST.TSum ("DictPayloadSum", [dictType])
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 16)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X2, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X3, LIR.Physical LIR.X2, LIR.Physical LIR.X3)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some dictType)
                LIR.RefCountDec (LIR.Physical LIR.X4, 16, LIR.GenericHeap, Some sumType)
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected boxed sum dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases dict root fields.
let testGenericRefCountDecDictField () : Result<unit, string> =
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 16)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X2, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X3, LIR.Physical LIR.X2, LIR.Physical LIR.X3)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X3), Some dictType)
                LIR.RefCountDec (LIR.Physical LIR.X4, 8, LIR.GenericHeap, Some (AST.TTuple [dictType]))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected dict field release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec preserves live RAX across nested list field release.
let testGenericRefCountDecPreservesLiveRaxAcrossListFieldRelease () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 8)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X3, LIR.Physical LIR.X2, LIR.Physical LIR.X3)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X3), Some listType)
                LIR.Mov (LIR.Physical LIR.X0, LIR.Imm 123L)
                LIR.RefCountDec (LIR.Physical LIR.X4, 8, LIR.GenericHeap, Some (AST.TTuple [listType]))
                LIR.PrintInt64 (LIR.Physical LIR.X0)
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, stdout, stderr) ->
        let output = stdout.Trim()
        let leaks = stderr.Trim()
        if output = "123" && leaks = "" then Ok ()
        else Error $"Expected live RAX value 123 and no leaks, got stdout '{output}' and stderr '{leaks}'"

/// Test: x64 generic fixed-block RefCountDec preserves live RAX across nested dict field release.
let testGenericRefCountDecPreservesLiveRaxAcrossDictFieldRelease () : Result<unit, string> =
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 16)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X2, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X3, LIR.Physical LIR.X2, LIR.Physical LIR.X3)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X3), Some dictType)
                LIR.Mov (LIR.Physical LIR.X0, LIR.Imm 456L)
                LIR.RefCountDec (LIR.Physical LIR.X4, 8, LIR.GenericHeap, Some (AST.TTuple [dictType]))
                LIR.PrintInt64 (LIR.Physical LIR.X0)
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, stdout, stderr) ->
        let output = stdout.Trim()
        let leaks = stderr.Trim()
        if output = "456" && leaks = "" then Ok ()
        else Error $"Expected live RAX value 456 and no leaks, got stdout '{output}' and stderr '{leaks}'"

/// Test: x64 generic fixed-block RefCountDec preserves live RAX across nested closure field release.
let testGenericRefCountDecPreservesLiveRaxAcrossClosureFieldRelease () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let program =
        makeSimpleProgram
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.Mov (LIR.Physical LIR.X0, LIR.Imm 789L)
                LIR.RefCountDec (LIR.Physical LIR.X3, 8, LIR.GenericHeap, Some (AST.TTuple [closureType]))
                LIR.PrintInt64 (LIR.Physical LIR.X0)
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, stdout, stderr) ->
        let output = stdout.Trim()
        let leaks = stderr.Trim()
        if output = "789" && leaks = "" then Ok ()
        else Error $"Expected live RAX value 789 and no leaks, got stdout '{output}' and stderr '{leaks}'"

/// Test: x64 closure allocation and explicit release balance leak accounting.
let testClosureAllocRefCountDecBalancesLeakCounter () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let program =
        makeSimpleProgram
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.RefCountDec (LIR.Physical LIR.X2, 8, LIR.ClosureHeap, Some closureType)
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure allocation and release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases closure root fields.
let testGenericRefCountDecClosureField () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let program =
        makeSimpleProgram
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.RefCountDec (LIR.Physical LIR.X3, 8, LIR.GenericHeap, Some (AST.TTuple [closureType]))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure field release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases boxed sum closure payloads.
let testGenericRefCountDecSumClosurePayload () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let sumType = AST.TSum ("ClosurePayloadSum", [closureType])
    let program =
        makeSimpleProgram
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.HeapAlloc (LIR.Physical LIR.X3, 16)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.RefCountDec (LIR.Physical LIR.X3, 16, LIR.GenericHeap, Some sumType)
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected boxed sum closure payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 closure RefCountDec releases dynamic string captures.
let testClosureRefCountDecStringCapture () : Result<unit, string> =
    let closureTupleType = AST.TTuple [AST.TInt64; AST.TString]
    let capturedFunc =
        makeEmptyFunction
            "x64_string_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureTupleType }]
    let main =
        match makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.ClosureAlloc (LIR.Physical LIR.X3, "x64_string_capture_fn", [LIR.Reg (LIR.Physical LIR.X2)])
                LIR.RefCountDec (LIR.Physical LIR.X3, 16, LIR.ClosureHeap, Some (AST.TFunction ([AST.TInt64], AST.TInt64)))
            ]
            LIR.Ret with
        | LIR.Program ([func], records) -> LIR.Program ([func; capturedFunc], records)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure string capture release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 closure RefCountDec releases dynamic bytes captures.
let testClosureRefCountDecBytesCapture () : Result<unit, string> =
    let closureTupleType = AST.TTuple [AST.TInt64; AST.TBytes]
    let capturedFunc =
        makeEmptyFunction
            "x64_bytes_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureTupleType }]
    let main =
        match makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.ClosureAlloc (LIR.Physical LIR.X3, "x64_bytes_capture_fn", [LIR.Reg (LIR.Physical LIR.X2)])
                LIR.RefCountDec (LIR.Physical LIR.X3, 16, LIR.ClosureHeap, Some (AST.TFunction ([AST.TInt64], AST.TInt64)))
            ]
            LIR.Ret with
        | LIR.Program ([func], records) -> LIR.Program ([func; capturedFunc], records)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure bytes capture release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 closure RefCountDec releases captured list roots.
let testClosureRefCountDecListCapture () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let closureTupleType = AST.TTuple [AST.TInt64; listType]
    let capturedFunc =
        makeEmptyFunction
            "x64_list_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureTupleType }]
    let main =
        match makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 8)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X3, LIR.Physical LIR.X2, LIR.Physical LIR.X3)
                LIR.ClosureAlloc (LIR.Physical LIR.X4, "x64_list_capture_fn", [LIR.Reg (LIR.Physical LIR.X3)])
                LIR.RefCountDec (LIR.Physical LIR.X4, 16, LIR.ClosureHeap, Some (AST.TFunction ([AST.TInt64], AST.TInt64)))
            ]
            LIR.Ret with
        | LIR.Program ([func], records) -> LIR.Program ([func; capturedFunc], records)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure list capture release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 closure RefCountDec releases captured dict roots.
let testClosureRefCountDecDictCapture () : Result<unit, string> =
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let closureTupleType = AST.TTuple [AST.TInt64; dictType]
    let capturedFunc =
        makeEmptyFunction
            "x64_dict_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureTupleType }]
    let main =
        match makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 16)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X2, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X3, LIR.Physical LIR.X2, LIR.Physical LIR.X3)
                LIR.ClosureAlloc (LIR.Physical LIR.X4, "x64_dict_capture_fn", [LIR.Reg (LIR.Physical LIR.X3)])
                LIR.RefCountDec (LIR.Physical LIR.X4, 16, LIR.ClosureHeap, Some (AST.TFunction ([AST.TInt64], AST.TInt64)))
            ]
            LIR.Ret with
        | LIR.Program ([func], records) -> LIR.Program ([func; capturedFunc], records)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure dict capture release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 closure RefCountDec releases captured closure roots.
let testClosureRefCountDecClosureCapture () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let closureTupleType = AST.TTuple [AST.TInt64; closureType]
    let capturedFunc =
        makeEmptyFunction
            "x64_closure_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureTupleType }]
    let main =
        match makeSimpleProgram
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.ClosureAlloc (LIR.Physical LIR.X3, "x64_closure_capture_fn", [LIR.Reg (LIR.Physical LIR.X2)])
                LIR.RefCountDec (LIR.Physical LIR.X3, 16, LIR.ClosureHeap, Some closureType)
            ]
            LIR.Ret with
        | LIR.Program ([func], records) -> LIR.Program ([func; capturedFunc], records)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure capture release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 closure RefCountDec releases captured fixed blocks and their fields.
let testClosureRefCountDecTupleStringCapture () : Result<unit, string> =
    let tupleType = AST.TTuple [AST.TString; AST.TInt64]
    let closureTupleType = AST.TTuple [AST.TInt64; tupleType]
    let capturedFunc =
        makeEmptyFunction
            "x64_tuple_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureTupleType }]
    let main =
        match makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 16)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Imm 7L, None)
                LIR.ClosureAlloc (LIR.Physical LIR.X4, "x64_tuple_capture_fn", [LIR.Reg (LIR.Physical LIR.X3)])
                LIR.RefCountDec (LIR.Physical LIR.X4, 16, LIR.ClosureHeap, Some (AST.TFunction ([AST.TInt64], AST.TInt64)))
            ]
            LIR.Ret with
        | LIR.Program ([func], records) -> LIR.Program ([func; capturedFunc], records)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure tuple capture release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 closure RefCountDec releases captured fixed blocks with multiple managed fields.
let testClosureRefCountDecTupleStringListDictCapture () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let tupleType = AST.TTuple [AST.TString; listType; dictType]
    let closureTupleType = AST.TTuple [AST.TInt64; tupleType]
    let capturedFunc =
        makeEmptyFunction
            "x64_tuple_string_list_dict_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureTupleType }]
    let main =
        match makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 16)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X5, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 24)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X6), Some dictType)
                LIR.ClosureAlloc (LIR.Physical LIR.X20, "x64_tuple_string_list_dict_capture_fn", [LIR.Reg (LIR.Physical LIR.X19)])
                LIR.RefCountDec (LIR.Physical LIR.X20, 16, LIR.ClosureHeap, Some (AST.TFunction ([AST.TInt64], AST.TInt64)))
            ]
            LIR.Ret with
        | LIR.Program ([func], records) -> LIR.Program ([func; capturedFunc], records)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure tuple string/list/dict capture release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 closure RefCountDec releases captured record fields.
let testClosureRefCountDecRecordStringCapture () : Result<unit, string> =
    let recordType = AST.TRecord ("X64ClosureCaptureRecord", [])
    let records = Map.ofList [("X64ClosureCaptureRecord", [("value", AST.TString)])]
    let closureTupleType = AST.TTuple [AST.TInt64; recordType]
    let capturedFunc =
        makeEmptyFunction
            "x64_record_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureTupleType }]
    let main =
        match makeSimpleProgramWithRecords
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.ClosureAlloc (LIR.Physical LIR.X4, "x64_record_capture_fn", [LIR.Reg (LIR.Physical LIR.X3)])
                LIR.RefCountDec (LIR.Physical LIR.X4, 16, LIR.ClosureHeap, Some (AST.TFunction ([AST.TInt64], AST.TInt64)))
            ]
            LIR.Ret
            records with
        | LIR.Program ([func], programRecords) -> LIR.Program ([func; capturedFunc], programRecords)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure record capture release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 closure RefCountDec releases captured records with multiple managed fields.
let testClosureRefCountDecRecordStringListDictCapture () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let recordType = AST.TRecord ("X64ClosureCaptureRecordStringListDict", [])
    let records =
        Map.ofList
            [("X64ClosureCaptureRecordStringListDict", [("name", AST.TString); ("items", listType); ("lookup", dictType)])]
    let closureTupleType = AST.TTuple [AST.TInt64; recordType]
    let capturedFunc =
        makeEmptyFunction
            "x64_record_string_list_dict_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureTupleType }]
    let main =
        match makeSimpleProgramWithRecords
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 16)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X5, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 24)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X6), Some dictType)
                LIR.ClosureAlloc (LIR.Physical LIR.X20, "x64_record_string_list_dict_capture_fn", [LIR.Reg (LIR.Physical LIR.X19)])
                LIR.RefCountDec (LIR.Physical LIR.X20, 16, LIR.ClosureHeap, Some (AST.TFunction ([AST.TInt64], AST.TInt64)))
            ]
            LIR.Ret
            records with
        | LIR.Program ([func], programRecords) -> LIR.Program ([func; capturedFunc], programRecords)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure record string/list/dict capture release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 closure RefCountDec releases captured boxed sum payloads.
let testClosureRefCountDecSumStringCapture () : Result<unit, string> =
    let sumType = AST.TSum ("X64ClosureCaptureSum", [AST.TString])
    let closureTupleType = AST.TTuple [AST.TInt64; sumType]
    let capturedFunc =
        makeEmptyFunction
            "x64_sum_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureTupleType }]
    let main =
        match makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 16)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.ClosureAlloc (LIR.Physical LIR.X4, "x64_sum_capture_fn", [LIR.Reg (LIR.Physical LIR.X3)])
                LIR.RefCountDec (LIR.Physical LIR.X4, 16, LIR.ClosureHeap, Some (AST.TFunction ([AST.TInt64], AST.TInt64)))
            ]
            LIR.Ret with
        | LIR.Program ([func], records) -> LIR.Program ([func; capturedFunc], records)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure sum capture release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 closure RefCountDec releases captured boxed sums with nested managed fields.
let testClosureRefCountDecSumTupleStringListDictCapture () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let tupleType = AST.TTuple [AST.TString; listType; dictType]
    let sumType = AST.TSum ("X64ClosureCaptureSumTupleStringListDict", [tupleType])
    let closureTupleType = AST.TTuple [AST.TInt64; sumType]
    let capturedFunc =
        makeEmptyFunction
            "x64_sum_tuple_string_list_dict_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureTupleType }]
    let main =
        match makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 16)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X5, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 24)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X6), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 16)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X19), Some tupleType)
                LIR.ClosureAlloc (LIR.Physical LIR.X21, "x64_sum_tuple_string_list_dict_capture_fn", [LIR.Reg (LIR.Physical LIR.X20)])
                LIR.RefCountDec (LIR.Physical LIR.X21, 16, LIR.ClosureHeap, Some (AST.TFunction ([AST.TInt64], AST.TInt64)))
            ]
            LIR.Ret with
        | LIR.Program ([func], records) -> LIR.Program ([func; capturedFunc], records)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure sum tuple string/list/dict capture release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 closure RefCountDec releases multiple managed captures.
let testClosureRefCountDecMultipleCaptures () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let closureTupleType = AST.TTuple [AST.TInt64; AST.TString; listType]
    let capturedFunc =
        makeEmptyFunction
            "x64_multi_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureTupleType }]
    let main =
        match makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.ClosureAlloc (LIR.Physical LIR.X5, "x64_multi_capture_fn", [LIR.Reg (LIR.Physical LIR.X2); LIR.Reg (LIR.Physical LIR.X4)])
                LIR.RefCountDec (LIR.Physical LIR.X5, 24, LIR.ClosureHeap, Some (AST.TFunction ([AST.TInt64], AST.TInt64)))
            ]
            LIR.Ret with
        | LIR.Program ([func], records) -> LIR.Program ([func; capturedFunc], records)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure multi-capture release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases closure leaf payloads.
let testTaggedListRefCountDecClosurePayload () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let program =
        makeSimpleProgram
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.RefCountDec (LIR.Physical LIR.X4, 0, LIR.TaggedList, Some (AST.TList closureType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list closure payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases dict leaf payloads.
let testTaggedListRefCountDecDictPayload () : Result<unit, string> =
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 16)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X2, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X3, LIR.Physical LIR.X2, LIR.Physical LIR.X3)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X3), Some dictType)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.TaggedList, Some (AST.TList dictType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases dynamic string leaf payloads.
let testTaggedListRefCountDecStringPayload () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.RefCountDec (LIR.Physical LIR.X4, 0, LIR.TaggedList, Some (AST.TList AST.TString))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list string payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases fields inside tuple leaf payloads.
let testTaggedListRefCountDecTupleStringPayload () : Result<unit, string> =
    let tupleType = AST.TTuple [AST.TString; AST.TInt64]
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 16)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Imm 42L, None)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X3), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.TaggedList, Some (AST.TList tupleType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple string payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases higher-arity tuple leaf payloads.
let testTaggedListRefCountDecTuple3DynamicPayload () : Result<unit, string> =
    let tupleType = AST.TTuple [AST.TString; AST.TInt64; AST.TBytes]
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 24)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Imm 42L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 16, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 8)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Reg (LIR.Physical LIR.X4), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.TaggedList, Some (AST.TList tupleType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple3 dynamic payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases tuple3 middle dynamic leaf payloads.
let testTaggedListRefCountDecTuple3MiddleDynamicPayload () : Result<unit, string> =
    let tupleType = AST.TTuple [AST.TInt64; AST.TString; AST.TInt64]
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 24)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X3, 16, LIR.Imm 3L, None)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X3), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.TaggedList, Some (AST.TList tupleType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple3 middle dynamic payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases every tuple3 dynamic field combination.
let testTaggedListRefCountDecTuple3DynamicPayloadCombinations () : Result<unit, string> =
    let dynamicRegForIndex (index: int) : LIR.PhysReg =
        match index with
        | 0 -> LIR.X2
        | 1 -> LIR.X3
        | 2 -> LIR.X4
        | _ -> Crash.crash $"Unexpected tuple3 field index {index}"

    let isDynamicField (fieldType: AST.Type) : bool =
        match fieldType with
        | AST.TString
        | AST.TBytes -> true
        | _ -> false

    let runCase (name: string, fields: AST.Type list) : Result<unit, string> =
        let tupleType = AST.TTuple fields
        let dynamicAllocs =
            fields
            |> List.mapi (fun index fieldType ->
                if isDynamicField fieldType then
                    let reg = dynamicRegForIndex index
                    Some (LIR.StringConcat (LIR.Physical reg, LIR.StringSymbol $"left{name}{index}", LIR.StringSymbol $"right{name}{index}"))
                else
                    None)
            |> List.choose id
        let fieldStores =
            fields
            |> List.mapi (fun index fieldType ->
                let offset = index * 8
                if isDynamicField fieldType then
                    LIR.HeapStore (LIR.Physical LIR.X5, offset, LIR.Reg (LIR.Physical (dynamicRegForIndex index)), Some fieldType)
                else
                    LIR.HeapStore (LIR.Physical LIR.X5, offset, LIR.Imm (int64 (index + 1)), None))
        let program =
            makeSimpleProgram
                (dynamicAllocs
                 @ [LIR.HeapAlloc (LIR.Physical LIR.X5, 24)]
                 @ fieldStores
                 @ [LIR.HeapAlloc (LIR.Physical LIR.X6, 8)
                    LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Reg (LIR.Physical LIR.X5), Some tupleType)
                    LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 5L)
                    LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                    LIR.RefCountDec (LIR.Physical LIR.X7, 0, LIR.TaggedList, Some (AST.TList tupleType))])
                LIR.Ret

        match runLIRProgramFullWithOptions program true with
        | Error e -> Error e
        | Ok (_, _, stderr) ->
            if stderr.Trim() = "" then Ok ()
            else Error $"Expected list tuple3 {name} dynamic payload release to balance leak counter, got stderr '{stderr.Trim()}'"

    let rec runCases (cases: (string * AST.Type list) list) : Result<unit, string> =
        match cases with
        | [] -> Ok ()
        | case :: rest ->
            match runCase case with
            | Ok () -> runCases rest
            | Error e -> Error e

    runCases
        [ ("first", [AST.TString; AST.TInt64; AST.TInt64])
          ("third", [AST.TInt64; AST.TInt64; AST.TString])
          ("first-second", [AST.TString; AST.TBytes; AST.TInt64])
          ("second-third", [AST.TInt64; AST.TString; AST.TBytes])
          ("all", [AST.TString; AST.TBytes; AST.TString]) ]

/// Test: x64 tagged-list RefCountDec releases tuple3 payloads with mixed managed fields.
let testTaggedListRefCountDecTuple3StringListDictPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let tupleType = AST.TTuple [AST.TString; listType; dictType]
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 16)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X5, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.HeapAlloc (LIR.Physical LIR.X7, 24)
                LIR.HeapStore (LIR.Physical LIR.X7, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X7, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X7, 16, LIR.Reg (LIR.Physical LIR.X6), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 8)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X7), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X20, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X20, LIR.Physical LIR.X19, LIR.Physical LIR.X20)
                LIR.RefCountDec (LIR.Physical LIR.X20, 0, LIR.TaggedList, Some (AST.TList tupleType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple3 string/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases fields inside record leaf payloads.
let testTaggedListRefCountDecRecordStringPayload () : Result<unit, string> =
    let recordType = AST.TRecord ("X64ListRcRecord", [])
    let records = Map.ofList [("X64ListRcRecord", [("value", AST.TString)])]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X3), Some recordType)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.TaggedList, Some (AST.TList recordType))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list record string payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases higher-field record leaf payloads.
let testTaggedListRefCountDecRecord3DynamicPayload () : Result<unit, string> =
    let recordType = AST.TRecord ("X64ListRcRecord3", [])
    let records =
        Map.ofList
            [("X64ListRcRecord3", [("name", AST.TString); ("count", AST.TInt64); ("data", AST.TBytes)])]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 24)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Imm 42L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 16, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 8)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Reg (LIR.Physical LIR.X4), Some recordType)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.TaggedList, Some (AST.TList recordType))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list record3 dynamic payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases mixed managed fields in record3 payloads.
let testTaggedListRefCountDecRecord3StringListDictPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let recordType = AST.TRecord ("X64ListRcRecord3StringListDict", [])
    let records =
        Map.ofList
            [("X64ListRcRecord3StringListDict", [("name", AST.TString); ("items", listType); ("lookup", dictType)])]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 16)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X5, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.HeapAlloc (LIR.Physical LIR.X7, 24)
                LIR.HeapStore (LIR.Physical LIR.X7, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X7, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X7, 16, LIR.Reg (LIR.Physical LIR.X6), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 8)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X7), Some recordType)
                LIR.Mov (LIR.Physical LIR.X20, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X20, LIR.Physical LIR.X19, LIR.Physical LIR.X20)
                LIR.RefCountDec (LIR.Physical LIR.X20, 0, LIR.TaggedList, Some (AST.TList recordType))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list record3 string/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases bytes/list/dict fields in record3 payloads.
let testTaggedListRefCountDecRecord3BytesListDictPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let recordType = AST.TRecord ("X64ListRcRecord3BytesListDict", [])
    let records =
        Map.ofList
            [("X64ListRcRecord3BytesListDict", [("blob", AST.TBytes); ("items", listType); ("lookup", dictType)])]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 16)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X5, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.HeapAlloc (LIR.Physical LIR.X7, 24)
                LIR.HeapStore (LIR.Physical LIR.X7, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X7, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X7, 16, LIR.Reg (LIR.Physical LIR.X6), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 8)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X7), Some recordType)
                LIR.Mov (LIR.Physical LIR.X20, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X20, LIR.Physical LIR.X19, LIR.Physical LIR.X20)
                LIR.RefCountDec (LIR.Physical LIR.X20, 0, LIR.TaggedList, Some (AST.TList recordType))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list record3 bytes/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases record3 middle dynamic leaf payloads.
let testTaggedListRefCountDecRecord3MiddleDynamicPayload () : Result<unit, string> =
    let recordType = AST.TRecord ("X64ListRcRecord3Middle", [])
    let records =
        Map.ofList
            [("X64ListRcRecord3Middle", [("count", AST.TInt64); ("name", AST.TString); ("size", AST.TInt64)])]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 24)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X3, 16, LIR.Imm 3L, None)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X3), Some recordType)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.TaggedList, Some (AST.TList recordType))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list record3 middle dynamic payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases every record3 dynamic field combination.
let testTaggedListRefCountDecRecord3DynamicPayloadCombinations () : Result<unit, string> =
    let dynamicRegForIndex (index: int) : LIR.PhysReg =
        match index with
        | 0 -> LIR.X2
        | 1 -> LIR.X3
        | 2 -> LIR.X4
        | _ -> Crash.crash $"Unexpected record3 field index {index}"

    let isDynamicField (fieldType: AST.Type) : bool =
        match fieldType with
        | AST.TString
        | AST.TBytes -> true
        | _ -> false

    let runCase (name: string, fields: AST.Type list) : Result<unit, string> =
        let recordName = $"X64ListRcRecord3{name}"
        let recordType = AST.TRecord (recordName, [])
        let records =
            fields
            |> List.mapi (fun index fieldType -> ($"field{index}", fieldType))
            |> fun recordFields -> Map.ofList [(recordName, recordFields)]
        let dynamicAllocs =
            fields
            |> List.mapi (fun index fieldType ->
                if isDynamicField fieldType then
                    let reg = dynamicRegForIndex index
                    Some (LIR.StringConcat (LIR.Physical reg, LIR.StringSymbol $"left{name}{index}", LIR.StringSymbol $"right{name}{index}"))
                else
                    None)
            |> List.choose id
        let fieldStores =
            fields
            |> List.mapi (fun index fieldType ->
                let offset = index * 8
                if isDynamicField fieldType then
                    LIR.HeapStore (LIR.Physical LIR.X5, offset, LIR.Reg (LIR.Physical (dynamicRegForIndex index)), Some fieldType)
                else
                    LIR.HeapStore (LIR.Physical LIR.X5, offset, LIR.Imm (int64 (index + 1)), None))
        let program =
            makeSimpleProgramWithRecords
                (dynamicAllocs
                 @ [LIR.HeapAlloc (LIR.Physical LIR.X5, 24)]
                 @ fieldStores
                 @ [LIR.HeapAlloc (LIR.Physical LIR.X6, 8)
                    LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Reg (LIR.Physical LIR.X5), Some recordType)
                    LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 5L)
                    LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                    LIR.RefCountDec (LIR.Physical LIR.X7, 0, LIR.TaggedList, Some (AST.TList recordType))])
                LIR.Ret
                records

        match runLIRProgramFullWithOptions program true with
        | Error e -> Error e
        | Ok (_, _, stderr) ->
            if stderr.Trim() = "" then Ok ()
            else Error $"Expected list record3 {name} dynamic payload release to balance leak counter, got stderr '{stderr.Trim()}'"

    let rec runCases (cases: (string * AST.Type list) list) : Result<unit, string> =
        match cases with
        | [] -> Ok ()
        | case :: rest ->
            match runCase case with
            | Ok () -> runCases rest
            | Error e -> Error e

    runCases
        [ ("First", [AST.TString; AST.TInt64; AST.TInt64])
          ("Third", [AST.TInt64; AST.TInt64; AST.TString])
          ("FirstSecond", [AST.TString; AST.TBytes; AST.TInt64])
          ("SecondThird", [AST.TInt64; AST.TString; AST.TBytes])
          ("All", [AST.TString; AST.TBytes; AST.TString]) ]

/// Test: x64 tagged-list RefCountDec releases boxed sum leaf payload fields.
let testTaggedListRefCountDecSumStringPayload () : Result<unit, string> =
    let sumType = AST.TSum ("X64ListRcSum", [AST.TString])
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 16)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X3), Some sumType)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.TaggedList, Some (AST.TList sumType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum string payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases boxed sum leaf list payloads.
let testTaggedListRefCountDecSumListPayload () : Result<unit, string> =
    let innerListType = AST.TList AST.TInt64
    let sumType = AST.TSum ("X64ListRcSumList", [innerListType])
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 8)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X3, LIR.Physical LIR.X2, LIR.Physical LIR.X3)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some innerListType)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 8)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Reg (LIR.Physical LIR.X4), Some sumType)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.TaggedList, Some (AST.TList sumType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum list payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases boxed sum leaf dict payloads.
let testTaggedListRefCountDecSumDictPayload () : Result<unit, string> =
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let sumType = AST.TSum ("X64ListRcSumDict", [dictType])
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 16)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X2, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X3, LIR.Physical LIR.X2, LIR.Physical LIR.X3)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 8)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Reg (LIR.Physical LIR.X4), Some sumType)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.TaggedList, Some (AST.TList sumType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases boxed sum leaf closure payloads.
let testTaggedListRefCountDecSumClosurePayload () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let sumType = AST.TSum ("X64ListRcSumClosure", [closureType])
    let program =
        makeSimpleProgram
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.HeapAlloc (LIR.Physical LIR.X3, 16)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X3), Some sumType)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.TaggedList, Some (AST.TList sumType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum closure payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases boxed sum leaf tuple payloads.
let testTaggedListRefCountDecSumTupleStringPayload () : Result<unit, string> =
    let tupleType = AST.TTuple [AST.TString; AST.TInt64]
    let sumType = AST.TSum ("X64ListRcSumTupleString", [tupleType])
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 16)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Imm 7L, None)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some tupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 8)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Reg (LIR.Physical LIR.X4), Some sumType)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.TaggedList, Some (AST.TList sumType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum tuple payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases boxed sum tuple2 dynamic combinations.
let testTaggedListRefCountDecSumTuple2DynamicPayloadCombinations () : Result<unit, string> =
    let runCase (name: string, tupleType: AST.Type, setup: LIR.Instr list, stores: LIR.Instr list) : Result<unit, string> =
        let sumType = AST.TSum ($"X64ListRcSumTuple{name}", [tupleType])
        let program =
            makeSimpleProgram
                (setup
                 @ [LIR.HeapAlloc (LIR.Physical LIR.X4, 16)]
                 @ stores
                 @ [LIR.HeapAlloc (LIR.Physical LIR.X5, 16)
                    LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Imm 0L, None)
                    LIR.HeapStore (LIR.Physical LIR.X5, 8, LIR.Reg (LIR.Physical LIR.X4), Some tupleType)
                    LIR.HeapAlloc (LIR.Physical LIR.X6, 8)
                    LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Reg (LIR.Physical LIR.X5), Some sumType)
                    LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 5L)
                    LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                    LIR.RefCountDec (LIR.Physical LIR.X7, 0, LIR.TaggedList, Some (AST.TList sumType))])
                LIR.Ret

        match runLIRProgramFullWithOptions program true with
        | Error e -> Error e
        | Ok (_, _, stderr) ->
            if stderr.Trim() = "" then Ok ()
            else Error $"Expected list sum tuple2 {name} payload release to balance leak counter, got stderr '{stderr.Trim()}'"

    let rec runCases (cases: (string * AST.Type * LIR.Instr list * LIR.Instr list) list) : Result<unit, string> =
        match cases with
        | [] -> Ok ()
        | case :: rest ->
            match runCase case with
            | Ok () -> runCases rest
            | Error e -> Error e

    runCases
        [ ("second",
           AST.TTuple [AST.TInt64; AST.TString],
           [LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "leftSecond", LIR.StringSymbol "rightSecond")],
           [LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 7L, None)
            LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TString)])
          ("both",
           AST.TTuple [AST.TString; AST.TBytes],
           [LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "leftBoth0", LIR.StringSymbol "rightBoth0")
            LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "leftBoth1", LIR.StringSymbol "rightBoth1")],
           [LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
            LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)]) ]

/// Test: x64 tagged-list RefCountDec releases boxed sum tuple3 dynamic combinations.
let testTaggedListRefCountDecSumTuple3DynamicPayloadCombinations () : Result<unit, string> =
    let dynamicRegForIndex (index: int) : LIR.PhysReg =
        match index with
        | 0 -> LIR.X2
        | 1 -> LIR.X3
        | 2 -> LIR.X4
        | _ -> Crash.crash $"Unexpected sum tuple3 field index {index}"

    let isDynamicField (fieldType: AST.Type) : bool =
        match fieldType with
        | AST.TString
        | AST.TBytes -> true
        | _ -> false

    let runCase (name: string, fields: AST.Type list) : Result<unit, string> =
        let tupleType = AST.TTuple fields
        let sumType = AST.TSum ($"X64ListRcSumTuple3{name}", [tupleType])
        let dynamicAllocs =
            fields
            |> List.mapi (fun index fieldType ->
                if isDynamicField fieldType then
                    let reg = dynamicRegForIndex index
                    Some (LIR.StringConcat (LIR.Physical reg, LIR.StringSymbol $"left{name}{index}", LIR.StringSymbol $"right{name}{index}"))
                else
                    None)
            |> List.choose id
        let fieldStores =
            fields
            |> List.mapi (fun index fieldType ->
                let offset = index * 8
                if isDynamicField fieldType then
                    LIR.HeapStore (LIR.Physical LIR.X5, offset, LIR.Reg (LIR.Physical (dynamicRegForIndex index)), Some fieldType)
                else
                    LIR.HeapStore (LIR.Physical LIR.X5, offset, LIR.Imm (int64 (index + 1)), None))
        let program =
            makeSimpleProgram
                (dynamicAllocs
                 @ [LIR.HeapAlloc (LIR.Physical LIR.X5, 24)]
                 @ fieldStores
                 @ [LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                    LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 0L, None)
                    LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Reg (LIR.Physical LIR.X5), Some tupleType)
                    LIR.HeapAlloc (LIR.Physical LIR.X7, 8)
                    LIR.HeapStore (LIR.Physical LIR.X7, 0, LIR.Reg (LIR.Physical LIR.X6), Some sumType)
                    LIR.Mov (LIR.Physical LIR.X8, LIR.Imm 5L)
                    LIR.Orr (LIR.Physical LIR.X8, LIR.Physical LIR.X7, LIR.Physical LIR.X8)
                    LIR.RefCountDec (LIR.Physical LIR.X8, 0, LIR.TaggedList, Some (AST.TList sumType))])
                LIR.Ret

        match runLIRProgramFullWithOptions program true with
        | Error e -> Error e
        | Ok (_, _, stderr) ->
            if stderr.Trim() = "" then Ok ()
            else Error $"Expected list sum tuple3 {name} payload release to balance leak counter, got stderr '{stderr.Trim()}'"

    let rec runCases (cases: (string * AST.Type list) list) : Result<unit, string> =
        match cases with
        | [] -> Ok ()
        | case :: rest ->
            match runCase case with
            | Ok () -> runCases rest
            | Error e -> Error e

    runCases
        [ ("First", [AST.TString; AST.TInt64; AST.TInt64])
          ("Second", [AST.TInt64; AST.TString; AST.TInt64])
          ("Third", [AST.TInt64; AST.TInt64; AST.TString])
          ("FirstSecond", [AST.TString; AST.TBytes; AST.TInt64])
          ("SecondThird", [AST.TInt64; AST.TString; AST.TBytes])
          ("FirstThird", [AST.TString; AST.TInt64; AST.TBytes])
          ("All", [AST.TString; AST.TBytes; AST.TString]) ]

/// Test: x64 tagged-list RefCountDec releases boxed sum tuple3 payloads with mixed managed fields.
let testTaggedListRefCountDecSumTuple3StringListDictPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let tupleType = AST.TTuple [AST.TString; listType; dictType]
    let sumType = AST.TSum ("X64ListRcSumTuple3StringListDict", [tupleType])
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 16)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X5, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.HeapAlloc (LIR.Physical LIR.X7, 24)
                LIR.HeapStore (LIR.Physical LIR.X7, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X7, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X7, 16, LIR.Reg (LIR.Physical LIR.X6), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 16)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X7), Some tupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 8)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Reg (LIR.Physical LIR.X19), Some sumType)
                LIR.Mov (LIR.Physical LIR.X21, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X21, LIR.Physical LIR.X20, LIR.Physical LIR.X21)
                LIR.RefCountDec (LIR.Physical LIR.X21, 0, LIR.TaggedList, Some (AST.TList sumType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum tuple3 string/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases boxed sum leaf record payloads.
let testTaggedListRefCountDecSumRecordStringPayload () : Result<unit, string> =
    let recordType = AST.TRecord ("X64ListRcSumRecordString", [])
    let records = Map.ofList [("X64ListRcSumRecordString", [("value", AST.TString)])]
    let sumType = AST.TSum ("X64ListRcSumRecordWrapper", [recordType])
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some recordType)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 8)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Reg (LIR.Physical LIR.X4), Some sumType)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.TaggedList, Some (AST.TList sumType))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum record payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases boxed sum record payloads with mixed managed fields.
let testTaggedListRefCountDecSumRecord3StringListDictPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let recordType = AST.TRecord ("X64ListRcSumRecord3StringListDict", [])
    let records =
        Map.ofList
            [("X64ListRcSumRecord3StringListDict", [("name", AST.TString); ("items", listType); ("lookup", dictType)])]
    let sumType = AST.TSum ("X64ListRcSumRecord3StringListDictWrapper", [recordType])
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 16)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X5, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.HeapAlloc (LIR.Physical LIR.X7, 24)
                LIR.HeapStore (LIR.Physical LIR.X7, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X7, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X7, 16, LIR.Reg (LIR.Physical LIR.X6), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 16)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X7), Some recordType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 8)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Reg (LIR.Physical LIR.X19), Some sumType)
                LIR.Mov (LIR.Physical LIR.X21, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X21, LIR.Physical LIR.X20, LIR.Physical LIR.X21)
                LIR.RefCountDec (LIR.Physical LIR.X21, 0, LIR.TaggedList, Some (AST.TList sumType))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum record3 string/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases boxed sum record3 middle dynamic payloads.
let testTaggedListRefCountDecSumRecord3MiddleDynamicPayload () : Result<unit, string> =
    let recordType = AST.TRecord ("X64ListRcSumRecord3Middle", [])
    let records =
        Map.ofList
            [("X64ListRcSumRecord3Middle", [("count", AST.TInt64); ("name", AST.TString); ("size", AST.TInt64)])]
    let sumType = AST.TSum ("X64ListRcSumRecord3MiddleWrapper", [recordType])
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 24)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X3, 16, LIR.Imm 3L, None)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some recordType)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 8)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Reg (LIR.Physical LIR.X4), Some sumType)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.TaggedList, Some (AST.TList sumType))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum record3 middle payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases boxed sum record3 dynamic payload combinations.
let testTaggedListRefCountDecSumRecord3DynamicPayloadCombinations () : Result<unit, string> =
    let dynamicRegForIndex (index: int) : LIR.PhysReg =
        match index with
        | 0 -> LIR.X2
        | 1 -> LIR.X3
        | 2 -> LIR.X4
        | _ -> Crash.crash $"Unexpected sum record3 field index {index}"

    let isDynamicField (fieldType: AST.Type) : bool =
        match fieldType with
        | AST.TString
        | AST.TBytes -> true
        | _ -> false

    let runCase (name: string, fields: AST.Type list) : Result<unit, string> =
        let recordName = $"X64ListRcSumRecord3{name}"
        let recordType = AST.TRecord (recordName, [])
        let records =
            fields
            |> List.mapi (fun index fieldType -> ($"field{index}", fieldType))
            |> fun recordFields -> Map.ofList [(recordName, recordFields)]
        let sumType = AST.TSum ($"{recordName}Wrapper", [recordType])
        let dynamicAllocs =
            fields
            |> List.mapi (fun index fieldType ->
                if isDynamicField fieldType then
                    let reg = dynamicRegForIndex index
                    Some (LIR.StringConcat (LIR.Physical reg, LIR.StringSymbol $"left{name}{index}", LIR.StringSymbol $"right{name}{index}"))
                else
                    None)
            |> List.choose id
        let fieldStores =
            fields
            |> List.mapi (fun index fieldType ->
                let offset = index * 8
                if isDynamicField fieldType then
                    LIR.HeapStore (LIR.Physical LIR.X5, offset, LIR.Reg (LIR.Physical (dynamicRegForIndex index)), Some fieldType)
                else
                    LIR.HeapStore (LIR.Physical LIR.X5, offset, LIR.Imm (int64 (index + 1)), None))
        let program =
            makeSimpleProgramWithRecords
                (dynamicAllocs
                 @ [LIR.HeapAlloc (LIR.Physical LIR.X5, 24)]
                 @ fieldStores
                 @ [LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                    LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 0L, None)
                    LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Reg (LIR.Physical LIR.X5), Some recordType)
                    LIR.HeapAlloc (LIR.Physical LIR.X7, 8)
                    LIR.HeapStore (LIR.Physical LIR.X7, 0, LIR.Reg (LIR.Physical LIR.X6), Some sumType)
                    LIR.Mov (LIR.Physical LIR.X8, LIR.Imm 5L)
                    LIR.Orr (LIR.Physical LIR.X8, LIR.Physical LIR.X7, LIR.Physical LIR.X8)
                    LIR.RefCountDec (LIR.Physical LIR.X8, 0, LIR.TaggedList, Some (AST.TList sumType))])
                LIR.Ret
                records

        match runLIRProgramFullWithOptions program true with
        | Error e -> Error e
        | Ok (_, _, stderr) ->
            if stderr.Trim() = "" then Ok ()
            else Error $"Expected list sum record3 {name} dynamic payload release to balance leak counter, got stderr '{stderr.Trim()}'"

    let rec runCases (cases: (string * AST.Type list) list) : Result<unit, string> =
        match cases with
        | [] -> Ok ()
        | case :: rest ->
            match runCase case with
            | Ok () -> runCases rest
            | Error e -> Error e

    runCases
        [ ("First", [AST.TString; AST.TInt64; AST.TInt64])
          ("Third", [AST.TInt64; AST.TInt64; AST.TString])
          ("FirstSecond", [AST.TString; AST.TBytes; AST.TInt64])
          ("SecondThird", [AST.TInt64; AST.TString; AST.TBytes])
          ("FirstThird", [AST.TString; AST.TInt64; AST.TBytes])
          ("All", [AST.TString; AST.TBytes; AST.TString]) ]

/// Test: x64 tagged-list RefCountDec releases nested boxed sum leaf payloads.
let testTaggedListRefCountDecNestedSumStringPayload () : Result<unit, string> =
    let innerSumType = AST.TSum ("X64ListRcNestedInnerSum", [AST.TString])
    let outerSumType = AST.TSum ("X64ListRcNestedOuterSum", [innerSumType])
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 16)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some innerSumType)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 8)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Reg (LIR.Physical LIR.X4), Some outerSumType)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.TaggedList, Some (AST.TList outerSumType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list nested sum payload release to balance leak counter, got stderr '{stderr.Trim()}'"

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
    ("LIR HeapAlloc increments leak counter", testHeapAllocIncrementsLeakCounter)
    ("LIR generic RefCountInc increments refcount", testGenericRefCountInc)
    ("LIR generic RefCountDec reclaims tuple2", testGenericRefCountDecTuple2)
    ("LIR generic RefCountDec reclaims fixed sizes", testGenericRefCountDecFixedSizes)
    ("LIR dynamic string RefCountDec reclaims concat", testDynamicStringRefCountDec)
    ("LIR generic RefCountDec releases string field", testGenericRefCountDecStringField)
    ("LIR generic RefCountDec releases bytes field", testGenericRefCountDecBytesField)
    ("LIR generic RefCountDec releases nested string tuple field", testGenericRefCountDecNestedStringTupleField)
    ("LIR generic RefCountDec releases record string field", testGenericRefCountDecRecordStringField)
    ("LIR generic RefCountDec releases record dict field", testGenericRefCountDecRecordDictField)
    ("LIR generic RefCountDec releases record closure field", testGenericRefCountDecRecordClosureField)
    ("LIR generic RefCountDec releases sum string payload", testGenericRefCountDecSumStringPayload)
    ("LIR generic RefCountDec releases sum bytes payload", testGenericRefCountDecSumBytesPayload)
    ("LIR generic RefCountDec releases nested sum string field", testGenericRefCountDecNestedSumStringField)
    ("LIR generic RefCountDec releases sum list payload", testGenericRefCountDecSumListPayload)
    ("LIR generic RefCountDec releases sum dict payload", testGenericRefCountDecSumDictPayload)
    ("LIR generic RefCountDec releases dict field", testGenericRefCountDecDictField)
    ("LIR generic RefCountDec preserves live RAX across list field release", testGenericRefCountDecPreservesLiveRaxAcrossListFieldRelease)
    ("LIR generic RefCountDec preserves live RAX across dict field release", testGenericRefCountDecPreservesLiveRaxAcrossDictFieldRelease)
    ("LIR generic RefCountDec preserves live RAX across closure field release", testGenericRefCountDecPreservesLiveRaxAcrossClosureFieldRelease)
    ("LIR closure alloc RefCountDec balances leak counter", testClosureAllocRefCountDecBalancesLeakCounter)
    ("LIR generic RefCountDec releases closure field", testGenericRefCountDecClosureField)
    ("LIR generic RefCountDec releases sum closure payload", testGenericRefCountDecSumClosurePayload)
    ("LIR closure RefCountDec releases string capture", testClosureRefCountDecStringCapture)
    ("LIR closure RefCountDec releases bytes capture", testClosureRefCountDecBytesCapture)
    ("LIR closure RefCountDec releases list capture", testClosureRefCountDecListCapture)
    ("LIR closure RefCountDec releases dict capture", testClosureRefCountDecDictCapture)
    ("LIR closure RefCountDec releases closure capture", testClosureRefCountDecClosureCapture)
    ("LIR closure RefCountDec releases tuple string capture", testClosureRefCountDecTupleStringCapture)
    ("LIR closure RefCountDec releases tuple string/list/dict capture", testClosureRefCountDecTupleStringListDictCapture)
    ("LIR closure RefCountDec releases record string capture", testClosureRefCountDecRecordStringCapture)
    ("LIR closure RefCountDec releases record string/list/dict capture", testClosureRefCountDecRecordStringListDictCapture)
    ("LIR closure RefCountDec releases sum string capture", testClosureRefCountDecSumStringCapture)
    ("LIR closure RefCountDec releases sum tuple string/list/dict capture", testClosureRefCountDecSumTupleStringListDictCapture)
    ("LIR closure RefCountDec releases multiple captures", testClosureRefCountDecMultipleCaptures)
    ("LIR tagged list RefCountDec releases closure payload", testTaggedListRefCountDecClosurePayload)
    ("LIR tagged list RefCountDec releases dict payload", testTaggedListRefCountDecDictPayload)
    ("LIR tagged list RefCountDec releases string payload", testTaggedListRefCountDecStringPayload)
    ("LIR tagged list RefCountDec releases tuple string payload", testTaggedListRefCountDecTupleStringPayload)
    ("LIR tagged list RefCountDec releases tuple3 dynamic payload", testTaggedListRefCountDecTuple3DynamicPayload)
    ("LIR tagged list RefCountDec releases tuple3 middle dynamic payload", testTaggedListRefCountDecTuple3MiddleDynamicPayload)
    ("LIR tagged list RefCountDec releases tuple3 dynamic payload combinations", testTaggedListRefCountDecTuple3DynamicPayloadCombinations)
    ("LIR tagged list RefCountDec releases tuple3 string/list/dict payload", testTaggedListRefCountDecTuple3StringListDictPayload)
    ("LIR tagged list RefCountDec releases record string payload", testTaggedListRefCountDecRecordStringPayload)
    ("LIR tagged list RefCountDec releases record3 dynamic payload", testTaggedListRefCountDecRecord3DynamicPayload)
    ("LIR tagged list RefCountDec releases record3 string/list/dict payload", testTaggedListRefCountDecRecord3StringListDictPayload)
    ("LIR tagged list RefCountDec releases record3 bytes/list/dict payload", testTaggedListRefCountDecRecord3BytesListDictPayload)
    ("LIR tagged list RefCountDec releases record3 middle dynamic payload", testTaggedListRefCountDecRecord3MiddleDynamicPayload)
    ("LIR tagged list RefCountDec releases record3 dynamic payload combinations", testTaggedListRefCountDecRecord3DynamicPayloadCombinations)
    ("LIR tagged list RefCountDec releases sum string payload", testTaggedListRefCountDecSumStringPayload)
    ("LIR tagged list RefCountDec releases sum list payload", testTaggedListRefCountDecSumListPayload)
    ("LIR tagged list RefCountDec releases sum dict payload", testTaggedListRefCountDecSumDictPayload)
    ("LIR tagged list RefCountDec releases sum closure payload", testTaggedListRefCountDecSumClosurePayload)
    ("LIR tagged list RefCountDec releases sum tuple string payload", testTaggedListRefCountDecSumTupleStringPayload)
    ("LIR tagged list RefCountDec releases sum tuple2 dynamic payload combinations", testTaggedListRefCountDecSumTuple2DynamicPayloadCombinations)
    ("LIR tagged list RefCountDec releases sum tuple3 dynamic payload combinations", testTaggedListRefCountDecSumTuple3DynamicPayloadCombinations)
    ("LIR tagged list RefCountDec releases sum tuple3 string/list/dict payload", testTaggedListRefCountDecSumTuple3StringListDictPayload)
    ("LIR tagged list RefCountDec releases sum record string payload", testTaggedListRefCountDecSumRecordStringPayload)
    ("LIR tagged list RefCountDec releases sum record3 string/list/dict payload", testTaggedListRefCountDecSumRecord3StringListDictPayload)
    ("LIR tagged list RefCountDec releases sum record3 middle dynamic payload", testTaggedListRefCountDecSumRecord3MiddleDynamicPayload)
    ("LIR tagged list RefCountDec releases sum record3 dynamic payload combinations", testTaggedListRefCountDecSumRecord3DynamicPayloadCombinations)
    ("LIR tagged list RefCountDec releases nested sum string payload", testTaggedListRefCountDecNestedSumStringPayload)
]
