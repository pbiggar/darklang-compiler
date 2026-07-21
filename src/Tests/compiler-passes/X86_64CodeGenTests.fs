// X86_64CodeGenTests.fs - Tests for x86-64 code generation from LIR
//
// Verifies that LIR programs translate to working x86-64 executables.

module X86_64CodeGenTests

let private mergeFixtureVariantRegistries (left: LIR.VariantRegistry) (right: LIR.VariantRegistry) : LIR.VariantRegistry =
    Map.fold
        (fun acc typeName variants ->
            match Map.tryFind typeName acc with
            | None ->
                Map.add typeName variants acc
            | Some existing when existing = variants ->
                acc
            | Some _ ->
                Crash.crash $"Conflicting inferred test variant metadata for {typeName}")
        left
        right

let rec private inferFixtureVariantsFromType (typ: AST.Type) : LIR.VariantRegistry =
    match typ with
    | AST.TSum (name, typeArgs) ->
        let self =
            match typeArgs with
            | [] ->
                let variants : LIR.TypeVariants =
                    { TypeParams = []
                      Variants = [{ Name = $"{name}_case"; Tag = 0; Payload = None }] }
                Map.ofList [
                    (name, variants)
                ]
            | [_] ->
                let variants : LIR.TypeVariants =
                    { TypeParams = ["a"]
                      Variants = [{ Name = $"{name}_payload"; Tag = 0; Payload = Some (AST.TVar "a") }] }
                Map.ofList [
                    (name, variants)
                ]
            | _ ->
                Crash.crash $"Cannot infer test variant metadata for multi-argument sum {name}"

        typeArgs
        |> List.map inferFixtureVariantsFromType
        |> List.fold mergeFixtureVariantRegistries self
    | AST.TTuple fields ->
        fields
        |> List.map inferFixtureVariantsFromType
        |> List.fold mergeFixtureVariantRegistries Map.empty
    | AST.TRecord (_, typeArgs) ->
        typeArgs
        |> List.map inferFixtureVariantsFromType
        |> List.fold mergeFixtureVariantRegistries Map.empty
    | AST.TList elemType ->
        inferFixtureVariantsFromType elemType
    | AST.TDict (keyType, valueType) ->
        mergeFixtureVariantRegistries (inferFixtureVariantsFromType keyType) (inferFixtureVariantsFromType valueType)
    | AST.TFunction (paramTypes, returnType) ->
        returnType :: paramTypes
        |> List.map inferFixtureVariantsFromType
        |> List.fold mergeFixtureVariantRegistries Map.empty
    | AST.TInt8
    | AST.TInt16
    | AST.TInt32
    | AST.TInt64
    | AST.TInt128
    | AST.TUInt8
    | AST.TUInt16
    | AST.TUInt32
    | AST.TUInt64
    | AST.TUInt128
    | AST.TBool
    | AST.TFloat64
    | AST.TString
    | AST.TBytes
    | AST.TChar
    | AST.TUnit
    | AST.TRawPtr
    | AST.TRuntimeError
    | AST.TVar _ ->
        Map.empty

let private inferFixtureVariantsFromRcMetadata (metadata: ANF.RcMetadata option) : LIR.VariantRegistry =
    metadata
    |> Option.bind (fun rcMetadata -> rcMetadata.SourceType)
    |> Option.map inferFixtureVariantsFromType
    |> Option.defaultValue Map.empty

let private inferFixtureVariantsFromInstr (instr: LIR.Instr) : LIR.VariantRegistry =
    match instr with
    | LIR.Phi (_, _, Some typ)
    | LIR.HeapStore (_, _, _, Some typ)
    | LIR.RawSlotInit (_, _, _, typ) ->
        inferFixtureVariantsFromType typ
    | LIR.RefCountInc (_, _, _, metadata)
    | LIR.RefCountDec (_, _, _, metadata) ->
        inferFixtureVariantsFromRcMetadata metadata
    | LIR.PrintList (_, elemType) ->
        inferFixtureVariantsFromType elemType
    | LIR.PrintSum (_, variants) ->
        variants
        |> List.choose (fun (_, _, payload) -> payload)
        |> List.map inferFixtureVariantsFromType
        |> List.fold mergeFixtureVariantRegistries Map.empty
    | LIR.PrintRecord (_, _, fields) ->
        fields
        |> List.map (fun (_, fieldType) -> inferFixtureVariantsFromType fieldType)
        |> List.fold mergeFixtureVariantRegistries Map.empty
    | _ ->
        Map.empty

let private inferFixtureVariantsFromFunction (func: LIR.Function) : LIR.VariantRegistry =
    let paramVariants =
        func.TypedParams
        |> List.map (fun param -> inferFixtureVariantsFromType param.Type)
        |> List.fold mergeFixtureVariantRegistries Map.empty

    func.CFG.Blocks
    |> Map.toList
    |> List.collect (fun (_, block) -> block.Instrs)
    |> List.map inferFixtureVariantsFromInstr
    |> List.fold mergeFixtureVariantRegistries paramVariants

let private completeFixtureVariants (program: LIR.Program) : LIR.Program =
    let mergeInferred explicit inferred =
        Map.fold
            (fun acc typeName variants ->
                match Map.tryFind typeName acc with
                | Some _ ->
                    acc
                | None ->
                    Map.add typeName variants acc)
            explicit
            inferred

    match program with
    | LIR.Program (functions, variants, records) ->
        let inferred =
            functions
            |> List.map inferFixtureVariantsFromFunction
            |> List.fold mergeInferred Map.empty

        LIR.Program (functions, mergeInferred variants inferred, records)

/// Build and run a LIR program, returning exit code, stdout, and stderr.
let private runLIRProgramFullWithOptions (program: LIR.Program) (enableLeakCheck: bool) : Result<int * string * string, string> =
    match CodeGen_X86_64.translateProgram (completeFixtureVariants program) enableLeakCheck with
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
    match CodeGen_X86_64.translateProgram (completeFixtureVariants program) false with
    | Error e -> Error $"Codegen error: {e}"
    | Ok instrs ->
        match X86_64_Resolve.resolveAndEncode instrs with
        | Error e -> Error $"Resolve error: {e}"
        | Ok resolveResult ->
            let binary =
                Binary_Generation_ELF_X86_64.createExecutableWithPools
                    resolveResult.MachineCode LiteralPool.emptyStringPool LiteralPool.emptyFloatPool false 0
            X86_64BinaryTests.runElfBinary binary

let private generatedCallLabels (program: LIR.Program) : Result<string list, string> =
    match CodeGen_X86_64.translateProgram (completeFixtureVariants program) false with
    | Error e -> Error $"Codegen error: {e}"
    | Ok instrs ->
        instrs
        |> List.choose (function
            | X86_64.CALL label -> Some label
            | _ -> None)
        |> Ok

let private assertCallsPlannedListHelper (context: string) (program: LIR.Program) : Result<unit, string> =
    match generatedCallLabels program with
    | Error e ->
        Error e
    | Ok labels ->
        if labels |> List.exists (fun label -> label.StartsWith("__dark_list_rc_dec_plan_")) then
            Ok ()
        else
            Error $"{context} did not call a planned list helper; calls were {labels}"

let private assertCallsPlannedDictHelper (context: string) (program: LIR.Program) : Result<unit, string> =
    match generatedCallLabels program with
    | Error e ->
        Error e
    | Ok labels ->
        if labels |> List.exists (fun label -> label.StartsWith("__dark_dict_rc_dec_plan_")) then
            Ok ()
        else
            Error $"{context} did not call a planned dict helper; calls were {labels}"

let private rcMetadata (typ: AST.Type) : ANF.RcMetadata =
    {
        ANF.ReleasePlan = None
        ANF.SourceType = Some typ
    }

let private rcMetadataWithSumShapes (sumShapes: ANF.RcSumShapeRegistry) (typ: AST.Type) : ANF.RcMetadata =
    {
        ANF.ReleasePlan = Some (ANF.rcReleasePlanOfTypeWithSums Map.empty sumShapes typ)
        ANF.SourceType = Some typ
    }

let private completeRcMetadata (records: LIR.RecordRegistry) (metadata: ANF.RcMetadata option) : ANF.RcMetadata option =
    match metadata with
    | Some ({ ReleasePlan = None; SourceType = Some sourceType } as value) ->
        Some { value with ReleasePlan = Some (ANF.rcReleasePlanOfType records sourceType) }
    | _ ->
        metadata

let private completeRcInstrMetadata (records: LIR.RecordRegistry) (instr: LIR.Instr) : LIR.Instr =
    match instr with
    | LIR.RefCountInc (addr, payloadSize, kind, metadata) ->
        LIR.RefCountInc (addr, payloadSize, kind, completeRcMetadata records metadata)
    | LIR.RefCountDec (addr, payloadSize, kind, metadata) ->
        LIR.RefCountDec (addr, payloadSize, kind, completeRcMetadata records metadata)
    | _ ->
        instr

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
        Instrs = instrs |> List.map (completeRcInstrMetadata records)
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
    LIR.Program ([func], Map.empty, records)

let private makeSimpleProgram (instrs: LIR.Instr list) (term: LIR.Terminator) : LIR.Program =
    makeSimpleProgramWithRecords instrs term Map.empty

let private runInNamedFunction (name: string) (instrs: LIR.Instr list) (term: LIR.Terminator) : LIR.Program =
    match makeSimpleProgram [LIR.Call (LIR.Physical LIR.X0, name, [])] LIR.Ret with
    | LIR.Program ([entryFunc], variants, records) ->
        let calleeLabel = LIR.Label $"{name}_entry"
        let callee : LIR.Function = {
            Name = name
            TypedParams = []
            CFG = {
                Entry = calleeLabel
                Blocks =
                    Map.ofList [
                        (calleeLabel, { Label = calleeLabel; Instrs = instrs |> List.map (completeRcInstrMetadata records); Terminator = term })
                    ]
            }
            StackSize = 0
            UsedCalleeSaved = []
        }
        LIR.Program ([entryFunc; callee], variants, records)
    | _ ->
        Crash.crash "Test fixture expected a single entry function"

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

/// Test: malformed x64 CFGs should be reported as codegen errors rather than throwing Map.find.
let testReportsMissingEntryBlock () : Result<unit, string> =
    let entryLabel = LIR.Label "_start_entry"
    let bodyLabel = LIR.Label "_start_body"
    let bodyBlock : LIR.BasicBlock = {
        Label = bodyLabel
        Instrs = []
        Terminator = LIR.Ret
    }
    let func : LIR.Function = {
        Name = "_start"
        TypedParams = []
        CFG = {
            Entry = entryLabel
            Blocks = Map.ofList [(bodyLabel, bodyBlock)]
        }
        StackSize = 0
        UsedCalleeSaved = []
    }
    let program = LIR.Program ([func], Map.empty, Map.empty)

    match CodeGen_X86_64.translateProgram (completeFixtureVariants program) false with
    | Error e when e.Contains "missing entry block" -> Ok ()
    | Error e -> Error $"Expected missing entry block error, got '{e}'"
    | Ok _ -> Error "Expected x64 codegen to reject a CFG whose entry block is absent"

/// Test: x64 codegen rejects ARM64-only/overflow physical registers instead of aliasing runtime state.
let testRejectsReservedOverflowPhysicalRegister () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [
                LIR.Mov (LIR.Physical LIR.X24, LIR.Imm 1L)
            ]
            LIR.Ret

    match CodeGen_X86_64.translateProgram (completeFixtureVariants program) false with
    | Error e when e.Contains "X24" -> Ok ()
    | Error e -> Error $"Expected X24-specific codegen error, got '{e}'"
    | Ok _ -> Error "Expected x64 codegen to reject X24, but translation succeeded"

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
    let program = LIR.Program ([func], Map.empty, Map.empty)
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

/// Test: x64 materialized string literals use a sentinel refcount and are not released as dynamic buffers.
let testMaterializedStringLiteralRefCountDecSkipsRelease () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [
                LIR.Mov (LIR.Physical LIR.X2, LIR.StringSymbol "literal")
                LIR.RefCountDecString (LIR.Reg (LIR.Physical LIR.X2))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected materialized literal RefCountDec to skip leak accounting, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases a dynamic string field.
let testGenericRefCountDecStringField () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "a", LIR.StringSymbol "b")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.RefCountDec (LIR.Physical LIR.X3, 8, LIR.GenericHeap, Some (rcMetadata ((AST.TTuple [AST.TString]))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected fixed-block string field release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec skips materialized literal string fields.
let testGenericRefCountDecLiteralStringFieldSkipsRelease () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 8)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.StringSymbol "literal", Some AST.TString)
                LIR.RefCountDec (LIR.Physical LIR.X2, 8, LIR.GenericHeap, Some (rcMetadata ((AST.TTuple [AST.TString]))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected literal string field release to skip leak accounting, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases a dynamic bytes field.
let testGenericRefCountDecBytesField () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "a", LIR.StringSymbol "b")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TBytes)
                LIR.RefCountDec (LIR.Physical LIR.X3, 8, LIR.GenericHeap, Some (rcMetadata ((AST.TTuple [AST.TBytes]))))
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
                LIR.RefCountDec (LIR.Physical LIR.X4, 8, LIR.GenericHeap, Some (rcMetadata ((AST.TTuple [AST.TTuple [AST.TString]]))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected nested tuple field release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases tuple fields with multiple nested managed values.
let testGenericRefCountDecTupleStringListDictFields () : Result<unit, string> =
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
                LIR.HeapAlloc (LIR.Physical LIR.X19, 24)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X6), Some dictType)
                LIR.RefCountDec (LIR.Physical LIR.X19, 24, LIR.GenericHeap, Some (rcMetadata (tupleType)))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected tuple string/list/dict field release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec uses dict field release metadata for managed dict values.
let testGenericRefCountDecDictListValueField () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let tupleType = AST.TTuple [dictType]
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 8)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X3, LIR.Physical LIR.X2, LIR.Physical LIR.X3)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some listType)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 8)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Reg (LIR.Physical LIR.X5), Some dictType)
                LIR.RefCountDec (LIR.Physical LIR.X6, 8, LIR.GenericHeap, Some (rcMetadata tupleType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected tuple dict field list values to be released, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec does not release pure enum fields.
let testGenericRefCountDecSkipsPureEnumField () : Result<unit, string> =
    let pureEnumType = AST.TSum ("X64PureEnum", [])
    let tupleType = AST.TTuple [pureEnumType]
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 8)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 1L, Some pureEnumType)
                LIR.RefCountDec (LIR.Physical LIR.X2, 8, LIR.GenericHeap, Some (rcMetadata (tupleType)))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected pure enum field release to skip heap cleanup, got stderr '{stderr.Trim()}'"

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
                LIR.RefCountDec (LIR.Physical LIR.X3, 8, LIR.GenericHeap, Some (rcMetadata (recordType)))
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
                LIR.RefCountDec (LIR.Physical LIR.X4, 8, LIR.GenericHeap, Some (rcMetadata (recordType)))
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
                LIR.RefCountDec (LIR.Physical LIR.X3, 8, LIR.GenericHeap, Some (rcMetadata (recordType)))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected record closure field release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases record fields with multiple nested managed values.
let testGenericRefCountDecRecordStringListDictFields () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let recordType = AST.TRecord ("X64RcRecordStringListDict", [])
    let records =
        Map.ofList
            [("X64RcRecordStringListDict", [("name", AST.TString); ("items", listType); ("lookup", dictType)])]
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
                LIR.HeapAlloc (LIR.Physical LIR.X19, 24)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X6), Some dictType)
                LIR.RefCountDec (LIR.Physical LIR.X19, 24, LIR.GenericHeap, Some (rcMetadata (recordType)))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected record string/list/dict field release to balance leak counter, got stderr '{stderr.Trim()}'"

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
                LIR.RefCountDec (LIR.Physical LIR.X3, 16, LIR.GenericHeap, Some (rcMetadata (sumType)))
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
                LIR.RefCountDec (LIR.Physical LIR.X3, 16, LIR.GenericHeap, Some (rcMetadata (sumType)))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected boxed sum bytes payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic boxed-sum RefCountDec dispatches mixed payload cleanup by tag.
let testGenericRefCountDecMixedSumPayloadUsesVariantDispatch () : Result<unit, string> =
    let sumName = "X64MixedSumPayloadDispatch"
    let sumType = AST.TSum (sumName, [])
    let variants : LIR.VariantRegistry =
        Map.ofList [
            (sumName,
                { TypeParams = []
                  Variants =
                    [
                        { Name = "X64MixedSumBytesPayload"; Tag = 0; Payload = Some AST.TBytes }
                        { Name = "X64MixedSumListPayload"; Tag = 1; Payload = Some (AST.TList AST.TInt64) }
                    ] })
        ]
    let sumShapes =
        variants
        |> Map.map (fun _ typeVariants ->
            { ANF.TypeParams = typeVariants.TypeParams
              ANF.Payloads =
                typeVariants.Variants
                |> List.sortBy (fun variant -> variant.Tag)
                |> List.map (fun variant -> variant.Tag, variant.Payload) })
    let program =
        match
            makeSimpleProgram
                [
                    LIR.RefCountDec (
                        LIR.Physical LIR.X3,
                        16,
                        LIR.GenericHeap,
                        Some (rcMetadataWithSumShapes sumShapes sumType))
                ]
                LIR.Ret
        with
        | LIR.Program (functions, _, records) ->
            LIR.Program (functions, variants, records)

    match CodeGen_X86_64.translateProgram (completeFixtureVariants program) false with
    | Error e ->
        Error e
    | Ok instrs ->
        let rec branchAppearsBeforeSecondCase (seenFirstCase: bool) (remaining: X86_64.Instr list) : bool =
            match remaining with
            | [] ->
                false
            | X86_64.CMP_imm (_, 0) :: rest ->
                branchAppearsBeforeSecondCase true rest
            | X86_64.CMP_imm (_, 1) :: _ when seenFirstCase ->
                false
            | X86_64.JMP _ :: _ when seenFirstCase ->
                true
            | _ :: rest ->
                branchAppearsBeforeSecondCase seenFirstCase rest

        if branchAppearsBeforeSecondCase false instrs then
            Ok ()
        else
            Error "x64 generic mixed boxed-sum payload release did not branch past remaining variant cases after a match"

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
                LIR.RefCountDec (LIR.Physical LIR.X4, 8, LIR.GenericHeap, Some (rcMetadata ((AST.TTuple [sumType]))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected nested boxed sum field release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec dispatches nested mixed boxed-sum cleanup by tag.
let testGenericRefCountDecNestedMixedSumPayloadUsesVariantDispatch () : Result<unit, string> =
    let sumName = "X64NestedMixedSumPayloadDispatch"
    let sumType = AST.TSum (sumName, [])
    let parentType = AST.TTuple [sumType]
    let variants : LIR.VariantRegistry =
        Map.ofList [
            (sumName,
                { TypeParams = []
                  Variants =
                    [
                        { Name = "X64NestedMixedSumNoPayload"; Tag = 0; Payload = None }
                        { Name = "X64NestedMixedSumBytesPayload"; Tag = 1; Payload = Some AST.TBytes }
                    ] })
        ]
    let sumShapes =
        variants
        |> Map.map (fun _ typeVariants ->
            { ANF.TypeParams = typeVariants.TypeParams
              ANF.Payloads =
                typeVariants.Variants
                |> List.sortBy (fun variant -> variant.Tag)
                |> List.map (fun variant -> variant.Tag, variant.Payload) })
    let program =
        match
            makeSimpleProgram
                [
                    LIR.RefCountDec (
                        LIR.Physical LIR.X3,
                        8,
                        LIR.GenericHeap,
                        Some (rcMetadataWithSumShapes sumShapes parentType))
                ]
                LIR.Ret
        with
        | LIR.Program (functions, _, records) ->
            LIR.Program (functions, variants, records)

    match CodeGen_X86_64.translateProgram (completeFixtureVariants program) false with
    | Error e ->
        Error e
    | Ok instrs ->
        let loadsNestedSumTag =
            instrs
            |> List.exists (function
                | X86_64.MOV_load (X86_64.R10, X86_64.RDX, 0) ->
                    true
                | _ ->
                    false)

        if loadsNestedSumTag then
            Ok ()
        else
            Error "x64 generic fixed-block nested mixed boxed-sum payload release did not dispatch on the child variant tag"

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
                LIR.RefCountDec (LIR.Physical LIR.X4, 16, LIR.GenericHeap, Some (rcMetadata (sumType)))
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
                LIR.RefCountDec (LIR.Physical LIR.X4, 16, LIR.GenericHeap, Some (rcMetadata (sumType)))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected boxed sum dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases boxed sum tuple payloads with nested managed fields.
let testGenericRefCountDecSumTupleStringListDictPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let tupleType = AST.TTuple [AST.TString; listType; dictType]
    let sumType = AST.TSum ("X64GenericSumTupleStringListDict", [tupleType])
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
                LIR.HeapAlloc (LIR.Physical LIR.X19, 24)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X6), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 16)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X19), Some tupleType)
                LIR.RefCountDec (LIR.Physical LIR.X20, 16, LIR.GenericHeap, Some (rcMetadata (sumType)))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected boxed sum tuple string/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 generic fixed-block RefCountDec releases boxed sum record payloads with nested managed fields.
let testGenericRefCountDecSumRecordStringListDictPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let recordType = AST.TRecord ("X64GenericSumRecordStringListDictPayload", [])
    let records =
        Map.ofList
            [("X64GenericSumRecordStringListDictPayload", [("name", AST.TString); ("items", listType); ("lookup", dictType)])]
    let sumType = AST.TSum ("X64GenericSumRecordStringListDict", [recordType])
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
                LIR.HeapAlloc (LIR.Physical LIR.X19, 24)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X6), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 16)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X19), Some recordType)
                LIR.RefCountDec (LIR.Physical LIR.X20, 16, LIR.GenericHeap, Some (rcMetadata (sumType)))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected boxed sum record string/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

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
                LIR.RefCountDec (LIR.Physical LIR.X4, 8, LIR.GenericHeap, Some (rcMetadata ((AST.TTuple [dictType]))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected dict field release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 DictHeap RefCountDec releases list leaf values through release-plan metadata.
let testDictRefCountDecListValue () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 8)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X3, LIR.Physical LIR.X2, LIR.Physical LIR.X3)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some listType)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.DictHeap, Some (rcMetadata dictType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected dict list values to be released, got stderr '{stderr.Trim()}'"

/// Test: x64 DictHeap RefCountDec releases dynamic string leaf keys through release-plan metadata.
let testDictRefCountDecStringKey () : Result<unit, string> =
    let dictType = AST.TDict (AST.TString, AST.TInt64)
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 16)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.RefCountDec (LIR.Physical LIR.X4, 0, LIR.DictHeap, Some (rcMetadata dictType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected dict string keys to be released, got stderr '{stderr.Trim()}'"

/// Test: x64 DictHeap RefCountDec releases dynamic string leaf values through release-plan metadata.
let testDictRefCountDecStringValue () : Result<unit, string> =
    let dictType = AST.TDict (AST.TInt64, AST.TString)
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 16)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.RefCountDec (LIR.Physical LIR.X4, 0, LIR.DictHeap, Some (rcMetadata dictType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected dict string values to be released, got stderr '{stderr.Trim()}'"

/// Test: x64 DictHeap RefCountDec releases dynamic string leaf keys and values.
let testDictRefCountDecStringKeyValue () : Result<unit, string> =
    let dictType = AST.TDict (AST.TString, AST.TString)
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "key", LIR.StringSymbol "!")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "value", LIR.StringSymbol "!")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TString)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.DictHeap, Some (rcMetadata dictType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected dict string keys and values to be released, got stderr '{stderr.Trim()}'"

/// Test: x64 DictHeap RefCountDec releases dynamic bytes leaf keys and values.
let testDictRefCountDecBytesKeyValue () : Result<unit, string> =
    let dictType = AST.TDict (AST.TBytes, AST.TBytes)
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "key", LIR.StringSymbol "!")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "value", LIR.StringSymbol "!")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.DictHeap, Some (rcMetadata dictType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected dict bytes keys and values to be released, got stderr '{stderr.Trim()}'"

/// Test: x64 DictHeap RefCountDec releases dynamic string keys with list leaf values.
let testDictRefCountDecStringKeyListValue () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TString, listType)
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "key", LIR.StringSymbol "!")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 16)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X5, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.DictHeap, Some (rcMetadata dictType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected dict string keys and list values to be released, got stderr '{stderr.Trim()}'"

/// Test: x64 DictHeap RefCountDec releases dynamic string keys with nested dict leaf values.
let testDictRefCountDecStringKeyDictValue () : Result<unit, string> =
    let nestedDictType = AST.TDict (AST.TInt64, AST.TInt64)
    let dictType = AST.TDict (AST.TString, nestedDictType)
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "key", LIR.StringSymbol "!")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 16)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 16)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X5, 8, LIR.Reg (LIR.Physical LIR.X4), Some nestedDictType)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.DictHeap, Some (rcMetadata dictType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected dict string keys and nested dict values to be released, got stderr '{stderr.Trim()}'"

/// Test: x64 DictHeap RefCountDec releases dynamic string keys with nested dict-list values.
let testDictRefCountDecStringKeyDictListValue () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let nestedDictType = AST.TDict (AST.TInt64, listType)
    let dictType = AST.TDict (AST.TString, nestedDictType)
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "key", LIR.StringSymbol "!")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 16)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X5, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.HeapAlloc (LIR.Physical LIR.X7, 16)
                LIR.HeapStore (LIR.Physical LIR.X7, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X7, 8, LIR.Reg (LIR.Physical LIR.X6), Some nestedDictType)
                LIR.Mov (LIR.Physical LIR.X8, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X8, LIR.Physical LIR.X7, LIR.Physical LIR.X8)
                LIR.RefCountDec (LIR.Physical LIR.X8, 0, LIR.DictHeap, Some (rcMetadata dictType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected dict string keys and nested dict-list values to be released, got stderr '{stderr.Trim()}'"

/// Test: x64 DictHeap RefCountDec releases nested dict leaf values through release-plan metadata.
let testDictRefCountDecDictValue () : Result<unit, string> =
    let innerDictType = AST.TDict (AST.TInt64, AST.TInt64)
    let outerDictType = AST.TDict (AST.TInt64, innerDictType)
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 16)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X2, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X3, LIR.Physical LIR.X2, LIR.Physical LIR.X3)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 3L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some innerDictType)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.DictHeap, Some (rcMetadata outerDictType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected nested dict values to be released, got stderr '{stderr.Trim()}'"

/// Test: x64 DictHeap RefCountDec preserves nested dict value release metadata.
let testDictRefCountDecDictListValue () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let innerDictType = AST.TDict (AST.TInt64, listType)
    let outerDictType = AST.TDict (AST.TInt64, innerDictType)
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 8)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X3, LIR.Physical LIR.X2, LIR.Physical LIR.X3)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some listType)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 3L, None)
                LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Reg (LIR.Physical LIR.X5), Some innerDictType)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.RefCountDec (LIR.Physical LIR.X7, 0, LIR.DictHeap, Some (rcMetadata outerDictType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected nested dict list values to be released, got stderr '{stderr.Trim()}'"

/// Test: x64 DictHeap RefCountDec selects a planned helper for nested dict/list payload cleanup.
let testDictRefCountDecDictListValueUsesPlannedHelper () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let innerDictType = AST.TDict (AST.TInt64, listType)
    let outerDictType = AST.TDict (AST.TInt64, innerDictType)
    let program =
        makeSimpleProgram
            [
                LIR.RefCountDec (LIR.Physical LIR.X0, 0, LIR.DictHeap, Some (rcMetadata outerDictType))
            ]
            LIR.Ret

    match generatedCallLabels program with
    | Error e -> Error e
    | Ok labels ->
        let callsPlannedDictHelper =
            labels |> List.exists (fun label -> label.StartsWith("__dark_dict_rc_dec_plan_"))
        let callsMatrixDictListHelper =
            labels |> List.exists ((=) "__dark_dict_rc_dec_dict_list_value_helper")

        if not callsPlannedDictHelper then
            Error $"Nested dict/list RefCountDec did not call a planned dict helper; calls were {labels}"
        elif callsMatrixDictListHelper then
            Error $"Nested dict/list RefCountDec still called the dict-list matrix helper; calls were {labels}"
        else
            Ok ()

/// Test: x64 tagged-list generic tuple payloads stay on planned list helpers.
let testTaggedListTuplePayloadUsesPlannedHelper () : Result<unit, string> =
    let tupleType = AST.TTuple [AST.TString; AST.TList AST.TInt64; AST.TDict (AST.TInt64, AST.TInt64)]
    let program =
        makeSimpleProgram
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadata (AST.TList tupleType)))
            ]
            LIR.Ret

    assertCallsPlannedListHelper "Tuple list payload" program

/// Test: x64 tagged-list generic record payloads stay on planned list helpers.
let testTaggedListRecordPayloadUsesPlannedHelper () : Result<unit, string> =
    let recordType = AST.TRecord ("X64PlannedListRecordPayload", [])
    let records =
        Map.ofList [
            ("X64PlannedListRecordPayload", [("name", AST.TString); ("items", AST.TList AST.TInt64)])
        ]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadata (AST.TList recordType)))
            ]
            LIR.Ret
            records

    assertCallsPlannedListHelper "Record list payload" program

/// Test: x64 DictHeap RefCountDec releases tuple leaf values with managed fields through release-plan metadata.
let testDictRefCountDecTupleStringListValue () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let tupleType = AST.TTuple [AST.TString; listType]
    let dictType = AST.TDict (AST.TInt64, tupleType)
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 16)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X5, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Reg (LIR.Physical LIR.X5), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.RefCountDec (LIR.Physical LIR.X7, 0, LIR.DictHeap, Some (rcMetadata dictType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected dict tuple string/list values to be released, got stderr '{stderr.Trim()}'"

/// Test: x64 DictHeap RefCountDec releases tuple leaf values with list and dict fields through release-plan metadata.
let testDictRefCountDecTupleStringListDictValue () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let nestedDictType = AST.TDict (AST.TInt64, AST.TInt64)
    let tupleType = AST.TTuple [AST.TString; listType; nestedDictType]
    let dictType = AST.TDict (AST.TInt64, tupleType)
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
                LIR.HeapAlloc (LIR.Physical LIR.X19, 24)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X6), Some nestedDictType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 16)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 3L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X19), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X21, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X21, LIR.Physical LIR.X20, LIR.Physical LIR.X21)
                LIR.RefCountDec (LIR.Physical LIR.X21, 0, LIR.DictHeap, Some (rcMetadata dictType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected dict tuple string/list/dict values to be released, got stderr '{stderr.Trim()}'"

/// Test: x64 DictHeap RefCountDec releases dynamic string keys with tuple leaf values.
let testDictRefCountDecStringKeyTupleStringListDictValue () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let nestedDictType = AST.TDict (AST.TInt64, AST.TInt64)
    let tupleType = AST.TTuple [AST.TString; listType; nestedDictType]
    let dictType = AST.TDict (AST.TString, tupleType)
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "key", LIR.StringSymbol "!")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X19, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X19, LIR.Physical LIR.X6, LIR.Physical LIR.X19)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 24)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Reg (LIR.Physical LIR.X3), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X20, 16, LIR.Reg (LIR.Physical LIR.X19), Some nestedDictType)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 16)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X21, 8, LIR.Reg (LIR.Physical LIR.X20), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X21, LIR.Physical LIR.X7)
                LIR.RefCountDec (LIR.Physical LIR.X7, 0, LIR.DictHeap, Some (rcMetadata dictType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected dict string keys and tuple string/list/dict values to be released, got stderr '{stderr.Trim()}'"

/// Test: x64 DictHeap RefCountDec releases every managed payload in collision nodes.
let testDictRefCountDecStringCollisionKeysAndValues () : Result<unit, string> =
    let dictType = AST.TDict (AST.TString, AST.TString)
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "key", LIR.StringSymbol "1")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "value", LIR.StringSymbol "1")
                LIR.StringConcat (LIR.Physical LIR.X4, LIR.StringSymbol "key", LIR.StringSymbol "2")
                LIR.StringConcat (LIR.Physical LIR.X5, LIR.StringSymbol "value", LIR.StringSymbol "2")
                LIR.HeapAlloc (LIR.Physical LIR.X6, 40)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 2L, None)
                LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X6, 16, LIR.Reg (LIR.Physical LIR.X3), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X6, 24, LIR.Reg (LIR.Physical LIR.X4), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X6, 32, LIR.Reg (LIR.Physical LIR.X5), Some AST.TString)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 3L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.RefCountDec (LIR.Physical LIR.X7, 0, LIR.DictHeap, Some (rcMetadata dictType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected dict collision string keys and values to be released, got stderr '{stderr.Trim()}'"

/// Test: x64 DictHeap RefCountDec releases managed string keys and recursive tuple/list leaf values.
let testDictRefCountDecStringKeyTupleListValue () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let tupleType = AST.TTuple [AST.TString; listType]
    let dictType = AST.TDict (AST.TString, tupleType)
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "key", LIR.StringSymbol "1")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "value", LIR.StringSymbol "1")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Reg (LIR.Physical LIR.X3), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapAlloc (LIR.Physical LIR.X7, 16)
                LIR.HeapStore (LIR.Physical LIR.X7, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X7, 8, LIR.Reg (LIR.Physical LIR.X6), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X8, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X8, LIR.Physical LIR.X7, LIR.Physical LIR.X8)
                LIR.RefCountDec (LIR.Physical LIR.X8, 0, LIR.DictHeap, Some (rcMetadata dictType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected dict string keys and tuple/list values to be released, got stderr '{stderr.Trim()}'"

/// Test: x64 DictHeap RefCountDec releases every managed key and recursive tuple/list value in collision nodes.
let testDictRefCountDecStringCollisionKeysAndTupleListValues () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let tupleType = AST.TTuple [AST.TString; listType]
    let dictType = AST.TDict (AST.TString, tupleType)
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "key", LIR.StringSymbol "1")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "value", LIR.StringSymbol "1")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Reg (LIR.Physical LIR.X3), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.Mov (LIR.Physical LIR.X19, LIR.Reg (LIR.Physical LIR.X6))
                LIR.Mov (LIR.Physical LIR.X20, LIR.Reg (LIR.Physical LIR.X2))

                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "key", LIR.StringSymbol "2")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "value", LIR.StringSymbol "2")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 99L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Reg (LIR.Physical LIR.X3), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Reg (LIR.Physical LIR.X5), Some listType)

                LIR.HeapAlloc (LIR.Physical LIR.X7, 40)
                LIR.HeapStore (LIR.Physical LIR.X7, 0, LIR.Imm 2L, None)
                LIR.HeapStore (LIR.Physical LIR.X7, 8, LIR.Reg (LIR.Physical LIR.X20), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X7, 16, LIR.Reg (LIR.Physical LIR.X19), Some tupleType)
                LIR.HeapStore (LIR.Physical LIR.X7, 24, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X7, 32, LIR.Reg (LIR.Physical LIR.X6), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X21, LIR.Imm 3L)
                LIR.Orr (LIR.Physical LIR.X21, LIR.Physical LIR.X7, LIR.Physical LIR.X21)
                LIR.RefCountDec (LIR.Physical LIR.X21, 0, LIR.DictHeap, Some (rcMetadata dictType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected dict collision string keys and tuple/list values to be released, got stderr '{stderr.Trim()}'"

/// Test: x64 DictHeap RefCountDec keeps recursive string-key tuple values on planned dict helpers.
let testDictRefCountDecStringKeyTupleValueUsesPlannedHelper () : Result<unit, string> =
    let dictType = AST.TDict (AST.TString, AST.TTuple [AST.TString; AST.TList AST.TInt64])
    let program =
        makeSimpleProgram
            [
                LIR.RefCountDec (LIR.Physical LIR.X0, 0, LIR.DictHeap, Some (rcMetadata dictType))
            ]
            LIR.Ret

    assertCallsPlannedDictHelper "Dict string key tuple value" program

/// Test: x64 higher-arity tuple list payloads stay on planned release helpers.
let testTaggedListTuple5PayloadUsesPlannedHelper () : Result<unit, string> =
    let tupleType =
        AST.TTuple [
            AST.TString
            AST.TBytes
            AST.TList AST.TInt64
            AST.TDict (AST.TInt64, AST.TList AST.TInt64)
            AST.TFunction ([AST.TInt64], AST.TInt64)
        ]
    let program =
        makeSimpleProgram
            [
                LIR.RefCountDec (LIR.Physical LIR.X0, 0, LIR.TaggedList, Some (rcMetadata (AST.TList tupleType)))
            ]
            LIR.Ret

    assertCallsPlannedListHelper "List tuple5 payload" program

/// Test: x64 higher-field record list payloads stay on planned release helpers.
let testTaggedListRecord5PayloadUsesPlannedHelper () : Result<unit, string> =
    let recordType = AST.TRecord ("X64PlannedListRecord5Payload", [])
    let records =
        Map.ofList [
            ("X64PlannedListRecord5Payload",
                [
                    ("name", AST.TString)
                    ("blob", AST.TBytes)
                    ("items", AST.TList AST.TInt64)
                    ("lookup", AST.TDict (AST.TInt64, AST.TList AST.TInt64))
                    ("fn", AST.TFunction ([AST.TInt64], AST.TInt64))
                ])
        ]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.RefCountDec (LIR.Physical LIR.X0, 0, LIR.TaggedList, Some (rcMetadata (AST.TList recordType)))
            ]
            LIR.Ret
            records

    assertCallsPlannedListHelper "List record5 payload" program

/// Test: x64 DictHeap RefCountDec releases boxed sum leaf values with string payloads through release-plan metadata.
let testDictRefCountDecSumStringValue () : Result<unit, string> =
    let sumType = AST.TSum ("X64DictRcSumStringValue", [AST.TString])
    let dictType = AST.TDict (AST.TInt64, sumType)
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 16)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some sumType)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.DictHeap, Some (rcMetadata dictType))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected dict sum string values to be released, got stderr '{stderr.Trim()}'"

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
                LIR.RefCountDec (LIR.Physical LIR.X4, 8, LIR.GenericHeap, Some (rcMetadata ((AST.TTuple [listType]))))
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
                LIR.RefCountDec (LIR.Physical LIR.X4, 8, LIR.GenericHeap, Some (rcMetadata ((AST.TTuple [dictType]))))
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
                LIR.RefCountDec (LIR.Physical LIR.X3, 8, LIR.GenericHeap, Some (rcMetadata ((AST.TTuple [closureType]))))
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

/// Test: x64 generic fixed-block RefCountDec preserves live RAX across dynamic string field release.
let testGenericRefCountDecPreservesLiveRaxAcrossStringFieldRelease () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.Mov (LIR.Physical LIR.X0, LIR.Imm 321L)
                LIR.RefCountDec (LIR.Physical LIR.X3, 8, LIR.GenericHeap, Some (rcMetadata ((AST.TTuple [AST.TString]))))
                LIR.PrintInt64 (LIR.Physical LIR.X0)
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, stdout, stderr) ->
        let output = stdout.Trim()
        let leaks = stderr.Trim()
        if output = "321" && leaks = "" then Ok ()
        else Error $"Expected live RAX value 321 and no leaks, got stdout '{output}' and stderr '{leaks}'"

/// Test: x64 generic fixed-block RefCountDec preserves live RAX across dynamic bytes field release.
let testGenericRefCountDecPreservesLiveRaxAcrossBytesFieldRelease () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TBytes)
                LIR.Mov (LIR.Physical LIR.X0, LIR.Imm 654L)
                LIR.RefCountDec (LIR.Physical LIR.X3, 8, LIR.GenericHeap, Some (rcMetadata ((AST.TTuple [AST.TBytes]))))
                LIR.PrintInt64 (LIR.Physical LIR.X0)
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, stdout, stderr) ->
        let output = stdout.Trim()
        let leaks = stderr.Trim()
        if output = "654" && leaks = "" then Ok ()
        else Error $"Expected live RAX value 654 and no leaks, got stdout '{output}' and stderr '{leaks}'"

/// Test: x64 generic fixed-block RefCountDec preserves live RAX across nested fixed-block field release.
let testGenericRefCountDecPreservesLiveRaxAcrossNestedFixedBlockRelease () : Result<unit, string> =
    let childType = AST.TTuple [AST.TString]
    let parentType = AST.TTuple [childType]
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X3), Some childType)
                LIR.Mov (LIR.Physical LIR.X0, LIR.Imm 987L)
                LIR.RefCountDec (LIR.Physical LIR.X4, 8, LIR.GenericHeap, Some (rcMetadata (parentType)))
                LIR.PrintInt64 (LIR.Physical LIR.X0)
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, stdout, stderr) ->
        let output = stdout.Trim()
        let leaks = stderr.Trim()
        if output = "987" && leaks = "" then Ok ()
        else Error $"Expected live RAX value 987 and no leaks, got stdout '{output}' and stderr '{leaks}'"

/// Test: x64 closure allocation and explicit release balance leak accounting.
let testClosureAllocRefCountDecBalancesLeakCounter () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let program =
        makeSimpleProgram
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.RefCountDec (LIR.Physical LIR.X2, 8, LIR.ClosureHeap, Some (rcMetadata (closureType)))
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
                LIR.RefCountDec (LIR.Physical LIR.X3, 8, LIR.GenericHeap, Some (rcMetadata ((AST.TTuple [closureType]))))
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
                LIR.RefCountDec (LIR.Physical LIR.X3, 16, LIR.GenericHeap, Some (rcMetadata (sumType)))
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
                LIR.RefCountDec (LIR.Physical LIR.X3, 16, LIR.ClosureHeap, Some (rcMetadata ((AST.TFunction ([AST.TInt64], AST.TInt64)))))
            ]
            LIR.Ret with
        | LIR.Program ([func], variants, records) -> LIR.Program ([func; capturedFunc], variants, records)
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
                LIR.RefCountDec (LIR.Physical LIR.X3, 16, LIR.ClosureHeap, Some (rcMetadata ((AST.TFunction ([AST.TInt64], AST.TInt64)))))
            ]
            LIR.Ret with
        | LIR.Program ([func], variants, records) -> LIR.Program ([func; capturedFunc], variants, records)
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
                LIR.RefCountDec (LIR.Physical LIR.X4, 16, LIR.ClosureHeap, Some (rcMetadata ((AST.TFunction ([AST.TInt64], AST.TInt64)))))
            ]
            LIR.Ret with
        | LIR.Program ([func], variants, records) -> LIR.Program ([func; capturedFunc], variants, records)
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
                LIR.RefCountDec (LIR.Physical LIR.X4, 16, LIR.ClosureHeap, Some (rcMetadata ((AST.TFunction ([AST.TInt64], AST.TInt64)))))
            ]
            LIR.Ret with
        | LIR.Program ([func], variants, records) -> LIR.Program ([func; capturedFunc], variants, records)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure dict capture release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 closure RefCountDec releases captured dict roots using value release metadata.
let testClosureRefCountDecDictListValueCapture () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let closureTupleType = AST.TTuple [AST.TInt64; dictType]
    let capturedFunc =
        makeEmptyFunction
            "x64_dict_list_value_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureTupleType }]
    let main =
        match makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 8)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X3, LIR.Physical LIR.X2, LIR.Physical LIR.X3)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some listType)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.ClosureAlloc (LIR.Physical LIR.X6, "x64_dict_list_value_capture_fn", [LIR.Reg (LIR.Physical LIR.X5)])
                LIR.RefCountDec (LIR.Physical LIR.X6, 16, LIR.ClosureHeap, Some (rcMetadata ((AST.TFunction ([AST.TInt64], AST.TInt64)))))
            ]
            LIR.Ret with
        | LIR.Program ([func], variants, records) -> LIR.Program ([func; capturedFunc], variants, records)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure dict capture list values to be released, got stderr '{stderr.Trim()}'"

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
                LIR.RefCountDec (LIR.Physical LIR.X3, 16, LIR.ClosureHeap, Some (rcMetadata (closureType)))
            ]
            LIR.Ret with
        | LIR.Program ([func], variants, records) -> LIR.Program ([func; capturedFunc], variants, records)
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
                LIR.RefCountDec (LIR.Physical LIR.X4, 16, LIR.ClosureHeap, Some (rcMetadata ((AST.TFunction ([AST.TInt64], AST.TInt64)))))
            ]
            LIR.Ret with
        | LIR.Program ([func], variants, records) -> LIR.Program ([func; capturedFunc], variants, records)
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
                LIR.RefCountDec (LIR.Physical LIR.X20, 16, LIR.ClosureHeap, Some (rcMetadata ((AST.TFunction ([AST.TInt64], AST.TInt64)))))
            ]
            LIR.Ret with
        | LIR.Program ([func], variants, records) -> LIR.Program ([func; capturedFunc], variants, records)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure tuple string/list/dict capture release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 closure RefCountDec releases captured tuples whose dict fields own list values.
let testClosureRefCountDecTupleStringBytesListDictListCapture () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let tupleType = AST.TTuple [AST.TString; AST.TBytes; listType; dictType]
    let closureTupleType = AST.TTuple [AST.TInt64; tupleType]
    let capturedFunc =
        makeEmptyFunction
            "x64_tuple_string_bytes_list_dict_list_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureTupleType }]
    let main =
        match makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "blob", LIR.StringSymbol "bytes")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 8)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 99L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 16)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X7), Some listType)
                LIR.Mov (LIR.Physical LIR.X20, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X20, LIR.Physical LIR.X19, LIR.Physical LIR.X20)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 32)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X21, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X21, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X21, 24, LIR.Reg (LIR.Physical LIR.X20), Some dictType)
                LIR.ClosureAlloc (LIR.Physical LIR.X4, "x64_tuple_string_bytes_list_dict_list_capture_fn", [LIR.Reg (LIR.Physical LIR.X21)])
                LIR.RefCountDec (LIR.Physical LIR.X4, 16, LIR.ClosureHeap, Some (rcMetadata ((AST.TFunction ([AST.TInt64], AST.TInt64)))))
            ]
            LIR.Ret with
        | LIR.Program ([func], variants, records) -> LIR.Program ([func; capturedFunc], variants, records)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure tuple string/bytes/list/dict-list capture release to balance leak counter, got stderr '{stderr.Trim()}'"

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
                LIR.RefCountDec (LIR.Physical LIR.X4, 16, LIR.ClosureHeap, Some (rcMetadata ((AST.TFunction ([AST.TInt64], AST.TInt64)))))
            ]
            LIR.Ret
            records with
        | LIR.Program ([func], variants, programRecords) -> LIR.Program ([func; capturedFunc], variants, programRecords)
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
                LIR.RefCountDec (LIR.Physical LIR.X20, 16, LIR.ClosureHeap, Some (rcMetadata ((AST.TFunction ([AST.TInt64], AST.TInt64)))))
            ]
            LIR.Ret
            records with
        | LIR.Program ([func], variants, programRecords) -> LIR.Program ([func; capturedFunc], variants, programRecords)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure record string/list/dict capture release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 closure RefCountDec releases captured records whose dict fields own list values.
let testClosureRefCountDecRecordStringBytesListDictListCapture () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let recordType = AST.TRecord ("X64ClosureCaptureRecordStringBytesListDictList", [])
    let records =
        Map.ofList
            [("X64ClosureCaptureRecordStringBytesListDictList",
              [("name", AST.TString); ("blob", AST.TBytes); ("items", listType); ("lookup", dictType)])]
    let closureTupleType = AST.TTuple [AST.TInt64; recordType]
    let capturedFunc =
        makeEmptyFunction
            "x64_record_string_bytes_list_dict_list_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureTupleType }]
    let main =
        match makeSimpleProgramWithRecords
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "blob", LIR.StringSymbol "bytes")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 8)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 99L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 16)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X7), Some listType)
                LIR.Mov (LIR.Physical LIR.X20, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X20, LIR.Physical LIR.X19, LIR.Physical LIR.X20)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 32)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X21, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X21, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X21, 24, LIR.Reg (LIR.Physical LIR.X20), Some dictType)
                LIR.ClosureAlloc (LIR.Physical LIR.X4, "x64_record_string_bytes_list_dict_list_capture_fn", [LIR.Reg (LIR.Physical LIR.X21)])
                LIR.RefCountDec (LIR.Physical LIR.X4, 16, LIR.ClosureHeap, Some (rcMetadata ((AST.TFunction ([AST.TInt64], AST.TInt64)))))
            ]
            LIR.Ret
            records with
        | LIR.Program ([func], variants, programRecords) -> LIR.Program ([func; capturedFunc], variants, programRecords)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure record string/bytes/list/dict-list capture release to balance leak counter, got stderr '{stderr.Trim()}'"

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
                LIR.RefCountDec (LIR.Physical LIR.X4, 16, LIR.ClosureHeap, Some (rcMetadata ((AST.TFunction ([AST.TInt64], AST.TInt64)))))
            ]
            LIR.Ret with
        | LIR.Program ([func], variants, records) -> LIR.Program ([func; capturedFunc], variants, records)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure sum capture release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 closure RefCountDec dispatches captured mixed boxed-sum cleanup by tag.
let testClosureRefCountDecMixedSumCaptureUsesVariantDispatch () : Result<unit, string> =
    let sumName = "X64ClosureMixedSumCaptureDispatch"
    let sumType = AST.TSum (sumName, [])
    let closureTupleType = AST.TTuple [AST.TInt64; sumType]
    let variants : LIR.VariantRegistry =
        Map.ofList [
            (sumName,
                { TypeParams = []
                  Variants =
                    [
                        { Name = "X64ClosureMixedSumNoPayload"; Tag = 0; Payload = None }
                        { Name = "X64ClosureMixedSumBytesPayload"; Tag = 1; Payload = Some AST.TBytes }
                    ] })
        ]
    let capturedFunc =
        makeEmptyFunction
            "x64_mixed_sum_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureTupleType }]
    let main =
        match
            makeSimpleProgram
                [
                    LIR.ClosureAlloc (
                        LIR.Physical LIR.X4,
                        "x64_mixed_sum_capture_fn",
                        [LIR.Reg (LIR.Physical LIR.X3)])
                    LIR.RefCountDec (
                        LIR.Physical LIR.X4,
                        16,
                        LIR.ClosureHeap,
                        Some (rcMetadata (AST.TFunction ([AST.TInt64], AST.TInt64))))
                ]
                LIR.Ret
        with
        | LIR.Program ([func], _, records) ->
            LIR.Program ([func; capturedFunc], variants, records)
        | other ->
            other

    match CodeGen_X86_64.translateProgram (completeFixtureVariants main) false with
    | Error e ->
        Error e
    | Ok instrs ->
        let emitsCapturedSumTagLoad =
            instrs
            |> List.exists (function
                | X86_64.MOV_load (X86_64.R10, X86_64.RDX, 0) ->
                    true
                | _ ->
                    false)

        if emitsCapturedSumTagLoad then
            Ok ()
        else
            Error "x64 closure mixed boxed-sum capture release did not dispatch on the captured sum variant tag"

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
                LIR.RefCountDec (LIR.Physical LIR.X21, 16, LIR.ClosureHeap, Some (rcMetadata ((AST.TFunction ([AST.TInt64], AST.TInt64)))))
            ]
            LIR.Ret with
        | LIR.Program ([func], variants, records) -> LIR.Program ([func; capturedFunc], variants, records)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure sum tuple string/list/dict capture release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 closure RefCountDec releases captured boxed sums containing records with nested managed fields.
let testClosureRefCountDecSumRecordStringListDictCapture () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let recordType = AST.TRecord ("X64ClosureCaptureSumRecordStringListDictPayload", [])
    let records =
        Map.ofList
            [("X64ClosureCaptureSumRecordStringListDictPayload", [("name", AST.TString); ("items", listType); ("lookup", dictType)])]
    let sumType = AST.TSum ("X64ClosureCaptureSumRecordStringListDict", [recordType])
    let closureTupleType = AST.TTuple [AST.TInt64; sumType]
    let capturedFunc =
        makeEmptyFunction
            "x64_sum_record_string_list_dict_capture_fn"
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
                LIR.HeapAlloc (LIR.Physical LIR.X20, 16)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X19), Some recordType)
                LIR.ClosureAlloc (LIR.Physical LIR.X21, "x64_sum_record_string_list_dict_capture_fn", [LIR.Reg (LIR.Physical LIR.X20)])
                LIR.RefCountDec (LIR.Physical LIR.X21, 16, LIR.ClosureHeap, Some (rcMetadata ((AST.TFunction ([AST.TInt64], AST.TInt64)))))
            ]
            LIR.Ret
            records with
        | LIR.Program ([func], variants, programRecords) -> LIR.Program ([func; capturedFunc], variants, programRecords)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected closure sum record string/list/dict capture release to balance leak counter, got stderr '{stderr.Trim()}'"

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
                LIR.RefCountDec (LIR.Physical LIR.X5, 24, LIR.ClosureHeap, Some (rcMetadata ((AST.TFunction ([AST.TInt64], AST.TInt64)))))
            ]
            LIR.Ret with
        | LIR.Program ([func], variants, records) -> LIR.Program ([func; capturedFunc], variants, records)
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
                LIR.RefCountDec (LIR.Physical LIR.X4, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList closureType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list closure payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases closure payloads in stdlib helper contexts.
let testTaggedListRefCountDecClosurePayloadInStdlibFunction () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let program =
        runInNamedFunction
            "Stdlib.List.__mapHelper_i64_fn_i64_acc_fn_i64"
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "Stdlib.List.__mapHelper_i64_fn_i64_acc_fn_i64", [])
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.RefCountDec (LIR.Physical LIR.X4, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList closureType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (exitCode, _, stderr) when exitCode <> 0 ->
        Error $"Expected stdlib list closure payload release to exit 0, got {exitCode}, stderr '{stderr.Trim()}'"
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected stdlib list closure payload release to balance leak counter, got stderr '{stderr.Trim()}'"

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
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList dictType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec preserves dict leaf payload value metadata.
let testTaggedListRefCountDecDictListPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 8)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X3, LIR.Physical LIR.X2, LIR.Physical LIR.X3)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some listType)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 8)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Reg (LIR.Physical LIR.X5), Some dictType)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.RefCountDec (LIR.Physical LIR.X7, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList dictType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list dict/list payload release to balance leak counter, got stderr '{stderr.Trim()}'"

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
                LIR.RefCountDec (LIR.Physical LIR.X4, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList AST.TString))))
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
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
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
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
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
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
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
                    LIR.RefCountDec (LIR.Physical LIR.X7, 0, LIR.TaggedList, Some (rcMetadata (AST.TList tupleType)))])
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
                LIR.RefCountDec (LIR.Physical LIR.X20, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple3 string/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec preserves dict field value metadata in tuple3 payloads.
let testTaggedListRefCountDecTuple3StringListDictListPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let tupleType = AST.TTuple [AST.TString; listType; dictType]
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 8)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Imm 7L, None)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.HeapAlloc (LIR.Physical LIR.X7, 16)
                LIR.HeapStore (LIR.Physical LIR.X7, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X7, 8, LIR.Reg (LIR.Physical LIR.X6), Some listType)
                LIR.Mov (LIR.Physical LIR.X19, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X19, LIR.Physical LIR.X7, LIR.Physical LIR.X19)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 24)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X20, 16, LIR.Reg (LIR.Physical LIR.X19), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 8)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X20), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X1, LIR.Physical LIR.X21, LIR.Physical LIR.X1)
                LIR.RefCountDec (LIR.Physical LIR.X1, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple3 string/list/dict-list payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases tuple3 closure/list/dict payloads.
let testTaggedListRefCountDecTuple3ClosureListDictPayload () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let tupleType = AST.TTuple [closureType; listType; dictType]
    let program =
        makeSimpleProgram
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
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
                LIR.HeapStore (LIR.Physical LIR.X7, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.HeapStore (LIR.Physical LIR.X7, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X7, 16, LIR.Reg (LIR.Physical LIR.X6), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 8)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X7), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X20, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X20, LIR.Physical LIR.X19, LIR.Physical LIR.X20)
                LIR.RefCountDec (LIR.Physical LIR.X20, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple3 closure/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec preserves dict field value metadata in closure tuple3 payloads.
let testTaggedListRefCountDecTuple3ClosureListDictListPayload () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let tupleType = AST.TTuple [closureType; listType; dictType]
    let program =
        makeSimpleProgram
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 8)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Imm 7L, None)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.HeapAlloc (LIR.Physical LIR.X7, 16)
                LIR.HeapStore (LIR.Physical LIR.X7, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X7, 8, LIR.Reg (LIR.Physical LIR.X6), Some listType)
                LIR.Mov (LIR.Physical LIR.X19, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X19, LIR.Physical LIR.X7, LIR.Physical LIR.X19)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 24)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X20, 16, LIR.Reg (LIR.Physical LIR.X19), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 8)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X20), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X1, LIR.Physical LIR.X21, LIR.Physical LIR.X1)
                LIR.RefCountDec (LIR.Physical LIR.X1, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple3 closure/list/dict-list payload release to balance leak counter, got stderr '{stderr.Trim()}'"

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
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList recordType))))
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
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList recordType))))
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
                LIR.RefCountDec (LIR.Physical LIR.X20, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList recordType))))
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
                LIR.RefCountDec (LIR.Physical LIR.X20, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList recordType))))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list record3 bytes/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases closure/list/dict fields in record3 payloads.
let testTaggedListRefCountDecRecord3ClosureListDictPayload () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let recordType = AST.TRecord ("X64ListRcRecord3ClosureListDict", [])
    let records =
        Map.ofList
            [("X64ListRcRecord3ClosureListDict", [("callback", closureType); ("items", listType); ("lookup", dictType)])]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
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
                LIR.HeapStore (LIR.Physical LIR.X7, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.HeapStore (LIR.Physical LIR.X7, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X7, 16, LIR.Reg (LIR.Physical LIR.X6), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 8)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X7), Some recordType)
                LIR.Mov (LIR.Physical LIR.X20, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X20, LIR.Physical LIR.X19, LIR.Physical LIR.X20)
                LIR.RefCountDec (LIR.Physical LIR.X20, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList recordType))))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list record3 closure/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases record4 string/bytes/list/dict payloads.
let testTaggedListRefCountDecRecord4StringBytesListDictPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let recordType = AST.TRecord ("X64ListRcRecord4StringBytesListDict", [])
    let records =
        Map.ofList
            [("X64ListRcRecord4StringBytesListDict", [("name", AST.TString); ("blob", AST.TBytes); ("items", listType); ("lookup", dictType)])]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 32)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 24, LIR.Reg (LIR.Physical LIR.X7), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 8)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Reg (LIR.Physical LIR.X19), Some recordType)
                LIR.Mov (LIR.Physical LIR.X21, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X21, LIR.Physical LIR.X20, LIR.Physical LIR.X21)
                LIR.RefCountDec (LIR.Physical LIR.X21, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList recordType))))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list record4 string/bytes/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases record4 closure/bytes/list/dict payloads.
let testTaggedListRefCountDecRecord4ClosureBytesListDictPayload () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let recordType = AST.TRecord ("X64ListRcRecord4ClosureBytesListDict", [])
    let records =
        Map.ofList
            [("X64ListRcRecord4ClosureBytesListDict", [("callback", closureType); ("blob", AST.TBytes); ("items", listType); ("lookup", dictType)])]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 32)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 24, LIR.Reg (LIR.Physical LIR.X7), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 8)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Reg (LIR.Physical LIR.X19), Some recordType)
                LIR.Mov (LIR.Physical LIR.X21, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X21, LIR.Physical LIR.X20, LIR.Physical LIR.X21)
                LIR.RefCountDec (LIR.Physical LIR.X21, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList recordType))))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list record4 closure/bytes/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases record4 nested tuple dynamic/list/dict payloads.
let testTaggedListRefCountDecRecord4NestedTupleStringListDictPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let nestedTupleType = AST.TTuple [AST.TString; listType; dictType]
    let recordType = AST.TRecord ("X64ListRcRecord4NestedTupleStringListDict", [])
    let records =
        Map.ofList
            [("X64ListRcRecord4NestedTupleStringListDict", [("a", AST.TInt64); ("b", AST.TInt64); ("c", AST.TInt64); ("nested", nestedTupleType)])]
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
                LIR.HeapAlloc (LIR.Physical LIR.X19, 32)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Imm 11L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Imm 22L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Imm 33L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 24, LIR.Reg (LIR.Physical LIR.X7), Some nestedTupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 8)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Reg (LIR.Physical LIR.X19), Some recordType)
                LIR.Mov (LIR.Physical LIR.X21, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X21, LIR.Physical LIR.X20, LIR.Physical LIR.X21)
                LIR.RefCountDec (LIR.Physical LIR.X21, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList recordType))))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list record4 nested tuple string/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec preserves dict value metadata in nested tuple payloads.
let testTaggedListRefCountDecRecord4NestedTupleStringListDictListPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let nestedTupleType = AST.TTuple [AST.TString; listType; dictType]
    let recordType = AST.TRecord ("X64ListRcRecord4NestedTupleStringListDictList", [])
    let records =
        Map.ofList
            [("X64ListRcRecord4NestedTupleStringListDictList", [("a", AST.TInt64); ("b", AST.TInt64); ("c", AST.TInt64); ("nested", nestedTupleType)])]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 8)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Imm 7L, None)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.HeapAlloc (LIR.Physical LIR.X7, 16)
                LIR.HeapStore (LIR.Physical LIR.X7, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X7, 8, LIR.Reg (LIR.Physical LIR.X6), Some listType)
                LIR.Mov (LIR.Physical LIR.X19, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X19, LIR.Physical LIR.X7, LIR.Physical LIR.X19)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 24)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X20, 16, LIR.Reg (LIR.Physical LIR.X19), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 32)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Imm 11L, None)
                LIR.HeapStore (LIR.Physical LIR.X21, 8, LIR.Imm 22L, None)
                LIR.HeapStore (LIR.Physical LIR.X21, 16, LIR.Imm 33L, None)
                LIR.HeapStore (LIR.Physical LIR.X21, 24, LIR.Reg (LIR.Physical LIR.X20), Some nestedTupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X21), Some recordType)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.RefCountDec (LIR.Physical LIR.X4, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList recordType))))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list record4 nested tuple string/list/dict-list payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases record4 nested tuple dynamic payloads.
let testTaggedListRefCountDecRecord4NestedTupleStringPayload () : Result<unit, string> =
    let nestedTupleType = AST.TTuple [AST.TString; AST.TInt64]
    let recordType = AST.TRecord ("X64ListRcRecord4NestedTupleString", [])
    let records =
        Map.ofList
            [("X64ListRcRecord4NestedTupleString", [("a", AST.TInt64); ("b", AST.TInt64); ("c", AST.TInt64); ("nested", nestedTupleType)])]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 16)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Imm 7L, None)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 32)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 11L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Imm 22L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 16, LIR.Imm 33L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 24, LIR.Reg (LIR.Physical LIR.X3), Some nestedTupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 8)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Reg (LIR.Physical LIR.X4), Some recordType)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList recordType))))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list record4 nested tuple string payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases record4 nested tuple closure/dynamic/list/dict payloads.
let testTaggedListRefCountDecRecord4NestedTupleClosureBytesListDictPayload () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let nestedTupleType = AST.TTuple [closureType; AST.TBytes; listType; dictType]
    let recordType = AST.TRecord ("X64ListRcRecord4NestedTupleClosureBytesListDict", [])
    let records =
        Map.ofList
            [("X64ListRcRecord4NestedTupleClosureBytesListDict", [("a", AST.TInt64); ("b", AST.TInt64); ("c", AST.TInt64); ("nested", nestedTupleType)])]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 32)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 24, LIR.Reg (LIR.Physical LIR.X7), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 32)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 11L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Imm 22L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 16, LIR.Imm 33L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 24, LIR.Reg (LIR.Physical LIR.X19), Some nestedTupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 8)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X20), Some recordType)
                LIR.Mov (LIR.Physical LIR.X2, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X2, LIR.Physical LIR.X21, LIR.Physical LIR.X2)
                LIR.RefCountDec (LIR.Physical LIR.X2, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList recordType))))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list record4 nested tuple closure/bytes/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec preserves dict value metadata in nested closure/dynamic/list/dict tuple payloads.
let testTaggedListRefCountDecRecord4NestedTupleClosureBytesListDictListPayload () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let nestedTupleType = AST.TTuple [closureType; AST.TBytes; listType; dictType]
    let recordType = AST.TRecord ("X64ListRcRecord4NestedTupleClosureBytesListDictList", [])
    let records =
        Map.ofList
            [("X64ListRcRecord4NestedTupleClosureBytesListDictList", [("a", AST.TInt64); ("b", AST.TInt64); ("c", AST.TInt64); ("nested", nestedTupleType)])]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 8)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 7L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 16)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X7), Some listType)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X19, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 32)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 24, LIR.Reg (LIR.Physical LIR.X7), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 32)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 11L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Imm 22L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 16, LIR.Imm 33L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 24, LIR.Reg (LIR.Physical LIR.X19), Some nestedTupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 8)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X20), Some recordType)
                LIR.Mov (LIR.Physical LIR.X2, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X2, LIR.Physical LIR.X21, LIR.Physical LIR.X2)
                LIR.RefCountDec (LIR.Physical LIR.X2, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList recordType))))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list record4 nested tuple closure/bytes/list/dict-list payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases tuple4 string/bytes/list/dict payloads.
let testTaggedListRefCountDecTuple4StringBytesListDictPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let tupleType = AST.TTuple [AST.TString; AST.TBytes; listType; dictType]
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 32)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 24, LIR.Reg (LIR.Physical LIR.X7), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 8)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Reg (LIR.Physical LIR.X19), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X21, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X21, LIR.Physical LIR.X20, LIR.Physical LIR.X21)
                LIR.RefCountDec (LIR.Physical LIR.X21, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple4 string/bytes/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec preserves dict field value metadata in tuple4 payloads.
let testTaggedListRefCountDecTuple4StringBytesListDictListPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let tupleType = AST.TTuple [AST.TString; AST.TBytes; listType; dictType]
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 8)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 7L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 16)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X7), Some listType)
                LIR.Mov (LIR.Physical LIR.X20, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X20, LIR.Physical LIR.X19, LIR.Physical LIR.X20)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 32)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X21, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X21, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X21, 24, LIR.Reg (LIR.Physical LIR.X20), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X21), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple4 string/bytes/list/dict-list payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases tuple4 closure/bytes/list/dict payloads.
let testTaggedListRefCountDecTuple4ClosureBytesListDictPayload () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let tupleType = AST.TTuple [closureType; AST.TBytes; listType; dictType]
    let program =
        makeSimpleProgram
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 32)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 24, LIR.Reg (LIR.Physical LIR.X7), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 8)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Reg (LIR.Physical LIR.X19), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X21, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X21, LIR.Physical LIR.X20, LIR.Physical LIR.X21)
                LIR.RefCountDec (LIR.Physical LIR.X21, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple4 closure/bytes/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec preserves dict field value metadata in tuple4 closure payloads.
let testTaggedListRefCountDecTuple4ClosureBytesListDictListPayload () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let tupleType = AST.TTuple [closureType; AST.TBytes; listType; dictType]
    let program =
        makeSimpleProgram
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 8)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 7L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 16)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X7), Some listType)
                LIR.Mov (LIR.Physical LIR.X20, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X20, LIR.Physical LIR.X19, LIR.Physical LIR.X20)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 32)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.HeapStore (LIR.Physical LIR.X21, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X21, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X21, 24, LIR.Reg (LIR.Physical LIR.X20), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X21), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple4 closure/bytes/list/dict-list payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases nested tuple dynamic payloads.
let testTaggedListRefCountDecTuple2NestedTupleStringPayload () : Result<unit, string> =
    let nestedTupleType = AST.TTuple [AST.TString; AST.TInt64]
    let tupleType = AST.TTuple [AST.TInt64; nestedTupleType]
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 16)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Imm 7L, None)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some nestedTupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 8)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Reg (LIR.Physical LIR.X4), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple2 nested tuple string payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases tuple4 nested tuple dynamic payloads.
let testTaggedListRefCountDecTuple4NestedTupleStringPayload () : Result<unit, string> =
    let nestedTupleType = AST.TTuple [AST.TString; AST.TInt64]
    let tupleType = AST.TTuple [AST.TInt64; AST.TInt64; AST.TInt64; nestedTupleType]
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 16)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X3, 8, LIR.Imm 7L, None)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 32)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 11L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Imm 22L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 16, LIR.Imm 33L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 24, LIR.Reg (LIR.Physical LIR.X3), Some nestedTupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 8)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Reg (LIR.Physical LIR.X4), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple4 nested tuple string payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases tuple4 nested tuple dynamic/list/dict payloads.
let testTaggedListRefCountDecTuple4NestedTupleStringListDictPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let nestedTupleType = AST.TTuple [AST.TString; listType; dictType]
    let tupleType = AST.TTuple [AST.TInt64; AST.TInt64; AST.TInt64; nestedTupleType]
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
                LIR.HeapAlloc (LIR.Physical LIR.X19, 32)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Imm 11L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Imm 22L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Imm 33L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 24, LIR.Reg (LIR.Physical LIR.X7), Some nestedTupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 8)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Reg (LIR.Physical LIR.X19), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X21, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X21, LIR.Physical LIR.X20, LIR.Physical LIR.X21)
                LIR.RefCountDec (LIR.Physical LIR.X21, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple4 nested tuple string/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases tuple4 nested record dynamic/list/dict payloads.
let testTaggedListRefCountDecTuple4NestedRecordMiddleStringListDictPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let nestedRecordType = AST.TRecord ("X64ListRcNestedRecordMiddleStringListDict", [])
    let tupleType = AST.TTuple [AST.TInt64; AST.TInt64; nestedRecordType; AST.TInt64]
    let records =
        Map.ofList
            [("X64ListRcNestedRecordMiddleStringListDict", [("name", AST.TString); ("items", listType); ("lookup", dictType)])]
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
                LIR.HeapAlloc (LIR.Physical LIR.X19, 32)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Imm 11L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Imm 22L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X7), Some nestedRecordType)
                LIR.HeapStore (LIR.Physical LIR.X19, 24, LIR.Imm 33L, None)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 8)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Reg (LIR.Physical LIR.X19), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X21, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X21, LIR.Physical LIR.X20, LIR.Physical LIR.X21)
                LIR.RefCountDec (LIR.Physical LIR.X21, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple4 nested record string/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases tuple4 nested record dict-list payloads in the middle field.
let testTaggedListRefCountDecTuple4NestedRecordMiddleStringListDictListPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let nestedRecordType = AST.TRecord ("X64ListRcNestedRecordMiddleStringListDictList", [])
    let tupleType = AST.TTuple [AST.TInt64; AST.TInt64; nestedRecordType; AST.TInt64]
    let records =
        Map.ofList
            [("X64ListRcNestedRecordMiddleStringListDictList", [("name", AST.TString); ("items", listType); ("lookup", dictType)])]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 24)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 8)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Imm 7L, None)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.HeapAlloc (LIR.Physical LIR.X7, 16)
                LIR.HeapStore (LIR.Physical LIR.X7, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X7, 8, LIR.Reg (LIR.Physical LIR.X6), Some listType)
                LIR.Mov (LIR.Physical LIR.X20, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X20, LIR.Physical LIR.X7, LIR.Physical LIR.X20)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X20), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 32)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 11L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Imm 22L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 16, LIR.Reg (LIR.Physical LIR.X19), Some nestedRecordType)
                LIR.HeapStore (LIR.Physical LIR.X20, 24, LIR.Imm 33L, None)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 8)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X20), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X1, LIR.Physical LIR.X21, LIR.Physical LIR.X1)
                LIR.RefCountDec (LIR.Physical LIR.X1, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (exitCode, _, stderr) when exitCode <> 0 ->
        Error $"Expected list tuple4 nested record string/list/dict-list payload release to exit 0, got {exitCode}"
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple4 nested record string/list/dict-list payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases tuple4 nested tuple closure/dynamic/list/dict payloads.
let testTaggedListRefCountDecTuple4NestedTupleClosureBytesListDictPayload () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let nestedTupleType = AST.TTuple [closureType; AST.TBytes; listType; dictType]
    let tupleType = AST.TTuple [AST.TInt64; AST.TInt64; AST.TInt64; nestedTupleType]
    let program =
        makeSimpleProgram
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 32)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 24, LIR.Reg (LIR.Physical LIR.X7), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 32)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 11L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Imm 22L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 16, LIR.Imm 33L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 24, LIR.Reg (LIR.Physical LIR.X19), Some nestedTupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 8)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X20), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X2, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X2, LIR.Physical LIR.X21, LIR.Physical LIR.X2)
                LIR.RefCountDec (LIR.Physical LIR.X2, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple4 nested tuple closure/bytes/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases nested tuple dynamic payloads at later offsets.
let testTaggedListRefCountDecTuple2NestedTupleDynamicPayloadCombinations () : Result<unit, string> =
    let runCase (name: string, nestedTupleType: AST.Type, setup: LIR.Instr list, nestedStores: LIR.Instr list) : Result<unit, string> =
        let tupleType = AST.TTuple [AST.TInt64; nestedTupleType]
        let program =
            makeSimpleProgram
                (setup
                 @ [LIR.HeapAlloc (LIR.Physical LIR.X4, 16)]
                 @ nestedStores
                 @ [LIR.HeapAlloc (LIR.Physical LIR.X5, 16)
                    LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Imm 42L, None)
                    LIR.HeapStore (LIR.Physical LIR.X5, 8, LIR.Reg (LIR.Physical LIR.X4), Some nestedTupleType)
                    LIR.HeapAlloc (LIR.Physical LIR.X6, 8)
                    LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Reg (LIR.Physical LIR.X5), Some tupleType)
                    LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 5L)
                    LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                    LIR.RefCountDec (LIR.Physical LIR.X7, 0, LIR.TaggedList, Some (rcMetadata (AST.TList tupleType)))])
                LIR.Ret

        match runLIRProgramFullWithOptions program true with
        | Error e -> Error e
        | Ok (_, _, stderr) ->
            if stderr.Trim() = "" then Ok ()
            else Error $"Expected list tuple2 nested tuple {name} dynamic payload release to balance leak counter, got stderr '{stderr.Trim()}'"

    let rec runCases (cases: (string * AST.Type * LIR.Instr list * LIR.Instr list) list) : Result<unit, string> =
        match cases with
        | [] -> Ok ()
        | case :: rest ->
            match runCase case with
            | Ok () -> runCases rest
            | Error e -> Error e

    runCases
        [ ("Second", AST.TTuple [AST.TInt64; AST.TString],
           [LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")],
           [LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 7L, None)
            LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)])
          ("Both", AST.TTuple [AST.TString; AST.TBytes],
           [LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
            LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")],
           [LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
            LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)]) ]

/// Test: x64 tagged-list RefCountDec releases nested tuple dynamic/list/dict payloads.
let testTaggedListRefCountDecTuple2NestedTupleStringListDictPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let nestedTupleType = AST.TTuple [AST.TString; listType; dictType]
    let tupleType = AST.TTuple [AST.TInt64; nestedTupleType]
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
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Imm 7L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X7), Some nestedTupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 8)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Reg (LIR.Physical LIR.X19), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X21, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X21, LIR.Physical LIR.X20, LIR.Physical LIR.X21)
                LIR.RefCountDec (LIR.Physical LIR.X21, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple2 nested tuple string/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases nested tuple list/dict payloads.
let testTaggedListRefCountDecTuple2NestedTupleListDictPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let nestedTupleType = AST.TTuple [listType; dictType]
    let tupleType = AST.TTuple [AST.TInt64; nestedTupleType]
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 8)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X3, LIR.Physical LIR.X2, LIR.Physical LIR.X3)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 16)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X3), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X5), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 16)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 7L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X19), Some nestedTupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 8)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X20), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X1, LIR.Physical LIR.X21, LIR.Physical LIR.X1)
                LIR.RefCountDec (LIR.Physical LIR.X1, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple2 nested tuple list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases nested tuple dict payloads.
let testTaggedListRefCountDecTuple2NestedTupleDictPayload () : Result<unit, string> =
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let nestedTupleType = AST.TTuple [dictType]
    let tupleType = AST.TTuple [AST.TInt64; nestedTupleType]
    let program =
        makeSimpleProgram
            [
                LIR.HeapAlloc (LIR.Physical LIR.X2, 16)
                LIR.HeapStore (LIR.Physical LIR.X2, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X2, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X3, LIR.Physical LIR.X2, LIR.Physical LIR.X3)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 8)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X3), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 16)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 7L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X19), Some nestedTupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 8)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X20), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X1, LIR.Physical LIR.X21, LIR.Physical LIR.X1)
                LIR.RefCountDec (LIR.Physical LIR.X1, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple2 nested tuple dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases nested tuple closure payloads.
let testTaggedListRefCountDecTuple2NestedTupleClosurePayload () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let nestedTupleType = AST.TTuple [closureType]
    let tupleType = AST.TTuple [AST.TInt64; nestedTupleType]
    let program =
        makeSimpleProgram
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.HeapAlloc (LIR.Physical LIR.X19, 8)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 16)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 7L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X19), Some nestedTupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 8)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X20), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X1, LIR.Physical LIR.X21, LIR.Physical LIR.X1)
                LIR.RefCountDec (LIR.Physical LIR.X1, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple2 nested tuple closure payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases nested tuple string/bytes/list/dict payloads.
let testTaggedListRefCountDecTuple2NestedTupleStringBytesListDictPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let nestedTupleType = AST.TTuple [AST.TString; AST.TBytes; listType; dictType]
    let tupleType = AST.TTuple [AST.TInt64; nestedTupleType]
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 32)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 16)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 8)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 24, LIR.Reg (LIR.Physical LIR.X7), Some dictType)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 7L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X19), Some nestedTupleType)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X20), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X1, LIR.Physical LIR.X21, LIR.Physical LIR.X1)
                LIR.RefCountDec (LIR.Physical LIR.X1, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple2 nested tuple string/bytes/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec preserves dict value metadata in nested string/bytes/list/dict tuple payloads.
let testTaggedListRefCountDecTuple2NestedTupleStringBytesListDictListPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let nestedTupleType = AST.TTuple [AST.TString; AST.TBytes; listType; dictType]
    let tupleType = AST.TTuple [AST.TInt64; nestedTupleType]
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 8)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 7L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 16)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X7), Some listType)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X19, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 32)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 16)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 8)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 24, LIR.Reg (LIR.Physical LIR.X7), Some dictType)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 7L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X19), Some nestedTupleType)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X20), Some tupleType)
                LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X1, LIR.Physical LIR.X21, LIR.Physical LIR.X1)
                LIR.RefCountDec (LIR.Physical LIR.X1, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList tupleType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list tuple2 nested tuple string/bytes/list/dict-list payload release to balance leak counter, got stderr '{stderr.Trim()}'"

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
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList recordType))))
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
                    LIR.RefCountDec (LIR.Physical LIR.X7, 0, LIR.TaggedList, Some (rcMetadata (AST.TList recordType)))])
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
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum string payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec dispatches mixed boxed-sum dynamic payload cleanup by tag.
let testTaggedListRefCountDecMixedSumDynamicPayloadUsesVariantDispatch () : Result<unit, string> =
    let sumName = "X64ListMixedSumDynamicDispatch"
    let sumType = AST.TSum (sumName, [])
    let variants : LIR.VariantRegistry =
        Map.ofList [
            (sumName,
                { TypeParams = []
                  Variants =
                    [
                        { Name = "X64ListMixedSumNoPayload"; Tag = 0; Payload = None }
                        { Name = "X64ListMixedSumBytesPayload"; Tag = 1; Payload = Some AST.TBytes }
                    ] })
        ]
    let sumShapes =
        variants
        |> Map.map (fun _ typeVariants ->
            { ANF.TypeParams = typeVariants.TypeParams
              ANF.Payloads =
                typeVariants.Variants
                |> List.sortBy (fun variant -> variant.Tag)
                |> List.map (fun variant -> variant.Tag, variant.Payload) })
    let program =
        match
            makeSimpleProgram
                [
                    LIR.RefCountDec (
                        LIR.Physical LIR.X5,
                        0,
                        LIR.TaggedList,
                        Some (rcMetadataWithSumShapes sumShapes (AST.TList sumType)))
                ]
                LIR.Ret
        with
        | LIR.Program (functions, _, records) ->
            LIR.Program (functions, variants, records)

    match CodeGen_X86_64.translateProgram (completeFixtureVariants program) false with
    | Error e ->
        Error e
    | Ok instrs ->
        let rec seesTagCheckBeforeDynamicRelease (sawTagLoad: bool) (remaining: X86_64.Instr list) : bool =
            match remaining with
            | [] ->
                false
            | X86_64.MOV_load (X86_64.R10, X86_64.RDX, 0) :: rest ->
                seesTagCheckBeforeDynamicRelease true rest
            | X86_64.CMP_imm (X86_64.R10, 1) :: _ when sawTagLoad ->
                true
            | _ :: rest ->
                seesTagCheckBeforeDynamicRelease sawTagLoad rest

        if seesTagCheckBeforeDynamicRelease false instrs then
            Ok ()
        else
            Error "x64 tagged-list mixed boxed-sum dynamic payload release did not check the active variant tag"

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
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
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
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
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
                LIR.RefCountDec (LIR.Physical LIR.X5, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
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
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
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
                    LIR.RefCountDec (LIR.Physical LIR.X7, 0, LIR.TaggedList, Some (rcMetadata (AST.TList sumType)))])
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
                    LIR.RefCountDec (LIR.Physical LIR.X8, 0, LIR.TaggedList, Some (rcMetadata (AST.TList sumType)))])
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
                LIR.RefCountDec (LIR.Physical LIR.X21, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum tuple3 string/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec preserves dict value metadata in boxed sum tuple3 payloads.
let testTaggedListRefCountDecSumTuple3StringListDictListPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let tupleType = AST.TTuple [AST.TString; listType; dictType]
    let sumType = AST.TSum ("X64ListRcSumTuple3StringListDictList", [tupleType])
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 8)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Imm 7L, None)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 16)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X5, 8, LIR.Reg (LIR.Physical LIR.X6), Some listType)
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
                LIR.RefCountDec (LIR.Physical LIR.X21, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum tuple3 string/list/dict-list payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases boxed sum tuple3 closure/list/dict payloads.
let testTaggedListRefCountDecSumTuple3ClosureListDictPayload () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let tupleType = AST.TTuple [closureType; listType; dictType]
    let sumType = AST.TSum ("X64ListRcSumTuple3ClosureListDict", [tupleType])
    let program =
        makeSimpleProgram
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
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
                LIR.HeapStore (LIR.Physical LIR.X7, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.HeapStore (LIR.Physical LIR.X7, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X7, 16, LIR.Reg (LIR.Physical LIR.X6), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 16)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X7), Some tupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 8)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Reg (LIR.Physical LIR.X19), Some sumType)
                LIR.Mov (LIR.Physical LIR.X21, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X21, LIR.Physical LIR.X20, LIR.Physical LIR.X21)
                LIR.RefCountDec (LIR.Physical LIR.X21, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum tuple3 closure/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec preserves dict value metadata in boxed sum tuple3 closure/list/dict payloads.
let testTaggedListRefCountDecSumTuple3ClosureListDictListPayload () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let tupleType = AST.TTuple [closureType; listType; dictType]
    let sumType = AST.TSum ("X64ListRcSumTuple3ClosureListDictList", [tupleType])
    let program =
        makeSimpleProgram
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X4, LIR.Physical LIR.X3, LIR.Physical LIR.X4)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 8)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Imm 7L, None)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.HeapAlloc (LIR.Physical LIR.X5, 16)
                LIR.HeapStore (LIR.Physical LIR.X5, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X5, 8, LIR.Reg (LIR.Physical LIR.X6), Some listType)
                LIR.Mov (LIR.Physical LIR.X6, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X6, LIR.Physical LIR.X5, LIR.Physical LIR.X6)
                LIR.HeapAlloc (LIR.Physical LIR.X7, 24)
                LIR.HeapStore (LIR.Physical LIR.X7, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.HeapStore (LIR.Physical LIR.X7, 8, LIR.Reg (LIR.Physical LIR.X4), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X7, 16, LIR.Reg (LIR.Physical LIR.X6), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 16)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X7), Some tupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 8)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Reg (LIR.Physical LIR.X19), Some sumType)
                LIR.Mov (LIR.Physical LIR.X21, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X21, LIR.Physical LIR.X20, LIR.Physical LIR.X21)
                LIR.RefCountDec (LIR.Physical LIR.X21, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum tuple3 closure/list/dict-list payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases boxed sum tuple4 payloads with string/bytes/list/dict fields.
let testTaggedListRefCountDecSumTuple4StringBytesListDictPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let tupleType = AST.TTuple [AST.TString; AST.TBytes; listType; dictType]
    let sumType = AST.TSum ("X64ListRcSumTuple4StringBytesListDict", [tupleType])
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 32)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 24, LIR.Reg (LIR.Physical LIR.X7), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 16)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X19), Some tupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 8)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X20), Some sumType)
                LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X1, LIR.Physical LIR.X21, LIR.Physical LIR.X1)
                LIR.RefCountDec (LIR.Physical LIR.X1, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum tuple4 string/bytes/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases boxed sum tuple4 payloads with nested tuple string/list/dict fields.
let testTaggedListRefCountDecSumTuple4NestedTupleStringListDictPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let nestedTupleType = AST.TTuple [AST.TString; listType; dictType]
    let tupleType = AST.TTuple [AST.TInt64; AST.TInt64; AST.TInt64; nestedTupleType]
    let sumType = AST.TSum ("X64ListRcSumTuple4NestedTupleStringListDict", [tupleType])
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
                LIR.HeapAlloc (LIR.Physical LIR.X19, 32)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Imm 11L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Imm 22L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Imm 33L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 24, LIR.Reg (LIR.Physical LIR.X7), Some nestedTupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 16)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X19), Some tupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 8)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X20), Some sumType)
                LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X1, LIR.Physical LIR.X21, LIR.Physical LIR.X1)
                LIR.RefCountDec (LIR.Physical LIR.X1, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum tuple4 nested tuple string/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec preserves dict value metadata in boxed sum tuple4 payloads.
let testTaggedListRefCountDecSumTuple4StringBytesListDictListPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let tupleType = AST.TTuple [AST.TString; AST.TBytes; listType; dictType]
    let sumType = AST.TSum ("X64ListRcSumTuple4StringBytesListDictList", [tupleType])
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 8)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 7L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 16)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X7), Some listType)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X19, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 32)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 24, LIR.Reg (LIR.Physical LIR.X7), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 16)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X19), Some tupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 8)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X20), Some sumType)
                LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X1, LIR.Physical LIR.X21, LIR.Physical LIR.X1)
                LIR.RefCountDec (LIR.Physical LIR.X1, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum tuple4 string/bytes/list/dict-list payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases boxed sum tuple4 payloads with closure/bytes/list/dict fields.
let testTaggedListRefCountDecSumTuple4ClosureBytesListDictPayload () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let tupleType = AST.TTuple [closureType; AST.TBytes; listType; dictType]
    let sumType = AST.TSum ("X64ListRcSumTuple4ClosureBytesListDict", [tupleType])
    let program =
        makeSimpleProgram
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 32)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 24, LIR.Reg (LIR.Physical LIR.X7), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 16)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X19), Some tupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 8)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X20), Some sumType)
                LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X1, LIR.Physical LIR.X21, LIR.Physical LIR.X1)
                LIR.RefCountDec (LIR.Physical LIR.X1, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum tuple4 closure/bytes/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec preserves dict value metadata in boxed sum tuple4 closure payloads.
let testTaggedListRefCountDecSumTuple4ClosureStringListDictListPayload () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let tupleType = AST.TTuple [closureType; AST.TString; listType; dictType]
    let sumType = AST.TSum ("X64ListRcSumTuple4ClosureStringListDictList", [tupleType])
    let program =
        makeSimpleProgram
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 8)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 7L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 16)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X7), Some listType)
                LIR.Mov (LIR.Physical LIR.X20, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X20, LIR.Physical LIR.X19, LIR.Physical LIR.X20)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 32)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.HeapStore (LIR.Physical LIR.X21, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X21, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X21, 24, LIR.Reg (LIR.Physical LIR.X20), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X21), Some tupleType)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 8)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Reg (LIR.Physical LIR.X4), Some sumType)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.RefCountDec (LIR.Physical LIR.X7, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum tuple4 closure/string/list/dict-list payload release to balance leak counter, got stderr '{stderr.Trim()}'"

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
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
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
                LIR.RefCountDec (LIR.Physical LIR.X21, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum record3 string/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases boxed sum record4 payloads with string/bytes/list/dict fields.
let testTaggedListRefCountDecSumRecord4StringBytesListDictPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let recordType = AST.TRecord ("X64ListRcSumRecord4StringBytesListDict", [])
    let records =
        Map.ofList
            [("X64ListRcSumRecord4StringBytesListDict", [("name", AST.TString); ("blob", AST.TBytes); ("items", listType); ("lookup", dictType)])]
    let sumType = AST.TSum ("X64ListRcSumRecord4StringBytesListDictWrapper", [recordType])
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 32)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 24, LIR.Reg (LIR.Physical LIR.X7), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 16)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X19), Some recordType)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 8)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X20), Some sumType)
                LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X1, LIR.Physical LIR.X21, LIR.Physical LIR.X1)
                LIR.RefCountDec (LIR.Physical LIR.X1, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum record4 string/bytes/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec preserves dict value metadata in boxed sum record4 payloads.
let testTaggedListRefCountDecSumRecord4StringBytesListDictListPayload () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let recordType = AST.TRecord ("X64ListRcSumRecord4StringBytesListDictList", [])
    let records =
        Map.ofList
            [("X64ListRcSumRecord4StringBytesListDictList", [("name", AST.TString); ("blob", AST.TBytes); ("items", listType); ("lookup", dictType)])]
    let sumType = AST.TSum ("X64ListRcSumRecord4StringBytesListDictListWrapper", [recordType])
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right")
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 8)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 7L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 16)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X7), Some listType)
                LIR.Mov (LIR.Physical LIR.X20, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X20, LIR.Physical LIR.X19, LIR.Physical LIR.X20)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 32)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X21, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X21, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X21, 24, LIR.Reg (LIR.Physical LIR.X20), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X4, 16)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X21), Some recordType)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 8)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Reg (LIR.Physical LIR.X4), Some sumType)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.RefCountDec (LIR.Physical LIR.X7, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum record4 string/bytes/list/dict-list payload release to balance leak counter, got stderr '{stderr.Trim()}'"

/// Test: x64 tagged-list RefCountDec releases boxed sum record4 payloads with closure/bytes/list/dict fields.
let testTaggedListRefCountDecSumRecord4ClosureBytesListDictPayload () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let recordType = AST.TRecord ("X64ListRcSumRecord4ClosureBytesListDict", [])
    let records =
        Map.ofList
            [("X64ListRcSumRecord4ClosureBytesListDict", [("callback", closureType); ("blob", AST.TBytes); ("items", listType); ("lookup", dictType)])]
    let sumType = AST.TSum ("X64ListRcSumRecord4ClosureBytesListDictWrapper", [recordType])
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, "_start", [])
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload")
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Imm 1L, None)
                LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Imm 2L, None)
                LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                LIR.HeapAlloc (LIR.Physical LIR.X19, 32)
                LIR.HeapStore (LIR.Physical LIR.X19, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.HeapStore (LIR.Physical LIR.X19, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBytes)
                LIR.HeapStore (LIR.Physical LIR.X19, 16, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.HeapStore (LIR.Physical LIR.X19, 24, LIR.Reg (LIR.Physical LIR.X7), Some dictType)
                LIR.HeapAlloc (LIR.Physical LIR.X20, 16)
                LIR.HeapStore (LIR.Physical LIR.X20, 0, LIR.Imm 0L, None)
                LIR.HeapStore (LIR.Physical LIR.X20, 8, LIR.Reg (LIR.Physical LIR.X19), Some recordType)
                LIR.HeapAlloc (LIR.Physical LIR.X21, 8)
                LIR.HeapStore (LIR.Physical LIR.X21, 0, LIR.Reg (LIR.Physical LIR.X20), Some sumType)
                LIR.Mov (LIR.Physical LIR.X1, LIR.Imm 5L)
                LIR.Orr (LIR.Physical LIR.X1, LIR.Physical LIR.X21, LIR.Physical LIR.X1)
                LIR.RefCountDec (LIR.Physical LIR.X1, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
            ]
            LIR.Ret
            records

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list sum record4 closure/bytes/list/dict payload release to balance leak counter, got stderr '{stderr.Trim()}'"

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
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList sumType))))
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
                    LIR.RefCountDec (LIR.Physical LIR.X8, 0, LIR.TaggedList, Some (rcMetadata (AST.TList sumType)))])
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
                LIR.RefCountDec (LIR.Physical LIR.X6, 0, LIR.TaggedList, Some (rcMetadata ((AST.TList outerSumType))))
            ]
            LIR.Ret

    match runLIRProgramFullWithOptions program true with
    | Error e -> Error e
    | Ok (_, _, stderr) ->
        if stderr.Trim() = "" then Ok ()
        else Error $"Expected list nested sum payload release to balance leak counter, got stderr '{stderr.Trim()}'"

let tests : (string * (unit -> Result<unit, string>)) list = [
    ("LIR x64 codegen reports missing entry block", testReportsMissingEntryBlock)
    ("LIR rejects x64 overflow physical registers", testRejectsReservedOverflowPhysicalRegister)
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
    ("LIR materialized string literal RefCountDec skips release", testMaterializedStringLiteralRefCountDecSkipsRelease)
    ("LIR generic RefCountDec releases string field", testGenericRefCountDecStringField)
    ("LIR generic RefCountDec skips literal string field release", testGenericRefCountDecLiteralStringFieldSkipsRelease)
    ("LIR generic RefCountDec releases bytes field", testGenericRefCountDecBytesField)
    ("LIR generic RefCountDec releases nested string tuple field", testGenericRefCountDecNestedStringTupleField)
    ("LIR generic RefCountDec releases tuple string/list/dict fields", testGenericRefCountDecTupleStringListDictFields)
    ("LIR generic RefCountDec releases dict field list values", testGenericRefCountDecDictListValueField)
    ("LIR generic RefCountDec skips pure enum field", testGenericRefCountDecSkipsPureEnumField)
    ("LIR generic RefCountDec releases record string field", testGenericRefCountDecRecordStringField)
    ("LIR generic RefCountDec releases record dict field", testGenericRefCountDecRecordDictField)
    ("LIR generic RefCountDec releases record closure field", testGenericRefCountDecRecordClosureField)
    ("LIR generic RefCountDec releases record string/list/dict fields", testGenericRefCountDecRecordStringListDictFields)
    ("LIR generic RefCountDec releases sum string payload", testGenericRefCountDecSumStringPayload)
    ("LIR generic RefCountDec releases sum bytes payload", testGenericRefCountDecSumBytesPayload)
    ("LIR generic RefCountDec dispatches mixed sum payload cleanup", testGenericRefCountDecMixedSumPayloadUsesVariantDispatch)
    ("LIR generic RefCountDec releases nested sum string field", testGenericRefCountDecNestedSumStringField)
    ("LIR generic RefCountDec dispatches nested mixed sum payload cleanup", testGenericRefCountDecNestedMixedSumPayloadUsesVariantDispatch)
    ("LIR generic RefCountDec releases sum list payload", testGenericRefCountDecSumListPayload)
    ("LIR generic RefCountDec releases sum dict payload", testGenericRefCountDecSumDictPayload)
    ("LIR generic RefCountDec releases sum tuple string/list/dict payload", testGenericRefCountDecSumTupleStringListDictPayload)
    ("LIR generic RefCountDec releases sum record string/list/dict payload", testGenericRefCountDecSumRecordStringListDictPayload)
    ("LIR generic RefCountDec releases dict field", testGenericRefCountDecDictField)
    ("LIR DictHeap RefCountDec releases list leaf values", testDictRefCountDecListValue)
    ("LIR DictHeap RefCountDec releases string leaf keys", testDictRefCountDecStringKey)
    ("LIR DictHeap RefCountDec releases string leaf values", testDictRefCountDecStringValue)
    ("LIR DictHeap RefCountDec releases string leaf keys and values", testDictRefCountDecStringKeyValue)
    ("LIR DictHeap RefCountDec releases bytes leaf keys and values", testDictRefCountDecBytesKeyValue)
    ("LIR DictHeap RefCountDec releases string leaf keys and list values", testDictRefCountDecStringKeyListValue)
    ("LIR DictHeap RefCountDec releases string leaf keys and dict values", testDictRefCountDecStringKeyDictValue)
    ("LIR DictHeap RefCountDec releases string leaf keys and dict-list values", testDictRefCountDecStringKeyDictListValue)
    ("LIR DictHeap RefCountDec releases nested dict leaf values", testDictRefCountDecDictValue)
    ("LIR DictHeap RefCountDec releases nested dict list leaf values", testDictRefCountDecDictListValue)
    ("LIR DictHeap RefCountDec uses planned helper for nested dict list leaf values", testDictRefCountDecDictListValueUsesPlannedHelper)
    ("LIR tagged list RefCountDec uses planned helper for tuple payload", testTaggedListTuplePayloadUsesPlannedHelper)
    ("LIR tagged list RefCountDec uses planned helper for record payload", testTaggedListRecordPayloadUsesPlannedHelper)
    ("LIR tagged list RefCountDec uses planned helper for tuple5 payload", testTaggedListTuple5PayloadUsesPlannedHelper)
    ("LIR tagged list RefCountDec uses planned helper for record5 payload", testTaggedListRecord5PayloadUsesPlannedHelper)
    ("LIR DictHeap RefCountDec releases tuple string/list leaf values", testDictRefCountDecTupleStringListValue)
    ("LIR DictHeap RefCountDec releases tuple string/list/dict leaf values", testDictRefCountDecTupleStringListDictValue)
    ("LIR DictHeap RefCountDec releases string keys and tuple string/list/dict leaf values", testDictRefCountDecStringKeyTupleStringListDictValue)
    ("LIR DictHeap RefCountDec releases string collision keys and values", testDictRefCountDecStringCollisionKeysAndValues)
    ("LIR DictHeap RefCountDec releases string keys and tuple/list values", testDictRefCountDecStringKeyTupleListValue)
    ("LIR DictHeap RefCountDec releases collision string keys and tuple/list values", testDictRefCountDecStringCollisionKeysAndTupleListValues)
    ("LIR DictHeap RefCountDec uses planned helper for string keys and tuple/list values", testDictRefCountDecStringKeyTupleValueUsesPlannedHelper)
    ("LIR DictHeap RefCountDec releases sum string leaf values", testDictRefCountDecSumStringValue)
    ("LIR generic RefCountDec preserves live RAX across list field release", testGenericRefCountDecPreservesLiveRaxAcrossListFieldRelease)
    ("LIR generic RefCountDec preserves live RAX across dict field release", testGenericRefCountDecPreservesLiveRaxAcrossDictFieldRelease)
    ("LIR generic RefCountDec preserves live RAX across closure field release", testGenericRefCountDecPreservesLiveRaxAcrossClosureFieldRelease)
    ("LIR generic RefCountDec preserves live RAX across string field release", testGenericRefCountDecPreservesLiveRaxAcrossStringFieldRelease)
    ("LIR generic RefCountDec preserves live RAX across bytes field release", testGenericRefCountDecPreservesLiveRaxAcrossBytesFieldRelease)
    ("LIR generic RefCountDec preserves live RAX across nested fixed-block release", testGenericRefCountDecPreservesLiveRaxAcrossNestedFixedBlockRelease)
    ("LIR closure alloc RefCountDec balances leak counter", testClosureAllocRefCountDecBalancesLeakCounter)
    ("LIR generic RefCountDec releases closure field", testGenericRefCountDecClosureField)
    ("LIR generic RefCountDec releases sum closure payload", testGenericRefCountDecSumClosurePayload)
    ("LIR closure RefCountDec releases string capture", testClosureRefCountDecStringCapture)
    ("LIR closure RefCountDec releases bytes capture", testClosureRefCountDecBytesCapture)
    ("LIR closure RefCountDec releases list capture", testClosureRefCountDecListCapture)
    ("LIR closure RefCountDec releases dict capture", testClosureRefCountDecDictCapture)
    ("LIR closure RefCountDec releases dict list value capture", testClosureRefCountDecDictListValueCapture)
    ("LIR closure RefCountDec releases closure capture", testClosureRefCountDecClosureCapture)
    ("LIR closure RefCountDec releases tuple string capture", testClosureRefCountDecTupleStringCapture)
    ("LIR closure RefCountDec releases tuple string/list/dict capture", testClosureRefCountDecTupleStringListDictCapture)
    ("LIR closure RefCountDec releases tuple string/bytes/list/dict-list capture", testClosureRefCountDecTupleStringBytesListDictListCapture)
    ("LIR closure RefCountDec releases record string capture", testClosureRefCountDecRecordStringCapture)
    ("LIR closure RefCountDec releases record string/list/dict capture", testClosureRefCountDecRecordStringListDictCapture)
    ("LIR closure RefCountDec releases record string/bytes/list/dict-list capture", testClosureRefCountDecRecordStringBytesListDictListCapture)
    ("LIR closure RefCountDec releases sum string capture", testClosureRefCountDecSumStringCapture)
    ("LIR closure RefCountDec dispatches mixed sum capture cleanup", testClosureRefCountDecMixedSumCaptureUsesVariantDispatch)
    ("LIR closure RefCountDec releases sum tuple string/list/dict capture", testClosureRefCountDecSumTupleStringListDictCapture)
    ("LIR closure RefCountDec releases sum record string/list/dict capture", testClosureRefCountDecSumRecordStringListDictCapture)
    ("LIR closure RefCountDec releases multiple captures", testClosureRefCountDecMultipleCaptures)
    ("LIR tagged list RefCountDec releases closure payload", testTaggedListRefCountDecClosurePayload)
    ("LIR tagged list RefCountDec releases closure payload in stdlib helper", testTaggedListRefCountDecClosurePayloadInStdlibFunction)
    ("LIR tagged list RefCountDec releases dict payload", testTaggedListRefCountDecDictPayload)
    ("LIR tagged list RefCountDec releases dict/list payload", testTaggedListRefCountDecDictListPayload)
    ("LIR tagged list RefCountDec releases string payload", testTaggedListRefCountDecStringPayload)
    ("LIR tagged list RefCountDec releases tuple string payload", testTaggedListRefCountDecTupleStringPayload)
    ("LIR tagged list RefCountDec releases tuple3 dynamic payload", testTaggedListRefCountDecTuple3DynamicPayload)
    ("LIR tagged list RefCountDec releases tuple3 middle dynamic payload", testTaggedListRefCountDecTuple3MiddleDynamicPayload)
    ("LIR tagged list RefCountDec releases tuple3 dynamic payload combinations", testTaggedListRefCountDecTuple3DynamicPayloadCombinations)
    ("LIR tagged list RefCountDec releases tuple3 string/list/dict payload", testTaggedListRefCountDecTuple3StringListDictPayload)
    ("LIR tagged list RefCountDec releases tuple3 string/list/dict-list payload", testTaggedListRefCountDecTuple3StringListDictListPayload)
    ("LIR tagged list RefCountDec releases tuple3 closure/list/dict payload", testTaggedListRefCountDecTuple3ClosureListDictPayload)
    ("LIR tagged list RefCountDec releases tuple3 closure/list/dict-list payload", testTaggedListRefCountDecTuple3ClosureListDictListPayload)
    ("LIR tagged list RefCountDec releases record string payload", testTaggedListRefCountDecRecordStringPayload)
    ("LIR tagged list RefCountDec releases record3 dynamic payload", testTaggedListRefCountDecRecord3DynamicPayload)
    ("LIR tagged list RefCountDec releases record3 string/list/dict payload", testTaggedListRefCountDecRecord3StringListDictPayload)
    ("LIR tagged list RefCountDec releases record3 bytes/list/dict payload", testTaggedListRefCountDecRecord3BytesListDictPayload)
    ("LIR tagged list RefCountDec releases record3 closure/list/dict payload", testTaggedListRefCountDecRecord3ClosureListDictPayload)
    ("LIR tagged list RefCountDec releases record4 string/bytes/list/dict payload", testTaggedListRefCountDecRecord4StringBytesListDictPayload)
    ("LIR tagged list RefCountDec releases record4 closure/bytes/list/dict payload", testTaggedListRefCountDecRecord4ClosureBytesListDictPayload)
    ("LIR tagged list RefCountDec releases record4 nested tuple string/list/dict payload", testTaggedListRefCountDecRecord4NestedTupleStringListDictPayload)
    ("LIR tagged list RefCountDec releases record4 nested tuple string/list/dict-list payload", testTaggedListRefCountDecRecord4NestedTupleStringListDictListPayload)
    ("LIR tagged list RefCountDec releases record4 nested tuple string payload", testTaggedListRefCountDecRecord4NestedTupleStringPayload)
    ("LIR tagged list RefCountDec releases record4 nested tuple closure/bytes/list/dict payload", testTaggedListRefCountDecRecord4NestedTupleClosureBytesListDictPayload)
    ("LIR tagged list RefCountDec releases record4 nested tuple closure/bytes/list/dict-list payload", testTaggedListRefCountDecRecord4NestedTupleClosureBytesListDictListPayload)
    ("LIR tagged list RefCountDec releases tuple4 string/bytes/list/dict payload", testTaggedListRefCountDecTuple4StringBytesListDictPayload)
    ("LIR tagged list RefCountDec releases tuple4 string/bytes/list/dict-list payload", testTaggedListRefCountDecTuple4StringBytesListDictListPayload)
    ("LIR tagged list RefCountDec releases tuple4 closure/bytes/list/dict payload", testTaggedListRefCountDecTuple4ClosureBytesListDictPayload)
    ("LIR tagged list RefCountDec releases tuple4 closure/bytes/list/dict-list payload", testTaggedListRefCountDecTuple4ClosureBytesListDictListPayload)
    ("LIR tagged list RefCountDec releases tuple2 nested tuple string payload", testTaggedListRefCountDecTuple2NestedTupleStringPayload)
    ("LIR tagged list RefCountDec releases tuple4 nested tuple string payload", testTaggedListRefCountDecTuple4NestedTupleStringPayload)
    ("LIR tagged list RefCountDec releases tuple4 nested tuple string/list/dict payload", testTaggedListRefCountDecTuple4NestedTupleStringListDictPayload)
    ("LIR tagged list RefCountDec releases tuple4 nested record middle string/list/dict payload", testTaggedListRefCountDecTuple4NestedRecordMiddleStringListDictPayload)
    ("LIR tagged list RefCountDec releases tuple4 nested record middle string/list/dict-list payload", testTaggedListRefCountDecTuple4NestedRecordMiddleStringListDictListPayload)
    ("LIR tagged list RefCountDec releases tuple4 nested tuple closure/bytes/list/dict payload", testTaggedListRefCountDecTuple4NestedTupleClosureBytesListDictPayload)
    ("LIR tagged list RefCountDec releases tuple2 nested tuple dynamic combinations", testTaggedListRefCountDecTuple2NestedTupleDynamicPayloadCombinations)
    ("LIR tagged list RefCountDec releases tuple2 nested tuple string/list/dict payload", testTaggedListRefCountDecTuple2NestedTupleStringListDictPayload)
    ("LIR tagged list RefCountDec releases tuple2 nested tuple list/dict payload", testTaggedListRefCountDecTuple2NestedTupleListDictPayload)
    ("LIR tagged list RefCountDec releases tuple2 nested tuple dict payload", testTaggedListRefCountDecTuple2NestedTupleDictPayload)
    ("LIR tagged list RefCountDec releases tuple2 nested tuple closure payload", testTaggedListRefCountDecTuple2NestedTupleClosurePayload)
    ("LIR tagged list RefCountDec releases tuple2 nested tuple string/bytes/list/dict payload", testTaggedListRefCountDecTuple2NestedTupleStringBytesListDictPayload)
    ("LIR tagged list RefCountDec releases tuple2 nested tuple string/bytes/list/dict-list payload", testTaggedListRefCountDecTuple2NestedTupleStringBytesListDictListPayload)
    ("LIR tagged list RefCountDec releases record3 middle dynamic payload", testTaggedListRefCountDecRecord3MiddleDynamicPayload)
    ("LIR tagged list RefCountDec releases record3 dynamic payload combinations", testTaggedListRefCountDecRecord3DynamicPayloadCombinations)
    ("LIR tagged list RefCountDec releases sum string payload", testTaggedListRefCountDecSumStringPayload)
    ("LIR tagged list RefCountDec dispatches mixed sum dynamic payload cleanup", testTaggedListRefCountDecMixedSumDynamicPayloadUsesVariantDispatch)
    ("LIR tagged list RefCountDec releases sum list payload", testTaggedListRefCountDecSumListPayload)
    ("LIR tagged list RefCountDec releases sum dict payload", testTaggedListRefCountDecSumDictPayload)
    ("LIR tagged list RefCountDec releases sum closure payload", testTaggedListRefCountDecSumClosurePayload)
    ("LIR tagged list RefCountDec releases sum tuple string payload", testTaggedListRefCountDecSumTupleStringPayload)
    ("LIR tagged list RefCountDec releases sum tuple2 dynamic payload combinations", testTaggedListRefCountDecSumTuple2DynamicPayloadCombinations)
    ("LIR tagged list RefCountDec releases sum tuple3 dynamic payload combinations", testTaggedListRefCountDecSumTuple3DynamicPayloadCombinations)
    ("LIR tagged list RefCountDec releases sum tuple3 string/list/dict payload", testTaggedListRefCountDecSumTuple3StringListDictPayload)
    ("LIR tagged list RefCountDec releases sum tuple3 string/list/dict-list payload", testTaggedListRefCountDecSumTuple3StringListDictListPayload)
    ("LIR tagged list RefCountDec releases sum tuple3 closure/list/dict payload", testTaggedListRefCountDecSumTuple3ClosureListDictPayload)
    ("LIR tagged list RefCountDec releases sum tuple3 closure/list/dict-list payload", testTaggedListRefCountDecSumTuple3ClosureListDictListPayload)
    ("LIR tagged list RefCountDec releases sum tuple4 string/bytes/list/dict payload", testTaggedListRefCountDecSumTuple4StringBytesListDictPayload)
    ("LIR tagged list RefCountDec releases sum tuple4 nested tuple string/list/dict payload", testTaggedListRefCountDecSumTuple4NestedTupleStringListDictPayload)
    ("LIR tagged list RefCountDec releases sum tuple4 string/bytes/list/dict-list payload", testTaggedListRefCountDecSumTuple4StringBytesListDictListPayload)
    ("LIR tagged list RefCountDec releases sum tuple4 closure/bytes/list/dict payload", testTaggedListRefCountDecSumTuple4ClosureBytesListDictPayload)
    ("LIR tagged list RefCountDec releases sum tuple4 closure/string/list/dict-list payload", testTaggedListRefCountDecSumTuple4ClosureStringListDictListPayload)
    ("LIR tagged list RefCountDec releases sum record string payload", testTaggedListRefCountDecSumRecordStringPayload)
    ("LIR tagged list RefCountDec releases sum record3 string/list/dict payload", testTaggedListRefCountDecSumRecord3StringListDictPayload)
    ("LIR tagged list RefCountDec releases sum record4 string/bytes/list/dict payload", testTaggedListRefCountDecSumRecord4StringBytesListDictPayload)
    ("LIR tagged list RefCountDec releases sum record4 string/bytes/list/dict-list payload", testTaggedListRefCountDecSumRecord4StringBytesListDictListPayload)
    ("LIR tagged list RefCountDec releases sum record4 closure/bytes/list/dict payload", testTaggedListRefCountDecSumRecord4ClosureBytesListDictPayload)
    ("LIR tagged list RefCountDec releases sum record3 middle dynamic payload", testTaggedListRefCountDecSumRecord3MiddleDynamicPayload)
    ("LIR tagged list RefCountDec releases sum record3 dynamic payload combinations", testTaggedListRefCountDecSumRecord3DynamicPayloadCombinations)
    ("LIR tagged list RefCountDec releases nested sum string payload", testTaggedListRefCountDecNestedSumStringPayload)
]
