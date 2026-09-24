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

let rec private inferFixtureVariantsFromType (typ: AST.SemanticType) : LIR.VariantRegistry =
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
    | AST.TStream elemType ->
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
    | AST.TInt
    | AST.TUInt8
    | AST.TUInt16
    | AST.TUInt32
    | AST.TUInt64
    | AST.TUInt128
    | AST.TBool
    | AST.TFloat64
    | AST.TString
    | AST.TBlob
    | AST.TChar
    | AST.TDateTime
    | AST.TUnit
    | AST.TInternalRawPtr
    | AST.TNever
    | AST.TVar _
    | AST.TInferenceVar _ ->
        Map.empty

let private inferFixtureVariantsFromRcMetadata (metadata: MemoryModel.RcMetadata option) : LIR.VariantRegistry =
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

let testBranchFalseEdgeFallsThrough () : Result<unit, string> =
    let entry = LIR.Label "x64_layout_entry"
    let trueBlock = LIR.Label "x64_layout_true"
    let falseBlock = LIR.Label "x64_layout_false"
    let block label terminator : LIR.BasicBlock = { Label = label; Instrs = []; Terminator = terminator }
    let func : LIR.Function = {
        Id = TestIds.functionIdForName "x64_layout"
        Name = "x64_layout"
        TypedParams = []
        CFG = {
            Entry = entry
            Blocks = Map.ofList [
                entry, block entry (LIR.Branch (LIR.Physical LIR.X0, trueBlock, falseBlock))
                trueBlock, block trueBlock LIR.Ret
                falseBlock, block falseBlock LIR.Ret
            ]
        }
        StackSize = 0
        UsedCalleeSaved = []
        CodegenFacts = None
    }
    let program = LIR.Program ([func], Map.empty, Map.empty)
    match CodeGen_X86_64.translateProgram program false with
    | Error e -> Error e
    | Ok instrs ->
        let epilogueJumps = instrs |> List.filter ((=) (X86_64.JMP "_epilogue_x64_layout")) |> List.length
        if List.contains (X86_64.JMP "x64_layout_false") instrs then
            Error "x64 emitted a jump to the immediately following false block"
        elif epilogueJumps <> 1 then
            Error $"x64 emitted {epilogueJumps} jumps to the epilogue; expected one before the final fallthrough"
        else Ok ()

/// Build and run a LIR program with process arguments, returning exit code,
/// stdout, and stderr.
let private runLIRProgramFullWithOptionsAndArgs
    (program: LIR.Program)
    (enableLeakCheck: bool)
    (args: string list)
    : Result<int * string * string, string> =
    match CodeGen_X86_64.translateProgram (completeFixtureVariants program) enableLeakCheck with
    | Error e -> Error $"Codegen error: {e}"
    | Ok instrs ->
        let stringPool = X86_64_Resolve.collectStringPool instrs
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
                    let dataLabels =
                        X86_64_Resolve.dataLabelOffsets codeFileOffset codeSize stringPool
                    X86_64_Resolve.patchDataLabels unresolvedResult dataLabels codeFileOffset

            match patchedResult with
            | Error e -> Error $"Data label error: {e}"
            | Ok resolveResult ->
            let binary =
                Binary_Generation_ELF_X86_64.createExecutableWithPools
                    resolveResult.MachineCode stringPool LiteralPool.emptyFloatPool enableLeakCheck 0
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
                    | Ok Platform.X86_64 ->
                        let value = System.Diagnostics.ProcessStartInfo(tempPath)
                        args |> List.iter value.ArgumentList.Add
                        value
                    | _ ->
                        let value =
                            System.Diagnostics.ProcessStartInfo(
                                "/opt/dcb/qemu/qemu-x86_64")
                        value.ArgumentList.Add(tempPath)
                        args |> List.iter value.ArgumentList.Add
                        value
                psi.UseShellExecute <- false
                psi.RedirectStandardOutput <- true
                psi.RedirectStandardError <- true
                use proc = System.Diagnostics.Process.Start(psi)
                let stdoutTask = proc.StandardOutput.ReadToEndAsync()
                let stderrTask = proc.StandardError.ReadToEndAsync()

                if proc.WaitForExit(10000) then
                    Ok (proc.ExitCode, stdoutTask.Result, stderrTask.Result)
                else
                    try proc.Kill(true) with _ -> ()
                    Error "Execution timed out after 10000ms"
            with ex -> Error $"Execution failed: {ex.Message}"
            |> fun result ->
                try System.IO.File.Delete(tempPath) with _ -> ()
                result

/// Build and run a LIR program, returning exit code, stdout, and stderr.
let private runLIRProgramFullWithOptions (program: LIR.Program) (enableLeakCheck: bool) : Result<int * string * string, string> =
    runLIRProgramFullWithOptionsAndArgs program enableLeakCheck []

/// Build and run a LIR program, returning the exit code
let private runLIRProgram (program: LIR.Program) : Result<int, string> =
    match CodeGen_X86_64.translateProgram (completeFixtureVariants program) false with
    | Error e -> Error $"Codegen error: {e}"
    | Ok instrs ->
        let stringPool = X86_64_Resolve.collectStringPool instrs
        match X86_64_Resolve.resolveAndEncode instrs with
        | Error e -> Error $"Resolve error: {e}"
        | Ok unresolvedResult ->
            let codeFileOffset = 64 + 56
            let dataLabels =
                X86_64_Resolve.dataLabelOffsets
                    codeFileOffset
                    unresolvedResult.MachineCode.Length
                    stringPool
            match X86_64_Resolve.patchDataLabels unresolvedResult dataLabels codeFileOffset with
            | Error e -> Error $"Data label error: {e}"
            | Ok resolveResult ->
                let binary =
                    Binary_Generation_ELF_X86_64.createExecutableWithPools
                        resolveResult.MachineCode stringPool LiteralPool.emptyFloatPool false 0
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

let private rcMetadata (typ: AST.SemanticType) : MemoryModel.RcMetadata =
    { MemoryModel.ReleasePlanCacheKey = None
      MemoryModel.ReleasePlan = None
      MemoryModel.SourceType = Some typ }

let private rcMetadataWithSumShapes (sumShapes: MemoryModel.RcSumShapeRegistry) (typ: AST.SemanticType) : MemoryModel.RcMetadata =
    let releasePlan = MemoryPlanning.rcReleasePlanOfTypeWithSums Map.empty sumShapes typ
    { MemoryModel.ReleasePlanCacheKey = ReleasePlanFingerprint.rcReleasePlanCacheKey typ releasePlan
      MemoryModel.ReleasePlan = Some releasePlan
      MemoryModel.SourceType = Some typ }

let private completeRcMetadata (records: LIR.RecordRegistry) (metadata: MemoryModel.RcMetadata option) : MemoryModel.RcMetadata option =
    match metadata with
    | Some ({ ReleasePlan = None; SourceType = Some sourceType } as value) ->
        let releasePlan = MemoryPlanning.rcReleasePlanOfType records sourceType
        Some {
            value with
                ReleasePlanCacheKey = ReleasePlanFingerprint.rcReleasePlanCacheKey sourceType releasePlan
                ReleasePlan = Some releasePlan
        }
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
        Instrs =
            instrs |> List.map (completeRcInstrMetadata records)
        Terminator = term
    }
    let func : LIR.Function = {
        Id = TestIds.functionIdForName "_start"
        Name = "_start"
        TypedParams = []
        CFG = {
            Entry = entryLabel
            Blocks = Map.ofList [(entryLabel, entryBlock); (bodyLabel, bodyBlock)]
        }
        StackSize = 0
        UsedCalleeSaved = []
        CodegenFacts = None
    }
    LIR.Program ([func], Map.empty, records)

let private makeSimpleProgram (instrs: LIR.Instr list) (term: LIR.Terminator) : LIR.Program =
    makeSimpleProgramWithRecords instrs term Map.empty

/// Literal strings belong in the executable's immutable literal pool. Emitting
/// bump-allocation instructions here leaks one heap object per execution and can
/// exhaust the runtime heap in string-heavy code.
let testStringLiteralUsesStaticStorage () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [ LIR.Mov (LIR.Physical LIR.X1, LIR.StringSymbol "pooled") ]
            LIR.Ret

    match CodeGen_X86_64.translateProgram program false with
    | Error error -> Error error
    | Ok instructions ->
        if instructions |> List.exists (function | X86_64.LEA_rip (_, label) when label.StartsWith("__dark_string_literal_") -> true | _ -> false) then Ok ()
        else Error "Expected x86 string literal to be loaded from static storage"

/// Initializing a string literal field uses RCX internally. A separate live X3
/// value must survive when the containing record is held in another register.
let testStringLiteralHeapStorePreservesX3 () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [ LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 123L)
              LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
              LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.StringSymbol "field", Some AST.TString)
              LIR.Mov (LIR.Physical LIR.X0, LIR.Reg (LIR.Physical LIR.X3))
              LIR.PrintInt64 (LIR.Physical LIR.X0) ]
            LIR.Ret

    match runLIRProgramFullWithOptions program false with
    | Error error -> Error error
    | Ok (_, stdout, stderr) ->
        let output = stdout.Trim()
        if output = "123" && stderr = "" then Ok ()
        else Error $"Expected string field initialization to preserve X3=123, got stdout '{output}' and stderr '{stderr}'"

/// RawSlotInit computes its destination through R11. When the retained value is
/// also in X12/R11, ownership must be established before that computation.
let testRawSlotInitRetainsX12Value () : Result<unit, string> =
    let tupleType = AST.TTuple [AST.TString]
    let program =
        makeSimpleProgram
            [ LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
              LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.StringSymbol "owned", Some AST.TString)
              LIR.Mov (LIR.Physical LIR.X19, LIR.Imm 8L)
              LIR.RawAlloc (LIR.Physical LIR.X20, LIR.Physical LIR.X19)
              LIR.Mov (LIR.Physical LIR.X19, LIR.Imm 0L)
              LIR.Mov (LIR.Physical LIR.X12, LIR.Reg (LIR.Physical LIR.X3))
              LIR.RawSlotInit
                  (LIR.Physical LIR.X20, LIR.Physical LIR.X19, LIR.Physical LIR.X12, tupleType)
              LIR.RefCountDec
                  (LIR.Physical LIR.X3, 8, LIR.GenericHeap, Some (rcMetadata tupleType))
              LIR.HeapLoad (LIR.Physical LIR.X1, LIR.Physical LIR.X20, 0)
              LIR.HeapLoad (LIR.Physical LIR.X1, LIR.Physical LIR.X1, 0)
              LIR.PrintHeapStringNoNewline (LIR.Physical LIR.X1) ]
            LIR.Ret

    match runLIRProgramFullWithOptions program false with
    | Error error -> Error error
    | Ok (exitCode, stdout, stderr) ->
        if exitCode <> 0 then Error $"Expected exit code 0, got {exitCode}: {stderr}"
        elif stdout = "owned" then Ok ()
        else Error $"Expected X12 RawSlotInit value to remain owned, got stdout '{stdout}' and stderr '{stderr}'"

let testStringConcatLoadsStackSlotOperand () : Result<unit, string> =
    let withStackFrame program =
        match program with
        | LIR.Program ([func], variants, records) ->
            LIR.Program ([{ func with StackSize = 16 }], variants, records)
        | _ -> Crash.crash "StringConcat stack-slot fixture expected one function"

    let program =
        makeSimpleProgram
            [ LIR.StringConcat
                  (LIR.Physical LIR.X1, LIR.StringSymbol "1", LIR.StringSymbol "", [])
              LIR.Store (-8, LIR.Physical LIR.X1)
              LIR.StringConcat
                  (LIR.Physical LIR.X2, LIR.StringSymbol "2", LIR.StringSymbol "", [])
              LIR.Store (-16, LIR.Physical LIR.X2)
              LIR.Mov (LIR.Physical LIR.X12, LIR.StackSlot -8)
              LIR.StringConcat
                  (LIR.Physical LIR.X11, LIR.Reg (LIR.Physical LIR.X12), LIR.StackSlot -16, [])
              LIR.Mov (LIR.Physical LIR.X0, LIR.Reg (LIR.Physical LIR.X11))
              LIR.PrintHeapStringNoNewline (LIR.Physical LIR.X0) ]
            LIR.Ret
        |> withStackFrame

    match runLIRProgramFullWithOptions program false with
    | Error error -> Error error
    | Ok (exitCode, stdout, stderr) ->
        if exitCode <> 0 then Error $"Expected exit code 0, got {exitCode}: {stderr}"
        elif stdout = "12" then Ok ()
        else Error $"Expected x64 stack-slot concatenation to print '12', got stdout '{stdout}' and stderr '{stderr}'"

/// CLI argv is implemented by an in-binary runtime helper. Its call must
/// resolve as a code label rather than being deferred as an ELF data fixup.
let testCliArgvHelperResolvesAsCodeLabel () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [LIR.CliNative (LIR.Physical LIR.X0, LIR.GetArgv, [LIR.Imm 0L])]
            LIR.Ret

    match CodeGen_X86_64.translateProgram (completeFixtureVariants program) false with
    | Error error -> Error $"CLI argv x64 lowering failed: {error}"
    | Ok instrs ->
        match X86_64_Resolve.resolveAndEncode instrs with
        | Error error -> Error $"CLI argv x64 resolution failed: {error}"
        | Ok resolveResult ->
            match
                resolveResult.DeferredFixups
                |> List.tryFind (fun fixup -> fixup.TargetLabel = "__dark_cli_argv")
            with
            | None -> Ok ()
            | Some _ -> Error "CLI argv helper call was deferred as an ELF data fixup"

/// Native argv entries must be copied into a managed String and returned in a
/// boxed Some value, matching the LIR type of Stdlib.Cli.__argv.
let testCliArgvReturnsManagedOptionString () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [ LIR.CliNative (LIR.Physical LIR.X1, LIR.GetArgv, [LIR.Imm 0L])
              LIR.HeapLoad (LIR.Physical LIR.X3, LIR.Physical LIR.X1, 8)
              LIR.PrintHeapStringNoNewline (LIR.Physical LIR.X3) ]
            LIR.Ret

    match runLIRProgramFullWithOptionsAndArgs program false ["hello"] with
    | Error error -> Error error
    | Ok (exitCode, stdout, stderr) ->
        if exitCode <> 0 then Error $"Expected exit code 0, got {exitCode}: {stderr}"
        elif stdout <> "hello" then Error $"Expected Some(hello), got '{stdout}'"
        else Ok ()

/// Run the x64 kernel boundary under QEMU: hostname and environment values
/// must be managed strings, CPU affinity must be counted, and kill(2) must
/// preserve EINVAL without consulting a command-line utility.
let testCliHostOperationsExecute () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [ LIR.CliNative (LIR.Physical LIR.X0, LIR.CpuCount, [])
              LIR.PrintInt64 (LIR.Physical LIR.X0)
              LIR.CliNative (LIR.Physical LIR.X0, LIR.Hostname, [])
              LIR.HeapLoad (LIR.Physical LIR.X1, LIR.Physical LIR.X0, 8)
              LIR.PrintHeapString (LIR.Physical LIR.X1)
              LIR.CliNative (LIR.Physical LIR.X0, LIR.GetEnv, [LIR.StringSymbol "PATH"])
              LIR.HeapLoad (LIR.Physical LIR.X1, LIR.Physical LIR.X0, 8)
              LIR.PrintHeapString (LIR.Physical LIR.X1)
              LIR.CliNative (LIR.Physical LIR.X0, LIR.GetPid, [])
              LIR.CliNative (LIR.Physical LIR.X0, LIR.Kill, [LIR.Reg (LIR.Physical LIR.X0); LIR.Imm 99999L])
              LIR.HeapLoad (LIR.Physical LIR.X1, LIR.Physical LIR.X0, 8)
              LIR.HeapLoad (LIR.Physical LIR.X2, LIR.Physical LIR.X1, 0)
              LIR.PrintInt64 (LIR.Physical LIR.X2) ]
            LIR.Ret

    match runLIRProgramFullWithOptions program false with
    | Error error -> Error error
    | Ok (exitCode, stdout, stderr) ->
        match stdout.Split('\n') |> Array.toList with
        | cpuCount :: hostname :: path :: errno :: _ ->
            match System.Int64.TryParse cpuCount, System.Int64.TryParse errno with
            | (true, count), (true, 22L)
                when exitCode = 0 && count > 0L && hostname <> "" && path <> "" && stderr = "" -> Ok ()
            | _ -> Error $"Unexpected x64 CLI host output: exit={exitCode}, stdout='{stdout}', stderr='{stderr}'"
        | _ -> Error $"Incomplete x64 CLI host output: exit={exitCode}, stdout='{stdout}', stderr='{stderr}'"

let testCliNativePreservesLiveCallerRegister () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [ LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 42L)
              LIR.SaveRegs ([LIR.X3], [])
              LIR.CliNative (LIR.Physical LIR.X0, LIR.CpuCount, [])
              LIR.RestoreRegs ([LIR.X3], [])
              LIR.Mov (LIR.Physical LIR.X0, LIR.Reg (LIR.Physical LIR.X3))
              LIR.PrintInt64 (LIR.Physical LIR.X0) ]
            LIR.Ret
    match runLIRProgramFullWithOptions program false with
    | Error error -> Error error
    | Ok (exitCode, stdout, stderr) ->
        if exitCode = 0 && stdout = "42\n" && stderr = "" then Ok ()
        else Error $"CLI native call corrupted a live x64 caller register: exit={exitCode}, stdout='{stdout}', stderr='{stderr}'"

/// The host is ARM64, so inspect the x64 syscall lowering directly. DateTime
/// clock values retain nanosecond-derived precision as 100ns Unix ticks.
let testDateTimeNowLowersTo100nsUnixTicks () : Result<unit, string> =
    let program = makeSimpleProgram [LIR.DateTimeNow (LIR.Physical LIR.X0)] LIR.Ret
    match CodeGen_X86_64.translateProgram (completeFixtureVariants program) false with
    | Error error -> Error $"DateTimeNow x64 lowering failed: {error}"
    | Ok instrs ->
        let hasTicksPerSecond =
            instrs
            |> List.exists (function
                | X86_64.IMUL_imm (_, _, 10000000) -> true
                | _ -> false)
        let hasNanosecondsPerTick =
            instrs
            |> List.exists (function
                | X86_64.MOV_imm32 (X86_64.RDI, 100) -> true
                | _ -> false)
        let hasTickDivision =
            instrs
            |> List.exists (function
                | X86_64.IDIV X86_64.RDI -> true
                | _ -> false)
        if hasTicksPerSecond && hasNanosecondsPerTick && hasTickDivision then Ok ()
        else Error $"DateTimeNow did not lower to 100ns Unix ticks: {instrs}"

let testSleepLowersToNormalizedInterruptSafeNanosleep () : Result<unit, string> =
    let program = makeSimpleProgram [LIR.Sleep (41, LIR.FPhysical LIR.D0)] LIR.Ret
    match CodeGen_X86_64.translateProgram (completeFixtureVariants program) false with
    | Error error -> Error $"Sleep x64 lowering failed: {error}"
    | Ok instrs ->
        let hasNormalization =
            instrs
            |> List.exists (function
                | X86_64.MULSD (X86_64.XMM1, X86_64.XMM0) -> true
                | _ -> false)
            && instrs
               |> List.exists (function
                   | X86_64.CVTTSD2SI (X86_64.R11, X86_64.XMM1) -> true
                   | _ -> false)
            && instrs
               |> List.exists (function
                   | X86_64.IDIV X86_64.R10 -> true
                   | _ -> false)
        let hasSyscall =
            instrs
            |> List.windowed 2
            |> List.exists (function
                | [ X86_64.MOV_imm32 (X86_64.RAX, number); X86_64.SYSCALL ] ->
                    number = int32 Platform.linuxX86_64SyscallNumbers.Nanosleep
                | [ X86_64.MOV_imm (X86_64.RAX, number); X86_64.SYSCALL ] ->
                    number = int64 Platform.linuxX86_64SyscallNumbers.Nanosleep
                | _ -> false)
        let retriesRemainder =
            instrs
            |> List.exists (function
                | X86_64.CMP_imm (X86_64.RAX, -4) -> true
                | _ -> false)
            && instrs
               |> List.exists (function
                   | X86_64.MOV_load (X86_64.R10, X86_64.RSP, 16) -> true
                   | _ -> false)
            && instrs
               |> List.exists (function
                   | X86_64.MOV_load (X86_64.R10, X86_64.RSP, 24) -> true
                   | _ -> false)
        if hasNormalization && hasSyscall && retriesRemainder then Ok ()
        else Error $"Sleep did not lower to normalized interrupt-safe x64 nanosleep: {instrs}"

/// Float arguments are parallel moves. A swap must retain both original values
/// rather than letting the first MOVSD overwrite the source of the second one.
let testFloatArgumentMovesResolveCycles () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [ LIR.FLoad (LIR.FPhysical LIR.D1, 1.0)
              LIR.FLoad (LIR.FPhysical LIR.D2, 2.0)
              LIR.FArgMoves [ (LIR.D1, LIR.FPhysical LIR.D2); (LIR.D2, LIR.FPhysical LIR.D1) ]
              LIR.FloatToInt64 (LIR.Physical LIR.X1, LIR.FPhysical LIR.D1)
              LIR.FloatToInt64 (LIR.Physical LIR.X2, LIR.FPhysical LIR.D2)
              LIR.Mov (LIR.Physical LIR.X3, LIR.Imm 10L)
              LIR.Mul (LIR.Physical LIR.X0, LIR.Physical LIR.X1, LIR.Physical LIR.X3)
              LIR.Add (LIR.Physical LIR.X0, LIR.Physical LIR.X0, LIR.Reg (LIR.Physical LIR.X2))
              LIR.PrintInt64 (LIR.Physical LIR.X0) ]
            LIR.Ret

    match runLIRProgramFullWithOptions program false with
    | Error error -> Error error
    | Ok (_, stdout, stderr) ->
        let output = stdout.Trim()
        if output = "21" && stderr = "" then Ok ()
        else Error $"Expected swapped float arguments to print 21, got stdout '{output}' and stderr '{stderr}'"

let testHighFloatRegistersExecute () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [ LIR.FLoad (LIR.FPhysical LIR.D0, 0.01)
              LIR.FMov (LIR.FPhysical LIR.D15, LIR.FPhysical LIR.D0)
              LIR.FLoad (LIR.FPhysical LIR.D2, 2000.0)
              LIR.FMul (LIR.FPhysical LIR.D15, LIR.FPhysical LIR.D15, LIR.FPhysical LIR.D2)
              LIR.FloatToInt64 (LIR.Physical LIR.X0, LIR.FPhysical LIR.D15)
              LIR.PrintInt64 (LIR.Physical LIR.X0) ]
            LIR.Ret

    match runLIRProgramFullWithOptions program false with
    | Error error -> Error error
    | Ok (_, stdout, stderr) ->
        let output = stdout.Trim()
        if output = "20" && stderr = "" then Ok ()
        else Error $"Expected high x64 float registers to print 20, got stdout '{output}' and stderr '{stderr}'"

let testNonCommutativeFloatAliasesPreserveScratch () : Result<unit, string> =
    let program =
        makeSimpleProgram
            [ LIR.FLoad (LIR.FPhysical LIR.D0, 7.0)
              LIR.FLoad (LIR.FPhysical LIR.D1, 10.0)
              LIR.FLoad (LIR.FPhysical LIR.D2, 2.0)
              LIR.FSub (LIR.FPhysical LIR.D2, LIR.FPhysical LIR.D1, LIR.FPhysical LIR.D2)
              LIR.FloatToInt64 (LIR.Physical LIR.X1, LIR.FPhysical LIR.D2)
              LIR.FLoad (LIR.FPhysical LIR.D2, 2.0)
              LIR.FDiv (LIR.FPhysical LIR.D2, LIR.FPhysical LIR.D1, LIR.FPhysical LIR.D2)
              LIR.FloatToInt64 (LIR.Physical LIR.X2, LIR.FPhysical LIR.D2)
              LIR.FloatToInt64 (LIR.Physical LIR.X3, LIR.FPhysical LIR.D0)
              LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 100L)
              LIR.Mul (LIR.Physical LIR.X1, LIR.Physical LIR.X1, LIR.Physical LIR.X4)
              LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 10L)
              LIR.Mul (LIR.Physical LIR.X2, LIR.Physical LIR.X2, LIR.Physical LIR.X4)
              LIR.Add (LIR.Physical LIR.X0, LIR.Physical LIR.X1, LIR.Reg (LIR.Physical LIR.X2))
              LIR.Add (LIR.Physical LIR.X0, LIR.Physical LIR.X0, LIR.Reg (LIR.Physical LIR.X3))
              LIR.PrintInt64 (LIR.Physical LIR.X0) ]
            LIR.Ret

    match runLIRProgramFullWithOptions program false with
    | Error error -> Error error
    | Ok (_, stdout, stderr) ->
        let output = stdout.Trim()
        if output = "857" && stderr = "" then Ok ()
        else Error $"Expected aliased x64 float operations to print 857, got stdout '{output}' and stderr '{stderr}'"

let private runInNamedFunction (name: string) (instrs: LIR.Instr list) (term: LIR.Terminator) : LIR.Program =
    match makeSimpleProgram [LIR.Call (LIR.Physical LIR.X0, TestIds.functionIdForName name, [])] LIR.Ret with
    | LIR.Program ([entryFunc], variants, records) ->
        let calleeLabel = LIR.Label $"{name}_entry"
        let callee : LIR.Function = {
            Id = TestIds.functionIdForName name
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
            CodegenFacts = None
        }
        LIR.Program ([entryFunc; callee], variants, records)
    | _ ->
        Crash.crash "Test fixture expected a single entry function"

let private makeEmptyFunction (name: string) (typedParams: LIR.TypedLIRParam list) : LIR.Function =
    let label = LIR.Label $"{name}_entry"
    {
        Id = TestIds.functionIdForName name
        Name = name
        TypedParams = typedParams
        CFG = {
            Entry = label
            Blocks = Map.ofList [(label, { Label = label; Instrs = []; Terminator = LIR.Ret })]
        }
        StackSize = 0
        UsedCalleeSaved = []
        CodegenFacts = None
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
        Id = TestIds.functionIdForName "_start"
        Name = "_start"
        TypedParams = []
        CFG = {
            Entry = entryLabel
            Blocks = Map.ofList [(bodyLabel, bodyBlock)]
        }
        StackSize = 0
        UsedCalleeSaved = []
        CodegenFacts = None
    }
    let program = LIR.Program ([func], Map.empty, Map.empty)

    match CodeGen_X86_64.translateProgram (completeFixtureVariants program) false with
    | Error e when e.Contains "missing entry block" -> Ok ()
    | Error e -> Error $"Expected missing entry block error, got '{e}'"
    | Ok _ -> Error "Expected x64 codegen to reject a CFG whose entry block is absent"

/// Test: condition consumers cannot inherit float comparison state from an earlier translation.
let testRejectsConditionsWithoutBlockComparison () : Result<unit, string> =
    let translate instrs term =
        makeSimpleProgram instrs term
        |> completeFixtureVariants
        |> fun program -> CodeGen_X86_64.translateProgram program false

    let floatComparison =
        LIR.FCmp (LIR.FPhysical LIR.D0, LIR.FPhysical LIR.D1)

    match translate [floatComparison] LIR.Ret with
    | Error e -> Error $"Expected float comparison primer to translate, got '{e}'"
    | Ok _ ->
        let target = LIR.Label "_start_target"
        let cases =
            [ ("Cset", [LIR.Cset (LIR.Physical LIR.X0, LIR.EQ)], LIR.Ret)
              ("CondBranch", [], LIR.CondBranch (LIR.EQ, target, target)) ]

        let rec runCases remaining =
            match remaining with
            | [] -> Ok ()
            | (name, instrs, term) :: rest ->
                match translate instrs term with
                | Error e when e.Contains "without a preceding comparison in the same block" ->
                    runCases rest
                | Error e ->
                    Error $"Expected {name} comparison-context error, got '{e}'"
                | Ok _ ->
                    Error $"Expected x64 codegen to reject {name} without a block-local comparison"

        runCases cases

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
        Id = TestIds.functionIdForName "_start"
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
        CodegenFacts = None
    }
    let program = LIR.Program ([func], Map.empty, Map.empty)
    match runLIRProgram program with
    | Error e -> Error e
    | Ok exitCode ->
        if exitCode = 42 then Ok ()
        else Error $"Expected exit code 42, got {exitCode}"

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
                        { Name = "X64MixedSumBytesPayload"; Tag = 0; Payload = Some AST.TBlob }
                        { Name = "X64MixedSumListPayload"; Tag = 1; Payload = Some (AST.TList AST.TInt64) }
                    ] })
        ]
    let sumShapes =
        variants
        |> Map.map (fun _ typeVariants ->
            { MemoryModel.TypeParams = typeVariants.TypeParams
              MemoryModel.Payloads =
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
                        { Name = "X64NestedMixedSumBytesPayload"; Tag = 1; Payload = Some AST.TBlob }
                    ] })
        ]
    let sumShapes =
        variants
        |> Map.map (fun _ typeVariants ->
            { MemoryModel.TypeParams = typeVariants.TypeParams
              MemoryModel.Payloads =
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

/// Test: x64 DictHeap RefCountDec releases every managed payload in collision nodes.
let testDictRefCountDecStringCollisionKeysAndValues () : Result<unit, string> =
    let dictType = AST.TDict (AST.TString, AST.TString)
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "key", LIR.StringSymbol "1", [])
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "value", LIR.StringSymbol "1", [])
                LIR.StringConcat (LIR.Physical LIR.X4, LIR.StringSymbol "key", LIR.StringSymbol "2", [])
                LIR.StringConcat (LIR.Physical LIR.X5, LIR.StringSymbol "value", LIR.StringSymbol "2", [])
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

/// Test: x64 DictHeap RefCountDec releases every managed key and recursive tuple/list value in collision nodes.
let testDictRefCountDecStringCollisionKeysAndTupleListValues () : Result<unit, string> =
    let listType = AST.TList AST.TInt64
    let tupleType = AST.TTuple [AST.TString; listType]
    let dictType = AST.TDict (AST.TString, tupleType)
    let program =
        makeSimpleProgram
            [
                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "key", LIR.StringSymbol "1", [])
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "value", LIR.StringSymbol "1", [])
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 42L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 2L)
                LIR.Orr (LIR.Physical LIR.X5, LIR.Physical LIR.X4, LIR.Physical LIR.X5)
                LIR.HeapAlloc (LIR.Physical LIR.X6, 16)
                LIR.HeapStore (LIR.Physical LIR.X6, 0, LIR.Reg (LIR.Physical LIR.X3), Some AST.TString)
                LIR.HeapStore (LIR.Physical LIR.X6, 8, LIR.Reg (LIR.Physical LIR.X5), Some listType)
                LIR.Mov (LIR.Physical LIR.X19, LIR.Reg (LIR.Physical LIR.X6))
                LIR.Mov (LIR.Physical LIR.X20, LIR.Reg (LIR.Physical LIR.X2))

                LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "key", LIR.StringSymbol "2", [])
                LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "value", LIR.StringSymbol "2", [])
                LIR.HeapAlloc (LIR.Physical LIR.X4, 8)
                LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 99L, None)
                LIR.Mov (LIR.Physical LIR.X5, LIR.Imm 2L)
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
            AST.TBlob
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
                    ("blob", AST.TBlob)
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

/// Test: releasing one closure preserves other live closures in x64 argument registers.
let testClosureRefCountDecPreservesLiveArgumentClosures () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let closureTupleType = AST.TTuple [AST.TInt64; AST.TInt64]
    let capturedFunction name =
        makeEmptyFunction
            name
            [{ Reg = LIR.Physical LIR.X0; Type = closureTupleType }]
    let firstCaptured = capturedFunction "x64_preserved_closure_first"
    let secondCaptured = capturedFunction "x64_preserved_closure_second"
    let main =
        match makeSimpleProgram
            [
                LIR.ClosureAlloc (
                    LIR.Physical LIR.X5,
                    firstCaptured.Id,
                    [LIR.Imm 11L]
                )
                LIR.ClosureAlloc (
                    LIR.Physical LIR.X7,
                    secondCaptured.Id,
                    [LIR.Imm 22L]
                )
                LIR.RefCountDec (
                    LIR.Physical LIR.X7,
                    16,
                    LIR.ClosureHeap,
                    Some (rcMetadata closureType)
                )
                LIR.RefCountDec (
                    LIR.Physical LIR.X5,
                    16,
                    LIR.ClosureHeap,
                    Some (rcMetadata closureType)
                )
            ]
            LIR.Ret with
        | LIR.Program ([func], variants, records) ->
            LIR.Program ([func; firstCaptured; secondCaptured], variants, records)
        | other -> other

    match runLIRProgramFullWithOptions main true with
    | Error e -> Error e
    | Ok (exitCode, _, stderr) ->
        let leaks = stderr.Trim()
        if exitCode = 0 && leaks = "" then Ok ()
        else
            Error $"Expected both live closures to release cleanly, got exit {exitCode} and stderr '{leaks}'"

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
                        { Name = "X64ClosureMixedSumBytesPayload"; Tag = 1; Payload = Some AST.TBlob }
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
                        TestIds.functionIdForName "x64_mixed_sum_capture_fn",
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

/// Test: x64 tagged-list RefCountDec releases closure payloads in stdlib helper contexts.
let testTaggedListRefCountDecClosurePayloadInStdlibFunction () : Result<unit, string> =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let program =
        runInNamedFunction
            "Darklang.Stdlib.List.__mapHelper_i64_fn_i64_acc_fn_i64"
            [
                LIR.ClosureAlloc (LIR.Physical LIR.X2, TestIds.functionIdForName "Darklang.Stdlib.List.__mapHelper_i64_fn_i64_acc_fn_i64", [])
                LIR.HeapAlloc (LIR.Physical LIR.X3, 8)
                LIR.HeapStore (LIR.Physical LIR.X3, 0, LIR.Reg (LIR.Physical LIR.X2), Some closureType)
                LIR.Mov (LIR.Physical LIR.X4, LIR.Imm 2L)
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

/// Test: x64 tagged-list RefCountDec releases every tuple3 dynamic field combination.
let testTaggedListRefCountDecTuple3DynamicPayloadCombinations () : Result<unit, string> =
    let dynamicRegForIndex (index: int) : LIR.PhysReg =
        match index with
        | 0 -> LIR.X2
        | 1 -> LIR.X3
        | 2 -> LIR.X4
        | _ -> Crash.crash $"Unexpected tuple3 field index {index}"

    let isDynamicField (fieldType: AST.SemanticType) : bool =
        match fieldType with
        | AST.TString
        | AST.TBlob -> true
        | _ -> false

    let runCase (name: string, fields: AST.SemanticType list) : Result<unit, string> =
        let tupleType = AST.TTuple fields
        let dynamicAllocs =
            fields
            |> List.mapi (fun index fieldType ->
                if isDynamicField fieldType then
                    let reg = dynamicRegForIndex index
                    Some (LIR.StringConcat (LIR.Physical reg, LIR.StringSymbol $"left{name}{index}", LIR.StringSymbol $"right{name}{index}", []))
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
                    LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                    LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                    LIR.RefCountDec (LIR.Physical LIR.X7, 0, LIR.TaggedList, Some (rcMetadata (AST.TList tupleType)))])
                LIR.Ret

        match runLIRProgramFullWithOptions program true with
        | Error e -> Error e
        | Ok (_, _, stderr) ->
            if stderr.Trim() = "" then Ok ()
            else Error $"Expected list tuple3 {name} dynamic payload release to balance leak counter, got stderr '{stderr.Trim()}'"

    let rec runCases (cases: (string * AST.SemanticType list) list) : Result<unit, string> =
        match cases with
        | [] -> Ok ()
        | case :: rest ->
            match runCase case with
            | Ok () -> runCases rest
            | Error e -> Error e

    runCases
        [ ("first", [AST.TString; AST.TInt64; AST.TInt64])
          ("third", [AST.TInt64; AST.TInt64; AST.TString])
          ("first-second", [AST.TString; AST.TBlob; AST.TInt64])
          ("second-third", [AST.TInt64; AST.TString; AST.TBlob])
          ("all", [AST.TString; AST.TBlob; AST.TString]) ]

/// Test: x64 tagged-list RefCountDec releases nested tuple dynamic payloads at later offsets.
let testTaggedListRefCountDecTuple2NestedTupleDynamicPayloadCombinations () : Result<unit, string> =
    let runCase (name: string, nestedTupleType: AST.SemanticType, setup: LIR.Instr list, nestedStores: LIR.Instr list) : Result<unit, string> =
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
                    LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                    LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                    LIR.RefCountDec (LIR.Physical LIR.X7, 0, LIR.TaggedList, Some (rcMetadata (AST.TList tupleType)))])
                LIR.Ret

        match runLIRProgramFullWithOptions program true with
        | Error e -> Error e
        | Ok (_, _, stderr) ->
            if stderr.Trim() = "" then Ok ()
            else Error $"Expected list tuple2 nested tuple {name} dynamic payload release to balance leak counter, got stderr '{stderr.Trim()}'"

    let rec runCases (cases: (string * AST.SemanticType * LIR.Instr list * LIR.Instr list) list) : Result<unit, string> =
        match cases with
        | [] -> Ok ()
        | case :: rest ->
            match runCase case with
            | Ok () -> runCases rest
            | Error e -> Error e

    runCases
        [ ("Second", AST.TTuple [AST.TInt64; AST.TString],
           [LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right", [])],
           [LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 7L, None)
            LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)])
          ("Both", AST.TTuple [AST.TString; AST.TBlob],
           [LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "left", LIR.StringSymbol "right", [])
            LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "bytes", LIR.StringSymbol "payload", [])],
           [LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
            LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBlob)]) ]

/// Test: x64 tagged-list RefCountDec releases every record3 dynamic field combination.
let testTaggedListRefCountDecRecord3DynamicPayloadCombinations () : Result<unit, string> =
    let dynamicRegForIndex (index: int) : LIR.PhysReg =
        match index with
        | 0 -> LIR.X2
        | 1 -> LIR.X3
        | 2 -> LIR.X4
        | _ -> Crash.crash $"Unexpected record3 field index {index}"

    let isDynamicField (fieldType: AST.SemanticType) : bool =
        match fieldType with
        | AST.TString
        | AST.TBlob -> true
        | _ -> false

    let runCase (name: string, fields: AST.SemanticType list) : Result<unit, string> =
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
                    Some (LIR.StringConcat (LIR.Physical reg, LIR.StringSymbol $"left{name}{index}", LIR.StringSymbol $"right{name}{index}", []))
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
                    LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                    LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                    LIR.RefCountDec (LIR.Physical LIR.X7, 0, LIR.TaggedList, Some (rcMetadata (AST.TList recordType)))])
                LIR.Ret
                records

        match runLIRProgramFullWithOptions program true with
        | Error e -> Error e
        | Ok (_, _, stderr) ->
            if stderr.Trim() = "" then Ok ()
            else Error $"Expected list record3 {name} dynamic payload release to balance leak counter, got stderr '{stderr.Trim()}'"

    let rec runCases (cases: (string * AST.SemanticType list) list) : Result<unit, string> =
        match cases with
        | [] -> Ok ()
        | case :: rest ->
            match runCase case with
            | Ok () -> runCases rest
            | Error e -> Error e

    runCases
        [ ("First", [AST.TString; AST.TInt64; AST.TInt64])
          ("Third", [AST.TInt64; AST.TInt64; AST.TString])
          ("FirstSecond", [AST.TString; AST.TBlob; AST.TInt64])
          ("SecondThird", [AST.TInt64; AST.TString; AST.TBlob])
          ("All", [AST.TString; AST.TBlob; AST.TString]) ]

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
                        { Name = "X64ListMixedSumBytesPayload"; Tag = 1; Payload = Some AST.TBlob }
                    ] })
        ]
    let sumShapes =
        variants
        |> Map.map (fun _ typeVariants ->
            { MemoryModel.TypeParams = typeVariants.TypeParams
              MemoryModel.Payloads =
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

/// Test: x64 tagged-list RefCountDec releases boxed sum tuple2 dynamic combinations.
let testTaggedListRefCountDecSumTuple2DynamicPayloadCombinations () : Result<unit, string> =
    let runCase (name: string, tupleType: AST.SemanticType, setup: LIR.Instr list, stores: LIR.Instr list) : Result<unit, string> =
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
                    LIR.Mov (LIR.Physical LIR.X7, LIR.Imm 2L)
                    LIR.Orr (LIR.Physical LIR.X7, LIR.Physical LIR.X6, LIR.Physical LIR.X7)
                    LIR.RefCountDec (LIR.Physical LIR.X7, 0, LIR.TaggedList, Some (rcMetadata (AST.TList sumType)))])
                LIR.Ret

        match runLIRProgramFullWithOptions program true with
        | Error e -> Error e
        | Ok (_, _, stderr) ->
            if stderr.Trim() = "" then Ok ()
            else Error $"Expected list sum tuple2 {name} payload release to balance leak counter, got stderr '{stderr.Trim()}'"

    let rec runCases (cases: (string * AST.SemanticType * LIR.Instr list * LIR.Instr list) list) : Result<unit, string> =
        match cases with
        | [] -> Ok ()
        | case :: rest ->
            match runCase case with
            | Ok () -> runCases rest
            | Error e -> Error e

    runCases
        [ ("second",
           AST.TTuple [AST.TInt64; AST.TString],
           [LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "leftSecond", LIR.StringSymbol "rightSecond", [])],
           [LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Imm 7L, None)
            LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TString)])
          ("both",
           AST.TTuple [AST.TString; AST.TBlob],
           [LIR.StringConcat (LIR.Physical LIR.X2, LIR.StringSymbol "leftBoth0", LIR.StringSymbol "rightBoth0", [])
            LIR.StringConcat (LIR.Physical LIR.X3, LIR.StringSymbol "leftBoth1", LIR.StringSymbol "rightBoth1", [])],
           [LIR.HeapStore (LIR.Physical LIR.X4, 0, LIR.Reg (LIR.Physical LIR.X2), Some AST.TString)
            LIR.HeapStore (LIR.Physical LIR.X4, 8, LIR.Reg (LIR.Physical LIR.X3), Some AST.TBlob)]) ]

/// Test: x64 tagged-list RefCountDec releases boxed sum tuple3 dynamic combinations.
let testTaggedListRefCountDecSumTuple3DynamicPayloadCombinations () : Result<unit, string> =
    let dynamicRegForIndex (index: int) : LIR.PhysReg =
        match index with
        | 0 -> LIR.X2
        | 1 -> LIR.X3
        | 2 -> LIR.X4
        | _ -> Crash.crash $"Unexpected sum tuple3 field index {index}"

    let isDynamicField (fieldType: AST.SemanticType) : bool =
        match fieldType with
        | AST.TString
        | AST.TBlob -> true
        | _ -> false

    let runCase (name: string, fields: AST.SemanticType list) : Result<unit, string> =
        let tupleType = AST.TTuple fields
        let sumType = AST.TSum ($"X64ListRcSumTuple3{name}", [tupleType])
        let dynamicAllocs =
            fields
            |> List.mapi (fun index fieldType ->
                if isDynamicField fieldType then
                    let reg = dynamicRegForIndex index
                    Some (LIR.StringConcat (LIR.Physical reg, LIR.StringSymbol $"left{name}{index}", LIR.StringSymbol $"right{name}{index}", []))
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
                    LIR.Mov (LIR.Physical LIR.X8, LIR.Imm 2L)
                    LIR.Orr (LIR.Physical LIR.X8, LIR.Physical LIR.X7, LIR.Physical LIR.X8)
                    LIR.RefCountDec (LIR.Physical LIR.X8, 0, LIR.TaggedList, Some (rcMetadata (AST.TList sumType)))])
                LIR.Ret

        match runLIRProgramFullWithOptions program true with
        | Error e -> Error e
        | Ok (_, _, stderr) ->
            if stderr.Trim() = "" then Ok ()
            else Error $"Expected list sum tuple3 {name} payload release to balance leak counter, got stderr '{stderr.Trim()}'"

    let rec runCases (cases: (string * AST.SemanticType list) list) : Result<unit, string> =
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
          ("FirstSecond", [AST.TString; AST.TBlob; AST.TInt64])
          ("SecondThird", [AST.TInt64; AST.TString; AST.TBlob])
          ("FirstThird", [AST.TString; AST.TInt64; AST.TBlob])
          ("All", [AST.TString; AST.TBlob; AST.TString]) ]

/// Test: x64 tagged-list RefCountDec releases boxed sum record3 dynamic payload combinations.
let testTaggedListRefCountDecSumRecord3DynamicPayloadCombinations () : Result<unit, string> =
    let dynamicRegForIndex (index: int) : LIR.PhysReg =
        match index with
        | 0 -> LIR.X2
        | 1 -> LIR.X3
        | 2 -> LIR.X4
        | _ -> Crash.crash $"Unexpected sum record3 field index {index}"

    let isDynamicField (fieldType: AST.SemanticType) : bool =
        match fieldType with
        | AST.TString
        | AST.TBlob -> true
        | _ -> false

    let runCase (name: string, fields: AST.SemanticType list) : Result<unit, string> =
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
                    Some (LIR.StringConcat (LIR.Physical reg, LIR.StringSymbol $"left{name}{index}", LIR.StringSymbol $"right{name}{index}", []))
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
                    LIR.Mov (LIR.Physical LIR.X8, LIR.Imm 2L)
                    LIR.Orr (LIR.Physical LIR.X8, LIR.Physical LIR.X7, LIR.Physical LIR.X8)
                    LIR.RefCountDec (LIR.Physical LIR.X8, 0, LIR.TaggedList, Some (rcMetadata (AST.TList sumType)))])
                LIR.Ret
                records

        match runLIRProgramFullWithOptions program true with
        | Error e -> Error e
        | Ok (_, _, stderr) ->
            if stderr.Trim() = "" then Ok ()
            else Error $"Expected list sum record3 {name} dynamic payload release to balance leak counter, got stderr '{stderr.Trim()}'"

    let rec runCases (cases: (string * AST.SemanticType list) list) : Result<unit, string> =
        match cases with
        | [] -> Ok ()
        | case :: rest ->
            match runCase case with
            | Ok () -> runCases rest
            | Error e -> Error e

    runCases
        [ ("First", [AST.TString; AST.TInt64; AST.TInt64])
          ("Third", [AST.TInt64; AST.TInt64; AST.TString])
          ("FirstSecond", [AST.TString; AST.TBlob; AST.TInt64])
          ("SecondThird", [AST.TInt64; AST.TString; AST.TBlob])
          ("FirstThird", [AST.TString; AST.TInt64; AST.TBlob])
          ("All", [AST.TString; AST.TBlob; AST.TString]) ]

let tests : (string * (unit -> Result<unit, string>)) list = [
    ("x64 branch false edge falls through", testBranchFalseEdgeFallsThrough)
    ("LIR CLI argv x64 helper resolves as code label", testCliArgvHelperResolvesAsCodeLabel)
    ("LIR CLI argv x64 returns managed Option String", testCliArgvReturnsManagedOptionString)
    ("LIR CLI host operations execute under x64", testCliHostOperationsExecute)
    ("LIR CLI native call preserves live x64 caller register", testCliNativePreservesLiveCallerRegister)
    ("LIR string literal x64 uses static storage", testStringLiteralUsesStaticStorage)
    ("LIR string literal x64 heap store preserves X3", testStringLiteralHeapStorePreservesX3)
    ("LIR x64 RawSlotInit retains X12 value", testRawSlotInitRetainsX12Value)
    ("LIR StringConcat x64 loads stack-slot operand", testStringConcatLoadsStackSlotOperand)
    ("LIR x64 codegen reports missing entry block", testReportsMissingEntryBlock)
    ("LIR x64 codegen rejects conditions without block comparison", testRejectsConditionsWithoutBlockComparison)
    ("LIR DateTimeNow x64 lowering uses 100ns Unix ticks", testDateTimeNowLowersTo100nsUnixTicks)
    ("LIR Sleep x64 lowering normalizes timeout and retries nanosleep", testSleepLowersToNormalizedInterruptSafeNanosleep)
    ("LIR float x64 argument moves resolve cycles", testFloatArgumentMovesResolveCycles)
    ("LIR high x64 float registers execute", testHighFloatRegistersExecute)
    ("LIR x64 noncommutative float aliases preserve scratch", testNonCommutativeFloatAliasesPreserveScratch)
    ("LIR conditional branch", testBranch)
    ("LIR generic RefCountDec dispatches mixed sum payload cleanup", testGenericRefCountDecMixedSumPayloadUsesVariantDispatch)
    ("LIR generic RefCountDec dispatches nested mixed sum payload cleanup", testGenericRefCountDecNestedMixedSumPayloadUsesVariantDispatch)
    ("LIR DictHeap RefCountDec uses planned helper for nested dict list leaf values", testDictRefCountDecDictListValueUsesPlannedHelper)
    ("LIR tagged list RefCountDec uses planned helper for tuple payload", testTaggedListTuplePayloadUsesPlannedHelper)
    ("LIR tagged list RefCountDec uses planned helper for record payload", testTaggedListRecordPayloadUsesPlannedHelper)
    ("LIR tagged list RefCountDec uses planned helper for tuple5 payload", testTaggedListTuple5PayloadUsesPlannedHelper)
    ("LIR tagged list RefCountDec uses planned helper for record5 payload", testTaggedListRecord5PayloadUsesPlannedHelper)
    ("LIR DictHeap RefCountDec releases string collision keys and values", testDictRefCountDecStringCollisionKeysAndValues)
    ("LIR DictHeap RefCountDec releases collision string keys and tuple/list values", testDictRefCountDecStringCollisionKeysAndTupleListValues)
    ("LIR DictHeap RefCountDec uses planned helper for string keys and tuple/list values", testDictRefCountDecStringKeyTupleValueUsesPlannedHelper)
    ("LIR closure RefCountDec preserves live argument closures", testClosureRefCountDecPreservesLiveArgumentClosures)
    ("LIR closure RefCountDec dispatches mixed sum capture cleanup", testClosureRefCountDecMixedSumCaptureUsesVariantDispatch)
    ("LIR tagged list RefCountDec releases closure payload in stdlib helper", testTaggedListRefCountDecClosurePayloadInStdlibFunction)
    ("LIR tagged list RefCountDec releases tuple3 dynamic payload combinations", testTaggedListRefCountDecTuple3DynamicPayloadCombinations)
    ("LIR tagged list RefCountDec releases tuple2 nested tuple dynamic combinations", testTaggedListRefCountDecTuple2NestedTupleDynamicPayloadCombinations)
    ("LIR tagged list RefCountDec releases record3 dynamic payload combinations", testTaggedListRefCountDecRecord3DynamicPayloadCombinations)
    ("LIR tagged list RefCountDec dispatches mixed sum dynamic payload cleanup", testTaggedListRefCountDecMixedSumDynamicPayloadUsesVariantDispatch)
    ("LIR tagged list RefCountDec releases sum tuple2 dynamic payload combinations", testTaggedListRefCountDecSumTuple2DynamicPayloadCombinations)
    ("LIR tagged list RefCountDec releases sum tuple3 dynamic payload combinations", testTaggedListRefCountDecSumTuple3DynamicPayloadCombinations)
    ("LIR tagged list RefCountDec releases sum record3 dynamic payload combinations", testTaggedListRefCountDecSumRecord3DynamicPayloadCombinations)
]
