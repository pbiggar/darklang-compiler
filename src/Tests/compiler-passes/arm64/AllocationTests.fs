// AllocationTests.fs - Verify allocation, operand preservation, and root ownership emission.

module ARM64AllocationTests

open ARM64CodeGenFixtures
open ARM64ReleasePlanningTests

let private convertRawAlloc
    (dest: LIR.PhysReg)
    (numBytes: LIR.PhysReg)
    : Result<ARM64Symbolic.Instr list, string> =
    let ctx : ARM64CodeGenTypes.CodeGenContext = {
        Target = target
        Options = ARM64CodeGenTypes.defaultOptions
        SumShapeRegistry = Map.empty
        RecordRegistry = Map.empty
        RawSlotInitRetainTargets = None
        ClosurePayloadSizes = Map.empty
        ClosureCaptureTypes = Map.empty
        FunctionNames = Map.empty
        FunctionName = "test"
        InstructionSite = "test_0"
        StackSize = 0
        UsedCalleeSaved = []
        HeapOverflowLabel = "__heap_oom_test"
        RecordLirOpExpansion = None
    }
    ARM64Instructions.convertInstr ctx (LIR.RawAlloc (LIR.Physical dest, LIR.Physical numBytes))

let testGeneratedCodeEliminatesSelfMoves () : TestResult =
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.Mov (LIR.Physical LIR.X1, LIR.Reg (LIR.Physical LIR.X1))
                LIR.FMov (LIR.FPhysical LIR.D1, LIR.FPhysical LIR.D1)
            ]
            Map.empty

    match generatePreparedARM64 target program with
    | Error e ->
        Error e
    | Ok instrs ->
        let hasSelfMove =
            instrs
            |> List.exists (function
                | ARM64Symbolic.MOV_reg (dest, src) when dest = src ->
                    true
                | ARM64Symbolic.FMOV_reg (dest, src) when dest = src ->
                    true
                | _ ->
                    false)

        if hasSelfMove then
            Error "Generated ARM64 code contains a redundant self-move"
        else
            Ok ()

/// Sleep is target-native on every supported ARM64 OS. Milliseconds are first
/// converted to integral nanoseconds, then split into a normalized timespec;
/// EINTR resumes from the kernel-provided remainder.
let testSleepUsesNormalizedInterruptSafeNanosleep () : TestResult =
    let program =
        makeSimpleProgramWithVariants [LIR.Sleep (41, LIR.FPhysical LIR.D0)] Map.empty
    let generate targetName targetConfig =
        generatePreparedARM64 targetConfig program
        |> Result.mapError (fun error -> $"{targetName} sleep lowering failed: {error}")
    match generate "Linux ARM64" (ARM64.targetConfigFor Platform.LinuxARM64),
          generate "macOS ARM64" (ARM64.targetConfigFor Platform.MacOSARM64) with
    | Error error, _ | _, Error error -> Error error
    | Ok linuxInstrs, Ok macInstrs ->
        let hasNormalization =
            linuxInstrs
            |> List.exists (function
                | ARM64Symbolic.FMUL (ARM64.D16, ARM64.D0, ARM64.D16) -> true
                | _ -> false)
            && linuxInstrs
               |> List.exists (function
                   | ARM64Symbolic.FCVTZS (ARM64.X9, ARM64.D16) -> true
                   | _ -> false)
            && linuxInstrs
               |> List.exists (function
                   | ARM64Symbolic.SDIV (ARM64.X10, ARM64.X9, ARM64.X12) -> true
                   | _ -> false)
            && linuxInstrs
               |> List.exists (function
                   | ARM64Symbolic.MSUB (ARM64.X11, ARM64.X10, ARM64.X12, ARM64.X9) -> true
                   | _ -> false)
        let hasLinuxSyscall =
            linuxInstrs
            |> List.windowed 2
            |> List.exists (function
                | [ ARM64Symbolic.MOVZ (ARM64.X8, number, 0)
                    ARM64Symbolic.SVC 0us ] -> number = Platform.linuxARM64SyscallNumbers.Nanosleep
                | _ -> false)
        let hasMacSyscall =
            macInstrs
            |> List.windowed 2
            |> List.exists (function
                | [ ARM64Symbolic.MOVZ (ARM64.X16, number, 0)
                    ARM64Symbolic.SVC 128us ] -> number = Platform.macOSARM64SyscallNumbers.Nanosleep
                | _ -> false)
        let retriesRemainder =
            linuxInstrs
            |> List.exists (function
                | ARM64Symbolic.LDP (ARM64.X10, ARM64.X11, ARM64.SP, 16s) -> true
                | _ -> false)
            && macInstrs
               |> List.exists (function
                   | ARM64Symbolic.B_cond_label (ARM64.LO, _) -> true
                   | _ -> false)
            && macInstrs
               |> List.exists (function
                   | ARM64Symbolic.CMP_imm (ARM64.X0, 4us) -> true
                   | _ -> false)
        if hasNormalization && hasLinuxSyscall && hasMacSyscall && retriesRemainder then Ok ()
        else Error "ARM64 sleep did not emit normalized interrupt-safe nanosleep lowering for both targets"

/// Host discovery and signalling have target-specific kernel ABIs. Exercise
/// the complete native lowering for each supported ARM64 target so a Linux
/// syscall number or error convention cannot accidentally leak into macOS.
let testCliHostOperationsUseTargetKernelABIs () : TestResult =
    let program =
        makeSimpleProgramWithVariants
            [ LIR.CliNative (LIR.Physical LIR.X0, LIR.HostArchitecture, [])
              LIR.CliNative (LIR.Physical LIR.X0, LIR.Hostname, [])
              LIR.CliNative (LIR.Physical LIR.X0, LIR.CpuCount, [])
              LIR.CliNative (LIR.Physical LIR.X0, LIR.Kill, [LIR.Imm 1L; LIR.Imm 0L]) ]
            Map.empty
    let generate targetName targetConfig =
        generatePreparedARM64 targetConfig program
        |> Result.mapError (fun error -> $"{targetName} CLI host lowering failed: {error}")
    let containsSyscall register number immediate instrs =
        instrs
        |> List.windowed 2
        |> List.exists (function
            | [ ARM64Symbolic.MOVZ (actualRegister, actualNumber, 0)
                ARM64Symbolic.SVC actualImmediate ] ->
                actualRegister = register && actualNumber = number && actualImmediate = immediate
            | _ -> false)
    match generate "Linux ARM64" (ARM64.targetConfigFor Platform.LinuxARM64),
          generate "macOS ARM64" (ARM64.targetConfigFor Platform.MacOSARM64) with
    | Error error, _ | _, Error error -> Error error
    | Ok linuxInstrs, Ok macInstrs ->
        let linuxContract =
            List.contains (ARM64Symbolic.MOVZ (ARM64.X0, 2us, 0)) linuxInstrs
            && containsSyscall ARM64.X8 160us 0us linuxInstrs
            && containsSyscall ARM64.X8 123us 0us linuxInstrs
            && containsSyscall ARM64.X8 129us 0us linuxInstrs
            && (linuxInstrs
                |> List.exists (function
                    | ARM64Symbolic.B_cond_label (ARM64.LT, _) -> true
                    | _ -> false))
        let macContract =
            List.contains (ARM64Symbolic.MOVZ (ARM64.X0, 3us, 0)) macInstrs
            && containsSyscall ARM64.X16 164us 128us macInstrs
            && containsSyscall ARM64.X16 202us 128us macInstrs
            && containsSyscall ARM64.X16 37us 128us macInstrs
            && (macInstrs
                |> List.exists (function
                    | ARM64Symbolic.B_cond_label (ARM64.HS, _) -> true
                    | _ -> false))
        if linuxContract && macContract then Ok ()
        else Error "ARM64 CLI host operations did not preserve the Linux and macOS kernel ABIs"

/// The shared list-retain helper's instruction shape is not observable in an
/// executable E2E test, so inspect the symbolic code generated by one retain.
let testListRetainHelperClearsTagWithImmediateMask () : TestResult =
    let program =
        makeSimpleProgramWithVariants
            [LIR.RefCountInc (LIR.Physical LIR.X1, 8, LIR.TaggedList, None)]
            Map.empty

    match generatePreparedARM64 target program with
    | Error e -> Error e
    | Ok instrs ->
        let hasImmediateTagClear =
            instrs
            |> List.contains
                (ARM64Symbolic.AND_imm (ARM64.X2, ARM64.X0, 0xFFFFFFFFFFFFFFF8UL))

        if hasImmediateTagClear then
            Ok ()
        else
            Error "ARM64 list retain helper did not clear tag bits with one immediate mask"

/// The shared dictionary-retain helper's instruction shape is not observable in
/// an executable E2E test, so inspect the symbolic code generated by one retain.
let testDictRetainHelperClearsTagWithImmediateMask () : TestResult =
    let program =
        makeSimpleProgramWithVariants
            [LIR.RefCountInc (LIR.Physical LIR.X1, 8, LIR.DictHeap, None)]
            Map.empty

    match generatePreparedARM64 target program with
    | Error e -> Error e
    | Ok instrs ->
        let hasImmediateTagClear =
            instrs
            |> List.contains
                (ARM64Symbolic.AND_imm (ARM64.X2, ARM64.X0, 0xFFFFFFFFFFFFFFF8UL))

        if hasImmediateTagClear then
            Ok ()
        else
            Error "ARM64 dictionary retain helper did not clear tag bits with one immediate mask"

/// The shared dictionary-release helper's instruction shape is not observable
/// in an executable E2E test, so inspect its symbolic code directly.
let testDictReleaseHelperClearsTagWithImmediateMask () : TestResult =
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.DictHeap,
                    Some (rcMetadata dictType))
            ]
            Map.empty

    match generatePreparedARM64 target program with
    | Error e -> Error e
    | Ok instrs ->
        let hasImmediateTagClear =
            instrs
            |> List.contains
                (ARM64Symbolic.AND_imm (ARM64.X3, ARM64.X0, 0xFFFFFFFFFFFFFFF8UL))

        if hasImmediateTagClear then
            Ok ()
        else
            Error "ARM64 dictionary release helper did not clear tag bits with one immediate mask"

/// Structural-child tag clearing inside the shared dictionary-release helper is
/// not observable in an executable E2E test, so inspect its symbolic code directly.
let testDictReleaseHelperClearsChildTagWithImmediateMask () : TestResult =
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.DictHeap,
                    Some (rcMetadata dictType))
            ]
            Map.empty

    match generatePreparedARM64 target program with
    | Error e -> Error e
    | Ok instrs ->
        let hasImmediateChildTagClear =
            instrs
            |> List.contains
                (ARM64Symbolic.AND_imm (ARM64.X10, ARM64.X8, 0xFFFFFFFFFFFFFFF8UL))

        if hasImmediateChildTagClear then
            Ok ()
        else
            Error "ARM64 dictionary release helper did not clear structural-child tag bits with one immediate mask"

/// Dictionary-helper bitmap popcount shape is not observable in an executable
/// E2E test, so ensure both shared helpers avoid a data-dependent counting loop.
let testDictHelpersUseConstantTimeBitmapPopcount () : TestResult =
    let dictType = AST.TDict (AST.TInt64, AST.TInt64)
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountInc (LIR.Physical LIR.X0, 0, LIR.DictHeap, None)
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.DictHeap,
                    Some (rcMetadata dictType))
            ]
            Map.empty

    match generatePreparedARM64 target program with
    | Error e -> Error e
    | Ok instrs ->
        let hasDataDependentPopcountLoop =
            instrs
            |> List.exists (function
                | ARM64Symbolic.Label label when label.Contains("_popcount_loop") ->
                    true
                | _ ->
                    false)

        if hasDataDependentPopcountLoop then
            Error "ARM64 dictionary helpers count bitmap bits with a data-dependent loop"
        else
            let hasPopcountSequence source result =
                instrs
                |> List.windowed 4
                |> List.exists (function
                    | [
                        ARM64Symbolic.FMOV_from_gp (ARM64.D16, actualSource)
                        ARM64Symbolic.CNT_8B (ARM64.D16, ARM64.D16)
                        ARM64Symbolic.ADDV_8B (ARM64.D16, ARM64.D16)
                        ARM64Symbolic.UMOV_byte (actualResult, ARM64.D16)
                      ] when actualSource = source && actualResult = result ->
                        true
                    | _ ->
                        false)

            if not (hasPopcountSequence ARM64.X4 ARM64.X3) then
                Error "ARM64 dictionary retain helper omitted constant-time bitmap popcount"
            else if not (hasPopcountSequence ARM64.X6 ARM64.X5) then
                Error "ARM64 dictionary release helper omitted constant-time bitmap popcount"
            else
                Ok ()

/// The shared list-release helper's traversal instruction shape is not
/// observable in an executable E2E test, so inspect its symbolic code directly.
let testListReleaseHelperClearsTagWithImmediateMask () : TestResult =
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadata (AST.TList AST.TInt64)))
            ]
            Map.empty

    match generatePreparedARM64 target program with
    | Error e -> Error e
    | Ok instrs ->
        let hasImmediateTagClear =
            instrs
            |> List.contains
                (ARM64Symbolic.AND_imm (ARM64.X3, ARM64.X0, 0xFFFFFFFFFFFFFFF8UL))

        if hasImmediateTagClear then
            Ok ()
        else
            Error "ARM64 list release helper did not clear tag bits with one immediate mask"

/// Structural-child tag clearing inside the shared list-release helper is not
/// observable in an executable E2E test, so inspect its symbolic code directly.
let testListReleaseHelperClearsChildTagWithImmediateMask () : TestResult =
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadata (AST.TList AST.TInt64)))
            ]
            Map.empty

    match generatePreparedARM64 target program with
    | Error e -> Error e
    | Ok instrs ->
        let hasImmediateChildTagClear =
            instrs
            |> List.contains
                (ARM64Symbolic.AND_imm (ARM64.X10, ARM64.X8, 0xFFFFFFFFFFFFFFF8UL))

        if hasImmediateChildTagClear then
            Ok ()
        else
            Error "ARM64 list release helper did not clear structural-child tag bits with one immediate mask"

let testPeepholeFusesBitClearSequence () : TestResult =
    let before = [
        ARM64Symbolic.MOVN (ARM64.X9, 0us, 0)
        ARM64Symbolic.EOR_reg (ARM64.X10, ARM64.X2, ARM64.X9)
        ARM64Symbolic.AND_reg (ARM64.X10, ARM64.X1, ARM64.X10)
    ]
    let liveTemporary = [
        ARM64Symbolic.MOVN (ARM64.X9, 0us, 0)
        ARM64Symbolic.EOR_reg (ARM64.X10, ARM64.X2, ARM64.X9)
        ARM64Symbolic.AND_reg (ARM64.X3, ARM64.X1, ARM64.X10)
    ]
    let liveMask =
        before @ [ARM64Symbolic.ADD_reg (ARM64.X0, ARM64.X9, ARM64.X4)]
    let expected = [ARM64Symbolic.BIC_reg (ARM64.X10, ARM64.X1, ARM64.X2)]
    let overwrittenMask =
        before @ [ARM64Symbolic.MOV_reg (ARM64.X9, ARM64.X4)]
    let expectedWithOverwrite =
        expected @ [ARM64Symbolic.MOV_reg (ARM64.X9, ARM64.X4)]
    let actual = ARM64Peephole.peepholeOptimize before

    if actual <> expected then
        let rendered =
            actual
            |> List.map TestDSL.PassTestRunner.prettyPrintARM64Instr
            |> String.concat "; "
        Error $"Expected BIC_reg(X10, X1, X2), got {rendered}"
    elif ARM64Peephole.peepholeOptimize liveTemporary <> liveTemporary then
        Error "Bit-clear peephole fused a sequence whose inverted temporary remains live"
    elif ARM64Peephole.peepholeOptimize liveMask <> liveMask then
        Error "Bit-clear peephole fused a sequence whose all-ones mask remains live"
    elif ARM64Peephole.peepholeOptimize overwrittenMask <> expectedWithOverwrite then
        Error "Bit-clear peephole did not fuse a sequence whose all-ones mask is overwritten"
    else
        Ok ()

/// Conditional block layout is not observable in an executable E2E test, so
/// inspect the symbolic branch sequence directly.
let testPeepholeFallsThroughToTrueTarget () : TestResult =
    let before = [
        ARM64Symbolic.B_cond_label (ARM64.LE, "true_target")
        ARM64Symbolic.B_label "false_target"
        ARM64Symbolic.Label "true_target"
    ]
    let expected = [
        ARM64Symbolic.B_cond_label (ARM64.GT, "false_target")
        ARM64Symbolic.Label "true_target"
    ]

    match ARM64Peephole.peepholeOptimize before with
    | actual when actual = expected -> Ok ()
    | actual ->
        let rendered =
            actual
            |> List.map TestDSL.PassTestRunner.prettyPrintARM64Instr
            |> String.concat "; "
        Error $"Expected inverted branch with true-target fallthrough, got {rendered}"

let testPeepholeCombinesOperandsAndMemoryPairs () : TestResult =
    let before = [
        ARM64Symbolic.LSL_imm (ARM64.X9, ARM64.X2, 3)
        ARM64Symbolic.ADD_reg (ARM64.X0, ARM64.X1, ARM64.X9)
        ARM64Symbolic.SXTW (ARM64.X10, ARM64.X3)
        ARM64Symbolic.ADD_reg (ARM64.X4, ARM64.X5, ARM64.X10)
        ARM64Symbolic.STR (ARM64.X11, ARM64.SP, 16s)
        ARM64Symbolic.STR (ARM64.X13, ARM64.SP, 24s)
    ]
    let expected = [
        ARM64Symbolic.ADD_shifted (ARM64.X0, ARM64.X1, ARM64.X2, 3)
        ARM64Symbolic.ADD_extended (ARM64.X4, ARM64.X5, ARM64.X3, ARM64.ExtendSXTW)
        ARM64Symbolic.STP (ARM64.X11, ARM64.X13, ARM64.SP, 16s)
    ]
    match ARM64Peephole.peepholeOptimize before with
    | actual when actual = expected -> Ok ()
    | actual -> Error $"Expected shifted/extended operands and paired memory operations, got {actual}"

let testPeepholeCombinesEveryExtensionAndFloatStorePairs () : TestResult =
    let extensionCases = [
        (ARM64Symbolic.UXTB (ARM64.X10, ARM64.X3), ARM64.ExtendUXTB)
        (ARM64Symbolic.UXTH (ARM64.X10, ARM64.X3), ARM64.ExtendUXTH)
        (ARM64Symbolic.UXTW (ARM64.X10, ARM64.X3), ARM64.ExtendUXTW)
        (ARM64Symbolic.SXTB (ARM64.X10, ARM64.X3), ARM64.ExtendSXTB)
        (ARM64Symbolic.SXTH (ARM64.X10, ARM64.X3), ARM64.ExtendSXTH)
        (ARM64Symbolic.SXTW (ARM64.X10, ARM64.X3), ARM64.ExtendSXTW)
    ]
    let extensionFailure =
        extensionCases
        |> List.tryPick (fun (extension, kind) ->
            let actual =
                ARM64Peephole.peepholeOptimize [
                    extension
                    ARM64Symbolic.ADD_reg (ARM64.X10, ARM64.X5, ARM64.X10)
                ]
            let expected = [ARM64Symbolic.ADD_extended (ARM64.X10, ARM64.X5, ARM64.X3, kind)]
            if actual = expected then None else Some $"{kind}: {actual}")
    match extensionFailure with
    | Some failure -> Error $"Expected every extension kind to fold into ADD_extended; failed {failure}"
    | None ->
        let actual =
            ARM64Peephole.peepholeOptimize [
                ARM64Symbolic.STR_fp (ARM64.D2, ARM64.SP, 32s)
                ARM64Symbolic.STR_fp (ARM64.D3, ARM64.SP, 40s)
            ]
        let expected = [ARM64Symbolic.STP_fp (ARM64.D2, ARM64.D3, ARM64.SP, 32s)]
        if actual = expected then Ok ()
        else Error $"Expected aligned Float stack stores to form STP_fp, got {actual}"

let testPeepholeCombinesShiftedSubtraction () : TestResult =
    let before = [
        ARM64Symbolic.LSL_imm (ARM64.X9, ARM64.X2, 3)
        ARM64Symbolic.SUB_reg (ARM64.X9, ARM64.X1, ARM64.X9)
    ]
    let expected = [
        ARM64Symbolic.SUB_shifted (ARM64.X9, ARM64.X1, ARM64.X2, 3)
    ]
    match ARM64Peephole.peepholeOptimize before with
    | actual when actual = expected -> Ok ()
    | actual -> Error $"Expected shifted subtraction to form SUB_shifted, got {actual}"

/// The multiply-by-constant selector creates this shared-source shape before
/// final block layout. A following branch must not hide the shift temporary's
/// established single-use contract from the target peephole.
let testPeepholePreservesSharedSourceShiftFusionAcrossBranch () : TestResult =
    let before = [
        ARM64Symbolic.LSL_imm (ARM64.X4, ARM64.X1, 1)
        ARM64Symbolic.ADD_reg (ARM64.X1, ARM64.X1, ARM64.X4)
        ARM64Symbolic.B_label "loop"
    ]
    let expected = [
        ARM64Symbolic.ADD_shifted (ARM64.X1, ARM64.X1, ARM64.X1, 1)
        ARM64Symbolic.B_label "loop"
    ]
    match ARM64Peephole.peepholeOptimize before with
    | actual when actual = expected -> Ok ()
    | actual -> Error $"Expected shared-source shifted ADD before control flow, got {actual}"

let testArm64FLoadEncodableConstantsUseImmediate () : TestResult =
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.FLoad (LIR.FPhysical LIR.D2, 1.0)
                LIR.FLoad (LIR.FPhysical LIR.D3, 4.0)
                LIR.FLoad (LIR.FPhysical LIR.D4, 0.0)
            ]
            Map.empty

    match generatePreparedARM64 target program with
    | Error e ->
        Error e
    | Ok instrs ->
        let hasOneImmediate =
            instrs
            |> List.exists (function
                | ARM64Symbolic.FMOV_imm (ARM64.D2, 1.0) ->
                    true
                | _ ->
                    false)
        let hasFourImmediate =
            instrs
            |> List.exists (function
                | ARM64Symbolic.FMOV_imm (ARM64.D3, 4.0) ->
                    true
                | _ ->
                    false)
        let hasLiteralLoad =
            instrs
            |> List.exists (function
                | ARM64Symbolic.ADRP (_, ARM64Symbolic.DataLabel (ARM64Symbolic.FloatLiteral 1.0))
                | ARM64Symbolic.ADD_label (_, _, ARM64Symbolic.DataLabel (ARM64Symbolic.FloatLiteral 1.0))
                | ARM64Symbolic.LDR_fp (ARM64.D2, ARM64.X9, 0s) ->
                    true
                | ARM64Symbolic.ADRP (_, ARM64Symbolic.DataLabel (ARM64Symbolic.FloatLiteral 4.0))
                | ARM64Symbolic.ADD_label (_, _, ARM64Symbolic.DataLabel (ARM64Symbolic.FloatLiteral 4.0))
                | ARM64Symbolic.LDR_fp (ARM64.D3, ARM64.X9, 0s) ->
                    true
                | _ ->
                    false)

        let hasZeroLiteralLoad =
            instrs
            |> List.exists (function
                | ARM64Symbolic.ADRP (_, ARM64Symbolic.DataLabel (ARM64Symbolic.FloatLiteral 0.0))
                | ARM64Symbolic.ADD_label (_, _, ARM64Symbolic.DataLabel (ARM64Symbolic.FloatLiteral 0.0))
                | ARM64Symbolic.LDR_fp (ARM64.D4, ARM64.X9, 0s) ->
                    true
                | _ ->
                    false)

        let hasZeroInstruction =
            instrs
            |> List.exists (function
                | ARM64Symbolic.FMOV_zero ARM64.D4 -> true
                | _ -> false)

        if not hasOneImmediate then
            Error "FLoad 1.0 did not emit a floating-point immediate"
        elif not hasFourImmediate then
            Error "FLoad 4.0 did not emit a floating-point immediate"
        elif hasLiteralLoad then
            Error "Encodable FLoad used a literal-pool load instead of an immediate"
        elif hasZeroLiteralLoad then
            Error "Positive-zero FLoad used a literal-pool load"
        elif not hasZeroInstruction then
            Error "Positive-zero FLoad did not use FMOV_zero"
        else
            Ok ()

/// RawAlloc should branch to a shared overflow label, rather than inlining
/// the full overflow trap sequence at each allocation site.
let testRawAllocUsesSharedHeapOverflowPath () : TestResult =
    match convertRawAlloc LIR.X0 LIR.X1 with
    | Error e -> Error $"Failed to convert RawAlloc: {e}"
    | Ok instrs ->
        let hasHeapEndCmp =
            instrs
            |> List.exists (function
                | ARM64Symbolic.CMP_reg (ARM64.X14, ARM64.X11) -> true
                | _ -> false)

        let hasInlineHeapEndRecompute =
            instrs
            |> List.exists (function
                | ARM64Symbolic.MOVZ (ARM64.X11, imm, 16) when imm = 0x2000us -> true
                | _ -> false)

        let hasOverflowLabelBranch =
            instrs
            |> List.exists (function
                | ARM64Symbolic.B_cond_label (ARM64.GT, _) -> true
                | _ -> false)

        let hasInlinedOverflowTrap =
            instrs
            |> List.exists (function
                | ARM64Symbolic.SVC _ -> true
                | _ -> false)

        if not hasHeapEndCmp then
            Error "Expected RawAlloc bounds check to compare next pointer against computed heap end in X11"
        else if not hasInlineHeapEndRecompute then
            Error "Expected RawAlloc bounds check to compute heap end in X11"
        else if not hasOverflowLabelBranch then
            Error "Expected RawAlloc bounds check to branch to shared overflow label (B_cond_label GT)"
        else if hasInlinedOverflowTrap then
            Error "RawAlloc still inlines overflow trap path (found SVC in fast path conversion)"
        else
            Ok ()

let testRuntimePrintStringLengthUsesFullImmediate () : TestResult =
    let instrs = ARM64PrintAndExit.generatePrintString target 65537

    let hasLowerLengthChunk =
        instrs
        |> List.exists (function
            | ARM64.MOVZ (ARM64.X2, 1us, 0) ->
                true
            | _ ->
                false)

    let hasUpperLengthChunk =
        instrs
        |> List.exists (function
            | ARM64.MOVK (ARM64.X2, 1us, 16) ->
                true
            | _ ->
                false)

    let truncatesLengthToLowChunkOnly =
        instrs
        |> List.forall (function
            | ARM64.MOVK (ARM64.X2, _, _) ->
                false
            | _ ->
                true)
        && instrs
        |> List.exists (function
            | ARM64.MOVZ (ARM64.X2, 0us, 0) ->
                true
            | ARM64.MOVZ (ARM64.X2, 1us, 0) ->
                true
            | _ ->
                false)

    if hasLowerLengthChunk && hasUpperLengthChunk then
        Ok ()
    elif truncatesLengthToLowChunkOnly then
        Error "Runtime print string length truncated 65537 bytes to a 16-bit low chunk"
    else
        Error "Runtime print string length did not emit both length chunks"

let testRawSlotInitPureEnumDoesNotEmitGenericRetain () : TestResult =
    let enumType = AST.TSum ("RawSlotInitPureEnum", [AST.TString])
    let variants : LIR.VariantRegistry =
        Map.ofList [
            ("RawSlotInitPureEnum",
                { TypeParams = ["a"]
                  Variants =
                    [
                        { Name = "RawSlotInitPureA"; Tag = 0; Payload = None }
                        { Name = "RawSlotInitPureB"; Tag = 1; Payload = None }
                    ] })
        ]
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RawSlotInit (
                    LIR.Physical LIR.X0,
                    LIR.Physical LIR.X1,
                    LIR.Physical LIR.X3,
                    enumType)
            ]
            variants

    match generatePreparedARM64 target program with
    | Error e ->
        Error e
    | Ok instrs ->
        let emittedGenericRetain =
            instrs
            |> List.exists (function
                | ARM64Symbolic.LDR (ARM64.X15, ARM64.X3, 16s)
                | ARM64Symbolic.LDR (ARM64.X14, ARM64.X3, 16s) ->
                    true
                | _ ->
                    false)
        if emittedGenericRetain then
            Error "RawSlotInit of a generic pure enum emitted a generic heap retain"
        else
            Ok ()

let testListTuple3BytesListDictListValueUsesTypedDictHelper () : TestResult =
    let tupleType = AST.TTuple [ AST.TBlob; AST.TList AST.TInt64; AST.TDict (AST.TInt64, AST.TList AST.TInt64) ]
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadata (AST.TList tupleType)))
            ]
            Map.empty

    match generatePreparedARM64 target program with
    | Error e ->
        Error e
    | Ok instrs ->
        let callsTypedDictListHelper =
            instrs
            |> List.exists (function
                | ARM64Symbolic.BL "__dark_dict_refcount_dec_list_value_helper" ->
                    true
                | _ ->
                    false)
        if callsTypedDictListHelper then
            Ok ()
        else
            Error "List of tuple(bytes, list, dict<int, list<int>>) did not emit typed dict-list value release helper"

let private assertListElementUsesTypedDictListHelper (elementType: AST.Type) (caseName: string) : TestResult =
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadata (AST.TList elementType)))
            ]
            Map.empty

    match generatePreparedARM64 target program with
    | Error e ->
        Error e
    | Ok instrs ->
        let callsTypedDictListHelper =
            instrs
            |> List.exists (function
                | ARM64Symbolic.BL "__dark_dict_refcount_dec_list_value_helper" ->
                    true
                | _ ->
                    false)
        if callsTypedDictListHelper then
            Ok ()
        else
            Error $"{caseName} did not emit typed dict-list value release helper"

let testListTuple3StringListDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TTuple [ AST.TString; AST.TList AST.TInt64; AST.TDict (AST.TInt64, AST.TList AST.TInt64) ])
        "List of tuple(string, list, dict<int, list<int>>)"

let testListTuple3ClosureListDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TTuple [
            AST.TFunction ([ AST.TInt64 ], AST.TInt64)
            AST.TList AST.TInt64
            AST.TDict (AST.TInt64, AST.TList AST.TInt64)
        ])
        "List of tuple(closure, list, dict<int, list<int>>)"

let testListTuple4StringBytesListDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TTuple [
            AST.TString
            AST.TBlob
            AST.TList AST.TInt64
            AST.TDict (AST.TInt64, AST.TList AST.TInt64)
        ])
        "List of tuple(string, bytes, list, dict<int, list<int>>)"

let testListTuple4ClosureStringListDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TTuple [
            AST.TFunction ([ AST.TInt64 ], AST.TInt64)
            AST.TString
            AST.TList AST.TInt64
            AST.TDict (AST.TInt64, AST.TList AST.TInt64)
        ])
        "List of tuple(closure, string, list, dict<int, list<int>>)"

let testListTuple4ClosureBytesListDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TTuple [
            AST.TFunction ([ AST.TInt64 ], AST.TInt64)
            AST.TBlob
            AST.TList AST.TInt64
            AST.TDict (AST.TInt64, AST.TList AST.TInt64)
        ])
        "List of tuple(closure, bytes, list, dict<int, list<int>>)"

let testListDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TDict (AST.TInt64, AST.TList AST.TInt64))
        "List of dict<int, list<int>>"

let testListDictStringPayloadUsesPlannedDictHelper () : TestResult =
    let listType = AST.TList (AST.TDict (AST.TString, AST.TString))
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadata listType))
            ]
            Map.empty

    match generatePreparedARM64 target program with
    | Error e ->
        Error e
    | Ok instrs ->
        let callsPlannedDictHelper =
            instrs
            |> List.exists (function
                | ARM64Symbolic.BL label when label.StartsWith("__dark_dict_refcount_dec_plan_") -> true
                | _ -> false)

        if not (emitsPlannedListHelperLabel instrs) then
            Error "List<Dict<String, String>> did not emit a planned list release helper"
        elif not callsPlannedDictHelper then
            Error "List<Dict<String, String>> did not call a planned dictionary release helper"
        else
            Ok ()

let testListNestedTupleDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TTuple [
            AST.TString
            AST.TBlob
            AST.TTuple [ AST.TDict (AST.TInt64, AST.TList AST.TInt64); AST.TString ]
            AST.TList AST.TInt64
        ])
        "List of tuple(string, bytes, tuple(dict<int, list<int>>, string), list<int>)"

let testListTuple2NestedTupleDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TTuple [
            AST.TInt64
            AST.TTuple [
                AST.TString
                AST.TBlob
                AST.TList AST.TInt64
                AST.TDict (AST.TInt64, AST.TList AST.TInt64)
            ]
        ])
        "List of tuple(int, tuple(string, bytes, list<int>, dict<int, list<int>>))"

let testListTuple4NestedTupleDynamicDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TTuple [
            AST.TInt64
            AST.TInt64
            AST.TInt64
            AST.TTuple [
                AST.TString
                AST.TList AST.TInt64
                AST.TDict (AST.TInt64, AST.TList AST.TInt64)
            ]
        ])
        "List of tuple(int, int, int, tuple(string, list<int>, dict<int, list<int>>))"

let testListTuple4NestedRecordMiddleDictListValueUsesTypedDictHelper () : TestResult =
    let listType = AST.TList AST.TInt64
    let dictType = AST.TDict (AST.TInt64, listType)
    let recordName = "ARM64ListRcNestedRecordMiddleStringListDictList"
    let nestedRecordType = AST.TRecord (recordName, [])
    let tupleType = AST.TTuple [ AST.TInt64; AST.TInt64; nestedRecordType; AST.TInt64 ]
    let records =
        Map.ofList [
            (recordName, [ ("name", AST.TString); ("items", listType); ("lookup", dictType) ])
        ]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadataWithRecords records (AST.TList tupleType)))
            ]
            records

    match generatePreparedARM64 target program with
    | Error e ->
        Error e
    | Ok instrs ->
        let callsTypedDictListHelper =
            instrs
            |> List.exists (function
                | ARM64Symbolic.BL "__dark_dict_refcount_dec_list_value_helper" ->
                    true
                | _ ->
                    false)
        if callsTypedDictListHelper then
            Ok ()
        else
            Error "List of tuple(int, int, record(string, list, dict<int, list<int>>), int) did not emit typed dict-list value release helper"

let testListTuple4NestedTupleClosureDictListValueUsesTypedDictHelper () : TestResult =
    assertListElementUsesTypedDictListHelper
        (AST.TTuple [
            AST.TInt64
            AST.TInt64
            AST.TInt64
            AST.TTuple [
                AST.TFunction ([ AST.TInt64 ], AST.TInt64)
                AST.TString
                AST.TList AST.TInt64
                AST.TDict (AST.TInt64, AST.TList AST.TInt64)
            ]
        ])
        "List of tuple(int, int, int, tuple(closure, string, list<int>, dict<int, list<int>>))"

let private assertListSumPayloadUsesTypedDictListHelper (payloadType: AST.Type) (caseName: string) : TestResult =
    let sanitizedName =
        caseName
            .Replace(" ", "")
            .Replace(",", "")
            .Replace("(", "")
            .Replace(")", "")
            .Replace("<", "")
            .Replace(">", "")
            .Replace("-", "")
    let sumName = $"ARM64{sanitizedName}"
    let sumType = AST.TSum (sumName, [])
    let variants : LIR.VariantRegistry =
        Map.ofList [
            (sumName,
                { TypeParams = []
                  Variants =
                    [
                        { Name = $"{sumName}Case"; Tag = 0; Payload = Some payloadType }
                    ] })
        ]
    let sumShapes : MemoryModel.RcSumShapeRegistry =
        Map.ofList [
            (sumName,
                { TypeParams = []
                  Payloads = [ 0, Some payloadType ] })
        ]
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadataWithSumShapes sumShapes (AST.TList sumType)))
            ]
            variants

    match generatePreparedARM64 target program with
    | Error e ->
        Error e
    | Ok instrs ->
        let callsTypedDictListHelper =
            instrs
            |> List.exists (function
                | ARM64Symbolic.BL "__dark_dict_refcount_dec_list_value_helper" ->
                    true
                | _ ->
                    false)
        if callsTypedDictListHelper then
            Ok ()
        else
            Error $"{caseName} sum payload did not emit typed dict-list value release helper"

let testListSumTuple3DictListValueUsesTypedDictHelper () : TestResult =
    assertListSumPayloadUsesTypedDictListHelper
        (AST.TTuple [ AST.TString; AST.TList AST.TInt64; AST.TDict (AST.TInt64, AST.TList AST.TInt64) ])
        "sum tuple3 string list dict-list"

let testListSumTuple4DictListValueUsesTypedDictHelper () : TestResult =
    assertListSumPayloadUsesTypedDictListHelper
        (AST.TTuple [ AST.TString; AST.TBlob; AST.TList AST.TInt64; AST.TDict (AST.TInt64, AST.TList AST.TInt64) ])
        "sum tuple4 string bytes list dict-list"

let testListSumTuple3ClosureDictListValueUsesTypedDictHelper () : TestResult =
    assertListSumPayloadUsesTypedDictListHelper
        (AST.TTuple [
            AST.TFunction ([ AST.TInt64 ], AST.TInt64)
            AST.TList AST.TInt64
            AST.TDict (AST.TInt64, AST.TList AST.TInt64)
        ])
        "sum tuple3 closure list dict-list"

let testListSumTuple4ClosureDictListValueUsesTypedDictHelper () : TestResult =
    assertListSumPayloadUsesTypedDictListHelper
        (AST.TTuple [
            AST.TFunction ([ AST.TInt64 ], AST.TInt64)
            AST.TBlob
            AST.TList AST.TInt64
            AST.TDict (AST.TInt64, AST.TList AST.TInt64)
        ])
        "sum tuple4 closure bytes list dict-list"

let testListSumTuple4ClosureStringDictListValueUsesTypedDictHelper () : TestResult =
    assertListSumPayloadUsesTypedDictListHelper
        (AST.TTuple [
            AST.TFunction ([ AST.TInt64 ], AST.TInt64)
            AST.TString
            AST.TList AST.TInt64
            AST.TDict (AST.TInt64, AST.TList AST.TInt64)
        ])
        "sum tuple4 closure string list dict-list"

let testDictDictListValueUsesPlannedDictHelper () : TestResult =
    let dictType = AST.TDict (AST.TInt64, AST.TDict (AST.TInt64, AST.TList AST.TInt64))
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.DictHeap,
                    Some (rcMetadata dictType))
            ]
            Map.empty

    match generatePreparedARM64 target program with
    | Error e ->
        Error e
    | Ok instrs ->
        let callsPlannedDictHelper =
            instrs
            |> List.exists (function
                | ARM64Symbolic.BL label when label.StartsWith("__dark_dict_refcount_dec_plan_") -> true
                | _ ->
                    false)
        let callsMatrixNestedDictListHelper =
            instrs
            |> List.exists (function
                | ARM64Symbolic.BL "__dark_dict_refcount_dec_dict_list_value_helper" -> true
                | _ -> false)
        if not callsPlannedDictHelper then
            Error "Dict<int, dict<int, list<int>>> did not emit a planned dict release helper"
        elif callsMatrixNestedDictListHelper then
            Error "Dict<int, dict<int, list<int>>> still emitted the typed nested dict-list helper"
        else
            Ok ()

let private assertDictRefCountDecUsesPlannedDictHelper
    (dictType: AST.Type)
    (caseName: string)
    : TestResult =
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.DictHeap,
                    Some (rcMetadata dictType))
            ]
            Map.empty

    match generatePreparedARM64 target program with
    | Error e ->
        Error e
    | Ok instrs ->
        let callsPlannedDictHelper =
            instrs
            |> List.exists (function
                | ARM64Symbolic.BL label when label.StartsWith("__dark_dict_refcount_dec_plan_") -> true
                | _ -> false)

        if callsPlannedDictHelper then
            Ok ()
        else
            Error $"{caseName} did not emit a planned dict release helper"

let testDictStringKeyUsesPlannedDictHelper () : TestResult =
    assertDictRefCountDecUsesPlannedDictHelper
        (AST.TDict (AST.TString, AST.TInt64))
        "Dict<string, int>"

let testDictStringValueUsesPlannedDictHelper () : TestResult =
    assertDictRefCountDecUsesPlannedDictHelper
        (AST.TDict (AST.TInt64, AST.TString))
        "Dict<int, string>"

let testDictStringKeyListValueUsesPlannedDictHelper () : TestResult =
    assertDictRefCountDecUsesPlannedDictHelper
        (AST.TDict (AST.TString, AST.TList AST.TInt64))
        "Dict<string, list<int>>"

let testDictStringKeyTupleValueUsesPlannedDictHelper () : TestResult =
    assertDictRefCountDecUsesPlannedDictHelper
        (AST.TDict (AST.TString, AST.TTuple [ AST.TString; AST.TList AST.TInt64 ]))
        "Dict<string, tuple<string, list<int>>>"

let testDictStringKeyValuePlannedHelperReleasesCollisionPayloads () : TestResult =
    let dictType = AST.TDict (AST.TString, AST.TString)
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.DictHeap,
                    Some (rcMetadata dictType))
            ]
            Map.empty

    match generatePreparedARM64 target program with
    | Error e ->
        Error e
    | Ok instrs ->
        let hasCollisionPayloadLoop =
            instrs
            |> List.exists (function
                | ARM64Symbolic.Label label
                    when label.Contains("collision_payload_loop") ->
                    true
                | _ ->
                    false)

        if hasCollisionPayloadLoop then
            Ok ()
        else
            Error "Dict<string, string> planned helper did not emit a collision payload release loop"
