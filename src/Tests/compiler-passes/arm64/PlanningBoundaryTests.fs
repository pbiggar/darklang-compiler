// PlanningBoundaryTests.fs - Verify code-generation planning preconditions and cost attribution.

module ARM64PlanningBoundaryTests

open ARM64CodeGenFixtures

let testRejectsUnpreparedCodegenFacts () : TestResult =
    let unprepared =
        makeSimpleProgramWithVariants [LIR.Mov (LIR.Physical LIR.X0, LIR.Imm 1L)] Map.empty
    let commonFactsOnly = LIR.attachCodegenFacts unprepared

    match CodeGen.generateARM64 target unprepared,
          CodeGen.generateARM64 target commonFactsOnly with
    | Error missingFacts, Error missingPlan
        when missingFacts.Contains "has no codegen facts"
             && missingPlan.Contains "has no ARM64 helper plan" ->
        Ok ()
    | first, second ->
        Error $"Expected unprepared ARM64 LIR to be rejected, got facts={first}; plan={second}"

let testLirOpExpansionRecorderAttributesGeneratedInstructions () : TestResult =
    let observations = ResizeArray<string * string * string * int * int64>()
    let ctx : ARM64CodeGenTypes.CodeGenContext = {
        Target = target
        Options = ARM64CodeGenTypes.defaultOptions
        SumShapeRegistry = Map.empty
        RecordRegistry = Map.empty
        RawSlotInitRetainTargets = None
        ClosurePayloadSizes = Map.empty
        ClosureCaptureTypes = Map.empty
        FunctionNames = Map.empty
        FunctionName = "lir_op_profile"
        InstructionSite = ""
        StackSize = 0
        UsedCalleeSaved = []
        HeapOverflowLabel = "__heap_oom_lir_op_profile"
        RecordLirOpExpansion =
            Some (fun functionName opcode detail instructionCount elapsedTicks ->
                observations.Add(functionName, opcode, detail, instructionCount, elapsedTicks))
    }
    let label = LIR.Label "lir_op_profile_entry"
    let block : LIR.BasicBlock = {
        Label = label
        Instrs = [LIR.PrintHeapString (LIR.Physical LIR.X0)]
        Terminator = LIR.Ret
    }

    match ARM64Blocks.convertBlock ctx "_epilogue_lir_op_profile" None block with
    | Error error -> Error error
    | Ok _ ->
        match observations |> Seq.toList with
        | [("lir_op_profile", "PrintHeapString", "", 16, elapsedTicks)] when elapsedTicks >= 0L -> Ok ()
        | actual ->
            Error $"Expected one attributed 16-instruction PrintHeapString expansion, got {actual}"
