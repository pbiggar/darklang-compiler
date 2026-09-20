// ReleasePlanningTests.fs - Verify planned release outlining and cache behavior.

module ARM64ReleasePlanningTests

open ARM64CodeGenFixtures

/// Native record descriptors are compile-time metadata. This fixture locks the
/// compact payload layout at ARM64 codegen: fields begin at byte zero and no
/// descriptor immediate is materialized in the heap object.
let testCompactRecordFieldsStartAtOffsetZero () : TestResult =
    let records : LIR.RecordRegistry =
        Map.ofList [ ("Arm64CompactRecord", [("left", AST.TInt64); ("right", AST.TInt64)]) ]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.HeapAlloc (LIR.Physical LIR.X1, 16)
                LIR.HeapStore (LIR.Physical LIR.X1, 0, LIR.Imm 10L, None)
                LIR.HeapStore (LIR.Physical LIR.X1, 8, LIR.Imm 20L, None)
            ]
            records

    match generatePreparedARM64 target program with
    | Error error -> Error $"Compact record ARM64 lowering failed: {error}"
    | Ok instructions ->
        let fieldStoreOffsets =
            instructions
            |> List.choose (function
                | ARM64Symbolic.STR (ARM64.X9, ARM64.X1, offset) -> Some offset
                | _ -> None)
        if fieldStoreOffsets = [0s; 8s] then
            Ok ()
        else
            Error $"Expected compact record field stores at offsets 0 and 8 only, got {fieldStoreOffsets}"

let internal emitsPlannedListHelperLabel (instrs: ARM64Symbolic.Instr list) : bool =
    instrs
    |> List.exists (function
        | ARM64Symbolic.Label label
        | ARM64Symbolic.BL label ->
            label.StartsWith("__dark_list_refcount_dec_plan_")
        | _ ->
            false)

let testSmallGenericReleasePlanRemainsInline () : TestResult =
    let valueType = AST.TTuple [ AST.TString ]
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    8,
                    LIR.GenericHeap,
                    Some (rcMetadata valueType))
            ]
            Map.empty
        |> ARM64PrepareFunctions.prepareARM64Program
    let generatedCacheEntries = ResizeArray<string>()
    let cache
        (func: LIR.Function)
        (generate: unit -> Result<ARM64Symbolic.Instr list, string>)
        : Result<ARM64Symbolic.Instr list, string> =
        generatedCacheEntries.Add func.Name
        generate ()

    match CodeGen.generateARM64WithOptionsAndCache
              target
              ARM64CodeGenTypes.defaultOptions
              (Some cache)
              None
              program with
    | Error error -> Error $"Small generic release lowering failed: {error}"
    | Ok _ ->
        if Seq.toList generatedCacheEntries = ["_start"] then
            Ok ()
        else
            Error $"Small generic release plan should cache only the stable entry trampoline, got {Seq.toList generatedCacheEntries}"

let testExpensiveGenericReleaseIsPreparedAsCall () : TestResult =
    let valueType = AST.TTuple (List.replicate 32 AST.TString)
    let source = LIR.Virtual 42
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    source,
                    256,
                    LIR.GenericHeap,
                    Some (rcMetadata valueType))
            ]
            Map.empty
        |> ARM64PrepareFunctions.prepareARM64Program
    let (LIR.Program (functions, _, _)) = program
    let helperIdsByName =
        let helperNames =
            functions
            |> List.collect (fun func ->
                func.CodegenFacts
                |> Option.bind (fun facts -> facts.Arm64RcHelperRequirements)
                |> Option.map (fun requirements -> requirements.PlannedGenericDecHelpers |> Map.keys |> Seq.toList)
                |> Option.defaultValue [])
        AST.allocateFunctionIds (functions |> List.map (fun func -> func.Id)) helperNames
    let instructions =
        functions
        |> List.collect (fun func ->
            func.CFG.Blocks
            |> Map.values
            |> Seq.collect (fun block -> block.Instrs)
            |> Seq.toList)
    let helperIds =
        functions
        |> List.collect (fun func ->
            func.CodegenFacts
            |> Option.bind (fun facts -> facts.Arm64RcHelperRequirements)
            |> Option.map (fun requirements ->
                requirements.PlannedGenericDecHelpers
                |> Map.keys
                |> Seq.choose (fun name -> Map.tryFind name helperIdsByName)
                |> Seq.toList)
            |> Option.defaultValue [])
        |> Set.ofList
    match instructions with
    | [ LIR.SaveRegs ([], [])
        LIR.ArgMoves [(LIR.X0, LIR.Reg argMoveSource)]
        LIR.Call (LIR.Physical LIR.X0, helperLabel, [LIR.Reg callSource])
        LIR.RestoreRegs ([], []) ]
        when argMoveSource = source
             && callSource = source
             && Set.contains helperLabel helperIds ->
        Ok ()
    | _ ->
        Error $"Expected an expensive generic release to become one allocator-visible helper call, got {instructions}"

let testGenericReleaseHelperPreservesCachedInstructions () : TestResult =
    let valueType = AST.TTuple (List.replicate 32 AST.TString)
    let metadata = rcMetadata valueType
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (LIR.Physical LIR.X0, 256, LIR.GenericHeap, Some metadata)
                LIR.RefCountDec (LIR.Physical LIR.X0, 256, LIR.GenericHeap, Some metadata)
            ]
            Map.empty
        |> ARM64PrepareFunctions.prepareARM64Program
    let target = ARM64.targetConfigFor Platform.LinuxARM64
    let generatedFunctions = ResizeArray<string>()
    let entries =
        System.Collections.Generic.Dictionary<
            LIR.Function,
            Result<ARM64Symbolic.Instr list, string>>()
    let cache
        (func: LIR.Function)
        (generate: unit -> Result<ARM64Symbolic.Instr list, string>)
        : Result<ARM64Symbolic.Instr list, string> =
        match entries.TryGetValue func with
        | true, result ->
            result
        | false, _ ->
            let result = generate ()
            generatedFunctions.Add func.Name
            entries.[func] <- result
            result

    match CodeGen.generateARM64WithOptions target ARM64CodeGenTypes.defaultOptions program,
          CodeGen.generateARM64WithOptionsAndCache
              target
              ARM64CodeGenTypes.defaultOptions
              (Some cache)
              None
              program with
    | Error error, _
    | _, Error error ->
        Error $"Generic release helper lowering failed: {error}"
    | Ok uncachedProgram, Ok cachedProgram ->
        let uncached = CodeGen.generatedProgramInstructions uncachedProgram
        let cached = CodeGen.generatedProgramInstructions cachedProgram
        let plannedCalls =
            cached
            |> List.choose (function
                | ARM64Symbolic.BL label when label.StartsWith("__dark_generic_refcount_dec_plan_") ->
                    Some label
                | _ -> None)
        let plannedLabels =
            cached
            |> List.choose (function
                | ARM64Symbolic.Label label when label.StartsWith("__dark_generic_refcount_dec_plan_") ->
                    Some label
                | _ -> None)

        if cached <> uncached then
            Error "Caching changed outlined generic release instructions"
        elif generatedFunctions.Count <> 2
             || generatedFunctions.[0] <> "_start"
             || not (generatedFunctions.[1].StartsWith("__dark_generic_refcount_dec_plan_")) then
            Error $"Expected the caller and one generic helper in the function cache, got {Seq.toList generatedFunctions}"
        else
            match plannedCalls, plannedLabels with
            | [firstCall; secondCall], [helperLabel]
                when firstCall = helperLabel && secondCall = helperLabel ->
                Ok ()
            | _ ->
                Error
                    $"Expected two calls to one generic release helper, got calls={plannedCalls}; labels={plannedLabels}"

let testOutlinedGenericReleaseUsesAllocatorLiveness () : TestResult =
    let valueType = AST.TTuple (List.replicate 32 AST.TString)
    let liveAcrossCall = LIR.Virtual 40
    let released = LIR.Virtual 41
    let result = LIR.Virtual 42
    let prepared =
        makeSimpleProgramWithVariants
            [
                LIR.Mov (liveAcrossCall, LIR.Imm 10L)
                LIR.Mov (released, LIR.Imm 0L)
                LIR.RefCountDec (
                    released,
                    256,
                    LIR.GenericHeap,
                    Some (rcMetadata valueType))
                LIR.Add (result, liveAcrossCall, LIR.Imm 1L)
            ]
            Map.empty
        |> ARM64PrepareFunctions.prepareARM64Program
    let (LIR.Program (functions, variants, records)) = prepared
    let allocatedFunctions =
        functions
        |> List.map (RegisterAllocation.allocateRegisters Platform.ARM64)
    let allocatedProgram = LIR.Program (allocatedFunctions, variants, records)
    let allocatedInstrs =
        allocatedFunctions
        |> List.collect (fun func ->
            func.CFG.Blocks
            |> Map.values
            |> Seq.collect (fun block -> block.Instrs)
            |> Seq.toList)
    let saves =
        allocatedInstrs
        |> List.choose (function
            | LIR.SaveRegs (intRegs, floatRegs) -> Some (intRegs, floatRegs)
            | _ -> None)

    match CodeGen.generateARM64 target allocatedProgram with
    | Error error ->
        Error $"Allocated generic release helper lowering failed: {error}"
    | Ok generated ->
        let instructions = CodeGen.generatedProgramInstructions generated
        let savesEveryAllocatableRegister =
            instructions
            |> List.exists (function
                | ARM64Symbolic.STP_pre (_, _, stackReg, offset)
                    when stackReg = ARM64Symbolic.SP && offset = -128s -> true
                | _ -> false)
        match saves with
        | [(intRegs, [])] when List.length intRegs < 7 && not savesEveryAllocatableRegister ->
            Ok ()
        | _ ->
            Error
                $"Expected allocator-selected caller saves, got saves={saves}; emittedSaveAll={savesEveryAllocatableRegister}"

let testGenericReleaseHelpersPreserveOwnershipPolicy () : TestResult =
    let sumName = "ARM64OutlinedOwnership"
    let payloadType = AST.TTuple (List.replicate 32 AST.TString)
    let sumType = AST.TSum (sumName, [])
    let variants : LIR.VariantRegistry =
        Map.ofList [
            (sumName,
             { TypeParams = []
               Variants = [
                   { Name = "Only"; Tag = 0; Payload = Some payloadType }
               ] })
        ]
    let sumShapes : MemoryModel.RcSumShapeRegistry =
        Map.ofList [
            (sumName,
             { TypeParams = []
               Payloads = [0, Some payloadType] })
        ]
    let metadata = rcMetadataWithSumShapes sumShapes sumType
    let makeFunction (name: string) : LIR.Function =
        let entry = LIR.Label $"{name}_entry"
        { Id = TestIds.functionIdForName name
          Name = name
          TypedParams = []
          CFG = {
              Entry = entry
              Blocks =
                  Map.ofList [
                      (entry,
                       { Label = entry
                         Instrs = [
                             LIR.RefCountDec (
                                 LIR.Physical LIR.X0,
                                 264,
                                 LIR.GenericHeap,
                                 Some metadata)
                         ]
                         Terminator = LIR.Ret })
                  ]
          }
          StackSize = 0
          UsedCalleeSaved = []
          CodegenFacts = None }
    let prepared =
        LIR.Program (
            [ makeFunction "User.owns"; makeFunction "Darklang.Stdlib.List.borrows" ],
            variants,
            Map.empty)
        |> ARM64PrepareFunctions.prepareARM64Program
    let (LIR.Program (functions, _, _)) = prepared
    let helperIdsByName =
        let helperNames =
            functions
            |> List.collect (fun func ->
                func.CodegenFacts
                |> Option.bind (fun facts -> facts.Arm64RcHelperRequirements)
                |> Option.map (fun requirements -> requirements.PlannedGenericDecHelpers |> Map.keys |> Seq.toList)
                |> Option.defaultValue [])
        AST.allocateFunctionIds (functions |> List.map (fun func -> func.Id)) helperNames
    let helperInfo
        (func: LIR.Function)
        : string option * LIR.Arm64PlannedGenericDecHelper list =
        let callLabel =
            let helperNamesById =
                func.CodegenFacts
                |> Option.bind (fun facts -> facts.Arm64RcHelperRequirements)
                |> Option.map (fun requirements ->
                    requirements.PlannedGenericDecHelpers
                    |> Map.keys
                    |> Seq.choose (fun name -> Map.tryFind name helperIdsByName |> Option.map (fun id -> id, name))
                    |> Map.ofSeq)
                |> Option.defaultValue Map.empty
            func.CFG.Blocks
            |> Map.values
            |> Seq.collect (fun block -> block.Instrs)
            |> Seq.tryPick (function
                | LIR.Call (_, id, _) -> Map.tryFind id helperNamesById
                | _ -> None)
        let specs =
            func.CodegenFacts
            |> Option.bind (fun facts -> facts.Arm64RcHelperRequirements)
            |> Option.map (fun requirements -> requirements.PlannedGenericDecHelpers |> Map.values |> Seq.toList)
            |> Option.defaultValue []
        callLabel, specs
    match functions |> List.map helperInfo with
    | [ (Some ownedLabel, [ownedSpec]); (Some borrowedLabel, [borrowedSpec]) ]
        when ownedLabel.EndsWith("_owned")
             && borrowedLabel.EndsWith("_borrowed")
             && ownedSpec.OwnsSinglePayloadSum
             && not borrowedSpec.OwnsSinglePayloadSum ->
        Ok ()
    | actual ->
        Error $"Expected distinct owned and borrowed generic release helpers, got {actual}"
