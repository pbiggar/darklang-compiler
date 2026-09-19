// ClosureReferenceCounts.fs - Generate closure and recursive-payload lifetime helpers.

module X64ClosureReferenceCounts

open X64Operands
open X64CodeGenTypes
open X64ReleaseSelection
open X64FieldReferenceCounts
open X64ListReferenceCounts

let internal closurePayloadSizesFromAllocs (functions: LIR.Function list) : Map<AST.FunctionId, int> =
    functions
    |> List.collect (fun func ->
        func.CFG.Blocks
        |> Map.toList
        |> List.collect (fun (_, block) ->
            block.Instrs
            |> List.choose (function
                | LIR.ClosureAlloc (_, funcName, captures) ->
                    Some (funcName, (List.length captures + 1) * 8)
                | _ ->
                    None)))
    |> Map.ofList

let internal closureCaptureTypesFromParams (functions: LIR.Function list) : Map<string, AST.Type list> =
    functions
    |> List.choose (fun func ->
        match func.TypedParams with
        | { Type = AST.TTuple (_funcPtrType :: captures) } :: _ ->
            Some (func.Name, captures)
        | _ ->
            None)
    |> Map.ofList

let internal closurePayloadSizesFromParams (functions: LIR.Function list) : Map<string, int> =
    functions
    |> List.choose (fun func ->
        match func.TypedParams with
        | { Type = AST.TTuple fields } :: _ ->
            Some (func.Name, List.length fields * 8)
        | _ ->
            None)
    |> Map.ofList

let internal generateClosureRefCountIncHelper (closurePayloadSizes: Map<string, int>) : X86_64.Instr list =
    let label name = $"__dark_closure_rc_inc_{name}"
    let helperRet = label "ret"
    let payloadReady = label "payload_ready"

    let payloadCases =
        closurePayloadSizes
        |> Map.toList
        |> List.filter (fun (_, payloadSize) -> payloadSize <> 8)
        |> List.mapi (fun index (funcName, payloadSize) ->
            let nextCase = label $"payload_next_{index}"
            [X86_64.LEA_rip (X86_64.R10, funcName)
             X86_64.CMP_reg (X86_64.RDX, X86_64.R10)
             X86_64.Jcc (X86_64.NE, nextCase)
             X86_64.MOV_imm32 (X86_64.RCX, int32 payloadSize)
             X86_64.JMP payloadReady
             X86_64.Label nextCase])
        |> List.concat

    [X86_64.Label closureRefCountIncHelperLabel
     X86_64.TEST_reg (X86_64.RAX, X86_64.RAX)
     X86_64.Jcc (X86_64.EQ, helperRet)
     X86_64.MOV_load (X86_64.RDX, X86_64.RAX, 0)
     X86_64.MOV_imm32 (X86_64.RCX, 8)]
    @ payloadCases
    @ [X86_64.Label payloadReady
       X86_64.MOV_reg (X86_64.RDI, X86_64.RAX)
       X86_64.ADD_reg (X86_64.RDI, X86_64.RCX)
       X86_64.MOV_load (X86_64.RDX, X86_64.RDI, 0)
       X86_64.ADD_imm (X86_64.RDX, 1)
       X86_64.MOV_store (X86_64.RDI, 0, X86_64.RDX)
       X86_64.Label helperRet
       X86_64.RET]

let internal generateClosureRefCountDecHelper
    (enableLeakCheck: bool)
    (recordRegistry: LIR.RecordRegistry)
    (sumShapeRegistry: MemoryModel.RcSumShapeRegistry)
    (closurePayloadSizes: Map<string, int>)
    (closureCaptureTypes: Map<string, AST.Type list>)
    : X86_64.Instr list =
    let label name = $"__dark_closure_rc_dec_{name}"
    let helperRet = label "ret"
    let payloadReady = label "payload_ready"
    let skipFreeList = label "skip_freelist"
    let capturesReleased = label "captures_released"

    let leakDec =
        if enableLeakCheck then
            [X86_64.PUSH scratch
             X86_64.PUSH X86_64.RCX
             X86_64.LEA_rip (scratch, "_leak_count")
             X86_64.MOV_load (X86_64.RCX, scratch, 0)
             X86_64.SUB_imm (X86_64.RCX, 1)
             X86_64.MOV_store (scratch, 0, X86_64.RCX)
             X86_64.POP X86_64.RCX
             X86_64.POP scratch]
        else
            []

    let helperCtx : FuncCtx = {
        FunctionName = closureRefCountDecHelperLabel
        StackSize = 0
        UsedCalleeSaved = []
        EnableLeakCheck = enableLeakCheck
        RecordRegistry = recordRegistry
        SumShapeRegistry = sumShapeRegistry
        FunctionNames = Map.empty
    }

    let payloadCases =
        closurePayloadSizes
        |> Map.toList
        |> List.filter (fun (_, payloadSize) -> payloadSize <> 8)
        |> List.mapi (fun index (funcName, payloadSize) ->
            let nextCase = label $"payload_next_{index}"
            [X86_64.LEA_rip (X86_64.R10, funcName)
             X86_64.CMP_reg (X86_64.RDX, X86_64.R10)
             X86_64.Jcc (X86_64.NE, nextCase)
             X86_64.MOV_imm32 (X86_64.RCX, int32 payloadSize)
             X86_64.JMP payloadReady
             X86_64.Label nextCase])
        |> List.concat

    let releaseDynamicBufferCapture (fieldOffset: int) (suffix: string) : X86_64.Instr list =
        let doneLabel = label $"capture_dynamic_done_{suffix}"
        [X86_64.MOV_load (X86_64.R9, X86_64.RAX, fieldOffset)
         X86_64.TEST_reg (X86_64.R9, X86_64.R9)
         X86_64.Jcc (X86_64.EQ, doneLabel)
         X86_64.MOV_reg (X86_64.R10, X86_64.R9)
         X86_64.MOV_load (X86_64.RDX, X86_64.R10, 0)]
        @ loadImm64 scratch 0x7FFFFFFFFFFFFFFFL
        @ [X86_64.CMP_reg (X86_64.RDX, scratch)
           X86_64.Jcc (X86_64.EQ, doneLabel)
           X86_64.SUB_imm (X86_64.RDX, 1)
           X86_64.MOV_store (X86_64.R10, 0, X86_64.RDX)
           X86_64.TEST_reg (X86_64.RDX, X86_64.RDX)
           X86_64.Jcc (X86_64.NE, doneLabel)]
        @ leakDec
        @ [X86_64.Label doneLabel]

    let releaseHeapRootCapture (fieldOffset: int) (helperLabel: string) (suffix: string) : X86_64.Instr list =
        let doneLabel = label $"capture_root_done_{suffix}"
        let saveRegs = [X86_64.RAX; X86_64.RCX; X86_64.RDX; X86_64.RDI; X86_64.RSI; X86_64.R8; X86_64.R9; X86_64.R10; scratch]
        let saves = saveRegs |> List.map X86_64.PUSH
        let restores = saveRegs |> List.rev |> List.map X86_64.POP
        [X86_64.MOV_load (X86_64.R9, X86_64.RAX, fieldOffset)
         X86_64.TEST_reg (X86_64.R9, X86_64.R9)
         X86_64.Jcc (X86_64.EQ, doneLabel)]
        @ saves
        @ [X86_64.MOV_reg (X86_64.RAX, X86_64.R9)
           X86_64.CALL helperLabel]
        @ restores
        @ [X86_64.Label doneLabel]

    let releaseFixedBlockCapture (fieldOffset: int) (captureType: AST.Type) : X86_64.Instr list =
        match tryRcReleasePlanOfType recordRegistry sumShapeRegistry captureType with
        | Some (MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, (MemoryModel.FixedBlockPayloadRelease _ | MemoryModel.BoxedSumPayloadRelease _)) as releasePlan) ->
            [X86_64.MOV_load (X86_64.R9, X86_64.RAX, fieldOffset)
             X86_64.PUSH X86_64.RAX]
            @ genRefCountDecGenericWithPlan helperCtx X86_64.R9 payloadSize (Some releasePlan)
            @ [X86_64.POP X86_64.RAX]
        | _ ->
            []

    let releaseCaptureCases =
        closureCaptureTypes
        |> Map.toList
        |> List.mapi (fun index (funcName, captureTypes) ->
            let nextCase = label $"captures_next_{index}"
            let releases =
                captureTypes
                |> List.mapi (fun captureIndex captureType ->
                    let fieldOffset = (captureIndex + 1) * 8
                    match captureType with
                    | AST.TString
                    | AST.TChar
                    | AST.TInt
                    | AST.TBlob ->
                        releaseDynamicBufferCapture fieldOffset $"{index}_{captureIndex}"
                    | AST.TList _ ->
                        releaseHeapRootCapture fieldOffset (listDecHelperForType recordRegistry sumShapeRegistry captureType) $"{index}_{captureIndex}_list"
                    | AST.TDict _ ->
                        match tryRcReleasePlanOfType recordRegistry sumShapeRegistry captureType with
                        | Some releasePlan ->
                            releaseHeapRootCapture fieldOffset (dictDecHelperForReleasePlan releasePlan) $"{index}_{captureIndex}_dict"
                        | None ->
                            Crash.crash $"generateClosureRefCountDecHelper: missing RC metadata for dict capture type {captureType}"
                    | AST.TFunction _ ->
                        releaseHeapRootCapture fieldOffset closureRefCountDecHelperLabel $"{index}_{captureIndex}_closure"
                    | AST.TStream _ ->
                        releaseHeapRootCapture fieldOffset streamRefCountDecHelperLabel $"{index}_{captureIndex}_stream"
                    | AST.TTuple _
                    | AST.TRecord _
                    | AST.TSum _ ->
                        releaseFixedBlockCapture fieldOffset captureType
                    | _ ->
                        [])
                |> List.concat
            [X86_64.LEA_rip (X86_64.R9, funcName)
             X86_64.CMP_reg (X86_64.R8, X86_64.R9)
             X86_64.Jcc (X86_64.NE, nextCase)]
            @ releases
            @ [X86_64.JMP capturesReleased
               X86_64.Label nextCase])
        |> List.concat

    [X86_64.Label closureRefCountDecHelperLabel
     X86_64.TEST_reg (X86_64.RAX, X86_64.RAX)
     X86_64.Jcc (X86_64.EQ, helperRet)
     X86_64.MOV_load (X86_64.RDX, X86_64.RAX, 0)
     X86_64.MOV_imm32 (X86_64.RCX, 8)]
    @ payloadCases
    @ [X86_64.Label payloadReady
       X86_64.MOV_reg (X86_64.RDI, X86_64.RAX)
       X86_64.ADD_reg (X86_64.RDI, X86_64.RCX)
       X86_64.MOV_load (X86_64.RDX, X86_64.RDI, 0)
       X86_64.SUB_imm (X86_64.RDX, 1)
       X86_64.MOV_store (X86_64.RDI, 0, X86_64.RDX)
       X86_64.TEST_reg (X86_64.RDX, X86_64.RDX)
       X86_64.Jcc (X86_64.NE, helperRet)
       X86_64.MOV_load (X86_64.R8, X86_64.RAX, 0)]
    @ releaseCaptureCases
    @ [X86_64.Label capturesReleased
       X86_64.CMP_imm (X86_64.RCX, freeListSize)
       X86_64.Jcc (X86_64.GE, skipFreeList)
       X86_64.MOV_reg (X86_64.RDI, freeListBase)
       X86_64.ADD_reg (X86_64.RDI, X86_64.RCX)
       X86_64.MOV_load (X86_64.RDX, X86_64.RDI, 0)
       X86_64.MOV_store (X86_64.RAX, 0, X86_64.RDX)
       X86_64.MOV_store (X86_64.RDI, 0, X86_64.RAX)
       X86_64.Label skipFreeList]
    @ leakDec
    @ [X86_64.Label helperRet
       X86_64.RET]
