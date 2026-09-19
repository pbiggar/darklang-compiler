// FieldReferenceCounts.fs - Generate shape-directed field and fixed-block destruction.

module X64FieldReferenceCounts

open X64Operands
open X64CodeGenTypes
open X64ReleaseSelection

let rec private genFieldReleases
    (recursiveHelperLabel: AST.Type -> string)
    (preserveRegisters: bool)
    (ctx: FuncCtx)
    (fieldReleases: MemoryModel.RcFieldRelease list)
    : X86_64.Instr list =
    fieldReleases
    |> List.collect (function
        | MemoryModel.FieldRelease (fieldOffset, fieldReleasePlan) ->
            match fieldReleasePlan with
            | MemoryModel.DynamicBufferRelease _ ->
                genDynamicBufferFieldRelease ctx fieldOffset
            | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
                genDictFieldRelease fieldOffset fieldReleasePlan
            | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
                genClosureFieldRelease fieldOffset
            | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) ->
                [X86_64.PUSH X86_64.RDX
                 X86_64.MOV_load (X86_64.RAX, X86_64.RDX, fieldOffset)
                 X86_64.CALL streamRefCountDecHelperLabel
                 X86_64.POP X86_64.RDX]
            | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
                genListFieldRelease fieldOffset fieldReleasePlan
            | MemoryModel.RootRelease (childPayloadSize, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease _)
            | MemoryModel.RootRelease (childPayloadSize, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease _) ->
                genFixedBlockFieldRelease recursiveHelperLabel preserveRegisters ctx fieldOffset childPayloadSize fieldReleasePlan
            | MemoryModel.RecursiveRelease sourceType ->
                [X86_64.PUSH X86_64.RDX
                 X86_64.MOV_load (X86_64.RAX, X86_64.RDX, fieldOffset)
                 X86_64.CALL (recursiveHelperLabel sourceType)
                 X86_64.POP X86_64.RDX]
            | _ ->
                [])

and private genBoxedSumVariantFieldReleases
    (recursiveHelperLabel: AST.Type -> string)
    (preserveRegisters: bool)
    (ctx: FuncCtx)
    (variants: MemoryModel.RcBoxedSumVariantRelease list)
    : X86_64.Instr list =
    let releaseVariant (variant: MemoryModel.RcBoxedSumVariantRelease) : (int * X86_64.Instr list) option =
        let releaseInstrs = genFieldReleases recursiveHelperLabel preserveRegisters ctx variant.FieldReleases

        if List.isEmpty releaseInstrs then
            None
        else
            Some (variant.Tag, releaseInstrs)

    let cases = variants |> List.choose releaseVariant

    if List.isEmpty cases then
        []
    else
        let doneLabel = freshLabel "rc_dec_sum_done"
        [X86_64.MOV_load (X86_64.R10, X86_64.RDX, 0)]
        @
        (cases
         |> List.mapi (fun index (tag, releaseInstrs) ->
            let nextCaseLabel = freshLabel $"rc_dec_sum_case_{index}_next"
            [X86_64.CMP_imm (X86_64.R10, tag)
             X86_64.Jcc (X86_64.NE, nextCaseLabel)]
            @ releaseInstrs
            @ [X86_64.JMP doneLabel
               X86_64.Label nextCaseLabel])
         |> List.concat)
        @ [X86_64.Label doneLabel]

and private genFixedBlockFieldReleases
    (recursiveHelperLabel: AST.Type -> string)
    (preserveRegisters: bool)
    (ctx: FuncCtx)
    (releasePlan: MemoryModel.RcReleasePlan option)
    : X86_64.Instr list =
    match releasePlan with
    | Some (MemoryModel.RootRelease (_, _, MemoryModel.FixedBlockPayloadRelease (_, plannedFieldReleases))) ->
        genFieldReleases recursiveHelperLabel preserveRegisters ctx plannedFieldReleases
    | Some (MemoryModel.RootRelease (_, _, MemoryModel.BoxedSumPayloadRelease (_, plannedFieldReleases, []))) ->
        genFieldReleases recursiveHelperLabel preserveRegisters ctx plannedFieldReleases
    | Some (MemoryModel.RootRelease (_, _, MemoryModel.BoxedSumPayloadRelease (_, _, variants))) ->
        genBoxedSumVariantFieldReleases recursiveHelperLabel preserveRegisters ctx variants
    | _ ->
        []

and private genFixedBlockFieldRelease
    (recursiveHelperLabel: AST.Type -> string)
    (preserveRegisters: bool)
    (ctx: FuncCtx)
    (fieldOffset: int)
    (childPayloadSize: int)
    (fieldReleasePlan: MemoryModel.RcReleasePlan)
    : X86_64.Instr list =
        let saveParent = if preserveRegisters then [] else [X86_64.PUSH X86_64.RDX]
        let restoreParent = if preserveRegisters then [] else [X86_64.POP X86_64.RDX]
        saveParent
        @ [X86_64.MOV_load (X86_64.R8, X86_64.RDX, fieldOffset)]
        @ genRefCountDecGenericWithPlanUsing recursiveHelperLabel preserveRegisters ctx X86_64.R8 childPayloadSize (Some fieldReleasePlan)
        @ restoreParent

/// Generic RefCountDec: decrement refcount at [addr + payloadSize].
/// If zero, release known fields, free block to free list, and update leak accounting.
/// Public lowering preserves scratch registers; recursive workers preserve only
/// parent roots at nested fixed-block boundaries to keep deep release bounded.
and private genRefCountDecGenericWithPlanUsing
    (recursiveHelperLabel: AST.Type -> string)
    (preserveRegisters: bool)
    (ctx: FuncCtx)
    (addrReg: X86_64.Reg)
    (payloadSize: int)
    (releasePlan: MemoryModel.RcReleasePlan option)
    : X86_64.Instr list =
    let skipLabel = freshLabel "rc_dec_skip"
    let noFreeLabel = freshLabel "rc_dec_nofree"
    let leakDec = genLeakCounterDec ctx
    let fieldReleases = genFixedBlockFieldReleases recursiveHelperLabel preserveRegisters ctx releasePlan
    let saveRegs =
        [ X86_64.RAX
          X86_64.RDI
          X86_64.RSI
          X86_64.RDX
          X86_64.RCX
          X86_64.R8
          X86_64.R9
          X86_64.R10
          scratch ]
    let saves = if preserveRegisters then saveRegs |> List.map X86_64.PUSH else []
    let restores = if preserveRegisters then saveRegs |> List.rev |> List.map X86_64.POP else []
    [X86_64.TEST_reg (addrReg, addrReg)
     X86_64.Jcc (X86_64.EQ, skipLabel)]
    @ saves
    @ [X86_64.MOV_reg (X86_64.RDX, addrReg)
       X86_64.MOV_load (X86_64.RCX, X86_64.RDX, payloadSize)
       X86_64.SUB_imm (X86_64.RCX, 1)
       X86_64.MOV_store (X86_64.RDX, payloadSize, X86_64.RCX)
       X86_64.TEST_reg (X86_64.RCX, X86_64.RCX)
       X86_64.Jcc (X86_64.NE, noFreeLabel)]
    @ fieldReleases
    @ (if payloadSize >= 0 && payloadSize < freeListSize then
        [X86_64.MOV_load (X86_64.RCX, freeListBase, payloadSize)
         X86_64.MOV_store (X86_64.RDX, 0, X86_64.RCX)
         X86_64.MOV_store (freeListBase, payloadSize, X86_64.RDX)]
       else [])
    @ leakDec
    @ [X86_64.Label noFreeLabel]
    @ restores
    @ [X86_64.Label skipLabel]

let internal genRefCountDecGenericWithPlan
    (ctx: FuncCtx)
    (addrReg: X86_64.Reg)
    (payloadSize: int)
    (releasePlan: MemoryModel.RcReleasePlan option)
    : X86_64.Instr list =
    genRefCountDecGenericWithPlanUsing recursiveSumRefCountDecHelperLabel true ctx addrReg payloadSize releasePlan

let internal genRefCountDecGeneric (ctx: FuncCtx) (addrReg: X86_64.Reg) (payloadSize: int) (metadata: MemoryModel.RcMetadata option) : X86_64.Instr list =
    genRefCountDecGenericWithPlan ctx addrReg payloadSize (rcMetadataReleasePlan metadata)

/// Stream roots have the generic fixed-block layout, but their close callback
/// must run before the two owned callback closures are released. The lifecycle
/// word makes this finalizer share close's idempotence boundary.
let private genRefCountDecStream
    (ctx: FuncCtx)
    (addrReg: X86_64.Reg)
    (metadata: MemoryModel.RcMetadata option)
    : X86_64.Instr list =
    let skipLabel = freshLabel "stream_rc_dec_skip"
    let noFreeLabel = freshLabel "stream_rc_dec_nofree"
    let alreadyClosedLabel = freshLabel "stream_rc_dec_closed"
    let fieldReleases =
        genFixedBlockFieldReleases
            recursiveSumRefCountDecHelperLabel
            true
            ctx
            (rcMetadataReleasePlan metadata)
    let savedRegs = [X86_64.RAX; X86_64.RDI; X86_64.RSI; X86_64.RDX; X86_64.RCX; X86_64.R8; X86_64.R9; X86_64.R10; scratch]
    let saves = savedRegs |> List.map X86_64.PUSH
    let restores = savedRegs |> List.rev |> List.map X86_64.POP
    [X86_64.TEST_reg (addrReg, addrReg)
     X86_64.Jcc (X86_64.EQ, skipLabel)]
    @ saves
    @ [X86_64.MOV_reg (X86_64.RDX, addrReg)
       X86_64.MOV_load (X86_64.RCX, X86_64.RDX, 24)
       X86_64.SUB_imm (X86_64.RCX, 1)
       X86_64.MOV_store (X86_64.RDX, 24, X86_64.RCX)
       X86_64.TEST_reg (X86_64.RCX, X86_64.RCX)
       X86_64.Jcc (X86_64.NE, noFreeLabel)
       X86_64.MOV_load (X86_64.RCX, X86_64.RDX, 0)
       X86_64.CMP_imm (X86_64.RCX, 5)
       X86_64.Jcc (X86_64.EQ, alreadyClosedLabel)
       X86_64.MOV_imm32 (X86_64.RCX, 5)
       X86_64.MOV_store (X86_64.RDX, 0, X86_64.RCX)
       X86_64.PUSH X86_64.RDX
       X86_64.MOV_load (X86_64.RAX, X86_64.RDX, 16)
       X86_64.MOV_load (X86_64.R10, X86_64.RAX, 0)
       X86_64.MOV_imm32 (X86_64.RDI, 0)
       X86_64.CALL_reg X86_64.R10
       X86_64.POP X86_64.RDX
       X86_64.Label alreadyClosedLabel]
    @ fieldReleases
    @ [X86_64.MOV_load (X86_64.RCX, freeListBase, 24)
       X86_64.MOV_store (X86_64.RDX, 0, X86_64.RCX)
       X86_64.MOV_store (freeListBase, 24, X86_64.RDX)]
    @ genLeakCounterDec ctx
    @ [X86_64.Label noFreeLabel]
    @ restores
    @ [X86_64.Label skipLabel]

let internal generateStreamRefCountDecHelper (ctx: FuncCtx) : X86_64.Instr list =
    let sourceType = AST.TStream (AST.TVar "a")
    let releasePlan =
        MemoryPlanning.rcReleasePlanOfTypeWithSums
            ctx.RecordRegistry
            ctx.SumShapeRegistry
            sourceType
    let metadata : MemoryModel.RcMetadata = {
        ReleasePlanCacheKey = ReleasePlanFingerprint.rcReleasePlanCacheKey sourceType releasePlan
        ReleasePlan = Some releasePlan
        SourceType = Some sourceType
    }
    [X86_64.Label streamRefCountDecHelperLabel]
    @ genRefCountDecStream ctx X86_64.RAX (Some metadata)
    @ [X86_64.RET]

let internal generateRecursiveSumRefCountDecHelper
    (enableLeakCheck: bool)
    (recordRegistry: LIR.RecordRegistry)
    (sumShapeRegistry: MemoryModel.RcSumShapeRegistry)
    (sourceType: AST.Type)
    : X86_64.Instr list =
    let releasePlan =
        MemoryPlanning.rcReleasePlanOfTypeWithSums recordRegistry sumShapeRegistry sourceType
    let helperCtx : FuncCtx = {
        FunctionName = "__dark_recursive_sum_rc_dec"
        StackSize = 0
        UsedCalleeSaved = []
        EnableLeakCheck = enableLeakCheck
        RecordRegistry = recordRegistry
        SumShapeRegistry = sumShapeRegistry
    }
    let helperLabel = recursiveSumRefCountDecHelperLabel sourceType
    let workerLabel (typ: AST.Type) = $"{recursiveSumRefCountDecHelperLabel typ}_worker"
    let savedRegs =
        [ X86_64.RAX
          X86_64.RDI
          X86_64.RSI
          X86_64.RDX
          X86_64.RCX
          X86_64.R8
          X86_64.R9
          X86_64.R10
          scratch ]
    let saves = savedRegs |> List.map X86_64.PUSH
    let restores = savedRegs |> List.rev |> List.map X86_64.POP
    match releasePlan with
    | MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, _) ->
        [X86_64.Label helperLabel]
        @ saves
        @ [X86_64.CALL (workerLabel sourceType)]
        @ restores
        @ [X86_64.RET
           X86_64.Label (workerLabel sourceType)]
        @ genRefCountDecGenericWithPlanUsing workerLabel false helperCtx X86_64.RAX payloadSize (Some releasePlan)
        @ [X86_64.RET]
    | _ ->
        Crash.crash $"x64 recursive sum RC helper requires a generic root release plan, got {releasePlan}"

let internal recursiveReleaseTypesInFunctions (functions: LIR.Function list) : Set<AST.Type> =
    functions
    |> List.collect (fun func ->
        func.CFG.Blocks
        |> Map.toList
        |> List.collect (fun (_, block) -> block.Instrs))
    |> List.fold (fun recursiveTypes instr ->
        match instr with
        | LIR.RefCountDec (_, _, _, Some metadata) ->
            metadata.ReleasePlan
            |> Option.map MemoryPlanning.recursiveReleaseTypes
            |> Option.defaultValue Set.empty
            |> Set.union recursiveTypes
        | _ ->
            recursiveTypes) Set.empty

/// Generic RefCountInc: increment refcount at [addr + payloadSize].
let internal genRefCountIncGeneric (addrReg: X86_64.Reg) (payloadSize: int) : X86_64.Instr list =
    let skipLabel = freshLabel "rc_inc_skip"
    [X86_64.TEST_reg (addrReg, addrReg)
     X86_64.Jcc (X86_64.EQ, skipLabel)
     X86_64.PUSH X86_64.RDX
     X86_64.PUSH X86_64.R10
     X86_64.MOV_reg (X86_64.R10, addrReg)
     X86_64.MOV_load (X86_64.RDX, X86_64.R10, payloadSize)
     X86_64.ADD_imm (X86_64.RDX, 1)
     X86_64.MOV_store (X86_64.R10, payloadSize, X86_64.RDX)
     X86_64.POP X86_64.R10
     X86_64.POP X86_64.RDX
     X86_64.Label skipLabel]
