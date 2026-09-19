// DictReferenceCounts.fs - Generate HAMT root and recursive payload lifetime helpers.

module X64DictReferenceCounts

open X64Operands
open X64CodeGenTypes
open X64ReleaseSelection
open X64FieldReferenceCounts
open X64ListReferenceCounts

let internal generateDictRefCountIncHelper () : X86_64.Instr list =
    let label name = $"__dark_dict_rc_inc_{name}"
    let helperRet = label "ret"
    let internalTag = label "internal"
    let leafTag = label "leaf"
    let collisionTag = label "collision"
    let popcountLoop = label "popcount_loop"
    let popcountDone = label "popcount_done"
    let haveOffset = label "have_offset"

    [X86_64.Label dictRefCountIncHelperLabel
     // RAX = tagged HAMT root. Tags: 1 internal, 2 leaf, 3 collision.
     X86_64.TEST_reg (X86_64.RAX, X86_64.RAX)
     X86_64.Jcc (X86_64.EQ, helperRet)
     X86_64.MOV_reg (X86_64.RCX, X86_64.RAX)
     X86_64.AND_imm (X86_64.RCX, 3)
     X86_64.TEST_reg (X86_64.RCX, X86_64.RCX)
     X86_64.Jcc (X86_64.EQ, helperRet)
     X86_64.CMP_imm (X86_64.RCX, 3)
     X86_64.Jcc (X86_64.GT, helperRet)
     X86_64.MOV_reg (X86_64.RDI, X86_64.RAX)
     X86_64.AND_imm (X86_64.RDI, -8)
     X86_64.CMP_reg (X86_64.RDI, freeListBase)
     X86_64.Jcc (X86_64.B, helperRet)
     X86_64.CMP_reg (X86_64.RDI, heapPtr)
     X86_64.Jcc (X86_64.AE, helperRet)

     X86_64.CMP_imm (X86_64.RCX, 1)
     X86_64.Jcc (X86_64.EQ, internalTag)
     X86_64.CMP_imm (X86_64.RCX, 2)
     X86_64.Jcc (X86_64.EQ, leafTag)
     X86_64.JMP collisionTag

     X86_64.Label leafTag
     X86_64.MOV_imm32 (X86_64.RSI, 16)
     X86_64.JMP haveOffset

     X86_64.Label collisionTag
     X86_64.MOV_load (X86_64.RSI, X86_64.RDI, 0)
     X86_64.SHL_imm (X86_64.RSI, 4)
     X86_64.ADD_imm (X86_64.RSI, 8)
     X86_64.JMP haveOffset

     X86_64.Label internalTag
     X86_64.MOV_load (X86_64.R8, X86_64.RDI, 0)
     X86_64.XOR_reg (X86_64.RSI, X86_64.RSI)
     X86_64.Label popcountLoop
     X86_64.TEST_reg (X86_64.R8, X86_64.R8)
     X86_64.Jcc (X86_64.EQ, popcountDone)
     X86_64.MOV_reg (X86_64.R9, X86_64.R8)
     X86_64.SUB_imm (X86_64.R9, 1)
     X86_64.AND_reg (X86_64.R8, X86_64.R9)
     X86_64.ADD_imm (X86_64.RSI, 1)
     X86_64.JMP popcountLoop
     X86_64.Label popcountDone
     X86_64.SHL_imm (X86_64.RSI, 3)
     X86_64.ADD_imm (X86_64.RSI, 8)

     X86_64.Label haveOffset
     X86_64.MOV_reg (X86_64.R8, X86_64.RDI)
     X86_64.ADD_reg (X86_64.R8, X86_64.RSI)
     X86_64.MOV_load (X86_64.R9, X86_64.R8, 0)
     X86_64.ADD_imm (X86_64.R9, 1)
     X86_64.MOV_store (X86_64.R8, 0, X86_64.R9)

     X86_64.Label helperRet
     X86_64.RET]

let internal generateDictRefCountDecHelper
    (helperLabel: string)
    (keyReleasePlan: MemoryModel.RcReleasePlan)
    (releaseLeafDynamicValue: bool)
    (releaseLeafListValue: bool)
    (releaseLeafDictValueHelper: string option)
    (releaseLeafClosureValue: bool)
    (releaseLeafStreamValue: bool)
    (leafFixedBlockValueRelease: (int * MemoryModel.RcReleasePlan) option)
    (enableLeakCheck: bool)
    (recordRegistry: LIR.RecordRegistry)
    (sumShapeRegistry: MemoryModel.RcSumShapeRegistry)
    : X86_64.Instr list =
    let label name = $"{helperLabel}_{name}"
    let helperRet = label "ret"
    let loopCheck = label "loop_check"
    let popOrRet = label "pop_or_ret"
    let internalTag = label "internal"
    let leafTag = label "leaf"
    let collisionTag = label "collision"
    let popcountLoop = label "popcount_loop"
    let popcountDone = label "popcount_done"
    let haveOffset = label "have_offset"
    let collectInternal = label "collect_internal"
    let collectLoop = label "collect_loop"
    let freeNode = label "free_node"
    let skipFreeList = label "skip_freelist"
    let skipLeafPayloadRelease = label "skip_leaf_payload_release"
    let skipCollisionPayloadRelease = label "skip_collision_payload_release"
    let collisionPayloadLoop = label "collision_payload_loop"
    let collisionPayloadDone = label "collision_payload_done"

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
        FunctionName = "__dark_dict_refcount_dec_helper"
        StackSize = 0
        UsedCalleeSaved = []
        EnableLeakCheck = enableLeakCheck
        RecordRegistry = recordRegistry
        SumShapeRegistry = sumShapeRegistry
        FunctionNames = Map.empty
    }

    let addChild (suffix: string) : X86_64.Instr list =
        let doneLabel = label $"child_done_{suffix}"
        let pushLabel = label $"child_push_{suffix}"
        [X86_64.TEST_reg (X86_64.R10, X86_64.R10)
         X86_64.Jcc (X86_64.EQ, doneLabel)
         X86_64.MOV_reg (X86_64.R11, X86_64.R10)
         X86_64.AND_imm (X86_64.R11, 3)
         X86_64.TEST_reg (X86_64.R11, X86_64.R11)
         X86_64.Jcc (X86_64.EQ, doneLabel)
         X86_64.CMP_imm (X86_64.R11, 3)
         X86_64.Jcc (X86_64.GT, doneLabel)
         X86_64.MOV_reg (X86_64.R11, X86_64.R10)
         X86_64.AND_imm (X86_64.R11, -8)
         X86_64.CMP_reg (X86_64.R11, freeListBase)
         X86_64.Jcc (X86_64.B, doneLabel)
         X86_64.CMP_reg (X86_64.R11, heapPtr)
         X86_64.Jcc (X86_64.AE, doneLabel)
         X86_64.TEST_reg (X86_64.RAX, X86_64.RAX)
         X86_64.Jcc (X86_64.NE, pushLabel)
         X86_64.MOV_reg (X86_64.RAX, X86_64.R10)
         X86_64.JMP doneLabel
         X86_64.Label pushLabel
         X86_64.PUSH X86_64.R10
         X86_64.ADD_imm (X86_64.RCX, 1)
         X86_64.Label doneLabel]

    let releaseManagedRootValueInstrs
        (baseReg: X86_64.Reg)
        (fieldOffset: int)
        (targetHelperLabel: string)
        (skipLabel: string)
        =
        let saveRegs = [X86_64.RAX; X86_64.RCX; X86_64.RDX; X86_64.RDI; X86_64.RSI; X86_64.R8; X86_64.R9; X86_64.R10; X86_64.R11; scratch]
        let saves = saveRegs |> List.map X86_64.PUSH
        let restores = saveRegs |> List.rev |> List.map X86_64.POP
        saves
        @ [X86_64.MOV_load (X86_64.RAX, baseReg, fieldOffset)
           X86_64.CALL targetHelperLabel]
        @ restores
        @ [X86_64.Label skipLabel]

    let releaseDynamicBufferFieldInstrs
        (baseReg: X86_64.Reg)
        (fieldOffset: int)
        (skipLabel: string)
        : X86_64.Instr list =
        let saveRegs = [X86_64.RAX; X86_64.RCX; X86_64.RDX; X86_64.RDI; X86_64.RSI; X86_64.R8; X86_64.R9; X86_64.R10; X86_64.R11; scratch]
        let saves = saveRegs |> List.map X86_64.PUSH
        let restores = saveRegs |> List.rev |> List.map X86_64.POP
        saves
        @ [X86_64.MOV_reg (X86_64.RDX, baseReg)]
        @ genDynamicBufferFieldRelease helperCtx fieldOffset
        @ restores
        @ [X86_64.Label skipLabel]

    let releaseDynamicBufferValueInstrs baseReg skipLabel =
        if releaseLeafDynamicValue then
            releaseDynamicBufferFieldInstrs baseReg 8 skipLabel
        else
            []

    let releaseFixedBlockValueInstrs
        (baseReg: X86_64.Reg)
        (fieldOffset: int)
        (payloadSize: int)
        (releasePlan: MemoryModel.RcReleasePlan)
        (skipLabel: string)
        =
        let saveRegs = [X86_64.RAX; X86_64.RCX; X86_64.RDX; X86_64.RDI; X86_64.RSI; X86_64.R8; X86_64.R9; X86_64.R10; X86_64.R11; scratch]
        let saves = saveRegs |> List.map X86_64.PUSH
        let restores = saveRegs |> List.rev |> List.map X86_64.POP
        saves
        @ [X86_64.MOV_load (X86_64.RAX, baseReg, fieldOffset)]
        @ genRefCountDecGenericWithPlan helperCtx X86_64.RAX payloadSize (Some releasePlan)
        @ restores
        @ [X86_64.Label skipLabel]

    let releasePayloadInstrs (baseReg: X86_64.Reg) (suffix: string) =
        let skipLeafListValueRelease = label $"skip_leaf_list_value_release_{suffix}"
        let skipLeafDictValueRelease = label $"skip_leaf_dict_value_release_{suffix}"
        let skipLeafClosureValueRelease = label $"skip_leaf_closure_value_release_{suffix}"
        let skipLeafStreamValueRelease = label $"skip_leaf_stream_value_release_{suffix}"
        let skipLeafFixedBlockValueRelease = label $"skip_leaf_fixed_block_value_release_{suffix}"
        let skipLeafDynamicKeyRelease = label $"skip_leaf_dynamic_key_release_{suffix}"
        let skipLeafDynamicValueRelease = label $"skip_leaf_dynamic_value_release_{suffix}"
        let skipKeyRelease = label $"skip_key_release_{suffix}"
        let keyInstrs =
            match keyReleasePlan with
            | MemoryModel.NoReleasePlan -> []
            | MemoryModel.DynamicBufferRelease _ ->
                releaseDynamicBufferFieldInstrs baseReg 0 skipKeyRelease
            | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
                releaseManagedRootValueInstrs
                    baseReg
                    0
                    (listDecHelperForReleasePlan keyReleasePlan)
                    skipKeyRelease
            | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
                releaseManagedRootValueInstrs
                    baseReg
                    0
                    (dictDecHelperForReleasePlan keyReleasePlan)
                    skipKeyRelease
            | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
                releaseManagedRootValueInstrs baseReg 0 closureRefCountDecHelperLabel skipKeyRelease
            | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) ->
                releaseManagedRootValueInstrs baseReg 0 streamRefCountDecHelperLabel skipKeyRelease
            | MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, _) ->
                releaseFixedBlockValueInstrs baseReg 0 payloadSize keyReleasePlan skipKeyRelease
            | _ -> []
        let listValueInstrs =
            if releaseLeafListValue then
                releaseManagedRootValueInstrs baseReg 8 listRefCountDecHelperLabel skipLeafListValueRelease
            else
                []
        let dictValueInstrs =
            match releaseLeafDictValueHelper with
            | Some targetHelperLabel ->
                releaseManagedRootValueInstrs baseReg 8 targetHelperLabel skipLeafDictValueRelease
            | None ->
                []
        let closureValueInstrs =
            if releaseLeafClosureValue then
                releaseManagedRootValueInstrs baseReg 8 closureRefCountDecHelperLabel skipLeafClosureValueRelease
            else
                []
        let streamValueInstrs =
            if releaseLeafStreamValue then
                releaseManagedRootValueInstrs baseReg 8 streamRefCountDecHelperLabel skipLeafStreamValueRelease
            else
                []
        let fixedBlockValueInstrs =
            match leafFixedBlockValueRelease with
            | Some (payloadSize, releasePlan) ->
                releaseFixedBlockValueInstrs baseReg 8 payloadSize releasePlan skipLeafFixedBlockValueRelease
            | None ->
                []

        keyInstrs @ releaseDynamicBufferValueInstrs baseReg skipLeafDynamicValueRelease @ listValueInstrs @ dictValueInstrs @ closureValueInstrs @ streamValueInstrs @ fixedBlockValueInstrs

    let hasPayloadRelease =
        keyReleasePlan <> MemoryModel.NoReleasePlan
        || releaseLeafDynamicValue
        || releaseLeafListValue
        || Option.isSome releaseLeafDictValueHelper
        || releaseLeafClosureValue
        || releaseLeafStreamValue
        || Option.isSome leafFixedBlockValueRelease

    let releaseLeafValueInstrs =
        if hasPayloadRelease then
            [X86_64.CMP_imm (X86_64.RDX, 2)
             X86_64.Jcc (X86_64.NE, skipLeafPayloadRelease)]
            @ releasePayloadInstrs X86_64.RDI "leaf"
            @ [X86_64.Label skipLeafPayloadRelease]
        else
            []

    let releaseCollisionValueInstrs =
        if hasPayloadRelease then
            [X86_64.CMP_imm (X86_64.RDX, 3)
             X86_64.Jcc (X86_64.NE, skipCollisionPayloadRelease)
             X86_64.MOV_load (X86_64.R8, X86_64.RDI, 0)
             X86_64.XOR_reg (X86_64.R9, X86_64.R9)
             X86_64.Label collisionPayloadLoop
             X86_64.CMP_reg (X86_64.R9, X86_64.R8)
             X86_64.Jcc (X86_64.GE, collisionPayloadDone)
             X86_64.MOV_reg (X86_64.R10, X86_64.R9)
             X86_64.SHL_imm (X86_64.R10, 4)
             X86_64.ADD_imm (X86_64.R10, 8)
             X86_64.ADD_reg (X86_64.R10, X86_64.RDI)]
            @ releasePayloadInstrs X86_64.R10 "collision"
            @ [X86_64.ADD_imm (X86_64.R9, 1)
               X86_64.JMP collisionPayloadLoop
               X86_64.Label collisionPayloadDone
               X86_64.Label skipCollisionPayloadRelease]
        else
            []

    [X86_64.Label helperLabel
     X86_64.XOR_reg (X86_64.RCX, X86_64.RCX)
     X86_64.JMP loopCheck

     X86_64.Label loopCheck
     X86_64.TEST_reg (X86_64.RAX, X86_64.RAX)
     X86_64.Jcc (X86_64.EQ, popOrRet)
     X86_64.MOV_reg (X86_64.RDX, X86_64.RAX)
     X86_64.AND_imm (X86_64.RDX, 3)
     X86_64.TEST_reg (X86_64.RDX, X86_64.RDX)
     X86_64.Jcc (X86_64.EQ, popOrRet)
     X86_64.CMP_imm (X86_64.RDX, 3)
     X86_64.Jcc (X86_64.GT, popOrRet)
     X86_64.MOV_reg (X86_64.RDI, X86_64.RAX)
     X86_64.AND_imm (X86_64.RDI, -8)
     X86_64.CMP_reg (X86_64.RDI, freeListBase)
     X86_64.Jcc (X86_64.B, popOrRet)
     X86_64.CMP_reg (X86_64.RDI, heapPtr)
     X86_64.Jcc (X86_64.AE, popOrRet)

     X86_64.CMP_imm (X86_64.RDX, 1)
     X86_64.Jcc (X86_64.EQ, internalTag)
     X86_64.CMP_imm (X86_64.RDX, 2)
     X86_64.Jcc (X86_64.EQ, leafTag)
     X86_64.JMP collisionTag

     X86_64.Label leafTag
     X86_64.MOV_imm32 (X86_64.RSI, 16)
     X86_64.XOR_reg (X86_64.R8, X86_64.R8)
     X86_64.JMP haveOffset

     X86_64.Label collisionTag
     X86_64.MOV_load (X86_64.RSI, X86_64.RDI, 0)
     X86_64.SHL_imm (X86_64.RSI, 4)
     X86_64.ADD_imm (X86_64.RSI, 8)
     X86_64.XOR_reg (X86_64.R8, X86_64.R8)
     X86_64.JMP haveOffset

     X86_64.Label internalTag
     X86_64.MOV_load (X86_64.R9, X86_64.RDI, 0)
     X86_64.XOR_reg (X86_64.R8, X86_64.R8)
     X86_64.Label popcountLoop
     X86_64.TEST_reg (X86_64.R9, X86_64.R9)
     X86_64.Jcc (X86_64.EQ, popcountDone)
     X86_64.MOV_reg (X86_64.R10, X86_64.R9)
     X86_64.SUB_imm (X86_64.R10, 1)
     X86_64.AND_reg (X86_64.R9, X86_64.R10)
     X86_64.ADD_imm (X86_64.R8, 1)
     X86_64.JMP popcountLoop
     X86_64.Label popcountDone
     X86_64.MOV_reg (X86_64.RSI, X86_64.R8)
     X86_64.SHL_imm (X86_64.RSI, 3)
     X86_64.ADD_imm (X86_64.RSI, 8)

     X86_64.Label haveOffset
     X86_64.MOV_reg (X86_64.R9, X86_64.RDI)
     X86_64.ADD_reg (X86_64.R9, X86_64.RSI)
     X86_64.MOV_load (X86_64.R10, X86_64.R9, 0)
     X86_64.SUB_imm (X86_64.R10, 1)
     X86_64.MOV_store (X86_64.R9, 0, X86_64.R10)
     X86_64.TEST_reg (X86_64.R10, X86_64.R10)
     X86_64.Jcc (X86_64.NE, popOrRet)
     X86_64.CMP_imm (X86_64.RDX, 1)
     X86_64.Jcc (X86_64.EQ, collectInternal)
     X86_64.JMP freeNode

     X86_64.Label collectInternal
     X86_64.XOR_reg (X86_64.RAX, X86_64.RAX)
     X86_64.XOR_reg (X86_64.R9, X86_64.R9)
     X86_64.Label collectLoop
     X86_64.CMP_reg (X86_64.R9, X86_64.R8)
     X86_64.Jcc (X86_64.GE, freeNode)
     X86_64.MOV_reg (X86_64.R10, X86_64.R9)
     X86_64.SHL_imm (X86_64.R10, 3)
     X86_64.ADD_imm (X86_64.R10, 8)
     X86_64.ADD_reg (X86_64.R10, X86_64.RDI)
     X86_64.MOV_load (X86_64.R10, X86_64.R10, 0)]
    @ addChild "internal"
    @ [X86_64.ADD_imm (X86_64.R9, 1)
       X86_64.JMP collectLoop
       X86_64.Label freeNode]
    @ releaseLeafValueInstrs
    @ releaseCollisionValueInstrs
    @ [X86_64.CMP_imm (X86_64.RSI, freeListSize)
       X86_64.Jcc (X86_64.GE, skipFreeList)
       X86_64.MOV_reg (X86_64.R9, freeListBase)
       X86_64.ADD_reg (X86_64.R9, X86_64.RSI)
       X86_64.MOV_load (X86_64.R10, X86_64.R9, 0)
       X86_64.MOV_store (X86_64.RDI, 0, X86_64.R10)
       X86_64.MOV_store (X86_64.R9, 0, X86_64.RDI)
       X86_64.Label skipFreeList]
    @ leakDec
    @ [X86_64.JMP loopCheck

       X86_64.Label popOrRet
       X86_64.TEST_reg (X86_64.RCX, X86_64.RCX)
       X86_64.Jcc (X86_64.EQ, helperRet)
       X86_64.POP X86_64.RAX
       X86_64.SUB_imm (X86_64.RCX, 1)
       X86_64.JMP loopCheck

       X86_64.Label helperRet
       X86_64.RET]

let internal generatePlannedDictRefCountDecHelper
    (helperLabel: string)
    (releasePlan: MemoryModel.RcReleasePlan)
    (enableLeakCheck: bool)
    (recordRegistry: LIR.RecordRegistry)
    (sumShapeRegistry: MemoryModel.RcSumShapeRegistry)
    : X86_64.Instr list =
    let unsupported context release =
        Crash.crash $"x64 planned dict RefCountDec does not support {context} release plan {release}"

    match releasePlan with
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (keyRelease, valueRelease)) ->
        let releaseLeafDynamicValue, releaseLeafListValue, releaseLeafDictValueHelper, releaseLeafClosureValue, releaseLeafStreamValue, leafFixedBlockValueRelease =
            match valueRelease with
            | MemoryModel.NoReleasePlan ->
                false, false, None, false, false, None
            | MemoryModel.DynamicBufferRelease _ ->
                true, false, None, false, false, None
            | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
                false, true, None, false, false, None
            | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
                false, false, Some (dictDecHelperForReleasePlan valueRelease), false, false, None
            | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
                false, false, None, true, false, None
            | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) ->
                false, false, None, false, true, None
            | MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, _) ->
                false, false, None, false, false, Some (payloadSize, valueRelease)
            | other ->
                unsupported "value" other

        generateDictRefCountDecHelper
            helperLabel
            keyRelease
            releaseLeafDynamicValue
            releaseLeafListValue
            releaseLeafDictValueHelper
            releaseLeafClosureValue
            releaseLeafStreamValue
            leafFixedBlockValueRelease
            enableLeakCheck
            recordRegistry
            sumShapeRegistry
    | other ->
        Crash.crash $"x64 planned dict RefCountDec helper requires a DictHeap release plan, got {other}"
