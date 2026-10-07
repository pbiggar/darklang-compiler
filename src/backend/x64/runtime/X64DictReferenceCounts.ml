(* X64DictReferenceCounts.ml - Generate HAMT root and recursive payload lifetime helpers. *)
[@@@warning "-4"]

open X64Operands
open X64CodeGenTypes
open X64ReleaseSelection
open FieldReferenceCounts
module X = X86_64

let generateDictRefCountIncHelper () =
  let label name = "__dark_dict_rc_inc_" ^ name in
  let helperRet = label "ret" in
  let internalTag = label "internal" in
  let leafTag = label "leaf" in
  let collisionTag = label "collision" in
  let popcountLoop = label "popcount_loop" in
  let popcountDone = label "popcount_done" in
  let haveOffset = label "have_offset" in

  [
    X86_64.Label dictRefCountIncHelperLabel;
    (* RAX = tagged HAMT root. Tags: 1 internal, 2 leaf, 3 collision. *)
    X86_64.TEST_reg (X86_64.RAX, X86_64.RAX);
    X86_64.Jcc (X86_64.EQ, helperRet);
    X86_64.MOV_reg (X86_64.RCX, X86_64.RAX);
    X86_64.AND_imm (X86_64.RCX, 3l);
    X86_64.TEST_reg (X86_64.RCX, X86_64.RCX);
    X86_64.Jcc (X86_64.EQ, helperRet);
    X86_64.CMP_imm (X86_64.RCX, 3l);
    X86_64.Jcc (X86_64.GT, helperRet);
    X86_64.MOV_reg (X86_64.RDI, X86_64.RAX);
    X86_64.AND_imm (X86_64.RDI, -8l);
    X86_64.CMP_reg (X86_64.RDI, freeListBase);
    X86_64.Jcc (X86_64.B, helperRet);
    X86_64.CMP_reg (X86_64.RDI, heapPtr);
    X86_64.Jcc (X86_64.AE, helperRet);
    X86_64.CMP_imm (X86_64.RCX, 1l);
    X86_64.Jcc (X86_64.EQ, internalTag);
    X86_64.CMP_imm (X86_64.RCX, 2l);
    X86_64.Jcc (X86_64.EQ, leafTag);
    X86_64.JMP collisionTag;
    X86_64.Label leafTag;
    X86_64.MOV_imm32 (X86_64.RSI, 16l);
    X86_64.JMP haveOffset;
    X86_64.Label collisionTag;
    X86_64.MOV_load (X86_64.RSI, X86_64.RDI, 0l);
    X86_64.SHL_imm (X86_64.RSI, 4);
    X86_64.ADD_imm (X86_64.RSI, 8l);
    X86_64.JMP haveOffset;
    X86_64.Label internalTag;
    X86_64.MOV_load (X86_64.R8, X86_64.RDI, 0l);
    X86_64.XOR_reg (X86_64.RSI, X86_64.RSI);
    X86_64.Label popcountLoop;
    X86_64.TEST_reg (X86_64.R8, X86_64.R8);
    X86_64.Jcc (X86_64.EQ, popcountDone);
    X86_64.MOV_reg (X86_64.R9, X86_64.R8);
    X86_64.SUB_imm (X86_64.R9, 1l);
    X86_64.AND_reg (X86_64.R8, X86_64.R9);
    X86_64.ADD_imm (X86_64.RSI, 1l);
    X86_64.JMP popcountLoop;
    X86_64.Label popcountDone;
    X86_64.SHL_imm (X86_64.RSI, 3);
    X86_64.ADD_imm (X86_64.RSI, 8l);
    X86_64.Label haveOffset;
    X86_64.MOV_reg (X86_64.R8, X86_64.RDI);
    X86_64.ADD_reg (X86_64.R8, X86_64.RSI);
    X86_64.MOV_load (X86_64.R9, X86_64.R8, 0l);
    X86_64.ADD_imm (X86_64.R9, 1l);
    X86_64.MOV_store (X86_64.R8, 0l, X86_64.R9);
    X86_64.Label helperRet;
    X86_64.RET;
  ]

let generateDictRefCountDecHelper helperLabel keyReleasePlan
    releaseLeafDynamicValue releaseLeafListValue releaseLeafDictValueHelper
    releaseLeafClosureValue releaseLeafStreamValue leafFixedBlockValueRelease
    enableLeakCheck recordRegistry sumShapeRegistry =
  let label name = helperLabel ^ "_" ^ name in
  let helperRet = label "ret" in
  let loopCheck = label "loop_check" in
  let popOrRet = label "pop_or_ret" in
  let internalTag = label "internal" in
  let leafTag = label "leaf" in
  let collisionTag = label "collision" in
  let popcountLoop = label "popcount_loop" in
  let popcountDone = label "popcount_done" in
  let haveOffset = label "have_offset" in
  let collectInternal = label "collect_internal" in
  let collectLoop = label "collect_loop" in
  let freeNode = label "free_node" in
  let nextNode = label "next_node" in
  let skipFreeList = label "skip_freelist" in
  let skipLeafPayloadRelease = label "skip_leaf_payload_release" in
  let skipCollisionPayloadRelease = label "skip_collision_payload_release" in
  let collisionPayloadLoop = label "collision_payload_loop" in
  let collisionPayloadDone = label "collision_payload_done" in

  let helperCtx =
    {
      functionName = "__dark_dict_refcount_dec_helper";
      stackSize = 0;
      usedCalleeSaved = [];
      enableLeakCheck;
      recordRegistry;
      sumShapeRegistry;
      functionNames = FunctionIdMap.empty;
    }
  in
  let leakDec = genLeakCounterDec helperCtx in
  let addChild suffix =
    let doneLabel = label ("child_done_" ^ suffix) in
    let pushLabel = label ("child_push_" ^ suffix) in
    [
      X86_64.TEST_reg (X86_64.R10, X86_64.R10);
      X86_64.Jcc (X86_64.EQ, doneLabel);
      X86_64.MOV_reg (X86_64.R11, X86_64.R10);
      X86_64.AND_imm (X86_64.R11, 3l);
      X86_64.TEST_reg (X86_64.R11, X86_64.R11);
      X86_64.Jcc (X86_64.EQ, doneLabel);
      X86_64.CMP_imm (X86_64.R11, 3l);
      X86_64.Jcc (X86_64.GT, doneLabel);
      X86_64.MOV_reg (X86_64.R11, X86_64.R10);
      X86_64.AND_imm (X86_64.R11, -8l);
      X86_64.CMP_reg (X86_64.R11, freeListBase);
      X86_64.Jcc (X86_64.B, doneLabel);
      X86_64.CMP_reg (X86_64.R11, heapPtr);
      X86_64.Jcc (X86_64.AE, doneLabel);
      X86_64.TEST_reg (X86_64.RAX, X86_64.RAX);
      X86_64.Jcc (X86_64.NE, pushLabel);
      X86_64.MOV_reg (X86_64.RAX, X86_64.R10);
      X86_64.JMP doneLabel;
      X86_64.Label pushLabel;
      X86_64.PUSH X86_64.R10;
      X86_64.ADD_imm (X86_64.RCX, 1l);
      X86_64.Label doneLabel;
    ]
  in
  let saveRegs =
    [ X.RAX; X.RCX; X.RDX; X.RDI; X.RSI; X.R8; X.R9; X.R10; X.R11; scratch ]
  in
  let saves = List.map (fun reg -> X.PUSH reg) saveRegs in
  let restores = List.map (fun reg -> X.POP reg) (List.rev saveRegs) in
  let releaseManagedRootValueInstrs baseReg fieldOffset targetHelperLabel
      skipLabel =
    saves
    @ [
        X.MOV_load (X.RAX, baseReg, Int32.of_int fieldOffset);
        X.CALL targetHelperLabel;
      ]
    @ restores @ [ X.Label skipLabel ]
  in
  let releaseDynamicBufferFieldInstrs baseReg fieldOffset skipTagged skipLabel =
    let release =
      genDynamicBufferFieldRelease helperCtx skipTagged fieldOffset
    in
    saves
    @ [ X.MOV_reg (X.RDX, baseReg) ]
    @ release @ restores @ [ X.Label skipLabel ]
  in
  let releaseDynamicBufferValueInstrs baseReg skipLabel =
    match releaseLeafDynamicValue with
    | Some operation ->
        releaseDynamicBufferFieldInstrs baseReg 8
          (operation = MemoryModel.DynamicIntBuffer)
          skipLabel
    | None -> []
  in
  let releaseFixedBlockValueInstrs baseReg fieldOffset payloadSize releasePlan
      skipLabel =
    let release =
      genRefCountDecGenericWithPlan helperCtx X.RAX payloadSize
        (Some releasePlan)
    in
    saves
    @ [ X.MOV_load (X.RAX, baseReg, Int32.of_int fieldOffset) ]
    @ release @ restores @ [ X.Label skipLabel ]
  in
  let releasePayloadInstrs baseReg suffix =
    let skipLeafListValueRelease =
      label ("skip_leaf_list_value_release_" ^ suffix)
    in
    let skipLeafDictValueRelease =
      label ("skip_leaf_dict_value_release_" ^ suffix)
    in
    let skipLeafClosureValueRelease =
      label ("skip_leaf_closure_value_release_" ^ suffix)
    in
    let skipLeafStreamValueRelease =
      label ("skip_leaf_stream_value_release_" ^ suffix)
    in
    let skipLeafFixedBlockValueRelease =
      label ("skip_leaf_fixed_block_value_release_" ^ suffix)
    in
    let _skipLeafDynamicKeyRelease =
      label ("skip_leaf_dynamic_key_release_" ^ suffix)
    in
    let skipLeafDynamicValueRelease =
      label ("skip_leaf_dynamic_value_release_" ^ suffix)
    in
    let skipKeyRelease = label ("skip_key_release_" ^ suffix) in
    let keyInstrs =
      match keyReleasePlan with
      | MemoryModel.NoReleasePlan -> []
      | MemoryModel.DynamicBufferRelease operation ->
          releaseDynamicBufferFieldInstrs baseReg 0
            (operation = MemoryModel.DynamicIntBuffer)
            skipKeyRelease
      | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
          releaseManagedRootValueInstrs baseReg 0
            (listDecHelperForReleasePlan keyReleasePlan)
            skipKeyRelease
      | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
          releaseManagedRootValueInstrs baseReg 0
            (dictDecHelperForReleasePlan keyReleasePlan)
            skipKeyRelease
      | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
          releaseManagedRootValueInstrs baseReg 0 closureRefCountDecHelperLabel
            skipKeyRelease
      | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) ->
          releaseManagedRootValueInstrs baseReg 0 streamRefCountDecHelperLabel
            skipKeyRelease
      | MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, _) ->
          releaseFixedBlockValueInstrs baseReg 0 payloadSize keyReleasePlan
            skipKeyRelease
      | MemoryModel.RecursiveRelease sourceType ->
          releaseManagedRootValueInstrs baseReg 0
            (recursiveNominalRefCountDecHelperLabel sourceType)
            skipKeyRelease
    in
    let listValueInstrs =
      if releaseLeafListValue then
        releaseManagedRootValueInstrs baseReg 8 listRefCountDecHelperLabel
          skipLeafListValueRelease
      else []
    in
    let dictValueInstrs =
      match releaseLeafDictValueHelper with
      | Some target ->
          releaseManagedRootValueInstrs baseReg 8 target
            skipLeafDictValueRelease
      | None -> []
    in
    let closureValueInstrs =
      if releaseLeafClosureValue then
        releaseManagedRootValueInstrs baseReg 8 closureRefCountDecHelperLabel
          skipLeafClosureValueRelease
      else []
    in
    let streamValueInstrs =
      if releaseLeafStreamValue then
        releaseManagedRootValueInstrs baseReg 8 streamRefCountDecHelperLabel
          skipLeafStreamValueRelease
      else []
    in
    let fixedBlockValueInstrs =
      match leafFixedBlockValueRelease with
      | Some (_, MemoryModel.RecursiveRelease sourceType) ->
          releaseManagedRootValueInstrs baseReg 8
            (recursiveNominalRefCountDecHelperLabel sourceType)
            skipLeafFixedBlockValueRelease
      | Some (payloadSize, releasePlan) ->
          releaseFixedBlockValueInstrs baseReg 8 payloadSize releasePlan
            skipLeafFixedBlockValueRelease
      | None -> []
    in
    let dynamicValueInstrs =
      releaseDynamicBufferValueInstrs baseReg skipLeafDynamicValueRelease
    in
    keyInstrs @ dynamicValueInstrs @ listValueInstrs @ dictValueInstrs
    @ closureValueInstrs @ streamValueInstrs @ fixedBlockValueInstrs
  in
  let hasPayloadRelease =
    keyReleasePlan <> MemoryModel.NoReleasePlan
    || Option.is_some releaseLeafDynamicValue
    || releaseLeafListValue
    || Option.is_some releaseLeafDictValueHelper
    || releaseLeafClosureValue || releaseLeafStreamValue
    || Option.is_some leafFixedBlockValueRelease
  in
  let releaseLeafValueInstrs =
    if hasPayloadRelease then
      let release = releasePayloadInstrs X.RDI "leaf" in
      [ X.CMP_imm (X.RDX, 2l); X.Jcc (X.NE, skipLeafPayloadRelease) ]
      @ release
      @ [ X.Label skipLeafPayloadRelease ]
    else []
  in
  let releaseCollisionValueInstrs =
    if hasPayloadRelease then
      let release = releasePayloadInstrs X.R10 "collision" in
      [
        X.CMP_imm (X.RDX, 3l);
        X.Jcc (X.NE, skipCollisionPayloadRelease);
        X.MOV_load (X.R8, X.RDI, 0l);
        X.XOR_reg (X.R9, X.R9);
        X.Label collisionPayloadLoop;
        X.CMP_reg (X.R9, X.R8);
        X.Jcc (X.GE, collisionPayloadDone);
        X.MOV_reg (X.R10, X.R9);
        X.SHL_imm (X.R10, 4);
        X.ADD_imm (X.R10, 8l);
        X.ADD_reg (X.R10, X.RDI);
      ]
      @ release
      @ [
          X.ADD_imm (X.R9, 1l);
          X.JMP collisionPayloadLoop;
          X.Label collisionPayloadDone;
          X.Label skipCollisionPayloadRelease;
        ]
    else []
  in
  [
    X86_64.Label helperLabel;
    X86_64.XOR_reg (X86_64.RCX, X86_64.RCX);
    X86_64.JMP loopCheck;
    X86_64.Label loopCheck;
    X86_64.TEST_reg (X86_64.RAX, X86_64.RAX);
    X86_64.Jcc (X86_64.EQ, popOrRet);
    X86_64.MOV_reg (X86_64.RDX, X86_64.RAX);
    X86_64.AND_imm (X86_64.RDX, 3l);
    X86_64.TEST_reg (X86_64.RDX, X86_64.RDX);
    X86_64.Jcc (X86_64.EQ, popOrRet);
    X86_64.CMP_imm (X86_64.RDX, 3l);
    X86_64.Jcc (X86_64.GT, popOrRet);
    X86_64.MOV_reg (X86_64.RDI, X86_64.RAX);
    X86_64.AND_imm (X86_64.RDI, -8l);
    X86_64.CMP_reg (X86_64.RDI, freeListBase);
    X86_64.Jcc (X86_64.B, popOrRet);
    X86_64.CMP_reg (X86_64.RDI, heapPtr);
    X86_64.Jcc (X86_64.AE, popOrRet);
    X86_64.CMP_imm (X86_64.RDX, 1l);
    X86_64.Jcc (X86_64.EQ, internalTag);
    X86_64.CMP_imm (X86_64.RDX, 2l);
    X86_64.Jcc (X86_64.EQ, leafTag);
    X86_64.JMP collisionTag;
    X86_64.Label leafTag;
    X86_64.MOV_imm32 (X86_64.RSI, 16l);
    X86_64.XOR_reg (X86_64.R8, X86_64.R8);
    X86_64.JMP haveOffset;
    X86_64.Label collisionTag;
    X86_64.MOV_load (X86_64.RSI, X86_64.RDI, 0l);
    X86_64.SHL_imm (X86_64.RSI, 4);
    X86_64.ADD_imm (X86_64.RSI, 8l);
    X86_64.XOR_reg (X86_64.R8, X86_64.R8);
    X86_64.JMP haveOffset;
    X86_64.Label internalTag;
    X86_64.MOV_load (X86_64.R9, X86_64.RDI, 0l);
    X86_64.XOR_reg (X86_64.R8, X86_64.R8);
    X86_64.Label popcountLoop;
    X86_64.TEST_reg (X86_64.R9, X86_64.R9);
    X86_64.Jcc (X86_64.EQ, popcountDone);
    X86_64.MOV_reg (X86_64.R10, X86_64.R9);
    X86_64.SUB_imm (X86_64.R10, 1l);
    X86_64.AND_reg (X86_64.R9, X86_64.R10);
    X86_64.ADD_imm (X86_64.R8, 1l);
    X86_64.JMP popcountLoop;
    X86_64.Label popcountDone;
    X86_64.MOV_reg (X86_64.RSI, X86_64.R8);
    X86_64.SHL_imm (X86_64.RSI, 3);
    X86_64.ADD_imm (X86_64.RSI, 8l);
    X86_64.Label haveOffset;
    X86_64.MOV_reg (X86_64.R9, X86_64.RDI);
    X86_64.ADD_reg (X86_64.R9, X86_64.RSI);
    X86_64.MOV_load (X86_64.R10, X86_64.R9, 0l);
    X86_64.SUB_imm (X86_64.R10, 1l);
    X86_64.MOV_store (X86_64.R9, 0l, X86_64.R10);
    X86_64.TEST_reg (X86_64.R10, X86_64.R10);
    X86_64.Jcc (X86_64.NE, popOrRet);
    X86_64.CMP_imm (X86_64.RDX, 1l);
    X86_64.Jcc (X86_64.EQ, collectInternal);
    X86_64.JMP freeNode;
    X86_64.Label collectInternal;
    X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
    X86_64.XOR_reg (X86_64.R9, X86_64.R9);
    X86_64.Label collectLoop;
    X86_64.CMP_reg (X86_64.R9, X86_64.R8);
    X86_64.Jcc (X86_64.GE, freeNode);
    X86_64.MOV_reg (X86_64.R10, X86_64.R9);
    X86_64.SHL_imm (X86_64.R10, 3);
    X86_64.ADD_imm (X86_64.R10, 8l);
    X86_64.ADD_reg (X86_64.R10, X86_64.RDI);
    X86_64.MOV_load (X86_64.R10, X86_64.R10, 0l);
  ]
  @ addChild "internal"
  @ [
      X86_64.ADD_imm (X86_64.R9, 1l);
      X86_64.JMP collectLoop;
      X86_64.Label freeNode;
    ]
  @ releaseLeafValueInstrs @ releaseCollisionValueInstrs
  @ [
      X86_64.CMP_imm (X86_64.RSI, Int32.of_int freeListSize);
      X86_64.Jcc (X86_64.GE, skipFreeList);
      X86_64.MOV_reg (X86_64.R9, freeListBase);
      X86_64.ADD_reg (X86_64.R9, X86_64.RSI);
      X86_64.MOV_load (X86_64.R10, X86_64.R9, 0l);
      X86_64.MOV_store (X86_64.RDI, 0l, X86_64.R10);
      X86_64.MOV_store (X86_64.R9, 0l, X86_64.RDI);
      X86_64.Label skipFreeList;
    ]
  @ [
      (* Internal nodes selected their first child before reaching freeNode. *)
      (* A released leaf or collision must be cleared before the next loop. *)
      X86_64.CMP_imm (X86_64.RDX, 1l);
      X86_64.Jcc (X86_64.EQ, nextNode);
      X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
      X86_64.Label nextNode;
    ]
  @ leakDec
  @ [
      X86_64.JMP loopCheck;
      X86_64.Label popOrRet;
      X86_64.TEST_reg (X86_64.RCX, X86_64.RCX);
      X86_64.Jcc (X86_64.EQ, helperRet);
      X86_64.POP X86_64.RAX;
      X86_64.SUB_imm (X86_64.RCX, 1l);
      X86_64.JMP loopCheck;
      X86_64.Label helperRet;
      X86_64.RET;
    ]

open! MemoryModel
module F = StructuralFormat

let number n = F.Scalar (string_of_int n)

let kindValue kind =
  F.Union
    ( (match kind with
      | MemoryModel.GenericHeap -> "GenericHeap"
      | StreamHeap -> "StreamHeap"
      | TaggedList -> "TaggedList"
      | DictHeap -> "DictHeap"
      | ClosureHeap -> "ClosureHeap"),
      [] )

let operationValue = function
  | MemoryModel.FixedSizeRoot (size, kind) ->
      F.Union ("FixedSizeRoot", [ number size; kindValue kind ])
  | DynamicStringBuffer -> F.Union ("DynamicStringBuffer", [])
  | DynamicBlobBuffer -> F.Union ("DynamicBlobBuffer", [])
  | DynamicIntBuffer -> F.Union ("DynamicIntBuffer", [])

let rec planValue = function
  | MemoryModel.NoReleasePlan -> F.Union ("NoReleasePlan", [])
  | DynamicBufferRelease operation ->
      F.Union ("DynamicBufferRelease", [ operationValue operation ])
  | RecursiveRelease typ -> F.Union ("RecursiveRelease", [ F.semanticValue typ ])
  | RootRelease (size, kind, payload) ->
      F.Union
        ("RootRelease", [ number size; kindValue kind; payloadValue payload ])

and fieldValue (MemoryModel.FieldRelease (offset, plan)) =
  F.Union ("FieldRelease", [ number offset; planValue plan ])

and fieldsValue fields = F.Sequence (List.map fieldValue fields)

and payloadValue = function
  | MemoryModel.NoPayloadRelease -> F.Union ("NoPayloadRelease", [])
  | FixedBlockPayloadRelease (size, fields) ->
      F.Union ("FixedBlockPayloadRelease", [ number size; fieldsValue fields ])
  | BoxedSumPayloadRelease (size, fields, variants) ->
      F.Union
        ( "BoxedSumPayloadRelease",
          [
            number size;
            fieldsValue fields;
            F.Sequence
              (List.map
                 (fun (variant : MemoryModel.rcBoxedSumVariantRelease) ->
                   F.Record
                     [
                       ("Tag", number variant.MemoryModel.tag);
                       ( "FieldReleases",
                         fieldsValue variant.MemoryModel.fieldReleases );
                     ])
                 variants);
          ] )
  | TaggedListPayloadRelease plan ->
      F.Union ("TaggedListPayloadRelease", [ planValue plan ])
  | DictPayloadRelease (key, value) ->
      F.Union ("DictPayloadRelease", [ planValue key; planValue value ])
  | ClosurePayloadRelease fields ->
      F.Union ("ClosurePayloadRelease", [ fieldsValue fields ])

let planText value = F.format (planValue value)

let generatePlannedDictRefCountDecHelper helperLabel releasePlan enableLeakCheck
    recordRegistry sumShapeRegistry =
  match releasePlan with
  | MemoryModel.RootRelease
      ( _,
        MemoryModel.DictHeap,
        MemoryModel.DictPayloadRelease (keyRelease, valueRelease) ) ->
      let dynamic, list, dict, closure, stream, fixed =
        match valueRelease with
        | MemoryModel.NoReleasePlan -> (None, false, None, false, false, None)
        | MemoryModel.DynamicBufferRelease operation ->
            (Some operation, false, None, false, false, None)
        | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
            (None, true, None, false, false, None)
        | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
            ( None,
              false,
              Some (dictDecHelperForReleasePlan valueRelease),
              false,
              false,
              None )
        | MemoryModel.RecursiveRelease sourceType ->
            ( None,
              false,
              Some (recursiveNominalRefCountDecHelperLabel sourceType),
              false,
              false,
              None )
        | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
            (None, false, None, true, false, None)
        | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) ->
            (None, false, None, false, true, None)
        | MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, _) ->
            (None, false, None, false, false, Some (payloadSize, valueRelease))
      in
      generateDictRefCountDecHelper helperLabel keyRelease dynamic list dict
        closure stream fixed enableLeakCheck recordRegistry sumShapeRegistry
  | other ->
      Crash.crash
        ("x64 planned dict RefCountDec helper requires a DictHeap release \
          plan, got " ^ planText other)
