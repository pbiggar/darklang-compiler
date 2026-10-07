(* X64ListReferenceCounts.ml - Generate tagged-list retain and iterative destruction helpers. *)
[@@@warning "-4"]
open X64Operands
open X64CodeGenTypes
open X64ReleaseSelection
open FieldReferenceCounts
(*
   TaggedList RefCountDec Helper (skew-list iterative DFS)
*)
type listLeafPayloadRelease =
    | NoLeafPayloadRelease
    | FixedBlockPlannedLeafPayload of int * MemoryModel.rcReleasePlan
    | RecursivePlannedLeafPayload of AST.semanticType
    | ListLeafPayload
    | PlannedListLeafPayload of MemoryModel.rcReleasePlan
    | ClosureLeafPayload
    | DictLeafPayload
    | DictListLeafPayload
    | PlannedDictLeafPayload of MemoryModel.rcReleasePlan
    | DynamicBufferLeafPayload
    | DynamicIntLeafPayload

 (* Generate the TaggedList RefCountDec helper function. *)
 (* Called via CALL with the tagged list pointer in RAX. *)
 (* Uses iterative DFS with the machine stack as a work stack. *)
 (* Clobbers all caller-saved registers. Caller must save/restore. *)
 (* *)
 (* Register usage inside the helper: *)
 (*   RAX = current node (tagged pointer, iterative work item) *)
 (*   RCX = pending work count (number of items pushed on stack) *)
 (*   RDX = tag bits *)
 (*   RDI = untagged node address *)
 (*   RSI = payload size *)
 (*   R8  = refcount address, then child pointer for addChild *)
 (*   R9  = refcount value, old free list head, bounds check temp *)
 (*   R10 = prefix/suffix count *)
 (*   R14 = heap pointer (upper bound, read-only) *)
 (*   R15 = free list base (read-only) *)
let generateListRefCountDecHelperWith helperLabel enableLeakCheck recordRegistry sumShapeRegistry leafPayloadRelease =
 let label name=helperLabel^"_"^name in
 let helperCtx={functionName=helperLabel;stackSize=0;usedCalleeSaved=[];enableLeakCheck;recordRegistry;sumShapeRegistry;functionNames=FunctionIdMap.empty} in
 let leakDec=genLeakCounterDec helperCtx in
 (* Inline helper: process child pointer in R8. *)
 (* If R8 is a valid tagged list pointer: *)
 (*   - If RAX == 0: set RAX = R8 (use as next work item) *)
 (*   - Else: PUSH R8 and increment RCX (add to work stack) *)
    let addChild suffix =
        let doneLabel = label ("child_done_"^suffix) in
        let pushLabel = label ("child_push_"^suffix) in
        [ (* Skip null *)
         X86_64.TEST_reg (X86_64.R8, X86_64.R8);
         X86_64.Jcc (X86_64.EQ, doneLabel);
 (* Check tag is in [1,3]. *)
         X86_64.MOV_reg (X86_64.R9, X86_64.R8);
         X86_64.AND_imm (X86_64.R9, 7l);
         X86_64.TEST_reg (X86_64.R9, X86_64.R9);
         X86_64.Jcc (X86_64.EQ, doneLabel);
         X86_64.CMP_imm (X86_64.R9, 3l);
         X86_64.Jcc (X86_64.GT, doneLabel);
 (* Bounds check untagged address *)
         X86_64.MOV_reg (X86_64.R9, X86_64.R8);
         X86_64.AND_imm (X86_64.R9, -8l);
         X86_64.CMP_reg (X86_64.R9, freeListBase);
         X86_64.Jcc (X86_64.B, doneLabel);
         X86_64.CMP_reg (X86_64.R9, heapPtr);
         X86_64.Jcc (X86_64.AE, doneLabel);
 (* Valid child: add to work *)
         X86_64.TEST_reg (X86_64.RAX, X86_64.RAX);
         X86_64.Jcc (X86_64.NE, pushLabel);
         X86_64.MOV_reg (X86_64.RAX, X86_64.R8);
         X86_64.JMP doneLabel;
         X86_64.Label pushLabel;
         X86_64.PUSH X86_64.R8;
         X86_64.ADD_imm (X86_64.RCX, 1l);
         X86_64.Label doneLabel]

 in
    let loopCheck = label "loop_check" in
    let popOrRet = label "pop_or_ret" in
    let helperRet = label "ret" in
    let size8 = label "size_8" in
    let size24 = label "size_24" in
    let size32 = label "size_32" in
    let size96 = label "size_96" in
    let haveSize = label "have_size" in
    let collectSingle = label "collect_single" in
    let collectDeep = label "collect_deep" in
    let collectNode2 = label "collect_node2" in
    let collectNodeChildren = label "collect_node_children" in
    let collectNode3 = label "collect_node3" in
    let collectLeaf = label "collect_leaf" in
    let releaseValue = label "release_value" in
    let leafPayloadDone = label "leaf_payload_done" in
    let afterPrefix = label "after_prefix" in
    let afterSuffix = label "after_suffix" in
    let freeNode = label "free_node" in

    let releaseLeafPayload =
        (match leafPayloadRelease with
        | NoLeafPayloadRelease -> []
        | ListLeafPayload ->
            [X86_64.MOV_load (X86_64.R8, X86_64.RDI, 0l)]
            @ addChild "leaf_payload_list"
        | PlannedListLeafPayload releasePlan ->
            [ X86_64.PUSH X86_64.RDI;
              X86_64.PUSH X86_64.RSI;
              X86_64.PUSH X86_64.RCX;
              X86_64.MOV_load (X86_64.RAX, X86_64.RDI, 0l);
              X86_64.CALL (listDecHelperForReleasePlan releasePlan);
              X86_64.POP X86_64.RCX;
              X86_64.POP X86_64.RSI;
              X86_64.POP X86_64.RDI;
              X86_64.XOR_reg (X86_64.RAX, X86_64.RAX) ]
        | ClosureLeafPayload ->
            [X86_64.PUSH X86_64.RDI;
             X86_64.PUSH X86_64.RSI;
             X86_64.PUSH X86_64.RCX;
             X86_64.MOV_load (X86_64.RAX, X86_64.RDI, 0l);
             X86_64.CALL closureRefCountDecHelperLabel;
             X86_64.POP X86_64.RCX;
             X86_64.POP X86_64.RSI;
             X86_64.POP X86_64.RDI;
             X86_64.XOR_reg (X86_64.RAX, X86_64.RAX)]
        | DictLeafPayload ->
            [X86_64.PUSH X86_64.RDI;
             X86_64.PUSH X86_64.RSI;
             X86_64.PUSH X86_64.RCX;
             X86_64.MOV_load (X86_64.RAX, X86_64.RDI, 0l);
             X86_64.CALL dictRefCountDecHelperLabel;
             X86_64.POP X86_64.RCX;
             X86_64.POP X86_64.RSI;
             X86_64.POP X86_64.RDI;
             X86_64.XOR_reg (X86_64.RAX, X86_64.RAX)]
        | DictListLeafPayload ->
            [X86_64.PUSH X86_64.RDI;
             X86_64.PUSH X86_64.RSI;
             X86_64.PUSH X86_64.RCX;
             X86_64.MOV_load (X86_64.RAX, X86_64.RDI, 0l);
             X86_64.CALL dictRefCountDecListValueHelperLabel;
             X86_64.POP X86_64.RCX;
             X86_64.POP X86_64.RSI;
             X86_64.POP X86_64.RDI;
             X86_64.XOR_reg (X86_64.RAX, X86_64.RAX)]
        | PlannedDictLeafPayload releasePlan ->
            [ X86_64.PUSH X86_64.RDI;
              X86_64.PUSH X86_64.RSI;
              X86_64.PUSH X86_64.RCX;
              X86_64.MOV_load (X86_64.RAX, X86_64.RDI, 0l);
              X86_64.CALL (dictDecHelperForReleasePlan releasePlan);
              X86_64.POP X86_64.RCX;
              X86_64.POP X86_64.RSI;
              X86_64.POP X86_64.RDI;
              X86_64.XOR_reg (X86_64.RAX, X86_64.RAX) ]
        | DynamicBufferLeafPayload ->
            [X86_64.MOV_load (X86_64.R8, X86_64.RDI, 0l);
             X86_64.TEST_reg (X86_64.R8, X86_64.R8);
             X86_64.Jcc (X86_64.EQ, leafPayloadDone);
             X86_64.MOV_reg (X86_64.R10, X86_64.R8);
             X86_64.MOV_load (X86_64.R9, X86_64.R10, 0l)]
            @ loadImm64 scratch 0x7FFFFFFFFFFFFFFFL
            @ [X86_64.CMP_reg (X86_64.R9, scratch);
               X86_64.Jcc (X86_64.EQ, leafPayloadDone);
               X86_64.SUB_imm (X86_64.R9, 1l);
               X86_64.MOV_store (X86_64.R10, 0l, X86_64.R9);
               X86_64.TEST_reg (X86_64.R9, X86_64.R9);
               X86_64.Jcc (X86_64.NE, leafPayloadDone)]
            @ leakDec
            @ [X86_64.Label leafPayloadDone]
        | DynamicIntLeafPayload ->
            [X86_64.MOV_load (X86_64.R8, X86_64.RDI, 0l);
             X86_64.TEST_reg (X86_64.R8, X86_64.R8);
             X86_64.Jcc (X86_64.EQ, leafPayloadDone);
             X86_64.MOV_reg (X86_64.R10, X86_64.R8);
             X86_64.AND_imm (X86_64.R10, 1l);
             X86_64.Jcc (X86_64.NE, leafPayloadDone);
             X86_64.MOV_reg (X86_64.R10, X86_64.R8);
             X86_64.MOV_load (X86_64.R9, X86_64.R10, 0l)]
            @ loadImm64 scratch 0x7FFFFFFFFFFFFFFFL
            @ [X86_64.CMP_reg (X86_64.R9, scratch);
               X86_64.Jcc (X86_64.EQ, leafPayloadDone);
               X86_64.SUB_imm (X86_64.R9, 1l);
               X86_64.MOV_store (X86_64.R10, 0l, X86_64.R9);
               X86_64.TEST_reg (X86_64.R9, X86_64.R9);
               X86_64.Jcc (X86_64.NE, leafPayloadDone)]
            @ leakDec
            @ [X86_64.Label leafPayloadDone]
        | FixedBlockPlannedLeafPayload (payloadSize, releasePlan) ->
            [X86_64.PUSH X86_64.RDI;
             X86_64.PUSH X86_64.RSI;
             X86_64.MOV_load (X86_64.R8, X86_64.RDI, 0l)]
            @ genRefCountDecGenericWithPlan helperCtx X86_64.R8 payloadSize (Some releasePlan)
            @ [X86_64.POP X86_64.RSI;
               X86_64.POP X86_64.RDI]
        | RecursivePlannedLeafPayload sourceType ->
            [X86_64.PUSH X86_64.RDI;
             X86_64.PUSH X86_64.RSI;
             X86_64.PUSH X86_64.RCX;
             X86_64.MOV_load (X86_64.RAX, X86_64.RDI, 0l);
             X86_64.CALL (recursiveNominalRefCountDecHelperLabel sourceType);
             X86_64.POP X86_64.RCX;
             X86_64.POP X86_64.RSI;
             X86_64.POP X86_64.RDI;
             X86_64.XOR_reg (X86_64.RAX, X86_64.RAX)]
 ) in
    [X86_64.Label helperLabel;
 (* Preserve callee-saved registers used to keep the current node stable *)
 (* across payload-release helpers. Pending DFS work is pushed above them. *)
     X86_64.PUSH X86_64.R12;
     X86_64.PUSH X86_64.R13;
 (* RAX = tagged list pointer, init pending count *)
     X86_64.XOR_reg (X86_64.RCX, X86_64.RCX); (* pending = 0 *)
     X86_64.JMP loopCheck;

     X86_64.Label loopCheck;
     X86_64.TEST_reg (X86_64.RAX, X86_64.RAX);
     X86_64.Jcc (X86_64.EQ, popOrRet);
 (* Extract tag *)
     X86_64.MOV_reg (X86_64.RDX, X86_64.RAX);
     X86_64.AND_imm (X86_64.RDX, 7l);
     X86_64.TEST_reg (X86_64.RDX, X86_64.RDX);
     X86_64.Jcc (X86_64.EQ, popOrRet);
 (* Untag: RDI = RAX & ~7 *)
     X86_64.MOV_reg (X86_64.RDI, X86_64.RAX);
     X86_64.AND_imm (X86_64.RDI, -8l);
 (* Bounds check *)
     X86_64.CMP_reg (X86_64.RDI, freeListBase);
     X86_64.Jcc (X86_64.B, popOrRet);
     X86_64.CMP_reg (X86_64.RDI, heapPtr);
     X86_64.Jcc (X86_64.AE, popOrRet);

 (* Resolve payload size from tag *)
     X86_64.CMP_imm (X86_64.RDX, 1l);
     X86_64.Jcc (X86_64.EQ, size32); (* DIGIT → 32 *)
     X86_64.CMP_imm (X86_64.RDX, 2l);
     X86_64.Jcc (X86_64.EQ, size8); (* LEAF → 8 *)
     X86_64.CMP_imm (X86_64.RDX, 3l);
     X86_64.Jcc (X86_64.EQ, size24); (* NODE → 24 *)
     X86_64.JMP popOrRet;

     X86_64.Label size8;
     X86_64.MOV_imm32 (X86_64.RSI, 8l);
     X86_64.JMP haveSize;
     X86_64.Label size24;
     X86_64.MOV_imm32 (X86_64.RSI, 24l);
     X86_64.JMP haveSize;
     X86_64.Label size32;
     X86_64.MOV_imm32 (X86_64.RSI, 32l);
     X86_64.JMP haveSize;
     X86_64.Label size96;
     X86_64.MOV_imm32 (X86_64.RSI, 96l);

 (* RSI = payload size, RDI = untagged address, RDX = tag *)
     X86_64.Label haveSize;
 (* Refcount at [RDI + RSI] *)
     X86_64.MOV_reg (X86_64.R8, X86_64.RDI);
     X86_64.ADD_reg (X86_64.R8, X86_64.RSI); (* R8 = &refcount *)
     X86_64.MOV_load (X86_64.R9, X86_64.R8, 0l); (* R9 = refcount *)
     X86_64.SUB_imm (X86_64.R9, 1l);
     X86_64.MOV_store (X86_64.R8, 0l, X86_64.R9); (* store decremented *)
     X86_64.TEST_reg (X86_64.R9, X86_64.R9);
     X86_64.Jcc (X86_64.NE, popOrRet); (* refcount > 0, done *)

 (* Refcount zero: collect children then free *)
     X86_64.CMP_imm (X86_64.RDX, 1l);
     X86_64.Jcc (X86_64.EQ, collectSingle);
     X86_64.CMP_imm (X86_64.RDX, 2l);
     X86_64.Jcc (X86_64.EQ, collectLeaf);
     X86_64.CMP_imm (X86_64.RDX, 3l);
     X86_64.Jcc (X86_64.EQ, collectNode2);
     X86_64.JMP freeNode]

 (* --- DIGIT (tag 1): tree and remaining-spine children at 16 and 24 --- *)
    @ [X86_64.Label collectSingle;
       X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 16l)]
    @ addChild "digit_tree"
    @ [X86_64.MOV_load (X86_64.R8, X86_64.RDI, 24l)]
    @ addChild "digit_rest"
    @ [X86_64.JMP freeNode]

 (* --- NODE (tag 3): release the value before collecting children. Payload *)
 (* helpers may use RAX as scratch, so no structural work can be pending yet. *)
    @ [X86_64.Label collectNode2;
       X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
       X86_64.JMP releaseValue]

 (* --- NODE3 (tag 4): three children at offsets 0, 8, 16 --- *)
    @ [X86_64.Label collectNode3;
       X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 0l)]
    @ addChild "node3_0"
    @ [X86_64.MOV_load (X86_64.R8, X86_64.RDI, 8l)]
    @ addChild "node3_1"
    @ [X86_64.MOV_load (X86_64.R8, X86_64.RDI, 16l)]
    @ addChild "node3_2"
    @ [X86_64.JMP freeNode]

 (* --- DEEP (tag 2): prefix[0..3], middle, suffix[0..3] --- *)
    @ [X86_64.Label collectDeep;
       X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
 (* Prefix: count at offset 8, children at offsets 16,24,32,40 *)
       X86_64.MOV_load (X86_64.R10, X86_64.RDI, 8l); (* prefix_count *)
       X86_64.TEST_reg (X86_64.R10, X86_64.R10);
       X86_64.Jcc (X86_64.LE, afterPrefix);
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 16l)]
    @ addChild "deep_p0"
    @ [X86_64.CMP_imm (X86_64.R10, 1l);
       X86_64.Jcc (X86_64.LE, afterPrefix);
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 24l)]
    @ addChild "deep_p1"
    @ [X86_64.CMP_imm (X86_64.R10, 2l);
       X86_64.Jcc (X86_64.LE, afterPrefix);
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 32l)]
    @ addChild "deep_p2"
    @ [X86_64.CMP_imm (X86_64.R10, 3l);
       X86_64.Jcc (X86_64.LE, afterPrefix);
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 40l)]
    @ addChild "deep_p3"
    @ [X86_64.Label afterPrefix;
 (* Middle tree at offset 48 *)
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 48l)]
    @ addChild "deep_middle"
    @ [ (* Suffix: count at offset 56, children at offsets 64,72,80,88 *)
       X86_64.MOV_load (X86_64.R10, X86_64.RDI, 56l); (* suffix_count *)
       X86_64.TEST_reg (X86_64.R10, X86_64.R10);
       X86_64.Jcc (X86_64.LE, afterSuffix);
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 64l)]
    @ addChild "deep_s0"
    @ [X86_64.CMP_imm (X86_64.R10, 1l);
       X86_64.Jcc (X86_64.LE, afterSuffix);
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 72l)]
    @ addChild "deep_s1"
    @ [X86_64.CMP_imm (X86_64.R10, 2l);
       X86_64.Jcc (X86_64.LE, afterSuffix);
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 80l)]
    @ addChild "deep_s2"
    @ [X86_64.CMP_imm (X86_64.R10, 3l);
       X86_64.Jcc (X86_64.LE, afterSuffix);
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 88l)]
    @ addChild "deep_s3"
    @ [X86_64.Label afterSuffix;
       X86_64.JMP freeNode]

 (* --- LEAF (tag 2): value at offset 0 and no children --- *)
    @ [X86_64.Label collectLeaf;
       X86_64.XOR_reg (X86_64.RAX, X86_64.RAX);
       X86_64.Label releaseValue;
       X86_64.MOV_reg (X86_64.R12, X86_64.RDX);
       X86_64.MOV_reg (X86_64.R13, X86_64.RDI)]
    @ releaseLeafPayload

 (* A leaf is finished after payload release. An internal node still owns *)
 (* its left and right tree edges. *)
    @ [X86_64.MOV_reg (X86_64.RDX, X86_64.R12);
       X86_64.MOV_reg (X86_64.RDI, X86_64.R13);
       X86_64.CMP_imm (X86_64.RDX, 3l);
       X86_64.Jcc (X86_64.NE, freeNode);
       X86_64.Label collectNodeChildren;
       X86_64.MOV_load (X86_64.R8, X86_64.RDI, 8l)]
    @ addChild "node_left"
    @ [X86_64.MOV_load (X86_64.R8, X86_64.RDI, 16l)]
    @ addChild "node_right"

 (* --- Free node to free list by payload size class --- *)
    @ [X86_64.Label freeNode;
 (* freeList[RSI] = node; node.next = old_head *)
       X86_64.MOV_reg (X86_64.R8, freeListBase);
       X86_64.ADD_reg (X86_64.R8, X86_64.RSI); (* R8 = &freeList[payload_class] *)
       X86_64.MOV_load (X86_64.R9, X86_64.R8, 0l); (* R9 = old head *)
       X86_64.MOV_store (X86_64.RDI, 0l, X86_64.R9); (* node.next = old head *)
       X86_64.MOV_store (X86_64.R8, 0l, X86_64.RDI)] (* freeList[class] = node *)
    @ leakDec
    @ [X86_64.JMP loopCheck]

 (* --- Pop from work stack or return --- *)
    @ [X86_64.Label popOrRet;
       X86_64.TEST_reg (X86_64.RCX, X86_64.RCX);
       X86_64.Jcc (X86_64.EQ, helperRet);
       X86_64.POP X86_64.RAX;
       X86_64.SUB_imm (X86_64.RCX, 1l);
       X86_64.JMP loopCheck;

       X86_64.Label helperRet;
       X86_64.POP X86_64.R13;
       X86_64.POP X86_64.R12;
       X86_64.RET]

let listRefCountDecHelperSpecs : (string * listLeafPayloadRelease) list =
    [
    (listRefCountDecHelperLabel, NoLeafPayloadRelease);
    (listRefCountDecListHelperLabel, ListLeafPayload);
    (listRefCountDecClosureHelperLabel, ClosureLeafPayload);
    (listRefCountDecDictHelperLabel, DictLeafPayload);
    (listRefCountDecDictListHelperLabel, DictListLeafPayload);
    (listRefCountDecDynamicBufferHelperLabel, DynamicBufferLeafPayload);
    (listRefCountDecDynamicIntHelperLabel, DynamicIntLeafPayload);
    ]

let listLeafPayloadNeedsDictDecHelper (leafPayloadRelease: listLeafPayloadRelease) : bool =
    match leafPayloadRelease with
    | DictLeafPayload ->
        true
    | FixedBlockPlannedLeafPayload (_, releasePlan) ->
        rcReleasePlanContains (releasePlanIsRootKind MemoryModel.DictHeap) releasePlan
    | RecursivePlannedLeafPayload _ ->
        false
    | DictListLeafPayload ->
        false
    | PlannedDictLeafPayload _ ->
        false
    | NoLeafPayloadRelease
    | ListLeafPayload
    | PlannedListLeafPayload _
    | ClosureLeafPayload
    | DynamicBufferLeafPayload
    | DynamicIntLeafPayload ->
        false

let listLeafPayloadNeedsDictListValueDecHelper (leafPayloadRelease: listLeafPayloadRelease) : bool =
    match leafPayloadRelease with
    | DictListLeafPayload ->
        true
    | PlannedDictLeafPayload _ ->
        false
    | FixedBlockPlannedLeafPayload (_, releasePlan) ->
        rcReleasePlanContains releasePlanIsDictWithListValue releasePlan
    | RecursivePlannedLeafPayload _ ->
        false
    | NoLeafPayloadRelease
    | ListLeafPayload
    | PlannedListLeafPayload _
    | ClosureLeafPayload
    | DictLeafPayload
    | DynamicBufferLeafPayload
    | DynamicIntLeafPayload ->
        false

let listLeafPayloadNeedsClosureDecHelper (leafPayloadRelease: listLeafPayloadRelease) : bool =
    match leafPayloadRelease with
    | ClosureLeafPayload ->
        true
    | FixedBlockPlannedLeafPayload (_, releasePlan) ->
        rcReleasePlanContains (releasePlanIsRootKind MemoryModel.ClosureHeap) releasePlan
    | RecursivePlannedLeafPayload _ ->
        false
    | NoLeafPayloadRelease
    | ListLeafPayload
    | PlannedListLeafPayload _
    | DictLeafPayload
    | DictListLeafPayload
    | PlannedDictLeafPayload _
    | DynamicBufferLeafPayload
    | DynamicIntLeafPayload ->
        false

let generateNeededListRefCountDecHelpers neededListDecHelperLabels plannedListDecHelpers enableLeakCheck recordRegistry sumShapeRegistry =
 let staticHelpers=List.concat_map (fun (helperLabel,leafPayloadRelease)->
  if StringOrder.Set.mem helperLabel neededListDecHelperLabels then generateListRefCountDecHelperWith helperLabel enableLeakCheck recordRegistry sumShapeRegistry leafPayloadRelease else []) listRefCountDecHelperSpecs in
 let plannedHelpers=StringOrder.Map.bindings plannedListDecHelpers |> List.concat_map (fun (helperLabel,(payloadSize,releasePlan))->
  if StringOrder.Set.mem helperLabel neededListDecHelperLabels then
   let leafPayloadRelease=match releasePlan with
    | MemoryModel.RecursiveRelease sourceType -> RecursivePlannedLeafPayload sourceType
    | MemoryModel.RootRelease (_,MemoryModel.TaggedList,_) -> PlannedListLeafPayload releasePlan
    | MemoryModel.RootRelease (_,MemoryModel.DictHeap,_) -> PlannedDictLeafPayload releasePlan
    | _ -> FixedBlockPlannedLeafPayload (payloadSize,releasePlan) in
   generateListRefCountDecHelperWith helperLabel enableLeakCheck recordRegistry sumShapeRegistry leafPayloadRelease
  else []) in
 staticHelpers@plannedHelpers
let selectedListRefCountDecHelpersNeedDictDecHelper neededListDecHelperLabels =
 List.exists (fun (helperLabel,leafPayloadRelease)->StringOrder.Set.mem helperLabel neededListDecHelperLabels && listLeafPayloadNeedsDictDecHelper leafPayloadRelease) listRefCountDecHelperSpecs
let selectedListRefCountDecHelpersNeedDictListValueDecHelper neededListDecHelperLabels =
 List.exists (fun (helperLabel,leafPayloadRelease)->StringOrder.Set.mem helperLabel neededListDecHelperLabels && listLeafPayloadNeedsDictListValueDecHelper leafPayloadRelease) listRefCountDecHelperSpecs
let selectedListRefCountDecHelpersNeedClosureDecHelper neededListDecHelperLabels =
 List.exists (fun (helperLabel,leafPayloadRelease)->StringOrder.Set.mem helperLabel neededListDecHelperLabels && listLeafPayloadNeedsClosureDecHelper leafPayloadRelease) listRefCountDecHelperSpecs
 (* ============================================================================ *)
 (* TaggedList RefCountInc Helper (increment root node refcount only) *)
 (* ============================================================================ *)

 (* Label for the shared list refcount inc helper function *)
let listRefCountIncHelperLabel = "__dark_list_rc_inc_helper"

 (* Generate the TaggedList RefCountInc helper function. *)
 (* Called via CALL with the tagged list pointer in RAX. *)
 (* Just increments the refcount of the root node (no recursion). *)
 (* Clobbers RCX, RDX, RDI. Caller must save/restore. *)
let generateListRefCountIncHelper () =
    let label name = "__dark_list_rc_inc_"^name in
    let helperRet = label "ret" in
    let size24 = label "size_24" in
    let size32 = label "size_32" in
    let haveSize = label "have_size" in

    [X86_64.Label listRefCountIncHelperLabel;
 (* RAX = tagged list pointer (or 0) *)
     X86_64.TEST_reg (X86_64.RAX, X86_64.RAX);
     X86_64.Jcc (X86_64.EQ, helperRet);
 (* Extract tag *)
     X86_64.MOV_reg (X86_64.RCX, X86_64.RAX);
     X86_64.AND_imm (X86_64.RCX, 7l);
     X86_64.TEST_reg (X86_64.RCX, X86_64.RCX);
     X86_64.Jcc (X86_64.EQ, helperRet);
     X86_64.CMP_imm (X86_64.RCX, 3l);
     X86_64.Jcc (X86_64.GT, helperRet);
 (* Untag: RDI = RAX & ~7 *)
     X86_64.MOV_reg (X86_64.RDI, X86_64.RAX);
     X86_64.AND_imm (X86_64.RDI, -8l);
 (* Resolve payload size from tag *)
     X86_64.CMP_imm (X86_64.RCX, 1l);
     X86_64.Jcc (X86_64.EQ, size32);
     X86_64.CMP_imm (X86_64.RCX, 3l);
     X86_64.Jcc (X86_64.EQ, size24);
 (* Tag 2 (LEAF): payload = 8. *)
     X86_64.MOV_imm32 (X86_64.RDX, 8l);
     X86_64.JMP haveSize;
     X86_64.Label size24;
     X86_64.MOV_imm32 (X86_64.RDX, 24l);
     X86_64.JMP haveSize;
     X86_64.Label size32;
     X86_64.MOV_imm32 (X86_64.RDX, 32l);
     X86_64.JMP haveSize;
 (* RDX = payload size, RDI = untagged address *)
     X86_64.Label haveSize;
     X86_64.ADD_reg (X86_64.RDI, X86_64.RDX); (* RDI = &refcount *)
     X86_64.MOV_load (X86_64.RDX, X86_64.RDI, 0l); (* RDX = refcount *)
     X86_64.ADD_imm (X86_64.RDX, 1l);
     X86_64.MOV_store (X86_64.RDI, 0l, X86_64.RDX); (* store incremented *)
     X86_64.Label helperRet;
     X86_64.RET]
