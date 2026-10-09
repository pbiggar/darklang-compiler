(*
   ARM64ListReferenceCounts.ml - Generate tagged-list retain and iterative destruction helpers.
*)
[@@@warning "-4"]

let isEmpty xs = xs = []
let mul a b = Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))

let int16 value =
  let low = value land 65535 in
  if low >= 32768 then low - 65536 else low

let uint16 value = value land 65535

open ARM64CodeGenTypes
open HeapAllocation

(*
   X0 = tagged list pointer (or 0)
   Untagged pointer => not a skew-list node
   This contiguous all-ones mask is an encodable AArch64 logical immediate.
   Tag 2 (LEAF).
*)
let generateListRefCountIncHelper () : Symbolic.instr list =
  let label (name : string) : string =
    Printf.sprintf "__dark_list_rc_inc_%s" name
  in
  let size24 = label "size_24" in
  let size32 = label "size_32" in
  let haveSize = label "have_size" in
  let helperRet = label "ret" in
  [
    Symbolic.Label listRefCountIncHelperLabel;
    Symbolic.CBZ (Symbolic.X0, helperRet);
    Symbolic.AND_imm (Symbolic.X1, Symbolic.X0, 7L);
    Symbolic.CBZ (Symbolic.X1, helperRet);
    Symbolic.CMP_imm (Symbolic.X1, 3);
    Symbolic.B_cond_label (Symbolic.GT, helperRet);
    Symbolic.AND_imm (Symbolic.X2, Symbolic.X0, 0xFFFFFFFFFFFFFFF8L);
    Symbolic.CMP_imm (Symbolic.X1, 1);
    Symbolic.B_cond_label (Symbolic.EQ, size32);
    Symbolic.CMP_imm (Symbolic.X1, 3);
    Symbolic.B_cond_label (Symbolic.EQ, size24);
    Symbolic.MOVZ (Symbolic.X3, 8, 0);
    Symbolic.B_label haveSize;
    Symbolic.Label size24;
    Symbolic.MOVZ (Symbolic.X3, 24, 0);
    Symbolic.B_label haveSize;
    Symbolic.Label size32;
    Symbolic.MOVZ (Symbolic.X3, 32, 0);
    Symbolic.B_label haveSize;
    Symbolic.Label haveSize;
    Symbolic.ADD_reg (Symbolic.X2, Symbolic.X2, Symbolic.X3);
    Symbolic.LDR (Symbolic.X4, Symbolic.X2, 0);
    Symbolic.ADD_imm (Symbolic.X4, Symbolic.X4, 1);
    Symbolic.STR (Symbolic.X4, Symbolic.X2, 0);
    Symbolic.Label helperRet;
    Symbolic.RET;
  ]

(*
   Only traverse plausible tagged list pointers inside the managed heap range.
   This contiguous all-ones mask is an encodable AArch64 logical immediate.
   Specialized fixed-block helpers can encounter primitive leaves from nested lists.
   Only aligned managed-heap payload pointers have trailing refcounts to decrement.
   Field release may use X8 for nested payloads; reload the leaf payload before freeing it.
   Primitive elements have no payload-release code and cannot clobber the
   current node. Only helpers with actual payload work need X19-X21 or a
   callee-save frame; the separate DFS work stack is unchanged.
   Preserve callee-saved registers used to keep the current node stable
   across payload-release helpers. Pending DFS entries are pushed above.
   X0 = current tagged list pointer to process, X1 = number of pending stack entries.
   Resolve payload size from skew-list node tag.
   Refcount reached zero: collect child pointers for further decref work.
   DIGIT: tree and remaining-spine children at 16 and 24.
   NODE: release the value before collecting structural children because
   payload helpers may use X0 as scratch/work state.
   prefix_count
   middle tree
   suffix_count
   Leaves are done after payload release. Internal nodes still own two
   complete-tree edges.
   Recycle node memory by payload size class.
*)
let generateListRefCountDecHelperWith (helperLabel : string)
    (ctx : codeGenContext) (leafGenericPayloadSize : int option)
    (leafGenericReleasePlan : MemoryModel.rcReleasePlan option)
    (releaseLeafDynamicBufferPayload : bool) (releaseLeafListPayload : bool)
    (releaseLeafDictPayload : bool) (releaseLeafClosurePayload : bool)
    (managedLeafFieldTypes : AST.semanticType list) : Symbolic.instr list =
  let label (name : string) : string =
    Printf.sprintf "%s_%s" helperLabel name
  in
  let leakDec =
    if ctx.options.enableLeakCheck then
      let labelRef = dataLabel leakCounterLabel in
      [
        Symbolic.ADRP (Symbolic.X17, labelRef);
        Symbolic.ADD_label (Symbolic.X17, Symbolic.X17, labelRef);
        Symbolic.LDR (Symbolic.X16, Symbolic.X17, 0);
        Symbolic.SUB_imm (Symbolic.X16, Symbolic.X16, 1);
        Symbolic.STR (Symbolic.X16, Symbolic.X17, 0);
      ]
    else []
  in
  let addChild (suffix : string) : Symbolic.instr list =
    let doneLabel = label (Printf.sprintf "child_done_%s" suffix) in
    let pushLabel = label (Printf.sprintf "child_push_%s" suffix) in
    [
      Symbolic.CBZ (Symbolic.X8, doneLabel);
      Symbolic.AND_imm (Symbolic.X9, Symbolic.X8, 7L);
      Symbolic.CBZ (Symbolic.X9, doneLabel);
      Symbolic.CMP_imm (Symbolic.X9, 3);
      Symbolic.B_cond_label (Symbolic.GT, doneLabel);
      Symbolic.AND_imm (Symbolic.X10, Symbolic.X8, 0xFFFFFFFFFFFFFFF8L);
      Symbolic.CMP_reg (Symbolic.X10, Symbolic.X27);
      Symbolic.B_cond_label (Symbolic.LT, doneLabel);
      Symbolic.CMP_reg (Symbolic.X10, Symbolic.X28);
      Symbolic.B_cond_label (Symbolic.GT, doneLabel);
      Symbolic.CBNZ (Symbolic.X0, pushLabel);
      Symbolic.MOV_reg (Symbolic.X0, Symbolic.X8);
      Symbolic.B_label doneLabel;
      Symbolic.Label pushLabel;
      Symbolic.SUB_imm (Symbolic.SP, Symbolic.SP, 16);
      Symbolic.STR (Symbolic.X8, Symbolic.SP, 0);
      Symbolic.ADD_imm (Symbolic.X1, Symbolic.X1, 1);
      Symbolic.Label doneLabel;
    ]
  in
  let loopCheck = label "loop_check" in
  let popOrRet = label "pop_or_ret" in
  let helperRet = label "ret" in
  let size8 = label "size_8" in
  let size24 = label "size_24" in
  let size32 = label "size_32" in
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
  let releaseClosurePayload =
    [
      Symbolic.LDR (Symbolic.X8, Symbolic.X3, 0);
      Symbolic.CBZ (Symbolic.X8, leafPayloadDone);
      Symbolic.STP_pre (Symbolic.X0, Symbolic.X1, Symbolic.SP, -96);
      Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
      Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
      Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
      Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
      Symbolic.STR (Symbolic.X30, Symbolic.SP, 80);
      Symbolic.MOV_reg (Symbolic.X0, Symbolic.X8);
      Symbolic.BL closureRefCountDecHelperLabel;
      Symbolic.LDR (Symbolic.X30, Symbolic.SP, 80);
      Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
      Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
      Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
      Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
      Symbolic.LDP_post (Symbolic.X0, Symbolic.X1, Symbolic.SP, 96);
      Symbolic.Label leafPayloadDone;
    ]
  in
  let releaseDynamicBufferPayload =
    let refcountUpdate =
      if isEmpty leakDec then
        [
          Symbolic.SUB_imm (Symbolic.X14, Symbolic.X14, 1);
          Symbolic.STR (Symbolic.X14, Symbolic.X12, 0);
        ]
      else
        [
          Symbolic.SUB_imm (Symbolic.X14, Symbolic.X14, 1);
          Symbolic.STR (Symbolic.X14, Symbolic.X12, 0);
          Symbolic.CBNZ (Symbolic.X14, leafPayloadDone);
        ]
        @ leakDec
    in
    let taggedGuard =
      if helperLabel = listRefCountDecBlobHelperLabel then
        [
          Symbolic.AND_imm (Symbolic.X13, Symbolic.X12, 1L);
          Symbolic.CBNZ (Symbolic.X13, leafPayloadDone);
        ]
      else []
    in
    [
      Symbolic.LDR (Symbolic.X12, Symbolic.X3, 0);
      Symbolic.CBZ (Symbolic.X12, leafPayloadDone);
    ]
    @ taggedGuard
    @ [
        Symbolic.CMP_reg (Symbolic.X12, Symbolic.X27);
        Symbolic.B_cond_label (Symbolic.LT, leafPayloadDone);
        Symbolic.CMP_reg (Symbolic.X12, Symbolic.X28);
        Symbolic.B_cond_label (Symbolic.GT, leafPayloadDone);
        Symbolic.LDR (Symbolic.X14, Symbolic.X12, 0);
        Symbolic.MOVZ (Symbolic.X15, 0xFFFF, 0);
        Symbolic.MOVK (Symbolic.X15, 0xFFFF, 16);
        Symbolic.MOVK (Symbolic.X15, 0xFFFF, 32);
        Symbolic.MOVK (Symbolic.X15, 0x7FFF, 48);
        Symbolic.CMP_reg (Symbolic.X14, Symbolic.X15);
        Symbolic.B_cond_label (Symbolic.EQ, leafPayloadDone);
      ]
    @ refcountUpdate
    @ [ Symbolic.Label leafPayloadDone ]
  in
  let releaseRecursivePayload (sourceType : AST.semanticType) =
    [
      Symbolic.LDR (Symbolic.X8, Symbolic.X3, 0);
      Symbolic.CBZ (Symbolic.X8, leafPayloadDone);
      Symbolic.STP_pre (Symbolic.X0, Symbolic.X1, Symbolic.SP, -96);
      Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
      Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
      Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
      Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
      Symbolic.STR (Symbolic.X30, Symbolic.SP, 80);
      Symbolic.MOV_reg (Symbolic.X0, Symbolic.X8);
      Symbolic.BL (recursiveNominalRefCountDecHelperLabel sourceType);
      Symbolic.LDR (Symbolic.X30, Symbolic.SP, 80);
      Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
      Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
      Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
      Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
      Symbolic.LDP_post (Symbolic.X0, Symbolic.X1, Symbolic.SP, 96);
      Symbolic.Label leafPayloadDone;
    ]
  in
  let leafListHelperLabelForReleasePlan
      (releasePlan : MemoryModel.rcReleasePlan) : string =
    match releasePlan with
    | MemoryModel.RootRelease
        ( _,
          MemoryModel.TaggedList,
          MemoryModel.TaggedListPayloadRelease elementRelease ) -> (
        match elementRelease with
        | MemoryModel.NoReleasePlan -> listRefCountDecHelperLabel
        | MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer ->
            listRefCountDecStringHelperLabel
        | MemoryModel.DynamicBufferRelease MemoryModel.DynamicBlobBuffer ->
            listRefCountDecBlobHelperLabel
        | MemoryModel.DynamicBufferRelease MemoryModel.DynamicIntBuffer ->
            listRefCountDecBlobHelperLabel
        | MemoryModel.DynamicBufferRelease _ ->
            Crash.crash
              "list dynamic-buffer release used a fixed-size operation"
        | MemoryModel.RecursiveRelease sourceType ->
            plannedListDecHelperLabelForReleasePlan
              (MemoryModel.RecursiveRelease sourceType)
        | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
            plannedListDecHelperLabelForReleasePlan elementRelease
        | MemoryModel.RootRelease
            ( _,
              MemoryModel.DictHeap,
              MemoryModel.DictPayloadRelease
                (MemoryModel.DynamicBufferRelease _, _) )
        | MemoryModel.RootRelease
            ( _,
              MemoryModel.DictHeap,
              MemoryModel.DictPayloadRelease
                (_, MemoryModel.DynamicBufferRelease _) ) ->
            plannedListDecHelperLabelForReleasePlan elementRelease
        | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
            listRefCountDecDictHelperLabel
        | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
            listRefCountDecClosureHelperLabel
        | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) ->
            plannedListDecHelperLabelForReleasePlan elementRelease
        | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, _) ->
            plannedListDecHelperLabelForReleasePlan elementRelease)
    | _ -> listRefCountDecHelperLabel
  in
  let leafDictHelperLabelForReleasePlan
      (releasePlan : MemoryModel.rcReleasePlan) : string =
    match releasePlan with
    | (MemoryModel.RootRelease
         ( _,
           MemoryModel.DictHeap,
           MemoryModel.DictPayloadRelease (MemoryModel.DynamicBufferRelease _, _)
         ) as dictReleasePlan)
    | (MemoryModel.RootRelease
         ( _,
           MemoryModel.DictHeap,
           MemoryModel.DictPayloadRelease (_, MemoryModel.DynamicBufferRelease _)
         ) as dictReleasePlan) ->
        plannedDictDecHelperLabelForReleasePlan dictReleasePlan
    | MemoryModel.RootRelease
        ( _,
          MemoryModel.DictHeap,
          MemoryModel.DictPayloadRelease
            (_, MemoryModel.RootRelease (_, MemoryModel.TaggedList, _)) ) ->
        dictRefCountDecListValueHelperLabel
    | MemoryModel.RootRelease
        ( _,
          MemoryModel.DictHeap,
          MemoryModel.DictPayloadRelease
            (_, MemoryModel.RootRelease (_, MemoryModel.DictHeap, _)) ) ->
        dictRefCountDecDictValueHelperLabel
    | _ -> dictRefCountDecHelperLabel
  in
  let releaseLeafDictPayloadWithHelper (dictHelperLabel : string) =
    [
      Symbolic.LDR (Symbolic.X8, Symbolic.X3, 0);
      Symbolic.CBZ (Symbolic.X8, leafPayloadDone);
      Symbolic.STP_pre (Symbolic.X0, Symbolic.X1, Symbolic.SP, -96);
      Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
      Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
      Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
      Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
      Symbolic.STR (Symbolic.X30, Symbolic.SP, 80);
      Symbolic.MOV_reg (Symbolic.X0, Symbolic.X8);
      Symbolic.BL dictHelperLabel;
      Symbolic.LDR (Symbolic.X30, Symbolic.SP, 80);
      Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
      Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
      Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
      Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
      Symbolic.LDP_post (Symbolic.X0, Symbolic.X1, Symbolic.SP, 96);
      Symbolic.Label leafPayloadDone;
    ]
  in
  let releasePlanDynamicBufferFieldFrom (baseReg : Symbolic.reg)
      (fieldOffset : int) (path : string) (operation : MemoryModel.rcOperation)
      : Symbolic.instr list =
    let fieldDone =
      label (Printf.sprintf "leaf_plan_dynamic_%s_%d_done" path fieldOffset)
    in
    let refcountUpdate =
      if isEmpty leakDec then
        [
          Symbolic.SUB_imm (Symbolic.X14, Symbolic.X14, 1);
          Symbolic.STR (Symbolic.X14, Symbolic.X12, 0);
        ]
      else
        [
          Symbolic.SUB_imm (Symbolic.X14, Symbolic.X14, 1);
          Symbolic.STR (Symbolic.X14, Symbolic.X12, 0);
          Symbolic.CBNZ (Symbolic.X14, fieldDone);
        ]
        @ leakDec
    in
    let taggedGuard =
      match operation with
      | MemoryModel.DynamicIntBuffer ->
          [
            Symbolic.AND_imm (Symbolic.X13, Symbolic.X12, 1L);
            Symbolic.CBNZ (Symbolic.X13, fieldDone);
          ]
      | _ -> []
    in
    [
      Symbolic.LDR (Symbolic.X12, baseReg, int16 fieldOffset);
      Symbolic.CBZ (Symbolic.X12, fieldDone);
    ]
    @ taggedGuard
    @ [
        Symbolic.CMP_reg (Symbolic.X12, Symbolic.X27);
        Symbolic.B_cond_label (Symbolic.LT, fieldDone);
        Symbolic.CMP_reg (Symbolic.X12, Symbolic.X28);
        Symbolic.B_cond_label (Symbolic.GT, fieldDone);
        Symbolic.LDR (Symbolic.X14, Symbolic.X12, 0);
        Symbolic.MOVZ (Symbolic.X15, 0xFFFF, 0);
        Symbolic.MOVK (Symbolic.X15, 0xFFFF, 16);
        Symbolic.MOVK (Symbolic.X15, 0xFFFF, 32);
        Symbolic.MOVK (Symbolic.X15, 0x7FFF, 48);
        Symbolic.CMP_reg (Symbolic.X14, Symbolic.X15);
        Symbolic.B_cond_label (Symbolic.EQ, fieldDone);
      ]
    @ refcountUpdate
    @ [ Symbolic.Label fieldDone ]
  in
  let releasePlanManagedRootFieldFrom (baseReg : Symbolic.reg)
      (fieldOffset : int) (path : string) (helperLabel : string) :
      Symbolic.instr list =
    let fieldDone =
      label (Printf.sprintf "leaf_plan_root_%s_%d_done" path fieldOffset)
    in
    [
      Symbolic.LDR (Symbolic.X12, baseReg, int16 fieldOffset);
      Symbolic.CBZ (Symbolic.X12, fieldDone);
      Symbolic.STP_pre (Symbolic.X0, Symbolic.X1, Symbolic.SP, -112);
      Symbolic.STP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
      Symbolic.STP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
      Symbolic.STP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
      Symbolic.STP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
      Symbolic.STP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
      Symbolic.STR (Symbolic.X30, Symbolic.SP, 96);
      Symbolic.MOV_reg (Symbolic.X0, Symbolic.X12);
      Symbolic.BL helperLabel;
      Symbolic.LDR (Symbolic.X30, Symbolic.SP, 96);
      Symbolic.LDP (Symbolic.X10, Symbolic.X11, Symbolic.SP, 80);
      Symbolic.LDP (Symbolic.X8, Symbolic.X9, Symbolic.SP, 64);
      Symbolic.LDP (Symbolic.X6, Symbolic.X7, Symbolic.SP, 48);
      Symbolic.LDP (Symbolic.X4, Symbolic.X5, Symbolic.SP, 32);
      Symbolic.LDP (Symbolic.X2, Symbolic.X3, Symbolic.SP, 16);
      Symbolic.LDP_post (Symbolic.X0, Symbolic.X1, Symbolic.SP, 112);
      Symbolic.Label fieldDone;
    ]
  in
  let rec releasePlanFieldFrom (baseReg : Symbolic.reg) (fieldOffset : int)
      (path : string) (fieldReleasePlan : MemoryModel.rcReleasePlan) :
      Symbolic.instr list =
    match fieldReleasePlan with
    | MemoryModel.DynamicBufferRelease operation ->
        releasePlanDynamicBufferFieldFrom baseReg fieldOffset path operation
    | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
        releasePlanManagedRootFieldFrom baseReg fieldOffset path
          (leafListHelperLabelForReleasePlan fieldReleasePlan)
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
        releasePlanManagedRootFieldFrom baseReg fieldOffset path
          (leafDictHelperLabelForReleasePlan fieldReleasePlan)
    | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
        releasePlanManagedRootFieldFrom baseReg fieldOffset path
          closureRefCountDecHelperLabel
    | MemoryModel.RootRelease
        ( payloadSize,
          MemoryModel.GenericHeap,
          MemoryModel.FixedBlockPayloadRelease _ )
    | MemoryModel.RootRelease
        ( payloadSize,
          MemoryModel.GenericHeap,
          MemoryModel.BoxedSumPayloadRelease _ ) ->
        releasePlanGenericFieldFrom baseReg fieldOffset payloadSize path
          fieldReleasePlan
    | MemoryModel.RecursiveRelease sourceType ->
        releasePlanManagedRootFieldFrom baseReg fieldOffset path
          (recursiveNominalRefCountDecHelperLabel sourceType)
    | _ -> []
  and releasePlanBoxedSumVariantFieldsFrom (baseReg : Symbolic.reg)
      (path : string) (variants : MemoryModel.rcBoxedSumVariantRelease list) :
      Symbolic.instr list =
    let releaseVariant (variant : MemoryModel.rcBoxedSumVariantRelease) :
        (int * Symbolic.instr list) option =
      let releaseInstrs =
        variant.MemoryModel.fieldReleases
        |> List.concat_map
             (fun (MemoryModel.FieldRelease (fieldOffset, fieldReleasePlan)) ->
               releasePlanFieldFrom baseReg fieldOffset
                 (Printf.sprintf "%s_tag_%d" path variant.MemoryModel.tag)
                 fieldReleasePlan)
      in
      if isEmpty releaseInstrs then None
      else Some (variant.MemoryModel.tag, releaseInstrs)
    in
    let cases = variants |> List.filter_map releaseVariant in
    if isEmpty cases then []
    else
      let sumDone = label (Printf.sprintf "leaf_plan_sum_%s_done" path) in
      [ Symbolic.LDR (Symbolic.X10, baseReg, 0) ]
      @ (cases
        |> List.mapi (fun index (tag, releaseInstrs) ->
            let nextCase =
              label
                (Printf.sprintf "leaf_plan_sum_%s_variant_%d_next" path index)
            in
            [
              Symbolic.CMP_imm (Symbolic.X10, uint16 tag);
              Symbolic.B_cond_label (Symbolic.NE, nextCase);
            ]
            @ releaseInstrs
            @ [ Symbolic.B_label sumDone; Symbolic.Label nextCase ])
        |> List.concat)
      @ [ Symbolic.Label sumDone ]
  and releasePlanGenericFieldFrom (baseReg : Symbolic.reg) (fieldOffset : int)
      (payloadSize : int) (path : string)
      (fieldReleasePlan : MemoryModel.rcReleasePlan) : Symbolic.instr list =
    let fieldDone =
      label (Printf.sprintf "leaf_plan_generic_%s_%d_done" path fieldOffset)
    in
    let childFieldReleases =
      match fieldReleasePlan with
      | MemoryModel.RootRelease
          ( _,
            MemoryModel.GenericHeap,
            MemoryModel.FixedBlockPayloadRelease (_, fieldReleases) ) ->
          fieldReleases
          |> List.concat_map
               (fun
                 (MemoryModel.FieldRelease (childOffset, childReleasePlan)) ->
                 releasePlanFieldFrom Symbolic.X11 childOffset
                   (Printf.sprintf "%s_%d" path fieldOffset)
                   childReleasePlan)
      | MemoryModel.RootRelease
          ( _,
            MemoryModel.GenericHeap,
            MemoryModel.BoxedSumPayloadRelease (_, _, variants) ) ->
          releasePlanBoxedSumVariantFieldsFrom Symbolic.X11
            (Printf.sprintf "%s_%d" path fieldOffset)
            variants
      | _ -> []
    in
    [
      Symbolic.LDR (Symbolic.X12, baseReg, int16 fieldOffset);
      Symbolic.CBZ (Symbolic.X12, fieldDone);
      Symbolic.LDR (Symbolic.X15, Symbolic.X12, int16 payloadSize);
      Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
      Symbolic.STR (Symbolic.X15, Symbolic.X12, int16 payloadSize);
      Symbolic.CBNZ (Symbolic.X15, fieldDone);
      Symbolic.MOV_reg (Symbolic.X11, Symbolic.X12);
      Symbolic.STP_pre (Symbolic.X12, Symbolic.X30, Symbolic.SP, -16);
    ]
    @ childFieldReleases
    @ [ Symbolic.LDR (Symbolic.X12, Symbolic.SP, 0) ]
    @ (if payloadSize >= 0 && payloadSize < 256 then
         [
           Symbolic.ADD_imm (Symbolic.X13, Symbolic.X27, uint16 payloadSize);
           Symbolic.LDR (Symbolic.X14, Symbolic.X13, 0);
           Symbolic.STR (Symbolic.X14, Symbolic.X12, 0);
           Symbolic.STR (Symbolic.X12, Symbolic.X13, 0);
         ]
       else [])
    @ leakDec
    @ [
        Symbolic.LDP_post (Symbolic.X12, Symbolic.X30, Symbolic.SP, 16);
        Symbolic.Label fieldDone;
      ]
  in
  let releaseManagedLeafFieldsFromPlan (releasePlan : MemoryModel.rcReleasePlan)
      : Symbolic.instr list =
    match releasePlan with
    | MemoryModel.RootRelease
        ( _,
          MemoryModel.GenericHeap,
          MemoryModel.FixedBlockPayloadRelease (_, fieldReleases) ) ->
        fieldReleases
        |> List.concat_map
             (fun (MemoryModel.FieldRelease (fieldOffset, fieldReleasePlan)) ->
               releasePlanFieldFrom Symbolic.X8 fieldOffset "root"
                 fieldReleasePlan)
    | MemoryModel.RootRelease
        ( _,
          MemoryModel.GenericHeap,
          MemoryModel.BoxedSumPayloadRelease (_, _, variants) ) ->
        releasePlanBoxedSumVariantFieldsFrom Symbolic.X8 "root" variants
    | _ -> []
  in
  let managedLeafFieldReleasePlan (fieldType : AST.semanticType) :
      MemoryModel.rcReleasePlan option =
    match fieldType with
    | AST.TRecord (name, _)
      when not (StringOrder.Map.mem name ctx.recordRegistry) ->
        None
    | _ ->
        Some
          (MemoryPlanning.rcReleasePlanOfTypeWithSums ctx.recordRegistry
             ctx.sumShapeRegistry fieldType)
  in
  let releaseManagedLeafFields =
    managedLeafFieldTypes
    |> List.mapi (fun index fieldType ->
        let fieldOffset = mul index 8 in
        match managedLeafFieldReleasePlan fieldType with
        | Some fieldReleasePlan ->
            releasePlanFieldFrom Symbolic.X8 fieldOffset
              (Printf.sprintf "legacy_%d" index)
              fieldReleasePlan
        | None -> [])
    |> List.concat
  in
  let releaseLeafFieldPayloads =
    match leafGenericReleasePlan with
    | Some releasePlan -> releaseManagedLeafFieldsFromPlan releasePlan
    | None -> releaseManagedLeafFields
  in
  let releaseLeafPayload =
    match
      ( leafGenericPayloadSize,
        releaseLeafDynamicBufferPayload,
        releaseLeafListPayload,
        releaseLeafDictPayload,
        releaseLeafClosurePayload )
    with
    | None, false, false, false, false -> (
        match leafGenericReleasePlan with
        | Some (MemoryModel.RecursiveRelease sourceType) ->
            releaseRecursivePayload sourceType
        | Some
            (MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) as
             listReleasePlan) ->
            releasePlanManagedRootFieldFrom Symbolic.X3 0 "nested_list"
              (leafListHelperLabelForReleasePlan listReleasePlan)
        | Some
            (MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) as
             dictReleasePlan) ->
            releaseLeafDictPayloadWithHelper
              (leafDictHelperLabelForReleasePlan dictReleasePlan)
        | _ -> [])
    | None, true, _, _, _ -> releaseDynamicBufferPayload
    | None, false, true, _, _ ->
        [ Symbolic.LDR (Symbolic.X8, Symbolic.X3, 0) ]
        @ addChild "leaf_payload_list"
    | None, false, false, true, _ ->
        let helperLabel =
          if helperLabel = listRefCountDecDictListHelperLabel then
            dictRefCountDecListValueHelperLabel
          else dictRefCountDecHelperLabel
        in
        releaseLeafDictPayloadWithHelper helperLabel
    | None, false, false, false, true -> releaseClosurePayload
    | Some payloadSize, _, _, _, _ ->
        [
          Symbolic.LDR (Symbolic.X8, Symbolic.X3, 0);
          Symbolic.CBZ (Symbolic.X8, leafPayloadDone);
          Symbolic.AND_imm (Symbolic.X9, Symbolic.X8, 7L);
          Symbolic.CBNZ (Symbolic.X9, leafPayloadDone);
          Symbolic.CMP_reg (Symbolic.X8, Symbolic.X27);
          Symbolic.B_cond_label (Symbolic.LT, leafPayloadDone);
          Symbolic.CMP_reg (Symbolic.X8, Symbolic.X28);
          Symbolic.B_cond_label (Symbolic.GT, leafPayloadDone);
          Symbolic.LDR (Symbolic.X9, Symbolic.X8, int16 payloadSize);
          Symbolic.SUB_imm (Symbolic.X9, Symbolic.X9, 1);
          Symbolic.STR (Symbolic.X9, Symbolic.X8, int16 payloadSize);
          Symbolic.CBNZ (Symbolic.X9, leafPayloadDone);
        ]
        @ releaseLeafFieldPayloads
        @ (if isEmpty releaseLeafFieldPayloads then []
           else [ Symbolic.LDR (Symbolic.X8, Symbolic.X3, 0) ])
        @ (if payloadSize >= 0 && payloadSize < 256 then
             [
               Symbolic.LDR (Symbolic.X10, Symbolic.X27, int16 payloadSize);
               Symbolic.STR (Symbolic.X10, Symbolic.X8, 0);
               Symbolic.STR (Symbolic.X8, Symbolic.X27, int16 payloadSize);
             ]
           else [])
        @ leakDec
        @ [ Symbolic.Label leafPayloadDone ]
  in
  let preserveForPayloadRelease instructions =
    if isEmpty releaseLeafPayload then [] else instructions
  in
  [ Symbolic.Label helperLabel ]
  @ preserveForPayloadRelease
      [
        Symbolic.STP_pre (Symbolic.X19, Symbolic.X20, Symbolic.SP, -32);
        Symbolic.STR (Symbolic.X21, Symbolic.SP, 16);
      ]
  @ [
      Symbolic.MOVZ (Symbolic.X1, 0, 0);
      Symbolic.B_label loopCheck;
      Symbolic.Label loopCheck;
      Symbolic.CBZ (Symbolic.X0, popOrRet);
      Symbolic.AND_imm (Symbolic.X2, Symbolic.X0, 7L);
      Symbolic.CBZ (Symbolic.X2, popOrRet);
      Symbolic.AND_imm (Symbolic.X3, Symbolic.X0, 0xFFFFFFFFFFFFFFF8L);
      Symbolic.CMP_reg (Symbolic.X3, Symbolic.X27);
      Symbolic.B_cond_label (Symbolic.LT, popOrRet);
      Symbolic.CMP_reg (Symbolic.X3, Symbolic.X28);
      Symbolic.B_cond_label (Symbolic.GT, popOrRet);
      Symbolic.CMP_imm (Symbolic.X2, 1);
      Symbolic.B_cond_label (Symbolic.EQ, size32);
      Symbolic.CMP_imm (Symbolic.X2, 2);
      Symbolic.B_cond_label (Symbolic.EQ, size8);
      Symbolic.CMP_imm (Symbolic.X2, 3);
      Symbolic.B_cond_label (Symbolic.EQ, size24);
      Symbolic.B_label popOrRet;
      Symbolic.Label size8;
      Symbolic.MOVZ (Symbolic.X4, 8, 0);
      Symbolic.B_label haveSize;
      Symbolic.Label size24;
      Symbolic.MOVZ (Symbolic.X4, 24, 0);
      Symbolic.B_label haveSize;
      Symbolic.Label size32;
      Symbolic.MOVZ (Symbolic.X4, 32, 0);
      Symbolic.B_label haveSize;
      Symbolic.Label haveSize;
      Symbolic.ADD_reg (Symbolic.X5, Symbolic.X3, Symbolic.X4);
      Symbolic.LDR (Symbolic.X6, Symbolic.X5, 0);
      Symbolic.SUB_imm (Symbolic.X6, Symbolic.X6, 1);
      Symbolic.STR (Symbolic.X6, Symbolic.X5, 0);
      Symbolic.CBNZ (Symbolic.X6, popOrRet);
      Symbolic.CMP_imm (Symbolic.X2, 1);
      Symbolic.B_cond_label (Symbolic.EQ, collectSingle);
      Symbolic.CMP_imm (Symbolic.X2, 2);
      Symbolic.B_cond_label (Symbolic.EQ, collectLeaf);
      Symbolic.CMP_imm (Symbolic.X2, 3);
      Symbolic.B_cond_label (Symbolic.EQ, collectNode2);
      Symbolic.B_label freeNode;
      Symbolic.Label collectSingle;
      Symbolic.MOVZ (Symbolic.X0, 0, 0);
      Symbolic.LDR (Symbolic.X8, Symbolic.X3, 16);
    ]
  @ addChild "digit_tree"
  @ [ Symbolic.LDR (Symbolic.X8, Symbolic.X3, 24) ]
  @ addChild "digit_rest"
  @ [
      Symbolic.B_label freeNode;
      Symbolic.Label collectNode2;
      Symbolic.MOVZ (Symbolic.X0, 0, 0);
      Symbolic.B_label releaseValue;
      Symbolic.Label collectNode3;
      Symbolic.MOVZ (Symbolic.X0, 0, 0);
      Symbolic.LDR (Symbolic.X8, Symbolic.X3, 0);
    ]
  @ addChild "node3_0"
  @ [ Symbolic.LDR (Symbolic.X8, Symbolic.X3, 8) ]
  @ addChild "node3_1"
  @ [ Symbolic.LDR (Symbolic.X8, Symbolic.X3, 16) ]
  @ addChild "node3_2"
  @ [
      Symbolic.B_label freeNode;
      Symbolic.Label collectDeep;
      Symbolic.MOVZ (Symbolic.X0, 0, 0);
      Symbolic.LDR (Symbolic.X7, Symbolic.X3, 8);
      Symbolic.CMP_imm (Symbolic.X7, 0);
      Symbolic.B_cond_label (Symbolic.LE, afterPrefix);
      Symbolic.LDR (Symbolic.X8, Symbolic.X3, 16);
    ]
  @ addChild "deep_p0"
  @ [
      Symbolic.CMP_imm (Symbolic.X7, 1);
      Symbolic.B_cond_label (Symbolic.LE, afterPrefix);
      Symbolic.LDR (Symbolic.X8, Symbolic.X3, 24);
    ]
  @ addChild "deep_p1"
  @ [
      Symbolic.CMP_imm (Symbolic.X7, 2);
      Symbolic.B_cond_label (Symbolic.LE, afterPrefix);
      Symbolic.LDR (Symbolic.X8, Symbolic.X3, 32);
    ]
  @ addChild "deep_p2"
  @ [
      Symbolic.CMP_imm (Symbolic.X7, 3);
      Symbolic.B_cond_label (Symbolic.LE, afterPrefix);
      Symbolic.LDR (Symbolic.X8, Symbolic.X3, 40);
    ]
  @ addChild "deep_p3"
  @ [ Symbolic.Label afterPrefix; Symbolic.LDR (Symbolic.X8, Symbolic.X3, 48) ]
  @ addChild "deep_middle"
  @ [
      Symbolic.LDR (Symbolic.X7, Symbolic.X3, 56);
      Symbolic.CMP_imm (Symbolic.X7, 0);
      Symbolic.B_cond_label (Symbolic.LE, afterSuffix);
      Symbolic.LDR (Symbolic.X8, Symbolic.X3, 64);
    ]
  @ addChild "deep_s0"
  @ [
      Symbolic.CMP_imm (Symbolic.X7, 1);
      Symbolic.B_cond_label (Symbolic.LE, afterSuffix);
      Symbolic.LDR (Symbolic.X8, Symbolic.X3, 72);
    ]
  @ addChild "deep_s1"
  @ [
      Symbolic.CMP_imm (Symbolic.X7, 2);
      Symbolic.B_cond_label (Symbolic.LE, afterSuffix);
      Symbolic.LDR (Symbolic.X8, Symbolic.X3, 80);
    ]
  @ addChild "deep_s2"
  @ [
      Symbolic.CMP_imm (Symbolic.X7, 3);
      Symbolic.B_cond_label (Symbolic.LE, afterSuffix);
      Symbolic.LDR (Symbolic.X8, Symbolic.X3, 88);
    ]
  @ addChild "deep_s3"
  @ [
      Symbolic.Label afterSuffix;
      Symbolic.B_label freeNode;
      Symbolic.Label collectLeaf;
      Symbolic.MOVZ (Symbolic.X0, 0, 0);
      Symbolic.Label releaseValue;
    ]
  @ preserveForPayloadRelease
      [
        Symbolic.MOV_reg (Symbolic.X19, Symbolic.X2);
        Symbolic.MOV_reg (Symbolic.X20, Symbolic.X3);
        Symbolic.MOV_reg (Symbolic.X21, Symbolic.X4);
      ]
  @ releaseLeafPayload
  @ preserveForPayloadRelease
      [
        Symbolic.MOV_reg (Symbolic.X2, Symbolic.X19);
        Symbolic.MOV_reg (Symbolic.X3, Symbolic.X20);
        Symbolic.MOV_reg (Symbolic.X4, Symbolic.X21);
      ]
  @ [
      Symbolic.CMP_imm (Symbolic.X2, 3);
      Symbolic.B_cond_label (Symbolic.NE, freeNode);
      Symbolic.Label collectNodeChildren;
      Symbolic.LDR (Symbolic.X8, Symbolic.X3, 8);
    ]
  @ addChild "node_left"
  @ [ Symbolic.LDR (Symbolic.X8, Symbolic.X3, 16) ]
  @ addChild "node_right"
  @ [
      Symbolic.Label freeNode;
      Symbolic.ADD_reg (Symbolic.X5, Symbolic.X27, Symbolic.X4);
      Symbolic.LDR (Symbolic.X6, Symbolic.X5, 0);
      Symbolic.STR (Symbolic.X6, Symbolic.X3, 0);
      Symbolic.STR (Symbolic.X3, Symbolic.X5, 0);
    ]
  @ leakDec
  @ [
      Symbolic.B_label loopCheck;
      Symbolic.Label popOrRet;
      Symbolic.CBZ (Symbolic.X1, helperRet);
      Symbolic.LDR (Symbolic.X0, Symbolic.SP, 0);
      Symbolic.ADD_imm (Symbolic.SP, Symbolic.SP, 16);
      Symbolic.SUB_imm (Symbolic.X1, Symbolic.X1, 1);
      Symbolic.B_label loopCheck;
      Symbolic.Label helperRet;
    ]
  @ preserveForPayloadRelease
      [
        Symbolic.LDR (Symbolic.X21, Symbolic.SP, 16);
        Symbolic.LDP_post (Symbolic.X19, Symbolic.X20, Symbolic.SP, 32);
      ]
  @ [ Symbolic.RET ]

type listRefCountDecHelperSpec = {
  label : string;
  releaseLeafListPayload : bool;
  releaseLeafDictPayload : bool;
  releaseLeafClosurePayload : bool;
}

let listRefCountDecHelperSpecs : listRefCountDecHelperSpec list =
  [
    {
      label = listRefCountDecHelperLabel;
      releaseLeafListPayload = false;
      releaseLeafDictPayload = false;
      releaseLeafClosurePayload = false;
    };
    {
      label = listRefCountDecListHelperLabel;
      releaseLeafListPayload = true;
      releaseLeafDictPayload = false;
      releaseLeafClosurePayload = false;
    };
    {
      label = listRefCountDecDictHelperLabel;
      releaseLeafListPayload = false;
      releaseLeafDictPayload = true;
      releaseLeafClosurePayload = false;
    };
    {
      label = listRefCountDecDictListHelperLabel;
      releaseLeafListPayload = false;
      releaseLeafDictPayload = true;
      releaseLeafClosurePayload = false;
    };
    {
      label = listRefCountDecClosureHelperLabel;
      releaseLeafListPayload = false;
      releaseLeafDictPayload = false;
      releaseLeafClosurePayload = true;
    };
  ]

let generateNeededListRefCountDecHelpers (ctx : codeGenContext)
    (neededHelperLabels : StringOrder.Set.t)
    (plannedListDecHelpers :
      (int * MemoryModel.rcReleasePlan) StringOrder.Map.t) : Symbolic.instr list
    =
  let staticHelpers =
    (listRefCountDecHelperSpecs
    |> List.concat_map (fun spec ->
        if StringOrder.Set.mem spec.label neededHelperLabels then
          generateListRefCountDecHelperWith spec.label ctx None None false
            spec.releaseLeafListPayload spec.releaseLeafDictPayload
            spec.releaseLeafClosurePayload []
        else []))
    @ (if
         StringOrder.Set.mem listRefCountDecStringHelperLabel neededHelperLabels
       then
         generateListRefCountDecHelperWith listRefCountDecStringHelperLabel ctx
           None None true false false false []
       else [])
    @
    if StringOrder.Set.mem listRefCountDecBlobHelperLabel neededHelperLabels
    then
      generateListRefCountDecHelperWith listRefCountDecBlobHelperLabel ctx None
        None true false false false []
    else []
  in
  let plannedHelpers =
    plannedListDecHelpers |> StringOrder.Map.bindings
    |> List.concat_map (fun (helperLabel, (payloadSize, releasePlan)) ->
        if StringOrder.Set.mem helperLabel neededHelperLabels then
          let plannedPayloadSize =
            match releasePlan with
            | MemoryModel.RecursiveRelease _
            | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
                None
            | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) -> None
            | _ -> Some payloadSize
          in
          generateListRefCountDecHelperWith helperLabel ctx plannedPayloadSize
            (Some releasePlan) false false false false []
        else [])
  in
  staticHelpers @ plannedHelpers
