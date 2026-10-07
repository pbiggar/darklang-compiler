(*
   ClosureReferenceCounts.fs - Generate closure and recursive-payload lifetime helpers.
*)
[@@@warning "-4"]
let isEmpty xs=xs=[]
let add a b=Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let mul a b=Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))
let int16 value=let low=value land 65535 in if low>=32768 then low-65536 else low
let uint16 value=value land 65535
open! MemoryModel
module F=StructuralFormat
let number n=F.Scalar (string_of_int n)
let kindValue kind=F.Union ((match kind with MemoryModel.GenericHeap->"GenericHeap"|StreamHeap->"StreamHeap"|TaggedList->"TaggedList"|DictHeap->"DictHeap"|ClosureHeap->"ClosureHeap"),[])
let operationValue = function
 | MemoryModel.FixedSizeRoot (size,kind)->F.Union ("FixedSizeRoot",[number size;kindValue kind])
 | DynamicStringBuffer->F.Union ("DynamicStringBuffer",[])
 | DynamicBlobBuffer->F.Union ("DynamicBlobBuffer",[])
 | DynamicIntBuffer->F.Union ("DynamicIntBuffer",[])
let rec planValue = function
 | MemoryModel.NoReleasePlan->F.Union ("NoReleasePlan",[])
 | DynamicBufferRelease operation->F.Union ("DynamicBufferRelease",[operationValue operation])
 | RecursiveRelease typ->F.Union ("RecursiveRelease",[F.semanticValue typ])
 | RootRelease (size,kind,payload)->F.Union ("RootRelease",[number size;kindValue kind;payloadValue payload])
and fieldValue (MemoryModel.FieldRelease (offset,plan))=F.Union ("FieldRelease",[number offset;planValue plan])
and fieldsValue fields=F.Sequence (List.map fieldValue fields)
and payloadValue = function
 | MemoryModel.NoPayloadRelease->F.Union ("NoPayloadRelease",[])
 | FixedBlockPayloadRelease (size,fields)->F.Union ("FixedBlockPayloadRelease",[number size;fieldsValue fields])
 | BoxedSumPayloadRelease (size,fields,variants)->F.Union ("BoxedSumPayloadRelease",[number size;fieldsValue fields;F.Sequence (List.map (fun (variant:MemoryModel.rcBoxedSumVariantRelease)->F.Record ["Tag",number variant.MemoryModel.tag;"FieldReleases",fieldsValue variant.MemoryModel.fieldReleases]) variants)])
 | TaggedListPayloadRelease plan->F.Union ("TaggedListPayloadRelease",[planValue plan])
 | DictPayloadRelease (key,value)->F.Union ("DictPayloadRelease",[planValue key;planValue value])
 | ClosurePayloadRelease fields->F.Union ("ClosurePayloadRelease",[fieldsValue fields])
let planText value=F.format (planValue value)
let payloadText value=F.format (payloadValue value)



open ARM64CodeGenTypes
open HeapAllocation

let generateClosurePayloadSizeResolver
    (ctx: codeGenContext)
    (label: string -> string)
    (readyLabel: string)
    : Symbolic.instr list =
    let cases =
        ctx.closurePayloadSizes
        |> StringOrder.Map.bindings
        |> List.filter (fun (_, payloadSize) -> payloadSize <> 8)
        |> List.mapi (fun index (funcName, payloadSize) ->
            let nextLabel = label (Printf.sprintf "payload_next_%d" (index))
            in
            [
                Symbolic.ADR (Symbolic.X11, codeLabel funcName);
                Symbolic.CMP_reg (Symbolic.X9, Symbolic.X11);
                Symbolic.B_cond_label (Symbolic.NE, nextLabel);
                Symbolic.MOVZ (Symbolic.X10, uint16 payloadSize, 0);
                Symbolic.B_label readyLabel;
                Symbolic.Label nextLabel
            ])
        |> List.concat

    in
    [
        Symbolic.LDR (Symbolic.X9, Symbolic.X0, 0);
        Symbolic.MOVZ (Symbolic.X10, 8, 0)
    ]
    @ cases

let generateClosureRefCountIncHelper (ctx: codeGenContext) : Symbolic.instr list =
    let label (name: string) : string = (Printf.sprintf "__dark_closure_rc_inc_%s" (name))
    in
    let ready = label "payload_ready"
    in
    let helperRet = label "ret"
    in
    [
        Symbolic.Label closureRefCountIncHelperLabel;
        Symbolic.CBZ (Symbolic.X0, helperRet)
    ]
    @ generateClosurePayloadSizeResolver ctx label ready
    @ [
        Symbolic.Label ready;
        Symbolic.ADD_reg (Symbolic.X12, Symbolic.X0, Symbolic.X10);
        Symbolic.LDR (Symbolic.X15, Symbolic.X12, 0);
        Symbolic.ADD_imm (Symbolic.X15, Symbolic.X15, 1);
        Symbolic.STR (Symbolic.X15, Symbolic.X12, 0);
        Symbolic.Label helperRet;
        Symbolic.RET
    ]

let tryRcReleasePlanOfType
    (recordRegistry: LIR.recordRegistry)
    (sumShapeRegistry: MemoryModel.rcSumShapeRegistry)
    (typ: AST.semanticType)
    : MemoryModel.rcReleasePlan option =
    (match typ with
    | AST.TRecord (name, _) when not (StringOrder.Map.mem name recordRegistry) ->
        None
    | _ ->
        Some (MemoryPlanning.rcReleasePlanOfTypeWithSums recordRegistry sumShapeRegistry typ)

    )
let rcMetadataReleasePlan (metadata: MemoryModel.rcMetadata option) : MemoryModel.rcReleasePlan option =
    Option.bind metadata (fun m -> m.MemoryModel.releasePlan)

let requiredRcMetadataReleasePlan (context: string) (metadata: MemoryModel.rcMetadata option) : MemoryModel.rcReleasePlan =
    (match rcMetadataReleasePlan metadata with
    | Some releasePlan -> releasePlan
    | None -> Crash.crash (Printf.sprintf "%s: missing RC release plan metadata" (context))

    )
let generateRecursiveNominalRefCountDecHelper
    (_dictHelperForReleasePlan: MemoryModel.rcReleasePlan -> string)
    (ctx: codeGenContext)
    (sourceType: AST.semanticType)
    : Symbolic.instr list =
    let helperLabel = recursiveNominalRefCountDecHelperLabel sourceType
    in
    let label suffix = (Printf.sprintf "%s_%s" (helperLabel) (suffix))
    in
    let releasePlan =
        MemoryPlanning.rcReleasePlanOfTypeWithSums ctx.recordRegistry ctx.sumShapeRegistry sourceType

    in
    let rec releaseFromX0 (path: string) (plan: MemoryModel.rcReleasePlan) : Symbolic.instr list =
        (match plan with
        | MemoryModel.NoReleasePlan ->
            []
        | MemoryModel.DynamicBufferRelease operation ->
            let doneLabel = label (Printf.sprintf "%s_dynamic_done" (path))
            in
            let leakRelease =
                if ctx.options.enableLeakCheck then
                    let labelRef = dataLabel leakCounterLabel
                    in
                    [
                        Symbolic.CBNZ (Symbolic.X1, doneLabel);
                        Symbolic.ADRP (Symbolic.X17, labelRef);
                        Symbolic.ADD_label (Symbolic.X17, Symbolic.X17, labelRef);
                        Symbolic.LDR (Symbolic.X16, Symbolic.X17, 0);
                        Symbolic.SUB_imm (Symbolic.X16, Symbolic.X16, 1);
                        Symbolic.STR (Symbolic.X16, Symbolic.X17, 0)
                    ]
                else
                    []
            in
            let taggedGuard =
                (match operation with
                | MemoryModel.DynamicIntBuffer ->
                    [ Symbolic.AND_imm (Symbolic.X3, Symbolic.X0, 1L);
                      Symbolic.CBNZ (Symbolic.X3, doneLabel) ]
                | _ -> []
                )
            in
            [
                Symbolic.CBZ (Symbolic.X0, doneLabel)
            ]
            @ taggedGuard
            @ [
                Symbolic.CMP_reg (Symbolic.X0, Symbolic.X27);
                Symbolic.B_cond_label (Symbolic.LT, doneLabel);
                Symbolic.CMP_reg (Symbolic.X0, Symbolic.X28);
                Symbolic.B_cond_label (Symbolic.GT, doneLabel);
                Symbolic.LDR (Symbolic.X1, Symbolic.X0, 0);
                Symbolic.MOVZ (Symbolic.X3, 0xFFFF, 0);
                Symbolic.MOVK (Symbolic.X3, 0xFFFF, 16);
                Symbolic.MOVK (Symbolic.X3, 0xFFFF, 32);
                Symbolic.MOVK (Symbolic.X3, 0x7FFF, 48);
                Symbolic.CMP_reg (Symbolic.X1, Symbolic.X3);
                Symbolic.B_cond_label (Symbolic.EQ, doneLabel);
                Symbolic.SUB_imm (Symbolic.X1, Symbolic.X1, 1);
                Symbolic.STR (Symbolic.X1, Symbolic.X0, 0)
            ]
            @ leakRelease
            @ [Symbolic.Label doneLabel]
        | MemoryModel.RecursiveRelease recursiveType ->
            [Symbolic.BL (recursiveNominalRefCountDecHelperLabel recursiveType)]
        | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
            [Symbolic.BL (plannedDictDecHelperLabelForReleasePlan plan)]
        | MemoryModel.RootRelease (_, MemoryModel.TaggedList, MemoryModel.TaggedListPayloadRelease elementRelease) ->
            let helper =
                (match elementRelease with
                | MemoryModel.NoReleasePlan -> listRefCountDecHelperLabel
                | MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer -> listRefCountDecStringHelperLabel
                | MemoryModel.DynamicBufferRelease MemoryModel.DynamicBlobBuffer -> listRefCountDecBlobHelperLabel
                | MemoryModel.DynamicBufferRelease MemoryModel.DynamicIntBuffer -> listRefCountDecBlobHelperLabel
                | MemoryModel.DynamicBufferRelease _ -> Crash.crash "closure list release used a fixed-size dynamic-buffer operation"
                | MemoryModel.RecursiveRelease recursiveType ->
                    plannedListDecHelperLabelForReleasePlan (MemoryModel.RecursiveRelease recursiveType)
                | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) -> listRefCountDecListHelperLabel
                | MemoryModel.RootRelease (
                      _,
                      MemoryModel.DictHeap,
                      MemoryModel.DictPayloadRelease (MemoryModel.DynamicBufferRelease _, _))
                | MemoryModel.RootRelease (
                      _,
                      MemoryModel.DictHeap,
                      MemoryModel.DictPayloadRelease (_, MemoryModel.DynamicBufferRelease _)) ->
                    plannedListDecHelperLabelForReleasePlan elementRelease
                | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) -> listRefCountDecDictHelperLabel
                | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) -> listRefCountDecClosureHelperLabel
                | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) -> plannedListDecHelperLabelForReleasePlan elementRelease
                | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, _) -> plannedListDecHelperLabelForReleasePlan elementRelease
                )
            in
            [Symbolic.BL helper]
        | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
            [Symbolic.BL closureRefCountDecHelperLabel]
        | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) ->
            [Symbolic.BL streamRefCountDecHelperLabel]
        | MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, payloadPlan) ->
            let doneLabel = label (Printf.sprintf "%s_done" (path))
            in
            let payloadReleases = releasePayload path payloadPlan
            in
            let freeRoot =
                if payloadSize >= 0 && payloadSize < 256 then
                    [
                        Symbolic.LDR (Symbolic.X0, Symbolic.SP, 0);
                        Symbolic.LDR (Symbolic.X2, Symbolic.X27, int16 payloadSize);
                        Symbolic.STR (Symbolic.X2, Symbolic.X0, 0);
                        Symbolic.STR (Symbolic.X0, Symbolic.X27, int16 payloadSize)
                    ]
                else
                    []
            in
            [
                Symbolic.CBZ (Symbolic.X0, doneLabel);
                Symbolic.LDR (Symbolic.X1, Symbolic.X0, int16 payloadSize);
                Symbolic.SUB_imm (Symbolic.X1, Symbolic.X1, 1);
                Symbolic.STR (Symbolic.X1, Symbolic.X0, int16 payloadSize);
                Symbolic.CBNZ (Symbolic.X1, doneLabel);
                Symbolic.STP_pre (Symbolic.X0, Symbolic.X30, Symbolic.SP, -16)
            ]
            @ payloadReleases
            @ freeRoot
            @ (if ctx.options.enableLeakCheck then
                let labelRef = dataLabel leakCounterLabel
                in
                [
                    Symbolic.ADRP (Symbolic.X17, labelRef);
                    Symbolic.ADD_label (Symbolic.X17, Symbolic.X17, labelRef);
                    Symbolic.LDR (Symbolic.X16, Symbolic.X17, 0);
                    Symbolic.SUB_imm (Symbolic.X16, Symbolic.X16, 1);
                    Symbolic.STR (Symbolic.X16, Symbolic.X17, 0)
                ]
               else
                [])
            @ [
                Symbolic.LDP_post (Symbolic.X0, Symbolic.X30, Symbolic.SP, 16);
                Symbolic.Label doneLabel
            ]
        | unsupported ->
            Crash.crash (Printf.sprintf "ARM64 recursive nominal RC helper does not support nested release plan %s" (planText unsupported))

        )
    and releaseField (path: string) (index: int) (MemoryModel.FieldRelease (offset, plan)) : Symbolic.instr list =
        [
            Symbolic.LDR (Symbolic.X0, Symbolic.SP, 0);
            Symbolic.LDR (Symbolic.X0, Symbolic.X0, int16 offset)
        ]
        @ releaseFromX0 (Printf.sprintf "%s_field_%d" (path) (index)) plan

    and releaseFields (path: string) (fields: MemoryModel.rcFieldRelease list) : Symbolic.instr list =
        fields
        |> List.mapi (releaseField path)
        |> List.concat

    and releasePayload (path: string) (payload: MemoryModel.rcPayloadReleasePlan) : Symbolic.instr list =
        (match payload with
        | MemoryModel.NoPayloadRelease ->
            []
        | MemoryModel.FixedBlockPayloadRelease (_, fields)
        | MemoryModel.ClosurePayloadRelease fields ->
            releaseFields path fields
        | MemoryModel.BoxedSumPayloadRelease (_, _, variants) ->
            let doneLabel = label (Printf.sprintf "%s_variant_done" (path))
            in
            let cases =
                variants
                |> List.filter (fun variant -> not (isEmpty variant.MemoryModel.fieldReleases))
                |> List.mapi (fun index (variant:MemoryModel.rcBoxedSumVariantRelease) ->
                    let nextLabel = label (Printf.sprintf "%s_variant_%d_next" (path) (index))
                    in
                    [
                        Symbolic.LDR (Symbolic.X0, Symbolic.SP, 0);
                        Symbolic.LDR (Symbolic.X1, Symbolic.X0, 0);
                        Symbolic.CMP_imm (Symbolic.X1, uint16 variant.MemoryModel.tag);
                        Symbolic.B_cond_label (Symbolic.NE, nextLabel)
                    ]
                    @ releaseFields (Printf.sprintf "%s_variant_%d" (path) (index)) variant.MemoryModel.fieldReleases
                    @ [
                        Symbolic.B_label doneLabel;
                        Symbolic.Label nextLabel
                    ])
                |> List.concat
            in
            cases @ [Symbolic.Label doneLabel]
        | unsupported ->
            Crash.crash (Printf.sprintf "ARM64 recursive nominal RC helper does not support payload release plan %s" (payloadText unsupported))

        )
    in
    (match releasePlan with
    | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, _) ->
        [Symbolic.Label helperLabel]
        @ releaseFromX0 "root" releasePlan
        @ [Symbolic.RET]
    | MemoryModel.RootRelease _ ->
        [Symbolic.Label helperLabel;
         Symbolic.STP_pre (Symbolic.X0, Symbolic.X30, Symbolic.SP, -16)]
        @ releaseFromX0 "root" releasePlan
        @ [Symbolic.LDP_post (Symbolic.X0, Symbolic.X30, Symbolic.SP, 16);
           Symbolic.RET]
    | _ ->
        Crash.crash (Printf.sprintf "ARM64 recursive nominal RC helper requires a managed root release plan, got %s" (planText releasePlan))

    )
let generateClosureRefCountDecHelper
    (dictHelperForReleasePlan: MemoryModel.rcReleasePlan -> string)
    (ctx: codeGenContext)
    : Symbolic.instr list =
    let label (name: string) : string = (Printf.sprintf "__dark_closure_rc_dec_%s" (name))
    in
    let ready = label "payload_ready"
    in
    let helperRet = label "ret"
    in
    let skipFreelist = label "skip_freelist"
    in
    let capturesReleased = label "captures_released"
    in
    let leakDec =
        if ctx.options.enableLeakCheck then
            let labelRef = dataLabel leakCounterLabel
            in
            [
                Symbolic.ADRP (Symbolic.X17, labelRef);
                Symbolic.ADD_label (Symbolic.X17, Symbolic.X17, labelRef);
                Symbolic.LDR (Symbolic.X16, Symbolic.X17, 0);
                Symbolic.SUB_imm (Symbolic.X16, Symbolic.X16, 1);
                Symbolic.STR (Symbolic.X16, Symbolic.X17, 0)
            ]
        else
            []

    in
    let rec releaseFixedChildField
        (baseReg: Symbolic.reg)
        (fieldOffset: int)
        (payloadSize: int)
        (fieldReleasePlan: MemoryModel.rcReleasePlan)
        (doneLabel: string)
        : Symbolic.instr list =
        let childDone = label (Printf.sprintf "%s_child_%d_done" (doneLabel) (fieldOffset))
        in
        let childSkipFreelist = label (Printf.sprintf "%s_child_%d_skip_freelist" (doneLabel) (fieldOffset))
        in
        let childFieldReleaseInstrs =
            (match fieldReleasePlan with
            | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases)) ->
                fieldReleases
                |> List.concat_map (fun (MemoryModel.FieldRelease (childFieldOffset, childFieldReleasePlan)) ->
                    releaseFieldPlanFrom Symbolic.X11 childFieldOffset childFieldReleasePlan childDone)
            | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease (_, _, variants)) ->
                releaseBoxedSumVariantFieldsFrom Symbolic.X11 variants childDone
            | _ ->
                []
            )
        in
        let releaseChildFields =
            if isEmpty childFieldReleaseInstrs then
                []
            else
                [
                    Symbolic.STP_pre (Symbolic.X11, Symbolic.X12, Symbolic.SP, -16);
                    Symbolic.MOV_reg (Symbolic.X11, Symbolic.X12)
                ]
                @ childFieldReleaseInstrs
                @ [Symbolic.LDP_post (Symbolic.X11, Symbolic.X12, Symbolic.SP, 16)]
        in
        [
            Symbolic.LDR (Symbolic.X12, baseReg, int16 fieldOffset);
            Symbolic.CBZ (Symbolic.X12, childDone);
            Symbolic.LDR (Symbolic.X15, Symbolic.X12, int16 payloadSize);
            Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
            Symbolic.STR (Symbolic.X15, Symbolic.X12, int16 payloadSize);
            Symbolic.CBNZ (Symbolic.X15, childDone)
        ]
        @ releaseChildFields
        @ (if payloadSize >= 0 && payloadSize < 256 then
            [
                Symbolic.ADD_imm (Symbolic.X13, Symbolic.X27, uint16 payloadSize);
                Symbolic.LDR (Symbolic.X14, Symbolic.X13, 0);
                Symbolic.STR (Symbolic.X14, Symbolic.X12, 0);
                Symbolic.STR (Symbolic.X12, Symbolic.X13, 0)
            ]
           else
            [Symbolic.B_label childSkipFreelist])
        @ [
            Symbolic.Label childSkipFreelist
        ]
        @ leakDec
        @ [Symbolic.Label childDone]

    and releaseDynamicBufferChildField
        (baseReg: Symbolic.reg)
        (fieldOffset: int)
        (operation: MemoryModel.rcOperation)
        (doneLabel: string)
        : Symbolic.instr list =
        let bufferDone = label (Printf.sprintf "%s_dynamic_buffer_%d_done" (doneLabel) (fieldOffset))
        in
        let refcountUpdate =
            if isEmpty leakDec then
                [
                    Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
                    Symbolic.STR (Symbolic.X15, Symbolic.X12, 0)
                ]
            else
                [
                    Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
                    Symbolic.STR (Symbolic.X15, Symbolic.X12, 0);
                    Symbolic.CBNZ (Symbolic.X15, bufferDone)
                ] @ leakDec
        in
        let taggedGuard =
            (match operation with
            | MemoryModel.DynamicIntBuffer ->
                [ Symbolic.AND_imm (Symbolic.X13, Symbolic.X12, 1L);
                  Symbolic.CBNZ (Symbolic.X13, bufferDone) ]
            | _ -> []
            )
        in
        [
            Symbolic.LDR (Symbolic.X12, baseReg, int16 fieldOffset);
            Symbolic.CBZ (Symbolic.X12, bufferDone)
        ]
        @ taggedGuard
        @ [
            Symbolic.LDR (Symbolic.X15, Symbolic.X12, 0);
            Symbolic.MOVZ (Symbolic.X13, 0xFFFF, 0);
            Symbolic.MOVK (Symbolic.X13, 0xFFFF, 16);
            Symbolic.MOVK (Symbolic.X13, 0xFFFF, 32);
            Symbolic.MOVK (Symbolic.X13, 0x7FFF, 48);
            Symbolic.CMP_reg (Symbolic.X15, Symbolic.X13);
            Symbolic.B_cond_label (Symbolic.EQ, bufferDone)
        ]
        @ refcountUpdate
        @ [Symbolic.Label bufferDone]

    and releaseManagedRootChildField
        (baseReg: Symbolic.reg)
        (fieldOffset: int)
        (helperLabel: string)
        (doneLabel: string)
        : Symbolic.instr list =
        let childDone = label (Printf.sprintf "%s_child_root_%d_done" (doneLabel) (fieldOffset))
        in
        [
            Symbolic.LDR (Symbolic.X12, baseReg, int16 fieldOffset);
            Symbolic.CBZ (Symbolic.X12, childDone);
            Symbolic.STP_pre (Symbolic.X0, Symbolic.X8, Symbolic.SP, -32);
            Symbolic.STR (Symbolic.X30, Symbolic.SP, 16);
            Symbolic.MOV_reg (Symbolic.X0, Symbolic.X12);
            Symbolic.BL helperLabel;
            Symbolic.LDR (Symbolic.X30, Symbolic.SP, 16);
            Symbolic.LDP_post (Symbolic.X0, Symbolic.X8, Symbolic.SP, 32);
            Symbolic.Label childDone
        ]

    and releaseFieldPlanFrom
        (baseReg: Symbolic.reg)
        (fieldOffset: int)
        (fieldReleasePlan: MemoryModel.rcReleasePlan)
        (doneLabel: string)
        : Symbolic.instr list =
        let dictHelperForChildPlan (fieldReleasePlan: MemoryModel.rcReleasePlan) : string =
            (match fieldReleasePlan with
            | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, MemoryModel.RootRelease (_, MemoryModel.TaggedList, _))) ->
                dictRefCountDecListValueHelperLabel
            | _ ->
                dictRefCountDecHelperLabel

            )
        in
        (match fieldReleasePlan with
        | MemoryModel.DynamicBufferRelease operation ->
            releaseDynamicBufferChildField baseReg fieldOffset operation doneLabel
        | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
            releaseManagedRootChildField baseReg fieldOffset listRefCountDecHelperLabel doneLabel
        | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
            releaseManagedRootChildField baseReg fieldOffset (dictHelperForChildPlan fieldReleasePlan) doneLabel
        | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) ->
            releaseManagedRootChildField baseReg fieldOffset closureRefCountDecHelperLabel doneLabel
        | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) ->
            releaseManagedRootChildField baseReg fieldOffset streamRefCountDecHelperLabel doneLabel
        | MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease _)
        | MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease _) ->
            releaseFixedChildField baseReg fieldOffset payloadSize fieldReleasePlan doneLabel
        | _ ->
            []

        )
    and releaseBoxedSumVariantFieldsFrom
        (baseReg: Symbolic.reg)
        (variants: MemoryModel.rcBoxedSumVariantRelease list)
        (doneLabel: string)
        : Symbolic.instr list =
        let releaseVariant (variant: MemoryModel.rcBoxedSumVariantRelease) : (int * Symbolic.instr list) option =
            let releaseInstrs =
                variant.MemoryModel.fieldReleases
                |> List.concat_map (fun (MemoryModel.FieldRelease (fieldOffset, fieldReleasePlan)) ->
                    releaseFieldPlanFrom baseReg fieldOffset fieldReleasePlan doneLabel)

            in
            if isEmpty releaseInstrs then
                None
            else
                Some (variant.MemoryModel.tag, releaseInstrs)

        in
        let cases = variants |> List.filter_map releaseVariant

        in
        if isEmpty cases then
            []
        else
            let sumDone = label (Printf.sprintf "%s_sum_done" (doneLabel))
            in
            [
                Symbolic.LDR (Symbolic.X10, baseReg, 0)
            ]
            @
            (cases
             |> List.mapi (fun index (tag, releaseInstrs) ->
                let nextCase = label (Printf.sprintf "%s_sum_variant_%d_next" (doneLabel) (index))
                in
                [
                    Symbolic.CMP_imm (Symbolic.X10, uint16 tag);
                    Symbolic.B_cond_label (Symbolic.NE, nextCase)
                ]
                @ releaseInstrs
                @ [
                    Symbolic.B_label sumDone;
                    Symbolic.Label nextCase
                ])
             |> List.concat)
            @ [Symbolic.Label sumDone]

    in
    let releaseDynamicCapture (skipTagged: bool) (fieldOffset: int) (doneLabel: string) : Symbolic.instr list =
        let bufferDone = label (Printf.sprintf "%s_dynamic_capture_%d_done" (doneLabel) (fieldOffset))
        in
        let refcountUpdate =
            if isEmpty leakDec then
                [
                    Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
                    Symbolic.STR (Symbolic.X15, Symbolic.X12, 0)
                ]
            else
                [
                    Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
                    Symbolic.STR (Symbolic.X15, Symbolic.X12, 0);
                    Symbolic.CBNZ (Symbolic.X15, bufferDone)
                ] @ leakDec
        in
        let taggedGuard =
            if skipTagged then
                [ Symbolic.AND_imm (Symbolic.X13, Symbolic.X12, 1L);
                  Symbolic.CBNZ (Symbolic.X13, bufferDone) ]
            else
                []
        in
        [
            Symbolic.LDR (Symbolic.X12, Symbolic.X0, int16 fieldOffset);
            Symbolic.CBZ (Symbolic.X12, bufferDone)
        ]
        @ taggedGuard
        @ [
            Symbolic.LDR (Symbolic.X15, Symbolic.X12, 0);
            Symbolic.MOVZ (Symbolic.X13, 0xFFFF, 0);
            Symbolic.MOVK (Symbolic.X13, 0xFFFF, 16);
            Symbolic.MOVK (Symbolic.X13, 0xFFFF, 32);
            Symbolic.MOVK (Symbolic.X13, 0x7FFF, 48);
            Symbolic.CMP_reg (Symbolic.X15, Symbolic.X13);
            Symbolic.B_cond_label (Symbolic.EQ, bufferDone)
        ]
        @ refcountUpdate
        @ [Symbolic.Label bufferDone]

    in
    let fixedBlockFieldReleases
        (releasePlan: MemoryModel.rcReleasePlan)
        (doneLabel: string)
        : Symbolic.instr list =
        (match releasePlan with
        | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases)) ->
            fieldReleases
            |> List.concat_map (fun (MemoryModel.FieldRelease (fieldOffset, fieldReleasePlan)) ->
                releaseFieldPlanFrom Symbolic.X8 fieldOffset fieldReleasePlan doneLabel)
        | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, MemoryModel.BoxedSumPayloadRelease (_, _, variants)) ->
            releaseBoxedSumVariantFieldsFrom Symbolic.X8 variants doneLabel
        | _ ->
            []

        )
    in
    let releaseFixedCapture (fieldOffset: int) (releasePlan: MemoryModel.rcReleasePlan) (payloadSize: int) (doneLabel: string) : Symbolic.instr list =
        let captureDone = label (Printf.sprintf "%s_field_%d_done" (doneLabel) (fieldOffset))
        in
        let captureSkipFreelist = label (Printf.sprintf "%s_field_%d_skip_freelist" (doneLabel) (fieldOffset))
        in
        [
            Symbolic.LDR (Symbolic.X8, Symbolic.X0, int16 fieldOffset);
            Symbolic.CBZ (Symbolic.X8, captureDone);
            Symbolic.LDR (Symbolic.X15, Symbolic.X8, int16 payloadSize);
            Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
            Symbolic.STR (Symbolic.X15, Symbolic.X8, int16 payloadSize);
            Symbolic.CBNZ (Symbolic.X15, captureDone)
        ]
        @ fixedBlockFieldReleases releasePlan doneLabel
        @ (if payloadSize >= 0 && payloadSize < 256 then
            [
                Symbolic.ADD_imm (Symbolic.X13, Symbolic.X27, uint16 payloadSize);
                Symbolic.LDR (Symbolic.X14, Symbolic.X13, 0);
                Symbolic.STR (Symbolic.X14, Symbolic.X8, 0);
                Symbolic.STR (Symbolic.X8, Symbolic.X13, 0)
            ]
           else
            [Symbolic.B_label captureSkipFreelist])
        @ [
            Symbolic.Label captureSkipFreelist
        ]
        @ leakDec
        @ [Symbolic.Label captureDone]

    in
    let releaseCaptureCases =
        ctx.closureCaptureTypes
        |> StringOrder.Map.bindings
        |> List.mapi (fun index (funcName, captureTypes) ->
            let nextCase = label (Printf.sprintf "captures_next_%d" (index))
            in
            let releaseLabel = label (Printf.sprintf "captures_release_%d" (index))
            in
            let releaseInstrs =
                captureTypes
                |> List.mapi (fun captureIndex captureType ->
                    let fieldOffset = mul (add captureIndex 1) 8
                    in
                    (match captureType with
                    | AST.TString
                    | AST.TChar
                    | AST.TBlob ->
                        releaseDynamicCapture false fieldOffset (Printf.sprintf "captures_%d_%d" (index) (captureIndex))
                    | AST.TInt ->
                        releaseDynamicCapture true fieldOffset (Printf.sprintf "captures_%d_%d" (index) (captureIndex))
                    | _ ->
                        (match tryRcReleasePlanOfType ctx.recordRegistry ctx.sumShapeRegistry captureType with
                        | Some (MemoryModel.RootRelease (_, MemoryModel.TaggedList, MemoryModel.TaggedListPayloadRelease elementRelease)) ->
                            let helper =
                                (match elementRelease with
                                | MemoryModel.NoReleasePlan -> listRefCountDecHelperLabel
                                | MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer -> listRefCountDecStringHelperLabel
                                | MemoryModel.DynamicBufferRelease MemoryModel.DynamicBlobBuffer -> listRefCountDecBlobHelperLabel
                                | MemoryModel.DynamicBufferRelease MemoryModel.DynamicIntBuffer -> listRefCountDecBlobHelperLabel
                                | MemoryModel.DynamicBufferRelease _ -> Crash.crash "closure list release used a fixed-size dynamic-buffer operation"
                                | MemoryModel.RecursiveRelease sourceType ->
                                    plannedListDecHelperLabelForReleasePlan (MemoryModel.RecursiveRelease sourceType)
                                | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) -> listRefCountDecListHelperLabel
                                | MemoryModel.RootRelease (
                                      _,
                                      MemoryModel.DictHeap,
                                      MemoryModel.DictPayloadRelease (MemoryModel.DynamicBufferRelease _, _))
                                | MemoryModel.RootRelease (
                                      _,
                                      MemoryModel.DictHeap,
                                      MemoryModel.DictPayloadRelease (_, MemoryModel.DynamicBufferRelease _)) ->
                                    plannedListDecHelperLabelForReleasePlan elementRelease
                                | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (_, MemoryModel.RootRelease (_, MemoryModel.TaggedList, _))) ->
                                    listRefCountDecDictListHelperLabel
                                | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) -> listRefCountDecDictHelperLabel
                                | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) -> listRefCountDecClosureHelperLabel
                                | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) ->
                                    plannedListDecHelperLabelForReleasePlan elementRelease
                                | MemoryModel.RootRelease (_, MemoryModel.GenericHeap, _) ->
                                    plannedListDecHelperLabelForReleasePlan elementRelease
                                )
                            in
                            releaseManagedRootChildField Symbolic.X0 fieldOffset helper (Printf.sprintf "captures_%d_%d" (index) (captureIndex))
                        | Some (MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) as releasePlan) ->
                            releaseManagedRootChildField
                                Symbolic.X0
                                fieldOffset
                                (dictHelperForReleasePlan releasePlan)
                                (Printf.sprintf "captures_%d_%d" (index) (captureIndex))
                        | Some (MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _)) ->
                            releaseManagedRootChildField Symbolic.X0 fieldOffset closureRefCountDecHelperLabel (Printf.sprintf "captures_%d_%d" (index) (captureIndex))
                        | Some (MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _)) ->
                            releaseManagedRootChildField Symbolic.X0 fieldOffset streamRefCountDecHelperLabel (Printf.sprintf "captures_%d_%d" (index) (captureIndex))
                        | Some (MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, (MemoryModel.FixedBlockPayloadRelease _ | MemoryModel.BoxedSumPayloadRelease _)) as releasePlan) ->
                            releaseFixedCapture fieldOffset releasePlan payloadSize (Printf.sprintf "captures_%d_%d" (index) (captureIndex))
                        | _ ->
                            []
                        )
                    ))
                |> List.concat
            in
            [
                Symbolic.ADR (Symbolic.X11, codeLabel funcName);
                Symbolic.CMP_reg (Symbolic.X9, Symbolic.X11);
                Symbolic.B_cond_label (Symbolic.NE, nextCase);
                Symbolic.Label releaseLabel
            ]
            @ releaseInstrs
            @ [
                Symbolic.B_label capturesReleased;
                Symbolic.Label nextCase
            ])
        |> List.concat

    in
    [
        Symbolic.Label closureRefCountDecHelperLabel;
        Symbolic.CBZ (Symbolic.X0, helperRet)
    ]
    @ generateClosurePayloadSizeResolver ctx label ready
    @ [
        Symbolic.Label ready;
        Symbolic.ADD_reg (Symbolic.X12, Symbolic.X0, Symbolic.X10);
        Symbolic.LDR (Symbolic.X15, Symbolic.X12, 0);
        Symbolic.SUB_imm (Symbolic.X15, Symbolic.X15, 1);
        Symbolic.STR (Symbolic.X15, Symbolic.X12, 0);
        Symbolic.CBNZ (Symbolic.X15, helperRet)
    ]
    @ releaseCaptureCases
    @ [
        Symbolic.Label capturesReleased;
        Symbolic.CMP_imm (Symbolic.X10, 256);
        Symbolic.B_cond_label (Symbolic.GE, skipFreelist);
        Symbolic.ADD_reg (Symbolic.X13, Symbolic.X27, Symbolic.X10);
        Symbolic.LDR (Symbolic.X14, Symbolic.X13, 0);
        Symbolic.STR (Symbolic.X14, Symbolic.X0, 0);
        Symbolic.STR (Symbolic.X0, Symbolic.X13, 0);
        Symbolic.Label skipFreelist
    ]
    @ leakDec
    @ [
        Symbolic.Label helperRet;
        Symbolic.RET
    ]

let generateStreamRefCountDecHelper (ctx: codeGenContext) : Symbolic.instr list =
    let helperRet = (Printf.sprintf "%s_ret" (streamRefCountDecHelperLabel))
    in
    let alreadyClosed = (Printf.sprintf "%s_closed" (streamRefCountDecHelperLabel))
    in
    let leakDec =
        if ctx.options.enableLeakCheck then
            let labelRef = dataLabel leakCounterLabel
            in
            [
                Symbolic.ADRP (Symbolic.X17, labelRef);
                Symbolic.ADD_label (Symbolic.X17, Symbolic.X17, labelRef);
                Symbolic.LDR (Symbolic.X16, Symbolic.X17, 0);
                Symbolic.SUB_imm (Symbolic.X16, Symbolic.X16, 1);
                Symbolic.STR (Symbolic.X16, Symbolic.X17, 0)
            ]
        else
            []
    in
    [
        Symbolic.Label streamRefCountDecHelperLabel;
        Symbolic.CBZ (Symbolic.X0, helperRet);
        Symbolic.LDR (Symbolic.X1, Symbolic.X0, 24);
        Symbolic.SUB_imm (Symbolic.X1, Symbolic.X1, 1);
        Symbolic.STR (Symbolic.X1, Symbolic.X0, 24);
        Symbolic.CBNZ (Symbolic.X1, helperRet);
        Symbolic.STP_pre (Symbolic.X19, Symbolic.X30, Symbolic.SP, -16);
        Symbolic.MOV_reg (Symbolic.X19, Symbolic.X0);
        Symbolic.LDR (Symbolic.X2, Symbolic.X19, 0);
        Symbolic.CMP_imm (Symbolic.X2, 5);
        Symbolic.B_cond_label (Symbolic.EQ, alreadyClosed);
        Symbolic.MOVZ (Symbolic.X2, 5, 0);
        Symbolic.STR (Symbolic.X2, Symbolic.X19, 0);
        Symbolic.LDR (Symbolic.X0, Symbolic.X19, 16);
        Symbolic.MOVZ (Symbolic.X1, 0, 0);
        Symbolic.LDR (Symbolic.X9, Symbolic.X0, 0);
        Symbolic.BLR Symbolic.X9;
        Symbolic.Label alreadyClosed;
        Symbolic.LDR (Symbolic.X0, Symbolic.X19, 8);
        Symbolic.BL closureRefCountDecHelperLabel;
        Symbolic.LDR (Symbolic.X0, Symbolic.X19, 16);
        Symbolic.BL closureRefCountDecHelperLabel;
        Symbolic.LDR (Symbolic.X14, Symbolic.X27, 24);
        Symbolic.STR (Symbolic.X14, Symbolic.X19, 0);
        Symbolic.STR (Symbolic.X19, Symbolic.X27, 24)
    ]
    @ leakDec
    @ [
        Symbolic.LDP_post (Symbolic.X19, Symbolic.X30, Symbolic.SP, 16);
        Symbolic.Label helperRet;
        Symbolic.RET
    ]
