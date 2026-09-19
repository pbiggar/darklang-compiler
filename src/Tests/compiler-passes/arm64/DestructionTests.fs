// DestructionTests.fs - Verify recursive payload destruction and helper register preservation.

module ARM64DestructionTests

open ARM64CodeGenFixtures
open ARM64ReleasePlanningTests
open ARM64ControlFlowTests

let testDictListValuePlannedHelperReleasesCollisionPayloads () : TestResult =
    let dictType = AST.TDict (AST.TInt64, AST.TList AST.TInt64)
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
        let hasCollisionRootPayloadLoop =
            instrs
            |> List.exists (function
                | ARM64Symbolic.Label label
                    when label.Contains("collision_root_payload_loop") ->
                    true
                | _ ->
                    false)

        if hasCollisionRootPayloadLoop then
            Ok ()
        else
            Error "Dict<int, list<int>> planned helper did not emit a collision root payload release loop"

let testDictTupleValuePlannedHelperReleasesCollisionPayloads () : TestResult =
    let dictType = AST.TDict (AST.TInt64, AST.TTuple [ AST.TString; AST.TList AST.TInt64 ])
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
        let hasCollisionGenericPayloadLoop =
            instrs
            |> List.exists (function
                | ARM64Symbolic.Label label
                    when label.Contains("collision_generic_payload_loop") ->
                    true
                | _ ->
                    false)

        if hasCollisionGenericPayloadLoop then
            Ok ()
        else
            Error "Dict<int, tuple<string, list<int>>> planned helper did not emit a collision generic payload release loop"

let testDictStringKeyTupleValuePlannedHelperReleasesCollisionPayloads () : TestResult =
    let dictType = AST.TDict (AST.TString, AST.TTuple [ AST.TString; AST.TList AST.TInt64 ])
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
        let hasCollisionGenericPayloadLoop =
            instrs
            |> List.exists (function
                | ARM64Symbolic.Label label
                    when label.Contains("collision_generic_payload_loop") ->
                    true
                | _ ->
                    false)

        if hasCollisionGenericPayloadLoop then
            Ok ()
        else
            Error "Dict<string, tuple<string, list<int>>> planned helper did not emit a collision generic payload release loop"

let testGenericFixedBlockNestedBytesFieldUsesReleasePlan () : TestResult =
    let nestedType = AST.TTuple [ AST.TBlob ]
    let parentType = AST.TTuple [ nestedType ]
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    8,
                    LIR.GenericHeap,
                    Some (rcMetadata parentType))
            ]
            Map.empty

    match generatePreparedARM64 target program with
    | Error e ->
        Error e
    | Ok instrs ->
        let releasesNestedBytesField =
            instrs
            |> List.exists (function
                | ARM64Symbolic.LDR (ARM64.X12, ARM64.X11, 0s) ->
                    true
                | _ ->
                    false)
        let preservesNestedBaseRegister =
            instrs
            |> List.exists (function
                | ARM64Symbolic.STP_pre (ARM64.X10, ARM64.X11, ARM64.SP, -48s) ->
                    true
                | _ ->
                    false)
        if not releasesNestedBytesField then
            Error "Generic fixed-block nested bytes field release did not consume the nested release plan"
        elif not preservesNestedBaseRegister then
            Error "Generic fixed-block nested release did not preserve X11 while using it as child base"
        else
            Ok ()

let testPlannedListGenericLeafReleaseReloadsBlockPointer () : TestResult =
    let listType = AST.TList (AST.TTuple [ AST.TString; AST.TInt64 ])
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
        let reloadsGenericLeafPointer =
            instrs
            |> List.exists (function
                | ARM64Symbolic.LDR (ARM64.X8, ARM64.X3, 0s) ->
                    true
                | _ ->
                    false)

        if reloadsGenericLeafPointer then
            Ok ()
        else
            Error "ARM64 planned list generic release did not reload the leaf pointer before freeing it"

let testPlannedListNestedGenericReleasePreservesBlockPointer () : TestResult =
    let listType = AST.TList (AST.TTuple [ AST.TTuple [ AST.TString; AST.TInt64 ]; AST.TInt64 ])
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
        let preservesNestedGenericBlockPointer =
            instrs
            |> List.exists (function
                | ARM64Symbolic.STP_pre (ARM64.X12, ARM64.X30, ARM64.SP, -16s) ->
                    true
                | _ ->
                    false)

        if preservesNestedGenericBlockPointer then
            Ok ()
        else
            Error "ARM64 planned list nested generic release did not preserve the block pointer across nested field releases"

let testPlannedListTuplePayloadUsesPlannedHelper () : TestResult =
    let tupleType =
        AST.TTuple [ AST.TString; AST.TList AST.TInt64; AST.TDict (AST.TInt64, AST.TInt64) ]
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
        if emitsPlannedListHelperLabel instrs then
            Ok ()
        else
            Error "ARM64 tuple list payload did not emit a planned list helper"

let testPlannedListRecordPayloadUsesPlannedHelper () : TestResult =
    let recordType = AST.TRecord ("ARM64PlannedListRecordPayload", [])
    let records =
        Map.ofList [
            ("ARM64PlannedListRecordPayload", [ ("name", AST.TString); ("items", AST.TList AST.TInt64) ])
        ]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadataWithRecords records (AST.TList recordType)))
            ]
            records

    match generatePreparedARM64 target program with
    | Error e ->
        Error e
    | Ok instrs ->
        if emitsPlannedListHelperLabel instrs then
            Ok ()
        else
            Error "ARM64 record list payload did not emit a planned list helper"

let testPlannedListRecordNestedStringDictUsesPlannedListHelper () : TestResult =
    let recordType = AST.TRecord ("ARM64PlannedListRecordNestedStringDict", [])
    let records =
        Map.ofList [
            ("ARM64PlannedListRecordNestedStringDict",
             [ ("items", AST.TList (AST.TDict (AST.TString, AST.TString))) ])
        ]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadataWithRecords records (AST.TList recordType)))
            ]
            records

    match generatePreparedARM64 target program with
    | Error e ->
        Error e
    | Ok instrs ->
        let callsLegacyDictListHelper =
            instrs
            |> List.exists (function
                | ARM64Symbolic.BL "__dark_list_refcount_dec_dict_helper" -> true
                | _ -> false)
        let plannedListHelperCount =
            instrs
            |> List.choose (function
                | ARM64Symbolic.Label label when label.StartsWith("__dark_list_refcount_dec_plan_") ->
                    Some label
                | _ -> None)
            |> Set.ofList
            |> Set.count

        if callsLegacyDictListHelper then
            Error "Nested List<Dict<String, String>> called the unplanned legacy list/dict helper"
        elif plannedListHelperCount < 2 then
            Error $"Expected outer-record and inner-dict planned list helpers, found {plannedListHelperCount}"
        else
            Ok ()

let testPlannedListTuple5PayloadUsesPlannedHelper () : TestResult =
    let tupleType =
        AST.TTuple [
            AST.TString
            AST.TBlob
            AST.TList AST.TInt64
            AST.TDict (AST.TInt64, AST.TList AST.TInt64)
            AST.TFunction ([AST.TInt64], AST.TInt64)
        ]
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
        if emitsPlannedListHelperLabel instrs then
            Ok ()
        else
            Error "ARM64 tuple5 list payload did not emit a planned list helper"

let testPlannedListRecord5PayloadUsesPlannedHelper () : TestResult =
    let recordType = AST.TRecord ("ARM64PlannedListRecord5Payload", [])
    let records =
        Map.ofList [
            ("ARM64PlannedListRecord5Payload",
                [
                    ("name", AST.TString)
                    ("blob", AST.TBlob)
                    ("items", AST.TList AST.TInt64)
                    ("lookup", AST.TDict (AST.TInt64, AST.TList AST.TInt64))
                    ("fn", AST.TFunction ([AST.TInt64], AST.TInt64))
                ])
        ]
    let program =
        makeSimpleProgramWithRecords
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    0,
                    LIR.TaggedList,
                    Some (rcMetadataWithRecords records (AST.TList recordType)))
            ]
            records

    match generatePreparedARM64 target program with
    | Error e ->
        Error e
    | Ok instrs ->
        if emitsPlannedListHelperLabel instrs then
            Ok ()
        else
            Error "ARM64 record5 list payload did not emit a planned list helper"

let testGenericFixedBlockNestedImmediateFieldReleasesChildRoot () : TestResult =
    let nestedType = AST.TTuple [ AST.TInt64 ]
    let parentType = AST.TTuple [ nestedType ]
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    8,
                    LIR.GenericHeap,
                    Some (rcMetadata parentType))
            ]
            Map.empty

    match generatePreparedARM64 target program with
    | Error e ->
        Error e
    | Ok instrs ->
        let releasesNestedRoot =
            instrs
            |> List.exists (function
                | ARM64Symbolic.LDR (ARM64.X12, ARM64.X0, 0s) ->
                    true
                | _ ->
                    false)
        if releasesNestedRoot then
            Ok ()
        else
            Error "Generic fixed-block nested immediate field release did not release the child root"

let testGenericFixedBlockNestedMixedBoxedSumBytesPayloadUsesVariantDispatch () : TestResult =
    let sumName = "Arm64NestedFixedBlockSumBytes"
    let sumType = AST.TSum (sumName, [])
    let parentType = AST.TTuple [ sumType ]
    let variants : LIR.VariantRegistry =
        Map.ofList [
            (sumName,
                { TypeParams = []
                  Variants =
                    [
                        { Name = "Arm64NestedFixedBlockNoPayload"; Tag = 0; Payload = None }
                        { Name = "Arm64NestedFixedBlockSumBytesPayload"; Tag = 1; Payload = Some AST.TBlob }
                    ] })
        ]
    let sumShapes =
        variants
        |> Map.map (fun _ typeVariants ->
            { MemoryModel.TypeParams = typeVariants.TypeParams
              MemoryModel.Payloads =
                typeVariants.Variants
                |> List.sortBy (fun variant -> variant.Tag)
                |> List.map (fun variant -> variant.Tag, variant.Payload) })
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    8,
                    LIR.GenericHeap,
                    Some (rcMetadataWithSumShapes sumShapes parentType))
            ]
            variants

    match generatePreparedARM64 target program with
    | Error e ->
        Error e
    | Ok instrs ->
        let loadsNestedSumTag =
            instrs
            |> List.exists (function
                | ARM64Symbolic.LDR (ARM64.X10, ARM64.X11, 0s) ->
                    true
                | _ ->
                    false)
        if loadsNestedSumTag then
            Ok ()
        else
            Error "Generic fixed-block nested mixed boxed-sum payload release did not dispatch on the child variant tag"

let testGenericMixedBoxedSumPayloadDispatchSkipsRemainingCases () : TestResult =
    let sumName = "Arm64MixedSumPayloadDispatch"
    let sumType = AST.TSum (sumName, [])
    let variants : LIR.VariantRegistry =
        Map.ofList [
            (sumName,
                { TypeParams = []
                  Variants =
                    [
                        { Name = "Arm64MixedSumBytesPayload"; Tag = 0; Payload = Some AST.TBlob }
                        { Name = "Arm64MixedSumListPayload"; Tag = 1; Payload = Some (AST.TList AST.TInt64) }
                    ] })
        ]
    let sumShapes =
        variants
        |> Map.map (fun _ typeVariants ->
            { MemoryModel.TypeParams = typeVariants.TypeParams
              MemoryModel.Payloads =
                typeVariants.Variants
                |> List.sortBy (fun variant -> variant.Tag)
                |> List.map (fun variant -> variant.Tag, variant.Payload) })
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    16,
                    LIR.GenericHeap,
                    Some (rcMetadataWithSumShapes sumShapes sumType))
            ]
            variants

    match generatePreparedARM64 target program with
    | Error e ->
        Error e
    | Ok instrs ->
        let rec branchAppearsBeforeSecondCase (seenFirstCase: bool) (remaining: ARM64Symbolic.Instr list) : bool =
            match remaining with
            | [] ->
                false
            | ARM64Symbolic.CMP_imm (ARM64.X10, 0us) :: rest ->
                branchAppearsBeforeSecondCase true rest
            | ARM64Symbolic.CMP_imm (ARM64.X10, 1us) :: _ when seenFirstCase ->
                false
            | ARM64Symbolic.B _ :: _ when seenFirstCase ->
                true
            | _ :: rest ->
                branchAppearsBeforeSecondCase seenFirstCase rest

        let emitsBranchAfterMatchedPayload =
            branchAppearsBeforeSecondCase false instrs

        if emitsBranchAfterMatchedPayload then
            Ok ()
        else
            Error "Generic mixed boxed-sum payload release did not branch past remaining variant cases after a match"

/// Recursive-sum release dispatch shape is not observable in an executable
/// E2E test. A variant without managed fields must not consume a tag case in
/// the generated helper, while the recursive variant must remain dispatched.
let testRecursiveSumReleaseSkipsVariantWithoutManagedFields () : TestResult =
    let sumName = "Arm64RecursiveReleaseTree"
    let sumType = AST.TSum (sumName, [])
    let variants : LIR.VariantRegistry =
        Map.ofList [
            (sumName,
                { TypeParams = []
                  Variants =
                    [
                        { Name = "Arm64RecursiveReleaseLeaf"; Tag = 0; Payload = Some AST.TInt64 }
                        { Name = "Arm64RecursiveReleaseNode"
                          Tag = 1
                          Payload = Some (AST.TTuple [ sumType; sumType ]) }
                    ] })
        ]
    let sumShapes =
        variants
        |> Map.map (fun _ typeVariants ->
            { MemoryModel.TypeParams = typeVariants.TypeParams
              MemoryModel.Payloads =
                typeVariants.Variants
                |> List.sortBy (fun variant -> variant.Tag)
                |> List.map (fun variant -> variant.Tag, variant.Payload) })
    let program =
        makeSimpleProgramWithVariants
            [
                LIR.RefCountDec (
                    LIR.Physical LIR.X0,
                    16,
                    LIR.GenericHeap,
                    Some (rcMetadataWithSumShapes sumShapes sumType))
            ]
            variants

    match generatePreparedARM64 target program with
    | Error e ->
        Error e
    | Ok instrs ->
        let rec findHelperBody remaining =
            match remaining with
            | ARM64Symbolic.Label helperLabel :: rest
                when helperLabel.StartsWith("__dark_recursive_sum_rc_dec_") ->
                Some (rest |> List.takeWhile (fun instr -> instr <> ARM64Symbolic.RET))
            | _ :: rest ->
                findHelperBody rest
            | [] ->
                None

        match findHelperBody instrs with
        | None ->
            Error "Recursive-sum release helper was not generated"
        | Some helperBody ->
            let dispatchesLeaf =
                helperBody
                |> List.exists (function
                    | ARM64Symbolic.CBNZ (ARM64.X1, targetLabel)
                        when targetLabel.Contains("_variant_0_next") ->
                        true
                    | _ ->
                        false)
            let dispatchesNode =
                helperBody
                |> List.contains (ARM64Symbolic.CMP_imm (ARM64.X1, 1us))

            if dispatchesLeaf then
                Error "Recursive-sum release helper dispatched a variant without managed fields"
            else if not dispatchesNode then
                Error "Recursive-sum release helper omitted the recursive variant"
            else
                Ok ()

let testClosureCaptureNestedFixedBlockBytesFieldUsesReleasePlan () : TestResult =
    let nestedType = AST.TTuple [ AST.TBlob ]
    let captureType = AST.TTuple [ nestedType ]
    let closureParamType = AST.TTuple [ AST.TInt64; captureType ]
    let capturedFunc =
        makeEmptyFunction
            "arm64_nested_tuple_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureParamType }]
    let main =
        match
            makeSimpleProgramWithVariants
                [
                    LIR.ClosureAlloc (
                        LIR.Physical LIR.X1,
                        AST.functionIdForName "arm64_nested_tuple_capture_fn",
                        [LIR.Reg (LIR.Physical LIR.X2)])
                    LIR.RefCountDec (
                        LIR.Physical LIR.X1,
                        16,
                        LIR.ClosureHeap,
                        Some (rcMetadata (AST.TFunction ([AST.TInt64], AST.TInt64))))
                ]
                Map.empty
        with
        | LIR.Program ([func], variants, records) ->
            LIR.Program ([func; capturedFunc], variants, records)
        | other ->
            other

    match generatePreparedARM64 target main with
    | Error e ->
        Error e
    | Ok instrs ->
        let releasesNestedBytesField =
            instrs
            |> List.exists (function
                | ARM64Symbolic.LDR (ARM64.X12, ARM64.X11, 0s) ->
                    true
                | _ ->
                    false)
        if releasesNestedBytesField then
            Ok ()
        else
            Error "Closure capture nested fixed-block bytes field release did not consume the nested release plan"

let testClosureCaptureBoxedSumBytesPayloadUsesReleasePlan () : TestResult =
    let sumName = "Arm64ClosureCaptureSumBytes"
    let sumType = AST.TSum (sumName, [])
    let closureParamType = AST.TTuple [ AST.TInt64; sumType ]
    let variants : LIR.VariantRegistry =
        Map.ofList [
            (sumName,
                { TypeParams = []
                  Variants =
                    [
                        { Name = "Arm64ClosureCaptureSumBytesPayload"; Tag = 0; Payload = Some AST.TBlob }
                    ] })
        ]
    let capturedFunc =
        makeEmptyFunction
            "arm64_sum_bytes_capture_fn"
            [{ Reg = LIR.Physical LIR.X0; Type = closureParamType }]
    let main =
        match
            makeSimpleProgramWithVariants
                [
                    LIR.ClosureAlloc (
                        LIR.Physical LIR.X1,
                        AST.functionIdForName "arm64_sum_bytes_capture_fn",
                        [LIR.Reg (LIR.Physical LIR.X2)])
                    LIR.RefCountDec (
                        LIR.Physical LIR.X1,
                        16,
                        LIR.ClosureHeap,
                        Some (rcMetadata (AST.TFunction ([AST.TInt64], AST.TInt64))))
                ]
                variants
        with
        | LIR.Program ([func], programVariants, records) ->
            LIR.Program ([func; capturedFunc], programVariants, records)
        | other ->
            other

    match generatePreparedARM64 target main with
    | Error e ->
        Error e
    | Ok instrs ->
        let releasesSumBytesPayload =
            instrs
            |> List.exists (function
                | ARM64Symbolic.LDR (ARM64.X12, ARM64.X8, 8s) ->
                    true
                | _ ->
                    false)
        if releasesSumBytesPayload then
            Ok ()
        else
            Error "Closure capture boxed-sum bytes payload release did not consume the variant release plan"
