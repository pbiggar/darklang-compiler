// MemoryShapeTests.fs - Verify memory shapes and stable recursive release plans.

module MemoryShapeTests

open MemoryModel
open ReleasePlanFingerprint
open MemoryPlanning
open ANF

type TestResult = Result<unit, string>

let testRcShapeConstructionAndEquality () : TestResult =
    let tupleShape =
        FixedBlock (
            16,
            [
                Immediate
                DynamicString
            ]
        )

    let dictShape = DictRoot (DynamicString, TaggedListShape Immediate)
    let closureShape = ClosureShape [tupleShape; dictShape]
    let expected = ClosureShape [FixedBlock (16, [Immediate; DynamicString]); DictRoot (DynamicString, TaggedListShape Immediate)]

    if closureShape = expected then
        Ok ()
    else
        Error $"Expected RcShape equality to use structural representation, got: {closureShape}"

let testRcShapeClassifiesPrimitivesAsImmediate () : TestResult =
    let primitiveTypes = [
        AST.TInt8
        AST.TInt16
        AST.TInt32
        AST.TInt64
        AST.TUInt8
        AST.TUInt16
        AST.TUInt32
        AST.TUInt64
        AST.TBool
        AST.TFloat64
        AST.TUnit
        AST.TNever
        AST.TVar "a"
    ]

    match primitiveTypes |> List.tryFind (fun typ -> rcShapeOfType Map.empty typ <> Immediate) with
    | None -> Ok ()
    | Some typ -> Error $"Expected primitive type {typ} to classify as Immediate"

let testRcShapeClassifiesManagedIntegerBuffers () : TestResult =
    let samples = [AST.TInt, DynamicInt; AST.TInt128, FixedBlock (16, []); AST.TUInt128, FixedBlock (16, [])]

    match samples |> List.tryFind (fun (typ, expected) -> rcShapeOfType Map.empty typ <> expected) with
    | None -> Ok ()
    | Some (typ, expected) -> Error $"Expected integer buffer type {typ} to classify as {expected}"

let testRcShapeClassifiesTuplesAndRecordsAsFixedBlocks () : TestResult =
    let typeReg =
        Map.ofList [
            ("Pair", [("left", AST.TInt64); ("right", AST.TString)])
        ]

    let tupleShape = rcShapeOfType typeReg (AST.TTuple [AST.TInt64; AST.TString; AST.TBool])
    let recordShape = rcShapeOfType typeReg (AST.TRecord ("Pair", []))

    match tupleShape, recordShape with
    | FixedBlock (24, [Immediate; DynamicString; Immediate]), FixedBlock (16, [Immediate; DynamicString]) ->
        Ok ()
    | _ ->
        Error $"Unexpected fixed-block shapes. tuple={tupleShape}; record={recordShape}"

let testRcShapeClassifiesRemainingRuntimeShapes () : TestResult =
    let samples = [
        (AST.TString, DynamicString)
        (AST.TChar, DynamicString)
        (AST.TBlob, DynamicBlob)
        (AST.TInternalRawPtr, RawUnmanaged)
        (AST.TFunction ([AST.TInt64], AST.TString), ClosureShape [])
        (AST.TSum ("Color", []), Immediate)
        (AST.TSum ("Option", [AST.TString]), BoxedSum (16, [(8, DynamicString)], []))
        (AST.TList AST.TString, TaggedListShape DynamicString)
        (AST.TDict (AST.TString, AST.TList AST.TInt64), DictRoot (DynamicString, TaggedListShape Immediate))
    ]

    match samples |> List.tryFind (fun (typ, expected) -> rcShapeOfType Map.empty typ <> expected) with
    | None -> Ok ()
    | Some (typ, expected) ->
        Error $"Expected {typ} to classify as {expected}, got {rcShapeOfType Map.empty typ}"

let testRcShapeClassifiesSumsWithVariantMetadata () : TestResult =
    let typeReg =
        Map.ofList [
            ("PayloadRecord", [("name", AST.TString)])
        ]

    let variantReg : RcSumShapeRegistry =
        Map.ofList [
            ("Enum", { TypeParams = []; Payloads = [0, None; 1, None] })
            ("Maybe", { TypeParams = ["a"]; Payloads = [0, None; 1, Some (AST.TVar "a")] })
            ("Packet", { TypeParams = []; Payloads = [0, Some (AST.TRecord ("PayloadRecord", [])); 1, Some AST.TBlob] })
        ]

    let samples = [
        AST.TSum ("Enum", []), Immediate
        AST.TSum ("Maybe", [AST.TString]),
            BoxedSum (
                16,
                [(8, DynamicString)],
                [
                    { Tag = 0; FieldShapes = [] }
                    { Tag = 1; FieldShapes = [(8, DynamicString)] }
                ])
        AST.TSum ("Packet", []),
            BoxedSum (
                16,
                [(8, FixedBlock (8, [DynamicString])); (8, DynamicBlob)],
                [
                    { Tag = 0; FieldShapes = [(8, FixedBlock (8, [DynamicString]))] }
                    { Tag = 1; FieldShapes = [(8, DynamicBlob)] }
                ])
    ]

    let recordTypeParams = inferredRecordTypeParamsRegistry typeReg
    match samples |> List.tryFind (fun (typ, expected) -> rcShapeOfTypeWithSums typeReg recordTypeParams variantReg typ <> expected) with
    | Some (typ, expected) ->
        Error $"Expected variant-aware shape for {typ} to be {expected}, got {rcShapeOfTypeWithSums typeReg recordTypeParams variantReg typ}"
    | None ->
        Ok ()

let testRcShapeOwnershipHelpersClassifyManagedRoots () : TestResult =
    let managedShapes = [
        DynamicString
        DynamicBlob
        FixedBlock (16, [Immediate; DynamicString])
        BoxedSum (16, [], [])
        TaggedListShape DynamicString
        DictRoot (DynamicString, Immediate)
        ClosureShape [DynamicString]
    ]

    let unmanagedShapes = [
        Immediate
        StaticString
        RawUnmanaged
    ]

    match managedShapes |> List.tryFind (fun shape -> not (rcShapeNeedsOwnedScopeRelease shape)) with
    | Some shape ->
        Error $"Expected managed shape {shape} to need owned scope release"
    | None ->
        match unmanagedShapes |> List.tryFind rcShapeNeedsOwnedScopeRelease with
        | Some shape ->
            Error $"Expected unmanaged shape {shape} to skip owned scope release"
        | None ->
            Ok ()

let testRcShapeOwnershipHelpersClassifyAutomaticBindingDecs () : TestResult =
    let automaticDecShapes = [
        DynamicString
        DynamicBlob
        FixedBlock (16, [Immediate; DynamicString])
        BoxedSum (16, [], [])
        TaggedListShape DynamicString
        DictRoot (DynamicString, Immediate)
    ]

    let skippedShapes = [
        Immediate
        StaticString
        RawUnmanaged
        ClosureShape [DynamicString]
    ]

    match automaticDecShapes |> List.tryFind (fun shape -> not (rcShapeNeedsAutomaticBindingDec shape)) with
    | Some shape ->
        Error $"Expected shape {shape} to need automatic binding dec"
    | None ->
        match skippedShapes |> List.tryFind rcShapeNeedsAutomaticBindingDec with
        | Some shape ->
            Error $"Expected shape {shape} to skip automatic binding dec"
        | None ->
            Ok ()

let testRcShapeOwnershipHelpersClassifyBorrowedRetains () : TestResult =
    let retainedShapes = [
        DynamicString
        DynamicBlob
        FixedBlock (16, [Immediate; DynamicString])
        BoxedSum (16, [], [])
        TaggedListShape DynamicString
        DictRoot (DynamicString, Immediate)
        ClosureShape [DynamicString]
    ]

    let skippedShapes = [
        Immediate
        StaticString
        RawUnmanaged
    ]

    match retainedShapes |> List.tryFind (fun shape -> not (rcShapeNeedsBorrowedRetain shape)) with
    | Some shape ->
        Error $"Expected borrowed shape {shape} to need retain when materializing ownership"
    | None ->
        match skippedShapes |> List.tryFind rcShapeNeedsBorrowedRetain with
        | Some shape ->
            Error $"Expected borrowed shape {shape} to skip retain"
        | None ->
            Ok ()

let testRcShapeOwnershipHelpersSelectRootDispatch () : TestResult =
    let samples = [
        (FixedBlock (16, [DynamicString]), Some GenericHeap)
        (BoxedSum (16, [], []), Some GenericHeap)
        (TaggedListShape DynamicString, Some TaggedList)
        (TaggedListShape (ClosureShape []), Some TaggedList)
        (DictRoot (Immediate, DynamicString), Some DictHeap)
        (ClosureShape [DynamicString], Some ClosureHeap)
        (Immediate, None)
        (DynamicString, None)
        (DynamicBlob, None)
        (RawUnmanaged, None)
    ]

    match samples |> List.tryFind (fun (shape, expected) -> rcShapeRootKind shape <> expected) with
    | None ->
        Ok ()
    | Some (shape, expected) ->
        Error $"Expected shape {shape} to use root kind {expected}, got {rcShapeRootKind shape}"

let testRcShapeOwnershipHelpersSelectRetainReleaseOperations () : TestResult =
    let samples = [
        (FixedBlock (16, [DynamicString]), Some (FixedSizeRoot (16, GenericHeap)))
        (BoxedSum (16, [], []), Some (FixedSizeRoot (16, GenericHeap)))
        (TaggedListShape DynamicString, Some (FixedSizeRoot (24, TaggedList)))
        (TaggedListShape (ClosureShape []), Some (FixedSizeRoot (24, TaggedList)))
        (DictRoot (Immediate, DynamicString), Some (FixedSizeRoot (8, DictHeap)))
        (ClosureShape [DynamicString], Some (FixedSizeRoot (0, ClosureHeap)))
        (DynamicString, Some DynamicStringBuffer)
        (DynamicBlob, Some DynamicBlobBuffer)
        (Immediate, None)
        (StaticString, None)
        (RawUnmanaged, None)
    ]

    match samples |> List.tryFind (fun (shape, expected) -> rcShapeRetainOperation shape <> expected) with
    | Some (shape, expected) ->
        Error $"Expected shape {shape} to use retain operation {expected}, got {rcShapeRetainOperation shape}"
    | None ->
        match samples |> List.tryFind (fun (shape, expected) -> rcShapeReleaseOperation shape <> expected) with
        | Some (shape, expected) ->
            Error $"Expected shape {shape} to use release operation {expected}, got {rcShapeReleaseOperation shape}"
        | None ->
            Ok ()

let testRcShapeOwnershipHelpersClassifyStorage () : TestResult =
    let samples = [
        (FixedBlock (16, [DynamicString]), ManagedRcRoot (16, GenericHeap))
        (BoxedSum (16, [], []), ManagedRcRoot (16, GenericHeap))
        (TaggedListShape DynamicString, ManagedRcRoot (24, TaggedList))
        (TaggedListShape (ClosureShape []), ManagedRcRoot (24, TaggedList))
        (DictRoot (Immediate, DynamicString), ManagedRcRoot (8, DictHeap))
        (ClosureShape [DynamicString], ManagedRcRoot (0, ClosureHeap))
        (DynamicString, ManagedDynamicBuffer DynamicStringBuffer)
        (DynamicBlob, ManagedDynamicBuffer DynamicBlobBuffer)
        (Immediate, UnmanagedStorage)
        (StaticString, UnmanagedStorage)
        (RawUnmanaged, UnmanagedStorage)
    ]

    match samples |> List.tryFind (fun (shape, expected) -> rcShapeStorageClass shape <> expected) with
    | None ->
        Ok ()
    | Some (shape, expected) ->
        Error $"Expected shape {shape} to use storage class {expected}, got {rcShapeStorageClass shape}"

let testRcShapeOwnershipHelpersClassifyRootManagement () : TestResult =
    let managedRootShapes = [
        FixedBlock (16, [Immediate; DynamicString])
        BoxedSum (16, [], [])
        TaggedListShape DynamicString
        DictRoot (DynamicString, TaggedListShape Immediate)
        ClosureShape [DynamicString]
    ]

    let nonRootShapes = [
        Immediate
        DynamicString
        DynamicBlob
        StaticString
        RawUnmanaged
    ]

    match managedRootShapes |> List.tryFind (fun shape -> not (rcShapeIsRootManaged shape)) with
    | Some shape ->
        Error $"Expected shape {shape} to be a managed RC root"
    | None ->
        match nonRootShapes |> List.tryFind rcShapeIsRootManaged with
        | Some shape ->
            Error $"Expected shape {shape} not to be a managed RC root"
        | None ->
            Ok ()

let testRcShapeOwnershipHelpersClassifyOwnershipTransferRoots () : TestResult =
    let transferRootShapes = [
        FixedBlock (16, [Immediate; DynamicString])
        BoxedSum (16, [], [])
        TaggedListShape DynamicString
        DictRoot (DynamicString, Immediate)
        ClosureShape [DynamicString]
    ]

    let nonTransferRootShapes = [
        Immediate
        DynamicString
        DynamicBlob
        StaticString
        RawUnmanaged
    ]

    match transferRootShapes |> List.tryFind (fun shape -> not (rcShapeIsOwnershipTransferRoot shape)) with
    | Some shape ->
        Error $"Expected shape {shape} to be an ownership-transfer root"
    | None ->
        match nonTransferRootShapes |> List.tryFind rcShapeIsOwnershipTransferRoot with
        | Some shape ->
            Error $"Expected shape {shape} not to be an ownership-transfer root"
        | None ->
            Ok ()

let testRcShapeOwnershipHelpersClassifyRecursiveRelease () : TestResult =
    let recursiveShapes = [
        FixedBlock (16, [Immediate; DynamicString])
        BoxedSum (16, [(8, DynamicString)], [])
        TaggedListShape (FixedBlock (8, [DynamicString]))
        DictRoot (DynamicString, TaggedListShape Immediate)
        ClosureShape [DynamicString]
    ]

    let nonRecursiveShapes = [
        Immediate
        DynamicString
        DynamicBlob
        StaticString
        RawUnmanaged
        FixedBlock (8, [Immediate])
        TaggedListShape Immediate
        DictRoot (Immediate, Immediate)
        ClosureShape []
    ]

    match recursiveShapes |> List.tryFind (fun shape -> not (rcShapeNeedsRecursiveRelease shape)) with
    | Some shape ->
        Error $"Expected shape {shape} to need recursive release"
    | None ->
        match nonRecursiveShapes |> List.tryFind rcShapeNeedsRecursiveRelease with
        | Some shape ->
            Error $"Expected shape {shape} not to need recursive release"
        | None ->
            Ok ()

let testRcShapeReleasePlanClassifiesFieldCleanup () : TestResult =
    let samples = [
        (Immediate, NoReleasePlan)
        (StaticString, NoReleasePlan)
        (RawUnmanaged, NoReleasePlan)
        (DynamicString, DynamicBufferRelease DynamicStringBuffer)
        (DynamicBlob, DynamicBufferRelease DynamicBlobBuffer)
        (TaggedListShape DynamicString, RootRelease (24, TaggedList, TaggedListPayloadRelease (DynamicBufferRelease DynamicStringBuffer)))
        (DictRoot (DynamicString, FixedBlock (8, [DynamicBlob])),
            RootRelease (
                8,
                DictHeap,
                DictPayloadRelease (
                    DynamicBufferRelease DynamicStringBuffer,
                    RootRelease (8, GenericHeap, FixedBlockPayloadRelease (8, [FieldRelease (0, DynamicBufferRelease DynamicBlobBuffer)])))))
        (FixedBlock (16, [Immediate; DynamicString]),
            RootRelease (16, GenericHeap, FixedBlockPayloadRelease (16, [FieldRelease (8, DynamicBufferRelease DynamicStringBuffer)])))
        (ClosureShape [DynamicString],
            RootRelease (0, ClosureHeap, ClosurePayloadRelease [FieldRelease (0, DynamicBufferRelease DynamicStringBuffer)]))
        (BoxedSum (16, [], []), RootRelease (16, GenericHeap, BoxedSumPayloadRelease (16, [], [])))
    ]

    match samples |> List.tryFind (fun (shape, expected) -> rcShapeReleasePlan shape <> expected) with
    | None ->
        Ok ()
    | Some (shape, expected) ->
        Error $"Expected shape {shape} to use release plan {expected}, got {rcShapeReleasePlan shape}"

let testRcSourceTypeFingerprintIsStructuralAndStable () : TestResult =
    let samples = [
        AST.TInt64
        AST.TString
        AST.TList AST.TString
        AST.TTuple [AST.TString; AST.TInt64]
        AST.TTuple [AST.TInt64; AST.TString]
        AST.TRecord ("Pair", [AST.TString; AST.TInt64])
        AST.TSum ("Pair", [AST.TString; AST.TInt64])
        AST.TDict (AST.TString, AST.TList AST.TBlob)
    ]
    let fingerprints = samples |> List.map rcSourceTypeFingerprint
    if fingerprints <> (samples |> List.map rcSourceTypeFingerprint) then
        Error "RC source-type fingerprints were not deterministic"
    elif (fingerprints |> List.distinct |> List.length) <> List.length samples then
        Error $"Distinct RC source types produced duplicate fingerprints: {List.zip samples fingerprints}"
    else
        Ok ()

let testRcReleasePlanFingerprintIsCompositionalAndStable () : TestResult =
    let nestedList =
        RootRelease (
            24,
            TaggedList,
            TaggedListPayloadRelease (DynamicBufferRelease DynamicStringBuffer))
    let samples = [
        NoReleasePlan
        DynamicBufferRelease DynamicBlobBuffer
        RecursiveRelease (AST.TSum ("Tree", [AST.TString]))
        nestedList
        RootRelease (
            16,
            DictHeap,
            DictPayloadRelease (DynamicBufferRelease DynamicStringBuffer, nestedList))
        RootRelease (
            24,
            GenericHeap,
            BoxedSumPayloadRelease (
                24,
                [FieldRelease (8, nestedList)],
                [{ Tag = 0; FieldReleases = [] }
                 { Tag = 1
                   FieldReleases =
                       [FieldRelease (16, DynamicBufferRelease DynamicBlobBuffer)] }]))
    ]
    let directChildren releasePlan =
        match releasePlan with
        | RootRelease (_, _, payload) ->
            match payload with
            | NoPayloadRelease -> []
            | FixedBlockPayloadRelease (_, fields)
            | ClosurePayloadRelease fields ->
                fields
                |> List.map (fun (FieldRelease (_, childPlan)) -> childPlan)
            | BoxedSumPayloadRelease (_, fields, variants) ->
                let fieldChildren =
                    fields
                    |> List.map (fun (FieldRelease (_, childPlan)) -> childPlan)
                let variantChildren =
                    variants
                    |> List.collect (fun variant ->
                        variant.FieldReleases
                        |> List.map (fun (FieldRelease (_, childPlan)) -> childPlan))
                fieldChildren @ variantChildren
            | TaggedListPayloadRelease elementRelease -> [elementRelease]
            | DictPayloadRelease (keyRelease, valueRelease) ->
                [keyRelease; valueRelease]
        | NoReleasePlan
        | DynamicBufferRelease _
        | RecursiveRelease _ -> []
    let compositionalFingerprints =
        samples
        |> List.map (fun releasePlan ->
            directChildren releasePlan
            |> List.map rcReleasePlanFingerprintHash
            |> rcReleasePlanFingerprintHashFromChildren releasePlan
            |> rcReleasePlanFingerprintString)
    let recursiveFingerprints = samples |> List.map rcReleasePlanFingerprint
    if compositionalFingerprints <> recursiveFingerprints then
        Error "Composed RC release-plan fingerprints differed from recursive fingerprints"
    elif recursiveFingerprints <> (samples |> List.map rcReleasePlanFingerprint) then
        Error "RC release-plan fingerprints were not deterministic"
    elif (recursiveFingerprints |> List.distinct |> List.length) <> List.length samples then
        Error $"Distinct RC release plans produced duplicate fingerprints: {List.zip samples recursiveFingerprints}"
    else
        Ok ()

let testRcReleasePlanCacheKeyOnlyFingerprintsLargePlans () : TestResult =
    let smallType = AST.TTuple [AST.TString; AST.TInt64]
    let smallPlan = rcReleasePlanOfType Map.empty smallType
    let largeType = AST.TTuple (List.replicate 30 AST.TString)
    let largePlan = rcReleasePlanOfType Map.empty largeType
    match rcReleasePlanCacheKey smallType smallPlan, rcReleasePlanCacheKey largeType largePlan with
    | None, Some cacheKey when cacheKey = rcSourceTypeFingerprint largeType -> Ok ()
    | smallKey, largeKey ->
        Error $"Expected only the large release plan to use a compact key, got small={smallKey}, large={largeKey}"

let testRcReleasePlanOfTypeUsesRecordMetadata () : TestResult =
    let typeReg =
        Map.ofList [
            ("Packet", [("header", AST.TInt64); ("body", AST.TString); ("tail", AST.TBlob)])
        ]

    let expected =
        RootRelease (
            24,
            GenericHeap,
            FixedBlockPayloadRelease (
                24,
                [
                    FieldRelease (8, DynamicBufferRelease DynamicStringBuffer)
                    FieldRelease (16, DynamicBufferRelease DynamicBlobBuffer)
                ]))

    let actual = rcReleasePlanOfType typeReg (AST.TRecord ("Packet", []))
    if actual = expected then
        Ok ()
    else
        Error $"Expected record type to use release plan {expected}, got {actual}"

let testRcReleasePlanOfTypeUsesSumPayloadMetadata () : TestResult =
    let sumType = AST.TSum ("MaybeString", [AST.TString])

    let expected =
        RootRelease (
            16,
            GenericHeap,
            BoxedSumPayloadRelease (
                16,
                [
                    FieldRelease (8, DynamicBufferRelease DynamicStringBuffer)
                ],
                []))

    let actual = rcReleasePlanOfType Map.empty sumType
    if actual = expected then
        Ok ()
    else
        Error $"Expected sum type to use release plan {expected}, got {actual}"

let testRcReleasePlanOfTypeWithSumsUsesVariantMetadata () : TestResult =
    let typeReg =
        Map.ofList [
            ("PayloadRecord", [("name", AST.TString); ("blob", AST.TBlob)])
        ]

    let sumReg : RcSumShapeRegistry =
        Map.ofList [
            ("Color", { TypeParams = []; Payloads = [0, None; 1, None; 2, None] })
            ("Maybe", { TypeParams = ["a"]; Payloads = [0, None; 1, Some (AST.TVar "a")] })
            ("Packet", { TypeParams = []; Payloads = [0, Some (AST.TRecord ("PayloadRecord", [])); 1, Some (AST.TList AST.TString)] })
        ]

    let samples = [
        (AST.TSum ("Color", []), NoReleasePlan)
        (AST.TSum ("Maybe", [AST.TString]),
            RootRelease (
                16,
                GenericHeap,
                BoxedSumPayloadRelease (
                16,
                [
                    FieldRelease (8, DynamicBufferRelease DynamicStringBuffer)
                ],
                [
                    { Tag = 0; FieldReleases = [] }
                    { Tag = 1; FieldReleases = [FieldRelease (8, DynamicBufferRelease DynamicStringBuffer)] }
                ])))
        (AST.TSum ("Packet", []),
            RootRelease (
                16,
                GenericHeap,
                BoxedSumPayloadRelease (
                    16,
                    [
                        FieldRelease (
                            8,
                            RootRelease (
                                16,
                                GenericHeap,
                                FixedBlockPayloadRelease (
                                    16,
                                    [
                                        FieldRelease (0, DynamicBufferRelease DynamicStringBuffer)
                                        FieldRelease (8, DynamicBufferRelease DynamicBlobBuffer)
                                    ])))
                    ; FieldRelease (
                        8,
                        RootRelease (
                            24,
                            TaggedList,
                            TaggedListPayloadRelease (DynamicBufferRelease DynamicStringBuffer)))
                ],
                [
                    {
                        Tag = 0
                        FieldReleases =
                            [
                                FieldRelease (
                                    8,
                                    RootRelease (
                                        16,
                                        GenericHeap,
                                        FixedBlockPayloadRelease (
                                            16,
                                            [
                                                FieldRelease (0, DynamicBufferRelease DynamicStringBuffer)
                                                FieldRelease (8, DynamicBufferRelease DynamicBlobBuffer)
                                            ])))
                            ]
                    }
                    {
                        Tag = 1
                        FieldReleases =
                            [
                                FieldRelease (
                                    8,
                                    RootRelease (
                                        24,
                                        TaggedList,
                                        TaggedListPayloadRelease (DynamicBufferRelease DynamicStringBuffer)))
                            ]
                    }
                ])))
    ]

    match samples |> List.tryFind (fun (typ, expected) -> rcReleasePlanOfTypeWithSums typeReg sumReg typ <> expected) with
    | None ->
        Ok ()
    | Some (typ, expected) ->
        Error $"Expected sum-aware release plan for {typ} to be {expected}, got {rcReleasePlanOfTypeWithSums typeReg sumReg typ}"

let testRecursiveSumReleasePlanUsesTypedBackEdge () : TestResult =
    let treeType = AST.TSum ("Tree", [AST.TInt64])
    let sumReg : RcSumShapeRegistry =
        Map.ofList [
            ("Tree",
             { TypeParams = ["a"]
               Payloads =
                   [
                       0, Some (AST.TVar "a")
                       1, Some (AST.TTuple [AST.TSum ("Tree", [AST.TVar "a"]); AST.TSum ("Tree", [AST.TVar "a"])])
                   ] })
        ]

    let plan = rcReleasePlanOfTypeWithSums Map.empty sumReg treeType
    let recursiveTypes = recursiveReleaseTypes plan
    if recursiveTypes = Set.singleton treeType then
        Ok ()
    else
        Error $"Expected recursive Tree release plan to contain one typed back-edge, got {plan}"

let testRecursiveRecordReleasePlanUsesTypedBackEdge () : TestResult =
    let nodeType = AST.TRecord ("RecursiveNode", [])
    let typeReg =
        Map.ofList [
            ("RecursiveNode",
             [
                 "value", AST.TInt64
                 "children", AST.TList nodeType
             ])
        ]

    let plan = rcReleasePlanOfTypeWithSums typeReg Map.empty nodeType
    let recursiveTypes = recursiveReleaseTypes plan
    if recursiveTypes = Set.singleton nodeType then
        Ok ()
    else
        Error $"Expected recursive RecursiveNode release plan to contain one typed back-edge, got {plan}"

let testRcReleasePlanOfTypeClassifiesRemainingRootKinds () : TestResult =
    let samples = [
        (AST.TSum ("Color", []), NoReleasePlan)
        (AST.TSum ("MaybeString", [AST.TString]),
            RootRelease (
                16,
                GenericHeap,
                BoxedSumPayloadRelease (
                16,
                [
                    FieldRelease (8, DynamicBufferRelease DynamicStringBuffer)
                ],
                [])))
        (AST.TFunction ([AST.TInt64], AST.TString), RootRelease (0, ClosureHeap, ClosurePayloadRelease []))
        (AST.TString, DynamicBufferRelease DynamicStringBuffer)
        (AST.TBlob, DynamicBufferRelease DynamicBlobBuffer)
        (AST.TDict (AST.TString, AST.TBlob),
            RootRelease (
                8,
                DictHeap,
                DictPayloadRelease (DynamicBufferRelease DynamicStringBuffer, DynamicBufferRelease DynamicBlobBuffer)))
        (AST.TInternalRawPtr, NoReleasePlan)
    ]

    match samples |> List.tryFind (fun (typ, expected) -> rcReleasePlanOfType Map.empty typ <> expected) with
    | None ->
        Ok ()
    | Some (typ, expected) ->
        Error $"Expected type {typ} to use release plan {expected}, got {rcReleasePlanOfType Map.empty typ}"

let testRcShapeRequiresRecordMetadata () : TestResult =
    try
        let _ = rcShapeOfType Map.empty (AST.TRecord ("MissingRecordMetadata", []))
        Error "Expected missing record metadata to fail before ownership decisions can fall back to source-level heap checks"
    with
    | ex when ex.Message.Contains("MissingRecordMetadata") ->
        Ok ()

let testRcShapeWithSumsRequiresSumMetadata () : TestResult =
    try
        let _ = rcShapeOfTypeWithSums Map.empty Map.empty Map.empty (AST.TSum ("MissingSumMetadata", []))
        Error "Expected missing sum metadata to fail before ownership decisions can fall back to generic boxed sums"
    with
    | ex when ex.Message.Contains("MissingSumMetadata") ->
        Ok ()
