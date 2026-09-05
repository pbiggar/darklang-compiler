// RefCountInsertionTests.fs - Unit tests for RefCountInsertion type inference behavior.
//
// Verifies inferCExprType returns call result types (not function types) so
// downstream RC insertion decisions can use heap/non-heap information correctly.

module RefCountInsertionTests

open ANF
open RefCountInsertion

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
        AST.TRuntimeError
        AST.TVar "a"
    ]

    match primitiveTypes |> List.tryFind (fun typ -> rcShapeOfType Map.empty typ <> Immediate) with
    | None -> Ok ()
    | Some typ -> Error $"Expected primitive type {typ} to classify as Immediate"

let testRcShapeClassifiesManagedIntegerBuffers () : TestResult =
    let managedIntegerTypes = [AST.TInt; AST.TInt128; AST.TUInt128]

    match managedIntegerTypes |> List.tryFind (fun typ -> rcShapeOfType Map.empty typ <> DynamicString) with
    | None -> Ok ()
    | Some typ -> Error $"Expected integer buffer type {typ} to classify as DynamicString"

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
        (AST.TRawPtr, RawUnmanaged)
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
        (AST.TRawPtr, NoReleasePlan)
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

let testInferCallReturnsFunctionReturnType () : TestResult =
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg =
            Map.ofList [
                ("mkPair", AST.TFunction ([AST.TInt64], AST.TTuple [AST.TInt64; AST.TInt64]))
            ]
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let cexpr = Call ("mkPair", [IntLiteral (Int64 1L)])

    match inferCExprType ctx cexpr with
    | Some (AST.TTuple [AST.TInt64; AST.TInt64]) ->
        Ok ()
    | Some actual ->
        Error $"Expected inferCExprType Call to return tuple return type, got: {actual}"
    | None ->
        Error "Expected inferCExprType Call to return a concrete type, got None"

let testMalformedRawGetIntrinsicDoesNotInferInt64 () : TestResult =
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = Map.empty
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let cexpr = Call ("__raw_get_not_a_mangled_type", [Var (TempId 1); IntLiteral (Int64 0L)])

    match inferCExprType ctx cexpr with
    | None ->
        Ok ()
    | Some actual ->
        Error $"Expected malformed __raw_get_ suffix to remain unknown, got: {actual}"

let rec private hasDecAfterNonSelfTailCall (funcName: string) (expr: AExpr) : bool =
    match expr with
    | Return _ ->
        false
    | Let (_, TailCall (target, _), Let (_, RefCountDec _, _)) when target <> funcName ->
        true
    | Let (_, TailCall (target, _), Let (_, RefCountDecString _, _)) when target <> funcName ->
        true
    | Let (_, _, body) ->
        hasDecAfterNonSelfTailCall funcName body
    | If (_, thenBranch, elseBranch) ->
        hasDecAfterNonSelfTailCall funcName thenBranch
        || hasDecAfterNonSelfTailCall funcName elseBranch

let rec private hasRefCountIncForTemp (target: TempId) (expr: AExpr) : bool =
    match expr with
    | Return _ ->
        false
    | Let (_, RefCountInc (Var tempId, _, _, _), _) when tempId = target ->
        true
    | Let (_, _, body) ->
        hasRefCountIncForTemp target body
    | If (_, thenBranch, elseBranch) ->
        hasRefCountIncForTemp target thenBranch
        || hasRefCountIncForTemp target elseBranch

let rec private hasRefCountDecForTemp (target: TempId) (expr: AExpr) : bool =
    match expr with
    | Return _ ->
        false
    | Let (_, RefCountDec (Var tempId, _, _, _), _) when tempId = target ->
        true
    | Let (_, _, body) ->
        hasRefCountDecForTemp target body
    | If (_, thenBranch, elseBranch) ->
        hasRefCountDecForTemp target thenBranch
        || hasRefCountDecForTemp target elseBranch

let rec private hasStringRetainForTemp (target: TempId) (expr: AExpr) : bool =
    match expr with
    | Return _ ->
        false
    | Let (_, RefCountIncString (Var tempId), _) when tempId = target ->
        true
    | Let (_, _, body) ->
        hasStringRetainForTemp target body
    | If (_, thenBranch, elseBranch) ->
        hasStringRetainForTemp target thenBranch
        || hasStringRetainForTemp target elseBranch

let rec private hasStringReleaseForTemp (target: TempId) (expr: AExpr) : bool =
    match expr with
    | Return _ ->
        false
    | Let (_, RefCountDecString (Var tempId), _) when tempId = target ->
        true
    | Let (_, _, body) ->
        hasStringReleaseForTemp target body
    | If (_, thenBranch, elseBranch) ->
        hasStringReleaseForTemp target thenBranch
        || hasStringReleaseForTemp target elseBranch

let private pathHasRetainsBeforeDec
    (retainTargets: TempId list)
    (decTarget: TempId)
    (expr: AExpr)
    : bool =
    let required = Set.ofList retainTargets

    let rec loop (seenRetains: Set<TempId>) (expr: AExpr) : bool =
        match expr with
        | Return _ ->
            false
        | Let (_, RefCountInc (Var tempId, _, _, _), body) ->
            loop (Set.add tempId seenRetains) body
        | Let (_, RefCountDec (Var tempId, _, _, _), _) when tempId = decTarget ->
            Set.isSubset required seenRetains
        | Let (_, _, body) ->
            loop seenRetains body
        | If (_, thenBranch, elseBranch) ->
            loop seenRetains thenBranch || loop seenRetains elseBranch

    loop Set.empty expr

let rec private tryRefCountDecSourceTypeForTemp (target: TempId) (expr: AExpr) : AST.Type option =
    match expr with
    | Return _ ->
        None
    | Let (_, RefCountDec (Var tempId, _, _, metadata), _) when tempId = target ->
        metadata |> Option.bind (fun value -> value.SourceType)
    | Let (_, _, body) ->
        tryRefCountDecSourceTypeForTemp target body
    | If (_, thenBranch, elseBranch) ->
        match tryRefCountDecSourceTypeForTemp target thenBranch with
        | Some typ -> Some typ
        | None -> tryRefCountDecSourceTypeForTemp target elseBranch

let testReturnedAggregateTransfersOwnedValueThroughAlias () : TestResult =
    let childType = AST.TTuple [AST.TInt64]
    let outerType = AST.TTuple [childType]
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            ("makeChild", AST.TFunction ([], childType))
            ("wrapChild", AST.TFunction ([], outerType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let childTemp = TempId 0
    let aliasTemp = TempId 1
    let outerTemp = TempId 2
    let func : Function = {
        Name = "wrapChild"
        TypedParams = []
        ReturnType = outerType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                childTemp,
                Call ("makeChild", []),
                Let (
                    aliasTemp,
                    Atom (Var childTemp),
                    Let (
                        outerTemp,
                        TupleAlloc [Var aliasTemp],
                        Return (Var outerTemp)
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp aliasTemp transformed.Body then
        Error "Returned aggregate should adopt owned value through a pure alias without retaining it"
    elif hasRefCountDecForTemp childTemp transformed.Body then
        Error "Returned aggregate alias transfer should remove the original owner's pending release"
    else
        Ok ()

let testReturnedAggregateTransfersOwnedValueThroughTypedAlias () : TestResult =
    let childType = AST.TTuple [AST.TInt64]
    let outerType = AST.TTuple [childType]
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            ("makeChild", AST.TFunction ([], childType))
            ("wrapChild", AST.TFunction ([], outerType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let childTemp = TempId 0
    let aliasTemp = TempId 1
    let outerTemp = TempId 2
    let func : Function = {
        Name = "wrapChild"
        TypedParams = []
        ReturnType = outerType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                childTemp,
                Call ("makeChild", []),
                Let (
                    aliasTemp,
                    TypedAtom (Var childTemp, childType),
                    Let (
                        outerTemp,
                        TupleAlloc [Var aliasTemp],
                        Return (Var outerTemp)
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp aliasTemp transformed.Body then
        Error "Returned aggregate should adopt owned value through a typed alias without retaining it"
    elif hasRefCountDecForTemp childTemp transformed.Body then
        Error "Returned aggregate typed-alias transfer should remove the original owner's pending release"
    else
        Ok ()

let testReturnedAggregateTransfersNestedOwnedAliases () : TestResult =
    let childType = AST.TTuple [AST.TInt64]
    let innerType = AST.TTuple [childType]
    let outerType = AST.TTuple [innerType]
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            ("makeChild", AST.TFunction ([], childType))
            ("wrapChild", AST.TFunction ([], outerType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let childTemp = TempId 0
    let childAlias = TempId 1
    let innerTemp = TempId 2
    let innerAlias = TempId 3
    let outerTemp = TempId 4
    let func : Function = {
        Name = "wrapChild"
        TypedParams = []
        ReturnType = outerType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                childTemp,
                Call ("makeChild", []),
                Let (
                    childAlias,
                    Atom (Var childTemp),
                    Let (
                        innerTemp,
                        TupleAlloc [Var childAlias],
                        Let (
                            innerAlias,
                            Atom (Var innerTemp),
                            Let (
                                outerTemp,
                                TupleAlloc [Var innerAlias],
                                Return (Var outerTemp)
                            )
                        )
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen
    let retainedAlias =
        hasRefCountIncForTemp childAlias transformed.Body
        || hasRefCountIncForTemp innerAlias transformed.Body
    let releasedOwner =
        hasRefCountDecForTemp childTemp transformed.Body
        || hasRefCountDecForTemp innerTemp transformed.Body

    if retainedAlias then
        Error "Nested returned aggregates should adopt owned values through pure aliases without retaining them"
    elif releasedOwner then
        Error "Nested returned aggregate alias transfer should remove each original owner's pending release"
    else
        Ok ()

let testReturnedAggregateDoesNotTransferDuplicatedAliases () : TestResult =
    let childType = AST.TTuple [AST.TInt64]
    let outerType = AST.TTuple [childType; childType]
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            ("makeChild", AST.TFunction ([], childType))
            ("duplicateChild", AST.TFunction ([], outerType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let childTemp = TempId 0
    let firstAlias = TempId 1
    let secondAlias = TempId 2
    let outerTemp = TempId 3
    let func : Function = {
        Name = "duplicateChild"
        TypedParams = []
        ReturnType = outerType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                childTemp,
                Call ("makeChild", []),
                Let (
                    firstAlias,
                    Atom (Var childTemp),
                    Let (
                        secondAlias,
                        TypedAtom (Var childTemp, childType),
                        Let (
                            outerTemp,
                            TupleAlloc [Var firstAlias; Var secondAlias],
                            Return (Var outerTemp)
                        )
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp firstAlias transformed.Body
       && hasRefCountIncForTemp secondAlias transformed.Body
       && hasRefCountDecForTemp childTemp transformed.Body then
        Ok ()
    else
        Error "Duplicated aliases must retain both aggregate edges and preserve the original owner's release"

let testStaticStringBindingSkipsNoOpRcTraffic () : TestResult =
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = Map.empty
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let stringTemp = TempId 0
    let resultTemp = TempId 1
    let func : Function = {
        Name = "staticString"
        TypedParams = []
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                stringTemp,
                Atom (StringLiteral "static"),
                Let (
                    resultTemp,
                    Atom (IntLiteral (Int64 1L)),
                    Return (Var resultTemp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasStringReleaseForTemp stringTemp transformed.Body then
        Error "Static string binding should not emit a runtime no-op release"
    else
        Ok ()

let testKnownEmptyListBindingSkipsNoOpRcTraffic () : TestResult =
    let listType = AST.TList AST.TInt64
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = Map.empty
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let listTemp = TempId 0
    let resultTemp = TempId 1
    let func : Function = {
        Name = "emptyList"
        TypedParams = []
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                listTemp,
                TypedAtom (IntLiteral (Int64 0L), listType),
                Let (
                    resultTemp,
                    Atom (IntLiteral (Int64 1L)),
                    Return (Var resultTemp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountDecForTemp listTemp transformed.Body then
        Error "Known empty-list binding should not emit a runtime no-op release"
    else
        Ok ()

let testAggregateSkipsRetainsForKnownNonRcSentinels () : TestResult =
    let listType = AST.TList AST.TInt64
    let resultType = AST.TTuple [AST.TString; listType]
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = Map.empty
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let stringTemp = TempId 0
    let listTemp = TempId 1
    let resultTemp = TempId 2
    let func : Function = {
        Name = "sentinelTuple"
        TypedParams = []
        ReturnType = resultType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                stringTemp,
                Atom (StringLiteral "static"),
                Let (
                    listTemp,
                    TypedAtom (IntLiteral (Int64 0L), listType),
                    Let (
                        resultTemp,
                        TupleAlloc [Var stringTemp; Var listTemp],
                        Return (Var resultTemp)
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasStringRetainForTemp stringTemp transformed.Body then
        Error "Aggregate should not retain a known static string field"
    elif hasRefCountIncForTemp listTemp transformed.Body then
        Error "Aggregate should not retain a known empty-list field"
    else
        Ok ()

let testNonSelfTailCallDoesNotLeaveDecAfterTailCall () : TestResult =
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            ("callee", AST.TFunction ([AST.TInt64], AST.TInt64))
            ("caller", AST.TFunction ([AST.TInt64], AST.TInt64))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let p0 = TempId 0
    let tupleTmp = TempId 1
    let callTmp = TempId 2
    let func : Function = {
        Name = "caller"
        TypedParams = [{ Id = p0; Type = AST.TInt64 }]
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                tupleTmp,
                TupleAlloc [Var p0; IntLiteral (Int64 1L)],
                Let (
                    callTmp,
                    TailCall ("callee", [Var p0]),
                    Return (Var callTmp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasDecAfterNonSelfTailCall transformed.Name transformed.Body then
        Error "Found RefCountDec after non-self TailCall; dec should execute before tailcall"
    else
        Ok ()

let testAliasReturnMaterializesOwnershipEvenIfFunctionMarkedBorrowed () : TestResult =
    let nodeType = AST.TList AST.TInt64
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            ("Stdlib.Internal.SkewList.__node2GetChild_i64", AST.TFunction ([nodeType; AST.TInt64], nodeType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let nodeParam = TempId 0
    let indexParam = TempId 1
    let childTemp = TempId 2

    let func : Function = {
        Name = "Stdlib.Internal.SkewList.__node2GetChild_i64"
        TypedParams = [
            { Id = nodeParam; Type = nodeType }
            { Id = indexParam; Type = AST.TInt64 }
        ]
        ReturnType = nodeType
        ReturnOwnership = BorrowedReturn
        Body =
            Let (
                childTemp,
                RawGet (Var nodeParam, IntLiteral (Int64 0L), Some nodeType),
                Return (Var childTemp)
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp childTemp transformed.Body then
        Ok ()
    else
        Error "Alias return should materialize ownership with RefCountInc even when function is marked BorrowedReturn"

let testMapHelperAccumulatorReturnDoesNotRetainOwnedAccumulator () : TestResult =
    let sourceListType = AST.TList AST.TInt64
    let mappedListType = AST.TList (AST.TFunction ([AST.TInt64], AST.TInt64))
    let mapperType = AST.TFunction ([AST.TInt64], AST.TFunction ([AST.TInt64], AST.TInt64))
    let helperName = "Stdlib.List.__mapHelper_i64_fn_i64_to_i64"
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            (helperName, AST.TFunction ([sourceListType; mapperType; mappedListType], mappedListType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let sourceParam = TempId 0
    let mapperParam = TempId 1
    let accParam = TempId 2
    let func : Function = {
        Name = helperName
        TypedParams = [
            { Id = sourceParam; Type = sourceListType }
            { Id = mapperParam; Type = mapperType }
            { Id = accParam; Type = mappedListType }
        ]
        ReturnType = mappedListType
        ReturnOwnership = OwnedReturn
        Body = Return (Var accParam)
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp accParam transformed.Body then
        Error "Stdlib.List.__mapHelper should transfer its owned accumulator return without retaining it"
    else
        Ok ()

let testMapHelperSelfTailCallReleasesReplacedAccumulator () : TestResult =
    let sourceListType = AST.TList AST.TInt64
    let mappedListType = AST.TList (AST.TFunction ([AST.TInt64], AST.TInt64))
    let mapperType = AST.TFunction ([AST.TInt64], AST.TFunction ([AST.TInt64], AST.TInt64))
    let helperName = "Stdlib.List.__mapHelper"
    let specializedHelperName = "Stdlib.List.__mapHelper_i64_fn_i64_to_i64"
    let pushBackName = "Stdlib.Internal.SkewList.pushBack_fn_i64_to_i64"
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            (helperName, AST.TFunction ([sourceListType; mapperType; mappedListType], mappedListType))
            (specializedHelperName, AST.TFunction ([sourceListType; mapperType; mappedListType], mappedListType))
            ("mappedClosure", AST.TFunction ([AST.TInt64], AST.TInt64))
            (pushBackName, AST.TFunction ([mappedListType; AST.TFunction ([AST.TInt64], AST.TInt64)], mappedListType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let sourceParam = TempId 0
    let mapperParam = TempId 1
    let accParam = TempId 2
    let closureTemp = TempId 3
    let newAccTemp = TempId 4
    let tailTemp = TempId 5
    let func : Function = {
        Name = helperName
        TypedParams = [
            { Id = sourceParam; Type = sourceListType }
            { Id = mapperParam; Type = mapperType }
            { Id = accParam; Type = mappedListType }
        ]
        ReturnType = mappedListType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                closureTemp,
                ClosureAlloc ("mappedClosure", []),
                Let (
                    newAccTemp,
                    Call (pushBackName, [Var accParam; Var closureTemp]),
                    Let (
                        tailTemp,
                        TailCall (specializedHelperName, [Var sourceParam; Var mapperParam; Var newAccTemp]),
                        Return (Var tailTemp)
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountDecForTemp accParam transformed.Body then
        Ok ()
    else
        Error "Stdlib.List.__mapHelper self tail-call should release the replaced owned accumulator"

let private testBorrowedProjectionRecursiveArgsAreRetained (recursiveCExpr: string -> Atom list -> CExpr) : TestResult =
    let state1Type = AST.TTuple [AST.TInt64; AST.TInt64; AST.TInt64]
    let state2Type = AST.TTuple [AST.TInt64; AST.TInt64]
    let resultType = AST.TTuple [state1Type; state2Type]
    let helperName = "loop"
    let roundName = "round"
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            (helperName, AST.TFunction ([state1Type; state2Type; AST.TInt64], resultType))
            (roundName, AST.TFunction ([state1Type; state2Type; AST.TInt64], resultType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let state1Param = TempId 0
    let state2Param = TempId 1
    let iParam = TempId 2
    let resultTemp = TempId 3
    let nextState1Temp = TempId 4
    let nextState2Temp = TempId 5
    let tailTemp = TempId 6
    let func : Function = {
        Name = helperName
        TypedParams = [
            { Id = state1Param; Type = state1Type }
            { Id = state2Param; Type = state2Type }
            { Id = iParam; Type = AST.TInt64 }
        ]
        ReturnType = resultType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                resultTemp,
                Call (roundName, [Var state1Param; Var state2Param; Var iParam]),
                Let (
                    nextState1Temp,
                    TupleGet (Var resultTemp, 0),
                    Let (
                        nextState2Temp,
                        TupleGet (Var resultTemp, 1),
                        Let (
                            tailTemp,
                            recursiveCExpr helperName [Var nextState1Temp; Var nextState2Temp; Var iParam],
                            Return (Var tailTemp)
                        )
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if pathHasRetainsBeforeDec [nextState1Temp; nextState2Temp] resultTemp transformed.Body then
        Ok ()
    else
        Error "Borrowed tuple projections passed as self-tail-call accumulators should be retained before parent cleanup"

let testBorrowedProjectionSelfTailCallArgsAreRetained () : TestResult =
    testBorrowedProjectionRecursiveArgsAreRetained (fun funcName args -> TailCall (funcName, args))

let testBorrowedProjectionSelfRecursiveCallArgsAreRetained () : TestResult =
    testBorrowedProjectionRecursiveArgsAreRetained (fun funcName args -> Call (funcName, args))

let testBorrowedProjectionAliasSelfRecursiveCallArgsAreRetained () : TestResult =
    let state1Type = AST.TTuple [AST.TInt64; AST.TInt64; AST.TInt64]
    let state2Type = AST.TTuple [AST.TInt64; AST.TInt64]
    let resultType = AST.TTuple [state1Type; state2Type]
    let helperName = "loop"
    let roundName = "round"
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            (helperName, AST.TFunction ([state1Type; state2Type; AST.TInt64], resultType))
            (roundName, AST.TFunction ([state1Type; state2Type; AST.TInt64], resultType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let state1Param = TempId 0
    let state2Param = TempId 1
    let iParam = TempId 2
    let resultTemp = TempId 3
    let nextState1Temp = TempId 4
    let nextState2Temp = TempId 5
    let nextState1Alias = TempId 6
    let nextState2Alias = TempId 7
    let tailTemp = TempId 8
    let func : Function = {
        Name = helperName
        TypedParams = [
            { Id = state1Param; Type = state1Type }
            { Id = state2Param; Type = state2Type }
            { Id = iParam; Type = AST.TInt64 }
        ]
        ReturnType = resultType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                resultTemp,
                Call (roundName, [Var state1Param; Var state2Param; Var iParam]),
                Let (
                    nextState1Temp,
                    TupleGet (Var resultTemp, 0),
                    Let (
                        nextState2Temp,
                        TupleGet (Var resultTemp, 1),
                        Let (
                            nextState1Alias,
                            TypedAtom (Var nextState1Temp, state1Type),
                            Let (
                                nextState2Alias,
                                TypedAtom (Var nextState2Temp, state2Type),
                                Let (
                                    tailTemp,
                                    Call (helperName, [Var nextState1Alias; Var nextState2Alias; Var iParam]),
                                    Return (Var tailTemp)
                                )
                            )
                        )
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp nextState1Temp transformed.Body
       && hasRefCountIncForTemp nextState2Temp transformed.Body then
        Ok ()
    else
        Error "Borrowed tuple projection aliases passed as self-recursive accumulators should be retained before parent cleanup"

let testBorrowedProjectionIfBranchSelfRecursiveCallArgsAreRetained () : TestResult =
    let state1Type = AST.TTuple [AST.TInt64; AST.TInt64; AST.TInt64]
    let state2Type = AST.TTuple [AST.TInt64; AST.TInt64]
    let resultType = AST.TTuple [state1Type; state2Type]
    let helperName = "loop"
    let roundName = "round"
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            (helperName, AST.TFunction ([state1Type; state2Type; AST.TInt64], resultType))
            (roundName, AST.TFunction ([state1Type; state2Type; AST.TInt64], resultType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let state1Param = TempId 0
    let state2Param = TempId 1
    let iParam = TempId 2
    let baseResultTemp = TempId 3
    let roundResultTemp = TempId 4
    let roundResultAliasTemp = TempId 5
    let nextState1Temp = TempId 6
    let nextState1AliasTemp = TempId 7
    let nextState1SecondAliasTemp = TempId 8
    let nextState2Temp = TempId 9
    let nextState2AliasTemp = TempId 10
    let nextState2SecondAliasTemp = TempId 11
    let nextI = TempId 12
    let recursiveResultTemp = TempId 13
    let func : Function = {
        Name = helperName
        TypedParams = [
            { Id = state1Param; Type = state1Type }
            { Id = state2Param; Type = state2Type }
            { Id = iParam; Type = AST.TInt64 }
        ]
        ReturnType = resultType
        ReturnOwnership = OwnedReturn
        Body =
            If (
                Var iParam,
                Let (
                    baseResultTemp,
                    TupleAlloc [Var state1Param; Var state2Param],
                    Return (Var baseResultTemp)
                ),
                Let (
                    roundResultTemp,
                    Call (roundName, [Var state1Param; Var state2Param; Var iParam]),
                    Let (
                        roundResultAliasTemp,
                        Atom (Var roundResultTemp),
                        Let (
                            nextState1Temp,
                            TupleGet (Var roundResultAliasTemp, 0),
                            Let (
                                nextState1AliasTemp,
                                TypedAtom (Var nextState1Temp, state1Type),
                                Let (
                                    nextState1SecondAliasTemp,
                                    TypedAtom (Var nextState1AliasTemp, state1Type),
                                    Let (
                                        nextState2Temp,
                                        TupleGet (Var roundResultAliasTemp, 1),
                                        Let (
                                            nextState2AliasTemp,
                                            TypedAtom (Var nextState2Temp, state2Type),
                                            Let (
                                                nextState2SecondAliasTemp,
                                                TypedAtom (Var nextState2AliasTemp, state2Type),
                                                Let (
                                                    nextI,
                                                    Prim (Add, Var iParam, IntLiteral (Int64 1L)),
                                                    Let (
                                                        recursiveResultTemp,
                                                        Call (
                                                            helperName,
                                                            [
                                                                Var nextState1SecondAliasTemp
                                                                Var nextState2SecondAliasTemp
                                                                Var nextI
                                                            ]
                                                        ),
                                                        Return (Var recursiveResultTemp)
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if pathHasRetainsBeforeDec [nextState1Temp; nextState2Temp] roundResultTemp transformed.Body then
        Ok ()
    else
        Error "Borrowed tuple projections in recursive if branches should be retained before parent cleanup"

let testBorrowedProjectionFromParameterSelfRecursiveCallStaysBorrowed () : TestResult =
    let childType = AST.TTuple [AST.TInt64]
    let parentType = AST.TTuple [childType]
    let helperName = "loop"
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            (helperName, AST.TFunction ([parentType; childType], childType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let parentParam = TempId 0
    let childParam = TempId 1
    let projectedTemp = TempId 2
    let resultTemp = TempId 3
    let func : Function = {
        Name = helperName
        TypedParams = [
            { Id = parentParam; Type = parentType }
            { Id = childParam; Type = childType }
        ]
        ReturnType = childType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                projectedTemp,
                TupleGet (Var parentParam, 0),
                Let (
                    resultTemp,
                    Call (helperName, [Var parentParam; Var projectedTemp]),
                    Return (Var resultTemp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp projectedTemp transformed.Body then
        Error "Borrowed projection from a parameter should not be retained solely because it feeds a self-recursive call"
    else
        Ok ()

let testMapHelperClosureProducingCallRetainsBorrowedSource () : TestResult =
    let sourceListType = AST.TList AST.TInt64
    let mappedListType = AST.TList (AST.TFunction ([AST.TInt64], AST.TInt64))
    let mapperType = AST.TFunction ([AST.TInt64], AST.TFunction ([AST.TInt64], AST.TInt64))
    let helperName = "Stdlib.List.__mapHelper_i64_fn_i64_to_i64"
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            (helperName, AST.TFunction ([sourceListType; mapperType; mappedListType], mappedListType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let sourceParam = TempId 0
    let mapperParam = TempId 1
    let accParam = TempId 2
    let mappedTemp = TempId 3
    let func : Function = {
        Name = "caller"
        TypedParams = [
            { Id = sourceParam; Type = sourceListType }
            { Id = mapperParam; Type = mapperType }
            { Id = accParam; Type = mappedListType }
        ]
        ReturnType = mappedListType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                mappedTemp,
                Call (helperName, [Var sourceParam; Var mapperParam; Var accParam]),
                Return (Var mappedTemp)
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp sourceParam transformed.Body then
        Ok ()
    else
        Error "Callers entering closure-producing Stdlib.List.__mapHelper should retain the borrowed source list"

let testMapHelperClosureSourceToValueKeepsSourceBorrowed () : TestResult =
    let sourceListType = AST.TList (AST.TFunction ([AST.TInt64], AST.TInt64))
    let mappedListType = AST.TList AST.TInt64
    let mapperType = AST.TFunction ([AST.TFunction ([AST.TInt64], AST.TInt64)], AST.TInt64)
    let helperName = "Stdlib.List.__mapHelper_fn_i64_to_i64_i64"
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            (helperName, AST.TFunction ([sourceListType; mapperType; mappedListType], mappedListType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let sourceParam = TempId 0
    let mapperParam = TempId 1
    let accParam = TempId 2
    let func : Function = {
        Name = helperName
        TypedParams = [
            { Id = sourceParam; Type = sourceListType }
            { Id = mapperParam; Type = mapperType }
            { Id = accParam; Type = mappedListType }
        ]
        ReturnType = mappedListType
        ReturnOwnership = OwnedReturn
        Body = Return (Var accParam)
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountIncForTemp sourceParam transformed.Body then
        Error "Stdlib.List.__mapHelper over closure source to value should not retain an unreturned borrowed source parameter"
    elif hasRefCountDecForTemp sourceParam transformed.Body then
        Error "Stdlib.List.__mapHelper over closure source to value should not release a borrowed source parameter"
    else
        Ok ()

let testClosurePushBackRetainsImmediateClosureCallResult () : TestResult =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let makerType = AST.TFunction ([AST.TInt64], closureType)
    let listType = AST.TList closureType
    let pushBackName = "Stdlib.Internal.SkewList.pushBack_fn_i64_to_i64"
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            ("makeClosure", makerType)
            ("mappedClosure", closureType)
            ("returnedClosure", closureType)
            (pushBackName, AST.TFunction ([listType; closureType], listType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let makerTemp = TempId 0
    let returnedTemp = TempId 1
    let listParam = TempId 2
    let pushedTemp = TempId 3
    let func : Function = {
        Name = "caller"
        TypedParams = [
            { Id = listParam; Type = listType }
        ]
        ReturnType = listType
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                makerTemp,
                ClosureAlloc ("makeClosure", []),
                Let (
                    returnedTemp,
                    ClosureCall (Var makerTemp, [IntLiteral (Int64 5L)]),
                    Let (
                        pushedTemp,
                        Call (pushBackName, [Var listParam; Var returnedTemp]),
                        Return (Var pushedTemp)
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountDecForTemp returnedTemp transformed.Body then
        Ok ()
    else
        Error "ClosureCall result passed directly to typed closure-list pushBack should get a local dec because raw storage retains the edge"

let testBorrowedCallStillGetsAutoDecUnderConservativePolicy () : TestResult =
    let nodeType = AST.TList AST.TInt64
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            ("consumer", AST.TFunction ([nodeType; AST.TInt64], AST.TInt64))
            ("Stdlib.Internal.SkewList.__node2GetChild_i64", AST.TFunction ([nodeType; AST.TInt64], nodeType))
            ("Stdlib.Internal.SkewList.__nodeMeasure_i64", AST.TFunction ([nodeType], AST.TInt64))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let nodeParam = TempId 0
    let indexParam = TempId 1
    let childTemp = TempId 2
    let measureTemp = TempId 3

    let func : Function = {
        Name = "consumer"
        TypedParams = [
            { Id = nodeParam; Type = nodeType }
            { Id = indexParam; Type = AST.TInt64 }
        ]
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                childTemp,
                BorrowedCall ("Stdlib.Internal.SkewList.__node2GetChild_i64", [Var nodeParam; Var indexParam]),
                Let (
                    measureTemp,
                    Call ("Stdlib.Internal.SkewList.__nodeMeasure_i64", [Var childTemp]),
                    Return (Var measureTemp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountDecForTemp childTemp transformed.Body then
        Ok ()
    else
        Error "BorrowedCall should be treated as owned result under conservative policy and get automatic RefCountDec"

let testCallReturningClosureGetsAutoDecAfterUse () : TestResult =
    let closureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            ("makeClosure", AST.TFunction ([], closureType))
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let closureTemp = TempId 0
    let resultTemp = TempId 1
    let func : Function = {
        Name = "caller"
        TypedParams = []
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                closureTemp,
                Call ("makeClosure", []),
                Let (
                    resultTemp,
                    ClosureCall (Var closureTemp, [IntLiteral (Int64 5L)]),
                    Return (Var resultTemp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountDecForTemp closureTemp transformed.Body then
        Ok ()
    else
        Error "Call result with function type should receive automatic closure RefCountDec after use"

let testClosureCallReturningClosureGetsAutoDecAfterUse () : TestResult =
    let returnedClosureType = AST.TFunction ([AST.TInt64], AST.TInt64)
    let makerClosureType = AST.TFunction ([AST.TInt64], returnedClosureType)
    let funcReg : AST_to_ANF.FunctionRegistry =
        Map.ofList [
            ("makeClosure", makerClosureType)
            ("returnedClosure", returnedClosureType)
        ]

    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg = Map.empty
        FuncReg = funcReg
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let makerTemp = TempId 0
    let returnedTemp = TempId 1
    let resultTemp = TempId 2
    let func : Function = {
        Name = "caller"
        TypedParams = []
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                makerTemp,
                ClosureAlloc ("makeClosure", []),
                Let (
                    returnedTemp,
                    ClosureCall (Var makerTemp, [IntLiteral (Int64 5L)]),
                    Let (
                        resultTemp,
                        Atom (IntLiteral (Int64 0L)),
                        Return (Var resultTemp)
                    )
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountDecForTemp returnedTemp transformed.Body then
        Ok ()
    else
        Error "ClosureCall result with function return type should receive automatic closure RefCountDec after use"

let testPureEnumBindingDoesNotGetAutomaticDec () : TestResult =
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup = Map.empty
        SumShapeReg =
            Map.ofList [
                ("Color", { TypeParams = []; Payloads = [0, None; 1, None] })
            ]
        FuncReg = Map.empty
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let enumTemp = TempId 0
    let resultTemp = TempId 1
    let enumType = AST.TSum ("Color", [])
    let func : Function = {
        Name = "pureEnumBinding"
        TypedParams = []
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                enumTemp,
                TypedAtom (IntLiteral (Int64 0L), enumType),
                Let (
                    resultTemp,
                    Atom (IntLiteral (Int64 1L)),
                    Return (Var resultTemp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountDecForTemp enumTemp transformed.Body then
        Error "Pure enum binding should classify as immediate and must not get automatic RefCountDec"
    else
        Ok ()

let testGenericPureEnumBindingDoesNotGetAutomaticDec () : TestResult =
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup =
            Map.ofList [
                ("Left", ("Phantom", ["a"], 0, None))
                ("Right", ("Phantom", ["a"], 1, None))
            ]
        SumShapeReg =
            Map.ofList [
                ("Phantom", { TypeParams = ["a"]; Payloads = [0, None; 1, None] })
            ]
        FuncReg = Map.empty
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let enumTemp = TempId 0
    let resultTemp = TempId 1
    let enumType = AST.TSum ("Phantom", [AST.TString])
    let func : Function = {
        Name = "genericPureEnumBinding"
        TypedParams = []
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                enumTemp,
                TypedAtom (IntLiteral (Int64 0L), enumType),
                Let (
                    resultTemp,
                    Atom (IntLiteral (Int64 1L)),
                    Return (Var resultTemp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen

    if hasRefCountDecForTemp enumTemp transformed.Body then
        Error "Generic pure enum binding should classify from variant metadata and must not get automatic RefCountDec"
    else
        Ok ()

let testBareSumTypeRefsAreCanonicalizedForRcSourceTypes () : TestResult =
    let payloadType = AST.TRecord ("Payload", [])
    let dictType = AST.TDict (AST.TInt64, payloadType)
    let ctx : TypeContext = {
        TypeReg = Map.empty
        VariantLookup =
            Map.ofList [
                ("Empty", ("Payload", [], 0, None))
                ("SomePayload", ("Payload", [], 1, Some AST.TString))
            ]
        SumShapeReg =
            Map.ofList [
                ("Payload", { TypeParams = []; Payloads = [0, None; 1, Some AST.TString] })
            ]
        FuncReg =
            Map.ofList [
                ("mkDict", AST.TFunction ([], dictType))
            ]
        FuncParams = Map.empty
        TempTypes = Map.empty
        ClosureFuncs = Map.empty
    }

    let dictTemp = TempId 0
    let resultTemp = TempId 1
    let func : Function = {
        Name = "canonicalBareSum"
        TypedParams = []
        ReturnType = AST.TInt64
        ReturnOwnership = OwnedReturn
        Body =
            Let (
                dictTemp,
                Call ("mkDict", []),
                Let (
                    resultTemp,
                    Atom (IntLiteral (Int64 1L)),
                    Return (Var resultTemp)
                )
            )
    }

    let (transformed, _, _) = insertRCInFunction ctx func initialVarGen
    match tryRefCountDecSourceTypeForTemp dictTemp transformed.Body with
    | Some (AST.TDict (AST.TInt64, AST.TSum ("Payload", []))) ->
        Ok ()
    | Some other ->
        Error $"Expected dict dec source type to canonicalize Payload as a sum, got {other}"
    | None ->
        Error "Expected dict binding to receive automatic RefCountDec"

let tests = [
    ("RcShape supports structural construction and equality", testRcShapeConstructionAndEquality)
    ("RcShape classifies primitives as immediate", testRcShapeClassifiesPrimitivesAsImmediate)
    ("RcShape classifies managed integer buffers", testRcShapeClassifiesManagedIntegerBuffers)
    ("RcShape classifies tuples and records as fixed blocks", testRcShapeClassifiesTuplesAndRecordsAsFixedBlocks)
    ("RcShape classifies remaining runtime shapes", testRcShapeClassifiesRemainingRuntimeShapes)
    ("RcShape classifies sums with variant metadata", testRcShapeClassifiesSumsWithVariantMetadata)
    ("RcShape ownership helpers classify managed roots", testRcShapeOwnershipHelpersClassifyManagedRoots)
    ("RcShape ownership helpers classify automatic binding decs", testRcShapeOwnershipHelpersClassifyAutomaticBindingDecs)
    ("RcShape ownership helpers classify borrowed retains", testRcShapeOwnershipHelpersClassifyBorrowedRetains)
    ("RcShape ownership helpers select root dispatch", testRcShapeOwnershipHelpersSelectRootDispatch)
    ("RcShape ownership helpers select retain/release operations", testRcShapeOwnershipHelpersSelectRetainReleaseOperations)
    ("RcShape ownership helpers classify storage", testRcShapeOwnershipHelpersClassifyStorage)
    ("RcShape ownership helpers classify managed RC roots", testRcShapeOwnershipHelpersClassifyRootManagement)
    ("RcShape ownership helpers classify ownership-transfer roots", testRcShapeOwnershipHelpersClassifyOwnershipTransferRoots)
    ("RcShape ownership helpers classify recursive release", testRcShapeOwnershipHelpersClassifyRecursiveRelease)
    ("RcShape release plan classifies field cleanup", testRcShapeReleasePlanClassifiesFieldCleanup)
    ("Rc source type fingerprints are structural and stable", testRcSourceTypeFingerprintIsStructuralAndStable)
    ("Rc release-plan cache keys are compact only for large plans", testRcReleasePlanCacheKeyOnlyFingerprintsLargePlans)
    ("RcReleasePlan of type uses record metadata", testRcReleasePlanOfTypeUsesRecordMetadata)
    ("RcReleasePlan of type uses sum payload metadata", testRcReleasePlanOfTypeUsesSumPayloadMetadata)
    ("RcReleasePlan of type with sums uses variant metadata", testRcReleasePlanOfTypeWithSumsUsesVariantMetadata)
    ("recursive sum release plan uses typed back-edge", testRecursiveSumReleasePlanUsesTypedBackEdge)
    ("RcReleasePlan of type classifies remaining root kinds", testRcReleasePlanOfTypeClassifiesRemainingRootKinds)
    ("RcShape requires record metadata", testRcShapeRequiresRecordMetadata)
    ("RcShape with sums requires sum metadata", testRcShapeWithSumsRequiresSumMetadata)
    ("inferCExprType Call returns function return type", testInferCallReturnsFunctionReturnType)
    ("malformed raw_get intrinsic does not infer Int64", testMalformedRawGetIntrinsicDoesNotInferInt64)
    ("returned aggregate transfers owned value through alias", testReturnedAggregateTransfersOwnedValueThroughAlias)
    ("returned aggregate transfers owned value through typed alias", testReturnedAggregateTransfersOwnedValueThroughTypedAlias)
    ("returned aggregate transfers nested owned aliases", testReturnedAggregateTransfersNestedOwnedAliases)
    ("returned aggregate does not transfer duplicated aliases", testReturnedAggregateDoesNotTransferDuplicatedAliases)
    ("static string binding skips no-op RC traffic", testStaticStringBindingSkipsNoOpRcTraffic)
    ("known empty-list binding skips no-op RC traffic", testKnownEmptyListBindingSkipsNoOpRcTraffic)
    ("aggregate skips retains for known non-RC sentinels", testAggregateSkipsRetainsForKnownNonRcSentinels)
    ("non-self tailcall does not keep dec after tailcall", testNonSelfTailCallDoesNotLeaveDecAfterTailCall)
    ("alias return materializes ownership even for borrowed-return function", testAliasReturnMaterializesOwnershipEvenIfFunctionMarkedBorrowed)
    ("map helper accumulator return transfers ownership without retain", testMapHelperAccumulatorReturnDoesNotRetainOwnedAccumulator)
    ("map helper self tail-call releases replaced accumulator", testMapHelperSelfTailCallReleasesReplacedAccumulator)
    ("borrowed projection self tail-call args are retained", testBorrowedProjectionSelfTailCallArgsAreRetained)
    ("borrowed projection self-recursive call args are retained", testBorrowedProjectionSelfRecursiveCallArgsAreRetained)
    ("borrowed projection alias self-recursive call args are retained", testBorrowedProjectionAliasSelfRecursiveCallArgsAreRetained)
    ("borrowed projection if-branch self-recursive call args are retained", testBorrowedProjectionIfBranchSelfRecursiveCallArgsAreRetained)
    ("borrowed projection from parameter self-recursive call stays borrowed", testBorrowedProjectionFromParameterSelfRecursiveCallStaysBorrowed)
    ("map helper closure-producing call retains borrowed source", testMapHelperClosureProducingCallRetainsBorrowedSource)
    ("map helper closure source to value keeps source borrowed", testMapHelperClosureSourceToValueKeepsSourceBorrowed)
    ("closure pushBack retains immediate closure-call result", testClosurePushBackRetainsImmediateClosureCallResult)
    ("borrowed call still gets auto-dec under conservative policy", testBorrowedCallStillGetsAutoDecUnderConservativePolicy)
    ("call returning closure gets auto-dec after use", testCallReturningClosureGetsAutoDecAfterUse)
    ("closure call returning closure gets auto-dec after use", testClosureCallReturningClosureGetsAutoDecAfterUse)
    ("pure enum binding does not get automatic dec", testPureEnumBindingDoesNotGetAutomaticDec)
    ("generic pure enum binding does not get automatic dec", testGenericPureEnumBindingDoesNotGetAutomaticDec)
    ("bare sum type refs are canonicalized for RC source types", testBareSumTypeRefsAreCanonicalizedForRcSourceTypes)
]
