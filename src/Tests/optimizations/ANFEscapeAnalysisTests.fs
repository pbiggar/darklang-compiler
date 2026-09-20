// ANFEscapeAnalysisTests.fs - Tests local scalar aggregate replacement.
//
// These fixtures pin both allocation removal and the conservative escape and
// ownership boundaries of the first escape-analysis implementation.

module ANFEscapeAnalysisTests

open ANF

type TestResult = Result<unit, string>

let private fid = AST.functionIdForName

let private pointDescriptor fieldType =
    { SourceTypeName = "Point"
      RuntimeTypeName = "Point"
      TypeArgs = []
      Fields = ["x", fieldType; "y", fieldType]
      ValueType = AST.TRecord ("Point", []) }

let rec private containsAggregateAllocation (expr: AExpr) : bool =
    match expr with
    | Jump _ | Return _ -> false
    | Let (_, cexpr, body) ->
        match cexpr with
        | TupleAlloc _
        | RecordAlloc _
        | RecordClone _ -> true
        | _ -> containsAggregateAllocation body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        containsAggregateAllocation thenBranch
        || containsAggregateAllocation elseBranch

let rec private aggregateAllocationCount (expr: AExpr) : int =
    match expr with
    | Jump _ | Return _ -> 0
    | Let (_, cexpr, body) ->
        let current =
            match cexpr with
            | TupleAlloc _
            | RecordAlloc _
            | RecordClone _ -> 1
            | RecordReuse _ -> 0
            | _ -> 0
        current + aggregateAllocationCount body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        aggregateAllocationCount thenBranch + aggregateAllocationCount elseBranch

let rec private containsRecordReuse (expr: AExpr) : bool =
    match expr with
    | Jump _ | Return _ -> false
    | Let (_, RecordReuse _, _) -> true
    | Let (_, _, body) -> containsRecordReuse body
    | Join (_, thenBranch, elseBranch)
    | If (_, thenBranch, elseBranch) ->
        containsRecordReuse thenBranch || containsRecordReuse elseBranch

let private optimizeBodyWithTypes
    (typeReg: TypeRegistries.TypeRegistry)
    (body: AExpr)
    : AExpr =
    let func =
        { Id = fid "fixture"
          Name = "fixture"
          TypedParams = []
          ReturnType = AST.TInt64
          ReturnOwnership = OwnedReturn
          Body = body }
    let (Program (functions, _)) =
        ANF_EscapeAnalysis.scalarReplaceProgram typeReg
            (Program ([func], Return UnitLiteral))
    match functions with
    | [optimized] -> optimized.Body
    | _ -> Crash.crash "ANFEscapeAnalysisTests: fixture function disappeared"

let private optimizeBody (body: AExpr) : AExpr =
    optimizeBodyWithTypes Map.empty body

let testScalarRecordProjectionRemovesAllocation () : TestResult =
    let descriptor = pointDescriptor AST.TInt64
    let body =
        Let (
            TempId 0,
            RecordAlloc (descriptor, [IntLiteral (Int64 10L); IntLiteral (Int64 20L)]),
            Let (TempId 1, RecordGet (descriptor, Var (TempId 0), 1), Return (Var (TempId 1)))
        )
        |> optimizeBody
    match body with
    | Let (TempId 1, Atom (IntLiteral (Int64 20L)), Return (Var (TempId 1))) -> Ok ()
    | _ -> Error $"Expected scalar record projection without allocation, got {body}"

let testAggregateAliasRemovesAllocation () : TestResult =
    let body =
        Let (
            TempId 0,
            TupleAlloc [IntLiteral (Int64 10L); IntLiteral (Int64 20L)],
            Let (
                TempId 1,
                Atom (Var (TempId 0)),
                Let (TempId 2, TupleGet (Var (TempId 1), 0), Return (Var (TempId 2)))
            )
        )
        |> optimizeBody
    if containsAggregateAllocation body then
        Error "Expected projection-only tuple alias to be scalar-replaced"
    else
        match body with
        | Let (TempId 2, Atom (IntLiteral (Int64 10L)), Return (Var (TempId 2))) -> Ok ()
        | _ -> Error $"Expected tuple alias and projection to become a scalar binding, got {body}"

let testScalarRecordCloneChainRemovesAllocations () : TestResult =
    let descriptor = pointDescriptor AST.TInt64
    let body =
        Let (
            TempId 0,
            RecordAlloc (descriptor, [IntLiteral (Int64 1L); IntLiteral (Int64 2L)]),
            Let (
                TempId 1,
                RecordClone (descriptor, Var (TempId 0), [IntLiteral (Int64 3L); IntLiteral (Int64 2L)]),
                Let (TempId 2, RecordGet (descriptor, Var (TempId 1), 0), Return (Var (TempId 2)))
            )
        )
        |> optimizeBody
    if containsAggregateAllocation body then
        Error "Expected scalar record clone chain to be scalar-replaced"
    else Ok ()

let testEscapingCloneRetainsOnlyCloneAllocation () : TestResult =
    let descriptor = pointDescriptor AST.TInt64
    let body =
        Let (
            TempId 0,
            RecordAlloc (descriptor, [IntLiteral (Int64 1L); IntLiteral (Int64 2L)]),
            Let (
                TempId 1,
                RecordClone (descriptor, Var (TempId 0), [IntLiteral (Int64 3L); IntLiteral (Int64 2L)]),
                Return (Var (TempId 1))
            )
        )
        |> optimizeBody
    match body with
    | Let (TempId 1, RecordAlloc (_, _), Return (Var (TempId 1))) -> Ok ()
    | _ -> Error $"Expected an escaping clone to retain only its own allocation, got {body}"

let testReturnedRecordPreservesAllocation () : TestResult =
    let descriptor = pointDescriptor AST.TInt64
    let body =
        Let (
            TempId 0,
            RecordAlloc (descriptor, [IntLiteral (Int64 1L); IntLiteral (Int64 2L)]),
            Return (Var (TempId 0))
        )
        |> optimizeBody
    if containsAggregateAllocation body then Ok ()
    else Error "Expected returned record allocation to be preserved"

let testRecordPassedToCallPreservesAllocation () : TestResult =
    let descriptor = pointDescriptor AST.TInt64
    let body =
        Let (
            TempId 0,
            RecordAlloc (descriptor, [IntLiteral (Int64 1L); IntLiteral (Int64 2L)]),
            Let (TempId 1, Call (fid "consume", [Var (TempId 0)]), Return (Var (TempId 1)))
        )
        |> optimizeBody
    if containsAggregateAllocation body then Ok ()
    else Error "Expected record passed to a call to preserve its allocation"

let testRecordCapturedByClosurePreservesAllocation () : TestResult =
    let descriptor = pointDescriptor AST.TInt64
    let body =
        Let (
            TempId 0,
            RecordAlloc (descriptor, [IntLiteral (Int64 1L); IntLiteral (Int64 2L)]),
            Let (TempId 1, ClosureAlloc (fid "capture", [Var (TempId 0)]), Return (IntLiteral (Int64 0L)))
        )
        |> optimizeBody
    if containsAggregateAllocation body then Ok ()
    else Error "Expected closure-captured record to preserve its allocation"

let testBranchLocalProjectionsRemoveAllocation () : TestResult =
    let descriptor = pointDescriptor AST.TInt64
    let body =
        Let (
            TempId 0,
            RecordAlloc (descriptor, [IntLiteral (Int64 1L); IntLiteral (Int64 2L)]),
            If (
                BoolLiteral true,
                Let (TempId 1, RecordGet (descriptor, Var (TempId 0), 0), Return (Var (TempId 1))),
                Let (TempId 2, RecordGet (descriptor, Var (TempId 0), 1), Return (Var (TempId 2)))
            )
        )
        |> optimizeBody
    if containsAggregateAllocation body then
        Error "Expected projection-only branch uses to be scalar-replaced"
    else Ok ()

let testManagedRecordPreservesAllocation () : TestResult =
    let descriptor = pointDescriptor AST.TString
    let body =
        Let (
            TempId 0,
            RecordAlloc (descriptor, [StringLiteral "left"; StringLiteral "right"]),
            Let (TempId 1, RecordGet (descriptor, Var (TempId 0), 0), Return (Var (TempId 1)))
        )
        |> optimizeBody
    if containsAggregateAllocation body then Ok ()
    else Error "Expected record with managed fields to be preserved"

let testFloatRecordIsScalarReplaced () : TestResult =
    let descriptor = pointDescriptor AST.TFloat64
    let body =
        Let (
            TempId 0,
            RecordAlloc (descriptor, [FloatLiteral 1.0; FloatLiteral 2.0]),
            Let (TempId 1, RecordGet (descriptor, Var (TempId 0), 0), Return (Var (TempId 1)))
        )
        |> optimizeBody
    if not (containsAggregateAllocation body) then Ok ()
    else Error "Expected projection-only Float record to be scalar-replaced"

let testFloatRecordCloneScalarizesSourceAllocation () : TestResult =
    let descriptor = pointDescriptor AST.TFloat64
    let body =
        Let (
            TempId 0,
            RecordAlloc (descriptor, [FloatLiteral 1.0; FloatLiteral 2.0]),
            Let (
                TempId 1,
                RecordClone (descriptor, Var (TempId 0), [FloatLiteral 3.0; FloatLiteral 2.0]),
                Return (Var (TempId 1))
            )
        )
        |> optimizeBody
    match aggregateAllocationCount body with
    | 1 -> Ok ()
    | count -> Error $"Expected the Float clone source to be scalar-replaced, got {count} allocations"

let testFloatRecordCloneReusesNonScalarizedSourceAllocation () : TestResult =
    let descriptor = pointDescriptor AST.TFloat64
    let body =
        Let (
            TempId 0,
            FloatSqrt (FloatLiteral 4.0),
            Let (
                TempId 1,
                RecordAlloc (descriptor, [Var (TempId 0); FloatLiteral 2.0]),
                Let (
                    TempId 2,
                    RecordClone (descriptor, Var (TempId 1), [FloatLiteral 3.0; FloatLiteral 2.0]),
                    Return (Var (TempId 2))
                )
            )
        )
        |> optimizeBody
    match body with
    | Let (
        TempId 0,
        FloatSqrt (FloatLiteral 4.0),
        Let (
            TempId 1,
            RecordAlloc _,
            Let (TempId 2, RecordReuse (_, _, Var (TempId 1), _), Return (Var (TempId 2)))
        )
      ) -> Ok ()
    | _ -> Error $"Expected the escaping Float clone to reuse its dead source allocation, got {body}"

let testFloatRecordCloneAliasChainScalarizesIntermediates () : TestResult =
    let descriptor = pointDescriptor AST.TFloat64
    let body =
        Let (
            TempId 0,
            RecordAlloc (descriptor, [FloatLiteral 1.0; FloatLiteral 2.0]),
            Let (
                TempId 1,
                RecordClone (descriptor, Var (TempId 0), [FloatLiteral 3.0; FloatLiteral 2.0]),
                Let (
                    TempId 2,
                    Atom (Var (TempId 1)),
                    Let (
                        TempId 3,
                        RecordClone (descriptor, Var (TempId 2), [FloatLiteral 4.0; FloatLiteral 2.0]),
                        Return (Var (TempId 3))
                    )
                )
            )
        )
        |> optimizeBody
    match aggregateAllocationCount body with
    | 1 -> Ok ()
    | count -> Error $"Expected Float clone aliases to scalarize before the escaping allocation, got {count}"

let testFloatRecordProjectionBeforeCloneScalarizesSource () : TestResult =
    let descriptor = pointDescriptor AST.TFloat64
    let body =
        Let (
            TempId 0,
            RecordAlloc (descriptor, [FloatLiteral 1.0; FloatLiteral 2.0]),
            Let (
                TempId 1,
                RecordGet (descriptor, Var (TempId 0), 0),
                Let (
                    TempId 2,
                    RecordClone (descriptor, Var (TempId 0), [FloatLiteral 3.0; Var (TempId 1)]),
                    Return (Var (TempId 2))
                )
            )
        )
        |> optimizeBody
    if aggregateAllocationCount body = 1 && not (containsRecordReuse body) then Ok ()
    else Error $"Expected the projected Float clone source to be scalar-replaced, got {body}"

let testFloatRecordUseAfterCloneRetainsEscapingSource () : TestResult =
    let descriptor = pointDescriptor AST.TFloat64
    let body =
        Let (
            TempId 0,
            RecordAlloc (descriptor, [FloatLiteral 1.0; FloatLiteral 2.0]),
            Let (
                TempId 1,
                RecordClone (descriptor, Var (TempId 0), [FloatLiteral 3.0; FloatLiteral 2.0]),
                Return (Var (TempId 0))
            )
        )
        |> optimizeBody
    if aggregateAllocationCount body = 1 && not (containsRecordReuse body) then Ok ()
    else Error $"Expected only the escaping Float source allocation to remain, got {body}"

let testFloatRecordCallBeforeCloneRejectsReuse () : TestResult =
    let descriptor = pointDescriptor AST.TFloat64
    let body =
        Let (
            TempId 0,
            RecordAlloc (descriptor, [FloatLiteral 1.0; FloatLiteral 2.0]),
            Let (
                TempId 1,
                Call (fid "observe", [Var (TempId 0)]),
                Let (
                    TempId 2,
                    RecordClone (descriptor, Var (TempId 0), [FloatLiteral 3.0; FloatLiteral 2.0]),
                    Return (Var (TempId 2))
                )
            )
        )
        |> optimizeBody
    if aggregateAllocationCount body = 2 && not (containsRecordReuse body) then Ok ()
    else Error $"Expected a call before a Float clone to reject reuse, got {body}"

let testCompositeManagedRecordsReuseUniqueAllocations () : TestResult =
    let optimized fieldType =
        let descriptor =
            { pointDescriptor AST.TFloat64 with
                Fields = ["x", AST.TFloat64; "items", fieldType] }
        Let (
            TempId 4,
            Call (fid "makeItems", []),
            Let (
                TempId 0,
                RecordAlloc (descriptor, [FloatLiteral 1.0; Var (TempId 4)]),
                Let (
                    TempId 1,
                    RecordClone (descriptor, Var (TempId 0), [FloatLiteral 2.0; Var (TempId 4)]),
                    Return (Var (TempId 1))
                )
            )
        )
        |> optimizeBody
    let samples = [
        AST.TTuple [AST.TString; AST.TInt64]
        AST.TList AST.TInt64
        AST.TDict (AST.TString, AST.TInt64)
    ]
    samples
    |> List.tryPick (fun fieldType ->
        let body = optimized fieldType
        if aggregateAllocationCount body = 1 && containsRecordReuse body then None
        else Some (fieldType, body))
    |> function
       | None -> Ok ()
       | Some (fieldType, body) ->
           Error $"Expected the unique {fieldType} record allocation to be reused, got {body}"

let testUnsupportedCompositeRecordsRejectReuse () : TestResult =
    let optimized fieldType =
        let descriptor =
            { pointDescriptor AST.TFloat64 with
                Fields = ["x", AST.TFloat64; "items", fieldType] }
        Let (
            TempId 4,
            Call (fid "makeManaged", []),
            Let (
                TempId 0,
                RecordAlloc (descriptor, [FloatLiteral 1.0; Var (TempId 4)]),
                Let (
                    TempId 1,
                    RecordClone (descriptor, Var (TempId 0), [FloatLiteral 2.0; Var (TempId 4)]),
                    Return (Var (TempId 1))
                )
            )
        )
        |> optimizeBody
    let samples = [
        AST.TStream AST.TInt64
        AST.TList (AST.TStream AST.TInt64)
        AST.TFunction ([], AST.TInt64)
        AST.TSum ("Maybe", [AST.TInt64])
        AST.TVar "unknown"
    ]
    samples
    |> List.tryPick (fun fieldType ->
        let body = optimized fieldType
        if aggregateAllocationCount body = 2 && not (containsRecordReuse body) then None
        else Some (fieldType, body))
    |> function
       | None -> Ok ()
       | Some (fieldType, body) ->
           Error $"Expected unsupported {fieldType} destruction to reject reuse, got {body}"

let testNestedRecordsReuseOnlyWithSafeInstantiatedFields () : TestResult =
    let optimized typeReg fieldType =
        let descriptor =
            { pointDescriptor AST.TFloat64 with
                Fields = ["x", AST.TFloat64; "nested", fieldType] }
        Let (
            TempId 4,
            Call (fid "makeNested", []),
            Let (
                TempId 0,
                RecordAlloc (descriptor, [FloatLiteral 1.0; Var (TempId 4)]),
                Let (
                    TempId 1,
                    RecordClone (descriptor, Var (TempId 0), [FloatLiteral 2.0; Var (TempId 4)]),
                    Return (Var (TempId 1))
                )
            )
        )
        |> optimizeBodyWithTypes typeReg
    let typeReg : TypeRegistries.TypeRegistry =
        Map.ofList [
            ("NestedSafe",
             { TypeParams = []
               Fields = ["items", AST.TList AST.TInt64] })
            ("NestedGeneric",
             { TypeParams = ["a"]
               Fields = ["value", AST.TVar "a"] })
            ("NestedStream",
             { TypeParams = []
               Fields = ["events", AST.TStream AST.TInt64] })
            ("NestedRecursive",
             { TypeParams = []
               Fields = ["next", AST.TRecord ("NestedRecursive", [])] })
            ("NestedGrowing",
             { TypeParams = ["a"]
               Fields = [
                   "next",
                   AST.TRecord ("NestedGrowing", [AST.TList (AST.TVar "a")])
               ] })
        ]
    let samples = [
        (AST.TRecord ("NestedSafe", []), true)
        (AST.TRecord ("NestedGeneric", [AST.TString]), true)
        (AST.TRecord ("NestedGeneric", [AST.TStream AST.TInt64]), false)
        (AST.TRecord ("NestedStream", []), false)
        (AST.TRecord ("NestedRecursive", []), true)
        (AST.TRecord ("NestedGrowing", [AST.TString]), false)
        (AST.TRecord ("Missing", []), false)
    ]
    samples
    |> List.tryPick (fun (fieldType, shouldReuse) ->
        let body = optimized typeReg fieldType
        let reused = aggregateAllocationCount body = 1 && containsRecordReuse body
        let rejected = aggregateAllocationCount body = 2 && not (containsRecordReuse body)
        if (shouldReuse && reused) || (not shouldReuse && rejected) then None
        else Some (fieldType, shouldReuse, body))
    |> function
       | None -> Ok ()
       | Some (fieldType, shouldReuse, body) ->
           Error $"Expected nested {fieldType} reuse={shouldReuse}, got {body}"

let testBoxedSumConstructorReusesUniqueAllocation () : TestResult =
    let sourceDescriptor = {
        SourceTypeName = "ReuseBox"
        RuntimeTypeName = "ReuseBox"
        TypeArgs = []
        Fields = ["$tag", AST.TInt64; "$payload", AST.TList AST.TInt64]
        ValueType = AST.TSum ("ReuseBox", [])
    }
    let targetDescriptor = {
        sourceDescriptor with
            Fields = ["$tag", AST.TInt64; "$payload", AST.TString]
    }
    let body =
        Let (
            TempId 4,
            Call (fid "makeOldItems", []),
            Let (
                TempId 0,
                RecordAlloc (sourceDescriptor, [IntLiteral (Int64 0L); Var (TempId 4)]),
                Let (
                    TempId 1,
                    TupleGet (Var (TempId 0), 1),
                    Let (
                        TempId 5,
                        Call (fid "makeNewLabel", []),
                        Let (
                            TempId 2,
                            RecordAlloc (targetDescriptor, [IntLiteral (Int64 1L); Var (TempId 5)]),
                            Return (Var (TempId 2))
                        )
                    )
                )
            )
        )
        |> optimizeBody
    if aggregateAllocationCount body = 1 && containsRecordReuse body then Ok ()
    else Error $"Expected the unique boxed-sum allocation to be reused, got {body}"

let testBoxedSumReuseRejectsObservablePayloads () : TestResult =
    let optimized sourcePayloadType targetPayloadType =
        let sourceDescriptor = {
            SourceTypeName = "UnsafeReuseBox"
            RuntimeTypeName = "UnsafeReuseBox"
            TypeArgs = []
            Fields = ["$tag", AST.TInt64; "$payload", sourcePayloadType]
            ValueType = AST.TSum ("UnsafeReuseBox", [])
        }
        let targetDescriptor = {
            sourceDescriptor with
                Fields = ["$tag", AST.TInt64; "$payload", targetPayloadType]
        }
        Let (
            TempId 4,
            Call (fid "makeOldPayload", []),
            Let (
                TempId 0,
                RecordAlloc (sourceDescriptor, [IntLiteral (Int64 0L); Var (TempId 4)]),
                Let (
                    TempId 1,
                    TupleGet (Var (TempId 0), 1),
                    Let (
                        TempId 5,
                        Call (fid "makeNewPayload", []),
                        Let (
                            TempId 2,
                            RecordAlloc (targetDescriptor, [IntLiteral (Int64 1L); Var (TempId 5)]),
                            Return (Var (TempId 2))
                        )
                    )
                )
            )
        )
        |> optimizeBody
    let samples = [
        (AST.TStream AST.TInt64, AST.TList AST.TInt64)
        (AST.TList AST.TInt64, AST.TStream AST.TInt64)
        (AST.TFunction ([], AST.TInt64), AST.TString)
        (AST.TString, AST.TSum ("Nested", []))
    ]
    samples
    |> List.tryPick (fun (sourceType, targetType) ->
        let body = optimized sourceType targetType
        if aggregateAllocationCount body = 2 && not (containsRecordReuse body) then None
        else Some (sourceType, targetType, body))
    |> function
       | None -> Ok ()
       | Some (sourceType, targetType, body) ->
           Error $"Expected boxed-sum reuse from {sourceType} to {targetType} to be rejected, got {body}"

let testBoxedSumReuseRejectsSurvivingPayloadProjection () : TestResult =
    let descriptor = {
        SourceTypeName = "ProjectedReuseBox"
        RuntimeTypeName = "ProjectedReuseBox"
        TypeArgs = []
        Fields = ["$tag", AST.TInt64; "$payload", AST.TList AST.TInt64]
        ValueType = AST.TSum ("ProjectedReuseBox", [])
    }
    let body =
        Let (
            TempId 4,
            Call (fid "makeOldItems", []),
            Let (
                TempId 0,
                RecordAlloc (descriptor, [IntLiteral (Int64 0L); Var (TempId 4)]),
                Let (
                    TempId 1,
                    TupleGet (Var (TempId 0), 1),
                    Let (
                        TempId 5,
                        Call (fid "makeNewItems", []),
                        Let (
                            TempId 2,
                            RecordAlloc (descriptor, [IntLiteral (Int64 0L); Var (TempId 5)]),
                            Let (
                                TempId 6,
                                Call (fid "consumeOldItems", [Var (TempId 1)]),
                                Return (Var (TempId 2))
                            )
                        )
                    )
                )
            )
        )
        |> optimizeBody
    if aggregateAllocationCount body = 2 && not (containsRecordReuse body) then Ok ()
    else Error $"Expected a surviving boxed-sum payload projection to reject reuse, got {body}"

let testManagedLeafRecordReusesUniqueAllocation () : TestResult =
    let descriptor =
        { pointDescriptor AST.TString with
            Fields = ["label", AST.TString; "count", AST.TInt64] }
    let body =
        Let (
            TempId 0,
            RecordAlloc (descriptor, [StringLiteral "old"; IntLiteral (Int64 1L)]),
            Let (
                TempId 1,
                RecordClone (descriptor, Var (TempId 0), [StringLiteral "new"; IntLiteral (Int64 2L)]),
                Return (Var (TempId 1))
            )
        )
        |> optimizeBody
    if aggregateAllocationCount body = 1 && containsRecordReuse body then Ok ()
    else Error $"Expected the unique managed-leaf record allocation to be reused, got {body}"

let testFloatRecordAliasUseAfterCloneRetainsEscapingSource () : TestResult =
    let descriptor = pointDescriptor AST.TFloat64
    let body =
        Let (
            TempId 0,
            RecordAlloc (descriptor, [FloatLiteral 1.0; FloatLiteral 2.0]),
            Let (
                TempId 1,
                Atom (Var (TempId 0)),
                Let (
                    TempId 2,
                    RecordClone (descriptor, Var (TempId 1), [FloatLiteral 3.0; FloatLiteral 2.0]),
                    Return (Var (TempId 0))
                )
            )
        )
        |> optimizeBody
    if aggregateAllocationCount body = 1 && not (containsRecordReuse body) then Ok ()
    else Error $"Expected only the escaping aliased Float source allocation to remain, got {body}"

let testFloatRecordBranchClonesScalarizeSharedSource () : TestResult =
    let descriptor = pointDescriptor AST.TFloat64
    let body =
        Let (
            TempId 0,
            RecordAlloc (descriptor, [FloatLiteral 1.0; FloatLiteral 2.0]),
            If (
                BoolLiteral true,
                Let (
                    TempId 1,
                    RecordClone (descriptor, Var (TempId 0), [FloatLiteral 3.0; FloatLiteral 2.0]),
                    Return (Var (TempId 1))
                ),
                Let (
                    TempId 2,
                    RecordClone (descriptor, Var (TempId 0), [FloatLiteral 4.0; FloatLiteral 2.0]),
                    Return (Var (TempId 2))
                )
            )
        )
        |> optimizeBody
    if aggregateAllocationCount body = 2 && not (containsRecordReuse body) then Ok ()
    else Error $"Expected the shared Float source to be scalar-replaced before branch clones, got {body}"

let tests =
    [ ("Scalar record projection removes allocation", testScalarRecordProjectionRemovesAllocation)
      ("Projection-only aggregate alias removes allocation", testAggregateAliasRemovesAllocation)
      ("Scalar record clone chain removes allocations", testScalarRecordCloneChainRemovesAllocations)
      ("Escaping clone retains only clone allocation", testEscapingCloneRetainsOnlyCloneAllocation)
      ("Returned record preserves allocation", testReturnedRecordPreservesAllocation)
      ("Record passed to call preserves allocation", testRecordPassedToCallPreservesAllocation)
      ("Closure-captured record preserves allocation", testRecordCapturedByClosurePreservesAllocation)
      ("Branch-local record projections remove allocation", testBranchLocalProjectionsRemoveAllocation)
      ("Managed record preserves allocation", testManagedRecordPreservesAllocation)
      ("Float record is scalar-replaced", testFloatRecordIsScalarReplaced)
      ("Float record clone scalarizes source allocation", testFloatRecordCloneScalarizesSourceAllocation)
      ("Float record clone reuses non-scalarized source allocation", testFloatRecordCloneReusesNonScalarizedSourceAllocation)
      ("Float record clone alias chain scalarizes intermediates", testFloatRecordCloneAliasChainScalarizesIntermediates)
      ("Float record projection before clone scalarizes source", testFloatRecordProjectionBeforeCloneScalarizesSource)
      ("Float record use after clone retains escaping source", testFloatRecordUseAfterCloneRetainsEscapingSource)
      ("Float record call before clone rejects reuse", testFloatRecordCallBeforeCloneRejectsReuse)
      ("Composite managed records reuse unique allocations", testCompositeManagedRecordsReuseUniqueAllocations)
      ("Unsupported composite records reject reuse", testUnsupportedCompositeRecordsRejectReuse)
      ("Nested records reuse only with safe instantiated fields", testNestedRecordsReuseOnlyWithSafeInstantiatedFields)
      ("Boxed sum constructor reuses unique allocation", testBoxedSumConstructorReusesUniqueAllocation)
      ("Boxed sum reuse rejects observable payloads", testBoxedSumReuseRejectsObservablePayloads)
      ("Boxed sum reuse rejects surviving payload projection", testBoxedSumReuseRejectsSurvivingPayloadProjection)
      ("Managed leaf record reuses unique allocation", testManagedLeafRecordReusesUniqueAllocation)
      ("Float record alias use after clone retains escaping source", testFloatRecordAliasUseAfterCloneRetainsEscapingSource)
      ("Float record branch clones scalarize shared source", testFloatRecordBranchClonesScalarizeSharedSource) ]
