// RefCountInsertionTests.fs - Register memory and ANF ownership test groups.

module RefCountInsertionTests

open MemoryModel
open ANF
open RcTypeFacts
open RcCleanup
open MemoryShapeTests
open RcTypeFactTests
open RcCleanupTests
open RcTransferTests
open RcCallTests
open RcJoinTests

let tests = [
    "Join cleanup releases locals at transfer and enclosing owners after continuation", testJoinCleanupPaths
    "Join verifier accepts enclosing captures and nested outward transfers", (fun () ->
        verifyJoin (
            Let (TempId 2, Atom joinValue,
                Join (joinParameter, Return (Var (TempId 2)),
                    Join ({ Id = TempId 3; Type = AST.TBool }, Jump (TempId 1, joinValue), Jump (TempId 3, BoolLiteral true))))))
    "Join verifier accepts immediate scalar block arguments", (fun () ->
        verifyJoin (
            Join (
                { Id = TempId 3; Type = AST.TInt8 },
                Return joinValue,
                Jump (TempId 3, IntLiteral (Int8 1y)))))
    "Join verifier rejects branch-local continuation capture", rejectsJoin "outside lexical scope"
        (Join (joinParameter, Return (Var (TempId 2)), Let (TempId 2, Atom joinValue, Jump (TempId 1, joinValue))))
    "Join verifier rejects parameter use in entry", rejectsJoin "outside lexical scope"
        (Join (joinParameter, Return (Var (TempId 1)), Jump (TempId 1, Var (TempId 1))))
    "Join verifier rejects recursive target", rejectsJoin "target"
        (Join (joinParameter, Jump (TempId 1, joinValue), Jump (TempId 1, joinValue)))
    "Join verifier rejects mismatched argument", rejectsJoin "expects"
        (Join (joinParameter, Return joinValue, Jump (TempId 1, BoolLiteral true)))
    "Join verifier rejects managed parameter", rejectsJoin "unsupported block argument"
        (Join ({ joinParameter with Type = AST.TString }, Return joinValue, Jump (TempId 1, StringLiteral "value")))
    "Join verifier rejects returning entry", rejectsJoin "instead of transferring"
        (Join (joinParameter, Return joinValue, Return joinValue))
    "Join verifier rejects target shadowing", rejectsJoin "shadows"
        (Let (TempId 1, Atom joinValue, Join (joinParameter, Return joinValue, Jump (TempId 1, joinValue))))
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
    ("Rc release-plan fingerprints are compositional and stable", testRcReleasePlanFingerprintIsCompositionalAndStable)
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
    ("fresh owned value transfers into raw slot", testFreshOwnedValueTransfersIntoRawSlot)
    ("raw slot retains value used after initialization", testRawSlotRetainsValueUsedAfterInitialization)
    ("raw slot retains fresh Stream value", testRawSlotRetainsFreshStreamValue)
    ("branch-local TempId reuse uses current RC type context", testBranchLocalTempReuseUsesCurrentTypeContext)
    ("returned aggregate transfers owned value through alias", testReturnedAggregateTransfersOwnedValueThroughAlias)
    ("returned aggregate transfers owned value through typed alias", testReturnedAggregateTransfersOwnedValueThroughTypedAlias)
    ("returned aggregate retains ownership-producing Stream alias", testReturnedAggregateRetainsOwnershipProducingStreamAlias)
    ("returned aggregate transfers owned value after borrowed use", testReturnedAggregateTransfersOwnedValueAfterBorrowedUse)
    ("explicit release blocks later aggregate transfer", testExplicitReleaseBlocksLaterAggregateTransfer)
    ("returned aggregate transfers owned value across branches", testReturnedAggregateTransfersOwnedValueAcrossBranches)
    ("returned aggregate requires every branch to transfer owned value", testReturnedAggregateRequiresEveryBranchToTransferOwnedValue)
    ("returned aggregate transfers nested owned aliases", testReturnedAggregateTransfersNestedOwnedAliases)
    ("returned aggregate does not transfer duplicated aliases", testReturnedAggregateDoesNotTransferDuplicatedAliases)
    ("static string binding skips no-op RC traffic", testStaticStringBindingSkipsNoOpRcTraffic)
    ("known empty-list binding skips no-op RC traffic", testKnownEmptyListBindingSkipsNoOpRcTraffic)
    ("aggregate skips retains for known non-RC sentinels", testAggregateSkipsRetainsForKnownNonRcSentinels)
    ("aggregate skips retain for conditional static string", testAggregateSkipsRetainForConditionalStaticString)
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
    ("borrowed call materializes owned local", testBorrowedCallMaterializesOwnedLocal)
    ("returned borrowed call materializes ownership", testReturnedBorrowedCallMaterializesOwnership)
    ("call returning closure gets auto-dec after use", testCallReturningClosureGetsAutoDecAfterUse)
    ("closure call returning closure gets auto-dec after use", testClosureCallReturningClosureGetsAutoDecAfterUse)
    ("pure enum binding does not get automatic dec", testPureEnumBindingDoesNotGetAutomaticDec)
    ("generic pure enum binding does not get automatic dec", testGenericPureEnumBindingDoesNotGetAutomaticDec)
    ("program RC fresh temps follow existing program temps", testProgramRcFreshTempsFollowExistingProgramTemps)
    ("program RC rejects drifted ownership contracts", testProgramRcRejectsDriftedOwnershipContract)
    ("bare sum type refs are canonicalized for RC source types", testBareSumTypeRefsAreCanonicalizedForRcSourceTypes)
]
