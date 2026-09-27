// ReleasePlanSummary.fs - Summarize recursive release plans and required runtime helpers.

module ARM64ReleasePlanSummary

open ARM64CodeGenTypes
open ARM64ClosureReferenceCounts
open ARM64ReleaseSelection

// Plan the expensive, registry-independent portion of ARM64 RC helper
// selection once per finalized LIR function. The result is carried through
// tree shaking and merged only when the final compilation unit is assembled.
let private precomputedEmptyReleasePlanSummary : RcReleasePlanSummary = {
    ListDecHelperLabels = Set.empty
    PlannedListDecHelpers = Map.empty
    ExpensiveGenericDecHelper = None
    DictDecHelperLabels = Set.empty
    PlannedDictDecHelpers = Map.empty
    NeedsClosureRcDecHelper = false
    NeedsStreamRcDecHelper = false
}

let private mergePrecomputedPlannedListHelpers
    (left: Map<string, int * MemoryModel.RcReleasePlan>)
    (right: Map<string, int * MemoryModel.RcReleasePlan>)
    : Map<string, int * MemoryModel.RcReleasePlan> =
    Map.fold
        (fun acc label spec ->
            match Map.tryFind label acc with
            | Some existing when existing <> spec ->
                Crash.crash $"planned list RC helper label collision for {label}"
            | Some _ -> acc
            | None -> Map.add label spec acc)
        left
        right

let private mergePrecomputedPlannedGenericHelpers
    (left: Map<string, LIR.Arm64PlannedGenericDecHelper>)
    (right: Map<string, LIR.Arm64PlannedGenericDecHelper>)
    : Map<string, LIR.Arm64PlannedGenericDecHelper> =
    Map.fold
        (fun acc label (spec: LIR.Arm64PlannedGenericDecHelper) ->
            match Map.tryFind label acc with
            | Some (existing: LIR.Arm64PlannedGenericDecHelper)
                when existing.PayloadSize <> spec.PayloadSize
                     || existing.ReleasePlan <> spec.ReleasePlan
                     || existing.OwnsSinglePayloadSum <> spec.OwnsSinglePayloadSum ->
                Crash.crash $"planned generic RC helper label collision for {label}"
            | Some (existing: LIR.Arm64PlannedGenericDecHelper) ->
                Map.add
                    label
                    { existing with
                        ReleasePlanMemoKeys =
                            Set.union
                                existing.ReleasePlanMemoKeys
                                spec.ReleasePlanMemoKeys }
                    acc
            | None -> Map.add label spec acc)
        left
        right

let private mergePrecomputedPlannedDictHelpers
    (left: Map<string, MemoryModel.RcReleasePlan>)
    (right: Map<string, MemoryModel.RcReleasePlan>)
    : Map<string, MemoryModel.RcReleasePlan> =
    Map.fold
        (fun acc label plan ->
            match Map.tryFind label acc with
            | Some existing when existing <> plan ->
                Crash.crash $"planned dict RC helper label collision for {label}"
            | Some _ -> acc
            | None -> Map.add label plan acc)
        left
        right

let private addPrecomputedPlannedListHelper
    payloadSize
    elementFingerprint
    elementRelease
    (summary: RcReleasePlanSummary)
    : RcReleasePlanSummary =
    let label = plannedListDecHelperLabelForFingerprint elementFingerprint
    match Map.tryFind label summary.PlannedListDecHelpers with
    | Some existing when existing <> (payloadSize, elementRelease) ->
        Crash.crash $"planned list RC helper label collision for {label}"
    | Some _ -> summary
    | None ->
        { summary with
            PlannedListDecHelpers =
                Map.add label (payloadSize, elementRelease) summary.PlannedListDecHelpers }

let private addPrecomputedPlannedDictHelper
    releasePlanFingerprint
    releasePlan
    (summary: RcReleasePlanSummary)
    : RcReleasePlanSummary =
    let label =
        dictDecHelperForReleasePlanWithFingerprint
            releasePlanFingerprint
            releasePlan
    match Map.tryFind label summary.PlannedDictDecHelpers with
    | Some existing when existing <> releasePlan ->
        Crash.crash $"planned dict RC helper label collision for {label}"
    | Some _ -> summary
    | None ->
        { summary with
            PlannedDictDecHelpers =
                Map.add label releasePlan summary.PlannedDictDecHelpers }

let private addPrecomputedPlannedGenericHelper
    ownsSinglePayloadSum
    memoKey
    baseLabel
    payloadSize
    releasePlan
    (requirements: RcHelperRequirements)
    : RcHelperRequirements =
    let label =
        specializePlannedGenericDecHelperLabel ownsSinglePayloadSum baseLabel
    let spec : LIR.Arm64PlannedGenericDecHelper = {
        ReleasePlanMemoKeys = Set.singleton memoKey
        PayloadSize = payloadSize
        ReleasePlan = releasePlan
        OwnsSinglePayloadSum = ownsSinglePayloadSum
    }
    match Map.tryFind label requirements.PlannedGenericDecHelpers with
    | Some existing
        when existing.PayloadSize <> spec.PayloadSize
             || existing.ReleasePlan <> spec.ReleasePlan
             || existing.OwnsSinglePayloadSum <> spec.OwnsSinglePayloadSum ->
        Crash.crash $"planned generic RC helper label collision for {label}"
    | Some existing ->
        { requirements with
            PlannedGenericDecHelpers =
                requirements.PlannedGenericDecHelpers
                |> Map.add
                    label
                    { existing with
                        ReleasePlanMemoKeys =
                            Set.union
                                existing.ReleasePlanMemoKeys
                                spec.ReleasePlanMemoKeys } }
    | None ->
        { requirements with
            PlannedGenericDecHelpers =
                Map.add label spec requirements.PlannedGenericDecHelpers }

let rec private collectPrecomputedReleasePlanSummary
    (includeStaticRootDependencies: bool)
    (collectListLabels: bool)
    (collectPlannedListHelpers: bool)
    (collectDictLabels: bool)
    (collectPlannedDictHelpers: bool)
    (collectClosureNeed: bool)
    (summary: RcReleasePlanSummary)
    (releasePlan: MemoryModel.RcReleasePlan)
    : RcReleasePlanSummary * uint64 =
    let summary =
        match releasePlan with
        | MemoryModel.RootRelease (_, MemoryModel.ClosureHeap, _) when collectClosureNeed ->
            { summary with NeedsClosureRcDecHelper = true }
        | MemoryModel.RootRelease (_, MemoryModel.StreamHeap, _) ->
            { summary with NeedsStreamRcDecHelper = true }
        | _ -> summary

    let collectFields
        childListLabels
        childPlannedListHelpers
        childDictLabels
        childPlannedDictHelpers
        childClosureNeed
        initialSummary
        fieldReleases =
        fieldReleases
        |> List.fold
            (fun (summary, childFingerprintsRev) (MemoryModel.FieldRelease (_, fieldReleasePlan)) ->
                let nextSummary, childFingerprint =
                    collectPrecomputedReleasePlanSummary
                        includeStaticRootDependencies
                        childListLabels
                        childPlannedListHelpers
                        childDictLabels
                        childPlannedDictHelpers
                        childClosureNeed
                        summary
                        fieldReleasePlan
                (nextSummary, childFingerprint :: childFingerprintsRev))
            (initialSummary, [])
        |> fun (collectedSummary, childFingerprintsRev) ->
            (collectedSummary, List.rev childFingerprintsRev)

    match releasePlan with
    | MemoryModel.RootRelease (_, _, MemoryModel.TaggedListPayloadRelease elementRelease) ->
        let summary, elementFingerprint =
            collectPrecomputedReleasePlanSummary
                includeStaticRootDependencies
                collectListLabels
                collectPlannedListHelpers
                collectDictLabels
                collectPlannedDictHelpers
                false
                summary
                elementRelease
        let fingerprint =
            ReleasePlanFingerprint.rcReleasePlanFingerprintHashFromChildren
                releasePlan
                [elementFingerprint]
        let elementFingerprintString =
            ReleasePlanFingerprint.rcReleasePlanFingerprintString elementFingerprint
        let summary =
            if collectListLabels then
                { summary with
                    ListDecHelperLabels =
                        Set.add
                            (listDecHelperForElementRelease
                                elementFingerprintString
                                elementRelease)
                            summary.ListDecHelperLabels }
            else summary
        let summary =
            match collectPlannedListHelpers, elementRelease with
            | true, MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
                addPrecomputedPlannedListHelper
                    8
                    elementFingerprintString
                    elementRelease
                    summary
            | true, MemoryModel.RootRelease (payloadSize, MemoryModel.GenericHeap, _)
            | true, MemoryModel.RootRelease (payloadSize, MemoryModel.StreamHeap, _) ->
                addPrecomputedPlannedListHelper
                    payloadSize
                    elementFingerprintString
                    elementRelease
                    summary
            | true, MemoryModel.RootRelease (
                  payloadSize,
                  MemoryModel.DictHeap,
                  MemoryModel.DictPayloadRelease (MemoryModel.DynamicBufferRelease _, _))
            | true, MemoryModel.RootRelease (
                  payloadSize,
                  MemoryModel.DictHeap,
                  MemoryModel.DictPayloadRelease (_, MemoryModel.DynamicBufferRelease _)) ->
                addPrecomputedPlannedListHelper
                    payloadSize
                    elementFingerprintString
                    elementRelease
                    summary
            | true, MemoryModel.RecursiveRelease _ ->
                addPrecomputedPlannedListHelper
                    8
                    elementFingerprintString
                    elementRelease
                    summary
            | _ -> summary
        (summary, fingerprint)
    | MemoryModel.RootRelease (_, kind, MemoryModel.DictPayloadRelease (keyRelease, valueRelease)) ->
        if collectPlannedDictHelpers && kind <> MemoryModel.DictHeap then
            Crash.crash $"ARM64 planned dict dependency collection saw DictPayloadRelease for non-dict kind {kind}"
        else
            let childDictLabels =
                collectDictLabels
                && (includeStaticRootDependencies || kind <> MemoryModel.DictHeap)
            let collectChild summary childRelease =
                collectPrecomputedReleasePlanSummary
                    includeStaticRootDependencies
                    collectListLabels
                    collectPlannedListHelpers
                    childDictLabels
                    collectPlannedDictHelpers
                    false
                    summary
                    childRelease
            let summary, keyFingerprint = collectChild summary keyRelease
            let summary, valueFingerprint = collectChild summary valueRelease
            let fingerprint =
                ReleasePlanFingerprint.rcReleasePlanFingerprintHashFromChildren
                    releasePlan
                    [keyFingerprint; valueFingerprint]
            let fingerprintString =
                ReleasePlanFingerprint.rcReleasePlanFingerprintString fingerprint
            let summary =
                if collectDictLabels && kind = MemoryModel.DictHeap then
                    { summary with
                        DictDecHelperLabels =
                            Set.add
                                (dictDecHelperForReleasePlanWithFingerprint
                                    fingerprintString
                                    releasePlan)
                                summary.DictDecHelperLabels }
                else summary
            let summary =
                if collectPlannedDictHelpers
                   && dictPayloadReleaseNeedsPlannedHelper keyRelease valueRelease then
                    addPrecomputedPlannedDictHelper
                        fingerprintString
                        releasePlan
                        summary
                else summary
            (summary, fingerprint)
    | MemoryModel.RootRelease (_, kind, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases)) ->
        let collectStaticOrGeneric = includeStaticRootDependencies || kind = MemoryModel.GenericHeap
        let summary, childFingerprints =
            collectFields
                (collectListLabels && collectStaticOrGeneric)
                (collectPlannedListHelpers && kind = MemoryModel.GenericHeap)
                (collectDictLabels && collectStaticOrGeneric)
                (collectPlannedDictHelpers && kind = MemoryModel.GenericHeap)
                (collectClosureNeed && kind = MemoryModel.GenericHeap)
                summary
                fieldReleases
        (summary,
         ReleasePlanFingerprint.rcReleasePlanFingerprintHashFromChildren
             releasePlan
             childFingerprints)
    | MemoryModel.RootRelease (_, kind, MemoryModel.BoxedSumPayloadRelease (_, fieldReleases, variants)) ->
        let collectStaticOrGeneric = includeStaticRootDependencies || kind = MemoryModel.GenericHeap
        let summary, fieldFingerprints =
            collectFields
                (collectListLabels && collectStaticOrGeneric)
                (collectPlannedListHelpers && kind = MemoryModel.GenericHeap)
                (collectDictLabels && collectStaticOrGeneric)
                (collectPlannedDictHelpers && kind = MemoryModel.GenericHeap)
                (collectClosureNeed && kind = MemoryModel.GenericHeap)
                summary
                fieldReleases
        // Variant-specific fields are already represented by the combined
        // release fields above. They still contribute to the stable helper
        // identity, so fingerprint their disjoint subtrees without collecting
        // the same requirements twice.
        let variantFingerprints =
            variants
            |> List.collect (fun variant ->
                variant.FieldReleases
                |> List.map (fun (MemoryModel.FieldRelease (_, childReleasePlan)) ->
                    ReleasePlanFingerprint.rcReleasePlanFingerprintHash childReleasePlan))
        (summary,
         ReleasePlanFingerprint.rcReleasePlanFingerprintHashFromChildren
             releasePlan
             (fieldFingerprints @ variantFingerprints))
    | MemoryModel.RootRelease (_, _, MemoryModel.ClosurePayloadRelease fieldReleases) ->
        let summary, childFingerprints =
            collectFields
                collectListLabels
                collectPlannedListHelpers
                collectDictLabels
                collectPlannedDictHelpers
                false
                summary
                fieldReleases
        (summary,
         ReleasePlanFingerprint.rcReleasePlanFingerprintHashFromChildren
             releasePlan
             childFingerprints)
    | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _)
        when collectDictLabels && not includeStaticRootDependencies ->
        let fingerprint = ReleasePlanFingerprint.rcReleasePlanFingerprintHash releasePlan
        let fingerprintString = ReleasePlanFingerprint.rcReleasePlanFingerprintString fingerprint
        ({ summary with
            DictDecHelperLabels =
                Set.add
                    (dictDecHelperForReleasePlanWithFingerprint
                        fingerprintString
                        releasePlan)
                    summary.DictDecHelperLabels },
         fingerprint)
    | _ ->
        (summary,
         ReleasePlanFingerprint.rcReleasePlanFingerprintHashFromChildren releasePlan [])

let internal summarizePrecomputedReleasePlan includeStaticRootDependencies releasePlan =
    let summary, releasePlanFingerprint =
        collectPrecomputedReleasePlanSummary
            includeStaticRootDependencies
            true
            true
            true
            true
            true
            precomputedEmptyReleasePlanSummary
            releasePlan
    match releasePlan with
    | MemoryModel.RootRelease (
          payloadSize,
          MemoryModel.GenericHeap,
          (MemoryModel.FixedBlockPayloadRelease _ | MemoryModel.BoxedSumPayloadRelease _))
        when genericReleasePlanIsExpensive releasePlan ->
        { summary with
            ExpensiveGenericDecHelper =
                Some (
                    plannedGenericDecHelperBaseLabelForFingerprint
                        (ReleasePlanFingerprint.rcReleasePlanFingerprintString releasePlanFingerprint),
                    payloadSize,
                    releasePlan) }
    | _ ->
        summary

let internal precomputedEmptyRcHelperRequirements : RcHelperRequirements = {
    ListDecHelperLabels = Set.empty
    PlannedListDecHelpers = Map.empty
    PlannedGenericDecHelpers = Map.empty
    PlannedDictDecHelpers = Map.empty
    DictDecHelperLabels = Set.empty
    NeedsListRcIncHelper = false
    NeedsDictRcIncHelper = false
    NeedsClosureRcIncHelper = false
    NeedsClosureRcDecHelper = false
    NeedsStreamRcDecHelper = false
    ReleasePlanSummaries = Map.empty
}

let private precomputedReleasePlanSummaryWithCache
    (summaryCache: ReleasePlanSummaryCache option)
    includeStaticRootDependencies
    memoKey
    releasePlan
    (requirements: RcHelperRequirements)
    : RcReleasePlanSummary * RcHelperRequirements =
    let key = (includeStaticRootDependencies, memoKey)
    match Map.tryFind key requirements.ReleasePlanSummaries with
    | Some summary -> (summary, requirements)
    | None ->
        let generate () =
            summarizePrecomputedReleasePlan includeStaticRootDependencies releasePlan
        let summary =
            match summaryCache, memoKey with
            | Some cache, LIR.FingerprintedReleasePlan cacheKey ->
                cache includeStaticRootDependencies cacheKey releasePlan generate
            | _ ->
                generate ()
        (summary,
         { requirements with
             ReleasePlanSummaries = Map.add key summary requirements.ReleasePlanSummaries })

let internal precomputedReleasePlanSummary =
    precomputedReleasePlanSummaryWithCache None

let internal addPrecomputedReleasePlanRequirements
    (summary: RcReleasePlanSummary)
    (requirements: RcHelperRequirements)
    : RcHelperRequirements =
    { requirements with
        PlannedListDecHelpers =
            mergePrecomputedPlannedListHelpers
                requirements.PlannedListDecHelpers
                summary.PlannedListDecHelpers
        PlannedDictDecHelpers =
            mergePrecomputedPlannedDictHelpers
                requirements.PlannedDictDecHelpers
                summary.PlannedDictDecHelpers }

let private collectPrecomputedRefCountDecRequirement
    (summaryCache: ReleasePlanSummaryCache option)
    (ownsSinglePayloadSum: bool)
    (requirements: RcHelperRequirements)
    (kind, memoKey, metadata)
    : RcHelperRequirements =
    match kind with
    | LIR.TaggedList
    | LIR.DictHeap ->
        let context =
            if kind = LIR.TaggedList then "TaggedList RefCountDec helper selection"
            else "DictHeap RefCountDec helper selection"
        let releasePlan = requiredRcMetadataReleasePlan context metadata
        let summary, requirements =
            precomputedReleasePlanSummaryWithCache
                summaryCache
                false
                (LIR.rcReleasePlanMemoKey metadata)
                releasePlan
                requirements
        let requirements = addPrecomputedReleasePlanRequirements summary requirements
        if kind = LIR.TaggedList then
            { requirements with
                ListDecHelperLabels =
                    Set.add (listDecHelperForReleasePlan releasePlan) requirements.ListDecHelperLabels }
        else
            { requirements with
                DictDecHelperLabels =
                    Set.add (dictDecHelperForReleasePlan releasePlan) requirements.DictDecHelperLabels }
    | LIR.GenericHeap ->
        match rcMetadataReleasePlan metadata with
        | None -> requirements
        | Some releasePlan ->
            let summary, requirements =
                precomputedReleasePlanSummaryWithCache
                    summaryCache
                    false
                    (LIR.rcReleasePlanMemoKey metadata)
                    releasePlan
                    requirements
            let requirements = addPrecomputedReleasePlanRequirements summary requirements
            let requirements = {
                requirements with
                    ListDecHelperLabels = Set.union requirements.ListDecHelperLabels summary.ListDecHelperLabels
                    DictDecHelperLabels = Set.union requirements.DictDecHelperLabels summary.DictDecHelperLabels
                    NeedsClosureRcDecHelper = requirements.NeedsClosureRcDecHelper || summary.NeedsClosureRcDecHelper
                    NeedsStreamRcDecHelper = requirements.NeedsStreamRcDecHelper || summary.NeedsStreamRcDecHelper
            }
            match summary.ExpensiveGenericDecHelper with
            | Some (baseLabel, payloadSize, releasePlan) ->
                addPrecomputedPlannedGenericHelper
                    ownsSinglePayloadSum
                    memoKey
                    baseLabel
                    payloadSize
                    releasePlan
                    requirements
            | _ ->
                requirements
    | LIR.ClosureHeap ->
        { requirements with NeedsClosureRcDecHelper = true }
    | LIR.StreamHeap ->
        { requirements with NeedsStreamRcDecHelper = true }

let private collectPrecomputedRefCountIncRequirement
    (requirements: RcHelperRequirements)
    kind
    : RcHelperRequirements =
    match kind with
    | LIR.TaggedList -> { requirements with NeedsListRcIncHelper = true }
    | LIR.DictHeap -> { requirements with NeedsDictRcIncHelper = true }
    | LIR.ClosureHeap -> { requirements with NeedsClosureRcIncHelper = true }
    | LIR.GenericHeap
    | LIR.StreamHeap -> requirements

let internal planFunctionArm64RcRequirements
    (summaryCache: ReleasePlanSummaryCache option)
    releasePlanSummaries
    functionName
    (facts: LIR.FunctionCodegenFacts)
    =
    let initialRequirements = {
        precomputedEmptyRcHelperRequirements with
            ReleasePlanSummaries = releasePlanSummaries
    }
    facts.RefCountDecRequirements
    |> Map.fold
        (fun requirements (kind, memoKey) metadata ->
            collectPrecomputedRefCountDecRequirement
                summaryCache
                (callerOwnsSinglePayloadSum functionName)
                requirements
                (kind, memoKey, metadata))
        initialRequirements
    |> fun requirements ->
        facts.RefCountIncRequirements
        |> Set.fold collectPrecomputedRefCountIncRequirement requirements

let internal planRawSlotInitRetainTargets
    (recordRegistry: LIR.RecordRegistry)
    (sumShapeRegistry: MemoryModel.RcSumShapeRegistry)
    (facts: LIR.FunctionCodegenFacts)
    : LIR.FunctionCodegenFacts =
    let targets =
        facts.RawSlotInitTypes
        |> Set.toList
        |> List.map (fun valueType ->
            (valueType,
             slotInitRootRetainTarget
                 recordRegistry
                 sumShapeRegistry
                 valueType))
        |> Map.ofList
    { facts with Arm64RawSlotInitRetainTargets = Some targets }

let private mergePrecomputedReleasePlanSummaries left right =
    Map.fold
        (fun acc key summary ->
            match Map.tryFind key acc with
            | Some existing when existing <> summary ->
                Crash.crash "ARM64 release-plan summary mismatch"
            | Some _ -> acc
            | None -> Map.add key summary acc)
        left
        right

let internal mergePrecomputedRcHelperRequirements
    (left: RcHelperRequirements)
    (right: RcHelperRequirements)
    : RcHelperRequirements =
    {
        ListDecHelperLabels = Set.union left.ListDecHelperLabels right.ListDecHelperLabels
        PlannedListDecHelpers =
            mergePrecomputedPlannedListHelpers left.PlannedListDecHelpers right.PlannedListDecHelpers
        PlannedGenericDecHelpers =
            mergePrecomputedPlannedGenericHelpers left.PlannedGenericDecHelpers right.PlannedGenericDecHelpers
        PlannedDictDecHelpers =
            mergePrecomputedPlannedDictHelpers left.PlannedDictDecHelpers right.PlannedDictDecHelpers
        DictDecHelperLabels = Set.union left.DictDecHelperLabels right.DictDecHelperLabels
        NeedsListRcIncHelper = left.NeedsListRcIncHelper || right.NeedsListRcIncHelper
        NeedsDictRcIncHelper = left.NeedsDictRcIncHelper || right.NeedsDictRcIncHelper
        NeedsClosureRcIncHelper = left.NeedsClosureRcIncHelper || right.NeedsClosureRcIncHelper
        NeedsClosureRcDecHelper = left.NeedsClosureRcDecHelper || right.NeedsClosureRcDecHelper
        NeedsStreamRcDecHelper = left.NeedsStreamRcDecHelper || right.NeedsStreamRcDecHelper
        ReleasePlanSummaries =
            mergePrecomputedReleasePlanSummaries left.ReleasePlanSummaries right.ReleasePlanSummaries
    }
