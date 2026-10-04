(* ReleasePlanSummary.fs - Summarize recursive release plans and required runtime helpers. *)
[@@@warning "-4"]
open ARM64CodeGenTypes
open ARM64ClosureReferenceCounts
open ARM64ReleaseSelection
open! LIR
module M=StringOrder.Map
module S=StringOrder.Set
(*
   Plan the expensive, registry-independent portion of ARM64 RC helper
   selection once per finalized LIR function. The result is carried through
   tree shaking and merged only when the final compilation unit is assembled.
*)
let precomputedEmptyReleasePlanSummary : rcReleasePlanSummary={listDecHelperLabels=S.empty;plannedListDecHelpers=M.empty;expensiveGenericDecHelper=None;dictDecHelperLabels=S.empty;plannedDictDecHelpers=M.empty;needsClosureRcDecHelper=false;needsStreamRcDecHelper=false}
let mergePrecomputedPlannedListHelpers left right=M.fold (fun label spec acc -> match M.find_opt label acc with Some existing when existing<>spec -> Crash.crash ("planned list RC helper label collision for "^label) | Some _ -> acc | None -> M.add label spec acc) right left
let mergePrecomputedPlannedGenericHelpers left right=M.fold (fun label (spec:LIR.arm64PlannedGenericDecHelper) acc -> match M.find_opt label acc with
 | Some (existing:LIR.arm64PlannedGenericDecHelper) when existing.payloadSize<>spec.payloadSize || existing.releasePlan<>spec.releasePlan || existing.ownsSinglePayloadSum<>spec.ownsSinglePayloadSum -> Crash.crash ("planned generic RC helper label collision for "^label)
 | Some existing -> M.add label {existing with releasePlanMemoKeys=LIR.RcReleasePlanMemoKeySet.union existing.releasePlanMemoKeys spec.releasePlanMemoKeys} acc
 | None -> M.add label spec acc) right left
let mergePrecomputedPlannedDictHelpers left right=M.fold (fun label plan acc -> match M.find_opt label acc with Some existing when existing<>plan -> Crash.crash ("planned dict RC helper label collision for "^label) | Some _ -> acc | None -> M.add label plan acc) right left
let addPrecomputedPlannedListHelper payloadSize elementFingerprint elementRelease (summary:rcReleasePlanSummary) : rcReleasePlanSummary=
 let label=plannedListDecHelperLabelForFingerprint elementFingerprint in
 match M.find_opt label summary.plannedListDecHelpers with
 | Some existing when existing<>(payloadSize,elementRelease) -> Crash.crash ("planned list RC helper label collision for "^label)
 | Some _ -> summary
 | None -> {summary with plannedListDecHelpers=M.add label (payloadSize,elementRelease) summary.plannedListDecHelpers}
let addPrecomputedPlannedDictHelper fingerprint plan (summary:rcReleasePlanSummary) : rcReleasePlanSummary=
 let label=dictDecHelperForReleasePlanWithFingerprint fingerprint plan in
 match M.find_opt label summary.plannedDictDecHelpers with
 | Some existing when existing<>plan -> Crash.crash ("planned dict RC helper label collision for "^label)
 | Some _ -> summary
 | None -> {summary with plannedDictDecHelpers=M.add label plan summary.plannedDictDecHelpers}
let addPrecomputedPlannedGenericHelper ownsSinglePayloadSum memoKey baseLabel payloadSize releasePlan (requirements:rcHelperRequirements) : rcHelperRequirements=
 let label=specializePlannedGenericDecHelperLabel ownsSinglePayloadSum baseLabel in
 let spec:LIR.arm64PlannedGenericDecHelper={releasePlanMemoKeys=LIR.RcReleasePlanMemoKeySet.singleton memoKey;payloadSize;releasePlan;ownsSinglePayloadSum} in
 match M.find_opt label requirements.plannedGenericDecHelpers with
 | Some existing when existing.payloadSize<>spec.payloadSize || existing.releasePlan<>spec.releasePlan || existing.ownsSinglePayloadSum<>spec.ownsSinglePayloadSum -> Crash.crash ("planned generic RC helper label collision for "^label)
 | Some existing -> {requirements with plannedGenericDecHelpers=M.add label {existing with releasePlanMemoKeys=LIR.RcReleasePlanMemoKeySet.union existing.releasePlanMemoKeys spec.releasePlanMemoKeys} requirements.plannedGenericDecHelpers}
 | None -> {requirements with plannedGenericDecHelpers=M.add label spec requirements.plannedGenericDecHelpers}
(*
   Variant-specific fields are already represented by the combined
   release fields above. They still contribute to the stable helper
   identity, so fingerprint their disjoint subtrees without collecting
   the same requirements twice.
*)
let rec collectPrecomputedReleasePlanSummary includeStaticRootDependencies collectListLabels collectPlannedListHelpers collectDictLabels collectPlannedDictHelpers collectClosureNeed (summary:rcReleasePlanSummary) releasePlan=
 let summary=match releasePlan with MemoryModel.RootRelease (_,MemoryModel.ClosureHeap,_) when collectClosureNeed -> {summary with needsClosureRcDecHelper=true} | MemoryModel.RootRelease (_,MemoryModel.StreamHeap,_) -> {summary with needsStreamRcDecHelper=true} | _ -> summary in
 let collectFields childListLabels childPlannedListHelpers childDictLabels childPlannedDictHelpers childClosureNeed initialSummary fieldReleases=
  let collectedSummary,fingerprints=List.fold_left (fun (summary,rev) (MemoryModel.FieldRelease (_,plan)) -> let next,fingerprint=collectPrecomputedReleasePlanSummary includeStaticRootDependencies childListLabels childPlannedListHelpers childDictLabels childPlannedDictHelpers childClosureNeed summary plan in next,fingerprint::rev) (initialSummary,[]) fieldReleases in collectedSummary,List.rev fingerprints
 in
 let hash children=ReleasePlanFingerprint.rcReleasePlanFingerprintHashFromChildren releasePlan children in
 match releasePlan with
 | MemoryModel.RootRelease (_,_,MemoryModel.TaggedListPayloadRelease elementRelease) ->
  let summary,elementFingerprint=collectPrecomputedReleasePlanSummary includeStaticRootDependencies collectListLabels collectPlannedListHelpers collectDictLabels collectPlannedDictHelpers false summary elementRelease in
  let fingerprint=hash [elementFingerprint] in
  let fingerprintString=ReleasePlanFingerprint.rcReleasePlanFingerprintString elementFingerprint in
  let summary=if collectListLabels then {summary with listDecHelperLabels=S.add (listDecHelperForElementRelease fingerprintString elementRelease) summary.listDecHelperLabels} else summary in
  let summary=match collectPlannedListHelpers,elementRelease with
   | true,MemoryModel.RootRelease (_,MemoryModel.TaggedList,_) -> addPrecomputedPlannedListHelper 8 fingerprintString elementRelease summary
   | true,MemoryModel.RootRelease (size,MemoryModel.GenericHeap,_) | true,MemoryModel.RootRelease (size,MemoryModel.StreamHeap,_) -> addPrecomputedPlannedListHelper size fingerprintString elementRelease summary
   | true,MemoryModel.RootRelease (size,MemoryModel.DictHeap,MemoryModel.DictPayloadRelease (MemoryModel.DynamicBufferRelease _,_)) | true,MemoryModel.RootRelease (size,MemoryModel.DictHeap,MemoryModel.DictPayloadRelease (_,MemoryModel.DynamicBufferRelease _)) -> addPrecomputedPlannedListHelper size fingerprintString elementRelease summary
   | true,MemoryModel.RecursiveRelease _ -> addPrecomputedPlannedListHelper 8 fingerprintString elementRelease summary
   | _ -> summary in summary,fingerprint
 | MemoryModel.RootRelease (_,kind,MemoryModel.DictPayloadRelease (keyRelease,valueRelease)) ->
  if collectPlannedDictHelpers && kind<>MemoryModel.DictHeap then Crash.crash ("ARM64 planned dict dependency collection saw DictPayloadRelease for non-dict kind "^(match kind with MemoryModel.GenericHeap->"GenericHeap"|MemoryModel.StreamHeap->"StreamHeap"|MemoryModel.TaggedList->"TaggedList"|MemoryModel.DictHeap->"DictHeap"|MemoryModel.ClosureHeap->"ClosureHeap")) else
  let childDictLabels=collectDictLabels && (includeStaticRootDependencies || kind<>MemoryModel.DictHeap) in
  let collectChild summary child=collectPrecomputedReleasePlanSummary includeStaticRootDependencies collectListLabels collectPlannedListHelpers childDictLabels collectPlannedDictHelpers false summary child in
  let summary,keyFingerprint=collectChild summary keyRelease in
  let summary,valueFingerprint=collectChild summary valueRelease in
  let fingerprint=hash [keyFingerprint;valueFingerprint] in
  let fingerprintString=ReleasePlanFingerprint.rcReleasePlanFingerprintString fingerprint in
  let summary=if collectDictLabels && kind=MemoryModel.DictHeap then {summary with dictDecHelperLabels=S.add (dictDecHelperForReleasePlanWithFingerprint fingerprintString releasePlan) summary.dictDecHelperLabels} else summary in
  let summary=if collectPlannedDictHelpers && dictPayloadReleaseNeedsPlannedHelper keyRelease valueRelease then addPrecomputedPlannedDictHelper fingerprintString releasePlan summary else summary in summary,fingerprint
 | MemoryModel.RootRelease (_,kind,MemoryModel.FixedBlockPayloadRelease (_,fields)) ->
  let collectStaticOrGeneric=includeStaticRootDependencies || kind=MemoryModel.GenericHeap in
  let summary,fingerprints=collectFields (collectListLabels && collectStaticOrGeneric) (collectPlannedListHelpers && kind=MemoryModel.GenericHeap) (collectDictLabels && collectStaticOrGeneric) (collectPlannedDictHelpers && kind=MemoryModel.GenericHeap) (collectClosureNeed && kind=MemoryModel.GenericHeap) summary fields in summary,hash fingerprints
 | MemoryModel.RootRelease (_,kind,MemoryModel.BoxedSumPayloadRelease (_,fields,variants)) ->
  let collectStaticOrGeneric=includeStaticRootDependencies || kind=MemoryModel.GenericHeap in
  let summary,fingerprints=collectFields (collectListLabels && collectStaticOrGeneric) (collectPlannedListHelpers && kind=MemoryModel.GenericHeap) (collectDictLabels && collectStaticOrGeneric) (collectPlannedDictHelpers && kind=MemoryModel.GenericHeap) (collectClosureNeed && kind=MemoryModel.GenericHeap) summary fields in
  let variantFingerprints=List.concat_map (fun (variant:MemoryModel.rcBoxedSumVariantRelease) -> List.map (fun (MemoryModel.FieldRelease (_,plan)) -> ReleasePlanFingerprint.rcReleasePlanFingerprintHash plan) variant.MemoryModel.fieldReleases) variants in summary,hash (fingerprints@variantFingerprints)
 | MemoryModel.RootRelease (_,_,MemoryModel.ClosurePayloadRelease fields) -> let summary,fingerprints=collectFields collectListLabels collectPlannedListHelpers collectDictLabels collectPlannedDictHelpers false summary fields in summary,hash fingerprints
 | MemoryModel.RootRelease (_,MemoryModel.DictHeap,_) when collectDictLabels && not includeStaticRootDependencies -> let fingerprint=ReleasePlanFingerprint.rcReleasePlanFingerprintHash releasePlan in let fingerprintString=ReleasePlanFingerprint.rcReleasePlanFingerprintString fingerprint in {summary with dictDecHelperLabels=S.add (dictDecHelperForReleasePlanWithFingerprint fingerprintString releasePlan) summary.dictDecHelperLabels},fingerprint
 | _ -> summary,hash []
let summarizePrecomputedReleasePlan includeStaticRootDependencies releasePlan=
 let summary,fingerprint=collectPrecomputedReleasePlanSummary includeStaticRootDependencies true true true true true precomputedEmptyReleasePlanSummary releasePlan in
 match releasePlan with
 | MemoryModel.RootRelease (size,MemoryModel.GenericHeap,(MemoryModel.FixedBlockPayloadRelease _ | MemoryModel.BoxedSumPayloadRelease _)) when genericReleasePlanIsExpensive releasePlan -> {summary with expensiveGenericDecHelper=Some (plannedGenericDecHelperBaseLabelForFingerprint (ReleasePlanFingerprint.rcReleasePlanFingerprintString fingerprint),size,releasePlan)}
 | _ -> summary
let precomputedEmptyRcHelperRequirements : rcHelperRequirements={listDecHelperLabels=S.empty;plannedListDecHelpers=M.empty;plannedGenericDecHelpers=M.empty;plannedDictDecHelpers=M.empty;dictDecHelperLabels=S.empty;needsListRcIncHelper=false;needsDictRcIncHelper=false;needsClosureRcIncHelper=false;needsClosureRcDecHelper=false;needsStreamRcDecHelper=false;releasePlanSummaries=LIR.ReleasePlanSummaryMap.empty}
let precomputedReleasePlanSummaryWithCache summaryCache includeStaticRootDependencies memoKey releasePlan (requirements:rcHelperRequirements)=
 let key=includeStaticRootDependencies,memoKey in match LIR.ReleasePlanSummaryMap.find_opt key requirements.releasePlanSummaries with
 | Some summary -> summary,requirements
 | None -> let generate ()=summarizePrecomputedReleasePlan includeStaticRootDependencies releasePlan in let summary=match summaryCache,memoKey with Some cache,LIR.FingerprintedReleasePlan cacheKey -> cache includeStaticRootDependencies cacheKey releasePlan generate | _ -> generate () in summary,{requirements with releasePlanSummaries=LIR.ReleasePlanSummaryMap.add key summary requirements.releasePlanSummaries}
let precomputedReleasePlanSummary=precomputedReleasePlanSummaryWithCache None
let addPrecomputedReleasePlanRequirements (summary:rcReleasePlanSummary) (requirements:rcHelperRequirements) : rcHelperRequirements={requirements with plannedListDecHelpers=mergePrecomputedPlannedListHelpers requirements.plannedListDecHelpers summary.plannedListDecHelpers;plannedDictDecHelpers=mergePrecomputedPlannedDictHelpers requirements.plannedDictDecHelpers summary.plannedDictDecHelpers}
let collectPrecomputedRefCountDecRequirement summaryCache ownsSinglePayloadSum (requirements:rcHelperRequirements) (kind,memoKey,metadata) : rcHelperRequirements=match kind with
 | LIR.TaggedList | LIR.DictHeap -> let context=if kind=LIR.TaggedList then "TaggedList RefCountDec helper selection" else "DictHeap RefCountDec helper selection" in let plan=requiredRcMetadataReleasePlan context metadata in let summary,requirements=precomputedReleasePlanSummaryWithCache summaryCache false (LIR.rcReleasePlanMemoKey metadata) plan requirements in let requirements=addPrecomputedReleasePlanRequirements summary requirements in if kind=LIR.TaggedList then {requirements with listDecHelperLabels=S.add (listDecHelperForReleasePlan plan) requirements.listDecHelperLabels} else {requirements with dictDecHelperLabels=S.add (dictDecHelperForReleasePlan plan) requirements.dictDecHelperLabels}
 | LIR.GenericHeap -> (match rcMetadataReleasePlan metadata with None -> requirements | Some plan -> let summary,requirements=precomputedReleasePlanSummaryWithCache summaryCache false (LIR.rcReleasePlanMemoKey metadata) plan requirements in let requirements=addPrecomputedReleasePlanRequirements summary requirements in let requirements={requirements with listDecHelperLabels=S.union requirements.listDecHelperLabels summary.listDecHelperLabels;dictDecHelperLabels=S.union requirements.dictDecHelperLabels summary.dictDecHelperLabels;needsClosureRcDecHelper=requirements.needsClosureRcDecHelper || summary.needsClosureRcDecHelper;needsStreamRcDecHelper=requirements.needsStreamRcDecHelper || summary.needsStreamRcDecHelper} in match summary.expensiveGenericDecHelper with Some (label,size,plan) -> addPrecomputedPlannedGenericHelper ownsSinglePayloadSum memoKey label size plan requirements | _ -> requirements)
 | LIR.ClosureHeap -> {requirements with needsClosureRcDecHelper=true}
 | LIR.StreamHeap -> {requirements with needsStreamRcDecHelper=true}
let collectPrecomputedRefCountIncRequirement (requirements:rcHelperRequirements) kind : rcHelperRequirements=match kind with LIR.TaggedList->{requirements with needsListRcIncHelper=true}|LIR.DictHeap->{requirements with needsDictRcIncHelper=true}|LIR.ClosureHeap->{requirements with needsClosureRcIncHelper=true}|LIR.GenericHeap|LIR.StreamHeap->requirements
let planFunctionArm64RcRequirements summaryCache releasePlanSummaries functionName (facts:LIR.functionCodegenFacts)=
 let initialRequirements={precomputedEmptyRcHelperRequirements with releasePlanSummaries} in
 let requirements=LIR.RefCountDecRequirementMap.fold (fun (kind,memoKey) metadata requirements -> collectPrecomputedRefCountDecRequirement summaryCache (callerOwnsSinglePayloadSum functionName) requirements (kind,memoKey,metadata)) facts.refCountDecRequirements initialRequirements in LIR.RcKindSet.fold (fun kind requirements -> collectPrecomputedRefCountIncRequirement requirements kind) facts.refCountIncRequirements requirements
let planRawSlotInitRetainTargets recordRegistry sumShapeRegistry (facts:LIR.functionCodegenFacts)=
 let targets=MemoryPlanning.SemanticTypeSet.elements facts.rawSlotInitTypes |> List.fold_left (fun map valueType -> LIR.SemanticTypeMap.add valueType (slotInitRootRetainTarget recordRegistry sumShapeRegistry valueType) map) LIR.SemanticTypeMap.empty in {facts with arm64RawSlotInitRetainTargets=Some targets}
let summaryEqual (left:rcReleasePlanSummary) (right:rcReleasePlanSummary)=S.equal left.listDecHelperLabels right.listDecHelperLabels && M.equal (=) left.plannedListDecHelpers right.plannedListDecHelpers && left.expensiveGenericDecHelper=right.expensiveGenericDecHelper && S.equal left.dictDecHelperLabels right.dictDecHelperLabels && M.equal (=) left.plannedDictDecHelpers right.plannedDictDecHelpers && left.needsClosureRcDecHelper=right.needsClosureRcDecHelper && left.needsStreamRcDecHelper=right.needsStreamRcDecHelper
let mergePrecomputedReleasePlanSummaries left right=LIR.ReleasePlanSummaryMap.fold (fun key summary acc -> match LIR.ReleasePlanSummaryMap.find_opt key acc with Some existing when not (summaryEqual existing summary) -> Crash.crash "ARM64 release-plan summary mismatch" | Some _ -> acc | None -> LIR.ReleasePlanSummaryMap.add key summary acc) right left
let mergePrecomputedRcHelperRequirements (left:rcHelperRequirements) (right:rcHelperRequirements) : rcHelperRequirements={listDecHelperLabels=S.union left.listDecHelperLabels right.listDecHelperLabels;plannedListDecHelpers=mergePrecomputedPlannedListHelpers left.plannedListDecHelpers right.plannedListDecHelpers;plannedGenericDecHelpers=mergePrecomputedPlannedGenericHelpers left.plannedGenericDecHelpers right.plannedGenericDecHelpers;plannedDictDecHelpers=mergePrecomputedPlannedDictHelpers left.plannedDictDecHelpers right.plannedDictDecHelpers;dictDecHelperLabels=S.union left.dictDecHelperLabels right.dictDecHelperLabels;needsListRcIncHelper=left.needsListRcIncHelper || right.needsListRcIncHelper;needsDictRcIncHelper=left.needsDictRcIncHelper || right.needsDictRcIncHelper;needsClosureRcIncHelper=left.needsClosureRcIncHelper || right.needsClosureRcIncHelper;needsClosureRcDecHelper=left.needsClosureRcDecHelper || right.needsClosureRcDecHelper;needsStreamRcDecHelper=left.needsStreamRcDecHelper || right.needsStreamRcDecHelper;releasePlanSummaries=mergePrecomputedReleasePlanSummaries left.releasePlanSummaries right.releasePlanSummaries}
