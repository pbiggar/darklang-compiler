(* Observe recursive ownership summaries, memoization callbacks and merge conflicts. *)
open Dark_compiler
open! MemoryModel
module E=ReleasePlanSummary
module J=ProductionLIR
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let call encode f=try tuple [`Bool false;encode (f ())] with Failure _ | Invalid_argument _ -> tuple [`Bool true]
let observe source=
 let dynamic=DynamicBufferRelease DynamicStringBuffer in
 let simple=[NoReleasePlan;dynamic;DynamicBufferRelease DynamicBlobBuffer;DynamicBufferRelease DynamicIntBuffer;RecursiveRelease (AST.TRecord (source,[]))]@List.concat_map (fun kind -> [RootRelease (8,kind,NoPayloadRelease);RootRelease (8,kind,TaggedListPayloadRelease dynamic);RootRelease (8,kind,DictPayloadRelease (NoReleasePlan,dynamic));RootRelease (8,kind,ClosurePayloadRelease [FieldRelease (0,dynamic)])]) [GenericHeap;StreamHeap;TaggedList;DictHeap;ClosureHeap] in
 let fields=List.mapi (fun index plan -> FieldRelease (index*8,plan)) simple in
 let rich=[RootRelease (256,GenericHeap,FixedBlockPayloadRelease (256,fields));RootRelease (256,GenericHeap,BoxedSumPayloadRelease (256,fields,[{tag=1;fieldReleases=fields}]));RootRelease (16,DictHeap,DictPayloadRelease (dynamic,RootRelease (8,TaggedList,TaggedListPayloadRelease dynamic)));RootRelease (8,TaggedList,TaggedListPayloadRelease (RootRelease (16,DictHeap,DictPayloadRelease (dynamic,dynamic))));RootRelease (8,ClosureHeap,ClosurePayloadRelease fields)] in
 let validFields=List.init 25 (fun index -> FieldRelease (index*8,dynamic)) in
 let rich=rich@[RootRelease (200,GenericHeap,FixedBlockPayloadRelease (200,validFields));RootRelease (200,GenericHeap,BoxedSumPayloadRelease (200,validFields,[{tag=1;fieldReleases=validFields}]))] in
 let plans=simple@rich in
 let summaries=list (fun plan -> list (fun static -> call J.arm64ReleasePlanSummary (fun () -> E.summarizePrecomputedReleasePlan static plan)) [false;true]) plans in
 let metadata=None::Some {releasePlanCacheKey=None;releasePlan=None;sourceType=None}::List.concat_map (fun plan -> List.map (fun key -> Some {releasePlanCacheKey=key;releasePlan=Some plan;sourceType=Some AST.TString}) [None;Some source]) plans in
 let block={LIR.label=LIR.Label "entry";instrs=[];terminator=LIR.Ret} in
 let func={LIR.id=AST.functionId 0L;name=source;typedParams=[];cfg={LIR.entry=block.LIR.label;blocks=LIR.LabelMap.singleton block.LIR.label block};stackSize=0;usedCalleeSaved=[];codegenFacts=None} in
 let base=LIR.analyzeFunctionCodegenFacts func in
 let kinds=[LIR.GenericHeap;LIR.StreamHeap;LIR.TaggedList;LIR.DictHeap;LIR.ClosureHeap] in
 let cases=list (fun meta -> list (fun kind ->
  let key=LIR.rcReleasePlanMemoKey meta in
  let facts={base with LIR.refCountDecRequirements=LIR.RefCountDecRequirementMap.singleton (kind,key) meta;refCountIncRequirements=LIR.RcKindSet.of_list kinds} in
  list (fun name ->
   let trace=ref [] in
   let cache static key plan generate=trace:=(static,key,plan):: !trace;generate () in
   let result=call J.arm64RcHelperRequirements (fun () -> E.planFunctionArm64RcRequirements (Some cache) LIR.ReleasePlanSummaryMap.empty name facts) in
   tuple [result;list (fun (static,key,plan) -> tuple [`Bool static;SemanticJson.string key;SemanticANF.memoryModel_rcReleasePlan plan]) (List.rev !trace)]) [source;"Darklang.Stdlib.List.foo";"Darklang.Stdlib.Dict.foo"]) kinds) metadata in
 let memoized=list (fun plan -> list (fun static -> list (fun key -> call (fun (summary,requirements) -> tuple [J.arm64ReleasePlanSummary summary;J.arm64RcHelperRequirements requirements]) (fun () -> let _,requirements=E.precomputedReleasePlanSummary static key plan E.precomputedEmptyRcHelperRequirements in E.precomputedReleasePlanSummary static key NoReleasePlan requirements)) [LIR.StructuralReleasePlan (Some plan);LIR.FingerprintedReleasePlan source]) [false;true]) plans in
 let successful=List.filter_map (fun plan -> try let summary=E.summarizePrecomputedReleasePlan false plan in let requirements=E.addPrecomputedReleasePlanRequirements summary E.precomputedEmptyRcHelperRequirements in Some requirements with Failure _ | Invalid_argument _ -> None) plans in
 let merged=list (fun left -> list (fun right -> call J.arm64RcHelperRequirements (fun () -> E.mergePrecomputedRcHelperRequirements left right)) successful) successful in
 let conflict plan={E.precomputedEmptyRcHelperRequirements with LIR.plannedListDecHelpers=StringOrder.Map.singleton source (8,plan);plannedDictDecHelpers=StringOrder.Map.singleton source plan} in
 let conflicts=list (fun plan -> call J.arm64RcHelperRequirements (fun () -> E.mergePrecomputedRcHelperRequirements (conflict NoReleasePlan) (conflict plan))) plans in
 let specs=List.concat_map (fun size -> List.concat_map (fun plan -> List.map (fun owns -> {LIR.releasePlanMemoKeys=LIR.RcReleasePlanMemoKeySet.of_list [LIR.FingerprintedReleasePlan source;LIR.StructuralReleasePlan (Some plan)];payloadSize=size;releasePlan=plan;ownsSinglePayloadSum=owns}) [false;true]) [NoReleasePlan;dynamic]) [8;16] in
 let genericMerges=list (fun left -> list (fun right -> call J.arm64RcHelperRequirements (fun () -> E.mergePrecomputedRcHelperRequirements {E.precomputedEmptyRcHelperRequirements with LIR.plannedGenericDecHelpers=StringOrder.Map.singleton source left} {E.precomputedEmptyRcHelperRequirements with LIR.plannedGenericDecHelpers=StringOrder.Map.singleton source right})) specs) specs in
 let summaryRequirements plan=let summary=E.summarizePrecomputedReleasePlan false plan in {E.precomputedEmptyRcHelperRequirements with LIR.releasePlanSummaries=LIR.ReleasePlanSummaryMap.singleton (false,LIR.FingerprintedReleasePlan source) summary} in
 let summaryConflicts=list (fun left -> list (fun right -> call J.arm64RcHelperRequirements (fun () -> E.mergePrecomputedRcHelperRequirements (summaryRequirements left) (summaryRequirements right))) [NoReleasePlan;RootRelease (8,TaggedList,TaggedListPayloadRelease dynamic);RootRelease (8,ClosureHeap,NoPayloadRelease)]) [NoReleasePlan;RootRelease (8,TaggedList,TaggedListPayloadRelease dynamic);RootRelease (8,ClosureHeap,NoPayloadRelease)] in
 let types=[AST.TInt64;AST.TString;AST.TBlob;AST.TInt;AST.TList AST.TString;AST.TDict (AST.TString,AST.TString);AST.TFunction ([AST.TInt64],AST.TString);AST.TStream AST.TString;AST.TTuple [AST.TString];AST.TRecord ("R",[]);AST.TRecord ("missing",[]);AST.TSum ("missing",[])] in
 let raw=list (fun typ -> let facts={base with LIR.rawSlotInitTypes=MemoryPlanning.SemanticTypeSet.singleton typ} in call J.functionCodegenFacts (fun () -> E.planRawSlotInitRetainTargets (StringOrder.Map.singleton "R" ["field",AST.TString]) StringOrder.Map.empty facts)) types in
 tuple [J.arm64RcHelperRequirements E.precomputedEmptyRcHelperRequirements;summaries;cases;memoized;merged;conflicts;genericMerges;summaryConflicts;raw]
