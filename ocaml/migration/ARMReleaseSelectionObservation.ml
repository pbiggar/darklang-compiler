(* Complete release selection, fingerprints and first-match field metadata. *)
open Dark_compiler
module E=ARM64ReleaseSelection
open! MemoryModel
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let call encode f=try tuple [`Bool false;encode (f ())] with Failure _ | Invalid_argument _ -> tuple [`Bool true]
let boolean value=`Bool value
let observe source=
 let kinds=[GenericHeap;StreamHeap;TaggedList;DictHeap;ClosureHeap] in
 let operations=[DynamicStringBuffer;DynamicBlobBuffer;DynamicIntBuffer]@List.concat_map (fun kind -> List.map (fun size -> FixedSizeRoot (size,kind)) [-2147483648;-1;0;8;2147483647]) kinds in
 let simple=[NoReleasePlan;RecursiveRelease (AST.TRecord (source,[]))]@List.map (fun operation -> DynamicBufferRelease operation) operations@List.concat_map (fun kind -> [RootRelease (8,kind,NoPayloadRelease);RootRelease (8,kind,FixedBlockPayloadRelease (8,[]));RootRelease (8,kind,BoxedSumPayloadRelease (8,[],[]));RootRelease (8,kind,ClosurePayloadRelease [])]) kinds in
 let listPlans=List.map (fun plan -> RootRelease (8,TaggedList,TaggedListPayloadRelease plan)) simple in
 let dictPlans=List.concat_map (fun value -> [RootRelease (16,DictHeap,DictPayloadRelease (NoReleasePlan,value));RootRelease (16,DictHeap,DictPayloadRelease (DynamicBufferRelease DynamicStringBuffer,value));RootRelease (16,DictHeap,DictPayloadRelease (value,NoReleasePlan))]) (simple@listPlans) in
 let fields=[FieldRelease (0,DynamicBufferRelease DynamicStringBuffer);FieldRelease (8,RootRelease (8,TaggedList,TaggedListPayloadRelease NoReleasePlan));FieldRelease (16,RootRelease (16,DictHeap,DictPayloadRelease (NoReleasePlan,NoReleasePlan)))] in
 let rich=List.concat_map (fun size -> List.concat_map (fun fields -> [RootRelease (size,GenericHeap,FixedBlockPayloadRelease (size,fields));RootRelease (size,GenericHeap,BoxedSumPayloadRelease (size,fields,[{tag=1;fieldReleases=fields}]))]) [fields;List.rev fields;[FieldRelease (8,NoReleasePlan)]@fields;fields@[FieldRelease (8,NoReleasePlan)];[FieldRelease (8,DynamicBufferRelease DynamicStringBuffer)];[]]) [8;16;24;256] in
 let plans=simple@listPlans@dictPlans@rich in
 let fingerprints=["";source;"hé😀";"a\000b";HostText.ofScalars [|0xfffd;0x61;0xfffd|]] in
 let selected=list (fun plan -> tuple [call SemanticJson.string (fun () -> E.listDecHelperForReleasePlan plan);call SemanticJson.string (fun () -> E.dictDecHelperForReleasePlan plan);list (fun fingerprint -> tuple [call SemanticJson.string (fun () -> E.listDecHelperForElementRelease fingerprint plan);call SemanticJson.string (fun () -> E.dictDecHelperForReleasePlanWithFingerprint fingerprint plan)]) fingerprints]) plans in
 let paired=list (fun key -> list (fun value -> call boolean (fun () -> E.dictPayloadReleaseNeedsPlannedHelper key value)) plans) plans in
 let fieldPlans=List.concat_map (fun offset -> List.map (fun plan -> [FieldRelease (offset,plan)]) simple) [-2147483648;-1;0;8;16;2147483647]@[fields;List.rev fields;[FieldRelease (8,NoReleasePlan)]@fields;fields@[FieldRelease (8,NoReleasePlan)];[]] in
 let fieldSelections=list (fun fields -> list (fun offset -> tuple [list (fun kind -> call boolean (fun () -> E.releasePlanRootKindAt offset kind fields)) kinds;list (fun operation -> call boolean (fun () -> E.releasePlanDynamicOperationAt offset operation fields)) operations]) [-2147483648;-1;0;8;16;2147483647]) fieldPlans in
 tuple [selected;paired;fieldSelections]
