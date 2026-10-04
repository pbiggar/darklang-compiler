(* Complete x64 HAMT retain, collision payload and planned helper emission. *)
open Dark_compiler
module E=X64DictReferenceCounts
open! MemoryModel
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let code=list MachineISAObservation.x64Instr
let call f=try tuple [`Bool false;code (f ())] with Failure error | Invalid_argument error->tuple [`Bool true;SemanticJson.string error]
let observe source=
 let kinds=[GenericHeap;StreamHeap;TaggedList;DictHeap;ClosureHeap] in
 let operations=[DynamicStringBuffer;DynamicBlobBuffer;DynamicIntBuffer]@List.concat_map (fun kind -> List.map (fun size -> FixedSizeRoot (size,kind)) [-2147483648;-1;0;8;2147483647]) kinds in
 let simple=[NoReleasePlan;RecursiveRelease (AST.TRecord (source,[]))]@List.map (fun operation -> DynamicBufferRelease operation) operations@List.concat_map (fun kind -> [RootRelease (8,kind,NoPayloadRelease);RootRelease (8,kind,FixedBlockPayloadRelease (8,[]));RootRelease (8,kind,BoxedSumPayloadRelease (8,[],[]));RootRelease (8,kind,ClosurePayloadRelease [])]) kinds in
 let _listPlans=List.map (fun plan -> RootRelease (8,TaggedList,TaggedListPayloadRelease plan)) simple in
 let _dictPlans=List.concat_map (fun value -> [RootRelease (16,DictHeap,DictPayloadRelease (NoReleasePlan,value));RootRelease (16,DictHeap,DictPayloadRelease (DynamicBufferRelease DynamicStringBuffer,value));RootRelease (16,DictHeap,DictPayloadRelease (value,NoReleasePlan))]) (simple@_listPlans) in
 let fields=[FieldRelease (0,DynamicBufferRelease DynamicStringBuffer);FieldRelease (8,RootRelease (8,TaggedList,TaggedListPayloadRelease NoReleasePlan));FieldRelease (16,RootRelease (16,DictHeap,DictPayloadRelease (NoReleasePlan,NoReleasePlan)))] in
 let rich=List.concat_map (fun size -> List.concat_map (fun fields -> [RootRelease (size,GenericHeap,FixedBlockPayloadRelease (size,fields));RootRelease (size,GenericHeap,BoxedSumPayloadRelease (size,fields,[{tag=1;fieldReleases=fields}]))]) [fields;List.rev fields;[FieldRelease (8,NoReleasePlan)]@fields;fields@[FieldRelease (8,NoReleasePlan)];[FieldRelease (8,DynamicBufferRelease DynamicStringBuffer)];[]]) [8;16;24;256] in
 let plans=simple@rich in

 let child=RootRelease (16,GenericHeap,FixedBlockPayloadRelease (16,[FieldRelease (0,DynamicBufferRelease DynamicStringBuffer);FieldRelease (8,RecursiveRelease (AST.TRecord (source,[])))])) in
 let keys=[NoReleasePlan;DynamicBufferRelease DynamicStringBuffer;DynamicBufferRelease DynamicIntBuffer;RootRelease (8,ClosureHeap,NoPayloadRelease);child;RecursiveRelease (AST.TRecord (source,[]))] in
 let keyCases=list (fun enabled->list (fun p->call (fun ()->E.generateDictRefCountDecHelper source p None false None false false None enabled StringOrder.Map.empty StringOrder.Map.empty)) plans) [false;true] in
 let planned=list (fun enabled->list (fun key->list (fun value->call (fun ()->E.generatePlannedDictRefCountDecHelper source (RootRelease (16,DictHeap,DictPayloadRelease (key,value))) enabled StringOrder.Map.empty StringOrder.Map.empty)) plans) keys) [false;true] in
 let flags=list (fun enabled->list (fun mask->list (fun dynamic->list (fun fixed->call (fun ()->E.generateDictRefCountDecHelper source child dynamic (mask land 1<>0) (if mask land 2<>0 then Some "custom-dict" else None) (mask land 4<>0) (mask land 8<>0) fixed enabled StringOrder.Map.empty StringOrder.Map.empty)) [None;Some (16,child);Some (16,RecursiveRelease (AST.TSum (source,[])))]) [None;Some DynamicStringBuffer;Some DynamicIntBuffer]) (List.init 16 Fun.id)) [false;true] in
 let sizes=list (fun size->call (fun ()->E.generateDictRefCountDecHelper source child None false None false false (Some (size,child)) false StringOrder.Map.empty StringOrder.Map.empty)) [-2147483648;-1;0;8;255;256;2147483647] in
 let invalid=list (fun p->call (fun ()->E.generatePlannedDictRefCountDecHelper source p false StringOrder.Map.empty StringOrder.Map.empty)) plans in
 tuple [call E.generateDictRefCountIncHelper;keyCases;planned;flags;sizes;invalid]
