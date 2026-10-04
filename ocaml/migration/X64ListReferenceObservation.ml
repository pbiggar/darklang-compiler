(* Complete x64 tagged-list traversal and typed payload/helper dependencies. *)
open Dark_compiler
module E=InstrumentedX64ListReferenceCounts
open! MemoryModel
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let code=list MachineISAObservation.x64Instr
let call encoder f=try tuple [`Bool false;encoder (f ())] with Failure error | Invalid_argument error->tuple [`Bool true;SemanticJson.string error]
let leaf=function
 | E.NoLeafPayloadRelease->SemanticJson.union "ListLeafPayloadRelease" "NoLeafPayloadRelease" []
 | E.FixedBlockPlannedLeafPayload (size,p)->SemanticJson.union "ListLeafPayloadRelease" "FixedBlockPlannedLeafPayload" [SemanticJson.int32 size;SemanticANF.memoryModel_rcReleasePlan p]
 | E.RecursivePlannedLeafPayload typ->SemanticJson.union "ListLeafPayloadRelease" "RecursivePlannedLeafPayload" [SemanticAST.semanticType typ]
 | E.ListLeafPayload->SemanticJson.union "ListLeafPayloadRelease" "ListLeafPayload" []
 | E.PlannedListLeafPayload p->SemanticJson.union "ListLeafPayloadRelease" "PlannedListLeafPayload" [SemanticANF.memoryModel_rcReleasePlan p]
 | E.ClosureLeafPayload->SemanticJson.union "ListLeafPayloadRelease" "ClosureLeafPayload" []
 | E.DictLeafPayload->SemanticJson.union "ListLeafPayloadRelease" "DictLeafPayload" []
 | E.DictListLeafPayload->SemanticJson.union "ListLeafPayloadRelease" "DictListLeafPayload" []
 | E.PlannedDictLeafPayload p->SemanticJson.union "ListLeafPayloadRelease" "PlannedDictLeafPayload" [SemanticANF.memoryModel_rcReleasePlan p]
 | E.DynamicBufferLeafPayload->SemanticJson.union "ListLeafPayloadRelease" "DynamicBufferLeafPayload" []
 | E.DynamicIntLeafPayload->SemanticJson.union "ListLeafPayloadRelease" "DynamicIntLeafPayload" []
let observe source=
 let kinds=[GenericHeap;StreamHeap;TaggedList;DictHeap;ClosureHeap] in
 let operations=[DynamicStringBuffer;DynamicBlobBuffer;DynamicIntBuffer]@List.concat_map (fun kind -> List.map (fun size -> FixedSizeRoot (size,kind)) [-2147483648;-1;0;8;2147483647]) kinds in
 let simple=[NoReleasePlan;RecursiveRelease (AST.TRecord (source,[]))]@List.map (fun operation -> DynamicBufferRelease operation) operations@List.concat_map (fun kind -> [RootRelease (8,kind,NoPayloadRelease);RootRelease (8,kind,FixedBlockPayloadRelease (8,[]));RootRelease (8,kind,BoxedSumPayloadRelease (8,[],[]));RootRelease (8,kind,ClosurePayloadRelease [])]) kinds in
 let _listPlans=List.map (fun plan -> RootRelease (8,TaggedList,TaggedListPayloadRelease plan)) simple in
 let _dictPlans=List.concat_map (fun value -> [RootRelease (16,DictHeap,DictPayloadRelease (NoReleasePlan,value));RootRelease (16,DictHeap,DictPayloadRelease (DynamicBufferRelease DynamicStringBuffer,value));RootRelease (16,DictHeap,DictPayloadRelease (value,NoReleasePlan))]) (simple@_listPlans) in
 let fields=[FieldRelease (0,DynamicBufferRelease DynamicStringBuffer);FieldRelease (8,RootRelease (8,TaggedList,TaggedListPayloadRelease NoReleasePlan));FieldRelease (16,RootRelease (16,DictHeap,DictPayloadRelease (NoReleasePlan,NoReleasePlan)))] in
 let rich=List.concat_map (fun size -> List.concat_map (fun fields -> [RootRelease (size,GenericHeap,FixedBlockPayloadRelease (size,fields));RootRelease (size,GenericHeap,BoxedSumPayloadRelease (size,fields,[{tag=1;fieldReleases=fields}]))]) [fields;List.rev fields;[FieldRelease (8,NoReleasePlan)]@fields;fields@[FieldRelease (8,NoReleasePlan)];[FieldRelease (8,DynamicBufferRelease DynamicStringBuffer)];[]]) [8;16;24;256] in
 let plans=simple@rich in

 let planned=List.mapi (fun index p->"planned-"^string_of_int index,(16,p)) plans |> StringOrder.Map.of_list in
 let leaves=[E.NoLeafPayloadRelease;E.ListLeafPayload;E.ClosureLeafPayload;E.DictLeafPayload;E.DictListLeafPayload;E.DynamicBufferLeafPayload;E.DynamicIntLeafPayload;E.RecursivePlannedLeafPayload (AST.TRecord (source,[]))]@List.concat_map (fun p->[E.FixedBlockPlannedLeafPayload (16,p);E.PlannedListLeafPayload p;E.PlannedDictLeafPayload p]) plans in
 let declarations=list (fun (name,value)->tuple [SemanticJson.string name;leaf value]) E.listRefCountDecHelperSpecs in
 let direct=list (fun enabled->list (fun value->let emitted=call code (fun ()->E.generateListRefCountDecHelperWith source enabled StringOrder.Map.empty StringOrder.Map.empty value) in tuple [emitted;call (fun b->`Bool b) (fun ()->E.listLeafPayloadNeedsDictDecHelper value);call (fun b->`Bool b) (fun ()->E.listLeafPayloadNeedsDictListValueDecHelper value);call (fun b->`Bool b) (fun ()->E.listLeafPayloadNeedsClosureDecHelper value)]) leaves) [false;true] in
 let selections=list (fun mask->let needed=E.listRefCountDecHelperSpecs |> List.mapi (fun index (label,_)->index,label) |> List.filter_map (fun (index,label)->if mask land (1 lsl index)<>0 then Some label else None) |> StringOrder.Set.of_list in tuple [call code (fun ()->E.generateNeededListRefCountDecHelpers needed StringOrder.Map.empty false StringOrder.Map.empty StringOrder.Map.empty);`Bool (E.selectedListRefCountDecHelpersNeedDictDecHelper needed);`Bool (E.selectedListRefCountDecHelpersNeedDictListValueDecHelper needed);`Bool (E.selectedListRefCountDecHelpersNeedClosureDecHelper needed)]) (List.init 128 Fun.id) in
 let plannedCases=list (fun enabled->list (fun names->call code (fun ()->E.generateNeededListRefCountDecHelpers (StringOrder.Set.of_list names) planned enabled StringOrder.Map.empty StringOrder.Map.empty)) [[];["missing"];StringOrder.Map.bindings planned |> List.map fst;(StringOrder.Map.bindings planned |> List.map fst)@List.map fst E.listRefCountDecHelperSpecs]) [false;true] in
 tuple [declarations;direct;selections;plannedCases;SemanticJson.string E.listRefCountIncHelperLabel;call code (fun ()->E.generateListRefCountIncHelper ())]
