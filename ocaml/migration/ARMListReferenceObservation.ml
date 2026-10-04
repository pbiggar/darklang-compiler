(* Complete tagged-list helper selection, ordered plans and recursive payload lowering. *)
open Dark_compiler
module E=ARM64ListReferenceCounts
module H=ARM64CodeGenTypes
module J=MachineISAObservation
open! MemoryModel
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let code xs=list J.symInstr xs
let call f=try tuple [`Bool false;code (f ())] with Failure _ | Invalid_argument _ -> tuple [`Bool true]
let observe source=
 let labels=[H.listRefCountDecHelperLabel;H.listRefCountDecListHelperLabel;H.listRefCountDecDictHelperLabel;H.listRefCountDecDictListHelperLabel;H.listRefCountDecClosureHelperLabel;H.listRefCountDecStringHelperLabel;H.listRefCountDecBlobHelperLabel] in
 let plans=[NoReleasePlan;DynamicBufferRelease DynamicStringBuffer;DynamicBufferRelease DynamicBlobBuffer;DynamicBufferRelease DynamicIntBuffer;DynamicBufferRelease (FixedSizeRoot (8,GenericHeap));RecursiveRelease (AST.TRecord (source,[]))]@List.concat_map (fun kind -> [RootRelease (8,kind,NoPayloadRelease);RootRelease (8,kind,TaggedListPayloadRelease NoReleasePlan);RootRelease (8,kind,DictPayloadRelease (NoReleasePlan,NoReleasePlan));RootRelease (8,kind,ClosurePayloadRelease [])]) [GenericHeap;StreamHeap;TaggedList;DictHeap;ClosureHeap] in
 let roots=List.map (fun plan -> RootRelease (8,TaggedList,TaggedListPayloadRelease plan)) plans@List.concat_map (fun plan -> [RootRelease (16,DictHeap,DictPayloadRelease (plan,NoReleasePlan));RootRelease (16,DictHeap,DictPayloadRelease (NoReleasePlan,plan))]) plans in
 let child=RootRelease (16,GenericHeap,FixedBlockPayloadRelease (16,[FieldRelease (0,DynamicBufferRelease DynamicStringBuffer);FieldRelease (8,RecursiveRelease (AST.TRecord (source,[])))])) in
 let fields=List.mapi (fun index plan -> FieldRelease (index*8,plan)) (plans@roots@[child]) in
 let variants=[{tag=0;fieldReleases=[]};{tag=1;fieldReleases=fields};{tag=(-1);fieldReleases=[FieldRelease (-32769,child)]};{tag=65536;fieldReleases=[FieldRelease (32768,DynamicBufferRelease DynamicBlobBuffer)]}] in
 let rich=[RootRelease (32,GenericHeap,FixedBlockPayloadRelease (32,fields));RootRelease (32,GenericHeap,BoxedSumPayloadRelease (32,fields,variants));RootRelease (8,GenericHeap,BoxedSumPayloadRelease (8,[],[]));RootRelease (8,GenericHeap,FixedBlockPayloadRelease (8,[]))] in
 let plans=plans@roots@rich in
 let specs=list (fun spec -> tuple [SemanticJson.string spec.E.label;`Bool spec.E.releaseLeafListPayload;`Bool spec.E.releaseLeafDictPayload;`Bool spec.E.releaseLeafClosurePayload]) E.listRefCountDecHelperSpecs in
 let contexts=list (fun target -> list (fun enabled ->
  let ctx=ARMPrintingObservation.context source target enabled in
  let static=list (fun mask -> let needed=List.mapi (fun index label -> index,label) labels |> List.filter_map (fun (index,label) -> if mask land (1 lsl index)=0 then None else Some label) |> StringOrder.Set.of_list in call (fun () -> E.generateNeededListRefCountDecHelpers ctx needed StringOrder.Map.empty)) (List.init 128 Fun.id) in
  let planned=list (fun (plan,sizes) -> list (fun size -> let name=source^"_planned" in let map=StringOrder.Map.singleton name (size,plan) in tuple [call (fun () -> E.generateNeededListRefCountDecHelpers ctx StringOrder.Set.empty map);call (fun () -> E.generateNeededListRefCountDecHelpers ctx (StringOrder.Set.singleton name) map)]) sizes) (List.map (fun plan -> plan,[16]) plans@List.map (fun plan -> plan,[-2147483648;-65536;-32769;-32768;-1;0;8;248;255;256;32767;32768;65535;65536;2147483647]) rich) in
  let ordered=let entries=List.mapi (fun index plan -> (if index mod 2=0 then "😀" else "\xee\x80\x80")^string_of_int index,(16,plan)) rich in let map=StringOrder.Map.of_list entries in call (fun () -> E.generateNeededListRefCountDecHelpers ctx (StringOrder.Set.of_list (labels@List.map fst entries@["unknown"])) map) in
  tuple [static;planned;ordered]) [false;true]) [ARM64.targetConfigFor Platform.LinuxARM64;ARM64.targetConfigFor Platform.MacOSARM64] in
 tuple [call E.generateListRefCountIncHelper;specs;contexts]
