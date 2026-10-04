(* Observe complete HAMT ownership helpers, flag combinations and release plans. *)
open Dark_compiler
open! MemoryModel
module E=ARM64DictReferenceCounts
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let code xs=list MachineISAObservation.symInstr xs
let call f=try tuple [`Bool false;code (f ())] with Failure _ | Invalid_argument _ -> tuple [`Bool true]
let observe source=
 let dynamic=DynamicBufferRelease DynamicStringBuffer in
 let simple=[NoReleasePlan;dynamic;DynamicBufferRelease DynamicBlobBuffer;DynamicBufferRelease DynamicIntBuffer;DynamicBufferRelease (FixedSizeRoot (8,GenericHeap));RecursiveRelease (AST.TRecord (source,[]))]@List.concat_map (fun kind -> [RootRelease (8,kind,NoPayloadRelease);RootRelease (8,kind,TaggedListPayloadRelease NoReleasePlan);RootRelease (8,kind,DictPayloadRelease (NoReleasePlan,NoReleasePlan));RootRelease (8,kind,ClosurePayloadRelease [])]) [GenericHeap;StreamHeap;TaggedList;DictHeap;ClosureHeap] in
 let nested=List.map (fun plan -> RootRelease (8,TaggedList,TaggedListPayloadRelease plan)) simple@List.concat_map (fun plan -> [RootRelease (16,DictHeap,DictPayloadRelease (NoReleasePlan,plan));RootRelease (16,DictHeap,DictPayloadRelease (dynamic,plan))]) simple in
 let child=RootRelease (16,GenericHeap,FixedBlockPayloadRelease (16,[FieldRelease (0,dynamic);FieldRelease (8,RecursiveRelease (AST.TRecord (source,[])))])) in
 let fields=List.mapi (fun index plan -> FieldRelease (index*8,plan)) (simple@nested@[child]) in
 let rich=[RootRelease (32,GenericHeap,FixedBlockPayloadRelease (32,fields));RootRelease (32,GenericHeap,BoxedSumPayloadRelease (32,fields,[{tag=0;fieldReleases=[]};{tag=1;fieldReleases=fields};{tag=65536;fieldReleases=[FieldRelease (-32769,child)]}]));RootRelease (16,GenericHeap,FixedBlockPayloadRelease (16,[FieldRelease (0,dynamic);FieldRelease (8,RootRelease (8,TaggedList,TaggedListPayloadRelease NoReleasePlan))]));RootRelease (24,GenericHeap,FixedBlockPayloadRelease (24,[FieldRelease (0,dynamic);FieldRelease (8,RootRelease (8,TaggedList,TaggedListPayloadRelease NoReleasePlan));FieldRelease (16,RootRelease (16,DictHeap,DictPayloadRelease (NoReleasePlan,NoReleasePlan)))]));RootRelease (16,GenericHeap,BoxedSumPayloadRelease (16,[FieldRelease (8,dynamic)],[]));RootRelease (8,GenericHeap,BoxedSumPayloadRelease (8,[],[]))] in
 let plans=simple@nested@rich in
 let contexts=list (fun target -> list (fun enabled ->
  let ctx=ARMPrintingObservation.context source target enabled in
  let generate key flags dict fixed=let flag bit=flags land (1 lsl bit)<>0 in E.generateDictRefCountDecHelper source key (flag 0) (flag 1) dict (flag 2) (flag 3) fixed (flag 4) (flag 5) (flag 6) ctx in
  let flags=list (fun mask -> call (fun () -> generate NoReleasePlan mask (Some "dict-target") None)) (List.init 128 Fun.id) in
  let keys=list (fun key -> list (fun mask -> call (fun () -> generate key mask None None)) [0;1]) plans in
  let fixed=list (fun (plan,sizes) -> list (fun size -> call (fun () -> generate NoReleasePlan 0 None (Some (size,plan)))) sizes) (List.map (fun plan -> plan,[16]) plans@List.map (fun plan -> plan,[-2147483648;-65536;-32769;-32768;-1;0;248;255;256;32767;32768;65535;65536;2147483647]) rich) in
  let labels=list (fun label -> call (fun () -> generate dynamic 0 label None)) [None;Some "";Some source;Some "hé😀";Some (HostText.ofUtf16Units [|0xd800;97;0xdc00|])] in
  let planned=list (fun key -> list (fun value -> call (fun () -> E.generatePlannedDictRefCountDecHelper source (RootRelease (16,DictHeap,DictPayloadRelease (key,value))) ctx)) plans) [NoReleasePlan;dynamic;child] in
  let invalid=list (fun plan -> call (fun () -> E.generatePlannedDictRefCountDecHelper source plan ctx)) simple in
  tuple [flags;keys;fixed;labels;planned;invalid]) [false;true]) [ARM64.targetConfigFor Platform.LinuxARM64;ARM64.targetConfigFor Platform.MacOSARM64] in
 tuple [call E.generateDictRefCountIncHelper;contexts]
