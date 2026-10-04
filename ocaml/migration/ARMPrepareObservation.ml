(* Complete helper preparation outputs, identities, cache events and phase order. *)
open Dark_compiler
module P=ARM64PrepareFunctions
module J=ProductionLIR
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let call encode f=try tuple [`Bool false;encode (f ())] with Failure _ | Invalid_argument _ -> tuple [`Bool true]
let ids map=`Assoc ["map",list (fun (label,id) -> tuple [SemanticJson.string label;SemanticJson.union "FunctionId" "FunctionId" [`Assoc ["kind",`String "uint64";"value",`String (Printf.sprintf "%Lu" (AST.functionIdValue id))]]]) (StringOrder.Map.bindings map)]
let observe source=
 let dynamic=MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer in
 let fields=List.init 25 (fun index -> MemoryModel.FieldRelease (index*8,dynamic)) in
 let plans=[MemoryModel.NoReleasePlan;MemoryModel.RootRelease (8,MemoryModel.GenericHeap,MemoryModel.NoPayloadRelease);MemoryModel.RootRelease (8,MemoryModel.TaggedList,MemoryModel.TaggedListPayloadRelease dynamic);MemoryModel.RootRelease (16,MemoryModel.DictHeap,MemoryModel.DictPayloadRelease (dynamic,dynamic));MemoryModel.RootRelease (200,MemoryModel.GenericHeap,MemoryModel.FixedBlockPayloadRelease (200,fields));MemoryModel.RootRelease (200,MemoryModel.GenericHeap,MemoryModel.BoxedSumPayloadRelease (200,fields,[{MemoryModel.tag=1;fieldReleases=fields}]));MemoryModel.RecursiveRelease (AST.TRecord (source,[]))] in
 let make id name instructions attached=
  let block={LIR.label=LIR.Label "entry";instrs=instructions;terminator=LIR.Ret} in
  let func={LIR.id=AST.functionId id;name;typedParams=[];cfg={LIR.entry=block.LIR.label;blocks=LIR.LabelMap.singleton block.LIR.label block};stackSize=0;usedCalleeSaved=[];codegenFacts=None} in
  if attached then LIR.attachFunctionCodegenFacts func else func in
 let metadata plan key=Some {MemoryModel.releasePlanCacheKey=key;releasePlan=Some plan;sourceType=Some AST.TString} in
 let functions=List.concat_map (fun plan -> List.concat_map (fun key -> List.map (fun name -> make 3L name [LIR.RefCountDec (LIR.Virtual 0,200,LIR.GenericHeap,metadata plan key);LIR.RefCountDec (LIR.Virtual 1,200,LIR.GenericHeap,metadata plan key);LIR.RefCountDec (LIR.Virtual 2,8,LIR.TaggedList,metadata plan key);LIR.RefCountInc (LIR.Virtual 0,8,LIR.ClosureHeap,None);LIR.RawSlotInit (LIR.Virtual 3,LIR.Virtual 4,LIR.Virtual 5,AST.TRecord ("R",[]))] true) [source;"Darklang.Stdlib.List.fn";"Darklang.Stdlib.Dict.fn"]) [None;Some source;Some "hé😀"]) plans in
 let records=StringOrder.Map.singleton "R" ["field",AST.TString] in
 let run batch highest known=
  let trace=ref [] and phases=ref [] in
  let cache static key plan generate=trace:=(static,key,plan):: !trace;generate () in
  let phase name elapsed=phases:=(name,elapsed>=0.0):: !phases in
  let result=call (fun (functions,helpers) -> tuple [list J.functionDef functions;ids helpers]) (fun () -> P.prepareARM64FunctionsForAllocationWithCache (Some cache) (Some phase) records StringOrder.Map.empty (AST.functionId highest) known batch) in
  tuple [result;list (fun (static,key,plan) -> tuple [`Bool static;SemanticJson.string key;SemanticANF.memoryModel_rcReleasePlan plan]) (List.rev !trace);list (fun (name,valid) -> tuple [SemanticJson.string name;`Bool valid]) (List.rev !phases)] in
 let individual=list (fun func -> tuple [call (list J.functionDef) (fun () -> P.attachARM64CodegenFactsToFunctions [func]);call (list J.functionDef) (fun () -> P.attachARM64CodegenFactsToFunctionsWithCache None records StringOrder.Map.empty [func]);call (list J.functionDef) (fun () -> P.prepareARM64FunctionsForAllocation [func]);run [func] 3L StringOrder.Map.empty]) functions in
 let expensive=List.nth functions 36 in
 let sibling={expensive with LIR.id=AST.functionId 4L;name=source^"_sibling"} in
 let batches=[[];[expensive;sibling];[sibling;expensive];functions;[make 0L source [] false];[make 0L source [] true]] in
 let boundaries=list (fun batch -> list (fun highest -> list (fun known -> run batch highest known) [StringOrder.Map.empty;StringOrder.Map.singleton source (AST.functionId 17L);StringOrder.Map.of_seq (List.to_seq ["a",AST.functionId Int64.min_int;"z",AST.functionId Int64.max_int])]) [0L;4L;Int64.max_int;Int64.min_int;-1L]) batches in
 let reuse=list (fun batch ->
  let first=P.prepareARM64FunctionsForAllocationWithCache None None records StringOrder.Map.empty (AST.functionId 4L) StringOrder.Map.empty batch in
  let _,known=first in run batch 100L known) [[];[expensive;sibling]] in
 let variants=StringOrder.Map.singleton "Choice" {LIR.typeParams=[];variants=[{LIR.name="A";tag=0;payload=None;fieldCount=0};{LIR.name="B";tag=1;payload=Some AST.TString;fieldCount=1}]} in
 let programs=list (fun batch -> list (fun variants -> call J.program (fun () -> P.prepareARM64Program (LIR.Program (batch,variants,records)))) [StringOrder.Map.empty;variants]) batches in
 tuple [individual;boundaries;reuse;programs]
