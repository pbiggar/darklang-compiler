(* Full x64 refcount instruction emission, metadata errors and buffer aliases. *)
open Dark_compiler
module E=InstrumentedX64EmitReferenceCounts
open! MemoryModel
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let code=list MachineISAObservation.x64Instr
let result=function Ok value->SemanticJson.union "FSharpResult" "Ok" [code value] | Error error->SemanticJson.union "FSharpResult" "Error" [SemanticJson.string error]
let call f=try tuple [`Bool false;result (f ())] with Failure error | Invalid_argument error->tuple [`Bool true;SemanticJson.string error]
let observe source=
 let kinds=[GenericHeap;StreamHeap;TaggedList;DictHeap;ClosureHeap] in
 let operations=[DynamicStringBuffer;DynamicBlobBuffer;DynamicIntBuffer]@List.concat_map (fun kind -> List.map (fun size -> FixedSizeRoot (size,kind)) [-2147483648;-1;0;8;2147483647]) kinds in
 let simple=[NoReleasePlan;RecursiveRelease (AST.TRecord (source,[]))]@List.map (fun operation -> DynamicBufferRelease operation) operations@List.concat_map (fun kind -> [RootRelease (8,kind,NoPayloadRelease);RootRelease (8,kind,FixedBlockPayloadRelease (8,[]));RootRelease (8,kind,BoxedSumPayloadRelease (8,[],[]));RootRelease (8,kind,ClosurePayloadRelease [])]) kinds in
 let listPlans=List.map (fun plan -> RootRelease (8,TaggedList,TaggedListPayloadRelease plan)) simple in
 let dictPlans=List.concat_map (fun value -> [RootRelease (16,DictHeap,DictPayloadRelease (NoReleasePlan,value));RootRelease (16,DictHeap,DictPayloadRelease (DynamicBufferRelease DynamicStringBuffer,value));RootRelease (16,DictHeap,DictPayloadRelease (value,NoReleasePlan))]) (simple@listPlans) in
 let fields=[FieldRelease (0,DynamicBufferRelease DynamicStringBuffer);FieldRelease (8,RootRelease (8,TaggedList,TaggedListPayloadRelease NoReleasePlan));FieldRelease (16,RootRelease (16,DictHeap,DictPayloadRelease (NoReleasePlan,NoReleasePlan)))] in
 let rich=List.concat_map (fun size -> List.concat_map (fun fields -> [RootRelease (size,GenericHeap,FixedBlockPayloadRelease (size,fields));RootRelease (size,GenericHeap,BoxedSumPayloadRelease (size,fields,[{tag=1;fieldReleases=fields}]))]) [fields;List.rev fields;[FieldRelease (8,NoReleasePlan)]@fields;fields@[FieldRelease (8,NoReleasePlan)];[FieldRelease (8,DynamicBufferRelease DynamicStringBuffer)];[]]) [8;16;24;256] in
 let plans=simple@listPlans@dictPlans@rich in

 let metadata p=Some {releasePlanCacheKey=Some source;releasePlan=Some p;sourceType=Some AST.TString} in
 let child=RootRelease (16,GenericHeap,FixedBlockPayloadRelease (16,fields)) in
 let physical=[LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X8;LIR.X9;LIR.X10;LIR.X11;LIR.X12;LIR.X13;LIR.X14;LIR.X15;LIR.X16;LIR.X17;LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26;LIR.X27;LIR.X29;LIR.X30;LIR.SP] in
 let regs=List.map (fun r->LIR.Physical r) physical@[LIR.Virtual (-1);LIR.Virtual 0;LIR.Virtual 2147483647] in
 let kinds=[LIR.GenericHeap;LIR.StreamHeap;LIR.TaggedList;LIR.DictHeap;LIR.ClosureHeap] in
 let cases=list (fun enabled->let ctx={X64CodeGenTypes.functionName=source;stackSize=32;usedCalleeSaved=[LIR.X19];enableLeakCheck=enabled;recordRegistry=StringOrder.Map.empty;sumShapeRegistry=StringOrder.Map.empty;functionNames=FunctionIdMap.empty} in
  let registerCases=list (fun reg->list (fun kind->list (fun size->let inc=call (fun ()->E.emitRefCountInc ctx reg size kind) in let dec=list (fun meta->call (fun ()->E.emitRefCountDec ctx reg size kind meta)) [None;Some {releasePlanCacheKey=None;releasePlan=None;sourceType=None};metadata child] in tuple [inc;dec]) [-2147483648;-1;0;8;16;255;256;2147483647]) kinds) regs in
  let planCases=list (fun p->list (fun kind->call (fun ()->E.emitRefCountDec ctx (LIR.Physical LIR.X19) 16 kind (metadata p))) kinds) plans in
  let values=[LIR.Imm 0L;LIR.Imm 1L;LIR.Imm (-1L);LIR.StringSymbol source;LIR.StringSymbol "hé😀";LIR.StackSlot 0;LIR.FloatImm (-0.);LIR.FloatSymbol nan;LIR.FuncAddr (AST.functionId (-1L))]@List.map (fun reg->LIR.Reg reg) regs in
  let buffers=list (fun value->list call [(fun ()->E.emitRefCountIncString ctx value);(fun ()->E.emitRefCountDecString ctx value);(fun ()->E.emitRefCountIncInt ctx value);(fun ()->E.emitRefCountDecInt ctx value);(fun ()->E.emitRefCountIncBuffer ctx false value);(fun ()->E.emitRefCountDecBuffer ctx true value)]) values in
  tuple [registerCases;planCases;buffers]) [false;true] in
 tuple [cases]
