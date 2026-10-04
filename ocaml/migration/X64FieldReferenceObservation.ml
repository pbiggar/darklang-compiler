(* Full x64 fixed-block, recursive and stream destruction observations. *)
open Dark_compiler
module E=InstrumentedX64FieldReferenceCounts
open! MemoryModel
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let code=list MachineISAObservation.x64Instr
let call encoder f=try tuple [`Bool false;encoder (f ())] with Failure error | Invalid_argument error->tuple [`Bool true;SemanticJson.string error]
let observe source=
 let kinds=[GenericHeap;StreamHeap;TaggedList;DictHeap;ClosureHeap] in
 let operations=[DynamicStringBuffer;DynamicBlobBuffer;DynamicIntBuffer]@List.concat_map (fun kind -> List.map (fun size -> FixedSizeRoot (size,kind)) [-2147483648;-1;0;8;2147483647]) kinds in
 let simple=[NoReleasePlan;RecursiveRelease (AST.TRecord (source,[]))]@List.map (fun operation -> DynamicBufferRelease operation) operations@List.concat_map (fun kind -> [RootRelease (8,kind,NoPayloadRelease);RootRelease (8,kind,FixedBlockPayloadRelease (8,[]));RootRelease (8,kind,BoxedSumPayloadRelease (8,[],[]));RootRelease (8,kind,ClosurePayloadRelease [])]) kinds in
 let listPlans=List.map (fun plan -> RootRelease (8,TaggedList,TaggedListPayloadRelease plan)) simple in
 let dictPlans=List.concat_map (fun value -> [RootRelease (16,DictHeap,DictPayloadRelease (NoReleasePlan,value));RootRelease (16,DictHeap,DictPayloadRelease (DynamicBufferRelease DynamicStringBuffer,value));RootRelease (16,DictHeap,DictPayloadRelease (value,NoReleasePlan))]) (simple@listPlans) in
 let fields=[FieldRelease (0,DynamicBufferRelease DynamicStringBuffer);FieldRelease (8,RootRelease (8,TaggedList,TaggedListPayloadRelease NoReleasePlan));FieldRelease (16,RootRelease (16,DictHeap,DictPayloadRelease (NoReleasePlan,NoReleasePlan)))] in
 let rich=List.concat_map (fun size -> List.concat_map (fun fields -> [RootRelease (size,GenericHeap,FixedBlockPayloadRelease (size,fields));RootRelease (size,GenericHeap,BoxedSumPayloadRelease (size,fields,[{tag=1;fieldReleases=fields}]))]) [fields;List.rev fields;[FieldRelease (8,NoReleasePlan)]@fields;fields@[FieldRelease (8,NoReleasePlan)];[FieldRelease (8,DynamicBufferRelease DynamicStringBuffer)];[]]) [8;16;24;256] in
 let plans=simple@listPlans@dictPlans@rich in
 let records=StringOrder.Map.of_list ["R",["a",AST.TString;"b",AST.TList AST.TBlob;"c",AST.TDict (AST.TInt64,AST.TInt64)];"Rec",["next",AST.TRecord ("Rec",[])];"Child",["x",AST.TTuple [AST.TString;AST.TList AST.TString]];"Large",List.init 35 (fun index -> string_of_int index,AST.TString)] in
 let info payloads unary={MemoryModel.typeParams=[];MemoryModel.payloads=payloads;MemoryModel.unaryPayloadTags=MemoryModel.IntSet.of_list unary} in
 let sums=StringOrder.Map.of_list ["None",info [] [];"Nullable",info [0,None;1,Some AST.TString] [1];"S",info [0,None;1,Some (AST.TTuple [AST.TString;AST.TList AST.TString]);65536,Some (AST.TRecord ("Child",[]))] [];"RecSum",info [0,None;1,Some (AST.TTuple [AST.TString;AST.TSum ("RecSum",[])] )] []] in
 let primitives=[AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TInt128;AST.TInt;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TUInt128;AST.TBool;AST.TFloat64;AST.TString;AST.TBlob;AST.TChar;AST.TDateTime;AST.TUnit;AST.TNever;AST.TInternalRawPtr;AST.TVar source;AST.TInferenceVar (source,source);AST.TFunction ([AST.TString],AST.TBool);AST.TRecord ("missing",[]);AST.TRecord ("R",[]);AST.TRecord ("Rec",[]);AST.TRecord ("Child",[]);AST.TRecord ("Large",[]);AST.TSum ("missing",[]);AST.TSum ("None",[]);AST.TSum ("Nullable",[]);AST.TSum ("S",[]);AST.TSum ("RecSum",[]);AST.TTuple [];AST.TTuple [AST.TString;AST.TList AST.TString]] in
 let types=primitives@List.concat_map (fun typ -> [AST.TList typ;AST.TList (AST.TList typ);AST.TStream typ;AST.TDict (AST.TInt64,typ);AST.TDict (AST.TString,typ);AST.TTuple [typ;AST.TString];AST.TTuple [AST.TTuple [typ;AST.TBlob];AST.TTuple [AST.TList typ;AST.TDict (AST.TString,typ)]]]) primitives in


 let child=RootRelease (16,GenericHeap,FixedBlockPayloadRelease (16,[FieldRelease (0,DynamicBufferRelease DynamicStringBuffer);FieldRelease (8,RecursiveRelease (AST.TRecord (source,[])))])) in
 let releaseFields=List.mapi (fun index p->FieldRelease (index*8,p)) (simple@listPlans@[child]) in
 let variants=[{tag=0;fieldReleases=[]};{tag=1;fieldReleases=releaseFields};{tag=1;fieldReleases=[FieldRelease (-1,child)]};{tag=2147483647;fieldReleases=[FieldRelease (8,RecursiveRelease (AST.TSum (source,[])))]}] in
 let plans=plans@[child;RootRelease (32,GenericHeap,FixedBlockPayloadRelease (32,releaseFields));RootRelease (32,GenericHeap,BoxedSumPayloadRelease (32,[],variants))] in
 let observing f=let trace=ref [] and count=ref 0 in
  let select typ=trace:=typ:: !trace;incr count;"callback-"^string_of_int !count in
  let result=call code (fun ()->f select) in tuple [result;list SemanticAST.semanticType (List.rev !trace)] in
 let cases=list (fun enabled->
  let ctx={X64CodeGenTypes.functionName=source;stackSize=32;usedCalleeSaved=[LIR.X19];enableLeakCheck=enabled;recordRegistry=records;sumShapeRegistry=sums;functionNames=FunctionIdMap.empty} in
  let workers=list (fun preserve->
   let fields=list (fun fs->observing (fun select->E.genFieldReleases select preserve ctx fs)) [ [];releaseFields;List.rev releaseFields] in
   let variantCases=list (fun vs->observing (fun select->E.genBoxedSumVariantFieldReleases select preserve ctx vs)) [ [];variants;List.rev variants;[{tag=0;fieldReleases=[]}]] in
   let planned=list (fun p->list (fun action->observing action) [(fun select->E.genFixedBlockFieldReleases select preserve ctx (Some p));(fun select->E.genFixedBlockFieldRelease select preserve ctx (-1) 16 p);(fun select->E.genRefCountDecGenericWithPlanUsing select preserve ctx X86_64.R8 16 (Some p))]) plans in
   tuple [fields;variantCases;planned]) [false;true] in
  let registerCases=list (fun reg->list (fun size->list (fun action->call code action) [(fun ()->E.genRefCountIncGeneric reg size);(fun ()->E.genRefCountDecGenericWithPlan ctx reg size None);(fun ()->E.genRefCountDecGeneric ctx reg size (Some {releasePlanCacheKey=Some source;releasePlan=Some child;sourceType=Some AST.TString}));(fun ()->E.genRefCountDecStream ctx reg None)]) [-2147483648;-1;0;8;24;255;256;2147483647]) (Array.to_list X64EncodingFixtures.regValues) in
  let recursive=list (fun typ->call code (fun ()->E.generateRecursiveNominalRefCountDecHelper enabled records sums typ)) types in
  tuple [workers;registerCases;recursive;call code (fun ()->E.generateStreamRefCountDecHelper ctx)]) [false;true] in
 let metadata p=Some {releasePlanCacheKey=None;releasePlan=Some p;sourceType=None} in
 let func instructions=let block={LIR.label=LIR.Label "entry";instrs=instructions;terminator=LIR.Ret} in
  {LIR.id=AST.functionId 0L;name=source;typedParams=[];cfg={LIR.entry=block.LIR.label;blocks=LIR.LabelMap.singleton block.LIR.label block};stackSize=0;usedCalleeSaved=[];codegenFacts=None} in
 let dec p=LIR.RefCountDec (LIR.Virtual 0,16,LIR.GenericHeap,metadata p) in
 let batches=[[];[func []];[func (List.map dec plans)];[func (List.map (fun p->LIR.RefCountInc (LIR.Virtual 0,16,LIR.GenericHeap,metadata p)) plans)];[func (List.map dec (List.rev plans));func (List.map dec plans)]] in
 let recursiveTypes=list (fun fs->call (fun set->`Assoc ["set",list SemanticAST.semanticType (MemoryPlanning.SemanticTypeSet.elements set)]) (fun ()->E.recursiveReleaseTypesInFunctions fs)) batches in
 tuple [cases;recursiveTypes]
