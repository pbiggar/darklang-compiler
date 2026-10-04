(* Complete x64 closure layout maps and capture destruction. *)
open Dark_compiler
module E=X64ClosureReferenceCounts
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let code=list MachineISAObservation.x64Instr
let call encoder f=try tuple [`Bool false;encoder (f ())] with Failure error | Invalid_argument error->tuple [`Bool true;SemanticJson.string error]
let stringMap encode values=`Assoc ["map",list (fun (name,value)->tuple [SemanticJson.string name;encode value]) (StringOrder.Map.bindings values)]
let ids values=`Assoc ["map",list (fun (id,value)->tuple [ProductionMIR.functionId id;SemanticJson.int32 value]) (FunctionIdMap.toList values)]
let observe source=
 let records=StringOrder.Map.of_list ["R",["a",AST.TString;"b",AST.TList AST.TBlob;"c",AST.TDict (AST.TInt64,AST.TInt64)];"Rec",["next",AST.TRecord ("Rec",[])];"Child",["x",AST.TTuple [AST.TString;AST.TList AST.TString]];"Large",List.init 35 (fun index -> string_of_int index,AST.TString)] in
 let info payloads unary={MemoryModel.typeParams=[];MemoryModel.payloads=payloads;MemoryModel.unaryPayloadTags=MemoryModel.IntSet.of_list unary} in
 let sums=StringOrder.Map.of_list ["None",info [] [];"Nullable",info [0,None;1,Some AST.TString] [1];"S",info [0,None;1,Some (AST.TTuple [AST.TString;AST.TList AST.TString]);65536,Some (AST.TRecord ("Child",[]))] [];"RecSum",info [0,None;1,Some (AST.TTuple [AST.TString;AST.TSum ("RecSum",[])] )] []] in
 let primitives=[AST.TInt8;AST.TInt16;AST.TInt32;AST.TInt64;AST.TInt128;AST.TInt;AST.TUInt8;AST.TUInt16;AST.TUInt32;AST.TUInt64;AST.TUInt128;AST.TBool;AST.TFloat64;AST.TString;AST.TBlob;AST.TChar;AST.TDateTime;AST.TUnit;AST.TNever;AST.TInternalRawPtr;AST.TVar source;AST.TInferenceVar (source,source);AST.TFunction ([AST.TString],AST.TBool);AST.TRecord ("missing",[]);AST.TRecord ("R",[]);AST.TRecord ("Rec",[]);AST.TRecord ("Child",[]);AST.TRecord ("Large",[]);AST.TSum ("missing",[]);AST.TSum ("None",[]);AST.TSum ("Nullable",[]);AST.TSum ("S",[]);AST.TSum ("RecSum",[]);AST.TTuple [];AST.TTuple [AST.TString;AST.TList AST.TString]] in
 let types=primitives@List.concat_map (fun typ -> [AST.TList typ;AST.TList (AST.TList typ);AST.TStream typ;AST.TDict (AST.TInt64,typ);AST.TDict (AST.TString,typ);AST.TTuple [typ;AST.TString];AST.TTuple [AST.TTuple [typ;AST.TBlob];AST.TTuple [AST.TList typ;AST.TDict (AST.TString,typ)]]]) primitives in

 let sizes=StringOrder.Map.of_list [source,24;"😀",8;"",16;"a",256] in
 let captures=StringOrder.Map.of_list [source,[AST.TString;AST.TInt];"😀",[];"",[AST.TTuple [AST.TString]]] in
 let contexts=list (fun enabled->
  let base=list (fun f->call code f) [(fun ()->E.generateClosureRefCountIncHelper sizes);(fun ()->E.generateClosureRefCountDecHelper enabled records sums sizes captures)] in
  let sizeCases=list (fun size->let sizes=StringOrder.Map.of_list [source,size;"😀",8;"",16;"a",size] in list (fun f->call code f) [(fun ()->E.generateClosureRefCountIncHelper sizes);(fun ()->E.generateClosureRefCountDecHelper enabled records sums sizes captures)]) [-2147483648;-1;0;8;16;248;255;256;2147483647] in
  let captureCases=list (fun ts->call code (fun ()->E.generateClosureRefCountDecHelper enabled records sums sizes (StringOrder.Map.singleton source ts))) ([]::types::List.map (fun typ->[typ]) types@List.map (fun count->List.init count (fun _->AST.TString)) [31;32;64;129]) in
  tuple [base;sizeCases;captureCases]) [false;true] in
 let make id name params instructions=
  let block={LIR.label=LIR.Label "entry";instrs=instructions;terminator=LIR.Ret} in
  let before={block with LIR.label=LIR.Label "😀";instrs=[LIR.ClosureAlloc (LIR.Virtual 0,AST.functionId 1L,[LIR.Imm 1L])]} in
  {LIR.id=AST.functionId id;name;typedParams=params;cfg={LIR.entry=block.LIR.label;blocks=LIR.LabelMap.of_seq (List.to_seq [before.LIR.label,before;block.LIR.label,block])};stackSize=0;usedCalleeSaved=[];codegenFacts=None} in
 let funcs=List.mapi (fun index typ->make (Int64.of_int index) (if index mod 2=0 then source else "😀") [{LIR.reg=LIR.Virtual index;typ};{LIR.reg=LIR.Virtual (-1);typ=AST.TTuple [AST.TString]}] [LIR.ClosureAlloc (LIR.Virtual index,AST.functionId 1L,List.init (index mod 4) (fun _->LIR.Imm 1L));LIR.ClosureAlloc (LIR.Virtual index,AST.functionId (-1L),[])]) (primitives@[AST.TTuple [];AST.TTuple [AST.TInternalRawPtr];AST.TTuple [AST.TInternalRawPtr;AST.TString;AST.TInt]]) in
 let batches=[[];[make 0L source [] []];funcs;List.rev funcs;funcs@funcs] in
 let layouts=list (fun fs->tuple [call ids (fun ()->E.closurePayloadSizesFromAllocs fs);call (stringMap (list SemanticAST.semanticType)) (fun ()->E.closureCaptureTypesFromParams fs);call (stringMap SemanticJson.int32) (fun ()->E.closurePayloadSizesFromParams fs)]) batches in
 tuple [contexts;layouts]
