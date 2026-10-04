[@@@warning "-4"]
(* Complete x64 program assembly, helper dependencies, resolution and ELF bytes. *)
open Dark_compiler
module G=CodeGen_X86_64
module R=X86_64_Resolve
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let result f=function Ok v->SemanticJson.union "FSharpResult" "Ok" [f v]|Error e->SemanticJson.union "FSharpResult" "Error" [SemanticJson.string e]
let observe source=
 let make id name instructions=
  let label=LIR.Label (name^"_entry") in
  let block={LIR.label;instrs=instructions;terminator=LIR.Ret} in
  {LIR.id=AST.functionId id;name;typedParams=[];cfg={LIR.entry=label;blocks=LIR.LabelMap.singleton label block};stackSize=32;usedCalleeSaved=[LIR.X19];codegenFacts=None} in
 let callee=make 1L "fn" [] in
 let instructions=LIRFixtures.instructionsWithRegisters source (LIR.Physical LIR.X19) (LIR.FPhysical LIR.D0) (LIR.Imm 1L) AST.TString in
 let dynamic=MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer in
 let child=MemoryModel.RootRelease (8,MemoryModel.GenericHeap,MemoryModel.FixedBlockPayloadRelease (8,[MemoryModel.FieldRelease (0,dynamic)])) in
 let fields=List.init 25 (fun index -> MemoryModel.FieldRelease (index*8,dynamic)) in
 let plans=[MemoryModel.RootRelease (200,MemoryModel.GenericHeap,MemoryModel.FixedBlockPayloadRelease (200,fields));MemoryModel.RootRelease (200,MemoryModel.GenericHeap,MemoryModel.BoxedSumPayloadRelease (200,fields,[{MemoryModel.tag=1;fieldReleases=fields}]));MemoryModel.RootRelease (8,MemoryModel.TaggedList,MemoryModel.TaggedListPayloadRelease (MemoryModel.RootRelease (16,MemoryModel.DictHeap,MemoryModel.DictPayloadRelease (dynamic,child))));MemoryModel.RootRelease (16,MemoryModel.DictHeap,MemoryModel.DictPayloadRelease (dynamic,child));MemoryModel.RootRelease (16,MemoryModel.DictHeap,MemoryModel.DictPayloadRelease (dynamic,MemoryModel.RootRelease (8,MemoryModel.TaggedList,MemoryModel.TaggedListPayloadRelease dynamic)))] in
 let kinds=[LIR.GenericHeap;LIR.GenericHeap;LIR.TaggedList;LIR.DictHeap;LIR.DictHeap] in
 let releaseInstructions=List.map2 (fun kind plan -> LIR.RefCountDec (LIR.Physical LIR.X19,200,kind,Some {MemoryModel.releasePlanCacheKey=None;releasePlan=Some plan;sourceType=Some AST.TString})) kinds plans in
 let catalog=List.map (fun instruction -> [make 0L "_start" [instruction];callee]) (instructions@releaseInstructions) in
 let saves=make 0L "_start" [LIR.SaveRegs ([LIR.X2;LIR.X3],[LIR.D2]);LIR.Call (LIR.Physical LIR.X0,AST.functionId 1L,[]);LIR.RestoreRegs ([LIR.X2;LIR.X3],[LIR.D2])] in
 let catalog=[[];[make 0L "_start" []];[callee;make 0L "_start" [LIR.PrintString source]];[saves;callee];[make 0L "_start" [];make 2L "_start" []];[make 2L source []];[make 0L "_start" [LIR.Mov (LIR.Virtual 0,LIR.Imm 1L)]]]@catalog in
 let variants=StringOrder.Map.singleton "Option" {LIR.typeParams=[];variants=[{LIR.name="None";tag=0;payload=None;fieldCount=0};{LIR.name="Some";tag=1;payload=Some AST.TString;fieldCount=1}]} in
 let records=StringOrder.Map.singleton "R" ["field",AST.TString] in

 let types=[AST.TString;AST.TBlob;AST.TInt;AST.TList AST.TString;AST.TList (AST.TList AST.TString);AST.TDict (AST.TString,AST.TList AST.TString);AST.TDict (AST.TString,AST.TDict (AST.TString,AST.TList AST.TString));AST.TList (AST.TDict (AST.TString,AST.TList AST.TString));AST.TList (AST.TTuple [AST.TString;AST.TList AST.TString]);AST.TFunction ([AST.TInt64],AST.TString);AST.TStream AST.TString;AST.TTuple [AST.TString;AST.TList AST.TString;AST.TDict (AST.TString,AST.TString)];AST.TRecord ("R",[]);AST.TSum ("Option",[])] in
 let captured=List.map (fun typ->let f={callee with LIR.typedParams=[{LIR.reg=LIR.Physical LIR.X0;typ=AST.TTuple [AST.TInternalRawPtr;typ]}]} in [make 0L "_start" [LIR.ClosureAlloc (LIR.Physical LIR.X19,AST.functionId 1L,[]);LIR.RefCountInc (LIR.Physical LIR.X19,8,LIR.ClosureHeap,None);LIR.RefCountDec (LIR.Physical LIR.X19,8,LIR.ClosureHeap,None)];f]) types in
 let slots=List.map (fun typ->[make 0L "_start" [LIR.RawSlotInit (LIR.Physical LIR.X19,LIR.Physical LIR.X20,LIR.Physical LIR.X21,typ)];callee]) types in
 let released=List.filter_map (fun typ->match X64ReleaseSelection.tryRcReleasePlanOfType records (X64CodeGenTypes.rcSumShapeRegistryFromVariantRegistry variants) typ with Some (MemoryModel.RootRelease (size,kind,_) as plan)->let kind=match kind with MemoryModel.GenericHeap->LIR.GenericHeap|MemoryModel.TaggedList->LIR.TaggedList|MemoryModel.DictHeap->LIR.DictHeap|MemoryModel.ClosureHeap->LIR.ClosureHeap|MemoryModel.StreamHeap->LIR.StreamHeap in Some [make 0L "_start" [LIR.RefCountDec (LIR.Physical LIR.X19,size,kind,Some {MemoryModel.releasePlanCacheKey=None;releasePlan=Some plan;sourceType=Some typ})];callee] |Some (MemoryModel.NoReleasePlan|MemoryModel.DynamicBufferRelease _|MemoryModel.RecursiveRelease _)|None->None) types in
 let cli=List.map (fun op->[make 0L "_start" [LIR.CliNative (LIR.Physical LIR.X19,op,[])];callee]) [LIR.GetArgv;LIR.GetEnvironmentPacked;LIR.DirectoryCurrent;LIR.GetEnv;LIR.Execute;LIR.RunProcess;LIR.SpawnProcess;LIR.ProcessIO;LIR.TerminateProcess] in
 let missing=[make 0L "_start" [LIR.ClosureAlloc (LIR.Physical LIR.X19,AST.functionId (-1L),[])]] in
 let catalog=catalog@captured@slots@released@cli@[missing] in
 let code enabled xs=
  let pool=R.collectStringPool xs in
  let encoded=result (fun r->let patched=R.patchDataLabels r (R.dataLabelOffsets 120 (Bytes.length r.R.machineCode) pool) 120 in result (fun r->tuple [X64EncodingObservation.bytes r.R.machineCode;X64EncodingObservation.bytes (Binary_Generation_ELF_X86_64.createExecutableWithPools r.R.machineCode pool LiteralPool.emptyFloatPool enabled 0)]) patched) (R.resolveAndEncode xs) in
  tuple [list MachineISAObservation.x64Instr xs;ARMEncodingObservation.stringPool pool;encoded] in
 list (fun enabled->list (fun functions->try tuple [`Bool false;result (code enabled) (G.translateProgram (LIR.Program (functions,variants,records)) enabled)] with Failure e|Invalid_argument e->tuple [`Bool true;SemanticJson.string e]) catalog) [false;true]
