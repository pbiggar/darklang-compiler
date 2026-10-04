(* Complete program chunks, helper/cache arguments, callbacks and emitted binaries. *)
open Dark_compiler
module G=Backend_Arm64_CodeGen
module C=ARM64CodeGenTypes
module J=ProductionLIR
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let array f xs=`List (Array.to_list (Array.map f xs))
let str=SemanticJson.string
let code=list MachineISAObservation.symInstr
let u64 value=`Assoc ["kind",`String "uint64";"value",`String (Printf.sprintf "%Lu" value)]
let word value=`Assoc ["kind",`String "uint32";"value",`String (Printf.sprintf "%lu" value)]
let map f value=`Assoc ["map",list (fun (key,value) -> tuple [str key;f value]) (StringOrder.Map.bindings value)]
let set f values=`Assoc ["set",list f values]
let metadata (value:C.arm64ProgramMetadata)=
 let facts=value.C.facts in
 let alloc=SemanticJson.union "FunctionIdMap" "FunctionIdMap" [`Assoc ["map",list (fun (id,size) -> tuple [u64 (AST.functionIdValue id);SemanticJson.int32 size]) (FunctionIdMap.toList facts.C.closurePayloadSizesFromAllocs)]] in
 let facts=SemanticJson.record "Arm64ProgramFacts" ["ClosurePayloadSizesFromParams",map SemanticJson.int32 facts.C.closurePayloadSizesFromParams;"ClosurePayloadSizesFromAllocs",alloc;"ClosureCaptureTypes",map (list SemanticAST.semanticType) facts.C.closureCaptureTypes;"RecursiveReleaseTypes",set SemanticAST.semanticType (MemoryPlanning.SemanticTypeSet.elements facts.C.recursiveReleaseTypes);"CliArgvHelperLabels",set str (StringOrder.Set.elements facts.C.cliArgvHelperLabels);"NeedsCliExecuteHelper",`Bool facts.C.needsCliExecuteHelper;"NeedsCliRunProcessHelper",`Bool facts.C.needsCliRunProcessHelper;"NeedsCliProcessLifecycleHelpers",`Bool facts.C.needsCliProcessLifecycleHelpers;"NeedsRuntimeErrorHelper",`Bool facts.C.needsRuntimeErrorHelper] in
 SemanticJson.record "Arm64ProgramMetadata" ["Facts",facts;"RcHelperRequirements",J.arm64RcHelperRequirements value.C.rcHelperRequirements]
let helperKey (value:G.helperCacheKey)=SemanticJson.record "HelperCacheKey" [
 "ClosurePayloadSizesFromParams",list (fun (name,size) -> tuple [str name;SemanticJson.int32 size]) value.G.closurePayloadSizesFromParams;
 "ClosurePayloadSizesFromAllocs",list (fun (id,size) -> tuple [SemanticJson.union "FunctionId" "FunctionId" [u64 (AST.functionIdValue id)];SemanticJson.int32 size]) value.G.closurePayloadSizesFromAllocs;
 "ClosureCaptureTypes",list (fun (name,types) -> tuple [str name;list SemanticAST.semanticType types]) value.G.closureCaptureTypes;
 "RecursiveReleaseTypes",list SemanticAST.semanticType value.G.recursiveReleaseTypes;"CliArgvHelperLabels",list str value.G.cliArgvHelperLabels;
 "NeedsCliExecuteHelper",`Bool value.G.needsCliExecuteHelper;"NeedsCliRunProcessHelper",`Bool value.G.needsCliRunProcessHelper;"NeedsCliProcessLifecycleHelpers",`Bool value.G.needsCliProcessLifecycleHelpers;"NeedsRuntimeErrorHelper",`Bool value.G.needsRuntimeErrorHelper;
 "ListDecHelperLabels",list str value.G.listDecHelperLabels;"PlannedListDecHelpers",list (fun (name,size) -> tuple [str name;SemanticJson.int32 size]) value.G.plannedListDecHelpers;
 "PlannedGenericDecHelperLabels",list str value.G.plannedGenericDecHelperLabels;"PlannedDictDecHelperLabels",list str value.G.plannedDictDecHelperLabels;"DictDecHelperLabels",list str value.G.dictDecHelperLabels;
 "NeedsListRcIncHelper",`Bool value.G.needsListRcIncHelper;"NeedsDictRcIncHelper",`Bool value.G.needsDictRcIncHelper;"NeedsClosureRcIncHelper",`Bool value.G.needsClosureRcIncHelper;"NeedsClosureRcDecHelper",`Bool value.G.needsClosureRcDecHelper;"NeedsStreamRcDecHelper",`Bool value.G.needsStreamRcDecHelper]
let chunk (value:G.generatedChunk)=SemanticJson.record "GeneratedChunk" ["InstructionParts",list code value.G.instructionParts;"ReusableAcrossCompilations",`Bool value.G.reusableAcrossCompilations]
let program value=tuple [list chunk (G.generatedProgramChunks value);code (G.generatedProgramInstructions value)]
let result=function Ok value->SemanticJson.union "FSharpResult" "Ok" [program value]|Error error->SemanticJson.union "FSharpResult" "Error" [str error]
let call encode action=try tuple [`Bool false;encode (action ())] with Failure _ | Invalid_argument _ | Not_found -> tuple [`Bool true]
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
 let run target leak mode functions=
  let trace=ref [] in let add value=trace:=value:: !trace in
  let identity=Obj.repr (ref ()) in
  let phase name elapsed=add (tuple [str "phase";str name;`Bool (elapsed>=0.0)]) in
  let expand name opcode detail count ticks=add (tuple [str "expand";str name;str opcode;str detail;SemanticJson.int32 count;`Bool (Int64.compare ticks 0L>=0)]) in
  let functionCache func generate=add (tuple [str "function";J.functionDef func]);if mode=4 then Error "cached function failure" else generate () in
  let refinementCache func writes generate=add (tuple [str "refine";J.functionDef func;(SemanticJson.union "FunctionIdMap" "FunctionIdMap" [`Assoc ["map",list (fun (id,value) -> tuple [u64 (AST.functionIdValue id);SemanticJson.record "Writes" ["Ints",u64 value.ARM64CalleeClobbers.ints;"Floats",u64 value.ARM64CalleeClobbers.floats]]) (FunctionIdMap.toList writes)]])]);generate () in
  let metadataCache _ funcs generate=add (tuple [str "metadata-input";list J.functionDef funcs]);let value=generate () in add (tuple [str "metadata-output";metadata value]);value in
  let helperCache key generate=add (tuple [str "helper";helperKey key]);generate () in
  let groupCache _ funcs generate=add (tuple [str "group";list J.functionDef funcs]);generate () in
  let action ()=
   let prepared=ARM64PrepareFunctions.prepareARM64Program (LIR.Program (functions,variants,records)) in
   let LIR.Program (preparedFunctions,_,_)=prepared in
   let sorted=match List.partition (fun (f:LIR.functionDef) -> f.LIR.name="_start") preparedFunctions with first::_,rest->first::rest|[],_->preparedFunctions in
   let groups=if mode=2 || mode=3 then List.mapi (fun index func -> {G.contextIdentity=identity;reusableAcrossCompilations=index mod 2=0;functions=[func]}) (if mode=3 then List.rev sorted else sorted) else [] in
   let metadataGroups=if mode=2 then List.map (fun func -> ({G.contextIdentity=identity;functions=[func]}:G.metadataGroup)) preparedFunctions else [] in
   let options={C.defaultOptions with C.enableLeakCheck=leak;disableFreeList=mode=2} in
   G.generateARM64WithOptionsAndCaches target options (if mode=2 then Some (C.rcSumShapeRegistryFromVariantRegistry variants) else None) (if mode=2 then Some FunctionIdMap.empty else None) (if mode=0 then None else Some functionCache) (if mode=0 then None else Some refinementCache) (if mode=2 || mode=3 then Some groupCache else None) groups (if mode=0 then None else Some metadataCache) (if mode=0 then None else Some helperCache) metadataGroups (Some expand) (Some phase) prepared in
  let output=call result action in
  tuple [output;`List (List.rev !trace)] in
 let cases=list (fun target -> list (fun leak -> list (fun functions -> list (fun mode -> run target leak mode functions) [0;1;2]) catalog) [false;true]) [ARM64.targetConfigFor Platform.LinuxARM64;ARM64.targetConfigFor Platform.MacOSARM64] in
 let failures=list (fun functions -> list (fun mode -> run (ARM64.targetConfigFor Platform.LinuxARM64) false mode functions) [3;4]) (List.filteri (fun index _ -> index<7) catalog) in
 let unprepared=list (fun functions -> list (fun target -> call result (fun () -> G.generateARM64 target (LIR.Program (functions,variants,records)))) [ARM64.targetConfigFor Platform.LinuxARM64;ARM64.targetConfigFor Platform.MacOSARM64]) (List.filteri (fun index _ -> index<7) catalog) in
 tuple [cases;failures;unprepared]
let observeEmit source=
 let make id name instructions=
  let label=LIR.Label (name^"_entry") in
  let block={LIR.label;instrs=instructions;terminator=LIR.Ret} in
  {LIR.id=AST.functionId id;name;typedParams=[];cfg={LIR.entry=label;blocks=LIR.LabelMap.singleton label block};stackSize=32;usedCalleeSaved=[LIR.X19];codegenFacts=None} in
 let callee=make 1L "fn" [] in
 let emitCases=list (fun os -> list (fun mode ->
  let target=ARM64.targetConfigFor (if os=Platform.Linux then Platform.LinuxARM64 else Platform.MacOSARM64) in
  let trace=ref [] in let add value=trace:=value:: !trace in
  let phase name elapsed=add (tuple [str "phase";str name;`Bool (elapsed>=0.0)]) in
  let preparePart instructions generate=add (tuple [str "part";code instructions]);generate () in
  let prepareGroup instructions generate=add (tuple [str "parts";list code instructions]);generate () in
  let functionCache _ generate=generate () in
  let helperCache _ generate=generate () in
  let groupCache _ _ generate=generate () in
  let emit ()=
   let prepared=ARM64PrepareFunctions.prepareARM64Program (LIR.Program ([make 0L "_start" [LIR.PrintString source;LIR.FLoad (LIR.FPhysical LIR.D0,1.5);LIR.PrintFloatNoNewline (LIR.FPhysical LIR.D0)];callee],StringOrder.Map.empty,StringOrder.Map.empty)) in
   let LIR.Program (functions,_,_)=prepared in
   let groups=if mode=2 then [{G.contextIdentity=Obj.repr (ref ());reusableAcrossCompilations=true;functions}] else [] in
   match G.generateARM64WithOptionsAndCaches target C.defaultOptions None None (if mode=0 then None else Some functionCache) None (if mode=2 then Some groupCache else None) groups None (if mode=0 then None else Some helperCache) [] None None prepared with
   | Error error->failwith error
   | Ok generated->let emitted=ControlledEmit.emitBinary generated os false (Some preparePart) (Some prepareGroup) (Some phase) in SemanticJson.record "EmitResult" ["MachineCode",array word emitted.ControlledEmit.machineCode;"Binary",X64EncodingObservation.bytes emitted.ControlledEmit.binary] in
  let output=call Fun.id emit in tuple [output;`List (List.rev !trace)]) [0;1;2]) [Platform.Linux;Platform.MacOS] in
 emitCases
