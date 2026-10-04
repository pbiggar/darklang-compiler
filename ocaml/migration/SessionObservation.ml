(* Observe every compilation-session cache and its lifetime and metrics. *)
open Dark_compiler
module C=CompilationCacheIdentity
module S=CompilationSession
module F=SpecializationIdentity.FunctionSet
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let i=SemanticJson.int32
let str=SemanticJson.string
let result f=function Ok value->SemanticJson.union "FSharpResult" "Ok" [f value]|Error message->SemanticJson.union "FSharpResult" "Error" [str message]
let ownership (value:OwnedIR.callSignature)=
 let parameter=function OwnedIR.UnmanagedCallParameter->"UnmanagedCallParameter"|OwnedIR.BorrowedCallParameter->"BorrowedCallParameter"|OwnedIR.ConsumedCallParameter->"ConsumedCallParameter"|OwnedIR.UniqueCallParameter->"UniqueCallParameter" in
 let output=match value.OwnedIR.result with OwnedIR.UnmanagedCallResult->SemanticJson.union "CallResultOwnership" "UnmanagedCallResult" []|OwnedIR.BorrowedCallResult n->SemanticJson.union "CallResultOwnership" "BorrowedCallResult" [i n]|OwnedIR.ProducedCallResult->SemanticJson.union "CallResultOwnership" "ProducedCallResult" []|OwnedIR.UniqueProducedCallResult->SemanticJson.union "CallResultOwnership" "UniqueProducedCallResult" [] in
 SemanticJson.record "CallSignature" ["Parameters",list (fun value->SemanticJson.union "CallParameterOwnership" (parameter value) []) value.OwnedIR.parameters;"Result",output]
let bindings encoder values=list (fun (key,value)->tuple [ProductionMIR.functionId key;encoder value]) (FunctionIdMap.toList values)
let snapshot (s:S.compilationSession)=tuple [
 i s#cachedArm64FunctionCount;
 i s#cachedArm64HelperCount;
 i s#cachedAnfDependencyCount;
 i s#cachedCompiledDependencyCount;
 i s#cachedMirOptimizationCount;
 i s#cachedAllocatedLirFunctionCount;
 i s#cachedStdlibReachabilityCount;
 i s#cachedMirRegistryProjectionCount;
 i s#cachedArm64MetadataGroupCount;
 i s#cachedArm64FunctionGroupCount;
 i s#cachedArm64EmissionChunkCount;
 i s#cachedArm64ReleasePlanSummaryCount;
 i s#cachedJsonPlanCount;
 i s#jsonPlanHitCount;
 i s#jsonPlanMissCount;
 i s#anfDependencyHitCount;
 i s#anfDependencyMissCount;
 i s#compiledDependencyHitCount;
 i s#compiledDependencyMissCount;
 i s#mirOptimizationHitCount;
 i s#mirOptimizationMissCount;
 i s#allocatedLirFunctionHitCount;
 i s#allocatedLirFunctionMissCount;
 i s#stdlibReachabilityHitCount;
 i s#stdlibReachabilityMissCount;
 i s#mirRegistryProjectionHitCount;
 i s#mirRegistryProjectionMissCount;
 i s#arm64CodegenHitCount;
 i s#arm64CodegenMissCount;
 i s#arm64StartCodegenHitCount;
 i s#arm64MetadataGroupHitCount;
 i s#arm64MetadataGroupMissCount;
 i s#arm64FunctionGroupHitCount;
 i s#arm64FunctionGroupMissCount;
 i s#arm64HelperHitCount;
 i s#arm64HelperMissCount;
 i s#arm64ReleasePlanSummaryHitCount;
 i s#arm64ReleasePlanSummaryMissCount;
]
let observe source=
 let fid n=AST.functionId (Int64.of_int n) in
 let make id name instructions=let label=LIR.Label (name^"_entry") in {LIR.id=fid id;name;typedParams=[];cfg={LIR.entry=label;blocks=LIR.LabelMap.singleton label {LIR.label;instrs=instructions;terminator=LIR.Ret}};stackSize=32;usedCalleeSaved=[LIR.X19];codegenFacts=None} in
 let base=make 1 source [LIR.PrintString source] in
 let clone={base with LIR.stackSize=32} in
 let functions=[base;clone;{base with LIR.stackSize=16};make 0 "_start" [];make 0 "__dark_compiler_program_entry" [];LIR.attachFunctionCodegenFacts base] in
 let mirLabel=MIR.Label "entry" in
 let mir={MIR.id=fid 1;name=source;typedParams=[];returnType=AST.TInt64;cfg={MIR.entry=mirLabel;blocks=MIR.LabelMap.singleton mirLabel {MIR.label=mirLabel;instrs=[];terminator=MIR.Ret (MIR.Int64Const 1L)}};floatRegs=MIR.IntSet.empty} in
 let contexts=[Obj.repr (ref 1);Obj.repr (ref 2)] in
 let targets=[ARM64.targetConfigFor Platform.LinuxARM64;ARM64.targetConfigFor Platform.MacOSARM64] in
 let options=[ARM64CodeGenTypes.defaultOptions;{ARM64CodeGenTypes.defaultOptions with ARM64CodeGenTypes.enableLeakCheck=true};{ARM64CodeGenTypes.defaultOptions with ARM64CodeGenTypes.enableCoverage=true}] in
 let contract:OwnedIR.callSignature={OwnedIR.parameters=[OwnedIR.UnmanagedCallParameter;OwnedIR.BorrowedCallParameter;OwnedIR.ConsumedCallParameter;OwnedIR.UniqueCallParameter];result=OwnedIR.BorrowedCallResult 2} in
 let conversion={AST_to_ANF.functions=[];varGen=ANF.VarGen 7;ownershipContracts=FunctionIdMap.ofList [fid 1,contract]} in
 let registries=AST_to_ANF.buildRegistries (CheckedAST.emptySymbols ()) StringOrder.Map.empty [] StringOrder.Map.empty [] in
 let anfKeys=List.map (fun nonInlineableFunctionNames->{C.functions=[];localRegistries=registries;nonInlineableFunctionNames}) [F.empty;F.singleton (fid 1)] in
 let configs=List.map (fun options->({C.target=Platform.LinuxX86_64;options;nonInlineableFunctionNames=F.empty;knownSummaries=FunctionIdMap.empty}:C.compiledDependencyConfig)) [CompilerOptions.defaultOptions;{CompilerOptions.defaultOptions with CompilerOptions.enableCoverage=true}] in
 let dynamic=MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer in
 let plans=[MemoryModel.NoReleasePlan;dynamic;MemoryModel.RootRelease (8,MemoryModel.GenericHeap,MemoryModel.NoPayloadRelease);MemoryModel.RecursiveRelease AST.TString] in
 let releaseSummary={LIR.listDecHelperLabels=StringOrder.Set.singleton source;plannedListDecHelpers=StringOrder.Map.empty;expensiveGenericDecHelper=None;dictDecHelperLabels=StringOrder.Set.empty;plannedDictDecHelpers=StringOrder.Map.empty;needsClosureRcDecHelper=false;needsStreamRcDecHelper=false} in
 let requirements={LIR.listDecHelperLabels=StringOrder.Set.empty;plannedListDecHelpers=StringOrder.Map.empty;plannedGenericDecHelpers=StringOrder.Map.empty;plannedDictDecHelpers=StringOrder.Map.empty;dictDecHelperLabels=StringOrder.Set.empty;needsListRcIncHelper=false;needsDictRcIncHelper=false;needsClosureRcIncHelper=false;needsClosureRcDecHelper=false;needsStreamRcDecHelper=false;releasePlanSummaries=LIR.ReleasePlanSummaryMap.empty} in
 let metadata={ARM64CodeGenTypes.facts={ARM64CodeGenTypes.closurePayloadSizesFromParams=StringOrder.Map.empty;closurePayloadSizesFromAllocs=FunctionIdMap.empty;closureCaptureTypes=StringOrder.Map.empty;recursiveReleaseTypes=MemoryPlanning.SemanticTypeSet.empty;cliArgvHelperLabels=StringOrder.Set.empty;needsCliExecuteHelper=false;needsCliRunProcessHelper=false;needsCliProcessLifecycleHelpers=false;needsRuntimeErrorHelper=false};rcHelperRequirements=requirements} in
 let helper:Backend_Arm64_CodeGen.helperCacheKey={Backend_Arm64_CodeGen.closurePayloadSizesFromParams=[];closurePayloadSizesFromAllocs=[];closureCaptureTypes=[];recursiveReleaseTypes=[];cliArgvHelperLabels=[];needsCliExecuteHelper=false;needsCliRunProcessHelper=false;needsCliProcessLifecycleHelpers=false;needsRuntimeErrorHelper=false;listDecHelperLabels=[];plannedListDecHelpers=[];plannedGenericDecHelperLabels=[];plannedDictDecHelperLabels=[];dictDecHelperLabels=[];needsListRcIncHelper=false;needsDictRcIncHelper=false;needsClosureRcIncHelper=false;needsClosureRcDecHelper=false;needsStreamRcDecHelper=false} in
 let helpers=[helper;{helper with Backend_Arm64_CodeGen.recursiveReleaseTypes=[AST.TString]};{helper with Backend_Arm64_CodeGen.closureCaptureTypes=[source,[AST.TString]]}] in
 let instructions=[Symbolic.Label source] in
 let chunks=[instructions;instructions;List.map Fun.id instructions;[]] in
 let parts=[instructions;instructions] in
 let chunkGroups=[parts;parts;List.map Fun.id parts;[]] in
 let session=new S.compilationSession ~collectCodegenMetrics:true () in
 let events=ref [] in
 let calls=ref 0 in
 let generate label value ()=incr calls;events:=tuple [str "generate";str label;i !calls]:: !events;value in
 let emit label value=events:=tuple [str label;value;snapshot session]:: !events in
 emit "initial" (snapshot session);
 let firstIdentity=ref None in
 List.iter (fun _->List.iter (fun context->List.iter (fun key->let value=session#convertAnfDependencies context key (generate "anf" (Ok conversion)) in let encoded=result (fun (value,identity)->let same=match !firstIdentity with None->firstIdentity:=Some identity;false|Some first->identity==first in tuple [list ProductionANF.aNF_functionDef value.AST_to_ANF.functions;ProductionANF.aNF_varGen value.AST_to_ANF.varGen;bindings ownership value.AST_to_ANF.ownershipContracts;`Bool same]) value in emit "anf" encoded) anfKeys) contexts) [0;1];
 List.iter (fun _->List.iter (fun context->List.iter (fun config->let value=session#compileDependencies context config (generate "compiled" (Ok ([base],FunctionIdMap.ofList [fid 1,C.unknownSummary]))) in emit "compiled" (result (fun (functions,summaries)->tuple [list ProductionLIR.functionDef functions;bindings CacheIdentityObservation.summary summaries]) value)) configs) contexts) [0;1];
 List.iter (fun _->List.iter (fun options->let key={C.func=mir;options;effectFreeCalls=F.empty} in emit "mir" (ProductionMIR.functionDef (session#optimizeMirFunction key (generate "mir" mir)))) [MIROptimizationFacts.defaultOptimizeOptions;{MIROptimizationFacts.defaultOptimizeOptions with MIROptimizationFacts.enableLICM=false}]) [0;1];
 List.iter (fun _->List.iter (fun arch->List.iter (fun func->emit "allocate" (ProductionLIR.functionDef (session#allocateLirFunction arch func (generate "allocate" func)))) functions) [Platform.ARM64;Platform.X86_64]) [0;1];
 let callees=[FunctionIdMap.empty;FunctionIdMap.ofList [fid 1,ARM64CalleeClobbers.all]] in
 List.iter (fun _->List.iter (fun func->List.iter (fun callees->emit "call-aware" (ProductionLIR.functionDef (session#allocateCallAwareLirFunction func callees (generate "call-aware" func)));emit "refine" (ProductionLIR.functionDef (session#refineArm64LirFunction func callees (generate "refine" func)))) callees) functions) [0;1];
 let graph=FunctionIdMap.ofList [fid 2,F.singleton (fid 3);fid 3,F.singleton (fid 2);fid 4,F.empty] in
 let stdlib=[make 3 "third" [];make 2 "second" [];make 4 "fourth" [];make 3 "third-last" []] in
 List.iter (fun _->List.iter (fun context->List.iter (fun roots->let userGraph=FunctionIdMap.ofList [base.LIR.id,F.of_list (fid 99::roots)] in emit "reachable" (list ProductionLIR.functionDef (session#reachableStdlibFunctions context userGraph [base] graph stdlib))) [[fid 2];[fid 3];[fid 2;fid 3];[fid 4];[]]) contexts) [0;1];
 List.iter (fun _->List.iter (fun context->List.iter (fun localRecordFields->let value=session#projectMirRegistries context (StringOrder.Map.empty,StringOrder.Map.empty) StringOrder.Map.empty localRecordFields in emit "registries" (tuple [ProductionMIR.variantRegistry (fst value);ProductionMIR.recordRegistry (snd value)])) [StringOrder.Map.singleton source ["x",AST.TString];StringOrder.Map.singleton source ["y",AST.TBool]]) contexts) [0;1];
 List.iter (fun _->List.iter (fun context->List.iter (fun funcs->emit "metadata" (ARMProgramObservation.metadata (session#arm64MetadataGroup context funcs (generate "metadata" metadata)));List.iter (fun target->List.iter (fun options->let generated=Ok [{Backend_Arm64_CodeGen.instructionParts=[instructions];reusableAcrossCompilations=true}] in emit "group" (result (list ARMProgramObservation.chunk) (session#codegenFunctionGroup context target options funcs (generate "group" generated)))) options) targets) [[base];List.map Fun.id [base];[clone];[]]) contexts) [0;1];
 List.iter (fun _->List.iter (fun static->List.iter (fun plan->emit "release" (ProductionLIR.arm64ReleasePlanSummary (session#arm64ReleasePlanSummary static source plan (generate "release" releaseSummary)))) plans) [false;true]) [0;1];
 List.iter (fun _->List.iter (fun context->List.iter (fun target->List.iter (fun options->List.iter (fun func->emit "codegen" (result (list MachineISAObservation.symInstr) (session#codegenFunction context target options func (generate "codegen" (Ok instructions))))) functions) options) targets) contexts) [0;1];
 List.iter (fun _->List.iter (fun context->List.iter (fun target->List.iter (fun options->List.iter (fun helper->emit "helpers" (list MachineISAObservation.symInstr (session#arm64Helpers context target options helper (generate "helpers" instructions)))) helpers) options) targets) contexts) [0;1];
 List.iter (fun _->List.iter (fun chunk->emit "chunk" (ARMEncodingObservation.prepared (session#prepareArm64EmissionChunk chunk (generate "chunk" (ARM64_Encoding.prepareSymbolicChunk chunk))))) chunks;List.iter (fun parts->emit "chunks" (ARMEncodingObservation.prepared (session#prepareArm64EmissionChunkGroup parts (generate "chunks" (ARM64_Encoding.combinePreparedChunks (List.map ARM64_Encoding.prepareSymbolicChunk parts)))))) chunkGroups) [0;1];
 let errorContext=Obj.repr (ref 3) in
 let failAnf ()=let value=session#convertAnfDependencies errorContext (List.hd anfKeys) (generate "anf-error" (Error source)) in emit "anf-error" (result (fun _->assert false) value) in
 let failCompiled ()=emit "compiled-error" (result (fun _->assert false) (session#compileDependencies errorContext (List.hd configs) (generate "compiled-error" (Error source)))) in
 let failGroup ()=emit "group-error" (result (fun _->assert false) (session#codegenFunctionGroup errorContext (List.hd targets) (List.hd options) [base] (generate "group-error" (Error source)))) in
 let failCodegen ()=emit "codegen-error" (result (fun _->assert false) (session#codegenFunction errorContext (List.hd targets) (List.hd options) base (generate "codegen-error" (Error source)))) in
 List.iter (fun _->failAnf ();failCompiled ();failGroup ();failCodegen ()) [0;1];
 let recorder=Option.get session#arm64LirOpExpansionRecorder in
 recorder source "op" "detail" 3 1250L;recorder source "op" "detail" 4 2550L;recorder "second" "op" "" 2147483647 1L;recorder "second" "op" "" 1 (-1L);
 let opMetrics ()=list (fun (m:CompilerOptions.codegenLirOpMetric)->tuple [str m.CompilerOptions.functionName;str m.CompilerOptions.opcode;str m.CompilerOptions.detail;i m.CompilerOptions.occurrences;i m.CompilerOptions.symbolicInstructionCount;`Assoc ["kind",`String "int64";"value",`String (Int64.to_string (HostTimeSpan.ticks m.CompilerOptions.elapsed))]]) session#arm64LirOpMetrics in
 emit "op-metrics" (opMetrics ());
 emit "function-metrics" (list (fun (m:CompilerOptions.codegenFunctionMetric)->tuple [str m.CompilerOptions.functionName;`Bool (HostTimeSpan.ticks m.CompilerOptions.elapsed>=0L);i m.CompilerOptions.lirInstructionCount;i m.CompilerOptions.symbolicInstructionCount]) session#arm64CodegenMetrics);
 session#jsonPlanning#store source [];ignore (session#jsonPlanning#tryFind source);ignore (session#jsonPlanning#tryFind "missing");emit "json" (snapshot session);
 session#dispose;emit "disposed" (tuple [`Bool (Option.is_none session#arm64LirOpExpansionRecorder);`Bool (session#arm64CodegenMetrics=[]);opMetrics ()]);
 recorder source "after-dispose" "" 2 100L;emit "retained-recorder" (opMetrics ());
 emit "uncached-codegen" (result (list MachineISAObservation.symInstr) (session#codegenFunction (List.hd contexts) (List.hd targets) (List.hd options) base (generate "uncached-codegen" (Error source))));
 List.iter (fun _->failAnf ();failCompiled ();failGroup ();failCodegen ();
 emit "disposed-mir" (ProductionMIR.functionDef (session#optimizeMirFunction {C.func=mir;options=MIROptimizationFacts.defaultOptimizeOptions;effectFreeCalls=F.empty} (generate "disposed-mir" mir)));
 emit "disposed-allocation" (ProductionLIR.functionDef (session#allocateLirFunction Platform.ARM64 base (generate "disposed-allocation" base)));
 emit "disposed-call-aware" (ProductionLIR.functionDef (session#allocateCallAwareLirFunction base FunctionIdMap.empty (generate "disposed-call-aware" base)));
 emit "disposed-refine" (ProductionLIR.functionDef (session#refineArm64LirFunction base FunctionIdMap.empty (generate "disposed-refine" base)));
 emit "disposed-reachable" (list ProductionLIR.functionDef (session#reachableStdlibFunctions errorContext (FunctionIdMap.ofList [base.LIR.id,F.singleton (fid 3)]) [base] graph stdlib));
 let projected=session#projectMirRegistries errorContext (StringOrder.Map.empty,StringOrder.Map.empty) StringOrder.Map.empty (StringOrder.Map.singleton source ["z",AST.TUnit]) in emit "disposed-registries" (tuple [ProductionMIR.variantRegistry (fst projected);ProductionMIR.recordRegistry (snd projected)]);
 emit "disposed-metadata" (ARMProgramObservation.metadata (session#arm64MetadataGroup errorContext [base] (generate "disposed-metadata" metadata)));
 emit "disposed-release" (ProductionLIR.arm64ReleasePlanSummary (session#arm64ReleasePlanSummary false source dynamic (generate "disposed-release" releaseSummary)));
 emit "disposed-helpers" (list MachineISAObservation.symInstr (session#arm64Helpers errorContext (List.hd targets) (List.hd options) helper (generate "disposed-helpers" instructions)));
 emit "disposed-chunk" (ARMEncodingObservation.prepared (session#prepareArm64EmissionChunk instructions (generate "disposed-chunk" (ARM64_Encoding.prepareSymbolicChunk instructions))));
 emit "disposed-chunks" (ARMEncodingObservation.prepared (session#prepareArm64EmissionChunkGroup parts (generate "disposed-chunks" (ARM64_Encoding.combinePreparedChunks (List.map ARM64_Encoding.prepareSymbolicChunk parts)))))) [0;1];
 session#dispose;emit "disposed-again" (opMetrics ());
 let unmeasured=new S.compilationSession () in
 emit "unmeasured" (tuple [`Bool (Option.is_none unmeasured#arm64LirOpExpansionRecorder);result (list MachineISAObservation.symInstr) (unmeasured#codegenFunction (List.hd contexts) (List.hd targets) (List.hd options) base (generate "unmeasured" (Ok instructions)));`Bool (unmeasured#arm64CodegenMetrics=[])]);
 unmeasured#dispose;
 list Fun.id (List.rev !events)
