(* Complete cache equality, producer identity, summary merging and overlays. *)
open Dark_compiler
module C=CompilationCacheIdentity
module F=SpecializationIdentity.FunctionSet
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let option f=function None->SemanticJson.union "FSharpOption" "None" []|Some value->SemanticJson.union "FSharpOption" "Some" [f value]
let purity (value:MIROptimizationFacts.puritySummary)=SemanticJson.record "PuritySummary" ["ObservableEffects",`Bool value.MIROptimizationFacts.observableEffects;"ReadsMutableState",`Bool value.MIROptimizationFacts.readsMutableState;"MayTrap",`Bool value.MIROptimizationFacts.mayTrap;"MayDiverge",`Bool value.MIROptimizationFacts.mayDiverge]
let writes (value:ARM64CalleeClobbers.writes)=SemanticJson.record "Writes" ["Ints",`Assoc ["kind",`String "uint64";"value",`String (Z.to_string (if value.ARM64CalleeClobbers.ints<0L then Z.add (Z.of_int64 value.ARM64CalleeClobbers.ints) (Z.shift_left Z.one 64) else Z.of_int64 value.ARM64CalleeClobbers.ints))];"Floats",`Assoc ["kind",`String "uint64";"value",`String (Z.to_string (if value.ARM64CalleeClobbers.floats<0L then Z.add (Z.of_int64 value.ARM64CalleeClobbers.floats) (Z.shift_left Z.one 64) else Z.of_int64 value.ARM64CalleeClobbers.floats))]]
let facts (value:C.functionSummaryFacts)=SemanticJson.record "FunctionSummaryFacts" ["Purity",purity value.C.purity;"ConstantReturn",option (fun (typ,operand)->tuple [SemanticAST.semanticType typ;ProductionMIR.operand operand]) value.C.constantReturn;"Arm64Writes",option writes value.C.arm64Writes;"X64Writes",option writes value.C.x64Writes]
let target = function Platform.LinuxX86_64 -> SemanticJson.union "Target" "LinuxX86_64" [] | Platform.ARM64Backend target -> SemanticJson.union "Target" "ARM64Backend" [SemanticJson.union "ARM64Target" (match target with Platform.LinuxARM64 -> "LinuxARM64" | Platform.MacOSARM64 -> "MacOSARM64") []]
let summary value=tuple [option (fun version->tuple [SemanticJson.string version.C.unitName;ProductionMIR.functionId version.C.functionId;target version.C.target;DriverDiagnosticsObservation.options version.C.options;ProductionLIR.functionDef version.C.body]) value.C.version;facts (C.summaryFacts value)]
let matrix (comparer:'a C.comparer) values=list (fun left->list (fun right->let equal=comparer.C.equals left right in tuple [`Bool equal;`Bool (not equal || comparer.C.getHashCode left=comparer.C.getHashCode right);`Bool (comparer.C.getHashCode left=comparer.C.getHashCode left)]) values) values
let observe source=
 let fid n=AST.functionId (Int64.of_int n) in
 let label name=LIR.Label name in
 let blocks=List.map (fun name->label name,{LIR.label=label name;instrs=[LIR.PrintString source];terminator=LIR.Ret}) ["a";"b";"c"] in
 let base={LIR.id=fid 1;name=source;typedParams=[];cfg={LIR.entry=label "a";blocks=LIR.LabelMap.of_list blocks};stackSize=32;usedCalleeSaved=[LIR.X19];codegenFacts=None} in
 let clone={base with LIR.stackSize=32} in
 let reversed={base with LIR.cfg={base.LIR.cfg with LIR.blocks=LIR.LabelMap.of_list (List.rev blocks)}} in
 let functions=[base;base;clone;reversed;{base with LIR.stackSize=16};{base with LIR.id=fid 2};{base with LIR.name=source^"_other"};LIR.attachFunctionCodegenFacts base] in
 let references=matrix C.lirFunctionReferenceComparer functions in
 let allocated=matrix C.allocatedLirFunctionKeyNameHashComparer (List.concat_map (fun arch->List.map (fun func->{C.arch;func}) functions) [Platform.ARM64;Platform.X86_64]) in
 let versions=List.map (fun body->C.functionVersion source (fid 1) Platform.LinuxX86_64 CompilerOptions.defaultOptions body) functions in
 let versions=versions@[C.functionVersion (source^"_unit") (fid 1) Platform.LinuxX86_64 CompilerOptions.defaultOptions base;C.functionVersion source (fid 2) Platform.LinuxX86_64 CompilerOptions.defaultOptions base;C.functionVersion source (fid 1) (Platform.ARM64Backend Platform.LinuxARM64) CompilerOptions.defaultOptions base;C.functionVersion source (fid 1) Platform.LinuxX86_64 {CompilerOptions.defaultOptions with CompilerOptions.enableLeakCheck=true} base] in
 let versionMatrix=matrix {C.equals=C.functionVersionEquals;getHashCode=C.functionVersionHashCode} versions in
 let p={MIROptimizationFacts.observableEffects=false;readsMutableState=false;mayTrap=false;mayDiverge=false} in
 let first={C.version=Some (List.hd versions);purity=p;constantReturn=Some (AST.TInt64,MIR.Int64Const 7L);arm64Writes=Some {ARM64CalleeClobbers.ints=1L;floats=2L};x64Writes=Some X64CalleeClobbers.all} in
 let summaries=List.map (fun version->{first with C.version=Some version}) versions
   @[C.unknownSummary;{first with C.version=None};{first with C.constantReturn=None};{first with C.arm64Writes=None};{first with C.x64Writes=None}]
   @List.map (fun operand->{first with C.constantReturn=Some (AST.TFloat64,operand)}) [MIR.FloatSymbol 1.5;MIR.FloatSymbol (-0.);MIR.FloatSymbol (Int64.float_of_bits 0xfff8000000000000L)]
   @List.init 16 (fun mask->{first with C.purity={MIROptimizationFacts.observableEffects=mask land 1<>0;readsMutableState=mask land 2<>0;mayTrap=mask land 4<>0;mayDiverge=mask land 8<>0}}) in
 let summaryMatrix=list (fun left->list (fun right->`Bool (C.functionSummaryEquals left right)) summaries) summaries in
 let merged=list (fun left->list (fun right->let table=C.mergeFunctionSummaries (FunctionIdMap.ofList [fid 1,left;AST.functionId (-1L),first]) (FunctionIdMap.ofList [fid 1,right;fid 2,right]) in tuple [list (fun (id,value)->tuple [ProductionMIR.functionId id;summary value]) (FunctionIdMap.toList table);`Bool (FunctionIdMap.find (fid 1) table == left)]) summaries) summaries in
 let callees=[FunctionIdMap.empty;FunctionIdMap.ofList [fid 1,ARM64CalleeClobbers.all;fid 2,{ARM64CalleeClobbers.ints=1L;floats=0L}];FunctionIdMap.ofList [fid 2,{ARM64CalleeClobbers.ints=1L;floats=0L};fid 1,ARM64CalleeClobbers.all];FunctionIdMap.ofList [fid 1,{ARM64CalleeClobbers.ints=1L;floats=0L}]] in
 let callAware=matrix C.callAwareLirFunctionKeyComparer (List.concat_map (fun base->List.map (fun callees->{C.base;callees}) callees) functions) in
 let groups=[[];[base];List.map Fun.id [base];[clone];[base;base];[base;clone];[base;List.nth functions 4];[List.nth functions 4;base]] in
 let groupMatrix=matrix C.arm64MetadataGroupKeyComparer (List.map (fun functions->({C.functions}:C.arm64MetadataGroupKey)) groups) in
 let armGroups=List.concat_map (fun target->List.concat_map (fun options->List.map (fun functions->({C.functions;target;options}:C.arm64FunctionGroupKey)) groups) [ARM64CodeGenTypes.defaultOptions;{ARM64CodeGenTypes.defaultOptions with ARM64CodeGenTypes.enableLeakCheck=true}]) [ARM64.targetConfigFor Platform.LinuxARM64;ARM64.targetConfigFor Platform.MacOSARM64] in
 let armGroupMatrix=matrix C.arm64FunctionGroupKeyComparer armGroups in
 let instructions=[Symbolic.Label source] in let copied=List.map Fun.id instructions in
 let chunks=[[];instructions;instructions;copied;[Symbolic.Label (source^"_other")]] in
 let chunkMatrix=matrix C.arm64InstructionChunkReferenceComparer chunks in
 let parts=[instructions;copied] in let chunkGroups=[[];parts;parts;List.map Fun.id parts;[instructions];[copied]] in
 let chunkGroupMatrix=matrix C.arm64InstructionChunkGroupReferenceComparer chunkGroups in
 let obj=Obj.repr (ref 1) in let objects=[obj;obj;Obj.repr (ref 1);Obj.repr (ref 2)] in
 let objectMatrix=matrix C.objectReferenceComparer objects in
 let mirBlocks=List.map (fun name->let label=MIR.Label name in label,{MIR.label;instrs=[MIR.Mov (MIR.VReg 0,MIR.Int64Const 7L,None)];terminator=MIR.Ret (MIR.Int64Const 7L)}) ["a";"b";"c"] in
 let mir={MIR.id=fid 1;name=source;typedParams=[];returnType=AST.TInt64;cfg={MIR.entry=MIR.Label "a";blocks=MIR.LabelMap.of_list mirBlocks};floatRegs=MIR.IntSet.of_list [1;2;3]} in
 let mirFunctions=[mir;{mir with MIR.cfg={mir.MIR.cfg with MIR.blocks=MIR.LabelMap.of_list (List.rev mirBlocks)};floatRegs=MIR.IntSet.of_list [3;2;1]};{mir with MIR.returnType=AST.TBool};{mir with MIR.name=source^"_other"}] in
 let mirKeys=List.concat_map (fun func->List.concat_map (fun options->List.map (fun effectFreeCalls->{C.func;options;effectFreeCalls}) [F.empty;F.of_list [fid 1;fid 2];F.of_list [fid 2;fid 1]]) [MIROptimizationFacts.defaultOptimizeOptions;{MIROptimizationFacts.defaultOptimizeOptions with MIROptimizationFacts.enableLICM=false}]) mirFunctions in
 let mirMatrix=matrix C.mirOptimizationKeyNameHashComparer mirKeys in
 let symbols=CheckedAST.emptySymbols () in
 let registries=AST_to_ANF.buildRegistries symbols StringOrder.Map.empty [] StringOrder.Map.empty [] in
 let maps=[StringOrder.Map.of_list ["a",["x",AST.TString];"b",["y",AST.TInt64];"c",[]];StringOrder.Map.of_list ["c",[];"b",["y",AST.TInt64];"a",["x",AST.TString]];StringOrder.Map.empty] in
 let checked:CheckedAST.functionDef={CheckedAST.id=fid 1;name=source;typeParams=[];params=CheckedAST.checkedParams (NonEmptyList.singleton (AST.bindingId 0,AST.TUnit));returnType=CheckedAST.checkedType AST.TInt64;body=CheckedAST.Int64Literal 7L;recursion=None} in
 let checkedFunctions=[[];[checked];[{checked with CheckedAST.name=source}];[{checked with CheckedAST.id=fid 2}];[checked;{checked with CheckedAST.id=fid 2}]] in
 let keys=List.concat_map (fun functions->List.concat_map (fun recordFieldsReg->List.map (fun nonInlineableFunctionNames->{C.functions;localRegistries={registries with AST_to_ANF.recordFieldsReg};nonInlineableFunctionNames}) [F.empty;F.of_list [fid 1;fid 2;fid 3];F.of_list [fid 3;fid 2;fid 1]]) maps) checkedFunctions in
 let anfMatrix=matrix C.anfDependencyKeyNameHashComparer keys in
 let known=List.map (fun value->C.summaryFacts value) summaries in
 let configs=List.concat_map (fun knownSummary->List.map (fun nonInlineableFunctionNames->({C.target=Platform.LinuxX86_64;options=CompilerOptions.defaultOptions;nonInlineableFunctionNames;knownSummaries=FunctionIdMap.ofList [fid 1,knownSummary]}:C.compiledDependencyConfig)) [F.empty;F.of_list [fid 1;fid 2];F.of_list [fid 2;fid 1]]) known in
 let configMatrix=matrix C.compiledDependencyConfigComparer configs in
 let helper:Backend_Arm64_CodeGen.helperCacheKey={Backend_Arm64_CodeGen.closurePayloadSizesFromParams=[];closurePayloadSizesFromAllocs=[];closureCaptureTypes=[];recursiveReleaseTypes=[];cliArgvHelperLabels=[];needsCliExecuteHelper=false;needsCliRunProcessHelper=false;needsCliProcessLifecycleHelpers=false;needsRuntimeErrorHelper=false;listDecHelperLabels=[];plannedListDecHelpers=[];plannedGenericDecHelperLabels=[];plannedDictDecHelperLabels=[];dictDecHelperLabels=[];needsListRcIncHelper=false;needsDictRcIncHelper=false;needsClosureRcIncHelper=false;needsClosureRcDecHelper=false;needsStreamRcDecHelper=false} in
 let helperValues=[helper;{helper with Backend_Arm64_CodeGen.closurePayloadSizesFromParams=[source,8]};{helper with Backend_Arm64_CodeGen.closurePayloadSizesFromAllocs=[fid 1,16]};{helper with Backend_Arm64_CodeGen.closureCaptureTypes=[source,[AST.TString;AST.TInt64]]};{helper with Backend_Arm64_CodeGen.recursiveReleaseTypes=[AST.TList AST.TString]};{helper with Backend_Arm64_CodeGen.cliArgvHelperLabels=[source]};{helper with Backend_Arm64_CodeGen.needsCliExecuteHelper=true};{helper with Backend_Arm64_CodeGen.needsCliRunProcessHelper=true};{helper with Backend_Arm64_CodeGen.needsCliProcessLifecycleHelpers=true};{helper with Backend_Arm64_CodeGen.needsRuntimeErrorHelper=true};{helper with Backend_Arm64_CodeGen.listDecHelperLabels=[source]};{helper with Backend_Arm64_CodeGen.plannedListDecHelpers=[source,8]};{helper with Backend_Arm64_CodeGen.plannedGenericDecHelperLabels=[source]};{helper with Backend_Arm64_CodeGen.plannedDictDecHelperLabels=[source]};{helper with Backend_Arm64_CodeGen.dictDecHelperLabels=[source]};{helper with Backend_Arm64_CodeGen.needsListRcIncHelper=true};{helper with Backend_Arm64_CodeGen.needsDictRcIncHelper=true};{helper with Backend_Arm64_CodeGen.needsClosureRcIncHelper=true};{helper with Backend_Arm64_CodeGen.needsClosureRcDecHelper=true};{helper with Backend_Arm64_CodeGen.needsStreamRcDecHelper=true}] in
 let helperKeys=List.concat_map (fun target->List.concat_map (fun options->List.map (fun helper->({C.target;options;helper}:C.arm64HelperCacheKey)) helperValues) [ARM64CodeGenTypes.defaultOptions;{ARM64CodeGenTypes.defaultOptions with ARM64CodeGenTypes.enableLeakCheck=true}]) [ARM64.targetConfigFor Platform.LinuxARM64;ARM64.targetConfigFor Platform.MacOSARM64] in
 let helperMatrix=matrix C.arm64HelperCacheKeyComparer helperKeys in
 let variants=StringOrder.Map.singleton "Option" ("Option",[],1,[AST.TString]) in
 let localRecords=StringOrder.Map.of_list [source,["local",AST.TInt64];"local",["name",AST.TString]] in
 let baseRecords=ANF_to_MIR.buildRecordRegistry (StringOrder.Map.of_list [source,["base",AST.TBool];"base",[]]) in
 let baseVariants=ANF_to_MIR.buildVariantRegistry (StringOrder.Map.of_list ["Base",("Base",[],0,[]);"Old",("Option",[],0,[])]) in
 let overlayVariants,overlayRecords=C.projectMirRegistryOverlay (baseVariants,baseRecords) variants localRecords in
 tuple [references;allocated;versionMatrix;list summary summaries;summaryMatrix;merged;callAware;groupMatrix;armGroupMatrix;chunkMatrix;chunkGroupMatrix;objectMatrix;mirMatrix;anfMatrix;configMatrix;helperMatrix;tuple [ProductionMIR.variantRegistry overlayVariants;ProductionMIR.recordRegistry overlayRecords]]
