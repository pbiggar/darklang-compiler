(* ANFPipeline.ml - Construct and optimize SSA after ANF helper lowering. *)
[@@@warning "-4"]
open CompilerOptions
open PipelineDiagnostics
module F=SpecializationIdentity.FunctionSet
let (let*)=Result.bind
let buildConversionResult (ANF.Program (functions,_) as program) (registries:AST_to_ANF.registries) ownershipContracts=
 let funcReg=AST_to_ANF.extendFunctionRegistryWithConverted registries.AST_to_ANF.funcReg functions in
 {AST_to_ANF.program;ownershipContracts;recursiveMembers=registries.AST_to_ANF.recursiveMembers;typeReg=registries.AST_to_ANF.typeReg;recordFieldsReg=registries.AST_to_ANF.recordFieldsReg;recordTypeParamsReg=registries.AST_to_ANF.recordTypeParamsReg;variantLookup=registries.AST_to_ANF.variantLookup;rcSumShapeReg=registries.AST_to_ANF.rcSumShapeReg;funcReg;funcParams=registries.AST_to_ANF.funcParams;moduleRegistry=registries.AST_to_ANF.moduleRegistry}
(* The stdlib contains enough mutually connected helpers that the general user
   program policy causes excessive compile-time and ANF growth. This policy is
   deliberately limited to shallow, very small ordinary helpers; the other
   specialized inlining modes remain available to user programs. *)
let stdlibInliningConfig={InliningCommon.maxFunctionSize=1;maxInlineDepth=1;maxExternalInlineSites=0;maxBoundedLoopIterations=0;maxBoundedLoopExpansion=0;maxProjectedTupleInlineSize=0;maxProjectedTupleInlineSites=0}
(* Lower accumulator helpers, construct SSA, then optimize and elaborate ownership. *)
let buildAnf verbosity (options:compilerOptions) elapsed (registries:AST_to_ANF.registries) nextFunctionOrdinal inliningConfig externalInlineCandidates externalOptimizationFunctions nonInlineableFunctionNames functions ownershipContracts specializeInternalSignatures passTimingRecorder=
 let elapsedDetail enabled duration=if verbosity>=2 && enabled then (
  let scaled=duration*.10. in let lower=Float.floor scaled in
  let rounded=if scaled-.lower=0.5 then (if Float.rem lower 2.=0. then lower else lower+.1.) else Float.round scaled in
  Output.println ("        "^FloatFormat.roundTrip (rounded/.10.)^"ms")) in
 let anfOptions=buildANFOptimizeOptions options in
 let ssaPassLabel=formatPassGroup "SSA Optimizations" ["const_folding",anfOptions.ANFConstants.enableConstFolding;"const_prop",anfOptions.ANFConstants.enableConstProp;"copy_prop",anfOptions.ANFConstants.enableCopyProp;"dce",anfOptions.ANFConstants.enableDCE;"cse",anfOptions.ANFConstants.enableCSE;"strength_reduction",anfOptions.ANFConstants.enableStrengthReduction] in
 if verbosity>=1 && anfOptions.ANFConstants.enableTailRecursionModuloOperation then Output.println "  [anf.accumulators] ANF Accumulator Lowering...";
 let anfProgram=ANF_Intrinsics.canonicalizeProgram registries.AST_to_ANF.functionIds registries.AST_to_ANF.funcReg (ANF.Program (functions,ANF.Return ANF.UnitLiteral)) in
 if shouldDumpIR verbosity options.dumpANF then printANFProgram options "=== ANF (before optimization) ===" anfProgram;
 let anfLoweringStart=elapsed () in
 let singletonRecursiveNames=List.filter_map (fun (func:ANF.functionDef)->match FunctionIdMap.tryFind func.ANF.id registries.AST_to_ANF.recursiveMembers with Some memberInfo when memberInfo.AST.typed.AST.resolved.AST.availability=AST.SelfRecursiveMember->Some func.ANF.id|_->None) functions |> F.of_list in
 let anfOptimizeContext={ANFConstants.typeReg=registries.AST_to_ANF.recordFieldsReg;recordTypeParams=registries.AST_to_ANF.recordTypeParamsReg;sumShapeReg=registries.AST_to_ANF.rcSumShapeReg;functionNames=registries.AST_to_ANF.functionNames;functionIds=registries.AST_to_ANF.functionIds} in
 let anfOptimized=if anfOptions.ANFConstants.enableTailRecursionModuloOperation then ANFAccumulatorLowering.lower nextFunctionOrdinal anfOptimizeContext singletonRecursiveNames externalOptimizationFunctions anfProgram else anfProgram in
 let anfLoweringElapsed=elapsed ()-.anfLoweringStart in
 recordPassTiming passTimingRecorder "ANF Accumulator Lowering" anfLoweringElapsed;elapsedDetail true anfLoweringElapsed;
 if shouldDumpIR verbosity options.dumpANF then printANFProgram options "=== ANF (after accumulator lowering) ===" anfOptimized;
 let convResult=buildConversionResult anfOptimized registries ownershipContracts in
 let preSpecializationContext=RcTypeFacts.createContext convResult in
 let ANF.Program (preRCFunctions,_)=anfOptimized in
 let convert funcs=let* reversed=List.fold_left (fun result func->let* accumulated=result in Result.map (fun ssa->ssa::accumulated) (SSAANF.convertFunctionBeforeRC (ANF_to_MIR.maxTempIdInFunction func) preSpecializationContext func)) (Ok []) funcs in Ok (List.rev reversed) in
 let before=let* ()=RefCountInsertion.verifyOwnershipContracts preSpecializationContext ownershipContracts anfOptimized in convert preRCFunctions in
 let* ssaBeforeSpecialization=Result.map_error (fun error->"Reference count insertion error: "^error) before in
 if verbosity>=1 then Output.println ("  [ssa.optimize] "^ssaPassLabel^"...");
 let ssaOptStart=elapsed () in
 let runSSAOptimize=anfOptions.ANFConstants.enableConstFolding || anfOptions.ANFConstants.enableConstProp || anfOptions.ANFConstants.enableCopyProp || anfOptions.ANFConstants.enableDCE || anfOptions.ANFConstants.enableCSE || anfOptions.ANFConstants.enableStrengthReduction in
 let ssaBeforeSpecialization=if runSSAOptimize then List.map (SSAOptimization.optimizeFunction anfOptimizeContext anfOptions) ssaBeforeSpecialization else ssaBeforeSpecialization in
 recordPassTiming passTimingRecorder "SSA Optimizations" (elapsed ()-.ssaOptStart);
 let externalSSAResult=if options.disableInlining || FunctionIdMap.isEmpty externalInlineCandidates then Ok [] else (
  let atomTargets=function ANF.FuncRef id->F.singleton id|_->F.empty in
  let operationTargets=function
  |ANF.Call (id,arguments)|ANF.BorrowedCall (id,arguments)|ANF.TailCall (id,arguments)->List.fold_left (fun ids atom->F.union ids (atomTargets atom)) (F.singleton id) arguments
  |ANF.ClosureAlloc (id,captures)->List.fold_left (fun ids atom->F.union ids (atomTargets atom)) (F.singleton id) captures
  |ANF.Atom atom|ANF.TypedAtom (atom,_)->atomTargets atom
  |ANF.IfValue (_,yes,no)->F.union (atomTargets yes) (atomTargets no)
  |_->F.empty in
  let rec anfTargets=function
  |ANF.Return atom|ANF.Jump (_,atom)->atomTargets atom
  |ANF.Let (_,operation,rest)->F.union (operationTargets operation) (anfTargets rest)
  |ANF.If (condition,yes,no)->F.union (atomTargets condition) (F.union (anfTargets yes) (anfTargets no))
  |ANF.Join (_,continuation,entry)->F.union (anfTargets continuation) (anfTargets entry) in
  let localTargets=List.fold_left (fun ids (func:SSAANF.functionDef)->SSAANF.LabelMap.fold (fun _ (block:SSAANF.block) current->List.fold_left (fun found (_,operation)->F.union found (operationTargets operation)) current block.SSAANF.operations) func.SSAANF.blocks ids) F.empty ssaBeforeSpecialization in
  let rec relevant seen=function []->seen|id::rest when F.mem id seen->relevant seen rest|id::rest->match FunctionIdMap.tryFind id externalInlineCandidates with None->relevant seen rest|Some (info:InliningCommon.functionInfo)->relevant (F.add id seen) (F.elements (anfTargets info.InliningCommon.func.ANF.body) @ rest) in
  F.elements (relevant F.empty (F.elements localTargets)) |> List.filter_map (fun id->Option.map (fun (info:InliningCommon.functionInfo)->info.InliningCommon.func) (FunctionIdMap.tryFind id externalInlineCandidates)) |> convert) in
 let* externalSSA=externalSSAResult in
 if verbosity>=1 then Output.println "  [ssa.inline] SSA Inlining...";
 let inlineStart=elapsed () in
 let ssaInlined=if options.disableInlining then ssaBeforeSpecialization else SSAInlining.inlineProgramWithExternalCandidatesAndExclusions inliningConfig externalInlineCandidates externalSSA nonInlineableFunctionNames preRCFunctions ssaBeforeSpecialization in
 let inlineElapsed=elapsed ()-.inlineStart in recordPassTiming passTimingRecorder "SSA Inlining" inlineElapsed;elapsedDetail true inlineElapsed;
 if verbosity>=1 && specializeInternalSignatures then Output.println "  [ssa.specialize-closures] SSA Higher-Order Specialization...";
 let higherOrderStart=elapsed () in
 let higherOrder=if options.disableInlining || not specializeInternalSignatures then {SSAHigherOrderSpecialization.functions=ssaInlined;cloneOrigins=FunctionIdMap.empty} else SSAHigherOrderSpecialization.specializeProgramWithExternalFunctionsAndNames registries.AST_to_ANF.functionIds nextFunctionOrdinal externalSSA ssaInlined in
 let duration=elapsed ()-.higherOrderStart in if specializeInternalSignatures then recordPassTiming passTimingRecorder "SSA Higher-Order Specialization" duration;elapsedDetail specializeInternalSignatures duration;
 if verbosity>=1 && specializeInternalSignatures then Output.println "  [ssa.specialize-calls] SSA Direct-Call Specialization...";
 let specializationStart=elapsed () in
 let specialization=if options.disableInlining || not specializeInternalSignatures then {SSADirectCallSpecialization.functions=higherOrder.SSAHigherOrderSpecialization.functions;cloneOrigins=FunctionIdMap.empty} else SSADirectCallSpecialization.specializeProgramWithFunctionNames registries.AST_to_ANF.functionNames higherOrder.SSAHigherOrderSpecialization.functions in
 let specializationElapsed=elapsed ()-.specializationStart in if specializeInternalSignatures then recordPassTiming passTimingRecorder "SSA Direct-Call Specialization" specializationElapsed;elapsedDetail specializeInternalSignatures specializationElapsed;
 if verbosity>=1 && not options.disableANFOpt then Output.println "  [ssa.escape-analysis] SSA Escape Analysis...";
 let escapeStart=elapsed () in
 let ssaAfterEscape=List.map (fun ssa->if options.disableANFOpt then ssa else SSAEscapeAnalysis.optimizeFunction registries.AST_to_ANF.typeReg registries.AST_to_ANF.rcSumShapeReg ssa) specialization.SSADirectCallSpecialization.functions in
 let escapeElapsed=elapsed ()-.escapeStart in if not options.disableANFOpt then recordPassTiming passTimingRecorder "SSA Escape Analysis" escapeElapsed;elapsedDetail (not options.disableANFOpt) escapeElapsed;
 let specializedRegistry=List.fold_left (fun registry (func:SSAANF.functionDef)->FunctionIdMap.add func.SSAANF.id (func.SSAANF.name,AST.TFunction (List.map (fun (parameter:ANF.typedParam)->parameter.ANF.typ) func.SSAANF.typedParams,func.SSAANF.returnType)) registry) convResult.AST_to_ANF.funcReg ssaAfterEscape in
 let ctx=RcTypeFacts.createContext {convResult with AST_to_ANF.funcReg=specializedRegistry} in
 let sourceId id=let id=Option.value ~default:id (FunctionIdMap.tryFind id specialization.SSADirectCallSpecialization.cloneOrigins) in Option.value ~default:id (FunctionIdMap.tryFind id higherOrder.SSAHigherOrderSpecialization.cloneOrigins) in
 let localTemplates=FunctionIdMap.ofList (List.map (fun (func:ANF.functionDef)->func.ANF.id,func) preRCFunctions) in
 let originalFrontiers=List.map (fun (func:SSAANF.functionDef)->sourceId func.SSAANF.id) ssaAfterEscape |> F.of_list |> fun ids->F.fold (fun id frontiers->let template=match FunctionIdMap.tryFind id localTemplates with Some _ as found->found|None->Option.map (fun (info:InliningCommon.functionInfo)->info.InliningCommon.func) (FunctionIdMap.tryFind id externalInlineCandidates) in match template with None->frontiers|Some func->FunctionIdMap.add id (RefCountInsertion.ownedDictionaryFrontierParams func) frontiers) ids FunctionIdMap.empty in
 if verbosity>=1 then Output.println "  [anf.reference-counts] Reference Count Insertion...";
 let rcStart=elapsed () in
 let ssaAfterRC=List.map (fun (ssa:SSAANF.functionDef)->let sourceId=sourceId ssa.SSAANF.id in let retainedParams=List.map (fun (parameter:ANF.typedParam)->parameter.ANF.id) ssa.SSAANF.typedParams |> RcReturnAnalysis.TempSet.of_list in let frontierParams=Option.value ~default:RcReturnAnalysis.TempSet.empty (FunctionIdMap.tryFind sourceId originalFrontiers) |> RcReturnAnalysis.TempSet.inter retainedParams in RcSSARefCountInsertion.insertBlockLocal ctx frontierParams ssa) ssaAfterEscape in
 let typeMap=List.concat_map (fun (func:SSAANF.functionDef)->RcTypeFacts.TempMap.bindings func.SSAANF.freshValueTypes) ssaAfterRC |> List.to_seq |> ANF.TypeMap.ofSeq in
 let rcElapsed=elapsed ()-.rcStart in recordPassTiming passTimingRecorder "Reference Count Insertion" rcElapsed;elapsedDetail true rcElapsed;
 Ok (preRCFunctions,ssaAfterRC,typeMap)
