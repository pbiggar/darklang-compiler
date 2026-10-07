(* NativePipeline.fs - Compile direct-call components callee-first. *)
[@@@warning "-4"]
open CompilerOptions
open PipelineDiagnostics
module F=SpecializationIdentity.FunctionSet
module M=StringOrder.Map
module S=StringOrder.Set
module C=CompilationCacheIdentity
module R=AST_to_ANF
module I=Set.Make(Int)
module Visits=Set.Make(struct type t=int*string let compare (a,x) (b,y)=let c=Int.compare a b in if c=0 then StringOrder.compare x y else c end)
let (let*)=Result.bind
let timingRecorder recorder=Option.map (fun recorder name elapsed->recorder {pass=name;elapsed=(Int64.of_float (elapsed *. 1e6))}) recorder
let elapsedDetail verbosity duration=if verbosity>=2 then (let scaled=duration*.10. in let lower=Float.floor scaled in let rounded=if scaled-.lower=0.5 then (if Float.rem lower 2.=0. then lower else lower+.1.) else Float.round scaled in Output.println ("        "^FloatFormat.roundTrip (rounded/.10.)^"ms"))
(* Run MIR/LIR optimizations on SSA MIR, returning an optimized LIR program. *)
(*
   functions through allocation and tree shaking, so each executable
   only unions metadata for its reachable compilation unit.
*)
let compileMirToLir arch knownEffectFree knownRemovable knownTypedConstants verbosity (options:compilerOptions) elapsed passTimingRecorder functionCaches (registries:R.registries) stageSuffix (MIR.Program (functions,variants,records))=
 let suffix=if stageSuffix="" then "" else " ("^stageSuffix^")" in
 let rewriteCall=function MIR.Call (dest,callee,[],[],returnType) as instr when F.mem callee knownRemovable->(match FunctionIdMap.tryFind callee knownTypedConstants with Some (typ,((MIR.Int64Const _|MIR.BoolConst _|MIR.FloatSymbol _) as value)) when typ=returnType->MIR.Mov (dest,value,Some returnType)|_->instr)|instr->instr in
 let ssaProgram=MIR.Program (List.map (fun (func:MIR.functionDef)->let blocks=MIR.LabelMap.map (fun (block:MIR.basicBlock)->{block with MIR.instrs=List.map rewriteCall block.MIR.instrs}) func.MIR.cfg.MIR.blocks in {func with MIR.cfg={func.MIR.cfg with MIR.blocks}}) functions,variants,records) in
 let mirOptions=buildMIROptimizeOptions options in let mirPassLabel=formatPassGroup "MIR Optimizations" ["sccp",mirOptions.MIROptimizationFacts.enableSCCP;"cse",mirOptions.MIROptimizationFacts.enableCSE;"dce",mirOptions.MIROptimizationFacts.enableDCE;"licm",mirOptions.MIROptimizationFacts.enableLICM] in
 if verbosity>=1 then Output.println ("  [mir.optimize] "^mirPassLabel^suffix^"...");let mirOptStart=elapsed () in
 let optimizedProgram=if shouldRunMIROptimize mirOptions then (
  let ticks=Hashtbl.create 8 in let order=ref [] in let addTicks name value=match Hashtbl.find_opt ticks name with Some existing->Hashtbl.replace ticks name (Int64.add existing value)|None->order:=name:: !order;Hashtbl.add ticks name value in
  let tickRecorder=Option.map (fun _->addTicks) passTimingRecorder in
  let start=Mtime_clock.elapsed_ns () in let effectFree=if mirOptions.MIROptimizationFacts.enableLICM || mirOptions.MIROptimizationFacts.enableCSE then knownEffectFree else F.empty in addTicks "MIR Effect Analysis" (Int64.sub (Mtime_clock.elapsed_ns ()) start);
  let optimizeFunction func=let optimize ()=MIR_Optimize.optimizeFunctionWithEffectFreeCallsAndTickTrace tickRecorder effectFree mirOptions func in let key={C.func;options=mirOptions;effectFreeCalls=MIROptimizationFacts.effectFreeCallsForFunction effectFree func} in match functionCaches with Some (caches:C.functionCompilationCaches)->caches.C.optimizeMir key optimize|None->optimize () in
  let MIR.Program (functions,variants,records)=ssaProgram in let optimized=MIR.Program (List.map optimizeFunction functions,variants,records) in
  Option.iter (fun recorder->List.iter (fun name->let value=Hashtbl.find ticks name in recorder {pass=name;elapsed=value}) (List.rev !order)) passTimingRecorder;optimized) else ssaProgram in
 let optimizedProgram=if mirOptions.MIROptimizationFacts.enableSCCP && not (FunctionIdMap.isEmpty knownTypedConstants) then (
  let MIR.Program (functions,variants,records)=optimizedProgram in let callResult id=Option.map snd (FunctionIdMap.tryFind id knownTypedConstants) in
  let functions=List.map (fun (func:MIR.functionDef)->let cfg,changed=MIRSparseConditionalConstants.applySparseConditionalConstantPropagationWithCallResults callResult func.MIR.cfg in if changed then {func with MIR.cfg} else func) functions in MIR.Program (functions,variants,records)) else optimizedProgram in
 let MIR.Program (functions,mirVariants,mirRecords)=optimizedProgram in
 let typedConstants=List.filter_map (fun (func:MIR.functionDef)->Option.map (fun value->func.MIR.id,(func.MIR.returnType,value)) (MIR_Optimize.constantReturnOperand func)) functions |> FunctionIdMap.ofList in
 let duration=elapsed ()-.mirOptStart in recordPassTiming passTimingRecorder "MIR Optimizations" duration;if shouldDumpIR verbosity options.dumpMIR then printMIRProgram options "=== MIR (Control Flow Graph) ===" optimizedProgram;elapsedDetail verbosity duration;
 if verbosity>=1 then Output.println ("  [lir.lower] MIR → LIR"^suffix^"...");let lirStart=elapsed () in
 let* lirFuncs=MIR_to_LIR.toLIRFunctionsForWithTraceAndRcRegistries (timingRecorder passTimingRecorder) arch registries.R.recordFieldsReg registries.R.recordTypeParamsReg registries.R.rcSumShapeReg optimizedProgram |> Result.map_error (fun error->"LIR conversion error: "^error) in
 let duration=elapsed ()-.lirStart in recordPassTiming passTimingRecorder "MIR -> LIR" duration;
 let lirProgramForDump=lazy (let variants=M.map (fun (info:MIR.typeVariants)->{LIR.typeParams=info.MIR.typeParams;variants=List.map (fun (variant:MIR.variantInfo)->{LIR.name=variant.MIR.name;tag=variant.MIR.tag;payload=variant.MIR.payload;fieldCount=variant.MIR.fieldCount}) info.MIR.variants}) mirVariants in let records=M.map (List.map (fun (field:MIR.recordField)->field.MIR.name,field.MIR.typ)) mirRecords in LIR.Program (lirFuncs,variants,records)) in
 if shouldDumpIR verbosity options.dumpLIR then printLIRProgram options "=== LIR (Low-level IR with CFG) ===" (Lazy.force lirProgramForDump);elapsedDetail verbosity duration;
 let lirPassLabel=formatPassGroup "LIR Peephole" ["peephole",not options.disableLIROpt && not options.disableLIRPeephole] in
 if verbosity>=1 then Output.println ("  [lir.peephole] "^lirPassLabel^suffix^"...");let start=elapsed () in let optimized=if options.disableLIROpt || options.disableLIRPeephole then lirFuncs else List.map (LIR_Peephole.optimizeFunctionFor arch) lirFuncs in
 let duration=elapsed ()-.start in recordPassTiming passTimingRecorder "LIR Peephole" duration;elapsedDetail verbosity duration;
 (* Summarize finalized symbolic LIR once. The facts remain attached to functions through allocation and tree shaking. *)
 Ok (List.map LIR.attachFunctionCodegenFacts optimized,typedConstants)
(* Allocate registers for one symbolic LIR function. *)
let allocateRegistersForFunction arch recorder func=let allocated=match recorder with None->RegisterAllocation.allocateRegisters arch func|Some record->let allocated,timings=RegisterAllocation.allocateRegistersWithTiming arch func in List.iter (fun (timing:AllocationModel.registerAllocationTiming)->record {pass=timing.AllocationModel.phase;elapsed=(Int64.of_float (timing.AllocationModel.elapsedMs *. 1e6))}) timings;allocated in LIR_Peephole.removeSelfMovesFromFunction allocated
let countIds key values=List.fold_left (fun counts value->let id=key value in FunctionIdMap.change id (fun count->Some (1+Option.value ~default:0 count)) counts) FunctionIdMap.empty values
let functionIdString id="FunctionId "^(let value=AST.functionIdValue id in Z.to_string (if value<0L then Z.add (Z.of_int64 value) (Z.shift_left Z.one 64) else Z.of_int64 value))
(* Run MIR+LIR passes (including register allocation) from SSA ANF functions. *)
(*
   completed function, rather than silently taking the
   same fallback as an unavailable external callee.
   Keep per-name queues so duplicate function names (e.g. lifted __closure_N from
   different compilation units) preserve distinct bodies in original order.
*)
let lowerToAllocatedLirWithKnownGroups externalSummaries target verbosity (options:compilerOptions) elapsed passTimingRecorder functionCaches releasePlanSummaryCache stageSuffix functionGroups (registries:R.registries) projectedMirRegistries externalReturnTypes=
 let suffix=if stageSuffix="" then "" else " ("^stageSuffix^")" in
 let functions=List.concat_map fst functionGroups in let functionOrder=List.map (fun (func:SSAANF.functionDef)->func.SSAANF.name) functions in
 (* Function-affinity batches still call helpers compiled in sibling batches. Keep the complete AOT return-type plan available while lowering each one. *)
 let returnTypeReg=List.fold_left (fun types (func:SSAANF.functionDef)->FunctionIdMap.add func.SSAANF.id func.SSAANF.returnType types) (FunctionIdMap.map (fun _ (_,typ)->typ) externalReturnTypes) functions in
 let arch=Platform.archFor target in
 let allWrites=match arch with Platform.ARM64->ARM64CalleeClobbers.all|Platform.X86_64->X64CalleeClobbers.all in
 let targetWrites (summary:C.functionSummary)=match arch with Platform.ARM64->summary.C.arm64Writes|Platform.X86_64->summary.C.x64Writes in
 let compileFunctions functionsToCompile=if functionsToCompile=[] then Ok ([],FunctionIdMap.empty) else (
  if verbosity>=1 then Output.println ("  [mir.lower] ANF → MIR"^suffix^"...");let mirStart=elapsed () in
  let* mirFuncs,variantRegistry,mirRecordRegistry=List.fold_left (fun result (groupFunctions,typeMap)->let* prior,_,_=result in if groupFunctions=[] then result else
   Result.map (fun (mir,variants,records)->prior @ mir,variants,records) (ANF_to_MIR.toMIRSSAFunctionsOnlyWithTrace (timingRecorder passTimingRecorder) projectedMirRegistries registries.R.recursiveMembers (not options.disableTCO) groupFunctions typeMap registries.R.funcParams registries.R.variantLookup registries.R.recordFieldsReg options.enableCoverage returnTypeReg registries.R.functionNames)) (Ok ([],M.empty,M.empty)) functionGroups |> Result.map_error (fun error->"MIR conversion error: "^error) in
  if List.length mirFuncs<>List.length functionsToCompile then Crash.crash "ANF to MIR did not emit exactly one node per function";
  let duration=elapsed ()-.mirStart in recordPassTiming passTimingRecorder "ANF -> MIR" duration;elapsedDetail verbosity duration;
  let scheduleStart=elapsed () in let components=CallGraphSchedule.calleeFirst mirFuncs in let expectedNodes=I.of_list (List.init (List.length mirFuncs) Fun.id) in let scheduledNodes=List.concat_map (fun (group:CallGraphSchedule.component)->group.CallGraphSchedule.nodeIndices) components in
  if List.length scheduledNodes<>List.length mirFuncs || not (I.equal (I.of_list scheduledNodes) expectedNodes) then Crash.crash "Call graph did not schedule each MIR function exactly once";
  let pipelineStages=S.of_list ["ANF -> MIR";"Purity";"MIR Optimization";"MIR -> LIR";"LIR Peephole";"Register Allocation";"Clobber Summary"] in
  let recordStages stages (group:CallGraphSchedule.component) visits=List.fold_left (fun visits node->List.fold_left (fun visits stage->let key=node,stage in if Visits.mem key visits then Crash.crash ("Pipeline stage "^stage^" ran twice for function node "^string_of_int node);Visits.add key visits) visits stages) visits group.CallGraphSchedule.nodeIndices in
  let initialVisits=List.fold_left (fun visits group->recordStages ["ANF -> MIR"] group visits) Visits.empty components in recordPassTiming passTimingRecorder "Call Graph Scheduling" (elapsed ()-.scheduleStart);
  let graphSetupStart=elapsed () in let localIds=List.map (fun (func:MIR.functionDef)->func.MIR.id) mirFuncs |> F.of_list in
  let highestLocal=if F.is_empty localIds then AST.functionId 0L else F.max_elt localIds in let highestReservedId=if FunctionIdMap.isEmpty registries.R.functionNames then highestLocal else let highest=fst (FunctionIdMap.maxKeyValue registries.R.functionNames) in if Int64.unsigned_compare (AST.functionIdValue highestLocal) (AST.functionIdValue highest)>=0 then highestLocal else highest in
  let ambiguousLocalIds=countIds (fun (func:MIR.functionDef)->func.MIR.id) mirFuncs |> FunctionIdMap.toList |> List.filter_map (fun (id,count)->if count>1 then Some id else None) |> F.of_list in
  let directCalleeIds=List.fold_left (fun callees func->F.union callees (CallGraphSchedule.directCallees func)) F.empty mirFuncs in
  (* A current body supersedes a cached fact with the same ID. Only direct external callees can affect this unit; their summaries already include transitive facts. *)
  let externalSummaries=F.fold (fun id summaries->FunctionIdMap.add id (Option.value ~default:C.unknownSummary (FunctionIdMap.tryFind id externalSummaries)) summaries) (F.diff directCalleeIds localIds) FunctionIdMap.empty in
  if verbosity>=2 then (
   let directCalls=List.concat_map (fun (func:MIR.functionDef)->MIR.LabelMap.bindings func.MIR.cfg.MIR.blocks |> List.concat_map (fun (_, (block:MIR.basicBlock))->List.filter_map (function MIR.Call (_,id,_,_,_)|MIR.TailCall (id,_,_,_)->Some id|_->None) block.MIR.instrs)) mirFuncs in
   let ambiguous,known,unresolved=List.fold_left (fun (ambiguous,known,unresolved) id->if F.mem id ambiguousLocalIds then ambiguous+1,known,unresolved else if F.mem id localIds || Option.is_some (Option.bind (FunctionIdMap.tryFind id externalSummaries) (fun (summary:C.functionSummary)->summary.C.version)) then ambiguous,known+1,unresolved else ambiguous,known,unresolved+1) (0,0,0) directCalls in
   Output.println (Printf.sprintf "  [callgraph] functions=%d batches=%d direct=%d known=%d ambiguous=%d unresolved=%d" (List.length mirFuncs) (List.length components) (List.length directCalls) known ambiguous unresolved));
  recordPassTiming passTimingRecorder "Call Graph Setup" (elapsed ()-.graphSetupStart);
  let compileComponent knownEffectFree knownRemovable knownTypedConstants knownWrites catalog helperIds (group:CallGraphSchedule.component)=
   let* lirFuncs,typedConstants=compileMirToLir arch knownEffectFree knownRemovable knownTypedConstants verbosity options elapsed passTimingRecorder functionCaches registries stageSuffix (MIR.Program (group.CallGraphSchedule.functions,variantRegistry,mirRecordRegistry)) in
   let metadataPlanningStart=elapsed () in
   let prepared,helperIds=match arch with Platform.ARM64->ARM64PrepareFunctions.prepareARM64FunctionsForAllocationWithCache releasePlanSummaryCache (timingRecorder passTimingRecorder) registries.R.recordFieldsReg registries.R.rcSumShapeReg highestReservedId helperIds lirFuncs|Platform.X86_64->lirFuncs,helperIds in
   recordPassTiming passTimingRecorder "ARM64 Function Metadata Planning" (elapsed ()-.metadataPlanningStart);
   if verbosity>=1 then Output.println "  [lir.allocate-registers] Register Allocation...";let allocStart=elapsed () in
   let allocateFunction func=let allocate ()=allocateRegistersForFunction arch passTimingRecorder func in match functionCaches with Some (caches:C.functionCompilationCaches)->caches.C.allocateLir arch func allocate|None->allocate () in
   let callAwareStart=elapsed () in
   let callWritesForSaves=match arch with Platform.ARM64->ARM64CalleeClobbers.callWritesForSaves|Platform.X86_64->X64CalleeClobbers.callWritesForSaves in
   (* Calls introduced after MIR receive an explicit pessimistic clobber result. A unique local callee must still have been finalized by the scheduler. *)
   let calls (func:LIR.functionDef)=LIR.LabelMap.bindings func.LIR.cfg.LIR.blocks |> List.concat_map (fun (_, (block:LIR.basicBlock))->List.filter_map (function LIR.Call (_,id,_)|LIR.TailCall (id,_)->Some id|_->None) block.LIR.instrs) |> F.of_list in
   let callEdges=List.fold_left (fun edges (func:LIR.functionDef)->let existing=Option.value ~default:F.empty (FunctionIdMap.tryFind func.LIR.id edges) in FunctionIdMap.add func.LIR.id (F.union existing (calls func)) edges) FunctionIdMap.empty prepared in
   let sccPeers=List.concat_map (fun scc->let ids=List.map (fun (func:MIR.functionDef)->func.MIR.id) scc |> F.of_list in List.map (fun (func:MIR.functionDef)->func.MIR.id,ids) scc) group.CallGraphSchedule.sccs |> FunctionIdMap.ofList in
   FunctionIdMap.iter (fun caller calls->let peers=Option.value ~default:F.empty (FunctionIdMap.tryFind caller sccPeers) in F.iter (fun callee->if not (F.mem caller ambiguousLocalIds) && F.mem callee localIds && not (F.mem callee ambiguousLocalIds) && not (F.mem callee peers) then
    match FunctionIdMap.tryFind callee catalog with
    |Some (summary:C.functionSummary)->(match summary.C.version with Some version when version.C.functionId=callee && version.C.target=target && Option.is_some (targetWrites summary)->()|_->Crash.crash ("LIR introduced a call from "^functionIdString caller^" before local callee "^functionIdString callee^" was finalized"))
    |None->Crash.crash ("LIR introduced a call from "^functionIdString caller^" before local callee "^functionIdString callee^" was scheduled")) calls) callEdges;
   let callees=FunctionIdMap.fold (fun writes _ calls->F.fold (fun callee writes->if FunctionIdMap.containsKey callee writes then writes else FunctionIdMap.add callee allWrites writes) calls writes) knownWrites callEdges in
   let rec canReach target seen current=if current=target then true else if F.mem current seen then false else let next=Option.value ~default:F.empty (FunctionIdMap.tryFind current callEdges) in F.exists (canReach target (F.add current seen)) next in
   let allocated=List.map (fun (prepared:LIR.functionDef)->
    let directCallees=LIR.LabelMap.bindings prepared.LIR.cfg.LIR.blocks |> List.concat_map (fun (_, (block:LIR.basicBlock))->List.filter_map (function LIR.Call (_,id,_)->Some id|_->None) block.LIR.instrs) |> F.of_list in
    let relevantCallees=F.elements directCallees |> List.map (fun id->let writes=if canReach prepared.LIR.id F.empty id then allWrites else match FunctionIdMap.tryFind id callees with Some value->value|None->Crash.crash ("Call graph has no LIR clobber summary for "^prepared.LIR.name^"'s callee "^functionIdString id) in id,writes) |> FunctionIdMap.ofList in
    let hasPreservedCallerReg=LIR.LabelMap.exists (fun _ block->List.exists (fun writes->List.exists (fun reg->not (ARM64CalleeClobbers.containsInt reg writes)) RegisterPolicy.callerSavedRegs || List.exists (fun reg->not (ARM64CalleeClobbers.containsFloat reg writes)) (FloatAllocation.floatCallerSavedRegsFor arch)) (callWritesForSaves relevantCallees block)) prepared.LIR.cfg.LIR.blocks in
    let allocated=if hasPreservedCallerReg then let allocate ()=RegisterAllocation.allocateRegistersWithCallSummaries arch relevantCallees prepared |> LIR_Peephole.removeSelfMovesFromFunction in (match functionCaches with Some (caches:C.functionCompilationCaches)->caches.C.allocateCallAwareLir prepared relevantCallees allocate|None->allocate ()) else allocateFunction prepared in
    match arch with Platform.ARM64->allocated|Platform.X86_64->X64CalleeClobbers.pruneFunction relevantCallees allocated) prepared in
   recordPassTiming passTimingRecorder "Call-aware Allocation and Save Pruning" (elapsed ()-.callAwareStart);let duration=elapsed ()-.allocStart in recordPassTiming passTimingRecorder "Register Allocation" duration;elapsedDetail verbosity duration;Ok (allocated,typedConstants,callees,helperIds) in
  let rec compile knownPurity knownEffectFree knownRemovable knownTypedConstants knownWrites published catalog helperIds completed visits remaining=match remaining with
  |[]->let expectedVisits=I.fold (fun node visits->S.fold (fun stage visits->Visits.add (node,stage) visits) pipelineStages visits) expectedNodes Visits.empty in if not (Visits.equal visits expectedVisits) then Crash.crash "A function skipped a native compiler pipeline stage";Ok (List.concat (List.rev completed),published)
  |(group:CallGraphSchedule.component)::rest->
   (* Same-SCC calls cannot yet have final allocation facts. Every other unique local edge must resolve to a completed function. *)
   List.iter (fun scc->let sccIds=List.map (fun (func:MIR.functionDef)->func.MIR.id) scc |> F.of_list in List.iter (fun (func:MIR.functionDef)->F.iter (fun callee->if not (F.mem callee sccIds) then (
    let summary=match FunctionIdMap.tryFind callee catalog with Some value->value|None->Crash.crash ("Call graph has no summary for "^func.MIR.name^"'s callee "^functionIdString callee) in
    (match summary.C.version with Some version when version.C.functionId<>callee || version.C.target<>target->Crash.crash ("Call graph has a mismatched version for "^func.MIR.name^"'s callee "^functionIdString callee)|_->());
    if F.mem callee localIds && not (F.mem callee ambiguousLocalIds) then (match summary.C.version with Some version when version.C.functionId=callee && version.C.target=target && Option.is_some (targetWrites summary)->()|_->Crash.crash ("Call graph scheduled "^func.MIR.name^" before finalized callee "^functionIdString callee));
    if not (F.mem callee ambiguousLocalIds) then (
     let purityMatches=FunctionIdMap.tryFind callee knownPurity=Some summary.C.purity in let constantMatches=FunctionIdMap.tryFind callee knownTypedConstants=summary.C.constantReturn in let writesMatch=Option.value ~default:allWrites (FunctionIdMap.tryFind callee knownWrites)=Option.value ~default:allWrites (targetWrites summary) in
     if not (purityMatches && constantMatches && writesMatch) then Crash.crash (Printf.sprintf "Call graph facts for %s's callee %s disagree with its saved summary (purity=%s, constant=%s, writes=%s)" func.MIR.name (functionIdString callee) (string_of_bool purityMatches) (string_of_bool constantMatches) (string_of_bool writesMatch))))) (CallGraphSchedule.directCallees func)) scc) group.CallGraphSchedule.sccs;
   let purityStart=elapsed () in let purity=MIROptimizationFacts.analyzePurityWithKnown knownPurity group.CallGraphSchedule.functions |> FunctionIdMap.map (fun id summary->if F.mem id ambiguousLocalIds then MIROptimizationFacts.unknownPurity else summary) in
   let visits=recordStages ["Purity"] group visits in recordPassTiming passTimingRecorder "Call Graph Purity Summary" (elapsed ()-.purityStart);
   let effectFree=MIROptimizationFacts.analyzeEffectFreeFunctionsWithKnown knownEffectFree group.CallGraphSchedule.functions in let batchEffectFree=F.union effectFree knownEffectFree in
   let newlyRemovable=FunctionIdMap.toList purity |> List.filter_map (fun (id,summary)->if MIROptimizationFacts.isPure summary then Some id else None) |> F.of_list in
   let* allocated,typedConstants,callWrites,helperIds=compileComponent batchEffectFree knownRemovable knownTypedConstants knownWrites catalog helperIds group in
   let visits=recordStages ["MIR Optimization";"MIR -> LIR";"LIR Peephole";"Register Allocation"] group visits in
   let scheduledIds=countIds (fun (func:MIR.functionDef)->func.MIR.id) group.CallGraphSchedule.functions in let emittedIds=countIds (fun (func:LIR.functionDef)->func.LIR.id) allocated in
   if FunctionIdMap.toList scheduledIds<>FunctionIdMap.toList emittedIds then Crash.crash "Call graph batch did not emit every scheduled function";
   let clobberStart=elapsed () in let localWrites=(match arch with Platform.ARM64->ARM64CalleeClobbers.summariesWithKnown callWrites allocated|Platform.X86_64->X64CalleeClobbers.summariesWithKnown callWrites allocated) |> FunctionIdMap.filter (fun id _->not (F.mem id ambiguousLocalIds)) in
   let knownWrites=FunctionIdMap.fold (fun writes id value->FunctionIdMap.add id value writes) knownWrites localWrites in let visits=recordStages ["Clobber Summary"] group visits in recordPassTiming passTimingRecorder "Call Graph Clobber Summary" (elapsed ()-.clobberStart);
   let knownTypedConstants=FunctionIdMap.fold (fun known id value->if F.mem id ambiguousLocalIds then known else FunctionIdMap.add id value known) knownTypedConstants typedConstants in
   let batchSummaries=List.fold_left (fun summaries (func:LIR.functionDef)->
    let purity=match FunctionIdMap.tryFind func.LIR.id purity with Some value->value|None->Crash.crash "Compiled function lacks a purity summary" in
    let writes=if F.mem func.LIR.id ambiguousLocalIds then None else (match FunctionIdMap.tryFind func.LIR.id knownWrites with Some value->Some value|None->Crash.crash (match arch with Platform.ARM64->"Final ARM64 function lacks a clobber summary"|Platform.X86_64->"Final x64 function lacks a clobber summary")) in
    let summary={C.version=Some (C.functionVersion stageSuffix func.LIR.id target options func);purity;constantReturn=FunctionIdMap.tryFind func.LIR.id typedConstants;arm64Writes=(match arch with Platform.ARM64->writes|Platform.X86_64->None);x64Writes=(match arch with Platform.X86_64->writes|Platform.ARM64->None)} in C.mergeFunctionSummaries summaries (FunctionIdMap.ofList [func.LIR.id,summary])) FunctionIdMap.empty allocated in
   let published=C.mergeFunctionSummaries published batchSummaries in let catalog=C.mergeFunctionSummaries catalog batchSummaries in
   let knownPurity=FunctionIdMap.fold (fun known id value->if F.mem id ambiguousLocalIds then known else FunctionIdMap.add id value known) knownPurity purity in
   let knownEffectFree=F.fold (fun id known->if F.mem id ambiguousLocalIds then known else F.add id known) effectFree knownEffectFree in
   let knownRemovable=F.fold (fun id known->if F.mem id ambiguousLocalIds then known else F.add id known) newlyRemovable knownRemovable in
   compile knownPurity knownEffectFree knownRemovable knownTypedConstants knownWrites published catalog helperIds (allocated::completed) visits rest in
  let initialFactsStart=elapsed () in
  let initialPurity=FunctionIdMap.map (fun _ (summary:C.functionSummary)->summary.C.purity) externalSummaries in
  let initialRemovable=FunctionIdMap.toList initialPurity |> List.filter_map (fun (id,summary)->if MIROptimizationFacts.isPure summary then Some id else None) |> F.of_list in
  let initialTypedConstants=FunctionIdMap.toList externalSummaries |> List.filter_map (fun (id,(summary:C.functionSummary))->Option.map (fun value->id,value) summary.C.constantReturn) |> FunctionIdMap.ofList in
  let initialWrites=FunctionIdMap.toList externalSummaries |> List.filter_map (fun (id,summary)->Option.map (fun value->id,value) (targetWrites summary)) |> FunctionIdMap.ofList in
  let initialCatalog=F.fold (fun id summaries->FunctionIdMap.add id C.unknownSummary summaries) ambiguousLocalIds externalSummaries in recordPassTiming passTimingRecorder "Call Graph Initial Facts" (elapsed ()-.initialFactsStart);
  compile initialPurity initialRemovable initialRemovable initialTypedConstants initialWrites FunctionIdMap.empty initialCatalog M.empty [] initialVisits components) in
 let compileWithTiming label funcs=if funcs=[] then Ok ([],FunctionIdMap.empty) else let start=elapsed () in Result.map (fun compiled->recordPassTiming passTimingRecorder label (elapsed ()-.start);compiled) (compileFunctions funcs) in
 let* compiled,summaries=compileWithTiming "Call Graph Compilation" functions in
 (* Keep per-name queues so duplicate function names from different compilation units preserve distinct bodies in original order. *)
 let queues=List.fold_right (fun (func:LIR.functionDef) queues->let existing=Option.value ~default:[] (M.find_opt func.LIR.name queues) in M.add func.LIR.name (func::existing) queues) compiled M.empty in
 let rec rebuildOrder names queues accumulated=match names with []->List.rev accumulated|name::rest->(match M.find_opt name queues with Some (next::remaining)->let queues=if remaining=[] then M.remove name queues else M.add name remaining queues in rebuildOrder rest queues (next::accumulated)|_->Crash.crash ("lowerToAllocatedLir: missing compiled function for '"^name^"'")) in
 Ok (rebuildOrder functionOrder queues [],summaries)
let lowerToAllocatedLirWithKnown externalSummaries target verbosity options elapsed recorder caches releaseCache suffix functions typeMap registries projected returnTypes=lowerToAllocatedLirWithKnownGroups externalSummaries target verbosity options elapsed recorder caches releaseCache suffix [functions,typeMap] registries projected returnTypes
