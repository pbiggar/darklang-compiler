(* ARM64PrepareFunctions.ml - Attach target planning facts and outline expensive release operations. *)
open ARM64CodeGenTypes
open ReleasePlanSummary
let helperLabelsForRequirements (requirements:LIR.arm64RcHelperRequirements)=
 StringOrder.Map.to_seq requirements.LIR.plannedGenericDecHelpers |> Seq.map fst |> StringOrder.Set.of_seq
let helperLabelsForFunctions functions=
 List.fold_left (fun labels (func:LIR.functionDef) ->
  let local=Option.bind func.LIR.codegenFacts (fun facts -> facts.LIR.arm64RcHelperRequirements) |> Option.map helperLabelsForRequirements |> Option.value ~default:StringOrder.Set.empty in
  StringOrder.Set.union labels local) StringOrder.Set.empty functions
(* Attach backend-specific helper planning to a compilation batch. Release
   plans shared by sibling functions are traversed once, while each function
   retains only its own semantic requirements for later tree-shaken unions. *)
let attachARM64CodegenFactsToFunctionsWithCache summaryCache recordRegistry sumShapeRegistry functions=
 let _,reversed=List.fold_left (fun (releasePlanSummaries,functions) (func:LIR.functionDef) ->
  let facts=match func.LIR.codegenFacts with Some facts->facts|None->Crash.crash ("ARM64 metadata planning requires LIR facts for function '"^func.LIR.name^"'") in
  let plannedSlotInitFacts=planRawSlotInitRetainTargets recordRegistry sumShapeRegistry facts in
  let requirementsWithMemo=planFunctionArm64RcRequirements summaryCache releasePlanSummaries func.LIR.name plannedSlotInitFacts in
  let functionRequirements={requirementsWithMemo with LIR.releasePlanSummaries=LIR.ReleasePlanSummaryMap.empty} in
  let plannedFacts={plannedSlotInitFacts with LIR.arm64RcHelperRequirements=Some functionRequirements} in
  requirementsWithMemo.LIR.releasePlanSummaries,{func with LIR.codegenFacts=Some plannedFacts}::functions) (LIR.ReleasePlanSummaryMap.empty,[]) functions in
 List.rev reversed
let attachARM64CodegenFactsToFunctions functions=
 attachARM64CodegenFactsToFunctionsWithCache None StringOrder.Map.empty StringOrder.Map.empty functions |> List.map (fun (func:LIR.functionDef) ->
  let facts=Option.map (fun facts -> {facts with LIR.arm64RawSlotInitRetainTargets=None}) func.LIR.codegenFacts in {func with LIR.codegenFacts=facts})
let outlineExpensiveGenericReleasesInFunction helperIds (func:LIR.functionDef)=
 let requirements=match Option.bind func.LIR.codegenFacts (fun facts -> facts.LIR.arm64RcHelperRequirements) with
  | None->Crash.crash ("ARM64 generic release outlining requires helper facts for '"^func.LIR.name^"'")
  | Some requirements->requirements in
 let helperLabelsByMemoKey=StringOrder.Map.bindings requirements.LIR.plannedGenericDecHelpers |> List.concat_map (fun (label,spec) -> LIR.RcReleasePlanMemoKeySet.elements spec.LIR.releasePlanMemoKeys |> List.map (fun memoKey -> memoKey,label)) |> List.fold_left (fun map (key,label) -> LIR.ReleasePlanSummaryMap.add (false,key) label map) LIR.ReleasePlanSummaryMap.empty in
 if LIR.ReleasePlanSummaryMap.is_empty helperLabelsByMemoKey then func else
 let outlineInstr=function
  | LIR.RefCountDec (addr,_,LIR.GenericHeap,metadata) as instruction ->
   (match LIR.ReleasePlanSummaryMap.find_opt (false,LIR.rcReleasePlanMemoKey metadata) helperLabelsByMemoKey with
    | None->[instruction]
    | Some helperLabel->
     (* The physical destination declares that this effect has no
        virtual result while retaining normal call liveness. *)
     let helperId=match StringOrder.Map.find_opt helperLabel helperIds with Some id->id|None->Crash.crash ("ARM64 helper identity is absent: "^helperLabel) in
     [LIR.SaveRegs ([],[]);LIR.ArgMoves [LIR.X0,LIR.Reg addr];LIR.Call (LIR.Physical LIR.X0,helperId,[LIR.Reg addr]);LIR.RestoreRegs ([],[])])
  | instruction->[instruction]
 in
 let blocks=LIR.LabelMap.map (fun (block:LIR.basicBlock) -> {block with LIR.instrs=List.concat_map outlineInstr block.LIR.instrs}) func.LIR.cfg.LIR.blocks in
 {func with LIR.cfg={func.LIR.cfg with LIR.blocks=blocks}}
[@@warning "-4"]
(* Plan ARM64 helpers from finalized symbolic LIR, then expose expensive
   generic releases as ordinary calls before register allocation. Attached
   facts still describe the original release effects and survive allocation. *)
let prepareARM64FunctionsForAllocationWithCache summaryCache phaseRecorder recordRegistry sumShapeRegistry highestReservedId knownHelperIds functions=
 let recordPhase name started=match phaseRecorder with Some record->record name ((Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) -. started)|None->() in
 let factsTimer=(Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) in
 let functionsWithFacts=attachARM64CodegenFactsToFunctionsWithCache summaryCache recordRegistry sumShapeRegistry functions in
 recordPhase "ARM64 Function Facts Planning" factsTimer;
 let outliningTimer=(Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) in
 let newHelperLabels=helperLabelsForFunctions functionsWithFacts |> StringOrder.Set.filter (fun label -> not (StringOrder.Map.mem label knownHelperIds)) in
 let newHelperIds=AST.allocateFunctionIds (Seq.append (List.to_seq [highestReservedId]) (StringOrder.Map.to_seq knownHelperIds |> Seq.map snd)) (StringOrder.Set.to_seq newHelperLabels) in
 let helperIds=StringOrder.Map.fold StringOrder.Map.add newHelperIds knownHelperIds in
 let helperId label=match StringOrder.Map.find_opt label helperIds with Some id->id|None->Crash.crash ("ARM64 helper '"^label^"' has no allocated identity") in
 let outlinedFunctions=List.map (fun (func:LIR.functionDef) ->
  let localIds=StringOrder.Set.fold (fun label ids -> StringOrder.Map.add label (helperId label) ids) (helperLabelsForFunctions [func]) StringOrder.Map.empty in
  let facts=Option.map (fun facts -> {facts with LIR.arm64GenericHelperIds=localIds}) func.LIR.codegenFacts in
  outlineExpensiveGenericReleasesInFunction helperIds {func with LIR.codegenFacts=facts}) functionsWithFacts in
 recordPhase "ARM64 Generic Release Outlining" outliningTimer;
 outlinedFunctions,helperIds
let prepareARM64FunctionsForAllocation functions=
 let functionsWithFacts=attachARM64CodegenFactsToFunctions functions in
 let helperLabels=helperLabelsForFunctions functionsWithFacts in
 let helperIds=AST.allocateFunctionIds (List.to_seq (List.map (fun (func:LIR.functionDef) -> func.LIR.id) functionsWithFacts)) (StringOrder.Set.to_seq helperLabels) in
 let helperId label=match StringOrder.Map.find_opt label helperIds with Some id->id|None->Crash.crash ("ARM64 helper '"^label^"' has no allocated identity") in
 List.map (fun (func:LIR.functionDef) ->
  let localIds=StringOrder.Set.fold (fun label ids -> StringOrder.Map.add label (helperId label) ids) (helperLabelsForFunctions [func]) StringOrder.Map.empty in
  let facts=Option.map (fun facts -> {facts with LIR.arm64GenericHelperIds=localIds}) func.LIR.codegenFacts in
  outlineExpensiveGenericReleasesInFunction helperIds {func with LIR.codegenFacts=facts}) functionsWithFacts
(* Explicit preparation entry point for tools that construct LIR directly.
   Production performs the same preparation before register allocation. *)
let prepareARM64Program (LIR.Program (functions,variants,records))=
 let sumShapeRegistry=rcSumShapeRegistryFromVariantRegistry variants in
 let highest=List.fold_left (fun highest (func:LIR.functionDef) -> if Int64.unsigned_compare (AST.functionIdValue func.LIR.id) (AST.functionIdValue highest)>0 then func.LIR.id else highest) (AST.functionId 0L) functions in
 let functionsWithFacts,_=prepareARM64FunctionsForAllocationWithCache None None records sumShapeRegistry highest StringOrder.Map.empty (List.map (fun (func:LIR.functionDef) -> match func.LIR.codegenFacts with Some _->func|None->LIR.attachFunctionCodegenFacts func) functions) in
 LIR.Program (functionsWithFacts,variants,records)
