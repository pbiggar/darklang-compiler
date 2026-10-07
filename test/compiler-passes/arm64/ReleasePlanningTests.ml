(*
   ReleasePlanningTests.ml - Verify planned release outlining and cache behavior.
   Native record descriptors are compile-time metadata. This fixture locks the
   compact payload layout at ARM64 codegen: fields begin at byte zero and no
   descriptor immediate is materialized in the heap object.
*)
[@@@warning "-4-42"]
open Dark_compiler
open Fixtures
module L=LIR
module S=Symbolic
module M=StringOrder.Map
module G=Backend_Arm64_CodeGen
let testCompactRecordFieldsStartAtOffsetZero ()=
 let records=M.singleton "Arm64CompactRecord" ["left",AST.TInt64;"right",AST.TInt64] in
 let program=makeSimpleProgramWithRecords [L.HeapAlloc (L.Physical L.X1,16);L.HeapStore (L.Physical L.X1,0,L.Imm 10L,None);L.HeapStore (L.Physical L.X1,8,L.Imm 20L,None)] records in
 match generatePreparedARM64 target program with Error error->Error ("Compact record ARM64 lowering failed: "^error)|Ok instructions->
 let offsets=List.filter_map (function S.STR (S.X9,S.X1,offset)->Some offset|_->None) instructions in
 if offsets=[0;8] then Ok () else Error "Expected compact record field stores at offsets 0 and 8 only"
let emitsPlannedListHelperLabel instrs=List.exists (function S.Label label|S.BL label->Text.startsWith label "__dark_list_refcount_dec_plan_"|_->false) instrs
let testSmallGenericReleasePlanRemainsInline ()=
 let valueType=AST.TTuple [AST.TString] in
 let program=makeSimpleProgramWithVariants [L.RefCountDec (L.Physical L.X0,8,L.GenericHeap,Some (rcMetadata valueType))] M.empty |> ARM64PrepareFunctions.prepareARM64Program in
 let generated=Queue.create () in
 let cache (func:L.functionDef) generate=Queue.add func.L.name generated;generate () in
 match G.generateARM64WithOptionsAndCache target ARM64CodeGenTypes.defaultOptions (Some cache) None program with
 |Error error->Error ("Small generic release lowering failed: "^error)|Ok _->if List.of_seq (Queue.to_seq generated)=["_start"] then Ok () else Error "Small generic release plan should cache only the stable entry trampoline"
let requirements (func:L.functionDef)=Option.bind func.L.codegenFacts (fun facts->facts.L.arm64RcHelperRequirements)
let helperNames func=requirements func |> Option.map (fun r->M.bindings r.L.plannedGenericDecHelpers |> List.map fst) |> Option.value ~default:[]
let helperIds functions=AST.allocateFunctionIds (List.to_seq (List.map (fun (f:L.functionDef)->f.L.id) functions)) (List.to_seq (List.concat_map helperNames functions))
let instructions functions=List.concat_map (fun (func:L.functionDef)->L.LabelMap.bindings func.L.cfg.L.blocks |> List.concat_map (fun (_,block)->block.L.instrs)) functions
let testExpensiveGenericReleaseIsPreparedAsCall ()=
 let valueType=AST.TTuple (List.init 32 (fun _->AST.TString)) in let source=L.Virtual 42 in
 let L.Program (functions,_,_)=makeSimpleProgramWithVariants [L.RefCountDec (source,256,L.GenericHeap,Some (rcMetadata valueType))] M.empty |> ARM64PrepareFunctions.prepareARM64Program in
 let ids=helperIds functions in let helpers=List.concat_map helperNames functions |> List.filter_map (fun name->M.find_opt name ids) in
 match instructions functions with
 |[L.SaveRegs ([],[]);L.ArgMoves [L.X0,L.Reg argMoveSource];L.Call (L.Physical L.X0,helperLabel,[L.Reg callSource]);L.RestoreRegs ([],[])] when argMoveSource=source && callSource=source && List.mem helperLabel helpers->Ok ()
 |_->Error "Expected an expensive generic release to become one allocator-visible helper call"
let testGenericReleaseHelperPreservesCachedInstructions ()=
 let metadata=rcMetadata (AST.TTuple (List.init 32 (fun _->AST.TString))) in
 let program=makeSimpleProgramWithVariants [L.RefCountDec (L.Physical L.X0,256,L.GenericHeap,Some metadata);L.RefCountDec (L.Physical L.X0,256,L.GenericHeap,Some metadata)] M.empty |> ARM64PrepareFunctions.prepareARM64Program in
 let target=ARM64.targetConfigFor Platform.LinuxARM64 in let generated=Queue.create () and entries=Hashtbl.create 4 in
 let cache (func:L.functionDef) generate=match Hashtbl.find_opt entries func with Some result->result|None->let result=generate () in Queue.add func.L.name generated;Hashtbl.add entries func result;result in
 match G.generateARM64WithOptions target ARM64CodeGenTypes.defaultOptions program,G.generateARM64WithOptionsAndCache target ARM64CodeGenTypes.defaultOptions (Some cache) None program with
 |Error error,_|_,Error error->Error ("Generic release helper lowering failed: "^error)
 |Ok uncachedProgram,Ok cachedProgram->
 let uncached=G.generatedProgramInstructions uncachedProgram and cached=G.generatedProgramInstructions cachedProgram in
 let calls=List.filter_map (function S.BL label when Text.startsWith label "__dark_generic_refcount_dec_plan_"->Some label|_->None) cached in
 let labels=List.filter_map (function S.Label label when Text.startsWith label "__dark_generic_refcount_dec_plan_"->Some label|_->None) cached in
 if cached<>uncached then Error "Caching changed outlined generic release instructions" else
 match List.of_seq (Queue.to_seq generated) with
 |["_start";helper] when Text.startsWith helper "__dark_generic_refcount_dec_plan_"->
   (match calls,labels with [first;second],[helper] when first=helper && second=helper->Ok ()|_->Error "Expected two calls to one generic release helper")
 |_->Error "Expected the caller and one generic helper in the function cache"
let testOutlinedGenericReleaseUsesAllocatorLiveness ()=
 let valueType=AST.TTuple (List.init 32 (fun _->AST.TString)) in let liveAcrossCall=L.Virtual 40 and released=L.Virtual 41 and result=L.Virtual 42 in
 let L.Program (functions,variants,records)=makeSimpleProgramWithVariants [L.Mov (liveAcrossCall,L.Imm 10L);L.Mov (released,L.Imm 0L);L.RefCountDec (released,256,L.GenericHeap,Some (rcMetadata valueType));L.Add (result,liveAcrossCall,L.Imm 1L)] M.empty |> ARM64PrepareFunctions.prepareARM64Program in
 let allocatedFunctions=List.map (RegisterAllocation.allocateRegisters Platform.ARM64) functions in
 let saves=instructions allocatedFunctions |> List.filter_map (function L.SaveRegs (ints,floats)->Some (ints,floats)|_->None) in
 match G.generateARM64 target (L.Program (allocatedFunctions,variants,records)) with
 |Error error->Error ("Allocated generic release helper lowering failed: "^error)|Ok generated->
 let saveAll=G.generatedProgramInstructions generated |> List.exists (function S.STP_pre (_,_,S.SP,-128)->true|_->false) in
 match saves with [(ints,[])] when List.length ints<7 && not saveAll->Ok ()|_->Error "Expected allocator-selected caller saves"
let testGenericReleaseHelpersPreserveOwnershipPolicy ()=
 let name="ARM64OutlinedOwnership" in let payloadType=AST.TTuple (List.init 32 (fun _->AST.TString)) in let sumType=AST.TSum (name,[]) in
 let variants=M.singleton name {L.typeParams=[];variants=[{L.name="Only";tag=0;payload=Some payloadType;fieldCount=2}]} in
 let sums=M.singleton name {MemoryModel.typeParams=[];payloads=[0,Some payloadType];unaryPayloadTags=MemoryModel.IntSet.empty} in
 let metadata=rcMetadataWithSumShapes sums sumType in
 let makeFunction name:L.functionDef=let entry=L.Label (name^"_entry") in {L.id=TestIds.functionIdForName name;name;typedParams=[];cfg={L.entry;blocks=L.LabelMap.singleton entry {L.label=entry;instrs=[L.RefCountDec (L.Physical L.X0,264,L.GenericHeap,Some metadata)];terminator=L.Ret}};stackSize=0;usedCalleeSaved=[];codegenFacts=None} in
 let L.Program (functions,_,_)=L.Program ([makeFunction "User.owns";makeFunction "Darklang.Stdlib.List.borrows"],variants,M.empty) |> ARM64PrepareFunctions.prepareARM64Program in
 let ids=helperIds functions in
 let helperInfo func=
  let namesById=helperNames func |> List.filter_map (fun name->Option.map (fun id->id,name) (M.find_opt name ids)) |> FunctionIdMap.ofList in
  let label=instructions [func] |> List.find_map (function L.Call (_,id,_)->FunctionIdMap.tryFind id namesById|_->None) in
  let specs=requirements func |> Option.map (fun r->M.bindings r.L.plannedGenericDecHelpers |> List.map snd) |> Option.value ~default:[] in label,specs in
 match List.map helperInfo functions with
 |[Some owned,[ownedSpec];Some borrowed,[borrowedSpec]] when Text.endsWith owned "_owned" && Text.endsWith borrowed "_borrowed" && ownedSpec.L.ownsSinglePayloadSum && not borrowedSpec.L.ownsSinglePayloadSum->Ok ()
 |_->Error "Expected distinct owned and borrowed generic release helpers"
