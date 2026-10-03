(* Observe complete float allocation, load scheduling and spill repair results. *)
[@@@warning "-4"]
open Dark_compiler
open AllocationModel
module F = FloatAllocation
module L = LIR
let tuple values = `Assoc ["tuple",`List values]
let list encode values = `List (List.map encode values)
let array encode values = `List (Array.to_list (Array.map encode values))
let i = SemanticJson.int32
let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value]
let domain (value:vRegDomain) = SemanticJson.record "VRegDomain" ["Ids",array i value.ids;"IndexOf",array i value.indexOf;"IndexOffset",i value.indexOffset;"WordCount",i value.wordCount]
let fAllocation = function F.FPhysReg reg -> SemanticJson.union "FAllocation" "FPhysReg" [ProductionLIR.physFPReg reg] | F.FStackSlot slot -> SemanticJson.union "FAllocation" "FStackSlot" [i slot] | F.FRematerialized value -> SemanticJson.union "FAllocation" "FRematerialized" [`Assoc ["kind",`String "float64";"value",`String (Printf.sprintf "%016Lx" (Int64.bits_of_float value))]]
let allocated (value:F.fAllocationResult) = SemanticJson.record "FAllocationResult" ["Domain",domain value.F.domain;"Allocations",array (option fAllocation) value.F.allocations;"StackSize",i value.F.stackSize;"UsedCalleeSavedF",list ProductionLIR.physFPReg value.F.usedCalleeSavedF;"SpillScratchLeft",ProductionLIR.fReg value.F.spillScratchLeft;"SpillScratchRight",ProductionLIR.fReg value.F.spillScratchRight;"SpillScratchThird",ProductionLIR.fReg value.F.spillScratchThird]
let attempt encode action = try SemanticJson.union "FSharpResult" "Ok" [encode (action ())] with Failure message | Invalid_argument message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let block label instrs terminator : L.basicBlock = {L.label;instrs;terminator}
let cfg entry blocks : L.cfg = {L.entry;blocks=L.LabelMap.of_list (List.map (fun (b:L.basicBlock) -> b.L.label,b) blocks)}
let observe source =
 let label=L.Label source in let other=L.Label "other" in
 let values=List.map Int64.float_of_bits [Int64.min_int;0x7ff8000000000001L;0x7ff0000000000000L;0xfff0000000000000L;0x3ff0000000000000L] in
 let ids=[-2000;-1002;-1001;-1000;-1;0;1;2;3;4;7;9;19] in let d=buildVRegDomain ids in
 let fregs=[L.FVirtual (-1);L.FVirtual 7;L.FVirtual (-1000);L.FVirtual (-1001);L.FVirtual (-1002);L.FVirtual (-2000);L.FVirtual 987;L.FPhysical L.D4] in
 let repairCases=List.concat_map (fun mode -> List.map (fun scratch ->
  let allocations=Array.mapi (fun n _ -> match mode with
   | 0 -> Some (F.FPhysReg (List.nth F.allocatableFloatRegs (n mod 16)))
   | 1 -> Some (F.FStackSlot (-(n+1)*8))
   | 2 -> Some (F.FRematerialized (List.nth values (n mod 5)))
   | 3 -> (match n mod 4 with 0 -> None | 1 -> Some (F.FPhysReg L.D0) | 2 -> Some (F.FStackSlot (-24)) | _ -> Some (F.FRematerialized (-0.)))
   | _ -> None) d.ids in
  let allocation:F.fAllocationResult={F.domain=d;allocations;stackSize=48;usedCalleeSavedF=[L.D8;L.D15];spillScratchLeft=(if scratch=0 then L.FVirtual (-1000) else L.FPhysical L.D14);spillScratchRight=(if scratch=0 then L.FVirtual (-1001) else L.FPhysical L.D15);spillScratchThird=L.FVirtual (-1002)} in
  let repairs=List.concat_map (fun freg -> List.concat_map (fun typ ->
   List.map (fun instr -> let b=block label [instr] L.Ret in let graph=cfg label [b] in
    tuple [ProductionLIR.instr instr;attempt (list ProductionLIR.instr) (fun () -> F.applyFloatAllocationToInstrs allocation instr);attempt ProductionLIR.basicBlock (fun () -> F.applyFloatAllocationToBlock allocation b);attempt (array ProductionLIR.basicBlock) (fun () -> F.applyFloatAllocationToBlocks allocation [|b|]);attempt ProductionLIR.cfg (fun () -> F.applyFloatAllocationToCFG allocation graph)])
    (LIRFixtures.instructionsWithRegisters source (L.Virtual 3) freg (L.Reg (L.Virtual 3)) typ)) [AST.TInt64;AST.TFloat64]) fregs in
  let moves=[L.FArgMoves [L.D0,L.FPhysical L.D1;L.D1,L.FPhysical L.D0];L.FArgMoves [L.D0,L.FVirtual 1;L.D1,L.FVirtual 2;L.D2,L.FVirtual 3];L.FArgMoves [L.D0,L.FPhysical L.D1;L.D1,L.FVirtual 3];L.FArgMoves [];L.FPhi (L.FVirtual 987,[L.FVirtual 7,label])]
   |> List.map (fun instr -> tuple [ProductionLIR.instr instr;attempt (list ProductionLIR.instr) (fun () -> F.applyFloatAllocationToInstrs allocation instr)]) in
  tuple [allocated allocation;list (fun id -> tuple [i id;option fAllocation (F.tryFloatAllocation allocation id)]) (ids @ [987]);list (fun freg -> attempt ProductionLIR.fReg (fun () -> F.applyFloatAllocationToFReg allocation freg)) fregs;`List repairs;`List moves]) [0;1]) [0;1;2;3;4] in
 let schedules=[[];[L.FLoad (L.FVirtual 1,-0.);L.Mov (L.Virtual 3,L.Imm 1L);L.FLoad (L.FVirtual 2,1.);L.FAdd (L.FVirtual 3,L.FVirtual 2,L.FVirtual 1)];[L.FLoad (L.FVirtual 1,1.);L.FLoad (L.FVirtual 1,2.);L.PrintFloat (L.FVirtual 1)];[L.PrintFloat (L.FVirtual 1);L.FLoad (L.FVirtual 1,1.)];[L.FLoad (L.FVirtual 1,1.);L.FPhi (L.FVirtual 2,[L.FVirtual 1,label])];[L.FLoad (L.FVirtual 2,2.);L.FLoad (L.FVirtual 1,1.);L.FAdd (L.FVirtual 3,L.FVirtual 1,L.FVirtual 2)]] in
 let scheduling=List.map (fun instructions -> let b=block label instructions (L.Jump other) in let graph=cfg label [b;block other [L.FPhi (L.FVirtual 7,[L.FVirtual 1,label])] L.Ret] in tuple [ProductionLIR.basicBlock (F.scheduleFloatLoadsInBlock b);ProductionLIR.cfg (F.scheduleFloatLoadsInCFG graph)]) schedules in
 let pressure literal count = let loads=List.init count (fun n -> if literal then L.FLoad (L.FVirtual n,List.nth values (n mod 5)) else L.Int64ToFloat (L.FVirtual n,L.Virtual 3)) in
  let uses=List.init count (fun n -> L.PrintFloat (L.FVirtual n)) in cfg label [block label (loads @ uses) L.Ret] in
 let fixtureCFGs=List.map (fun instr -> cfg label [block label [instr] L.Ret]) (LIRFixtures.instructions source) in
 let cfgs=fixtureCFGs @ List.map (fun instructions -> cfg label [block label instructions L.Ret]) schedules @
  [pressure true 33;pressure false 33;pressure false 65;cfg label [block label [L.FLoad (L.FVirtual 1,1.);L.FMov (L.FVirtual 2,L.FVirtual 1)] (L.Jump other);block other [L.FPhi (L.FVirtual 3,[L.FVirtual 2,label;L.FVirtual 7,other]);L.PrintFloat (L.FVirtual 3)] (L.Jump label)]] in
 let allocations=List.map (fun graph ->
  let scheduled=F.scheduleFloatLoadsInCFG graph in let idx,blocks=buildBlockIndex scheduled in let facts=RegisterFacts.classifyBlocks blocks in
  let variants=List.concat_map (fun extras -> let domain,liveness=RegisterLiveness.computeFloatLivenessBitsFromFacts idx facts extras in
   List.concat_map (fun registers -> List.concat_map (fun initial -> List.map (fun precolors -> attempt (fun allocation ->
    tuple [allocated allocation;attempt ProductionLIR.cfg (fun () -> F.applyFloatAllocationToCFG allocation scheduled)])
    (fun () -> F.chordalFloatAllocationWithLiveness registers initial idx blocks facts (vregBitsFromList domain extras) precolors domain liveness)) [[];[1,0;3,1;7,0];[1,1;3,0;7,1]]) [0;8;24]) [[];[L.D0];[L.D0;L.D1];F.allocatableFloatRegs;F.allocatableFloatRegsFor Platform.X86_64]) [[];[1;3;7]] in
  tuple [ProductionLIR.cfg scheduled;`List variants;attempt allocated (fun () -> F.chordalFloatAllocation graph []);attempt allocated (fun () -> F.chordalFloatAllocation graph [1;3;7])]) cfgs in
 tuple [list ProductionLIR.physFPReg F.floatCallerSavedRegs;list ProductionLIR.physFPReg F.floatCalleeSavedRegs;list ProductionLIR.physFPReg F.allocatableFloatRegs;
  list (fun arch -> tuple [list ProductionLIR.physFPReg (F.allocatableFloatRegsFor arch);list ProductionLIR.physFPReg (F.floatCallerSavedRegsFor arch)]) [Platform.ARM64;Platform.X86_64];list (fun reg -> i (F.physFPRegToInt reg)) F.allocatableFloatRegs;`List repairCases;`List scheduling;`List allocations]
