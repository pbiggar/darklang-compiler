(* Compare complete phi-edge moves, dead phi elimination, cycles and diagnostics. *)
[@@@warning "-4"]
open Dark_compiler
open AllocationModel
module L = LIR
module F = FloatAllocation
let tuple values = `Assoc ["tuple",`List values]
let list encode values = `List (List.map encode values)
let array encode values = `List (Array.to_list (Array.map encode values))
let attempt encode action = try SemanticJson.union "FSharpResult" "Ok" [encode (action ())] with Failure message | Invalid_argument message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let block label instrs terminator : L.basicBlock = {L.label;instrs;terminator}
let cfg entry blocks : L.cfg = {L.entry;blocks=L.LabelMap.of_list (List.map (fun (b:L.basicBlock) -> b.L.label,b) blocks)}
let observe source =
 let left=L.Label "left" in let right=L.Label "right" in let merge=L.Label source in let missing=L.Label "missing" in
 let vr id=L.Reg (L.Virtual id) in
 let intMapping domain mode =
  let allocations=Array.mapi (fun n _ -> match mode with 0 -> Some (PhysReg (List.nth [L.X0;L.X1;L.X2;L.X3;L.X4;L.X5;L.X6;L.X7] (n mod 8))) | 1 -> Some (StackSlot (-(n+1)*8)) | 2 -> Some (if n mod 2=0 then PhysReg L.X3 else StackSlot (-(n+1)*8)) | _ -> None) domain.ids in
  {domain;allocations;stackSize=64;usedCalleeSaved=[]} in
 let floatMapping domain mode scratch : F.fAllocationResult =
  let allocations=Array.mapi (fun n _ -> match mode with 0 -> Some (F.FPhysReg (List.nth F.allocatableFloatRegs (n mod 16))) | 1 -> Some (F.FStackSlot (-(n+1)*8)) | 2 -> Some (F.FRematerialized (Int64.float_of_bits (if n mod 2=0 then Int64.min_int else 0x7ff8000000000001L))) | 3 -> (match n mod 3 with 0 -> Some (F.FPhysReg L.D0) | 1 -> Some (F.FStackSlot (-24)) | _ -> None) | _ -> None) domain.ids in
  {F.domain;allocations;stackSize=64;usedCalleeSavedF=[];spillScratchLeft=(if scratch=0 then L.FVirtual (-1000) else L.FPhysical L.D14);spillScratchRight=(if scratch=0 then L.FVirtual (-1001) else L.FPhysical L.D15);spillScratchThird=L.FVirtual (-1002)} in
 let domain=buildVRegDomain [-9;0;1;2;3;4;5;7] in
 let fregs=[L.FVirtual 1;L.FVirtual 2;L.FVirtual 7;L.FVirtual (-1);L.FVirtual 987;L.FPhysical L.D0;L.FPhysical L.D1] in
 let moveLists=List.concat_map (fun dest -> List.map (fun src -> [dest,src]) fregs) fregs @
  [[];[L.FVirtual 1,L.FVirtual 2;L.FVirtual 2,L.FVirtual 1];[L.FVirtual 1,L.FVirtual 2;L.FVirtual 2,L.FVirtual 3;L.FVirtual 3,L.FVirtual 1];[L.FPhysical L.D0,L.FPhysical L.D1;L.FPhysical L.D1,L.FPhysical L.D0];[L.FVirtual 1,L.FVirtual 2;L.FVirtual 2,L.FVirtual 7]] in
 let moves=List.concat_map (fun mode -> List.map (fun scratch -> let allocation=floatMapping domain mode scratch in list (fun moves -> tuple [list (fun (dest,src) -> tuple [ProductionLIR.fReg dest;ProductionLIR.fReg src]) moves;attempt (list ProductionLIR.instr) (fun () -> PhiResolution.generateFloatMoveInstrsWithAllocation moves allocation)]) moveLists) [0;1]) [0;1;2;3;4] in
 let make instructions term = cfg left [block left [] (L.Jump merge);block right [] (L.Jump merge);block merge instructions term] in
 let operands=[vr 2;L.Reg (L.Physical L.X0);L.Imm Int64.min_int;L.StackSlot (-24);L.StringSymbol source;L.FloatImm (-0.);L.FloatSymbol (Int64.float_of_bits 0x7ff8000000000001L);L.FuncAddr (AST.functionId (-1L))] in
 let operandCFGs=List.map (fun op -> make [L.Phi (L.Virtual 1,[op,left;vr 1,right;vr 7,missing],Some AST.TInt64);L.PrintInt64 (L.Virtual 1)] L.Ret) operands in
 let patterns=[make [] L.Ret;
  make [L.Phi (L.Virtual 1,[vr 2,left;vr 1,right],None);L.Phi (L.Virtual 2,[vr 1,left;vr 2,right],None);L.PrintInt64 (L.Virtual 1)] L.Ret;
  make [L.Phi (L.Virtual 987,[vr 7,left],None)] L.Ret;
  make [L.Phi (L.Virtual 987,[vr 7,left],None)] (L.BranchZero (L.Virtual 987,left,right));
  make [L.Phi (L.Physical L.X0,[vr 1,left;vr 7,right],None);L.Phi (L.Virtual 1,[vr 2,left],None);L.Phi (L.Virtual 2,[vr 3,right],None)] L.Ret;
  make [L.Phi (L.Virtual 1,[vr 2,missing],None)] (L.BranchZero (L.Virtual 1,left,right));
  make [L.Phi (L.Virtual 1,[vr 2,left],None);L.Phi (L.Virtual 2,[vr 1,right],None)] L.Ret;
  make [L.FPhi (L.FVirtual 1,[L.FVirtual 2,left;L.FVirtual 1,right]);L.FPhi (L.FVirtual 2,[L.FVirtual 1,left;L.FVirtual 2,right])] L.Ret;
  make [L.FPhi (L.FPhysical L.D0,[L.FVirtual 7,left;L.FPhysical L.D1,right])] L.Ret;
  make [L.FPhi (L.FVirtual 987,[L.FVirtual 7,missing])] L.Ret;
  make [L.FPhi (L.FVirtual 987,[L.FVirtual 7,left])] L.Ret;
  cfg left [block left [L.FArgMoves [L.D0,L.FVirtual 1];L.TailCall (AST.functionId 3L,[])] (L.Jump merge);block merge [L.FPhi (L.FVirtual 2,[L.FVirtual 1,left])] L.Ret]] in
 let cfgCases=List.map (fun graph -> let idx,blocks=buildBlockIndex graph in
  let results=List.concat_map (fun intMode -> List.concat_map (fun floatMode -> List.map (fun scratch -> attempt (array ProductionLIR.basicBlock) (fun () -> PhiResolution.resolvePhiNodes idx blocks (intMapping domain intMode) (floatMapping domain floatMode scratch))) [0;1]) [0;1;2;3;4]) [0;1;2;3] in
  tuple [ProductionLIR.cfg graph;`List results]) (operandCFGs @ patterns) in
 let chains=List.map (fun count -> let domain=buildVRegDomain (List.init count Fun.id) in
  let phis=List.init (count-1) (fun n -> L.Phi (L.Virtual n,[vr (n+1),left],None)) in
  let graph=make (phis @ [L.PrintInt64 (L.Virtual 0)]) L.Ret in let idx,blocks=buildBlockIndex graph in
  list (fun mode -> attempt (array ProductionLIR.basicBlock) (fun () -> PhiResolution.resolvePhiNodes idx blocks (intMapping domain mode) (floatMapping domain 0 0))) [0;1;2;3]) [2;65;129] in
 tuple [`List moves;`List cfgCases;`List chains]
