(* Full block allocation, caller-save preparation and terminator observations. *)
[@@@warning "-4"]
open Dark_compiler
open AllocationModel
module L = LIR
module F = FloatAllocation
module B = ApplyBlockAllocation
let tuple values = `Assoc ["tuple",`List values]
let list encode values = `List (List.map encode values)
let array encode values = `List (Array.to_list (Array.map encode values))
let bits = array (fun value -> `Assoc ["kind",`String "uint64";"value",`String (Printf.sprintf "%Lu" value)])
let preparation (value:B.blockAllocationPreparation) = SemanticJson.record "BlockAllocationPreparation" ["SaveRegsLiveness",list (fun (a,b) -> tuple [bits a;bits b]) value.B.saveRegsLiveness]
let attempt encode action = try SemanticJson.union "FSharpResult" "Ok" [encode (action ())] with Failure message | Invalid_argument message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let block label instrs terminator : L.basicBlock = {L.label;instrs;terminator}
let observe source =
 let label=L.Label source in let other=L.Label "other" in let d=buildVRegDomain (List.init 16 Fun.id) in
 let integer mode : allocationResult = {domain=d;allocations=Array.init 16 (fun n -> match mode with 0 -> Some (PhysReg (List.nth [L.X1;L.X2;L.X3;L.X4;L.X5;L.X6;L.X7] (n mod 7))) | 1 -> Some (StackSlot (-(n+1)*8)) | 2 -> (match n mod 3 with 0 -> None | 1 -> Some (PhysReg L.X19) | _ -> Some (StackSlot (-24))) | _ -> Some (PhysReg (List.nth [L.X0;L.X1;L.X2;L.X3;L.X4;L.X5;L.X6;L.X7;L.X19;L.X20;L.X21;L.X22;L.X23;L.X24;L.X25;L.X26] n)));stackSize=128;usedCalleeSaved=[]} in
 let floating mode : F.fAllocationResult = {F.domain=d;allocations=Array.init 16 (fun n -> match mode with 0 -> Some (F.FPhysReg (List.nth F.allocatableFloatRegs n)) | 1 -> Some (F.FStackSlot (-(n+1)*8)) | 2 -> (match n mod 3 with 0 -> Some (F.FPhysReg L.D15) | 1 -> Some (F.FRematerialized (-0.)) | _ -> None) | _ -> None);stackSize=256;usedCalleeSavedF=[];spillScratchLeft=L.FVirtual (-1000);spillScratchRight=L.FVirtual (-1001);spillScratchThird=L.FVirtual (-1002)} in
 let live mask = let bits=Bitset.empty 1 in for n=0 to 15 do if mask land (1 lsl n)<>0 then Bitset.addIndexInPlace n bits done;bits in
 let fixtures=AllocationFixtures.instructions source [|L.Virtual 0;L.Virtual 1;L.Virtual 2;L.Virtual 3|] [|L.FVirtual 0;L.FVirtual 1;L.FVirtual 2;L.FVirtual 3|] (L.Reg (L.Virtual 3)) AST.TFloat64 in
 let instructions=List.concat_map (fun instr -> [[instr];[L.SaveRegs ([],[]);instr;L.RestoreRegs ([],[]);L.Add (L.Virtual 5,L.Virtual 0,L.Reg (L.Virtual 1));L.FAdd (L.FVirtual 5,L.FVirtual 0,L.FVirtual 1)]]) fixtures @
  [[];[L.SaveRegs ([],[]);L.SaveRegs ([],[]);L.Call (L.Virtual 0,AST.functionId 3L,[]);L.RestoreRegs ([],[]);L.RestoreRegs ([],[]);L.PrintInt64 (L.Virtual 1);L.PrintFloat (L.FVirtual 1)];[L.SaveRegs ([],[])];[L.RestoreRegs ([],[])];[L.SaveRegs ([L.X3],[L.D3]);L.RestoreRegs ([],[])];[L.SaveRegs ([L.X3],[L.D3]);L.RestoreRegs ([L.X3],[L.D3])]] in
 let blockCases=List.concat_map (fun im -> let mapping=integer im in List.concat_map (fun fm -> let floatAllocation=floating fm in List.concat_map (fun arch -> List.map (fun mask -> let liveOut=live mask in let floatLiveOut=live (mask lxor 65535) in
  list (fun instrs -> attempt ProductionLIR.basicBlock (fun () -> B.applyToBlockWithLiveness arch mapping floatAllocation liveOut floatLiveOut (block label instrs (L.BranchZero (L.Virtual 3,label,other))))) instructions) [0;85;65535]) [Platform.ARM64;Platform.X86_64]) [0;1;2;3]) [0;1;2;3] in
 let regs=[L.Virtual 0;L.Virtual 3;L.Virtual 987;L.Physical L.X0;L.Physical L.X12;L.Physical L.SP] in
 let terms=List.concat_map (fun reg -> [L.Ret;L.Jump other;L.Branch (reg,label,other);L.BranchZero (reg,label,other);L.BranchBitZero (reg,63,label,other);L.BranchBitNonZero (reg,63,label,other);L.CondBranch (L.NE,label,other)]) regs in
 let terminators=List.map (fun mode -> let mapping=integer mode in list (fun term -> let loads,allocated=B.applyToTerminator mapping term in tuple [ProductionLIR.terminator term;list ProductionLIR.instr loads;ProductionLIR.terminator allocated]) terms) [0;1;2;3] in
 let cfgCases=List.concat_map (fun im -> let mapping=integer im in List.concat_map (fun fm -> let floatAllocation=floating fm in List.map (fun floatCount ->
  let blocks=[|block label [L.SaveRegs ([],[]);L.Call (L.Virtual 0,AST.functionId 3L,[]);L.RestoreRegs ([],[]);L.PrintInt64 (L.Virtual 1);L.PrintFloat (L.FVirtual 1)] (L.Jump other);block other [L.FLoad (L.FVirtual 2,-0.);L.SaveRegs ([],[]);L.RestoreRegs ([],[])] L.Ret|] in
  let facts=RegisterFacts.classifyBlocks blocks in let liveness=[|{liveIn=live 85;liveOut=live 65535};{liveIn=live 170;liveOut=live 85}|] in
  let floatLiveness=Array.init floatCount (fun n -> {liveIn=live 170;liveOut=live (if n=0 then 43690 else 65535)}) in
  attempt (fun prep -> tuple [array preparation prep;list (fun arch -> tuple [attempt (array ProductionLIR.basicBlock) (fun () -> B.applyPreparedCFGAllocation arch blocks mapping floatAllocation prep);attempt (array ProductionLIR.basicBlock) (fun () -> B.applyToCFGWithLiveness arch blocks mapping floatAllocation liveness floatLiveness)]) [Platform.ARM64;Platform.X86_64]]) (fun () -> B.prepareCFGAllocation blocks mapping floatAllocation liveness floatLiveness facts)) [0;1;2]) [0;1;2;3]) [0;1;2;3] in
 let preparedCases=List.concat_map (fun instrs -> List.concat_map (fun count -> List.map (fun arch ->
  let prep=[|{B.saveRegsLiveness=List.init count (fun _ -> live 65535,live 65535)}|] in
  attempt (array ProductionLIR.basicBlock) (fun () -> B.applyPreparedCFGAllocation arch [|block label instrs L.Ret|] (integer 0) (floating 0) prep)) [Platform.ARM64;Platform.X86_64]) [0;1;2;3]) [[];[L.SaveRegs ([],[])];[L.RestoreRegs ([],[])];[L.SaveRegs ([],[]);L.RestoreRegs ([],[])];[L.SaveRegs ([],[]);L.SaveRegs ([],[]);L.RestoreRegs ([],[]);L.RestoreRegs ([],[])]] in
 tuple [`List blockCases;`List terminators;`List cfgCases;`List preparedCases]
