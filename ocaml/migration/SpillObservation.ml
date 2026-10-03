(* Complete integer spill helpers and both targets' caller-save observations. *)
[@@@warning "-4"]
open Dark_compiler
open AllocationModel
module S = SpillOperands
module L = LIR
module F = FloatAllocation
let tuple values = `Assoc ["tuple",`List values]
let list encode values = `List (List.map encode values)
let i = SemanticJson.int32
let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value]
let allocation = function PhysReg reg -> SemanticJson.union "Allocation" "PhysReg" [ProductionLIR.physReg reg] | StackSlot offset -> SemanticJson.union "Allocation" "StackSlot" [i offset]
let loaded (reg,instrs) = tuple [ProductionLIR.reg reg;list ProductionLIR.instr instrs]
let attempt encode action = try SemanticJson.union "FSharpResult" "Ok" [encode (action ())] with Failure message | Invalid_argument message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let observe source =
 let phys=[L.X0;L.X1;L.X2;L.X3;L.X4;L.X5;L.X6;L.X7;L.X8;L.X9;L.X10;L.X11;L.X12;L.X13;L.X14;L.X15;L.X16;L.X17;L.X19;L.X20;L.X21;L.X22;L.X23;L.X24;L.X25;L.X26;L.X27;L.X29;L.X30;L.SP] in
 let ids=[-9;0;1;2;3;7;29] in let d=buildVRegDomain ids in
 let regs=List.map (fun reg -> L.Physical reg) phys @ List.map (fun id -> L.Virtual id) (ids @ [987;-2147483648;2147483647]) in
 let pairRegs=List.map (fun id -> L.Virtual id) (ids @ [987]) @ [L.Physical L.X0;L.Physical L.X8;L.Physical L.X12;L.Physical L.X19] in
 let mappings=List.map (fun mode ->
  let allocations=Array.mapi (fun n _ -> match mode with
   | 0 -> Some (PhysReg (List.nth [L.X1;L.X2;L.X3;L.X4;L.X5;L.X6;L.X7] n))
   | 1 -> Some (StackSlot (-(n+1)*8))
   | 2 -> Some (if n mod 2=0 then PhysReg L.X12 else StackSlot (-24))
   | 3 -> (match n mod 3 with 0 -> None | 1 -> Some (PhysReg (List.nth phys (n*4))) | _ -> Some (StackSlot (-8)))
   | _ -> None) d.ids in
  let mapping:allocationResult={domain=d;allocations;stackSize=64;usedCalleeSaved=[]} in
  let scalar=List.map (fun reg ->
   let operand=L.Reg reg in
   tuple [ProductionLIR.reg reg;(match reg with L.Virtual id -> option allocation (S.tryAllocation mapping id) | L.Physical _ -> `Null);
    (let reg,info=S.applyToReg mapping reg in tuple [ProductionLIR.reg reg;option allocation info]);ProductionLIR.operand (S.applyToOperandNoLoad mapping operand);
    list (fun temp -> let op,loads=S.applyToOperand mapping operand temp in tuple [ProductionLIR.operand op;list ProductionLIR.instr loads;loaded (S.loadSpilled mapping reg temp)]) [L.X0;L.X12;L.X19]]) regs in
  let operands=List.map (fun op -> let allocated,loads=S.applyToOperand mapping op L.X12 in tuple [ProductionLIR.operand op;ProductionLIR.operand allocated;list ProductionLIR.instr loads;ProductionLIR.operand (S.applyToOperandNoLoad mapping op)]) [L.Imm Int64.min_int;L.FloatImm (-0.);L.StringSymbol source;L.FloatSymbol (Int64.float_of_bits 0x7ff8000000000001L);L.StackSlot (-24);L.FuncAddr (AST.functionId (-1L))] in
  let live=List.init 128 (fun mask -> let bits=Bitset.empty d.wordCount in for n=0 to 6 do if mask land (1 lsl n)<>0 then Bitset.addIndexInPlace n bits done;list ProductionLIR.physReg (S.getLiveCallerSavedRegs mapping bits)) in
  let pairs=List.concat_map (fun arch -> List.concat_map (fun left -> List.concat_map (fun right -> List.map (fun dest -> attempt (fun (l,r) -> tuple [loaded l;loaded r]) (fun () -> S.loadSpilledPair arch mapping left right dest)) (List.map (fun reg -> L.Physical reg) phys @ [L.Virtual 3;L.Virtual 987])) pairRegs) pairRegs) [Platform.ARM64;Platform.X86_64] in
  tuple [`List scalar;`List operands;`List live;`List pairs]) [0;1;2;3;4] in
 let candidates=[L.X3;L.X4;L.X5;L.X6;L.X7;L.X19;L.X20;L.X21;L.X0;L.X1;L.X2] in
 let exclusion=List.init 2048 (fun mask -> let excluded=List.filteri (fun n _ -> mask land (1 lsl n)<>0) candidates |> List.map (fun reg -> L.Physical reg) in attempt ProductionLIR.physReg (fun () -> S.x86SpillTempExcluding (L.Virtual 3 :: excluded @ excluded))) in
 let fd=buildVRegDomain (List.init 16 Fun.id) in
 let floatSaved=List.concat_map (fun mode ->
  let allocations=Array.init 16 (fun n -> match mode with 0 -> Some (F.FPhysReg (List.nth F.allocatableFloatRegs n)) | 1 -> Some (F.FStackSlot (-8)) | 2 -> Some (F.FRematerialized (-0.)) | 3 -> if n mod 2=0 then Some (F.FPhysReg (List.nth F.allocatableFloatRegs (15-n))) else None | _ -> None) in
  let allocation:F.fAllocationResult={F.domain=fd;allocations;stackSize=0;usedCalleeSavedF=[];spillScratchLeft=L.FVirtual (-1000);spillScratchRight=L.FVirtual (-1001);spillScratchThird=L.FVirtual (-1002)} in
  List.map (fun mask -> let bits=Bitset.empty 1 in for n=0 to 15 do if mask land (1 lsl n)<>0 then Bitset.addIndexInPlace n bits done;
   list (fun arch -> list ProductionLIR.physFPReg (S.getLiveCallerSavedFloatRegs arch bits allocation)) [Platform.ARM64;Platform.X86_64]) (0::65535::21845::43690::List.init 16 (fun n -> 1 lsl n))) [0;1;2;3;4] in
 tuple [`List mappings;list (fun reg -> `Bool (S.aliasesX86ScratchReg reg)) phys;`List exclusion;`List floatSaved;list (fun arch -> `Bool (S.isX86_64 arch)) [Platform.ARM64;Platform.X86_64]]
