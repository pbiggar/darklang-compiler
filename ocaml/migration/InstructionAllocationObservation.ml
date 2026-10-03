(* Full instruction rewrites for every register/spill class and both targets. *)
[@@@warning "-4"]
open Dark_compiler
open AllocationModel
module L = LIR
let tuple values = `Assoc ["tuple",`List values]
let list encode values = `List (List.map encode values)
let attempt encode action = try SemanticJson.union "FSharpResult" "Ok" [encode (action ())] with Failure message | Invalid_argument message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let observe source =
 let d=buildVRegDomain [0;1;2;3] in
 let regs=[|L.Virtual 0;L.Virtual 1;L.Virtual 2;L.Virtual 3|] in
 let fregs=[|L.FVirtual 7;L.FPhysical L.D0;L.FVirtual (-1);L.FVirtual (-1000)|] in
 let mapping mask : allocationResult =
  let allocations=Array.init 4 (fun n -> match (mask lsr (n*2)) land 3 with
   | 0 -> Some (PhysReg (List.nth [L.X19;L.X1;L.X2;L.X3] n))
   | 1 -> Some (PhysReg L.X12) | 2 -> Some (StackSlot (-(n+1)*8)) | _ -> None) in
  {domain=d;allocations;stackSize=32;usedCalleeSaved=[]} in
 let row arch mapping instr = tuple [ProductionLIR.instr instr;attempt (list ProductionLIR.instr) (fun () -> ApplyRegisterAllocation.applyToInstr arch mapping instr)] in
 let extras = [L.StringConcat (L.Virtual 0,L.Reg (L.Virtual 1),L.Reg (L.Virtual 2),[]);
  L.CanonicalBufferEq (L.Virtual 0,MemoryModel.NullableGraphemeCluster,L.Reg (L.Virtual 1),L.Reg (L.Virtual 2));
  L.CanonicalBufferEq (L.Virtual 0,MemoryModel.NullableGraphemeCluster,L.Reg (L.Virtual 1),L.Imm 0L);
  L.CanonicalBufferEq (L.Virtual 0,MemoryModel.NullableGraphemeCluster,L.Imm 0L,L.Reg (L.Virtual 2));
  L.CliNative (L.Virtual 0,L.Execute,[L.Reg (L.Virtual 0);L.Reg (L.Virtual 1);L.Reg (L.Virtual 2);L.Reg (L.Virtual 3)]);
  L.ArgMoves [L.X0,L.Reg (L.Virtual 1);L.X1,L.Reg (L.Virtual 0);L.X2,L.Reg (L.Virtual 2)];
  L.TailArgMoves [L.X0,L.Reg (L.Virtual 1);L.X1,L.Reg (L.Virtual 0);L.X2,L.Reg (L.Virtual 2)];
  L.Call (L.Virtual 0,AST.functionId (-1L),[L.Reg (L.Virtual 1);L.Reg (L.Virtual 2);L.Reg (L.Virtual 3)]);
  L.TailCall (AST.functionId (-1L),[L.Reg (L.Virtual 1);L.Reg (L.Virtual 2);L.Reg (L.Virtual 3)])] in
 let allClasses=List.init 256 (fun mask -> let mapping=mapping mask in list (fun arch -> list (row arch mapping) (AllocationFixtures.instructions source regs fregs (L.Reg (L.Virtual 3)) AST.TInt64 @ extras)) [Platform.ARM64;Platform.X86_64]) in
 let operands=[L.Reg (L.Virtual 3);L.Imm Int64.min_int;L.FloatImm (Int64.float_of_bits 0x7ff8000000000001L);L.FuncAddr (AST.functionId (-1L));L.StackSlot (-24);L.StringSymbol source] in
 let registerRoles=[regs;[|L.Physical L.X0;L.Physical L.X1;L.Physical L.X2;L.Physical L.X3|];[|L.Physical L.X12;L.Physical L.X13;L.Physical L.X14;L.Physical L.X15|];[|L.Virtual 987;L.Virtual 1;L.Virtual 2;L.Virtual 3|];[|L.Virtual (-2147483648);L.Virtual 2147483647;L.Virtual (-9);L.Virtual 0|]] in
 let varied=List.concat_map (fun mask -> let mapping=mapping mask in List.concat_map (fun arch -> List.concat_map (fun roles -> List.concat_map (fun operand -> List.map (fun typ -> list (row arch mapping) (AllocationFixtures.instructions source roles fregs operand typ)) [AST.TInt64;AST.TFloat64]) operands) registerRoles) [Platform.ARM64;Platform.X86_64]) [0;85;170;255;27;228;42;99] in
 tuple [`List allClasses;`List varied]
