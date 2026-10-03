(* Complete allocator orchestration and call-aware color permutation observations. *)
[@@@warning "-4"]
open Dark_compiler
open AllocationModel
module L=LIR
module F=FloatAllocation
module A=ARM64CalleeClobbers
module R=RegisterAllocation
module I=InstrumentedRegisterAllocation
let tuple values=`Assoc ["tuple",`List values]
let list fn values=`List (List.map fn values)
let array fn values=`List (Array.to_list (Array.map fn values))
let option fn = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [fn value]
let i=SemanticJson.int32
let domain (d:vRegDomain)=SemanticJson.record "VRegDomain" ["Ids",array i d.ids;"IndexOf",array i d.indexOf;"IndexOffset",i d.indexOffset;"WordCount",i d.wordCount]
let allocation = function PhysReg reg -> SemanticJson.union "Allocation" "PhysReg" [ProductionLIR.physReg reg] | StackSlot offset -> SemanticJson.union "Allocation" "StackSlot" [i offset]
let allocated (value:allocationResult)=SemanticJson.record "AllocationResult" ["Domain",domain value.domain;"Allocations",array (option allocation) value.allocations;"StackSize",i value.stackSize;"UsedCalleeSaved",list ProductionLIR.physReg value.usedCalleeSaved]
let fAllocation = function F.FPhysReg reg -> SemanticJson.union "FAllocation" "FPhysReg" [ProductionLIR.physFPReg reg] | F.FStackSlot offset -> SemanticJson.union "FAllocation" "FStackSlot" [i offset] | F.FRematerialized value -> SemanticJson.union "FAllocation" "FRematerialized" [`Assoc ["kind",`String "float64";"value",`String (Printf.sprintf "%016Lx" (Int64.bits_of_float value))]]
let fAllocated (value:F.fAllocationResult)=SemanticJson.record "FAllocationResult" ["Domain",domain value.F.domain;"Allocations",array (option fAllocation) value.F.allocations;"StackSize",i value.F.stackSize;"UsedCalleeSavedF",list ProductionLIR.physFPReg value.F.usedCalleeSavedF;"SpillScratchLeft",ProductionLIR.fReg value.F.spillScratchLeft;"SpillScratchRight",ProductionLIR.fReg value.F.spillScratchRight;"SpillScratchThird",ProductionLIR.fReg value.F.spillScratchThird]
let attempt fn action=try SemanticJson.union "FSharpResult" "Ok" [fn (action ())] with Failure message | Invalid_argument message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let block label instrs term : L.basicBlock={L.label;instrs;terminator=term}
let cfg entry blocks : L.cfg={L.entry;blocks=L.LabelMap.of_list (List.map (fun (b:L.basicBlock) -> b.L.label,b) blocks)}
let observe source=
 let label=L.Label source in let other=L.Label "other" in let back=L.Label "back" in let exit=L.Label "exit" in
 let known mode=match mode with 0 -> FunctionIdMap.empty | 1 -> FunctionIdMap.ofList [AST.functionId 3L,{A.ints=A.ofInts [L.X2];floats=A.ofFloats [L.D2]};AST.functionId 7L,{A.ints=A.ofInts [L.X1;L.X3];floats=A.ofFloats [L.D0]}] | _ -> FunctionIdMap.ofList [AST.functionId 3L,A.all;AST.functionId 7L,{A.ints=(-1L);floats=(-1L)}] in
 let param n typ : L.typedLIRParam={L.reg=L.Virtual n;typ} in
 let params=[[];[param 0 AST.TInt64;param 1 AST.TFloat64;param 2 AST.TInt64;param 3 AST.TFloat64];List.init 8 (fun n -> param n AST.TInt64);List.init 8 (fun n -> param n AST.TFloat64)] in
 let make params cfg : L.functionDef={L.id=AST.functionId 0L;name=source;typedParams=params;cfg;stackSize=32;usedCalleeSaved=[L.X19];codegenFacts=None} in
 let observeFunction func=list (fun arch -> list (fun style ->
  let allocated=attempt ProductionLIR.functionDef (fun () -> if style<0 then R.allocateRegisters arch func else R.allocateRegistersWithCallSummaries arch (known style) func) in
  let timed=if style<0 then attempt (fun (value,timings) -> tuple [ProductionLIR.functionDef value;list (fun (t:registerAllocationTiming) -> tuple [SemanticJson.string t.phase;`Bool (Float.is_finite t.elapsedMs && t.elapsedMs>=0.)]) timings]) (fun () -> R.allocateRegistersWithTiming arch func) else `Null in tuple [allocated;timed]) [-1;0;1;2]) [Platform.ARM64;Platform.X86_64] in
 let instructions=AllocationFixtures.instructions source [|L.Virtual 0;L.Virtual 1;L.Virtual 2;L.Virtual 3|] [|L.FVirtual 0;L.FVirtual 1;L.FVirtual 2;L.FVirtual 3|] (L.Reg (L.Virtual 3)) AST.TFloat64 in
 let constructorCases=list (fun parameters -> list (fun instr -> observeFunction (make parameters (cfg label [block label [instr] L.Ret]))) instructions) params in
 let bodies=[cfg label [block label [] L.Ret];cfg label [block label [L.SaveRegs ([],[]);L.ArgMoves [L.X0,L.Reg (L.Virtual 0)];L.Call (L.Virtual 4,AST.functionId 3L,[]);L.FMov (L.FVirtual (-1),L.FPhysical L.D0);L.RestoreRegs ([],[]);L.Add (L.Virtual 5,L.Virtual 0,L.Reg (L.Virtual 4));L.FAdd (L.FVirtual 5,L.FVirtual 1,L.FVirtual 3)] L.Ret];
  cfg label [block label [L.SaveRegs ([],[]);L.SaveRegs ([],[]);L.Call (L.Virtual 4,AST.functionId 3L,[]);L.RestoreRegs ([],[]);L.Call (L.Virtual 5,AST.functionId 7L,[]);L.RestoreRegs ([],[]);L.PrintInt64 (L.Virtual 0);L.PrintFloat (L.FVirtual 1)] L.Ret];
  cfg label [block label [L.Mov (L.Virtual 4,L.Imm 0L);L.FLoad (L.FVirtual 4,-0.)] (L.Jump other);block other [L.Phi (L.Virtual 0,[L.Reg (L.Virtual 4),label;L.Reg (L.Virtual 5),back],Some AST.TInt64);L.FPhi (L.FVirtual 0,[L.FVirtual 4,label;L.FVirtual 5,back]);L.Cmp (L.Virtual 0,L.Imm 10L)] (L.CondBranch (L.LT,back,exit));block back [L.Add (L.Virtual 5,L.Virtual 0,L.Imm 1L);L.FAdd (L.FVirtual 5,L.FVirtual 0,L.FVirtual 1)] (L.Jump other);block exit [L.PrintInt64 (L.Virtual 0);L.PrintFloat (L.FVirtual 0)] L.Ret];
  cfg label [block label [L.FPhi (L.FVirtual 4,[L.FVirtual 1,L.Label "entry-edge";L.FVirtual 5,other]);L.FAdd (L.FVirtual 5,L.FVirtual 4,L.FVirtual 3)] (L.Jump other);block other [] (L.Branch (L.Virtual 0,label,exit));block exit [L.PrintFloat (L.FVirtual 4)] L.Ret]] in
 let bodyCases=list (fun parameters -> list (fun body -> list (fun facts -> let func=make parameters body in observeFunction (if facts then L.attachFunctionCodegenFacts func else func)) [false;true]) bodies) params in
 let pressureCases=list (fun count ->
  let ints=List.init count (fun n -> L.Mov (L.Virtual n,L.Imm (Int64.of_int n))) @ List.init count (fun n -> L.PrintInt64 (L.Virtual n)) in
  let floats=List.init count (fun n -> L.FLoad (L.FVirtual n,Int64.float_of_bits (if n mod 2=0 then Int64.min_int else Int64.of_int n))) @ List.init count (fun n -> L.PrintFloat (L.FVirtual n)) in
  list (fun is -> observeFunction (make [] (cfg label [block label is L.Ret]))) [ints;floats;ints @ floats]) [0;1;7;8;10;15;16;17;65] in
 let invalidCases=list observeFunction [make (List.init 9 (fun n -> param n AST.TInt64)) (cfg label [block label [] L.Ret]);make (List.init 9 (fun n -> param n AST.TFloat64)) (cfg label [block label [] L.Ret])] in
 let d=buildVRegDomain (List.init 16 Fun.id) in
 let intAllocation arch mode : allocationResult=
  let regs=RegisterPolicy.callerSavedRegs @ RegisterPolicy.calleeSavedRegsFor arch in
  {domain=d;allocations=Array.init 16 (fun n -> match mode with 0 -> Some (PhysReg (List.nth RegisterPolicy.callerSavedRegs (n mod 7))) | 1 -> Some (PhysReg (List.nth regs (n mod List.length regs))) | 2 -> (match n mod 3 with 0 -> Some (PhysReg L.X19) | 1 -> Some (StackSlot (-24)) | _ -> None) | 3 -> Some (StackSlot (-(n+1)*8)) | _ -> None);stackSize=128;usedCalleeSaved=[L.X19]} in
 let floatAllocation mode : F.fAllocationResult={F.domain=d;allocations=Array.init 16 (fun n -> match mode with 0 -> Some (F.FPhysReg (List.nth F.allocatableFloatRegs n)) | 1 -> (match n mod 3 with 0 -> Some (F.FPhysReg L.D15) | 1 -> Some (F.FRematerialized (-0.)) | _ -> None) | 2 -> Some (F.FStackSlot (-(n+1)*8)) | _ -> None);stackSize=256;usedCalleeSavedF=[L.D8];spillScratchLeft=L.FVirtual (-1000);spillScratchRight=L.FVirtual (-1001);spillScratchThird=L.FVirtual (-1002)} in
 let envelopes=[[];[L.Call (L.Virtual 0,AST.functionId 3L,[])];[L.ArgMoves [L.X0,L.Reg (L.Virtual 2)];L.Call (L.Virtual 0,AST.functionId 3L,[])];[L.Call (L.Virtual 0,AST.functionId 7L,[]);L.FMov (L.FVirtual (-1),L.FPhysical L.D0)];[L.SaveRegs ([],[]);L.Call (L.Virtual 0,AST.functionId 3L,[]);L.RestoreRegs ([],[])];[L.PrintInt64 (L.Virtual 1);L.Call (L.Virtual 0,AST.functionId 3L,[])]] in
 let callPermutationCases=list (fun arch -> list (fun im -> list (fun fm -> list (fun mask -> list (fun envelope ->
  let blocks=[|block label (L.SaveRegs ([],[])::envelope @ [L.RestoreRegs ([],[]);L.PrintInt64 (L.Virtual 1);L.PrintFloat (L.FVirtual 2)]) L.Ret|] in
  let facts=RegisterFacts.classifyBlocks blocks in let liveness=[|{liveIn=[|Int64.of_int mask|];liveOut=[|Int64.of_int mask|]}|] in
  let floatLiveness=[|{liveIn=[|Int64.of_int (mask lxor 65535)|];liveOut=[|Int64.of_int (mask lxor 65535)|]}|] in
  list (fun mode -> tuple [attempt allocated (fun () -> I.chooseRegistersForCalls arch (if mode<0 then None else Some (known mode)) blocks facts d d liveness floatLiveness (intAllocation arch im));if mode<0 then `Null else attempt fAllocated (fun () -> I.chooseArm64FloatRegistersForCalls (known mode) blocks facts d d liveness floatLiveness (floatAllocation fm))]) [-1;0;1;2]) envelopes) [0;85;65535]) [0;1;2;3]) [0;1;2;3;4]) [Platform.ARM64;Platform.X86_64] in
 tuple [constructorCases;bodyCases;pressureCases;invalidCases;callPermutationCases;list (fun costs -> attempt i (fun () -> I.bestCostGap costs)) [[];[1];[3;1];[2;2;4];[Int32.to_int Int32.max_int;Int32.to_int Int32.min_int;0]]]
