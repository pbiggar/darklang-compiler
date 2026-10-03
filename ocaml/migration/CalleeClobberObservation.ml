(* Complete callee write summaries, save-envelope and pruning observations. *)
[@@@warning "-4"]
open Dark_compiler
module A=ARM64CalleeClobbers
module X=X64CalleeClobbers
module IA=InstrumentedARM64CalleeClobbers
module IX=InstrumentedX64CalleeClobbers
module L=LIR
let tuple values=`Assoc ["tuple",`List values]
let list fn values=`List (List.map fn values)
let u64 value=`Assoc ["kind",`String "uint64";"value",`String (Printf.sprintf "%Lu" value)]
let writes (value:A.writes)=SemanticJson.record "Writes" ["Ints",u64 value.A.ints;"Floats",u64 value.A.floats]
let instrumentedWrites (value:IA.writes)=SemanticJson.record "Writes" ["Ints",u64 value.IA.ints;"Floats",u64 value.IA.floats]
let map values=SemanticJson.union "FunctionIdMap" "FunctionIdMap" [`Assoc ["map",list (fun (id,value) -> tuple [u64 (AST.functionIdValue id);writes value]) (FunctionIdMap.toList values)]]
let observe source=
 let label=L.Label source in let fid n=AST.functionId (Int64.of_int n) in
 let block instrs : L.basicBlock={L.label;instrs;terminator=L.Ret} in
 let fn id instrs : L.functionDef={L.id=fid id;name=source;typedParams=[];cfg={L.entry=label;blocks=L.LabelMap.singleton label (block instrs)};stackSize=32;usedCalleeSaved=[L.X19];codegenFacts=None} in
 let known mode=match mode with 0 -> FunctionIdMap.empty | 1 -> FunctionIdMap.ofList [fid 3,{A.ints=A.ofInts [L.X2];floats=A.ofFloats [L.D2]};fid 7,{A.ints=A.ofInts [L.X0;L.X7];floats=A.ofFloats [L.D0;L.D15]}] | _ -> FunctionIdMap.ofList [fid 3,{A.ints=(-1L);floats=(-1L)};fid 7,A.all] in
 let gp=List.map (fun reg -> L.Physical reg) [L.X0;L.X1;L.X2;L.X3;L.X4;L.X5;L.X6;L.X7;L.X8;L.X9;L.X10;L.X11;L.X12;L.X13;L.X14;L.X15;L.X16;L.X17;L.X19;L.X20;L.X21;L.X22;L.X23;L.X24;L.X25;L.X26;L.X27;L.X29;L.X30;L.SP] @ [L.Virtual 3;L.Virtual (-1)] in
 let fp=List.map (fun reg -> L.FPhysical reg) FloatAllocation.allocatableFloatRegs @ [L.FVirtual (-1);L.FVirtual 3] in
 let operands=[L.Reg (L.Physical L.X2);L.Imm (-1L);L.Imm 0L;L.Imm 4095L;L.Imm 4096L;L.StackSlot (-257);L.StackSlot (-256);L.StackSlot 255;L.StackSlot 256;L.StringSymbol source;L.FloatImm (-0.);L.FuncAddr (fid 3)] in
 let instructions=(List.mapi (fun index reg -> List.concat_map (fun operand -> AllocationFixtures.instructions source (Array.make 4 reg) (Array.make 4 (List.nth fp (index mod List.length fp))) operand AST.TFloat64) operands) gp |> List.concat) @
  List.concat_map (fun offset -> [L.Store (offset,L.Physical L.X2);L.Mov (L.Physical L.X2,L.StackSlot offset)]) [-257;-256;255;256;Int32.to_int Int32.min_int;Int32.to_int Int32.max_int] @
  List.concat_map (fun freg -> [L.FLoad (freg,-0.);L.FMov (freg,L.FPhysical L.D0);L.FSpillLoad (freg,-24)]) fp @
  List.concat_map (fun id -> [L.Call (L.Physical L.X2,fid id,[]);L.TailCall (fid id,[])]) [3;7;9] in
 let instructionCases=list (fun mode ->
  let known=known mode in
  let lookup id=FunctionIdMap.tryFind id known in
  let lookupA id=Option.map (fun (value:A.writes) -> {IA.ints=value.A.ints;floats=value.A.floats}) (lookup id) in
  list (fun instr -> tuple [instrumentedWrites (IA.instructionWrites lookupA instr);writes (IX.instructionWrites lookup instr)]) instructions) [0;1;2] in
 let catalogs=[[];[fn 0 []];[fn 3 [L.Mov (L.Physical L.X5,L.Imm 0L)];fn 0 [L.Call (L.Physical L.X2,fid 3,[])]];
  [fn 0 [L.Call (L.Physical L.X2,fid 1,[])];fn 1 [L.Call (L.Physical L.X3,fid 0,[]);L.FMov (L.FPhysical L.D5,L.FPhysical L.D0)]];
  [fn 0 [L.Call (L.Physical L.X2,fid 3,[])];fn 3 [];fn 3 [L.Mov (L.Physical L.X7,L.Imm 0L)]];
  [fn 0 [L.Call (L.Physical L.X2,fid 987,[])]];
  List.init 65 (fun id -> fn id (if id=64 then [L.Mov (L.Physical L.X27,L.Imm 0L)] else [L.Call (L.Physical L.X2,fid (id+1),[])]))] in
 let summaryCases=list (fun mode -> list (fun funcs -> tuple [map (A.summariesWithKnown (known mode) funcs);map (X.summariesWithKnown (known mode) funcs);map (A.summaries funcs)]) catalogs) [0;1;2] in
 let envelopes=[[];[L.Call (L.Physical L.X2,fid 3,[])];[L.Call (L.Physical L.X2,fid 7,[])];[L.Call (L.Physical L.X2,fid 9,[])];[L.Call (L.Physical L.X2,fid 3,[L.Imm 0L])];[L.ArgMoves [L.X0,L.Imm 0L];L.Call (L.Physical L.X2,fid 3,[])];[L.FArgMoves [L.D0,L.FPhysical L.D2];L.Call (L.Physical L.X2,fid 3,[])];[L.Call (L.Physical L.X2,fid 3,[]);L.FMov (L.FVirtual (-1),L.FPhysical L.D0)];[L.Call (L.Physical L.X2,fid 3,[]);L.Call (L.Physical L.X2,fid 7,[])];[L.ArgMoves [L.X0,L.StringSymbol source];L.Call (L.Physical L.X2,fid 3,[])];[L.SaveRegs ([],[]);L.Call (L.Physical L.X2,fid 3,[]);L.RestoreRegs ([],[])];[L.Mov (L.Physical L.X3,L.Imm 1L);L.Call (L.Physical L.X2,fid 3,[])]] in
 let selected mask regs=List.filteri (fun index _ -> mask land (1 lsl index)<>0) regs in
 let saveCases=list (fun mode -> list (fun mask ->
  let ints=selected (mask land 15) [L.X0;L.X1;L.X2;L.X3] in let floats=selected (mask lsr 4) [L.D0;L.D1;L.D2;L.D3] in
  list (fun envelope -> list (fun ending ->
   let instrs=L.SaveRegs (ints,floats)::envelope @ ending in let func=fn 0 instrs in
   let known=known mode in
   tuple [list writes (A.callWritesForSaves known (block instrs));list writes (X.callWritesForSaves known (block instrs));list ProductionLIR.functionDef (A.refineWithCache None (Some known) [func]);ProductionLIR.functionDef (X.pruneFunction known func)])
   [ [L.RestoreRegs (ints,floats)];[];[L.RestoreRegs ([],[])] ]) envelopes) (List.init 256 Fun.id)) [0;1;2] in
 let cacheCases=list (fun knownMode ->
  let calls=ref [] in
  let cache func relevant generate=calls := (func.L.id,map relevant)::!calls;generate () in
  let functions=[fn 0 [L.SaveRegs ([L.X2;L.X3],[L.D2]);L.Call (L.Physical L.X2,fid 3,[]);L.RestoreRegs ([L.X2;L.X3],[L.D2])];fn 3 [L.Mov (L.Physical L.X5,L.Imm 0L)];fn 7 []] in
  let outputs=A.refineWithCache (Some cache) (if knownMode<0 then None else Some (known knownMode)) functions in
  tuple [list ProductionLIR.functionDef outputs;list (fun (id,relevant) -> tuple [(SemanticJson.union "FunctionId" "FunctionId" [u64 (AST.functionIdValue id)]);relevant]) (List.rev !calls);list ProductionLIR.functionDef (A.refine functions)]) [-1;0;1;2] in
 tuple [instructionCases;summaryCases;saveCases;cacheCases]
