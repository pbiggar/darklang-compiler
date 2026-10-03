(* Full allocator domains, register facts, liveness and interference observations. *)
[@@@warning "-4"]
open Dark_compiler
open AllocationModel
module L = LIR
let tuple values = `Assoc ["tuple",`List values]
let list encode values = `List (List.map encode values)
let array encode values = `List (Array.to_list (Array.map encode values))
let i = SemanticJson.int32
let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value]
let bits = array (fun value -> `Assoc ["kind",`String "uint64";"value",`String (Printf.sprintf "%Lu" value)])
let domain (value:vRegDomain) = SemanticJson.record "VRegDomain" ["Ids",array i value.ids;"IndexOf",array i value.indexOf;"IndexOffset",i value.indexOffset;"WordCount",i value.wordCount]
let blockIndex (value:blockIndex) = SemanticJson.record "BlockIndex" ["Labels",array ProductionLIR.label value.labels;"EntryIndex",i value.entryIndex]
let live (value:blockLiveness) = SemanticJson.record "BlockLiveness" ["LiveIn",bits value.liveIn;"LiveOut",bits value.liveOut]
let pairBits (a,b) = tuple [bits a;bits b]
let graph (value:interferenceGraph) = SemanticJson.record "InterferenceGraph" ["Domain",domain value.domain;"Vertices",bits value.vertices;"Neighbors",array bits value.neighbors]
let coloring (value:coloringResult) = SemanticJson.record "ColoringResult" ["Domain",domain value.domain;"Colors",array (option i) value.colors;"Spills",bits value.spills;"ChromaticNumber",i value.chromaticNumber]
let facts (value:instrRegisterFacts) = SemanticJson.record "InstrRegisterFacts" ["Instr",ProductionLIR.instr value.instr;"IntUses",list i value.intUses;"IntDef",option i value.intDef;"IntPhiUses",list (fun (id,label) -> tuple [i id;ProductionLIR.label label]) value.intPhiUses;"FloatUses",list i value.floatUses;"FloatDef",option i value.floatDef;"FloatPhiUses",list (fun (id,label) -> tuple [i id;ProductionLIR.label label]) value.floatPhiUses]
let classified (value:classifiedBlock) = SemanticJson.record "ClassifiedBlock" ["Block",ProductionLIR.basicBlock value.block;"InstrFacts",array facts value.instrFacts;"TerminatorUses",list i value.terminatorUses;"HasPhiNodes",`Bool value.hasPhiNodes]
let accumulator = function NoUnionBits -> SemanticJson.union "BitSetUnionAccumulator" "NoUnionBits" [] | BorrowedUnionBits value -> SemanticJson.union "BitSetUnionAccumulator" "BorrowedUnionBits" [bits value] | OwnedUnionBits value -> SemanticJson.union "BitSetUnionAccumulator" "OwnedUnionBits" [bits value]
let attempt encode action = try SemanticJson.union "FSharpResult" "Ok" [encode (action ())] with Failure message | Invalid_argument message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let block label instructions terminator : L.basicBlock = {L.label;instrs=instructions;terminator}
let cfg entry blocks : L.cfg = {L.entry;blocks=L.LabelMap.of_list (List.map (fun (b:L.basicBlock) -> b.L.label,b) blocks)}
let observe source =
 let label = L.Label source in
 let other = L.Label "other" in
 let missing = L.Label "missing" in
 let regs = [L.Virtual 3;L.Virtual (-9);L.Physical L.X0] in
 let fregs = [L.FVirtual (-1);L.FVirtual 7;L.FVirtual (-1000);L.FVirtual (-1001);L.FVirtual (-1002);L.FVirtual (-2000);L.FPhysical L.D0] in
 let instructionCases = List.concat_map (fun reg -> List.concat_map (fun freg -> List.concat_map (fun operand -> List.concat_map (fun typ ->
  List.map (fun instr -> let b=block label [instr] L.Ret in
   tuple [ProductionLIR.instr instr;list i (RegisterFacts.getUsedVRegs instr);option i (RegisterFacts.getDefinedVReg instr);list i (RegisterFacts.getUsedFVRegs instr);option i (RegisterFacts.getDefinedFVReg instr);array classified (RegisterFacts.classifyBlocks [|b|])])
    (LIRFixtures.instructionsWithRegisters source reg freg operand typ)) [AST.TInt64;AST.TFloat64]) [L.Reg reg;L.Imm 0L;L.FuncAddr (AST.functionId (-1L))]) fregs) regs in
 let terminatorCases = List.concat_map (fun reg -> List.map (fun term -> tuple [ProductionLIR.terminator term;list i (RegisterFacts.getTerminatorUsedVRegs term);list ProductionLIR.label (RegisterLiveness.getSuccessors term)])
  [L.Ret;L.Jump other;L.Branch (reg,label,other);L.BranchZero (reg,label,other);L.BranchBitZero (reg,3,label,other);L.BranchBitNonZero (reg,3,label,other);L.CondBranch (L.EQ,label,other)]) regs in
 let idCases = [[];[3];[-9;0;3;3;-9];List.init 63 Fun.id;List.init 64 Fun.id;List.init 65 Fun.id;List.init 129 (fun n -> n*2-129)] in
 let domains = List.map (fun ids ->
  let d=buildVRegDomain ids in
  let selected=List.filteri (fun n _ -> n mod 2=0) ids in
  let b=vregBitsFromList d selected in
  let before=bits b in
  let lookups=List.map (fun id -> tuple [i id;option i (tryIndexOf d id);`Bool (vregBitsContains d b id)]) (ids @ [-2147483648;2147483647;987]) in
  let mutations=List.map (fun id -> let value=Bitset.clone b in
   let add=attempt (fun () -> `Null) (fun () -> vregBitsAddInPlace d id value) in
   let afterAdd=bits value in
   let remove=attempt (fun () -> `Null) (fun () -> vregBitsRemoveInPlace d id value) in
   tuple [add;afterAdd;remove;bits value]) (ids @ [987]) in
  tuple [domain d;before;`List lookups;`List mutations;attempt bits (fun () -> vregBitsFromList d [987])]) idCases in
 let unions = List.map (fun size ->
  let empty=Bitset.empty size in
  let a=Bitset.empty size in let b=Bitset.empty size in
  if size>0 then (Bitset.addIndexInPlace 0 a;Bitset.addIndexInPlace (size*64-1) b);
  let current=ref NoUnionBits in
  (* Empty OCaml arrays share a runtime atom; aliasing matters for mutable words. *)
  let steps=List.map (fun input -> current:=bitsetAccumulateUnion !current input;let finished=bitsetFinishUnion empty !current in tuple [accumulator !current;bits finished;`Bool (size>0 && finished==empty);`Bool (size>0 && finished==a);`Bool (size>0 && finished==b)]) [empty;a;empty;b;a] in
  tuple [`List steps;bits empty;bits a;bits b]) [0;1;2;3] in
 let graphs = List.concat_map (fun ids -> let pairs=List.combine ids (match ids with [] -> [] | _::rest -> rest @ [List.hd ids]) in
  List.map (fun edges -> attempt (fun (g:interferenceGraph) ->
   let d=g.domain in let count=Array.length d.ids in
   let result={domain=d;colors=Array.init count (fun n -> if n mod 3=0 then None else Some (n mod 4));spills=vregBitsFromList d (List.filteri (fun n _ -> n mod 3=0) (Array.to_list d.ids));chromaticNumber=4} in
   tuple [graph g;list (fun id -> tuple [i id;`Bool (graphHasVertex g id);list i (graphNeighbors g id);option i (colorOf result id);`Bool (isSpill result id)]) (ids @ [987]);coloring result;i (spillCount result);i (coloredCount result)])
   (fun () -> buildInterferenceGraphFromEdges ids edges)) [[];pairs;pairs @ pairs;List.concat_map (fun a -> List.map (fun b -> a,b) ids) ids;[987,987];[987,3]]) [[];[-9;0;3];List.init 65 Fun.id;List.init 129 Fun.id] in
 let instructionCFGs = List.map (fun instr -> cfg label [block label [instr] (L.Jump other);block other [L.Add (L.Virtual 9,L.Virtual 3,L.Reg (L.Virtual (-9)));L.FAdd (L.FVirtual 9,L.FVirtual 7,L.FVirtual (-1))] L.Ret]) (LIRFixtures.instructions source) in
 let patterns = [cfg label [block label [] L.Ret];cfg label [block label [] (L.Jump missing)];cfg missing [block label [] L.Ret];
  cfg label [block label [L.Mov (L.Virtual 1,L.Reg (L.Virtual 2))] (L.BranchZero (L.Virtual 1,label,other));block other [L.Phi (L.Virtual 3,[L.Reg (L.Virtual 1),label;L.Reg (L.Virtual 2),missing],None);L.FPhi (L.FVirtual 7,[L.FVirtual (-1),label;L.FVirtual 8,missing])] (L.Jump label)];
  cfg label [block label [] (L.CondBranch (L.EQ,other,other));block other [L.Phi (L.Virtual 3,[L.Reg (L.Virtual 1),label;L.Reg (L.Virtual 2),label],None);L.FPhi (L.FVirtual 7,[L.FVirtual (-1000),label])] L.Ret];
  cfg label [block label (List.init 129 (fun n -> L.Mov (L.Virtual (n*2-129),L.Reg (L.Virtual (n*2-127))))) (L.Jump other);block other (List.init 65 (fun n -> L.FMov (L.FVirtual n,L.FVirtual (n+1)))) (L.BranchZero (L.Virtual 1,label,other))]] in
 let cfgCases = List.concat_map (fun cfg -> List.map (fun extra -> attempt (fun (idx,blocks) ->
  let classifiedBlocks=RegisterFacts.classifyBlocks blocks in
  let id,il,fd,fl=RegisterLiveness.computeCombinedLivenessBitsFromFacts idx classifiedBlocks extra extra in
  tuple [blockIndex idx;array ProductionLIR.basicBlock blocks;array classified classifiedBlocks;
   `Assoc ["map",list (fun (label,b) -> tuple [ProductionLIR.label label;ProductionLIR.basicBlock b]) (L.LabelMap.bindings (blocksToMap idx blocks))];
   tuple [domain id;array live il;domain fd;array live fl];
   (let d,l=RegisterLiveness.computeLivenessBitsFromFacts idx classifiedBlocks extra in tuple [domain d;array live l]);
   (let d,l=RegisterLiveness.computeFloatLivenessBitsFromFacts idx classifiedBlocks extra in tuple [domain d;array live l]);
   (let d,b,l=RegisterLiveness.computeLivenessBits cfg in tuple [domain d;blockIndex b;array live l]);
   (let d,b,l=RegisterLiveness.computeFloatLivenessBits cfg in tuple [domain d;blockIndex b;array live l]);
   array (fun b -> tuple [pairBits (RegisterLiveness.computeGenKill id b);pairBits (RegisterLiveness.computeFloatGenKill fd b)]) blocks;
   graph (RegisterInterference.buildInterferenceGraphBitsetWithLiveness idx classifiedBlocks id il (vregBitsFromList id extra));
   graph (RegisterInterference.buildFloatInterferenceGraphBitsetWithLiveness idx classifiedBlocks fd fl (vregBitsFromList fd extra));
   graph (RegisterInterference.buildInterferenceGraphBitsetFast cfg extra);graph (RegisterInterference.buildInterferenceGraphBitset cfg extra);
   list (fun label -> tuple [option i (tryBlockIndex idx label);option i (blockIndexOfLabel idx label);option live (blockLivenessForLabel idx il label)]) [label;other;missing]]) (fun () -> buildBlockIndex cfg)) [[];[-9;3;7;129]]) (instructionCFGs @ patterns) in
 let saveCases = List.map (fun instructions -> let b=block label instructions (L.BranchZero (L.Virtual 3,label,other)) in
  let classifiedBlocks=RegisterFacts.classifyBlocks [|b|] in
  let id,il,fd,fl=RegisterLiveness.computeCombinedLivenessBitsFromFacts {labels=[|label|];entryIndex=0} classifiedBlocks [1;3;9] [-1;7;9] in
  tuple [list ProductionLIR.instr instructions;list (fun instr -> `Bool (RegisterLiveness.isEmptySaveRegs instr)) instructions;attempt (list pairBits) (fun () -> RegisterLiveness.computeSaveRegsPreparation id fd b classifiedBlocks.(0).instrFacts il.(0).liveOut fl.(0).liveOut)])
  [[];[L.SaveRegs ([],[]);L.Mov (L.Virtual 1,L.Reg (L.Virtual 3));L.RestoreRegs ([],[]);L.FAdd (L.FVirtual 9,L.FVirtual 7,L.FVirtual (-1))];[L.SaveRegs ([],[]);L.SaveRegs ([],[]);L.RestoreRegs ([],[]);L.RestoreRegs ([],[])];[L.SaveRegs ([L.X0],[L.D0]);L.RestoreRegs ([L.X0],[L.D0])];[L.SaveRegs ([],[])];[L.RestoreRegs ([],[])]] in
 tuple [`List instructionCases;`List terminatorCases;`List domains;`List unions;`List graphs;`List cfgCases;`List saveCases]
