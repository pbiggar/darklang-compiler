(* Full coalescing and graph coloring observations, including profiles and allocation. *)
[@@@warning "-4"]
open Dark_compiler
open AllocationModel
module C = RegisterCoalescing
module R = RegisterColoring
module L = LIR
let tuple values = `Assoc ["tuple",`List values]
let list encode values = `List (List.map encode values)
let array encode values = `List (Array.to_list (Array.map encode values))
let i = SemanticJson.int32
let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value]
let bits = array (fun value -> `Assoc ["kind",`String "uint64";"value",`String (Printf.sprintf "%Lu" value)])
let domain (value:vRegDomain) = SemanticJson.record "VRegDomain" ["Ids",array i value.ids;"IndexOf",array i value.indexOf;"IndexOffset",i value.indexOffset;"WordCount",i value.wordCount]
let graph (value:interferenceGraph) = SemanticJson.record "InterferenceGraph" ["Domain",domain value.domain;"Vertices",bits value.vertices;"Neighbors",array bits value.neighbors]
let coloring (value:coloringResult) = SemanticJson.record "ColoringResult" ["Domain",domain value.domain;"Colors",array (option i) value.colors;"Spills",bits value.spills;"ChromaticNumber",i value.chromaticNumber]
let profile (value:mcsProfile) = SemanticJson.record "McsProfile" ["VertexCount",i value.vertexCount;"SelectionChecks",i value.selectionChecks;"WeightUpdates",i value.weightUpdates;"BucketSkips",i value.bucketSkips]
let coalesced (value:C.coalescedGraph) = SemanticJson.record "CoalescedGraph" ["Graph",graph value.C.graph;"RepOfIndex",array i value.C.repOfIndex;"RepMembers",array bits value.C.repMembers;"Preferences",array bits value.C.preferences;"Precolored",array (option i) value.C.precolored]
let allocation = function PhysReg reg -> SemanticJson.union "Allocation" "PhysReg" [ProductionLIR.physReg reg] | StackSlot slot -> SemanticJson.union "Allocation" "StackSlot" [i slot]
let allocated (value:allocationResult) = SemanticJson.record "AllocationResult" ["Domain",domain value.domain;"Allocations",array (option allocation) value.allocations;"StackSize",i value.stackSize;"UsedCalleeSaved",list ProductionLIR.physReg value.usedCalleeSaved]
let pair (a,b) = tuple [i a;i b]
let attempt encode action = try SemanticJson.union "FSharpResult" "Ok" [encode (action ())] with Failure message | Invalid_argument message -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string message]
let observe source =
 let regs=[L.Virtual 3;L.Virtual (-9);L.Physical L.X0] in
 let fregs=[L.FVirtual 7;L.FPhysical L.D0] in
 let collectors=List.concat_map (fun reg -> List.concat_map (fun freg ->
  List.map (fun instr -> let blocks=[|{L.label=L.Label source;instrs=[instr];terminator=L.Ret}|] in tuple [ProductionLIR.instr instr;list pair (C.collectMovePairs blocks);list pair (C.collectPhiPairs blocks);list pair (C.collectFPhiPairs blocks);list pair (C.collectFPhiSourceMovePairs blocks);list pair (C.collectPhiPreferences blocks)]) (LIRFixtures.instructionsWithRegisters source reg freg (L.Reg reg) AST.TFloat64)) fregs) regs in
 let chain=[|{L.label=L.Label source;instrs=[L.FMov (L.FVirtual 1,L.FVirtual 2);L.FMov (L.FVirtual 2,L.FVirtual 3);L.FPhi (L.FVirtual 7,[L.FVirtual 1,L.Label source;L.FVirtual 2,L.Label source]);L.Mov (L.Virtual 1,L.Reg (L.Virtual 1));L.Phi (L.Virtual 7,[L.Reg (L.Virtual 1),L.Label source;L.Reg (L.Virtual 7),L.Label source;L.Imm 3L,L.Label source],None)];terminator=L.Ret}|] in
 let chains=tuple [list pair (C.collectMovePairs chain);list pair (C.collectPhiPairs chain);list pair (C.collectFPhiPairs chain);list pair (C.collectFPhiSourceMovePairs chain);list pair (C.collectPhiPreferences chain);list pair (C.dedupePairs [3,1;1,3;0,0;-9,3;3,-9;7,1])] in
 let ids=[-9;0;3;7] in
 let allEdges=[-9,0;-9,3;-9,7;0,3;0,7;3,7] in
 let smallGraphs=List.init 64 (fun mask -> buildInterferenceGraphFromEdges ids (List.filteri (fun n _ -> mask land (1 lsl n)<>0) allEdges)) in
 let largeGraphs=List.concat_map (fun size -> let ids=List.init size (fun n -> n*2-129) in
  let edges=List.filteri (fun n _ -> n mod 4<>0) (List.combine (List.filteri (fun n _ -> n<size-1) ids) (List.tl ids)) in
  let graph=buildInterferenceGraphFromEdges ids edges in
  let vertices=Bitset.empty graph.domain.wordCount in
  Array.iteri (fun n _ -> if n mod 2=0 then Bitset.addIndexInPlace n vertices) graph.domain.ids;
  [graph;{graph with vertices}]) [65;129] in
 let inactive={ (List.hd smallGraphs) with vertices=Bitset.empty 1 } in
 let graphs=buildInterferenceGraphFromEdges [] [] :: inactive :: smallGraphs @ largeGraphs in
 let cases=List.map (fun (g:interferenceGraph) ->
  let order,p=C.maximumCardinalitySearchWithProfile g in
  let variants=List.concat_map (fun precolors -> List.concat_map (fun movePairs -> List.concat_map (fun prefs -> List.map (fun colors ->
   let c=C.coalesceGraphFast g precolors movePairs prefs in
   let synthetic={domain=g.domain;colors=Array.init (Array.length g.domain.ids) (fun n -> if n mod 3=0 then None else Some (n mod 4));spills=Bitset.clone c.C.graph.vertices;chromaticNumber=4} in
   let result=R.chordalGraphColor g precolors colors prefs movePairs in
   let timed,t=R.chordalGraphColorWithTiming HostClock.milliseconds g precolors colors prefs movePairs in
   let timing=tuple [coloring timed;`Bool (t.coalesceMs>=0. && t.mcsMs>=0. && t.greedyMs>=0. && t.expandMs>=0.);`Bool (if Bitset.isEmpty g.vertices then t.coalesceMs=0. && t.mcsMs=0. && t.greedyMs=0. && t.expandMs=0. else true);`Bool (if movePairs=[] && prefs=[] then t.expandMs=0. else true)] in
   tuple [coalesced c;coloring (C.expandColoring synthetic c.C.repMembers);coloring result;timing;
    list (fun registers -> attempt allocated (fun () -> R.coloringToAllocation result registers)) [[];[L.X0];[L.X19;L.X20;L.X0];[L.X26;L.X25;L.X24;L.X23;L.X22;L.X21;L.X20;L.X19]]]) [0;1;2;4]) [[];[-9,3;0,7];[-9,0;0,3;3,7;987,0]]) [[];[-9,3;0,7];[-9,0;0,3;3,7;987,0]]) [[];[-9,0;7,1];[-9,0;0,1;3,0;987,2];[-9,3;0,0;3,2;7,1];[-9,-1;3,2147483647]] in
  let precolored=Array.init (Array.length g.domain.ids) (fun n -> if n mod 3=0 then Some (n mod 4) else None) in
  let preferences=Array.init (Array.length g.domain.ids) (fun n -> let bits=Bitset.empty g.domain.wordCount in if n>0 then Bitset.addIndexInPlace (n-1) bits;bits) in
  let greedy=List.concat_map (fun ordering -> List.map (fun colors -> attempt coloring (fun () -> R.greedyColorReverse g ordering precolored colors preferences)) [0;1;2;4]) [order;List.rev order;order @ order;987::order] in
  tuple [graph g;list i order;profile p;list i (C.maximumCardinalitySearch g);`List variants;`List greedy]) graphs in
 tuple [`List collectors;chains;`List cases]
