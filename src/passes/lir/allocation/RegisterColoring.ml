(* RegisterColoring.ml - Color interference graphs and map colors to physical registers. *)
[@@@warning "-4"]
open AllocationModel
open RegisterCoalescing
let emptyColoringResult (domain:vRegDomain) = {domain;colors=Array.make (Array.length domain.ids) None;spills=Bitset.empty domain.wordCount;chromaticNumber=0}
(*
   Build the inputs needed by greedy coloring when there are no move or phi
   pairs to coalesce. Most small generated functions take this path, avoiding
   construction and cloning of several full-domain bitset arrays.
*)
let uncoalescedColoringInputs (graph:interferenceGraph) precoloredPairs =
 let domain=graph.domain in
 let precolored=Array.make (Array.length domain.ids) None in
 List.iter (fun (vregId,color) -> match tryIndexOf domain vregId with Some idx when Bitset.containsIndex idx graph.vertices -> precolored.(idx)<-Some color | _ -> ()) precoloredPairs;
 let emptyPreferences=Bitset.empty domain.wordCount in
 let preferences=Array.make (Array.length domain.ids) emptyPreferences in
 precolored,preferences
(*
   Greedy color in reverse PEO order with phi coalescing preferences
   For chordal graphs, this produces an optimal coloring.
   When preferences are provided, try to use colors that match coalesced partners.
   Uses two-pass approach: first color vregs with no uncolored phi partners,
   then color deferred vregs (whose partners are now colored).
   Apply pre-colored vertices
*)
let greedyColorReverse (graph:interferenceGraph) peo precolored numColors preferences =
 let domain=graph.domain in let n=Array.length domain.ids in let wordCount=domain.wordCount in
 let colors=Array.make n None in let spills=Bitset.empty wordCount in let maxColor=ref (-1) in
 for idx=0 to n-1 do match precolored.(idx) with Some c -> colors.(idx)<-Some c;if c> !maxColor then maxColor:=c | None -> () done;
 let inGraph=Array.make n false in Bitset.iterIndices graph.vertices (fun idx -> inGraph.(idx)<-true);
 let peoIndices=List.map (fun v -> match tryIndexOf domain v with Some idx -> idx | None -> Crash.crash ("Greedy coloring missing vertex "^string_of_int v)) peo in
 let markUsedColors idx used = Bitset.iterIndices graph.neighbors.(idx) (fun nidx -> if inGraph.(nidx) then match colors.(nidx) with Some c when c>=0 && c<numColors -> used.(c)<-true | _ -> ()) in
 let colorVertex idx = if colors.(idx)=None then (
  let used=Array.make numColors false in markUsedColors idx used;
  let prefColor=ref None in
  Bitset.iterIndices preferences.(idx) (fun pidx -> match colors.(pidx) with Some c when !prefColor=None && c>=0 && c<numColors && not used.(c) -> prefColor:=Some c | _ -> ());
  let assignColor c = colors.(idx)<-Some c;if c> !maxColor then maxColor:=c in
  match !prefColor with Some c -> assignColor c | None ->
  let assigned=ref false in
  for c=0 to numColors-1 do if not !assigned && not used.(c) then (assignColor c;assigned:=true) done;
  if not !assigned then Bitset.addIndexInPlace idx spills) in
 let hasUncoloredPartners idx =
  let found=ref false in Bitset.iterIndices preferences.(idx) (fun pidx -> if not !found && inGraph.(pidx) && colors.(pidx)=None then found:=true);!found in
 let interferes idx1 idx2 = Bitset.containsIndex idx2 graph.neighbors.(idx1) in
 let colorVertexWithPartners idx = if colors.(idx)=None then (
  let candidates=let acc=ref [] in Bitset.iterIndices preferences.(idx) (fun pidx -> if inGraph.(pidx) && colors.(pidx)=None && not (interferes idx pidx) then acc:=pidx::!acc);List.rev !acc in
  let rec filterMutuallyCompatible acc = function [] -> List.rev acc | p::rest -> if List.for_all (fun a -> not (interferes p a)) acc then filterMutuallyCompatible (p::acc) rest else filterMutuallyCompatible acc rest in
  let coalesceable=filterMutuallyCompatible [] candidates in let allVertices=idx::coalesceable in
  let used=Array.make numColors false in List.iter (fun vertex -> markUsedColors vertex used) allVertices;
  let assigned=ref false in
  for c=0 to numColors-1 do if not !assigned && not used.(c) then (
   List.iter (fun vertex -> if colors.(vertex)=None then colors.(vertex)<-Some c) allVertices;
   if c> !maxColor then maxColor:=c;
   assigned:=true) done;
  if not !assigned then Bitset.addIndexInPlace idx spills) in
 let deferred=Bitset.empty wordCount in
 List.iter (fun idx -> if colors.(idx)=None then if hasUncoloredPartners idx then Bitset.addIndexInPlace idx deferred else colorVertex idx) (List.rev peoIndices);
 List.iter (fun idx -> if Bitset.containsIndex idx deferred && colors.(idx)=None then colorVertexWithPartners idx) (List.rev peoIndices);
 {domain;colors;spills;chromaticNumber=if !maxColor<0 then 0 else Int32.to_int (Int32.add (Int32.of_int !maxColor) 1l)}
(*
   Main chordal graph coloring function with phi coalescing preferences
*)
let chordalGraphColor (graph:interferenceGraph) precoloredPairs numColors preferencePairs movePairs =
 if Bitset.isEmpty graph.vertices then emptyColoringResult graph.domain
 else if movePairs=[] && preferencePairs=[] then (
  let precolored,preferences=uncoalescedColoringInputs graph precoloredPairs in
  let peo=maximumCardinalitySearch graph in greedyColorReverse graph peo precolored numColors preferences)
 else let coalesced=coalesceGraphFast graph precoloredPairs movePairs preferencePairs in
 let peo=maximumCardinalitySearch coalesced.graph in
 let result=greedyColorReverse coalesced.graph peo coalesced.precolored numColors coalesced.preferences in
 expandColoring result coalesced.repMembers
let chordalGraphColorWithTiming elapsed (graph:interferenceGraph) precoloredPairs numColors preferencePairs movePairs =
 if Bitset.isEmpty graph.vertices then emptyColoringResult graph.domain,{coalesceMs=0.;mcsMs=0.;greedyMs=0.;expandMs=0.}
 else if movePairs=[] && preferencePairs=[] then (
  let start=elapsed () in
  let precolored,preferences=uncoalescedColoringInputs graph precoloredPairs in
  let prepMs=elapsed () -. start in let mcsStart=elapsed () in
  let peo=maximumCardinalitySearch graph in
  let mcsMs=elapsed () -. mcsStart in let greedyStart=elapsed () in
  let result=greedyColorReverse graph peo precolored numColors preferences in
  let greedyMs=elapsed () -. greedyStart in result,{coalesceMs=prepMs;mcsMs;greedyMs;expandMs=0.})
 else
 let timePhase action = let start=elapsed () in let result=action () in let elapsedMs=elapsed () -. start in result,elapsedMs in
 let coalesced,coalesceMs=timePhase (fun () -> coalesceGraphFast graph precoloredPairs movePairs preferencePairs) in
 let peo,mcsMs=timePhase (fun () -> maximumCardinalitySearch coalesced.graph) in
 let result,greedyMs=timePhase (fun () -> greedyColorReverse coalesced.graph peo coalesced.precolored numColors coalesced.preferences) in
 let expanded,expandMs=timePhase (fun () -> expandColoring result coalesced.repMembers) in
 expanded,{coalesceMs;mcsMs;greedyMs;expandMs}
(*
   Convert chordal graph coloring result to allocation result
   Colors map to physical registers, spills map to stack slots
   Map colored vertices to physical registers
   Track callee-saved register usage
   Color out of range - treat as spill
   Map spilled vertices to stack slots
   Compute 16-byte aligned stack size
*)
let coloringToAllocation (colorResult:coloringResult) registers =
 let domain=colorResult.domain in let n=Array.length domain.ids in let allocations=Array.make n None in
 let nextStackSlot=ref (-8) in let usedCalleeSaved=ref [] in
 for idx=0 to n-1 do match colorResult.colors.(idx) with
 | Some color -> if color<List.length registers then (
  if color<0 then invalid_arg "The index was outside the range of elements in the list. (Parameter 'index')";
  let reg=List.nth registers color in allocations.(idx)<-Some (PhysReg reg);
  if List.mem reg [LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26] && not (List.mem reg !usedCalleeSaved) then usedCalleeSaved:=reg::!usedCalleeSaved)
  else (allocations.(idx)<-Some (StackSlot !nextStackSlot);nextStackSlot:= !nextStackSlot-8)
 | None -> () done;
 Bitset.iterIndices colorResult.spills (fun idx -> if allocations.(idx)=None then (allocations.(idx)<-Some (StackSlot !nextStackSlot);nextStackSlot:= !nextStackSlot-8));
 let stackSize=if !nextStackSlot= -8 then 0 else ((abs !nextStackSlot+15)/16)*16 in
 {domain;allocations;stackSize;usedCalleeSaved=List.sort Stdlib.compare !usedCalleeSaved}
