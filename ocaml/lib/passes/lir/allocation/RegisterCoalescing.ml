(* Coalescing.fs - Collect move preferences and coalesce compatible graph vertices. *)
[@@@warning "-4"]
open AllocationModel
module IntSet = Set.Make (Int)
let normalizePair a b = if a < b then a,b else b,a
let dedupePairs pairs =
 let sorted = List.sort Stdlib.compare (List.map (fun (a,b) -> normalizePair a b) pairs) in
 let rec loop last acc = function
 | [] -> List.rev acc
 | head::tail -> (match last with Some prev when prev=head -> loop last acc tail | _ -> loop (Some head) (head::acc) tail) in
 loop None [] sorted
(*
   Collect move-related coalescing pairs from CFG.
   Returns undirected pairs of virtual registers that are directly moved between.
*)
let collectMovePairs blocks =
 Array.fold_left (fun acc (block:LIR.basicBlock) -> List.fold_left (fun acc -> function
 | LIR.Mov (LIR.Virtual destId,LIR.Reg (LIR.Virtual srcId)) -> (destId,srcId)::acc | _ -> acc) acc block.LIR.instrs) [] blocks |> dedupePairs
(*
   Collect phi-related coalescing pairs from CFG.
   Returns undirected pairs of virtual registers that flow into the same phi destination.
*)
let collectPhiPairs blocks =
 Array.fold_left (fun acc (block:LIR.basicBlock) -> List.fold_left (fun acc -> function
 | LIR.Phi (LIR.Virtual destId,sources,_) -> List.fold_left (fun acc -> function
  | LIR.Reg (LIR.Virtual srcId),_ when srcId<>destId -> (destId,srcId)::acc | _ -> acc) acc sources
 | _ -> acc) acc block.LIR.instrs) [] blocks |> dedupePairs
(*
   Collect float phi-related coalescing pairs from CFG blocks.
   Returns undirected pairs of non-identical virtual float registers that flow
   into the same FPhi destination.
*)
let collectFPhiPairs blocks =
 Array.fold_left (fun acc (block:LIR.basicBlock) -> List.fold_left (fun acc -> function
 | LIR.FPhi (LIR.FVirtual destId,sources) -> List.fold_left (fun acc -> function
  | LIR.FVirtual srcId,_ when srcId<>destId -> (destId,srcId)::acc | _ -> acc) acc sources
 | _ -> acc) acc block.LIR.instrs) [] blocks |> dedupePairs
(*
   Collect virtual float moves that define FPhi sources. Coalescing the whole
   incoming copy chain avoids trading a resolved phi move for its feeder move.
*)
let collectFPhiSourceMovePairs blocks =
 let phiSources=Array.fold_left (fun acc (block:LIR.basicBlock) -> List.fold_left (fun acc -> function
 | LIR.FPhi (_,sources) -> List.fold_left (fun acc -> function LIR.FVirtual srcId,_ -> IntSet.add srcId acc | LIR.FPhysical _,_ -> acc) acc sources
 | _ -> acc) acc block.LIR.instrs) IntSet.empty blocks in
 Array.fold_left (fun acc (block:LIR.basicBlock) -> List.fold_left (fun acc -> function
 | LIR.FMov (LIR.FVirtual destId,LIR.FVirtual srcId) when IntSet.mem destId phiSources -> (destId,srcId)::acc | _ -> acc) acc block.LIR.instrs) [] blocks |> dedupePairs
(*
   Collect phi coalescing preferences from CFG.
   Returns undirected pairs (vregId, vregId) representing preferred coalescing.
*)
let collectPhiPreferences blocks = collectPhiPairs blocks
(*
   Maximum Cardinality Search - computes Perfect Elimination Ordering for chordal graphs
   Returns vertices in PEO order (first vertex is most "central")
   Uses a bucket queue for linear-time selection in terms of vertices + edges.
   Track weights and ordered status
   Bucket queue state (weight -> list of vertices)
   Initialize all vertices in bucket 0
*)
let maximumCardinalitySearchCore (graph:interferenceGraph) =
 let domain=graph.domain in
 let n=Array.length domain.ids in
 let vertexCount=Bitset.count graph.vertices in
 if vertexCount=0 then [],{vertexCount=0;selectionChecks=0;weightUpdates=0;bucketSkips=0} else
 let inGraph=Array.make n false in
 Bitset.iterIndices graph.vertices (fun idx -> inGraph.(idx)<-true);
 let weights=Array.make n 0 in
 let ordered=Array.make n false in
 let bucketHeads=Array.make vertexCount (-1) in
 let next=Array.make n (-1) in
 let prev=Array.make n (-1) in
 for idx=0 to n-1 do if inGraph.(idx) then (
  let head=bucketHeads.(0) in next.(idx)<-head;prev.(idx)<-(-1);
  if head<> -1 then prev.(head)<-idx;
  bucketHeads.(0)<-idx) done;
 let removeFromBucket idx weight =
  let p=prev.(idx) in let nidx=next.(idx) in
  if p<> -1 then next.(p)<-nidx else bucketHeads.(weight)<-nidx;
  if nidx<> -1 then prev.(nidx)<-p;
  next.(idx)<-(-1);prev.(idx)<-(-1) in
 let addToBucket idx weight =
  let head=bucketHeads.(weight) in next.(idx)<-head;prev.(idx)<-(-1);
  if head<> -1 then prev.(head)<-idx;
  bucketHeads.(weight)<-idx in
 let currentMax=ref 0 in let ordering=ref [] in
 let selectionChecks=ref 0 in let weightUpdates=ref 0 in let bucketSkips=ref 0 in
 for _=0 to vertexCount-1 do
  while !currentMax>=0 && bucketHeads.(!currentMax)= -1 do decr currentMax;incr bucketSkips done;
  if !currentMax<0 then Crash.crash "MCS bucket queue empty before selecting all vertices";
  let idx=bucketHeads.(!currentMax) in incr selectionChecks;
  removeFromBucket idx !currentMax;ordered.(idx)<-true;ordering:=domain.ids.(idx)::!ordering;
  Bitset.iterIndices graph.neighbors.(idx) (fun nidx -> if inGraph.(nidx) && not ordered.(nidx) then (
   let oldWeight=weights.(nidx) in removeFromBucket nidx oldWeight;
   let newWeight=oldWeight+1 in
   if newWeight>=vertexCount then Crash.crash ("MCS weight overflow: "^string_of_int newWeight^" >= "^string_of_int vertexCount);
   weights.(nidx)<-newWeight;addToBucket nidx newWeight;
   if newWeight> !currentMax then currentMax:=newWeight;
   incr weightUpdates))
 done;
 List.rev !ordering,{vertexCount;selectionChecks= !selectionChecks;weightUpdates= !weightUpdates;bucketSkips= !bucketSkips}
let maximumCardinalitySearchWithProfile graph = maximumCardinalitySearchCore graph
let maximumCardinalitySearch graph = fst (maximumCardinalitySearchWithProfile graph)
type coalescedGraph = {graph : interferenceGraph; repOfIndex : int array; repMembers : bitSet array; preferences : bitSet array; precolored : int option array}
let coalesceGraphFast (graph:interferenceGraph) precoloredPairs movePairs preferencePairs =
 let domain=graph.domain in let n=Array.length domain.ids in let wordCount=domain.wordCount in
 if Bitset.isEmpty graph.vertices then
  {graph;repOfIndex=Array.init n Fun.id;repMembers=Array.init n (fun _ -> Bitset.empty wordCount);preferences=Array.init n (fun _ -> Bitset.empty wordCount);precolored=Array.make n None}
 else
 let inGraph=Array.make n false in Bitset.iterIndices graph.vertices (fun idx -> inGraph.(idx)<-true);
 let parent=Array.init n Fun.id in let sizes=Array.make n 0 in
 let members=Array.init n (fun _ -> Bitset.empty wordCount) in
 let neighbors=Array.init n (fun _ -> Bitset.empty wordCount) in
 let precolor=Array.make n None in let repId=Array.make n 0 in
 for idx=0 to n-1 do
  if inGraph.(idx) then (
   sizes.(idx)<-1;let bits=Bitset.empty wordCount in Bitset.addIndexInPlace idx bits;
   members.(idx)<-bits;neighbors.(idx)<-Bitset.clone graph.neighbors.(idx);repId.(idx)<-domain.ids.(idx))
  else repId.(idx)<-domain.ids.(idx)
 done;
 List.iter (fun (vregId,color) -> match tryIndexOf domain vregId with Some idx when inGraph.(idx) -> precolor.(idx)<-Some color | _ -> ()) precoloredPairs;
 let rec find idx = let p=parent.(idx) in if p=idx then idx else let root=find p in parent.(idx)<-root;root in
 let canMerge rootA rootB = if rootA=rootB then false else match precolor.(rootA),precolor.(rootB) with
  | Some c1,Some c2 when c1<>c2 -> false
  | _ -> if Bitset.intersects neighbors.(rootA) members.(rootB) then false else not (Bitset.intersects neighbors.(rootB) members.(rootA)) in
 let union rootA rootB =
  let ra,rb=if sizes.(rootA)<sizes.(rootB) then rootB,rootA else rootA,rootB in
  parent.(rb)<-ra;sizes.(ra)<-sizes.(ra)+sizes.(rb);
  Bitset.unionInPlace members.(ra) members.(rb);Bitset.unionInPlace neighbors.(ra) neighbors.(rb);Bitset.diffInPlace neighbors.(ra) members.(ra);
  (match precolor.(ra),precolor.(rb) with None,Some color -> precolor.(ra)<-Some color | _ -> ());
  if repId.(rb)<repId.(ra) then repId.(ra)<-repId.(rb) in
 List.iter (fun (u,v) -> match tryIndexOf domain u,tryIndexOf domain v with
  | Some idxU,Some idxV when inGraph.(idxU) && inGraph.(idxV) -> let rootU=find idxU in let rootV=find idxV in if canMerge rootU rootV then union rootU rootV
  | _ -> ()) movePairs;
 let rootOfIdx=Array.init n (fun idx -> if inGraph.(idx) then find idx else idx) in
 let repIndexOfRoot=Array.make n (-1) in
 for idx=0 to n-1 do if inGraph.(idx) && parent.(idx)=idx then (
  let repValue=repId.(idx) in match tryIndexOf domain repValue with Some repIdx -> repIndexOfRoot.(idx)<-repIdx | None -> Crash.crash ("coalesceGraphFast: Missing rep index for "^string_of_int repValue)) done;
 let repOfIndex=Array.make n (-1) in
 for idx=0 to n-1 do if inGraph.(idx) then (
  let root=rootOfIdx.(idx) in let repIdx=repIndexOfRoot.(root) in
  if repIdx<0 then Crash.crash ("coalesceGraphFast: Missing rep for "^string_of_int domain.ids.(idx));
  repOfIndex.(idx)<-repIdx) done;
 let repMembers=Array.init n (fun _ -> Bitset.empty wordCount) in let repVertices=Bitset.empty wordCount in
 for idx=0 to n-1 do if inGraph.(idx) then (let repIdx=repOfIndex.(idx) in Bitset.addIndexInPlace idx repMembers.(repIdx);Bitset.addIndexInPlace repIdx repVertices) done;
 let repPrecolored=Array.make n None in
 for idx=0 to n-1 do if inGraph.(idx) && parent.(idx)=idx then (
  let repIdx=repIndexOfRoot.(idx) in match precolor.(idx) with Some color -> repPrecolored.(repIdx)<-Some color | None -> ()) done;
 let repPreferences=Array.init n (fun _ -> Bitset.empty wordCount) in
 List.iter (fun (u,v) -> match tryIndexOf domain u,tryIndexOf domain v with
  | Some idxU,Some idxV when inGraph.(idxU) && inGraph.(idxV) -> let repU=repOfIndex.(idxU) in let repV=repOfIndex.(idxV) in if repU<>repV && repU>=0 && repV>=0 then (Bitset.addIndexInPlace repV repPreferences.(repU);Bitset.addIndexInPlace repU repPreferences.(repV))
  | _ -> ()) preferencePairs;
 let repNeighbors=Array.init n (fun _ -> Bitset.empty wordCount) in
 for idx=0 to n-1 do if inGraph.(idx) && parent.(idx)=idx then (
  let repIdx=repIndexOfRoot.(idx) in Bitset.iterIndices neighbors.(idx) (fun nidx -> if inGraph.(nidx) then (
   let rootN=rootOfIdx.(nidx) in if rootN<>idx then (
    let repN=repIndexOfRoot.(rootN) in if repIdx<>repN && repIdx>=0 && repN>=0 then (Bitset.addIndexInPlace repN repNeighbors.(repIdx);Bitset.addIndexInPlace repIdx repNeighbors.(repN)))))) done;
 let repGraph={domain;vertices=repVertices;neighbors=repNeighbors} in
 {graph=repGraph;repOfIndex;repMembers;preferences=repPreferences;precolored=repPrecolored}
let expandColoring (result:coloringResult) repMembers =
 let domain=result.domain in let n=Array.length domain.ids in
 let expandedColors=Array.make n None in let expandedSpills=Bitset.empty domain.wordCount in
 for repIdx=0 to n-1 do match result.colors.(repIdx) with
 | Some color -> Bitset.iterIndices repMembers.(repIdx) (fun memberIdx -> expandedColors.(memberIdx)<-Some color)
 | None -> () done;
 Bitset.iterIndices result.spills (fun repIdx -> Bitset.unionInPlace expandedSpills repMembers.(repIdx));
 {domain;colors=expandedColors;spills=expandedSpills;chromaticNumber=result.chromaticNumber}
