(* Model.fs - Represent register domains, liveness sets, and interference graphs. *)
[@@@warning "-4-30"]
type liveInterval = {vRegId : int; start : int; end_ : int}
(*
   Bitset of VRegDomain indices
*)
type bitSet = Bitset.bitset
(*
   Dense domain for VReg IDs used by bitsets
*)
type vRegDomain = {ids : int array; indexOf : int array; indexOffset : int; wordCount : int}
(*
   Dense domain for basic block labels
*)
type blockIndex = {labels : LIR.label array; entryIndex : int}
(*
   Allocation target for a virtual register
*)
type allocation = PhysReg of LIR.physReg | StackSlot of int
(*
   Result of register allocation
*)
type allocationResult = {domain : vRegDomain; allocations : allocation option array; stackSize : int; usedCalleeSaved : LIR.physReg list}
(*
   Liveness information for a basic block
*)
type blockLiveness = {liveIn : bitSet; liveOut : bitSet}
(*
   Timing information for register allocation phases
*)
type registerAllocationTiming = {phase : string; elapsedMs : float}
type chordalColoringTiming = {coalesceMs : float; mcsMs : float; greedyMs : float; expandMs : float}
(*
   Register facts shared by domain construction, liveness, and interference.
   Phi uses remain separate because they are live on predecessor edges rather
   than at the instruction's position in its block.
*)
type instrRegisterFacts = {instr : LIR.instr; intUses : int list; intDef : int option; intPhiUses : (int * LIR.label) list; floatUses : int list; floatDef : int option; floatPhiUses : (int * LIR.label) list}
type classifiedBlock = {block : LIR.basicBlock; instrFacts : instrRegisterFacts array; terminatorUses : int list; hasPhiNodes : bool}
let add a b = Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let sub a b = Int32.to_int (Int32.sub (Int32.of_int a) (Int32.of_int b))
(*
   Bitset Utilities (used to speed up register allocation)
*)
let buildVRegDomain ids =
 let sorted = List.sort Int.compare ids in
 let rec unique last remaining acc = match remaining with
  | [] -> List.rev acc
  | head :: tail -> (match last with Some value when value = head -> unique last tail acc | _ -> unique (Some head) tail (head :: acc)) in
 let ordered = unique None sorted [] in
 let idsArray = Array.of_list ordered in
 let wordCount = Bitset.wordCount (Array.length idsArray) in
 match ordered with
 | [] -> {ids=idsArray;indexOf=[||];indexOffset=0;wordCount=0}
 | minId :: _ ->
  let rec lastId current = function [] -> current | head :: tail -> lastId head tail in
  let maxId = lastId minId ordered in
  let size = add (sub maxId minId) 1 in
  let indexOf = Array.make size (-1) in
  List.iteri (fun index id -> indexOf.(sub id minId) <- index) ordered;
  {ids=idsArray;indexOf;indexOffset=minId;wordCount}
let tryIndexOf (domain : vRegDomain) value =
 if Array.length domain.indexOf = 0 then None else
 let index = sub value domain.indexOffset in
 if index < 0 || index >= Array.length domain.indexOf then None else
 let mapped = domain.indexOf.(index) in if mapped >= 0 then Some mapped else None
let vregBitsContains domain bits value = match tryIndexOf domain value with Some index -> Bitset.containsIndex index bits | None -> false
let vregBitsAddInPlace domain value bits = match tryIndexOf domain value with Some index -> Bitset.addIndexInPlace index bits | None -> Crash.crash ("RegisterAllocation: missing vreg " ^ string_of_int value ^ " in bitset domain")
let vregBitsRemoveInPlace domain value bits = match tryIndexOf domain value with Some index -> Bitset.removeIndexInPlace index bits | None -> Crash.crash ("RegisterAllocation: missing vreg " ^ string_of_int value ^ " in bitset domain")
type bitSetUnionAccumulator = NoUnionBits | BorrowedUnionBits of bitSet | OwnedUnionBits of bitSet
(*
   Accumulate unions without allocating for zero or one non-empty input and without
   replacing the owned result after the second non-empty input.
*)
let bitsetAccumulateUnion accumulator bits =
 if Bitset.isEmpty bits then accumulator else match accumulator with
 | NoUnionBits -> BorrowedUnionBits bits
 | BorrowedUnionBits existing -> OwnedUnionBits (Bitset.union existing bits)
 | OwnedUnionBits result -> Bitset.unionInPlace result bits;accumulator
let bitsetFinishUnion emptyBits = function NoUnionBits -> emptyBits | BorrowedUnionBits bits | OwnedUnionBits bits -> bits
let vregBitsFromList domain values =
 if values = [] then Bitset.empty domain.wordCount else
 let bits = Bitset.empty domain.wordCount in
 List.iter (fun value -> match tryIndexOf domain value with Some index -> Bitset.addIndexInPlace index bits | None -> Crash.crash ("BitSet: Missing vreg " ^ string_of_int value ^ " in domain")) values;
 bits
let tryLabelIndex labels label =
 if Array.length labels = 0 then None else
 let rec search low high =
  if low > high then None else
  let mid = add low high / 2 in
  let LIR.Label actual = labels.(mid) in
  let LIR.Label wanted = label in
  let compare = StringOrder.compare actual wanted in
  if compare = 0 then Some mid else if compare < 0 then search (add mid 1) high else search low (sub mid 1) in
 search 0 (sub (Array.length labels) 1)
let labelText (LIR.Label name) = StructuralFormat.format (StructuralFormat.Union ("Label",[StructuralFormat.Text name]))
let buildBlockIndex (cfg : LIR.cfg) =
 let entries = Array.of_list (LIR.LabelMap.bindings cfg.LIR.blocks) in
 let labels = Array.map fst entries in
 let blocks = Array.map snd entries in
 let entryIndex = match tryLabelIndex labels cfg.LIR.entry with Some index -> index | None -> Crash.crash ("BlockIndex: Missing entry label " ^ labelText cfg.LIR.entry) in
 {labels;entryIndex},blocks
let tryBlockIndex index label = tryLabelIndex index.labels label
let blockIndexOfLabel index label = tryBlockIndex index label
let blockLivenessForLabel index liveness label = match tryBlockIndex index label with Some index -> Some liveness.(index) | None -> None
let blocksToMap index blocks = LIR.LabelMap.of_list (Array.to_list (Array.combine index.labels blocks))
(*
   Chordal Graph Coloring Types
   Interference graph for register allocation
   In SSA form, this graph is guaranteed to be chordal
   Domain indices present in the graph
   Adjacency bitsets per domain index
*)
type interferenceGraph = {domain : vRegDomain; vertices : bitSet; neighbors : bitSet array}
(*
   Result of graph coloring
   Domain index → color (0..k-1)
   Domain indices that must be spilled
   Max color used + 1
*)
type coloringResult = {domain : vRegDomain; colors : int option array; spills : bitSet; chromaticNumber : int}
(*
   Profiling data for Maximum Cardinality Search
*)
type mcsProfile = {vertexCount : int; selectionChecks : int; weightUpdates : int; bucketSkips : int}
(*
   Build an interference graph from an explicit vertex list and edge list.
*)
let buildInterferenceGraphFromEdges vertices edges =
 let domain = buildVRegDomain vertices in
 let n = Array.length domain.ids in
 let wordCount = domain.wordCount in
 let neighbors = Array.init n (fun _ -> Bitset.empty wordCount) in
 let present = Bitset.empty wordCount in
 List.iter (fun vertex -> match tryIndexOf domain vertex with Some index -> Bitset.addIndexInPlace index present | None -> Crash.crash ("Interference graph missing vertex " ^ string_of_int vertex)) vertices;
 List.iter (fun (left,right) -> if left <> right then match tryIndexOf domain left,tryIndexOf domain right with
  | Some indexLeft,Some indexRight -> Bitset.addIndexInPlace indexLeft present;Bitset.addIndexInPlace indexRight present;Bitset.addIndexInPlace indexRight neighbors.(indexLeft);Bitset.addIndexInPlace indexLeft neighbors.(indexRight)
  | _ -> Crash.crash ("Interference graph missing edge endpoint " ^ string_of_int left ^ " or " ^ string_of_int right)) edges;
 {domain;vertices=present;neighbors}
(*
   Check if a graph contains a vertex.
*)
let graphHasVertex (graph : interferenceGraph) vregId = vregBitsContains graph.domain graph.vertices vregId
(*
   Get neighbors of a vertex in the interference graph.
*)
let graphNeighbors (graph : interferenceGraph) vregId = match tryIndexOf graph.domain vregId with
 | None -> []
 | Some index -> if not (Bitset.containsIndex index graph.vertices) then [] else
  Bitset.indicesToList graph.neighbors.(index) |> List.filter_map (fun neighbor -> if Bitset.containsIndex neighbor graph.vertices then Some graph.domain.ids.(neighbor) else None)
(*
   Get the assigned color of a vertex.
*)
let colorOf (result : coloringResult) vregId = match tryIndexOf result.domain vregId with Some index -> result.colors.(index) | None -> None
(*
   Check if a vertex was spilled.
*)
let isSpill (result : coloringResult) vregId = match tryIndexOf result.domain vregId with Some index -> Bitset.containsIndex index result.spills | None -> false
(*
   Count spilled vertices.
*)
let spillCount (result : coloringResult) = Bitset.count result.spills
(*
   Count colored vertices.
*)
let coloredCount (result : coloringResult) = Array.fold_left (fun count color -> if Option.is_some color then add count 1 else count) 0 result.colors
