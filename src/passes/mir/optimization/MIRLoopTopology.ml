(* LoopTopology.fs - Compute dominators and natural-loop interfaces. *)
[@@@warning "-4-30"]
open MIR
module S = SSA_Construction
(*
   Get successors from a basic block terminator
*)
let getSuccessors = S.getSuccessors
(*
   Build successor map for the CFG
*)
let buildSuccessors (cfg : cfg) = LabelMap.map getSuccessors cfg.blocks
(*
   Check whether the reachable CFG contains any cycle.
*)
let cfgHasReachableCycle (cfg : cfg) =
 let successors = buildSuccessors cfg in
 let rec visit active visited label = if LabelSet.mem label active then true, visited else if LabelSet.mem label visited then false, visited else
  let active = LabelSet.add label active in
  let rec next remaining visited = match remaining with [] -> false, visited | label :: rest -> let cycle, visited = visit active visited label in if cycle then true, visited else next rest visited in
  let cycle, visited = next (Option.value ~default:[] (LabelMap.find_opt label successors)) visited in cycle, LabelSet.add label visited in fst (visit LabelSet.empty LabelSet.empty cfg.entry)
(*
   Check if dominator dominates node (using idom chain)
*)
let dominates entry dominators dominator node = if dominator = node then true else if dominator = entry then node = entry || LabelMap.mem node dominators else let rec walk current = match LabelMap.find_opt current dominators with None -> false | Some parent -> if parent = dominator then true else if parent = entry then false else walk parent in walk node
(*
   Identify natural loops via backedges (header dominates source), reusing a
   predecessor map and dominators already computed for this CFG topology.
*)
let findNaturalLoopsWithTopology (cfg : cfg) predecessors dominators =
 let successors = buildSuccessors cfg in
 let backedges = LabelMap.fold (fun from successors backedges -> List.fold_left (fun backedges successor -> if dominates cfg.entry dominators successor from then LabelMap.add successor (from :: Option.value ~default:[] (LabelMap.find_opt successor backedges)) backedges else backedges) backedges successors) successors LabelMap.empty in
 LabelMap.fold (fun header sources loops ->
  let blocks = List.fold_left (fun all source ->
    let initial = LabelSet.of_list [header; source] in
    let rec grow work loop = match work with [] -> loop | node :: rest -> let loop, work = List.fold_left (fun (loop, work) predecessor -> if LabelSet.mem predecessor loop then loop, work else if dominates cfg.entry dominators header predecessor then LabelSet.add predecessor loop, predecessor :: work else loop, work) (loop, rest) (Option.value ~default:[] (LabelMap.find_opt node predecessors)) in grow work loop in LabelSet.union all (grow [source] initial)) LabelSet.empty sources in
  if LabelSet.is_empty blocks then loops else LabelMap.add header blocks loops) backedges LabelMap.empty
(*
   Immutable facts shared only while CFG blocks and edges are unchanged.
*)
type loopTopology = {loops : LabelSet.t LabelMap.t; predecessors : S.predecessors}
type dominatorTopology = {predecessors : S.predecessors; immediateDominators : S.dominators}
let buildDominatorTopology (cfg : cfg) = let predecessors = S.buildPredecessors cfg in {predecessors; immediateDominators = S.computeDominators cfg predecessors}
let tryBuildLoopTopologyWithDominators (cfg : cfg) (topology : dominatorTopology) = if not (cfgHasReachableCycle cfg) then None else Some {loops = findNaturalLoopsWithTopology cfg topology.predecessors topology.immediateDominators; predecessors = topology.predecessors}
let tryBuildLoopTopology cfg = if cfgHasReachableCycle cfg then tryBuildLoopTopologyWithDominators cfg (buildDominatorTopology cfg) else None
(*
   Identify natural loops via backedges (header dominates source).
*)
let findNaturalLoops cfg = match tryBuildLoopTopology cfg with None -> LabelMap.empty | Some topology -> topology.loops
