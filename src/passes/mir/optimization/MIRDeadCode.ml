(* DeadCode.fs - Eliminate MIR definitions unreachable from observable roots. *)
open MIR
module F = MIROptimizationFacts
let measure recorder name action = match recorder with None -> action () | Some record -> let started = (Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) in let result = action () in record name (Int64.of_float (((Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e6) -. started) *. 1000000.)); result
let buildDefUseMap (cfg : cfg) = LabelMap.fold (fun _ block defs -> List.fold_left (fun defs instruction -> match F.getInstrDest instruction with Some dest -> VRegMap.add dest (F.foldInstrUses (fun uses reg -> reg :: uses) [] instruction) defs | None -> defs) defs block.instrs) cfg.blocks VRegMap.empty
(*
   Collect registers that are directly required by side effects and control flow.
*)
let collectRootUses (cfg : cfg) = LabelMap.fold (fun _ block roots -> let roots = List.fold_left (fun roots instruction -> if F.hasSideEffects instruction then F.foldInstrUses (fun roots reg -> VRegSet.add reg roots) roots instruction else roots) roots block.instrs in F.foldTerminatorUses (fun roots reg -> VRegSet.add reg roots) roots block.terminator) cfg.blocks VRegSet.empty
(*
   Mark live SSA destinations by walking backwards from root uses.
   Parameters or registers without a local definition.
*)
let collectLiveDestinations recorder cfg =
 let defs = measure recorder "MIR DCE Def-Use Graph" (fun () -> buildDefUseMap cfg) in
 let roots = measure recorder "MIR DCE Root Collection" (fun () -> collectRootUses cfg) in
 measure recorder "MIR DCE Reachability" (fun () ->
  let rec visit work seen live = match work with [] -> live | reg :: rest -> match VRegMap.find_opt reg defs with None -> visit rest seen live | Some uses -> let work, seen = List.fold_left (fun (work, seen) reg -> if VRegSet.mem reg seen then work, seen else reg :: work, VRegSet.add reg seen) (rest, seen) uses in visit work seen (VRegSet.add reg live) in visit (VRegSet.elements roots) roots VRegSet.empty)
(*
   Dead Code Elimination
   Remove instructions whose destinations are never used (unless they have side effects)
   Dead instruction - remove it
   Keep instruction
*)
let eliminateDeadCodeWithTickTrace recorder (cfg : cfg) =
 let live = measure recorder "MIR DCE Liveness" (fun () -> collectLiveDestinations recorder cfg) in
 measure recorder "MIR DCE Rewrite" (fun () ->
 let blocks, changed = LabelMap.fold (fun label block (blocks, changed) ->
  let instrs, localChange = List.fold_left (fun (instrs, changed) instruction -> match F.getInstrDest instruction with Some dest when not (VRegSet.mem dest live) && not (F.hasSideEffects instruction) -> instrs, true | _ -> instruction :: instrs, changed) ([], false) block.instrs in
  LabelMap.add label {block with instrs = List.rev instrs} blocks, changed || localChange) cfg.blocks (LabelMap.empty, false) in {cfg with blocks}, changed)
let eliminateDeadCode cfg = eliminateDeadCodeWithTickTrace None cfg
