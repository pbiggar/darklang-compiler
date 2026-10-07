(* ControlFlow.fs - Simplify MIR branches, joins, and unreachable blocks. *)
[@@@warning "-4"]
open MIR
module S = SSA_Construction
module F = MIROptimizationFacts
module C = MIRCopyPropagation
let labelText (Label name) = StructuralFormat.format (StructuralFormat.Union ("Label", [StructuralFormat.Text name]))
(*
   Merge a block ending in an unconditional jump with its sole-predecessor
   successor. Successor phis become copies, while phi edges leaving the merged
   block are relabeled to preserve their predecessor identity.
*)
let mergeLinearBlocks (cfg : cfg) =
 let rec merge current changed =
  let predecessors = S.buildPredecessors current in
  let candidate = LabelMap.bindings current.blocks |> List.find_map (fun (source, block) -> match block.terminator with Jump successor when successor <> source && successor <> current.entry -> (match LabelMap.find_opt successor predecessors, LabelMap.find_opt successor current.blocks with Some [predecessor], Some next when predecessor = source && List.for_all (function Phi (_, [(_, from)], _) -> from = source | Phi _ -> false | _ -> true) next.instrs -> Some (source, block, successor, next) | _ -> None) | _ -> None) in
  match candidate with None -> current, changed | Some (source, block, successor, next) ->
   let instrs = List.map (function Phi (dest, [(operand, from)], typ) when from = source -> Mov (dest, operand, typ) | Phi _ -> Crash.crash ("mergeLinearBlocks: invalid phi in sole-predecessor block " ^ labelText successor) | instruction -> instruction) next.instrs in
   let block = {block with instrs = block.instrs @ instrs; terminator = next.terminator} in
   let blocks = LabelMap.remove successor current.blocks |> LabelMap.add source block |> LabelMap.map (fun block -> let instrs = List.map (function Phi (dest, sources, typ) -> Phi (dest, List.map (fun (operand, from) -> operand, if from = successor then source else from) sources, typ) | instruction -> instruction) block.instrs in {block with instrs}) in merge {current with blocks} true in merge cfg false
(*
   CFG Simplification: Remove empty blocks (just a jump)
   Find blocks that only contain a Jump
   Don't remove entry block
   Distinct empty predecessors may carry different phi values.
   Redirect jumps through empty blocks (follow chains)
   Also update phi sources
*)
let simplifyEmptyBlocks (cfg : cfg) =
 let phiSources = LabelMap.fold (fun _ block sources -> List.fold_left (fun sources instruction -> match instruction with Phi (_, edges, _) when List.length edges > 1 -> List.fold_left (fun sources (_, from) -> LabelSet.add from sources) sources edges | _ -> sources) sources block.instrs) cfg.blocks LabelSet.empty in
 let empty = LabelMap.filter (fun label block -> label <> cfg.entry && not (LabelSet.mem label phiSources) && block.instrs = [] && (match block.terminator with Jump _ -> true | _ -> false)) cfg.blocks |> LabelMap.map (fun block -> match block.terminator with Jump target -> target | _ -> Crash.crash "Expected Jump") in
 if LabelMap.is_empty empty then cfg, false else
 let predecessors = S.buildPredecessors cfg in
 let redirect label = let rec follow seen current = if LabelSet.mem current seen then current else match LabelMap.find_opt current empty with None -> current | Some next -> follow (LabelSet.add current seen) next in follow LabelSet.empty label in
 let replacement label =
  let rec collect seen current = if LabelSet.mem current seen then [] else if LabelMap.mem current empty then List.concat_map (collect (LabelSet.add current seen)) (Option.value ~default:[] (LabelMap.find_opt current predecessors)) else [current] in
  let _, distinct = List.fold_left (fun (seen, distinct) label -> if LabelSet.mem label seen then seen, distinct else LabelSet.add label seen, label :: distinct) (LabelSet.empty, []) (collect LabelSet.empty label) in
  match List.rev distinct with [] -> Crash.crash ("simplifyEmptyBlocks: no remaining predecessor for phi source " ^ labelText label) | labels -> labels in
 let blocks = LabelMap.filter (fun label _ -> not (LabelMap.mem label empty)) cfg.blocks |> LabelMap.map (fun block ->
  let terminator = match block.terminator with Jump target -> Jump (redirect target) | Branch (condition, yes, no) -> Branch (condition, redirect yes, redirect no) | Ret operand -> Ret operand in
  let instrs = List.map (function Phi (dest, sources, typ) -> Phi (dest, List.concat_map (fun (operand, label) -> List.map (fun label -> operand, label) (replacement label)) sources, typ) | instruction -> instruction) block.instrs in {block with instrs; terminator}) in {cfg with blocks}, true
(*
   Simplify join blocks that only return a phi-selected value.
   Pattern:
   pred1: ...; Jump join
   pred2: ...; Jump join
   join:
   p <- Phi([(v1, pred1), (v2, pred2)])
   Ret p
   Becomes:
   pred1: ...; Ret v1
   pred2: ...; Ret v2
   and removes `join`.
   Most functions have no return-phi join. Avoid constructing a complete
   predecessor map on every fixed-point iteration until a block can
   actually match the transformation.
   Require exact predecessor/source match and direct jumps to join.
   A predecessor may feed multiple candidate joins only in impossible CFGs
   (single terminator), so pick the matching join by current terminator.
*)
let simplifyRetPhiJoinLayer (cfg : cfg) =
 let potential = LabelMap.bindings cfg.blocks |> List.filter (fun (_, block) -> match block.terminator with Ret (Register _) -> List.exists (function Phi _ -> true | _ -> false) block.instrs | _ -> false) in
 let predecessors = if potential = [] then LabelMap.empty else S.buildPredecessors cfg in
 let candidates = potential |> List.filter_map (fun (join, block) -> match block.terminator with Ret (Register returned) ->
  let copies = List.fold_left (fun copies instruction -> match instruction with Mov (dest, source, _) -> VRegMap.add dest source copies | _ -> copies) VRegMap.empty block.instrs in
  let returned = C.resolveCopy copies (Register returned) in
  let phis, others = List.partition (function Phi (dest, _, _) -> returned = Register dest | _ -> false) block.instrs in
  (match phis with [Phi (_, sources, _)] when List.for_all (fun instruction -> not (F.hasSideEffects instruction)) others ->
    let preds = Option.value ~default:[] (LabelMap.find_opt join predecessors) in
    let allJump = List.for_all (fun predecessor -> match LabelMap.find_opt predecessor cfg.blocks with Some block -> (match block.terminator with Jump target -> target = join | _ -> false) | None -> false) preds in
    if LabelSet.equal (LabelSet.of_list preds) (LabelSet.of_list (List.map snd sources)) && allJump then Some (join, LabelMap.of_list (List.map (fun (operand, label) -> label, operand) sources)) else None | _ -> None)
  | _ -> None) |> LabelMap.of_list in
 if LabelMap.is_empty candidates then cfg, false else
 let joins = LabelSet.of_list (List.map fst (LabelMap.bindings candidates)) in
 let preds = LabelMap.fold (fun _ sources preds -> LabelMap.fold (fun label _ preds -> LabelSet.add label preds) sources preds) candidates LabelSet.empty in
 let blocks = LabelMap.filter (fun label _ -> not (LabelSet.mem label joins)) cfg.blocks |> LabelMap.mapi (fun label block -> if LabelSet.mem label preds then match block.terminator with Jump target -> (match LabelMap.find_opt target candidates with Some sources -> (match LabelMap.find_opt label sources with Some operand -> {block with terminator = Ret operand} | None -> block) | None -> block) | _ -> block else block) in {cfg with blocks}, true
let simplifyRetPhiJoins cfg = let rec collapse current changed = match simplifyRetPhiJoinLayer current with next, true -> collapse next true | next, false -> next, changed in collapse cfg false
