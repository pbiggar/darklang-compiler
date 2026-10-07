(*
   MIR_SSA_Verify.fs - Check the definition, dominance, and phi-edge invariants of SSA MIR.
*)
(* MIR_SSA_Verify.fs - Check SSA MIR definitions, dominance and phi edges. *)
[@@@warning "-4"]
open MIR
module S = SSA_Construction
module F = MIROptimizationFacts
type definition = {block : label; position : int}
let regText (VReg value) = "VReg " ^ string_of_int value
let labelText (Label value) = StructuralFormat.format (StructuralFormat.Union ("Label", [StructuralFormat.Text value]))
let reachableLabels (cfg : cfg) = let rec visit pending seen = match pending with [] -> seen | label :: rest when LabelSet.mem label seen -> visit rest seen | label :: rest -> match LabelMap.find_opt label cfg.blocks with None -> visit rest seen | Some block -> visit (S.getSuccessors block @ rest) (LabelSet.add label seen) in visit [cfg.entry] LabelSet.empty
let dominates dominators entry source target = let rec ascend seen current = if current = source then true else if current = entry || LabelSet.mem current seen then false else match LabelMap.find_opt current dominators with Some parent -> ascend (LabelSet.add current seen) parent | None -> false in ascend LabelSet.empty target
let firstError checks = match List.find_map (function Error error -> Some error | Ok () -> None) checks with Some error -> Error error | None -> Ok ()
let verifyFunction (func : functionDef) =
 let cfg = func.cfg in let error message = Error ("SSA MIR " ^ func.name ^ ": " ^ message) in
 match LabelMap.find_opt cfg.entry cfg.blocks with None -> error "missing entry block" | Some _ ->
 let reachable = reachableLabels cfg in let predecessors = S.buildPredecessors cfg in let dominators = S.computeDominators cfg predecessors in
 let parameters = List.map (fun (param : typedMIRParam) -> param.reg, {block = cfg.entry; position = -1}) func.typedParams in
 let instructions = LabelMap.bindings cfg.blocks |> List.concat_map (fun (label, block) -> List.mapi (fun index instruction -> Option.map (fun reg -> reg, {block = label; position = (match instruction with Phi _ -> -1 | _ -> index)}) (F.getInstrDest instruction)) block.instrs |> List.filter_map Fun.id) in
 let definitions = parameters @ instructions in
 let counts = List.fold_left (fun counts (reg, _) -> VRegMap.add reg (1 + Option.value ~default:0 (VRegMap.find_opt reg counts)) counts) VRegMap.empty definitions in
 let duplicate = List.find_opt (fun (reg, _) -> VRegMap.find reg counts <> 1) definitions in
 match duplicate with Some (reg, _) -> error ("repeated definition of " ^ regText reg) | None ->
 let byRegister = VRegMap.of_list definitions in
 let checkUse label position reg = match VRegMap.find_opt reg byRegister with None -> error ("undefined use of " ^ regText reg ^ " in " ^ labelText label) | Some definition when definition.block = label && definition.position < position -> Ok () | Some definition when definition.block <> label && dominates dominators cfg.entry definition.block label -> Ok () | Some _ -> error (regText reg ^ " does not dominate its use in " ^ labelText label) in
 let checkOperand label position = function Register reg -> checkUse label position reg | _ -> Ok () in
 let checkBlock label block =
  let edges = List.map (fun target -> if LabelMap.mem target cfg.blocks then Ok () else error ("missing target " ^ labelText target ^ " from " ^ labelText label)) (S.getSuccessors block) in
  let instructions = List.mapi (fun index instruction -> match instruction with
   | Phi (_, sources, _) -> let expected = LabelSet.of_list (Option.value ~default:[] (LabelMap.find_opt label predecessors)) in let actual = LabelSet.of_list (List.map snd sources) in let valid = if LabelSet.equal actual expected && LabelSet.cardinal actual = List.length sources then Ok () else error ("phi edges disagree with predecessors of " ^ labelText label) in let sources = List.map (fun (operand, predecessor) -> checkOperand predecessor 2147483647 operand) sources in firstError (valid :: sources)
   | other -> F.foldInstrUses (fun uses reg -> reg :: uses) [] other |> List.map (checkUse label index) |> firstError) block.instrs in
  let terminator = F.foldTerminatorUses (fun uses reg -> reg :: uses) [] block.terminator |> List.map (checkUse label 2147483647) in firstError (edges @ instructions @ terminator) in
 LabelMap.bindings cfg.blocks |> List.filter (fun (label, _) -> LabelSet.mem label reachable) |> List.map (fun (label, block) -> checkBlock label block) |> firstError
