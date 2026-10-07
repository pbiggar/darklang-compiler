(* CallGraphSchedule.ml - Order exact MIR function nodes before their callers. *)
[@@@warning "-4"]
module Set = MIR.IntSet
module M = ANFConstants.IntMap
module Functions = SpecializationIdentity.FunctionSet
type component = {nodeIndices : int list; sccs : MIR.functionDef list list; functions : MIR.functionDef list}
let required key values = match M.find_opt key values with Some value -> value | None -> Crash.crash "Call graph schedule lost an internal node"
let directCallees (func : MIR.functionDef) = MIR.LabelMap.fold (fun _ block callees -> List.fold_left (fun callees instruction -> match instruction with MIR.Call (_, id, _, _, _) | MIR.TailCall (id, _, _, _) -> Functions.add id callees | _ -> callees) callees block.MIR.instrs) func.MIR.cfg.MIR.blocks Functions.empty
(*
   Every function receives a distinct graph node. A repeated canonical ID is
   an unresolved edge: the scheduler cannot pick a body on the caller's behalf.
   SCCs at equal dependency depth are batched to amortize stage setup.
   Nodes without incoming edges still own an empty row. Fill the
   consecutive domain once, without copying an array for every edge.
*)
let calleeFirst functions =
 let indexed = List.mapi (fun index func -> index, func) functions in
 let functionByIndex = Array.of_list functions in
 let byId = List.fold_left (fun grouped (index, (func : MIR.functionDef)) -> FunctionIdMap.add func.MIR.id (index :: Option.value ~default:[] (FunctionIdMap.tryFind func.MIR.id grouped)) grouped) FunctionIdMap.empty indexed in
 let graph = Array.map (fun func -> Functions.elements (directCallees func) |> List.filter_map (fun id -> match FunctionIdMap.tryFind id byId with Some [unique] -> Some unique | _ -> None) |> Set.of_list) functionByIndex in
 let callersByNode = Array.to_list (Array.mapi (fun caller callees -> Set.elements callees |> List.map (fun callee -> callee, caller)) graph) |> List.concat |> List.fold_left (fun grouped (callee, caller) -> M.add callee (Set.add caller (Option.value ~default:Set.empty (M.find_opt callee grouped))) grouped) M.empty in
 let reverse = Array.init (Array.length graph) (fun node -> Option.value ~default:Set.empty (M.find_opt node callersByNode)) in
 let rec finish visited order index = if Set.mem index visited then visited, order else let visited, order = Set.fold (fun callee (visited, order) -> finish visited order callee) graph.(index) (Set.add index visited, order) in visited, index :: order in
 let _, finishOrder = List.fold_left (fun (visited, order) (index, _) -> finish visited order index) (Set.empty, []) indexed in
 let rec collect visited members index = if Set.mem index visited then visited, members else Set.fold (fun caller (visited, members) -> collect visited members caller) reverse.(index) (Set.add index visited, Set.add index members) in
 let _, components = List.fold_left (fun (visited, components) index -> if Set.mem index visited then visited, components else let visited, members = collect visited Set.empty index in visited, members :: components) (Set.empty, []) finishOrder in
 let nodesByIndex = Array.of_list (List.rev components) in
 let componentByNode = Array.to_list (Array.mapi (fun index members -> Set.elements members |> List.map (fun node -> node, index)) nodesByIndex) |> List.concat |> List.sort (fun (left, _) (right, _) -> Int.compare left right) |> List.map snd |> Array.of_list in
 let membersByIndex = Array.map (fun members -> List.map (Array.get functionByIndex) (Set.elements members)) nodesByIndex in
 let edges = Array.mapi (fun index members -> Set.fold (fun node edges -> Set.union edges graph.(node)) members Set.empty |> Set.elements |> List.map (Array.get componentByNode) |> List.filter ((<>) index) |> Set.of_list) nodesByIndex in
 let rec visit seen ordered index = if Set.mem index seen then seen, ordered else let seen, ordered = Set.fold (fun callee (seen, ordered) -> visit seen ordered callee) edges.(index) (seen, ordered) in Set.add index seen, index :: ordered in
 let _, reverseOrder = List.fold_left (fun (seen, ordered) index -> visit seen ordered index) (Set.empty, []) (List.init (Array.length nodesByIndex) Fun.id) in
 let ordered = List.rev reverseOrder in
 let depths = List.fold_left (fun depths index -> let depth = Set.fold (fun callee depth -> max depth (Int32.to_int (Int32.add 1l (Int32.of_int (required callee depths))))) edges.(index) 0 in M.add index depth depths) M.empty ordered in
 let grouped = List.fold_left (fun grouped index -> let depth = required index depths in M.add depth (index :: Option.value ~default:[] (M.find_opt depth grouped)) grouped) M.empty ordered in
 let rec chunks values = match values with [] -> [] | _ -> let rec take count reversed values = match count, values with 0, _ | _, [] -> List.rev reversed, values | _, head :: tail -> take (count - 1) (head :: reversed) tail in let first, rest = take 512 [] values in first :: chunks rest in
 M.bindings grouped |> List.concat_map (fun (_, layer) -> chunks (List.rev layer) |> List.map (fun batch -> let sccs = List.map (Array.get membersByIndex) batch in {nodeIndices = List.concat_map (fun index -> Set.elements nodesByIndex.(index)) batch; sccs; functions = List.concat sccs}))
