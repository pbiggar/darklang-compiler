(* OwnedFunctionGroups.fs - Discover deterministic call groups in owned HIR. *)
[@@@warning "-4"]
module O = OwnedIR
module F = FunctionIdMap
module S = SpecializationIdentity.FunctionSet
module Numeric = struct type t = int64 let compare = Int64.unsigned_compare end
module NMap = Map.Make (Numeric)
module NSet = Set.Make (Numeric)
module IMap = Map.Make (Int)
module ISet = Set.Make (Int)
type ('leaf, 'id) group = Group of ('leaf, 'id) O.functionDef * ('leaf, 'id) O.functionDef list * bool * S.t * S.t
type groupingError = DuplicateFunctionName of AST.functionId
let functions (Group (head, tail, _, _, _)) = head :: tail
let isRecursive (Group (_, _, recursive, _, _)) = recursive
let internalDependencies (Group (_, _, _, dependencies, _)) = dependencies
let externalTargets (Group (_, _, _, _, targets)) = targets
let rec blockCalls (block : ('leaf, 'id) O.block) =
 List.fold_left (fun calls -> function
 | O.Evaluate (HIR.Call call) -> S.add call.HIR.target calls
 | O.Evaluate (HIR.Branch (_, _, yes, no)) -> let yes = blockCalls yes in let no = blockCalls no in S.union calls (S.union yes no)
 | O.Evaluate (HIR.Leaf _ | HIR.ScalarBinding _) | O.Dup _ | O.Drop _ -> calls) S.empty block.O.body.HIR.operations
type dfsFrame = Enter of int64 | Exit of int64
let adjacent adjacency name = match NMap.find_opt name adjacency with Some targets -> targets | None -> Crash.crash "Owned function call graph lost a definition"
let prependInOrder wrap values tail = List.fold_left (fun pending value -> wrap value :: pending) tail (List.rev values)
let finishOrder vertices adjacency =
 let rec visit pending visited finished = match pending with
 | [] -> visited, finished
 | Enter name :: rest when NSet.mem name visited -> visit rest visited finished
 | Enter name :: rest -> let pending = prependInOrder (fun name -> Enter name) (adjacent adjacency name) (Exit name :: rest) in visit pending (NSet.add name visited) finished
 | Exit name :: rest -> visit rest visited (name :: finished) in
 List.fold_left (fun (visited, finished) name -> if NSet.mem name visited then visited, finished else visit [Enter name] visited finished) (NSet.empty, []) vertices |> snd
let reverseAdjacency vertices sourceIndex adjacency =
 let empty = NMap.of_list (List.map (fun name -> name, []) vertices) in
 let reversed = NMap.fold (fun caller targets reversed -> List.fold_left (fun reversed callee -> match NMap.find_opt callee reversed with
  | Some callers -> NMap.add callee (caller :: callers) reversed
  | None -> Crash.crash "Owned function reverse call graph lost a definition") reversed targets) adjacency empty in
 NMap.map (List.stable_sort (fun first second -> Int.compare (sourceIndex first) (sourceIndex second))) reversed
let stronglyConnectedComponents vertices sourceIndex adjacency =
 let reverse = reverseAdjacency vertices sourceIndex adjacency in
 let rec collect pending visited members = match pending with
 | [] -> visited, members
 | name :: rest when NSet.mem name visited -> collect rest visited members
 | name :: rest -> let pending = prependInOrder Fun.id (adjacent reverse name) rest in collect pending (NSet.add name visited) (NSet.add name members) in
 let rec partition remaining visited components = match remaining with
 | [] -> List.rev components
 | name :: rest when NSet.mem name visited -> partition rest visited components
 | name :: rest -> let visited, memberNames = collect [name] visited NSet.empty in partition rest visited (memberNames :: components) in
 partition (finishOrder vertices adjacency) NSet.empty []
type nameComponent = {index : int; members : int64 list; dependencies : ISet.t}
let orderedNames vertices adjacency =
 let vertexNames = NSet.of_list vertices in
 if NSet.cardinal vertexNames <> List.length vertices then Crash.crash "Function call graph contains duplicate definitions";
 let sourcePositions = NMap.of_list (List.mapi (fun index name -> name, index) vertices) in
 let sourceIndex name = match NMap.find_opt name sourcePositions with Some index -> index | None -> Crash.crash "Function call graph lost its source position" in
 let sortNames = List.stable_sort (fun first second -> Int.compare (sourceIndex first) (sourceIndex second)) in
 let graph = NMap.of_list (List.map (fun name ->
  let targets = Option.value ~default:[] (NMap.find_opt name adjacency) in
  name, sortNames (NSet.elements (NSet.of_list (List.filter (fun target -> NSet.mem target vertexNames) targets)))) vertices) in
 let discovered = stronglyConnectedComponents vertices sourceIndex graph |> List.map (fun names ->
  let members = sortNames (NSet.elements names) in match members with
  | head :: _ -> sourceIndex head, members, names | [] -> Crash.crash "Function SCC discovery returned an empty component")
  |> List.stable_sort (fun (first, _, _) (second, _, _) -> Int.compare first second) in
 let ownerByName = NMap.of_list (List.concat_map (fun (index, _, names) -> List.map (fun name -> name, index) (NSet.elements names)) discovered) in
 let components = List.map (fun (index, members, _) ->
  let dependencies = List.fold_left (fun dependencies target -> match NMap.find_opt target ownerByName with
   | Some dependency when dependency <> index -> ISet.add dependency dependencies
   | Some _ -> dependencies | None -> Crash.crash "Function dependency lost its component") ISet.empty (List.concat_map (adjacent graph) members) in
  {index; members; dependencies}) discovered in
 let byIndex = IMap.of_list (List.map (fun groupInfo -> groupInfo.index, groupInfo) components) in
 let dependents = List.fold_left (fun dependents caller -> ISet.fold (fun dependency dependents -> match IMap.find_opt dependency dependents with
  | Some callers -> IMap.add dependency (ISet.add caller.index callers) dependents
  | None -> Crash.crash "Function dependency target lost its component") caller.dependencies dependents)
  (IMap.of_list (List.map (fun groupInfo -> groupInfo.index, ISet.empty) components)) components in
 let unresolved = IMap.of_list (List.map (fun groupInfo -> groupInfo.index, ISet.cardinal groupInfo.dependencies) components) in
 let ready = IMap.fold (fun index count ready -> if count = 0 then ISet.add index ready else ready) unresolved ISet.empty in
 let rec order remaining unresolved ready ordered =
  if remaining = 0 then List.rev ordered else match ISet.min_elt_opt ready with
  | None -> Crash.crash "Function SCC condensation graph contains a cycle"
  | Some selectedIndex ->
    let selected = match IMap.find_opt selectedIndex byIndex with Some groupInfo -> groupInfo | None -> Crash.crash "Function ordering lost a ready component" in
    let selectedDependents = match IMap.find_opt selectedIndex dependents with Some values -> values | None -> Crash.crash "Function ordering lost dependent components" in
    let unresolved, ready = ISet.fold (fun dependent (unresolved, ready) -> match IMap.find_opt dependent unresolved with
     | Some count when count > 0 -> let next = count - 1 in let ready = if next = 0 then ISet.add dependent ready else ready in IMap.add dependent next unresolved, ready
     | _ -> Crash.crash "Function dependency count became invalid") selectedDependents (unresolved, ISet.remove selectedIndex ready) in
    order (remaining - 1) unresolved ready (selected.members :: ordered) in
 order (List.length components) unresolved ready []
(*
   Graph operations use the numeric value carried by each identity.
   This avoids boxing FunctionId structs at every map and set comparison.
   SCC members are returned callee-first with independent components in source order.
*)
let orderedFunctionIds vertices adjacency =
 let identityByName = NMap.of_list (List.map (fun id -> AST.functionIdValue id, id) vertices) in
 let namedAdjacency = NMap.of_list (List.map (fun (id, targets) -> AST.functionIdValue id, List.map AST.functionIdValue targets) (F.toList adjacency)) in
 orderedNames (List.map AST.functionIdValue vertices) namedAdjacency |> List.map (List.map (fun name -> match NMap.find_opt name identityByName with
 | Some id -> id | None -> Crash.crash "Function SCC lost its canonical identity"))
let groups definitions names callsByFunction adjacency =
 let definitionsByName = F.ofList (List.map (fun (definition : ('leaf, 'id) O.functionDef) -> definition.O.definition.HIR.id, definition) definitions) in
 orderedFunctionIds (List.map (fun (definition : ('leaf, 'id) O.functionDef) -> definition.O.definition.HIR.id) definitions) adjacency |> List.map (fun memberNames ->
  let componentNames = S.of_list memberNames in
  let members = List.map (fun name -> match F.tryFind name definitionsByName with Some definition -> definition | None -> Crash.crash "Owned function SCC lost its definition") memberNames in
  let calls = List.fold_left (fun calls (definition : ('leaf, 'id) O.functionDef) -> match F.tryFind definition.O.definition.HIR.id callsByFunction with
   | Some targets -> S.union calls targets | None -> Crash.crash "Owned function SCC lost its call set") S.empty members in
  match members with
  | memberHead :: memberTail -> let headName = memberHead.O.definition.HIR.id in
    Group (memberHead, memberTail, S.cardinal componentNames > 1 || S.mem headName calls, S.diff (S.inter calls names) componentNames, S.diff calls names)
  | [] -> Crash.crash "Owned function SCC partition produced an empty component")
(*
   Partition mutually visible owned functions into call-graph SCCs. Groups are
   callee-first; independent groups retain the source order of their earliest
   definition. Calls outside the supplied definitions remain external targets.
*)
let discover (definitions : ('leaf, 'id) O.functionDef list) =
 let counts = List.fold_left (fun counts (definition : ('leaf, 'id) O.functionDef) -> F.change definition.O.definition.HIR.id (fun count -> Some (1 + Option.value ~default:0 count)) counts) F.empty definitions in
 match List.find_opt (fun (definition : ('leaf, 'id) O.functionDef) -> F.find definition.O.definition.HIR.id counts > 1) definitions with
 | Some definition -> Error (DuplicateFunctionName definition.O.definition.HIR.id)
 | None ->
   let names = S.of_list (List.map (fun (definition : ('leaf, 'id) O.functionDef) -> definition.O.definition.HIR.id) definitions) in
   let callsByFunction = F.ofList (List.map (fun (definition : ('leaf, 'id) O.functionDef) -> definition.O.definition.HIR.id, blockCalls definition.O.definition.HIR.body) definitions) in
   let adjacency = F.map (fun _ calls -> S.elements calls) callsByFunction in
   Ok (groups definitions names callsByFunction adjacency)
