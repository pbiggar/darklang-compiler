// CallGraphSchedule.fs - Order exact MIR function nodes before their callers.

module CallGraphSchedule

let private required key values =
    Map.tryFind key values
    |> Option.defaultWith (fun () ->
        Crash.crash "Call graph schedule lost an internal node")

type Component = {
    NodeIndices: int list
    SCCs: MIR.Function list list
    Functions: MIR.Function list
}

let directCallees (func: MIR.Function) : Set<AST.FunctionId> =
    func.CFG.Blocks
    |> Map.fold (fun callees _ block ->
        block.Instrs
        |> List.fold (fun callees instr ->
            match instr with
            | MIR.Call (_, id, _, _, _)
            | MIR.TailCall (id, _, _, _) -> Set.add id callees
            | _ -> callees) callees) Set.empty

/// Every function receives a distinct graph node. A repeated canonical ID is
/// an unresolved edge: the scheduler cannot pick a body on the caller's behalf.
/// SCCs at equal dependency depth are batched to amortize stage setup.
let calleeFirst (functions: MIR.Function list) : Component list =
    let indexed = functions |> List.mapi (fun idx func -> idx, func)
    let byId =
        indexed
        |> List.groupBy (fun (_, func) -> func.Id)
        |> List.map (fun (id, entries) -> id, entries |> List.map fst)
        |> Map.ofList
    let graph =
        indexed
        |> List.map (fun (idx, func) ->
            let edges =
                directCallees func
                |> Set.toList
                |> List.choose (fun id ->
                    match Map.tryFind id byId with
                    | Some [unique] -> Some unique
                    | _ -> None)
                |> Set.ofList
            idx, edges)
        |> Map.ofList
    let reverse =
        graph
        |> Map.fold (fun reversed caller callees ->
            callees
            |> Set.fold (fun reversed callee ->
                let callers = Map.tryFind callee reversed |> Option.defaultValue Set.empty
                Map.add callee (Set.add caller callers) reversed) reversed)
            (indexed |> List.map (fun (idx, _) -> idx, Set.empty) |> Map.ofList)
    let rec finish visited order idx =
        if Set.contains idx visited then visited, order
        else
            let visited, order =
                required idx graph
                |> Set.fold (fun (visited, order) callee ->
                    finish visited order callee) (Set.add idx visited, order)
            visited, idx :: order
    let _, finishOrder =
        indexed
        |> List.fold (fun (visited, order) (idx, _) ->
            finish visited order idx) (Set.empty, [])
    let rec collect visited members idx =
        if Set.contains idx visited then visited, members
        else
            required idx reverse
            |> Set.fold (fun (visited, members) caller ->
                collect visited members caller)
                (Set.add idx visited, Set.add idx members)
    let _, components =
        finishOrder
        |> List.fold (fun (visited, components) idx ->
            if Set.contains idx visited then visited, components
            else
                let visited, members = collect visited Set.empty idx
                visited, members :: components) (Set.empty, [])
    let components = List.rev components
    let functionByIndex = indexed |> Map.ofList
    let componentByNode =
        components
        |> List.mapi (fun componentIdx members ->
            members |> Set.toList |> List.map (fun node -> node, componentIdx))
        |> List.concat
        |> Map.ofList
    let nodesByIndex = components |> List.mapi (fun idx members -> idx, members) |> Map.ofList
    let membersByIndex =
        components
        |> List.mapi (fun idx members ->
            idx,
            (members
             |> Set.toList
             |> List.map (fun node -> required node functionByIndex)))
        |> Map.ofList
    let edges =
        components
        |> List.mapi (fun idx members ->
            let callees =
                members
                |> Set.fold (fun acc node -> Set.union acc (required node graph)) Set.empty
                |> Set.toList
                |> List.map (fun node -> required node componentByNode)
                |> List.filter ((<>) idx)
                |> Set.ofList
            idx, callees)
        |> Map.ofList
    let rec visit seen ordered idx =
        if Set.contains idx seen then seen, ordered
        else
            let seen, ordered =
                required idx edges
                |> Set.fold (fun (seen, ordered) callee ->
                    visit seen ordered callee) (seen, ordered)
            Set.add idx seen, idx :: ordered
    let _, reverseOrder =
        [0 .. List.length components - 1]
        |> List.fold (fun (seen, ordered) idx ->
            visit seen ordered idx) (Set.empty, [])
    let ordered = List.rev reverseOrder
    let depths =
        ordered
        |> List.fold (fun depths idx ->
            let depth =
                required idx edges
                |> Set.fold (fun depth callee ->
                    max depth (1 + required callee depths)) 0
            Map.add idx depth depths) Map.empty
    ordered
    |> List.groupBy (fun idx -> required idx depths)
    |> List.sortBy fst
    |> List.collect (fun (_, layer) ->
        layer
        |> List.chunkBySize 512
        |> List.map (fun batch ->
            let sccs = batch |> List.map (fun idx -> required idx membersByIndex)
            { NodeIndices = batch |> List.collect (fun idx -> required idx nodesByIndex |> Set.toList)
              SCCs = sccs
              Functions = sccs |> List.concat }))
