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
    let functionByIndex = functions |> List.toArray
    let byId =
        indexed
        |> List.groupBy (fun (_, func) -> func.Id)
        |> List.map (fun (id, entries) -> id, entries |> List.map fst)
        |> Map.ofList
    let graph =
        functionByIndex
        |> Array.map (fun func ->
            directCallees func
            |> Set.toList
            |> List.choose (fun id ->
                match Map.tryFind id byId with
                | Some [unique] -> Some unique
                | _ -> None)
            |> Set.ofList)
    let reverse =
        let callersByNode =
            graph
            |> Array.mapi (fun caller callees ->
                callees |> Set.toArray |> Array.map (fun callee -> callee, caller))
            |> Array.concat
            |> Array.groupBy fst
            |> Array.sortBy fst
            |> Array.toList
        // Nodes without incoming edges still own an empty row. Fill the
        // consecutive domain once, without copying an array for every edge.
        Array.unfold (fun (node, remaining) ->
            if node = graph.Length then None
            else
                match remaining with
                | (callee, callers) :: rest when callee = node ->
                    Some (callers |> Array.map snd |> Set.ofArray, (node + 1, rest))
                | _ -> Some (Set.empty, (node + 1, remaining))) (0, callersByNode)
    let rec finish visited order idx =
        if Set.contains idx visited then visited, order
        else
            let visited, order =
                graph.[idx]
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
            reverse.[idx]
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
    let nodesByIndex = components |> List.rev |> List.toArray
    let componentByNode =
        nodesByIndex
        |> Array.mapi (fun componentIdx members ->
            members |> Set.toArray |> Array.map (fun node -> node, componentIdx))
        |> Array.concat
        |> Array.sortBy fst
        |> Array.map snd
    let membersByIndex =
        nodesByIndex
        |> Array.map (fun members ->
            members
            |> Set.toList
            |> List.map (fun node -> functionByIndex.[node]))
    let edges =
        nodesByIndex
        |> Array.mapi (fun idx members ->
            members
            |> Set.fold (fun acc node -> Set.union acc graph.[node]) Set.empty
            |> Set.toList
            |> List.map (fun node -> componentByNode.[node])
            |> List.filter ((<>) idx)
            |> Set.ofList)
    let rec visit seen ordered idx =
        if Set.contains idx seen then seen, ordered
        else
            let seen, ordered =
                edges.[idx]
                |> Set.fold (fun (seen, ordered) callee ->
                    visit seen ordered callee) (seen, ordered)
            Set.add idx seen, idx :: ordered
    let _, reverseOrder =
        [0 .. nodesByIndex.Length - 1]
        |> List.fold (fun (seen, ordered) idx ->
            visit seen ordered idx) (Set.empty, [])
    let ordered = List.rev reverseOrder
    let depths =
        ordered
        |> List.fold (fun depths idx ->
            let depth =
                edges.[idx]
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
            let sccs = batch |> List.map (fun idx -> membersByIndex.[idx])
            { NodeIndices = batch |> List.collect (fun idx -> nodesByIndex.[idx] |> Set.toList)
              SCCs = sccs
              Functions = sccs |> List.concat }))
