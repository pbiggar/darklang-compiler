// CallGraphReachability.fs - Shared call graph traversal helpers
//
// Provides deterministic reachability analysis for compiler passes that prune
// functions from different intermediate representations.

module CallGraphReachability

/// Compute the transitive closure of reachable nodes in a call graph.
let findReachable (callGraph: Map<'node, Set<'node>>) (roots: Set<'node>) : Set<'node> =
    let rec visit reachable toVisit =
        match toVisit with
        | [] -> reachable
        | name :: rest ->
            let calls = Map.tryFind name callGraph |> Option.defaultValue Set.empty
            let (reachable', toVisit') =
                calls
                |> Set.fold (fun (known, pending) calledName ->
                    if Set.contains calledName known then
                        (known, pending)
                    else
                        (Set.add calledName known, calledName :: pending)
                ) (reachable, rest)
            visit reachable' toVisit'

    visit roots (Set.toList roots)
