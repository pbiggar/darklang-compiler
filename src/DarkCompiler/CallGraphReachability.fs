// CallGraphReachability.fs - Shared call graph traversal helpers
//
// Provides deterministic reachability analysis for compiler passes that prune
// functions from different intermediate representations.

module CallGraphReachability

/// Compute the transitive closure of reachable nodes in a call graph.
let findReachable (callGraph: Map<string, Set<string>>) (roots: Set<string>) : Set<string> =
    let rec visit visited toVisit =
        if Set.isEmpty toVisit then
            visited
        else
            let name = Set.minElement toVisit
            let toVisit' = Set.remove name toVisit
            if Set.contains name visited then
                visit visited toVisit'
            else
                let visited' = Set.add name visited
                let calls = Map.tryFind name callGraph |> Option.defaultValue Set.empty
                let toVisit'' = Set.union toVisit' (Set.difference calls visited')
                visit visited' toVisit''

    visit Set.empty roots
