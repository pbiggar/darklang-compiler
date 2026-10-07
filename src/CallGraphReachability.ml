(*
   Provides deterministic reachability analysis for compiler passes that prune
   functions from different intermediate representations.
*)
(* CallGraphReachability.ml - Shared call graph traversal helpers *)
module S = SpecializationIdentity.FunctionSet

(*
   Compute the transitive closure of reachable nodes in a call graph.
*)
let findReachable callGraph roots =
  let rec visit reachable = function
    | [] -> reachable
    | name :: rest ->
        let calls =
          Option.value (FunctionIdMap.tryFind name callGraph) ~default:S.empty
        in
        let reachable, pending =
          S.fold
            (fun called (known, pending) ->
              if S.mem called known then (known, pending)
              else (S.add called known, called :: pending))
            calls (reachable, rest)
        in
        visit reachable pending
  in
  visit roots (S.elements roots)
