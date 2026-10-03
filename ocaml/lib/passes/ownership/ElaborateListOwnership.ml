(* ElaborateListOwnership.fs - Solve consuming uses and place branch-sensitive releases. *)
[@@@warning "-4"]
module H = HIR
module O = OwnedIR
module L = ListRegion
module S = H.ValueSet
(*
   Solve backwards through explicit joins. A continuation's live values must
   survive both paths; values needed by only one path die on the other edge.
*)
let elaborateOwnership (L.StorageRegion (L.FunctionalRegion block, layouts)) =
 let rec elaborate (L.FunctionalBlock block) liveAfter : L.ownedBlock * S.t =
  let operations, liveBefore = List.fold_right (fun operation (tail, live) ->
   let owned, duplicates, releases, before = match operation with
   | H.Branch (result, condition, yes, no) ->
     let managed (value : H.value) = if value.H.typ = AST.TList AST.TInt64 then Some value.H.id else None in
     let continuation = match managed result with Some id -> S.remove id live | None -> live in
     let branchLive (L.FunctionalBlock block) = match managed block.H.result with Some id -> S.add id continuation | None -> continuation in
     let yes, yesLive = elaborate yes (branchLive yes) in let no, noLive = elaborate no (branchLive no) in let before = S.union yesLive noLive in
     let edge (branch : L.ownedBlock) required = let drops = S.elements (S.diff before required) |> List.map (fun id -> O.Drop id) in {O.body = {branch.O.body with H.operations = drops @ branch.O.body.H.operations}} in
     let unused = match managed result with Some id when not (S.mem id live) -> [id] | _ -> [] in
     H.Branch (result, condition, edge yes yesLive, edge no noLive), [], unused, before
   | _ ->
     let unused = match operation with H.Leaf leaf -> H.managedOutputs (L.primitiveContract leaf) |> List.map (fun (output : H.value) -> output.H.id) |> List.filter (fun output -> not (S.mem output live))
      | H.Call call -> if call.H.result.H.typ = AST.TList AST.TInt64 && not (S.mem call.H.result.H.id live) then [call.H.result.H.id] else []
      | H.ScalarBinding _ -> [] | H.Branch _ -> Crash.crash "List HIR: branch handled before output accounting" in
     let owned, duplicates, releases = match operation with
     | H.Leaf (L.Construct (output, construction)) -> H.Leaf (L.Construct (output, construction)), [], []
     | H.Leaf (L.Transform (output, input, (transform, reuse))) -> let survives = S.mem input.H.id live in
       let ownership, duplicates = match reuse, survives with L.StaticReuse, false -> L.Consume, [] | L.StaticReuse, true -> L.BorrowAndCopy, [] | L.RuntimeReuse, false -> L.ConsumeOrCopy, [] | L.RuntimeReuse, true -> L.ConsumeOrCopy, [input.H.id] in
       H.Leaf (L.Transform (output, input, (transform, ownership))), duplicates, []
     | H.Leaf (L.Fold (name, input, initial, callback)) -> H.Leaf (L.Fold (name, input, initial, callback)), [], (if S.mem input.H.id live then [] else [input.H.id])
     | H.Call call -> H.Call call, [], [] | H.ScalarBinding (name, value) -> H.ScalarBinding (name, value), [], []
     | H.Branch _ -> Crash.crash "List HIR: branch handled before leaf ownership" in
     owned, duplicates, releases @ unused, ListLiveness.Liveness.liveBefore (ListLiveness.valueContract operation) live in
   List.map (fun id -> O.Dup id) duplicates @ (O.Evaluate owned :: (List.map (fun id -> O.Drop id) releases @ tail)), before) block.H.operations ([], liveAfter) in
  {O.body = {H.parameters = block.H.parameters; operations; result = block.H.result}}, liveBefore in
 let owned, _ = elaborate block S.empty in L.OwnedRegion (owned, layouts)
