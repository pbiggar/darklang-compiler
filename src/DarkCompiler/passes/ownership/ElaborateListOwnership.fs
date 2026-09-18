// ElaborateListOwnership.fs - Solve consuming uses and place branch-sensitive releases.

module ElaborateListOwnership

open HIR
open OwnedIR

open ListRegion
open ListLiveness

/// Solve backwards through explicit joins. A continuation's live values must
/// survive both paths; values needed by only one path die on the other edge.
let elaborateOwnership (StorageRegion (FunctionalRegion block, layouts)) : OwnedRegion =
    let rec elaborate (FunctionalBlock block) liveAfter : OwnedBlock * Set<ListId> =
        let operations, liveBefore =
            List.foldBack (fun operation (tail, live) ->
                let ownedOperation, releases, before =
                    match operation with
                    | Branch (result, condition, yes, no) ->
                        let managed (value: HIR.Value) =
                            if value.Type = AST.TList AST.TInt64 then Some value.Id else None
                        let continuation = managed result |> Option.map (fun id -> Set.remove id live) |> Option.defaultValue live
                        let branchLive (FunctionalBlock block) =
                            managed block.Result |> Option.map (fun id -> Set.add id continuation) |> Option.defaultValue continuation
                        let yes, yesLive = elaborate yes (branchLive yes)
                        let no, noLive = elaborate no (branchLive no)
                        let before = Set.union yesLive noLive
                        let edge (branch: OwnedBlock) required =
                            let drops = Set.difference before required |> Set.toList |> List.map Drop
                            { branch with Body = { branch.Body with Operations = drops @ branch.Body.Operations } }
                        let unusedResult =
                            managed result
                            |> Option.filter (fun id -> not (Set.contains id live))
                            |> Option.toList
                        Branch (result, condition, edge yes yesLive, edge no noLive), unusedResult, before
                    | _ ->
                        let unusedOutput =
                            match operation with
                            | Leaf leaf ->
                                primitiveContract leaf
                                |> HIR.managedOutputs
                                |> List.map (fun output -> output.Id)
                                |> List.filter (fun output -> not (Set.contains output live))
                            | Call call ->
                                if call.Result.Type = AST.TList AST.TInt64
                                   && not (Set.contains call.Result.Id live) then [call.Result.Id]
                                else []
                            | ScalarBinding _ -> []
                            | Branch _ -> Crash.crash "List HIR: branch handled before output accounting"
                        let owned, releases =
                            match operation with
                            | Leaf (Construct (output, construction)) -> Leaf (Construct (output, construction)), []
                            | Leaf (Transform (output, input, transform)) ->
                                let ownership = if Set.contains input.Id live then BorrowAndCopy else Consume
                                Leaf (Transform (output, input, (transform, ownership))), []
                            | Leaf (Fold (name, input, initial, callback)) ->
                                Leaf (Fold (name, input, initial, callback)), (if Set.contains input.Id live then [] else [input.Id])
                            | Call call -> Call call, []
                            | ScalarBinding (name, value) -> ScalarBinding (name, value), []
                            | Branch _ -> Crash.crash "List HIR: branch handled before leaf ownership"
                        owned, releases @ unusedOutput, ValueLiveness.liveBefore (valueContract operation) live
                Evaluate ownedOperation :: ((releases |> List.map Drop) @ tail), before)
                block.Operations ([], liveAfter)
        { Body = { Parameters = block.Parameters; Operations = operations; Result = block.Result } }, liveBefore
    let owned, _ = elaborate block Set.empty
    OwnedRegion (owned, layouts)
