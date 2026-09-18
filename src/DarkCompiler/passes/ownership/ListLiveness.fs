// ListLiveness.fs - Representation-independent collection liveness and entry verification.

module ListLiveness

open HIR

open ListRegion

/// Region aliases are canonical identities. Opaque scalar evaluations cannot
/// access them; callbacks cannot capture them. These are value-edge contracts,
/// not permission to reorder scalar effects or to mutate a borrowed parameter.
let rec internal valueContract operation : ValueLiveness.Contract<ListId> =
    let managed (value: HIR.Value) =
        if value.Type = AST.TList AST.TInt64 then Set.singleton value.Id else Set.empty
    let uses, defines =
        match operation with
        | Branch (output, _, yes, no) ->
            let branchUses (FunctionalBlock body as block) = entryLive block (managed body.Result)
            Set.union (branchUses yes) (branchUses no), managed output
        | Leaf leaf ->
            let contract = primitiveContract leaf
            contract.Inputs |> List.map (fun value -> value.Id) |> Set.ofList,
            HIR.managedOutputs contract |> List.map (fun value -> value.Id) |> Set.ofList
        | Call call ->
            call.Arguments |> List.collect (managed >> Set.toList) |> Set.ofList,
            managed call.Result
        | ScalarBinding _ -> Set.empty, Set.empty
    { Uses = uses; Defines = defines }
and private entryLive (FunctionalBlock block) liveAfter =
    List.foldBack (fun operation live -> ValueLiveness.liveBefore (valueContract operation) live) block.Operations liveAfter

/// No physical ownership is needed to check the region's incoming value
/// interface. The current representation permits no external collection roots.
let verifyFunctional (FunctionalRegion block) : Result<unit, string> =
    let dialect : VerifyHIR.Dialect<Operation<Transform>, FunctionalBlock> = {
        Body = fun (FunctionalBlock block) -> block
        Leaf = primitiveContract
        CallSignature = fun _ -> None
        CallContract = fun _ -> None
    }
    VerifyHIR.verify dialect block
    |> Result.mapError (fun error -> $"List HIR: {error}")
    |> Result.bind (fun () ->
        if Set.isEmpty (entryLive block Set.empty) then Ok ()
        else Error "List HIR: external collection roots in a closed region")
