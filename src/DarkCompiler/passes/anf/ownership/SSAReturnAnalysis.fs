// SSAReturnAnalysis.fs - Track values that flow to a return across SSA block edges.

module RcSSAReturnAnalysis

open ANF

type Facts = {
    AtEntry: Map<SSAANF.Label, Set<TempId>>
    AtTerminator: Map<SSAANF.Label, Set<TempId>>
    AfterDefinition: Map<TempId, Set<TempId>>
}

let private returnedAtom atom =
    match atom with
    | Var id -> Set.singleton id
    | _ -> Set.empty

let private returnedAcrossEdge
    (target: SSAANF.Block)
    (arguments: Atom list)
    (returned: Set<TempId>)
    : Set<TempId> =
    if List.length target.Parameters <> List.length arguments then
        Crash.crash "SSA return analysis: edge argument count does not match block parameters"
    let parameterIds = target.Parameters |> List.map (fun parameter -> parameter.Id) |> Set.ofList
    let edgeValues =
        List.zip target.Parameters arguments
        |> List.fold (fun values (parameter, argument) ->
            if Set.contains parameter.Id returned then
                Set.union values (returnedAtom argument)
            else values) Set.empty
    Set.union (Set.difference returned parameterIds) edgeValues

let private returnedAtTerminator
    (blocks: Map<SSAANF.Label, SSAANF.Block>)
    (known: Map<SSAANF.Label, Set<TempId>>)
    (block: SSAANF.Block)
    : Set<TempId> =
    let incoming label arguments =
        match Map.tryFind label blocks with
        | None -> Crash.crash "SSA return analysis: missing successor block"
        | Some target ->
            let returned = Map.tryFind label known |> Option.defaultValue Set.empty
            returnedAcrossEdge target arguments returned
    match block.Terminator with
    | SSAANF.Return atom -> returnedAtom atom
    | SSAANF.Jump (target, arguments) -> incoming target arguments
    | SSAANF.Branch (_, yes, no) ->
        Set.union (incoming yes []) (incoming no [])

let private beforeDefinition
    (returned: Set<TempId>)
    (id: TempId, operation: CExpr)
    : Set<TempId> =
    let source =
        if Set.contains id returned then
            RcReturnAnalysis.tryOwnershipPreservingAliasSource operation
            |> Option.map Set.singleton
            |> Option.defaultValue Set.empty
        else
            Set.empty
    Set.union (Set.remove id returned) source

let private factsForBlock
    (blocks: Map<SSAANF.Label, SSAANF.Block>)
    (known: Map<SSAANF.Label, Set<TempId>>)
    (block: SSAANF.Block)
    : Set<TempId> * Set<TempId> =
    let atTerminator = returnedAtTerminator blocks known block
    let atEntry = List.foldBack (fun definition returned -> beforeDefinition returned definition)
                      block.Operations atTerminator
    atEntry, atTerminator

/// A backwards fixed point is required for joins with backedges. Facts at an
/// edge refer to the source value, while facts inside a successor refer to its
/// block parameter. Only ownership-preserving aliases inherit return status.
let analyze (func: SSAANF.Function) : Facts =
    let rec settle known =
        let next =
            func.Blocks
            |> Map.map (fun _ block -> factsForBlock func.Blocks known block |> fst)
        if next = known then known else settle next
    let atEntry = settle Map.empty
    let atTerminator =
        func.Blocks
        |> Map.map (fun _ block -> returnedAtTerminator func.Blocks atEntry block)
    let afterDefinition =
        func.Blocks
        |> Map.fold (fun acc label block ->
            let start = Map.tryFind label atTerminator |> Option.defaultValue Set.empty
            let _, updates =
                List.foldBack (fun ((id, _) as definition) (returned, updates) ->
                    let updates = Map.add id returned updates
                    beforeDefinition returned definition, updates)
                    block.Operations
                    (start, Map.empty)
            Map.fold (fun state id returned -> Map.add id returned state) acc updates)
            Map.empty
    { AtEntry = atEntry; AtTerminator = atTerminator; AfterDefinition = afterDefinition }
