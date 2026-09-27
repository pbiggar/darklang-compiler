// SSAValueLiveness.fs - Compute operation and edge liveness for SSA ownership cleanup.

module RcSSAValueLiveness

open ANF

type Facts = {
    AtEntry: Map<SSAANF.Label, Set<TempId>>
    AtTerminator: Map<SSAANF.Label, Set<TempId>>
    AfterDefinition: Map<TempId, Set<TempId>>
}

let private atomUses atom =
    match atom with
    | Var id -> Set.singleton id
    | _ -> Set.empty

let private liveAcrossEdge
    (target: SSAANF.Block)
    (arguments: Atom list)
    (live: Set<TempId>)
    : Set<TempId> =
    if List.length target.Parameters <> List.length arguments then
        Crash.crash "SSA liveness: edge argument count does not match block parameters"
    let parameterIds = target.Parameters |> List.map (fun parameter -> parameter.Id) |> Set.ofList
    let edgeValues =
        List.zip target.Parameters arguments
        |> List.fold (fun values (parameter, argument) ->
            if Set.contains parameter.Id live then
                Set.union values (atomUses argument)
            else values) Set.empty
    Set.union (Set.difference live parameterIds) edgeValues

let private liveAtTerminator
    (blocks: Map<SSAANF.Label, SSAANF.Block>)
    (known: Map<SSAANF.Label, Set<TempId>>)
    (block: SSAANF.Block)
    : Set<TempId> =
    let incoming label arguments =
        match Map.tryFind label blocks with
        | None -> Crash.crash "SSA liveness: missing successor block"
        | Some target ->
            let live = Map.tryFind label known |> Option.defaultValue Set.empty
            liveAcrossEdge target arguments live
    match block.Terminator with
    | SSAANF.Return atom -> atomUses atom
    | SSAANF.Jump (target, arguments) -> incoming target arguments
    | SSAANF.Branch (condition, yes, no) ->
        Set.unionMany [atomUses condition; incoming yes []; incoming no []]

let private beforeDefinition
    (live: Set<TempId>)
    (id: TempId, operation: CExpr)
    : Set<TempId> =
    Set.union (Set.remove id live) (ANFEffects.cexprTempUses operation)

let analyze (func: SSAANF.Function) : Facts =
    let rec settle known =
        let next =
            func.Blocks
            |> Map.map (fun _ block ->
                let atTerminator = liveAtTerminator func.Blocks known block
                List.foldBack (fun definition live -> beforeDefinition live definition)
                    block.Operations atTerminator)
        if next = known then known else settle next
    let atEntry = settle Map.empty
    let atTerminator =
        func.Blocks
        |> Map.map (fun _ block -> liveAtTerminator func.Blocks atEntry block)
    let afterDefinition =
        func.Blocks
        |> Map.fold (fun acc label block ->
            let start = Map.tryFind label atTerminator |> Option.defaultValue Set.empty
            let _, updates =
                List.foldBack (fun ((id, _) as definition) (live, updates) ->
                    let updates = Map.add id live updates
                    beforeDefinition live definition, updates)
                    block.Operations
                    (start, Map.empty)
            Map.fold (fun state id live -> Map.add id live state) acc updates)
            Map.empty
    { AtEntry = atEntry; AtTerminator = atTerminator; AfterDefinition = afterDefinition }
