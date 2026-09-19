// OwnedFunctionGroups.fs - Discover deterministic call groups in owned HIR.

module OwnedFunctionGroups

open OwnedIR

type Group<'leaf, 'id> =
    private
    | Group of
        head: Function<'leaf, 'id> *
        tail: Function<'leaf, 'id> list *
        recursive: bool *
        internalDependencies: Set<string> *
        externalTargets: Set<string>

type GroupingError =
    | DuplicateFunctionName of string

let functions (Group (head, tail, _, _, _)) = head :: tail
let isRecursive (Group (_, _, recursive, _, _)) = recursive
let internalDependencies (Group (_, _, _, dependencies, _)) = dependencies
let externalTargets (Group (_, _, _, _, targets)) = targets

let rec private blockCalls (block: Block<'leaf, 'id>) =
    block.Body.Operations
    |> List.fold (fun calls step ->
        match step with
        | Evaluate (HIR.Call call) -> Set.add call.Target calls
        | Evaluate (HIR.Branch (_, _, ifTrue, ifFalse)) ->
            Set.unionMany [calls; blockCalls ifTrue; blockCalls ifFalse]
        | Evaluate (HIR.Leaf _ | HIR.ScalarBinding _)
        | Dup _
        | Drop _ -> calls) Set.empty

let private reachable adjacency start =
    let rec visit pending visited =
        match pending with
        | [] -> visited
        | name :: rest when Set.contains name visited -> visit rest visited
        | name :: rest ->
            let next =
                match Map.tryFind name adjacency with
                | Some targets -> Set.toList targets @ rest
                | None -> rest
            visit next (Set.add name visited)
    visit [start] Set.empty

type private Component<'leaf, 'id> = {
    Index: int
    Head: Function<'leaf, 'id>
    Tail: Function<'leaf, 'id> list
    Names: Set<string>
    InternalDependencies: Set<string>
    ExternalTargets: Set<string>
    Recursive: bool
}

let private components definitions names callsByFunction adjacency =
    let rec partition index remaining =
        match remaining with
        | [] -> []
        | head :: _ ->
            let headName = head.Definition.Name
            let headReachable = reachable adjacency headName
            let componentNames =
                remaining
                |> List.choose (fun definition ->
                    let name = definition.Definition.Name
                    if Set.contains name headReachable
                       && Set.contains headName (reachable adjacency name) then
                        Some name
                    else None)
                |> Set.ofList
            let members, rest =
                remaining
                |> List.partition (fun definition ->
                    Set.contains definition.Definition.Name componentNames)
            let calls =
                members
                |> List.map (fun definition ->
                    match Map.tryFind definition.Definition.Name callsByFunction with
                    | Some targets -> targets
                    | None -> Set.empty)
                |> Set.unionMany
            let recursive =
                Set.count componentNames > 1 || Set.contains headName calls
            let internalDependencies =
                Set.difference (Set.intersect calls names) componentNames
            let externalTargets = Set.difference calls names
            match members with
            | memberHead :: memberTail ->
                {
                    Index = index
                    Head = memberHead
                    Tail = memberTail
                    Names = componentNames
                    InternalDependencies = internalDependencies
                    ExternalTargets = externalTargets
                    Recursive = recursive
                }
                :: partition (index + 1) rest
            | [] -> Crash.crash "Owned function SCC partition produced an empty component"
    partition 0 definitions

let private orderComponents components =
    let rec order remaining ordered =
        match remaining with
        | [] -> List.rev ordered
        | _ ->
            let remainingNames =
                remaining
                |> List.map (fun groupInfo -> groupInfo.Names)
                |> Set.unionMany
            match
                remaining
                |> List.tryFind (fun groupInfo ->
                    Set.intersect groupInfo.InternalDependencies remainingNames
                    |> Set.isEmpty)
            with
            | Some selected ->
                let rest =
                    remaining
                    |> List.filter (fun groupInfo -> groupInfo.Index <> selected.Index)
                order rest (selected :: ordered)
            | None -> Crash.crash "Owned function SCC condensation graph contains a cycle"
    order components []

/// Partition mutually visible owned functions into call-graph SCCs. Groups are
/// callee-first; independent groups retain the source order of their earliest
/// definition. Calls outside the supplied definitions remain external targets.
let discover
    (definitions: Function<'leaf, 'id> list)
    : Result<Group<'leaf, 'id> list, GroupingError> =
    match
        definitions
        |> List.countBy (fun definition -> definition.Definition.Name)
        |> List.tryFind (fun (_, count) -> count > 1)
    with
    | Some (name, _) -> Error (DuplicateFunctionName name)
    | None ->
        let names =
            definitions
            |> List.map (fun definition -> definition.Definition.Name)
            |> Set.ofList
        let callsByFunction =
            definitions
            |> List.map (fun definition ->
                definition.Definition.Name,
                blockCalls definition.Definition.Body)
            |> Map.ofList
        let adjacency = callsByFunction |> Map.map (fun _ calls -> Set.intersect names calls)
        components definitions names callsByFunction adjacency
        |> orderComponents
        |> List.map (fun groupInfo ->
            Group (
                groupInfo.Head,
                groupInfo.Tail,
                groupInfo.Recursive,
                groupInfo.InternalDependencies,
                groupInfo.ExternalTargets))
        |> Ok
