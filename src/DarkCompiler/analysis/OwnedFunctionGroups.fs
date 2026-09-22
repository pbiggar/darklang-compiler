// OwnedFunctionGroups.fs - Discover deterministic call groups in owned HIR.

module OwnedFunctionGroups

open OwnedIR

type Group<'leaf, 'id> =
    private
    | Group of
        head: Function<'leaf, 'id> *
        tail: Function<'leaf, 'id> list *
        recursive: bool *
        internalDependencies: Set<AST.FunctionId> *
        externalTargets: Set<AST.FunctionId>

type GroupingError =
    | DuplicateFunctionName of AST.FunctionId

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

type private DfsFrame =
    | Enter of AST.FunctionId
    | Exit of AST.FunctionId

let private adjacent adjacency name =
    match Map.tryFind name adjacency with
    | Some targets -> targets
    | None -> Crash.crash "Owned function call graph lost a definition"

let private prependInOrder wrap values tail =
    values
    |> List.rev
    |> List.fold (fun pending value -> wrap value :: pending) tail

let private finishOrder vertices adjacency =
    let rec visit pending visited finished =
        match pending with
        | [] -> visited, finished
        | Enter name :: rest when Set.contains name visited ->
            visit rest visited finished
        | Enter name :: rest ->
            let pending =
                prependInOrder Enter (adjacent adjacency name) (Exit name :: rest)
            visit pending (Set.add name visited) finished
        | Exit name :: rest ->
            visit rest visited (name :: finished)
    vertices
    |> List.fold (fun (visited, finished) name ->
        if Set.contains name visited then visited, finished
        else visit [Enter name] visited finished) (Set.empty, [])
    |> snd

let private reverseAdjacency vertices sourceIndex adjacency =
    let empty = vertices |> List.map (fun name -> name, []) |> Map.ofList
    adjacency
    |> Map.fold (fun reversed caller targets ->
        targets
        |> List.fold (fun reversed callee ->
            match Map.tryFind callee reversed with
            | Some callers -> Map.add callee (caller :: callers) reversed
            | None -> Crash.crash "Owned function reverse call graph lost a definition") reversed) empty
    |> Map.map (fun _ callers -> List.sortBy sourceIndex callers)

let private stronglyConnectedComponents vertices sourceIndex adjacency =
    let reverse = reverseAdjacency vertices sourceIndex adjacency
    let rec collect pending visited members =
        match pending with
        | [] -> visited, members
        | name :: rest when Set.contains name visited ->
            collect rest visited members
        | name :: rest ->
            let pending =
                prependInOrder id (adjacent reverse name) rest
            collect pending (Set.add name visited) (Set.add name members)
    let rec partition remaining visited components =
        match remaining with
        | [] -> List.rev components
        | name :: rest when Set.contains name visited ->
            partition rest visited components
        | name :: rest ->
            let visited, memberNames = collect [name] visited Set.empty
            partition rest visited (memberNames :: components)
    finishOrder vertices adjacency
    |> fun order -> partition order Set.empty []

type private Component<'leaf, 'id> = {
    Index: int
    Head: Function<'leaf, 'id>
    Tail: Function<'leaf, 'id> list
    Names: Set<AST.FunctionId>
    InternalDependencies: Set<AST.FunctionId>
    ExternalTargets: Set<AST.FunctionId>
    Recursive: bool
}

let private components definitions sourceIndices names callsByFunction adjacency =
    let sourceIndex name =
        match Map.tryFind name sourceIndices with
        | Some index -> index
        | None -> Crash.crash "Owned function SCC lost its source position"
    let definitionsByName =
        definitions
        |> List.map (fun definition -> definition.Definition.Id, definition)
        |> Map.ofList
    stronglyConnectedComponents
        (definitions |> List.map (fun definition -> definition.Definition.Id))
        sourceIndex
        adjacency
    |> List.map (fun componentNames ->
        let members =
            componentNames
            |> Set.toList
            |> List.sortBy sourceIndex
            |> List.map (fun name ->
                match Map.tryFind name definitionsByName with
                | Some definition -> definition
                | None -> Crash.crash "Owned function SCC lost its definition")
        let calls =
            members
            |> List.map (fun definition ->
                match Map.tryFind definition.Definition.Id callsByFunction with
                | Some targets -> targets
                | None -> Crash.crash "Owned function SCC lost its call set")
            |> Set.unionMany
        match members with
        | memberHead :: memberTail ->
            let headName = memberHead.Definition.Id
            {
                Index = sourceIndex headName
                Head = memberHead
                Tail = memberTail
                Names = componentNames
                InternalDependencies =
                    Set.difference (Set.intersect calls names) componentNames
                ExternalTargets = Set.difference calls names
                Recursive =
                    Set.count componentNames > 1 || Set.contains headName calls
            }
        | [] -> Crash.crash "Owned function SCC partition produced an empty component")
    |> List.sortBy (fun groupInfo -> groupInfo.Index)

let private orderComponents components =
    let byIndex = components |> List.map (fun groupInfo -> groupInfo.Index, groupInfo) |> Map.ofList
    let ownerByName =
        components
        |> List.collect (fun groupInfo ->
            groupInfo.Names
            |> Set.toList
            |> List.map (fun name -> name, groupInfo.Index))
        |> Map.ofList
    let dependencies =
        components
        |> List.map (fun groupInfo ->
            let dependencyIndices =
                groupInfo.InternalDependencies
                |> Set.fold (fun indices name ->
                    match Map.tryFind name ownerByName with
                    | Some index -> Set.add index indices
                    | None -> Crash.crash "Owned function dependency lost its component") Set.empty
            groupInfo.Index, dependencyIndices)
        |> Map.ofList
    let dependents =
        components
        |> List.map (fun groupInfo -> groupInfo.Index, Set.empty)
        |> Map.ofList
        |> fun initial ->
            dependencies
            |> Map.fold (fun dependents caller dependencyIndices ->
                dependencyIndices
                |> Set.fold (fun dependents dependency ->
                    match Map.tryFind dependency dependents with
                    | Some callers -> Map.add dependency (Set.add caller callers) dependents
                    | None -> Crash.crash "Owned function dependency target lost its component") dependents) initial
    let unresolved = dependencies |> Map.map (fun _ values -> Set.count values)
    let ready =
        unresolved
        |> Map.fold (fun ready index count ->
            if count = 0 then Set.add index ready else ready) Set.empty
    let rec order remaining unresolved ready ordered =
        if remaining = 0 then List.rev ordered
        else
            match ready |> Set.toSeq |> Seq.tryHead with
            | None -> Crash.crash "Owned function SCC condensation graph contains a cycle"
            | Some selectedIndex ->
                let selected =
                    match Map.tryFind selectedIndex byIndex with
                    | Some groupInfo -> groupInfo
                    | None -> Crash.crash "Owned function ordering lost a ready component"
                let selectedDependents =
                    match Map.tryFind selectedIndex dependents with
                    | Some values -> values
                    | None -> Crash.crash "Owned function ordering lost dependent components"
                let unresolved, ready =
                    selectedDependents
                    |> Set.fold (fun (unresolved, ready) dependent ->
                        match Map.tryFind dependent unresolved with
                        | Some count when count > 0 ->
                            let next = count - 1
                            let ready = if next = 0 then Set.add dependent ready else ready
                            Map.add dependent next unresolved, ready
                        | _ -> Crash.crash "Owned function dependency count became invalid")
                        (unresolved, Set.remove selectedIndex ready)
                order (remaining - 1) unresolved ready (selected :: ordered)
    order components.Length unresolved ready []

/// Partition mutually visible owned functions into call-graph SCCs. Groups are
/// callee-first; independent groups retain the source order of their earliest
/// definition. Calls outside the supplied definitions remain external targets.
let discover
    (definitions: Function<'leaf, 'id> list)
    : Result<Group<'leaf, 'id> list, GroupingError> =
    match
        definitions
        |> List.countBy (fun definition -> definition.Definition.Id)
        |> List.tryFind (fun (_, count) -> count > 1)
    with
    | Some (name, _) -> Error (DuplicateFunctionName name)
    | None ->
        let names =
            definitions
            |> List.map (fun definition -> definition.Definition.Id)
            |> Set.ofList
        let callsByFunction =
            definitions
            |> List.map (fun definition ->
                definition.Definition.Id,
                blockCalls definition.Definition.Body)
            |> Map.ofList
        let sourceIndices =
            definitions
            |> List.mapi (fun index definition -> definition.Definition.Id, index)
            |> Map.ofList
        let sourceIndex name =
            match Map.tryFind name sourceIndices with
            | Some index -> index
            | None -> Crash.crash "Owned function call graph lost its source position"
        let adjacency =
            callsByFunction
            |> Map.map (fun _ calls ->
                Set.intersect names calls
                |> Set.toList
                |> List.sortBy sourceIndex)
        components definitions sourceIndices names callsByFunction adjacency
        |> orderComponents
        |> List.map (fun groupInfo ->
            Group (
                groupInfo.Head,
                groupInfo.Tail,
                groupInfo.Recursive,
                groupInfo.InternalDependencies,
                groupInfo.ExternalTargets))
        |> Ok
