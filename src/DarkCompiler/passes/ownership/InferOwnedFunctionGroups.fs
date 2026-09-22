// InferOwnedFunctionGroups.fs - Infer uniqueness variants for owned-HIR call groups.

module InferOwnedFunctionGroups

open OwnedIR

type FunctionBoundary<'id> = InferRecursiveOwnership.FunctionBoundary<'id>

type Candidate<'id> =
    private
    | Candidate of head: FunctionBoundary<'id> * tail: FunctionBoundary<'id> list

type Group<'id> =
    private
    | Group of
        head: Candidate<'id> *
        tail: Candidate<'id> list *
        recursive: bool *
        internalDependencies: Set<AST.FunctionId> *
        externalTargets: Set<AST.FunctionId>

type Program<'leaf, 'id> =
    private
    | Program of Map<AST.FunctionId, OwnedFunctionGroups.Group<'leaf, 'id>>

type InferenceError<'id when 'id: comparison> =
    | FunctionGroupingFailed of OwnedFunctionGroups.GroupingError
    | DemandTargetMissing of AST.FunctionId
    | GroupInferenceFailed of
        functionNames: AST.NonEmptyList<string> *
        cause: InferOwnershipUniqueness.InferenceError<'id>

let candidateBoundaries (Candidate (head, tail)) = head :: tail
let candidates (Group (head, tail, _, _, _)) = head :: tail
let isRecursive (Group (_, _, recursive, _, _)) = recursive
let internalDependencies (Group (_, _, _, dependencies, _)) = dependencies
let externalTargets (Group (_, _, _, _, targets)) = targets

let private nonEmpty context (values: 'value list) : AST.NonEmptyList<'value> =
    match values with
    | head :: tail -> { Head = head; Tail = tail }
    | [] -> Crash.crash context

let private inferredGroup discovered candidates =
    match candidates with
    | head :: tail ->
        Group (
            head,
            tail,
            OwnedFunctionGroups.isRecursive discovered,
            OwnedFunctionGroups.internalDependencies discovered,
            OwnedFunctionGroups.externalTargets discovered)
    | [] -> Crash.crash "Ownership uniqueness inference returned no group candidates"

let private singletonCandidate id name ownership =
    let boundary : FunctionBoundary<'id> = { Id = id; Name = name; Ownership = ownership }
    Candidate (boundary, [])

let private recursiveCandidate boundary =
    let boundaries = InferRecursiveOwnership.boundaryToList boundary
    match boundaries with
    | head :: tail -> Candidate (head, tail)
    | [] -> Crash.crash "Recursive ownership inference returned an empty boundary"

let private inferGroup
    (semantics: Semantics<'leaf, 'id>)
    (discovered: OwnedFunctionGroups.Group<'leaf, 'id>) =
    let definitions = OwnedFunctionGroups.functions discovered
    let names =
        definitions
        |> List.map (fun definition -> definition.Definition.Name)
        |> nonEmpty "Owned function discovery returned an empty group"
    let wrap result =
        result
        |> Result.mapError (fun error -> GroupInferenceFailed (names, error))
        |> Result.map (inferredGroup discovered)
    match definitions, OwnedFunctionGroups.isRecursive discovered with
    | [definition], false ->
        InferOwnershipUniqueness.infer semantics definition
        |> Result.map (fun inferred ->
            inferred
            |> InferOwnershipUniqueness.toList
            |> List.map (singletonCandidate definition.Definition.Id definition.Definition.Name))
        |> wrap
    | head :: tail, true ->
        InferRecursiveOwnership.infer semantics { Head = head; Tail = tail }
        |> Result.map (fun inferred ->
            inferred
            |> InferRecursiveOwnership.toList
            |> List.map recursiveCandidate)
        |> wrap
    | _ :: _, false ->
        Crash.crash "Owned function discovery produced a nonrecursive multi-function group"
    | [], _ -> Crash.crash "Owned function discovery returned an empty group"

/// Discover proof groups without inferring any variants. A function maps back
/// to its complete SCC so a later concrete call demand can be solved atomically.
let prepare
    (definitions: Function<'leaf, 'id> list)
    : Result<Program<'leaf, 'id>, InferenceError<'id>> =
    OwnedFunctionGroups.discover definitions
    |> Result.mapError FunctionGroupingFailed
    |> Result.map (fun groups ->
        groups
        |> List.fold (fun index group ->
            OwnedFunctionGroups.functions group
            |> List.fold (fun index definition ->
                Map.add definition.Definition.Id group index) index) Map.empty
        |> Program)

/// Recursive edges are implementation details of an atomic SCC candidate, not
/// independent external demands. Materialization rewrites them when the group
/// is selected by a call entering the component.
let isInternalRecursiveCall (Program groups) caller target =
    match Map.tryFind target groups with
    | Some group when OwnedFunctionGroups.isRecursive group ->
        group
        |> OwnedFunctionGroups.functions
        |> List.exists (fun definition -> definition.Definition.Id = caller)
    | Some _
    | None -> false

/// Infer at most one best candidate for an actual call demand. Uncalled groups
/// never reach uniqueness verification, and an unprovable demand safely keeps
/// the established boundary.
let inferDemand
    (semantics: Semantics<'leaf, 'id>)
    (Program groups)
    target
    uniqueArguments
    : Result<Group<'id> option, InferenceError<'id>> =
    match Map.tryFind target groups with
    | None -> Error (DemandTargetMissing target)
    | Some discovered ->
        let definitions = OwnedFunctionGroups.functions discovered
        let names =
            definitions
            |> List.map (fun definition -> definition.Definition.Name)
            |> nonEmpty "Owned function demand group is empty"
        let wrap result =
            result
            |> Result.mapError (fun error -> GroupInferenceFailed (names, error))
            |> Result.map (Option.map (fun candidate -> inferredGroup discovered [candidate]))
        match definitions, OwnedFunctionGroups.isRecursive discovered with
        | [definition], false ->
            InferOwnershipUniqueness.inferDemand semantics uniqueArguments definition
            |> Result.map (Option.map (fun ownership ->
                singletonCandidate definition.Definition.Id definition.Definition.Name ownership))
            |> wrap
        | head :: tail, true ->
            InferRecursiveOwnership.inferDemand
                semantics
                target
                uniqueArguments
                { Head = head; Tail = tail }
            |> Result.map (Option.map recursiveCandidate)
            |> wrap
        | _ :: _, false ->
            Crash.crash "Owned function discovery produced a nonrecursive multi-function group"
        | [], _ -> Crash.crash "Owned function discovery returned an empty group"

/// Discover callee-first owned-HIR SCCs and infer every nondominated uniqueness
/// boundary for each proof unit. Cross-group calls deliberately retain the
/// ownership contracts registered in `semantics`; selecting inferred callee
/// variants at call sites is a later specialization policy.
let infer
    (semantics: Semantics<'leaf, 'id>)
    (definitions: Function<'leaf, 'id> list)
    : Result<Group<'id> list, InferenceError<'id>> =
    let rec inferGroups inferred = function
        | [] -> Ok (List.rev inferred)
        | discovered :: rest ->
            inferGroup semantics discovered
            |> Result.bind (fun group -> inferGroups (group :: inferred) rest)
    OwnedFunctionGroups.discover definitions
    |> Result.mapError FunctionGroupingFailed
    |> Result.bind (inferGroups [])
