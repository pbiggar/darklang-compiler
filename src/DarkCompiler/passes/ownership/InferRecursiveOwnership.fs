// InferRecursiveOwnership.fs - Solve uniqueness boundaries across visible function groups.

module InferRecursiveOwnership

open OwnedIR
open InferOwnershipUniqueness

type FunctionBoundary<'id> = {
    Name: string
    Ownership: FunctionSignature<'id>
}

type GroupBoundary<'id> =
    private
    | GroupBoundary of head: FunctionBoundary<'id> * tail: FunctionBoundary<'id> list

type Candidates<'id> =
    private
    | Candidates of head: GroupBoundary<'id> * tail: GroupBoundary<'id> list

let toList (Candidates (head, tail)) = head :: tail
let boundaryToList (GroupBoundary (head, tail)) = head :: tail

let private dominates
    (first: Function<'leaf, 'id> list)
    (second: Function<'leaf, 'id> list) =
    let rec compare
        noWorse
        strictlyBetter
        (first: Function<'leaf, 'id> list)
        (second: Function<'leaf, 'id> list) =
        match first, second with
        | [], [] -> noWorse && strictlyBetter
        | first :: firstRest, second :: secondRest ->
            let boundaryNoWorse, boundaryStrict =
                boundaryRelation first.Ownership second.Ownership
            compare
                (noWorse && boundaryNoWorse)
                (strictlyBetter || boundaryStrict)
                firstRest
                secondRest
        | _ -> Crash.crash "Uniqueness group variants changed function count"
    compare true false first second

let private variants (functions: Function<'leaf, 'id> list) =
    functions
    |> List.fold (fun groups functionDefinition ->
        [ for group in groups do
            for ownership in signatures functionDefinition.Ownership do
                yield group @ [{ functionDefinition with Ownership = ownership }] ]) [[]]

let private boundary = function
    | [] -> Crash.crash "Ownership uniqueness inference produced an empty function group"
    | head :: tail ->
        let functionBoundary functionDefinition = {
            Name = functionDefinition.Definition.Name
            Ownership = functionDefinition.Ownership
        }
        GroupBoundary (functionBoundary head, List.map functionBoundary tail)

/// Infer a mutually visible function group as one proof unit. Internal direct
/// and recursive calls receive ownership contracts derived from each candidate
/// group, while external call contracts continue to come from the dialect.
/// Nondominance is evaluated across every function boundary together.
let infer
    (semantics: Semantics<'leaf, 'id>)
    (definitions: AST.NonEmptyList<Function<'leaf, 'id>>)
    : Result<Candidates<'id>, InferenceError<'id>> =
    let definitions = AST.NonEmptyList.toList definitions
    let refinableModes =
        definitions
        |> List.sumBy (fun definition -> refinableModeCount definition.Ownership)
    if not (withinVariantLimit refinableModes) then
        Error (VariantLimitExceeded (refinableModes, maximumVariants))
    else
        let verified, firstFailure =
            variants definitions
            |> List.fold (fun (verified, firstFailure) candidate ->
                match VerifyOwnership.verifyFunctions semantics candidate with
                | Ok () -> candidate :: verified, firstFailure
                | Error error ->
                    let firstFailure =
                        match firstFailure with
                        | Some _ -> firstFailure
                        | None -> Some error
                    verified, firstFailure) ([], None)
        let verified = List.rev verified
        let nondominated =
            verified
            |> List.filter (fun candidate ->
                verified
                |> List.exists (fun other -> dominates other candidate)
                |> not)
        match nondominated, firstFailure with
        | head :: tail, _ ->
            Ok (Candidates (boundary head, List.map boundary tail))
        | [], Some error -> Error (NoVerifiedFunctionGroup error)
        | [], None -> Crash.crash "Ownership uniqueness inference generated no function-group candidates"
