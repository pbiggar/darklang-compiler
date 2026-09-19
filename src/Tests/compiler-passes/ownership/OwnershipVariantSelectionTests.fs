// OwnershipVariantSelectionTests.fs - Deterministic call-site ownership selection laws.

module OwnershipVariantSelectionTests

open OwnedIR

type private TestLeaf = Reuse of input: string * output: string

let private value id : HIR.Value = {
    Id = HIR.ValueId id
    Type = AST.TList AST.TInt64
}

let private binding name =
    name |> Seq.fold (fun hash ch -> (hash * 31) + int ch) 17 |> AST.bindingId

let private parameter name value : HIR.Parameter =
    { Name = name; Binding = binding name; Value = value }

let private signature parameters result : FunctionSignature<string> = {
    Parameters = parameters
    Result = result
}

let private block parameters operations result : Block<TestLeaf, string> = {
    Body = {
        Parameters = parameters
        Operations = operations
        Result = result
    }
}

let private definition name ownership body : Function<TestLeaf, string> = {
    Definition = { Id = AST.functionIdForName name; Name = name; Body = body }
    Ownership = ownership
}

let private call target arguments result =
    Evaluate (HIR.Call {
        Target = AST.functionIdForName target
        Arguments = arguments
        Result = result
    })

let private condition : HIR.Operand = {
    Expression = CheckedAST.BoolLiteral true
    Type = AST.TBool
    Inputs = Map.empty
}

let private semantics mappings : Semantics<TestLeaf, string> =
    let ownershipByValue =
        mappings
        |> List.map (fun (value: HIR.Value, ownership) -> value.Id, ownership)
        |> Map.ofList
    {
        Leaf = function
            | Reuse (input, output) -> {
                Inputs = [Consumed input]
                Outputs = [output]
            }
        LeafUniqueness = function
            | Reuse (input, output) -> {
                RequiredInputs = Set.singleton input
                UniqueOutputs = Set.singleton output
            }
        CallOwnership = fun _ -> None
        ScalarUses = fun _ -> Set.empty
        ScalarEscapes = fun _ -> Set.empty
        BlockArgument = fun value ->
            match Map.tryFind value.Id ownershipByValue with
            | Some ownership -> Managed ownership
            | None -> Unmanaged
    }

let private transferredCall : CallSignature = {
    Parameters = [ConsumedCallParameter]
    Result = ProducedCallResult
}

let private siteWithBoundary target established uniqueArguments : SelectOwnershipVariants.CallSite = {
    Target = target
    Established = established
    UniqueArguments = uniqueArguments
}

let private site target uniqueArguments =
    siteWithBoundary target transferredCall uniqueArguments

let private catalog semantics definitions =
    InferOwnedFunctionGroups.infer semantics definitions
    |> Result.mapError (fun error -> $"Inference failed: {error}")
    |> Result.bind (fun groups ->
        SelectOwnershipVariants.create groups
        |> Result.mapError (fun error -> $"Catalog creation failed: {error}"))

let private selected selection =
    match selection with
    | SelectOwnershipVariants.EstablishedBoundary _ -> Error "Selected the established boundary"
    | SelectOwnershipVariants.InferredVariant variant -> Ok variant

let private testSelectsByAvailableUniqueness () =
    let input = value 0
    let boundary = signature [ConsumedParameter "input"] (ProducedResult "input")
    let identity = definition "identity" boundary (block [parameter "input" input] [] input)
    catalog (semantics [input, "input"]) [identity]
    |> Result.bind (fun variants ->
        SelectOwnershipVariants.select variants (site "identity" Set.empty)
        |> Result.mapError string
        |> Result.bind selected
        |> Result.bind (fun ordinary ->
            SelectOwnershipVariants.select variants (site "identity" (Set.singleton 0))
            |> Result.mapError string
            |> Result.bind selected
            |> Result.bind (fun unique ->
                SelectOwnershipVariants.select variants (site "identity" (Set.singleton 0))
                |> Result.mapError string
                |> Result.bind selected
                |> Result.bind (fun repeated ->
                    let ordinarySignature =
                        SelectOwnershipVariants.selectedCallSignature ordinary
                    let uniqueSignature =
                        SelectOwnershipVariants.selectedCallSignature unique
                    let identityIsStable =
                        SelectOwnershipVariants.selectedIdentity unique =
                            SelectOwnershipVariants.selectedIdentity repeated
                    let candidatesDiffer =
                        SelectOwnershipVariants.selectedIdentity ordinary <>
                            SelectOwnershipVariants.selectedIdentity unique
                    let expectedUnique = {
                        Parameters = [UniqueCallParameter]
                        Result = UniqueProducedCallResult
                    }
                    if ordinarySignature = transferredCall
                       && uniqueSignature = expectedUnique
                       && identityIsStable
                       && candidatesDiffer then Ok ()
                    else
                        Error
                            $"Expected stable capability-aware selection, got ordinary={ordinarySignature}, unique={uniqueSignature}, stable={identityIsStable}, distinct={candidatesDiffer}"))))

let private testFallsBackToEstablishedBoundary () =
    let input = value 10
    let output = value 11
    let reuse =
        definition
            "reuse"
            (signature [ConsumedParameter "input"] (ProducedResult "output"))
            (block
                [parameter "input" input]
                [Evaluate (HIR.Leaf (Reuse ("input", "output")))]
                output)
    catalog (semantics [input, "input"; output, "output"]) [reuse]
    |> Result.bind (fun variants ->
        SelectOwnershipVariants.select variants (site "reuse" Set.empty)
        |> Result.mapError string
        |> Result.bind (fun unavailable ->
            SelectOwnershipVariants.select variants (site "reuse" (Set.singleton 0))
            |> Result.mapError string
            |> Result.bind (fun available ->
                match unavailable, available with
                | SelectOwnershipVariants.EstablishedBoundary boundary,
                  SelectOwnershipVariants.InferredVariant selected
                    when boundary = transferredCall
                         && SelectOwnershipVariants.selectedCallSignature selected = {
                             Parameters = [UniqueCallParameter]
                             Result = UniqueProducedCallResult
                         } -> Ok ()
                | actual -> Error $"Expected established fallback followed by inferred reuse, got {actual}")))

let private testSelectsRecursiveGroupsAtomically () =
    let firstInput = value 20
    let firstRecursive = value 21
    let firstResult = value 22
    let secondInput = value 30
    let secondRecursive = value 31
    let secondResult = value 32
    let recursiveBranch target input result = block [] [call target [input] result] result
    let baseBranch input = block [] [] input
    let recursiveDefinition name target input recursiveResult result =
        definition
            name
            (signature [ConsumedParameter $"{name}Input"] (ProducedResult $"{name}Result"))
            (block
                [parameter "input" input]
                [Evaluate (HIR.Branch (
                    result,
                    condition,
                    recursiveBranch target input recursiveResult,
                    baseBranch input))]
                result)
    let first =
        recursiveDefinition "first" "second" firstInput firstRecursive firstResult
    let second =
        recursiveDefinition "second" "first" secondInput secondRecursive secondResult
    let mappings = [
        firstInput, "firstInput"
        firstRecursive, "firstRecursive"
        firstResult, "firstResult"
        secondInput, "secondInput"
        secondRecursive, "secondRecursive"
        secondResult, "secondResult"
    ]
    catalog (semantics mappings) [first; second]
    |> Result.bind (fun variants ->
        SelectOwnershipVariants.select variants (site "first" (Set.singleton 0))
        |> Result.mapError string
        |> Result.bind selected
        |> Result.bind (fun selection ->
            let boundaries =
                selection
                |> SelectOwnershipVariants.selectedCandidate
                |> InferOwnedFunctionGroups.candidateBoundaries
                |> List.map (fun boundary -> boundary.Name, boundary.Ownership)
            let expected = [
                "first", signature [UniqueParameter "firstInput"] (UniqueProducedResult "firstResult")
                "second", signature [UniqueParameter "secondInput"] (UniqueProducedResult "secondResult")
            ]
            let identityNames =
                selection
                |> SelectOwnershipVariants.selectedIdentity
                |> SelectOwnershipVariants.identityBoundaries
                |> List.map fst
            if boundaries = expected && identityNames = ["first"; "second"] then Ok ()
            else Error $"Expected one atomic recursive candidate {expected}, got {boundaries}"))

let private testRejectsInvalidCatalogAndCalls () =
    let input = value 40
    let boundary = signature [ConsumedParameter "input"] (ProducedResult "input")
    let identity = definition "identity" boundary (block [parameter "input" input] [] input)
    InferOwnedFunctionGroups.infer (semantics [input, "input"]) [identity]
    |> Result.mapError (fun error -> $"Inference failed: {error}")
    |> Result.bind (fun groups ->
        match SelectOwnershipVariants.create (groups @ groups) with
        | Error (SelectOwnershipVariants.DuplicateFunctionName "identity") ->
            SelectOwnershipVariants.create groups
            |> Result.mapError string
            |> Result.bind (fun variants ->
                match SelectOwnershipVariants.select variants (site "missing" Set.empty) with
                | Error (SelectOwnershipVariants.UnknownFunction "missing") ->
                    match SelectOwnershipVariants.select variants (site "identity" (Set.singleton 1)) with
                    | Error (SelectOwnershipVariants.InvalidUniqueArgumentIndex ("identity", 1)) ->
                        let establishedUnique = {
                            Parameters = [UniqueCallParameter]
                            Result = ProducedCallResult
                        }
                        match
                            SelectOwnershipVariants.select
                                variants
                                (siteWithBoundary "identity" establishedUnique Set.empty)
                        with
                        | Error (
                            SelectOwnershipVariants.MissingEstablishedUniqueArgument (
                                "identity",
                                0)) ->
                            let borrowed = {
                                Parameters = [BorrowedCallParameter]
                                Result = BorrowedCallResult 0
                            }
                            match
                                SelectOwnershipVariants.select
                                    variants
                                    (siteWithBoundary "identity" borrowed Set.empty)
                            with
                            | Error (
                                SelectOwnershipVariants.InconsistentEstablishedBoundary
                                    "identity") -> Ok ()
                            | actual -> Error $"Expected an inconsistent established boundary, got {actual}"
                        | actual -> Error $"Expected missing established uniqueness, got {actual}"
                    | actual -> Error $"Expected an invalid uniqueness index, got {actual}"
                | actual -> Error $"Expected an unknown call target, got {actual}")
        | actual -> Error $"Expected duplicate catalog entries to fail, got {actual}")

let tests = [
    "Ownership variants select by available argument uniqueness", testSelectsByAvailableUniqueness
    "Ownership variants retain the established fallback", testFallsBackToEstablishedBoundary
    "Ownership variants select recursive SCC candidates atomically", testSelectsRecursiveGroupsAtomically
    "Ownership variant catalogs reject invalid calls", testRejectsInvalidCatalogAndCalls
]
