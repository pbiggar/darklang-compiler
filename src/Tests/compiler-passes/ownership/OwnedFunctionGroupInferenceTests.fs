// OwnedFunctionGroupInferenceTests.fs - Program-level ownership inference laws.

module OwnedFunctionGroupInferenceTests

open OwnedIR

type private TestLeaf = TestLeaf

let private value id : HIR.Value = {
    Id = HIR.ValueId id
    Type = AST.TList AST.TInt64
}

let private unitValue id : HIR.Value = {
    Id = HIR.ValueId id
    Type = AST.TUnit
}

let private parameter name value : HIR.Parameter = { Name = name; Value = value }

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
    Definition = { Name = name; Body = body }
    Ownership = ownership
}

let private call target arguments result =
    Evaluate (HIR.Call { Target = target; Arguments = arguments; Result = result })

let private callSignature parameters result : CallSignature = {
    Parameters = parameters
    Result = result
}

let private semantics mappings registeredCalls : Semantics<TestLeaf, string> =
    let ownershipByValue =
        mappings
        |> List.map (fun (value: HIR.Value, ownership) -> value.Id, ownership)
        |> Map.ofList
    {
        Leaf = fun TestLeaf -> { Inputs = []; Outputs = [] }
        LeafUniqueness = fun TestLeaf -> {
            RequiredInputs = Set.empty
            UniqueOutputs = Set.empty
        }
        CallOwnership = fun call -> Map.tryFind call.Target registeredCalls
        ScalarUses = fun _ -> Set.empty
        ScalarEscapes = fun _ -> Set.empty
        BlockArgument = fun value ->
            match Map.tryFind value.Id ownershipByValue with
            | Some ownership -> Managed ownership
            | None -> Unmanaged
    }

let private candidateSummary candidate =
    candidate
    |> InferOwnedFunctionGroups.candidateBoundaries
    |> List.map (fun boundary -> boundary.Name, boundary.Ownership)

let private groupSummary group =
    InferOwnedFunctionGroups.isRecursive group,
    InferOwnedFunctionGroups.internalDependencies group,
    InferOwnedFunctionGroups.externalTargets group,
    (InferOwnedFunctionGroups.candidates group |> List.map candidateSummary)

let private testInfersAcyclicGroupsCalleeFirst () =
    let leafInput = value 0
    let leafBoundary =
        signature [ConsumedParameter "leafInput"] (ProducedResult "leafInput")
    let leaf =
        definition
            "leaf"
            leafBoundary
            (block [parameter "input" leafInput] [] leafInput)

    let entryInput = value 10
    let intermediate = value 11
    let entryResult = value 12
    let entryBoundary =
        signature [ConsumedParameter "entryInput"] (ProducedResult "entryResult")
    let entry =
        definition
            "entry"
            entryBoundary
            (block
                [parameter "input" entryInput]
                [
                    call "leaf" [entryInput] intermediate
                    call "external" [intermediate] entryResult
                ]
                entryResult)
    let transferredCall =
        callSignature [ConsumedCallParameter] ProducedCallResult
    let actual =
        InferOwnedFunctionGroups.infer
            (semantics
                [
                    leafInput, "leafInput"
                    entryInput, "entryInput"
                    intermediate, "intermediate"
                    entryResult, "entryResult"
                ]
                (Map.ofList ["leaf", transferredCall; "external", transferredCall]))
            [entry; leaf]
        |> Result.map (List.map groupSummary)
    let expected = Ok [
        (
            false,
            Set.empty,
            Set.empty,
            [
                ["leaf", leafBoundary]
                ["leaf", signature [UniqueParameter "leafInput"] (UniqueProducedResult "leafInput")]
            ])
        (
            false,
            Set.singleton "leaf",
            Set.singleton "external",
            [["entry", entryBoundary]])
    ]
    if actual = expected then Ok ()
    else Error $"Expected callee-first inferred groups with every boundary tradeoff {expected}, got {actual}"

let private testInfersSelfAndMutuallyRecursiveGroups () =
    let selfValue = unitValue 20
    let mutualAValue = unitValue 21
    let mutualBValue = unitValue 22
    let unmanagedBoundary = signature [UnmanagedParameter] UnmanagedResult
    let recursive name target bodyValue =
        definition
            name
            unmanagedBoundary
            (block
                [parameter "unit" bodyValue]
                [call target [bodyValue] bodyValue]
                bodyValue)
    let self = recursive "self" "self" selfValue
    let mutualA = recursive "mutualA" "mutualB" mutualAValue
    let mutualB = recursive "mutualB" "mutualA" mutualBValue
    let actual =
        InferOwnedFunctionGroups.infer
            (semantics [] Map.empty)
            [self; mutualA; mutualB]
        |> Result.map (List.map groupSummary)
    let expected = Ok [
        (true, Set.empty, Set.empty, [["self", unmanagedBoundary]])
        (
            true,
            Set.empty,
            Set.empty,
            [["mutualA", unmanagedBoundary; "mutualB", unmanagedBoundary]])
    ]
    if actual = expected then Ok ()
    else Error $"Expected self and mutual SCCs to use group-wide inference {expected}, got {actual}"

let private testReportsFailingGroupNames () =
    let parameters start count =
        [start .. start + count - 1]
        |> List.map (fun index -> value index, $"value{index}")
    let firstParameters = parameters 30 5
    let secondParameters = parameters 40 4
    let wideDefinition name target parameters result =
        let values = parameters |> List.map fst
        definition
            name
            (signature
                (parameters |> List.map (snd >> ConsumedParameter))
                UnmanagedResult)
            (block
                (parameters |> List.map (fun (value, name) -> parameter name value))
                [call target values result]
                result)
    let first = wideDefinition "first" "second" firstParameters (unitValue 50)
    let second = wideDefinition "second" "first" secondParameters (unitValue 51)
    match
        InferOwnedFunctionGroups.infer
            (semantics (firstParameters @ secondParameters) Map.empty)
            [first; second]
    with
    | Error (
        InferOwnedFunctionGroups.GroupInferenceFailed (
            names,
            InferOwnershipUniqueness.VariantLimitExceeded (9, 256)))
        when AST.NonEmptyList.toList names = ["first"; "second"] -> Ok ()
    | actual -> Error $"Expected the failing SCC names and inference failure, got {actual}"

let private testReportsGroupingFailures () =
    let bodyValue = unitValue 60
    let duplicate =
        definition
            "duplicate"
            (signature [UnmanagedParameter] UnmanagedResult)
            (block [parameter "unit" bodyValue] [] bodyValue)
    match InferOwnedFunctionGroups.infer (semantics [] Map.empty) [duplicate; duplicate] with
    | Error (
        InferOwnedFunctionGroups.FunctionGroupingFailed (
            OwnedFunctionGroups.DuplicateFunctionName "duplicate")) -> Ok ()
    | actual -> Error $"Expected duplicate definitions to retain their grouping error, got {actual}"

let tests = [
    "Owned function groups infer acyclic candidates in callee-first order", testInfersAcyclicGroupsCalleeFirst
    "Owned function groups infer self and mutual recursion as proof units", testInfersSelfAndMutuallyRecursiveGroups
    "Owned function group inference reports the failing SCC", testReportsFailingGroupNames
    "Owned function group inference preserves grouping failures", testReportsGroupingFailures
]
