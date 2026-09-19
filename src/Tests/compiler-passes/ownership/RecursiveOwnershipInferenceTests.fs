// RecursiveOwnershipInferenceTests.fs - Group-wide ownership uniqueness proof laws.

module RecursiveOwnershipInferenceTests

open OwnedIR

type private TestLeaf =
    | Reuse of input: string * output: string

let private value id : HIR.Value = {
    Id = HIR.ValueId id
    Type = AST.TList AST.TInt64
}

let private unitValue : HIR.Value = { Id = HIR.ValueId 100; Type = AST.TUnit }

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

let private parameter name value : HIR.Parameter = { Name = name; Value = value }

let private functionDefinition name ownership body : Function<TestLeaf, string> = {
    Definition = { Name = name; Body = body }
    Ownership = ownership
}

let private call target arguments result =
    Evaluate (HIR.Call { Target = target; Arguments = arguments; Result = result })

let private condition : HIR.Operand = {
    Expression = CheckedAST.BoolLiteral true
    Type = AST.TBool
    Inputs = Map.empty
}

let private infer semantics head tail =
    InferRecursiveOwnership.infer semantics { Head = head; Tail = tail }
    |> Result.map (fun candidates ->
        candidates
        |> InferRecursiveOwnership.toList
        |> List.map (fun boundary ->
            boundary
            |> InferRecursiveOwnership.boundaryToList
            |> List.map (fun functionBoundary ->
                functionBoundary.Name, functionBoundary.Ownership)))

let private testInfersSelfRecursiveUniqueness () =
    let input = value 0
    let reused = value 1
    let result = value 2
    let semantics = semantics [(input, "input"); (reused, "reused"); (result, "result")]
    let body =
        block
            [parameter "input" input]
            [
                Evaluate (HIR.Leaf (Reuse ("input", "reused")))
                call "loop" [reused] result
            ]
            result
    let definition =
        functionDefinition
            "loop"
            (signature [ConsumedParameter "input"] (ProducedResult "result"))
            body
    let expected = [[
        "loop", signature [UniqueParameter "input"] (UniqueProducedResult "result")
    ]]
    let actual = infer semantics definition []
    if actual = Ok expected then Ok ()
    else Error $"Expected one verified recursive uniqueness boundary {expected}, got {actual}"

let private testInfersMutualBoundaryTradeoffs () =
    let firstInput = value 10
    let firstRecursive = value 11
    let firstResult = value 12
    let secondInput = value 20
    let secondRecursive = value 21
    let secondResult = value 22
    let semantics =
        semantics [
            (firstInput, "firstInput")
            (firstRecursive, "firstRecursive")
            (firstResult, "firstResult")
            (secondInput, "secondInput")
            (secondRecursive, "secondRecursive")
            (secondResult, "secondResult")
        ]
    let firstRecursiveBranch =
        block [] [call "second" [firstInput] firstRecursive] firstRecursive
    let firstBaseBranch = block [] [] firstInput
    let first =
        functionDefinition
            "first"
            (signature [ConsumedParameter "firstInput"] (ProducedResult "firstResult"))
            (block
                [parameter "firstInput" firstInput]
                [Evaluate (HIR.Branch (
                    firstResult,
                    condition,
                    firstRecursiveBranch,
                    firstBaseBranch))]
                firstResult)
    let secondRecursiveBranch =
        block [] [call "first" [secondInput] secondRecursive] secondRecursive
    let secondBaseBranch = block [] [] secondInput
    let second =
        functionDefinition
            "second"
            (signature [ConsumedParameter "secondInput"] (ProducedResult "secondResult"))
            (block
                [parameter "secondInput" secondInput]
                [Evaluate (HIR.Branch (
                    secondResult,
                    condition,
                    secondRecursiveBranch,
                    secondBaseBranch))]
                secondResult)
    let expected = [
        [
            "first", signature [ConsumedParameter "firstInput"] (ProducedResult "firstResult")
            "second", signature [ConsumedParameter "secondInput"] (ProducedResult "secondResult")
        ]
        [
            "first", signature [UniqueParameter "firstInput"] (UniqueProducedResult "firstResult")
            "second", signature [UniqueParameter "secondInput"] (UniqueProducedResult "secondResult")
        ]
    ]
    let actual = infer semantics first [second]
    if actual = Ok expected then Ok ()
    else Error $"Expected group-wide transfer and uniqueness tradeoffs {expected}, got {actual}"

let private testRejectsInvalidFunctionGroup () =
    let first =
        functionDefinition
            "duplicate"
            (signature [UnmanagedParameter] UnmanagedResult)
            (block [parameter "unit" unitValue] [] unitValue)
    let second = first
    match infer (semantics []) first [second] with
    | Error (
        InferOwnershipUniqueness.NoVerifiedFunctionGroup (
            DuplicateFunctionName "duplicate")) -> Ok ()
    | actual -> Error $"Expected duplicate function names to reject group inference, got {actual}"

let private testBoundsGroupSearch () =
    let managedParameters =
        [0 .. 8]
        |> List.map (fun index -> value index, $"value{index}")
    let parameters =
        managedParameters
        |> List.map (fun (value, ownership) -> parameter ownership value)
    let boundary =
        signature
            (managedParameters |> List.map (snd >> ConsumedParameter))
            UnmanagedResult
    let definition =
        functionDefinition "wide" boundary (block parameters [] unitValue)
    match infer (semantics managedParameters) definition [] with
    | Error (InferOwnershipUniqueness.VariantLimitExceeded (9, 256)) -> Ok ()
    | actual -> Error $"Expected recursive-group inference to bound its search, got {actual}"

let tests = [
    "Recursive uniqueness inference verifies self calls as one group", testInfersSelfRecursiveUniqueness
    "Recursive uniqueness inference retains mutual boundary tradeoffs", testInfersMutualBoundaryTradeoffs
    "Recursive uniqueness inference rejects invalid function groups", testRejectsInvalidFunctionGroup
    "Recursive uniqueness inference bounds group-wide variants", testBoundsGroupSearch
]
