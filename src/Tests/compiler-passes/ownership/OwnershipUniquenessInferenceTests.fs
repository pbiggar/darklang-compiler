// OwnershipUniquenessInferenceTests.fs - Proven function-boundary refinement laws.

module OwnershipUniquenessInferenceTests

open OwnedIR

let private binding name =
    name |> Seq.fold (fun hash ch -> (hash * 31) + int ch) 17 |> AST.bindingId

type private TestLeaf =
    | Reuse of input: string * output: string

let private inputValue : HIR.Value = {
    Id = HIR.ValueId 0
    Type = AST.TList AST.TInt64
}

let private outputValue : HIR.Value = {
    Id = HIR.ValueId 1
    Type = AST.TList AST.TInt64
}

let private unitValue : HIR.Value = {
    Id = HIR.ValueId 100
    Type = AST.TUnit
}

let private ownershipIds mappings =
    mappings
    |> List.map (fun (value: HIR.Value, ownership) -> value.Id, ownership)
    |> Map.ofList

let private semantics mappings : Semantics<TestLeaf, string> =
    let ownershipByValue = ownershipIds mappings
    let scalarIds (operand: HIR.Operand) =
        operand.Inputs
        |> Map.values
        |> Seq.choose (fun value -> Map.tryFind value.Id ownershipByValue)
        |> Set.ofSeq
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
        ScalarUses = scalarIds
        ScalarEscapes = scalarIds
        BlockArgument = fun value ->
            match Map.tryFind value.Id ownershipByValue with
            | Some ownership -> Managed ownership
            | None -> Unmanaged
    }

let private parameter name value : HIR.Parameter =
    { Name = name; Binding = binding name; Value = value }

let private block parameters operations result : Block<TestLeaf, string> = {
    Body = {
        Parameters = parameters
        Operations = operations
        Result = result
    }
}

let private infer semantics signature body =
    let functionDefinition : Function<TestLeaf, string> = {
        Definition = {
            Id = TestIds.functionIdForName "test"
            Name = "test"
            Body = body
        }
        Ownership = signature
    }
    InferOwnershipUniqueness.infer semantics functionDefinition
    |> Result.map InferOwnershipUniqueness.toList

let private signature parameters result : FunctionSignature<string> = {
    Parameters = parameters
    Result = result
}

let private testRequiresAndReturnsUniqueReuse () =
    let semantics = semantics [(inputValue, "input"); (outputValue, "output")]
    let body =
        block
            [parameter "input" inputValue]
            [Evaluate (HIR.Leaf (Reuse ("input", "output")))]
            outputValue
    let initial = signature [ConsumedParameter "input"] (ProducedResult "output")
    let expected = [signature [UniqueParameter "input"] (UniqueProducedResult "output")]
    let actual = infer semantics initial body
    if actual = Ok expected then Ok ()
    else Error $"Expected the verified unique reuse boundary {expected}, got {actual}"

let private testRetainsInputOutputTradeoffs () =
    let semantics = semantics [(inputValue, "value")]
    let body = block [parameter "value" inputValue] [] inputValue
    let initial = signature [ConsumedParameter "value"] (ProducedResult "value")
    let expected = [
        signature [ConsumedParameter "value"] (ProducedResult "value")
        signature [UniqueParameter "value"] (UniqueProducedResult "value")
    ]
    let actual = infer semantics initial body
    if actual = Ok expected then Ok ()
    else Error $"Expected incomparable transfer and unique boundaries {expected}, got {actual}"

let private testEscapeRevokesUniqueResult () =
    let semantics = semantics [(inputValue, "input"); (outputValue, "output")]
    let escapeOperand : HIR.Operand = {
        Expression = CheckedAST.Local (binding "escape")
        Type = AST.TUnit
        Inputs = Map.ofList [(binding "output", outputValue)]
    }
    let body =
        block
            [parameter "input" inputValue]
            [
                Evaluate (HIR.Leaf (Reuse ("input", "output")))
                Evaluate (HIR.ScalarBinding (unitValue, escapeOperand))
            ]
            outputValue
    let initial = signature [ConsumedParameter "input"] (ProducedResult "output")
    let expected = [signature [UniqueParameter "input"] (ProducedResult "output")]
    let actual = infer semantics initial body
    if actual = Ok expected then Ok ()
    else Error $"Expected scalar escape to remove only the result uniqueness promise, got {actual}"

let private testRejectsUnprovableBoundary () =
    let semantics = semantics [(inputValue, "value")]
    let body = block [parameter "value" inputValue] [] inputValue
    let invalid = signature [BorrowedParameter "value"] (ProducedResult "value")
    match infer semantics invalid body with
    | Error (InferOwnershipUniqueness.NoVerifiedBoundary _) -> Ok ()
    | actual -> Error $"Expected no verified ownership boundary, got {actual}"

let private testBoundsVariantSearch () =
    let managedParameters =
        [0 .. 8]
        |> List.map (fun index ->
            let value : HIR.Value = {
                Id = HIR.ValueId index
                Type = AST.TList AST.TInt64
            }
            value, $"value{index}")
    let semantics = semantics managedParameters
    let parameters =
        managedParameters
        |> List.map (fun (value, ownership) -> parameter ownership value)
    let boundary =
        signature
            (managedParameters |> List.map (snd >> ConsumedParameter))
            UnmanagedResult
    let body = block parameters [] unitValue
    match infer semantics boundary body with
    | Error (InferOwnershipUniqueness.VariantLimitExceeded (9, 256)) -> Ok ()
    | actual -> Error $"Expected bounded uniqueness search, got {actual}"

let private testDefersRecursiveInference () =
    let semantics = semantics [(inputValue, "value")]
    let recursiveCall : HIR.FunctionCall = {
        Target = TestIds.functionIdForName "test"
        Arguments = [inputValue]
        Result = inputValue
    }
    let body =
        block
            [parameter "value" inputValue]
            [Evaluate (HIR.Call recursiveCall)]
            inputValue
    let boundary = signature [ConsumedParameter "value"] (ProducedResult "value")
    match infer semantics boundary body with
    | Error (InferOwnershipUniqueness.RecursiveFunctionRequiresGroupInference id)
        when id = TestIds.functionIdForName "test" -> Ok ()
    | actual -> Error $"Expected recursive uniqueness inference to require a group solver, got {actual}"

let tests = [
    "Uniqueness inference proves required reuse boundaries", testRequiresAndReturnsUniqueReuse
    "Uniqueness inference retains input and output tradeoffs", testRetainsInputOutputTradeoffs
    "Scalar escapes revoke inferred unique results", testEscapeRevokesUniqueResult
    "Uniqueness inference rejects unprovable boundaries", testRejectsUnprovableBoundary
    "Uniqueness inference bounds specialization variants", testBoundsVariantSearch
    "Uniqueness inference defers recursive functions to a group solver", testDefersRecursiveInference
]
