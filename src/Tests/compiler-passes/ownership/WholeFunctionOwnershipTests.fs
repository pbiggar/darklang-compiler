// WholeFunctionOwnershipTests.fs - Whole-function boundary inference and ownership placement laws.

module WholeFunctionOwnershipTests

open OwnedIR

type private TestLeaf = {
    Inputs: (HIR.Value * bool) list
    Outputs: HIR.Value list
}

type private TestBlock = TestBlock of HIR.Block<HIR.Operation<TestLeaf, TestBlock>>

let private body (TestBlock block) = block

let private managed id : HIR.Value = {
    Id = HIR.ValueId id
    Type = AST.TList AST.TInt64
}

let private unitValue id : HIR.Value = {
    Id = HIR.ValueId id
    Type = AST.TUnit
}

let private binding name =
    name |> Seq.fold (fun hash ch -> (hash * 31) + int ch) 17 |> AST.bindingId

let private parameter name value : HIR.Parameter = {
    Name = name
    Binding = binding name
    Value = value
}

let private block parameters operations result =
    TestBlock { Parameters = parameters; Operations = operations; Result = result }

let private definition name parameters operations result : HIR.Function<TestBlock> = {
    Id = TestIds.functionIdForName name
    Name = name
    Body = block parameters operations result
}

let private scalar result inputs =
    HIR.ScalarBinding (
        result,
        {
            Expression = CheckedAST.BoolLiteral true
            Type = result.Type
            Inputs = inputs |> List.map (fun (name, value) -> binding name, value) |> Map.ofList
        })

let private leaf inputs outputs = HIR.Leaf { Inputs = inputs; Outputs = outputs }

let private call target arguments result =
    HIR.Call {
        Target = TestIds.functionIdForName target
        Arguments = arguments
        Result = result
    }

let private isManaged (value: HIR.Value) =
    match value.Type with
    | AST.TList _ -> true
    | _ -> false

let private leafOwnership leaf : Contract<HIR.ValueId> = {
    Inputs =
        leaf.Inputs
        |> List.choose (fun (value, consumed) ->
            if not (isManaged value) then None
            elif consumed then Some (Consumed value.Id)
            else Some (Borrowed value.Id))
    Outputs = leaf.Outputs |> List.filter isManaged |> List.map (fun value -> value.Id)
}

let private dialect : ElaborateFunctionOwnership.Dialect<TestLeaf, TestBlock> = {
    Body = body
    LeafOwnership = leafOwnership
    LeafUniqueness = fun leaf -> {
        RequiredInputs = Set.empty
        UniqueOutputs = leaf.Outputs |> List.filter isManaged |> List.map (fun value -> value.Id) |> Set.ofList
    }
    IsManaged = isManaged
    ExternalCallOwnership = fun _ -> None
}

let private primitiveContract leaf : HIR.PrimitiveContract = {
    Inputs = leaf.Inputs |> List.map fst
    Operands = []
    Outputs =
        leaf.Outputs
        |> List.map (fun value -> {
            Value = value
            Alias = if isManaged value then HIR.FreshManaged else HIR.NoManagedAlias
        })
    Effects = Set.empty
}

let private callContract (call: HIR.FunctionCall) : HIR.PrimitiveContract = {
    Inputs = call.Arguments
    Operands = []
    Outputs = [{
        Value = call.Result
        Alias = if isManaged call.Result then HIR.UnknownManagedAlias else HIR.NoManagedAlias
    }]
    Effects = Set.singleton HIR.MayInvokeUserCode
}

let private elaborate definitions =
    ElaborateFunctionOwnership.elaborateFunctions dialect definitions
    |> Result.mapError (fun error -> $"Ownership elaboration failed: {error}")
    |> Result.bind (fun analysis ->
        let contracts : VerifyOwnedHIR.HIRContracts<TestLeaf> = {
            Leaf = primitiveContract
            CallSignature = fun _ -> None
            CallContract = fun call -> Some (callContract call)
        }
        VerifyOwnedHIR.verifyFunctions
            contracts
            (ElaborateFunctionOwnership.semantics analysis)
            (ElaborateFunctionOwnership.functions analysis)
        |> Result.mapError (fun error -> $"Owned HIR verification failed: {error}")
        |> Result.map (fun () -> analysis))

let private findFunction name analysis =
    ElaborateFunctionOwnership.functions analysis
    |> List.tryFind (fun functionDefinition -> functionDefinition.Definition.Name = name)

let private testInfersBorrowedOpaqueInput () =
    let input = managed 0
    let result = unitValue 1
    let source = definition "borrow" [parameter "input" input] [scalar result ["input", input]] result
    match elaborate [source] with
    | Ok analysis ->
        match findFunction "borrow" analysis with
        | Some owned when owned.Ownership = {
                Parameters = [BorrowedParameter input.Id]
                Result = UnmanagedResult
            } -> Ok ()
        | actual -> Error $"Expected an opaque managed use to infer a borrowed parameter, got {actual}"
    | Error error -> Error error

let private testInfersConsumedInput () =
    let input = managed 0
    let result = unitValue 1
    let source = definition "consume" [parameter "input" input] [leaf [input, true] [result]] result
    match elaborate [source] with
    | Ok analysis ->
        match findFunction "consume" analysis with
        | Some owned when owned.Ownership = {
                Parameters = [ConsumedParameter input.Id]
                Result = UnmanagedResult
            } -> Ok ()
        | actual -> Error $"Expected the leaf demand to infer a consumed parameter, got {actual}"
    | Error error -> Error error

let private testDropsOppositeBranchInputs () =
    let first = managed 0
    let second = managed 1
    let yesResult = unitValue 2
    let noResult = unitValue 3
    let result = unitValue 4
    let branch =
        HIR.Branch (
            result,
            { Expression = CheckedAST.BoolLiteral true; Type = AST.TBool; Inputs = Map.empty },
            block [] [leaf [first, true] [yesResult]] yesResult,
            block [] [leaf [second, true] [noResult]] noResult)
    let source =
        definition
            "branch"
            [parameter "first" first; parameter "second" second]
            [branch]
            result
    match elaborate [source] with
    | Ok analysis ->
        match findFunction "branch" analysis with
        | Some owned ->
            match owned.Definition.Body.Body.Operations with
            | [Evaluate (HIR.Branch (_, _, yes, no))]
                when yes.Body.Operations = [Drop second.Id; Evaluate (leaf [first, true] [yesResult])]
                     && no.Body.Operations = [Drop first.Id; Evaluate (leaf [second, true] [noResult])] -> Ok ()
            | actual -> Error $"Expected branch-edge cleanup for the opposite consumed input, got {actual}"
        | None -> Error "Elaboration omitted the branch function"
    | Error error -> Error error

let private testPreservesLiveBranchResults () =
    let first = managed 0
    let second = managed 1
    let selected = managed 2
    let result = unitValue 3
    let branch =
        HIR.Branch (
            selected,
            { Expression = CheckedAST.BoolLiteral true; Type = AST.TBool; Inputs = Map.empty },
            block [] [] first,
            block [] [] second)
    let source =
        definition
            "select"
            [parameter "first" first; parameter "second" second]
            [branch; scalar result ["first", first; "second", second]]
            result
    match elaborate [source] with
    | Ok analysis ->
        match findFunction "select" analysis with
        | Some owned ->
            match owned.Definition.Body.Body.Operations with
            | Evaluate (HIR.Branch (_, _, yes, no)) :: _
                when yes.Body.Operations = [Dup first.Id]
                     && no.Body.Operations = [Dup second.Id] -> Ok ()
            | actual -> Error $"Expected branch results that remain live to be duplicated on each edge, got {actual}"
        | None -> Error "Elaboration omitted the select function"
    | Error error -> Error error

let private testDuplicatesLiveValueBeforeConsumingCall () =
    let sinkInput = managed 0
    let sinkResult = unitValue 1
    let sink =
        definition "sink" [parameter "input" sinkInput] [leaf [sinkInput, true] [sinkResult]] sinkResult
    let input = managed 0
    let callResult = unitValue 1
    let result = unitValue 2
    let caller =
        definition
            "caller"
            [parameter "input" input]
            [call "sink" [input] callResult; scalar result ["input", input]]
            result
    match elaborate [sink; caller] with
    | Ok analysis ->
        match findFunction "caller" analysis with
        | Some owned ->
            match owned.Definition.Body.Body.Operations with
            | [Dup id; Evaluate (HIR.Call _); Evaluate (HIR.ScalarBinding _); Drop dropped]
                when id = input.Id && dropped = input.Id -> Ok ()
            | actual -> Error $"Expected a duplicate before the consuming call and cleanup after the final borrow, got {actual}"
        | None -> Error "Elaboration omitted the caller"
    | Error error -> Error error

let private testCleansUnusedManagedResults () =
    let sourceInput = managed 0
    let produced = managed 1
    let callee =
        definition
            "produce"
            [parameter "input" sourceInput]
            [scalar produced ["input", sourceInput]]
            produced
    let input = managed 0
    let unused = managed 1
    let result = unitValue 2
    let caller =
        definition
            "discard"
            [parameter "input" input]
            [call "produce" [input] unused; leaf [] [result]]
            result
    let scalarParameter = unitValue 0
    let unusedScalar = managed 1
    let scalarResult = unitValue 2
    let scalarDiscard =
        definition
            "discardScalar"
            [parameter "unit" scalarParameter]
            [scalar unusedScalar []; leaf [] [scalarResult]]
            scalarResult
    match elaborate [callee; caller; scalarDiscard] with
    | Ok analysis ->
        match findFunction "discard" analysis, findFunction "discardScalar" analysis with
        | Some callOwned, Some scalarOwned ->
            match callOwned.Definition.Body.Body.Operations, scalarOwned.Definition.Body.Body.Operations with
            | [Evaluate (HIR.Call _); Drop callId; Evaluate (HIR.Leaf _)],
              [Evaluate (HIR.ScalarBinding _); Drop scalarId; Evaluate (HIR.Leaf _)]
                when callId = unused.Id && scalarId = unusedScalar.Id -> Ok ()
            | actual -> Error $"Expected immediate cleanup of unused call and scalar results, got {actual}"
        | _ -> Error "Elaboration omitted a discard function"
    | Error error -> Error error

let private testStabilizesRecursiveBoundaries () =
    let input = managed 0
    let recursiveResult = managed 1
    let loop =
        definition
            "loop"
            [parameter "input" input]
            [call "loop" [input] recursiveResult]
            recursiveResult
    match elaborate [loop] with
    | Ok analysis ->
        match findFunction "loop" analysis with
        | Some owned when owned.Ownership = {
                Parameters = [ConsumedParameter input.Id]
                Result = ProducedResult recursiveResult.Id
            } -> Ok ()
        | actual -> Error $"Expected the recursive boundary to reach a stable transfer contract, got {actual}"
    | Error error -> Error error

let private testStabilizesMutualRecursiveBoundaries () =
    let firstInput = managed 0
    let firstResult = unitValue 1
    let first =
        definition
            "first"
            [parameter "input" firstInput]
            [call "second" [firstInput] firstResult]
            firstResult
    let secondInput = managed 0
    let secondResult = unitValue 1
    let second =
        definition
            "second"
            [parameter "input" secondInput]
            [leaf [secondInput, true] [secondResult]]
            secondResult
    match elaborate [first; second] with
    | Ok analysis ->
        let consumed name (input: HIR.Value) =
            findFunction name analysis
            |> Option.exists (fun owned ->
                owned.Ownership = {
                    Parameters = [ConsumedParameter input.Id]
                    Result = UnmanagedResult
                })
        if consumed "first" firstInput && consumed "second" secondInput then Ok ()
        else Error "Expected consumption to stabilize across the mutually visible call group"
    | Error error -> Error error

let private testStabilizesActualMutualRecursion () =
    let firstInput = managed 0
    let firstResult = unitValue 1
    let first =
        definition
            "recursiveFirst"
            [parameter "input" firstInput]
            [call "recursiveSecond" [firstInput] firstResult]
            firstResult
    let secondInput = managed 0
    let secondResult = unitValue 1
    let second =
        definition
            "recursiveSecond"
            [parameter "input" secondInput]
            [call "recursiveFirst" [secondInput] secondResult]
            secondResult
    match elaborate [first; second] with
    | Ok analysis ->
        let consumed name (input: HIR.Value) =
            findFunction name analysis
            |> Option.exists (fun owned ->
                owned.Ownership = {
                    Parameters = [ConsumedParameter input.Id]
                    Result = UnmanagedResult
                })
        if consumed "recursiveFirst" firstInput
           && consumed "recursiveSecond" secondInput then Ok ()
        else Error "Expected a mutually recursive ownership group to converge atomically"
    | Error error -> Error error

let private testInfersDeepAcyclicBoundariesCalleeFirst () =
    let functionCount = 256
    let name index = sprintf "ownershipChain%04i" index
    let definitions =
        [0 .. functionCount - 1]
        |> List.map (fun index ->
            let input = managed 0
            let result = unitValue 1
            let operations =
                if index + 1 < functionCount then
                    [call (name (index + 1)) [input] result]
                else
                    [scalar result ["input", input]]
            definition (name index) [parameter "input" input] operations result)
    match elaborate definitions with
    | Ok analysis ->
        let everyBoundaryBorrowed =
            ElaborateFunctionOwnership.functions analysis
            |> List.forall (fun owned ->
                owned.Ownership = {
                    Parameters = [BorrowedParameter (HIR.ValueId 0)]
                    Result = UnmanagedResult
                })
        if everyBoundaryBorrowed then Ok ()
        else Error "Expected deep acyclic ownership boundaries to propagate callee-first"
    | Error error -> Error error

let private testRejectsMissingCallOwnership () =
    let input = managed 0
    let result = managed 1
    let source =
        definition
            "caller"
            [parameter "input" input]
            [call "missing" [input] result]
            result
    match ElaborateFunctionOwnership.elaborateFunctions dialect [source] with
    | Error (ElaborateFunctionOwnership.UnknownCallOwnership target)
        when target = TestIds.functionIdForName "missing" -> Ok ()
    | actual -> Error $"Expected explicit rejection of missing call ownership, got {actual}"

let private testRejectsMismatchedCallOwnership () =
    let input = managed 0
    let result = unitValue 1
    let target = TestIds.functionIdForName "external"
    let source =
        definition
            "caller"
            [parameter "input" input]
            [call "external" [input] result]
            result
    let mismatchedDialect = {
        dialect with
            ExternalCallOwnership = fun call ->
                if call.Target = target then
                    Some { Parameters = [UnmanagedCallParameter]; Result = UnmanagedCallResult }
                else None
    }
    match ElaborateFunctionOwnership.elaborateFunctions mismatchedDialect [source] with
    | Error (ElaborateFunctionOwnership.InconsistentCallParameters actual) when actual = target -> Ok ()
    | actual -> Error $"Expected explicit rejection of mismatched call ownership, got {actual}"

let tests = [
    "Whole-function ownership infers borrowed opaque inputs", testInfersBorrowedOpaqueInput
    "Whole-function ownership infers consumed inputs", testInfersConsumedInput
    "Whole-function ownership inserts branch-edge cleanup", testDropsOppositeBranchInputs
    "Whole-function ownership preserves live branch results", testPreservesLiveBranchResults
    "Whole-function ownership duplicates values live after consuming calls", testDuplicatesLiveValueBeforeConsumingCall
    "Whole-function ownership cleans unused managed results", testCleansUnusedManagedResults
    "Whole-function ownership stabilizes recursive boundaries", testStabilizesRecursiveBoundaries
    "Whole-function ownership stabilizes mutual call boundaries", testStabilizesMutualRecursiveBoundaries
    "Whole-function ownership stabilizes an actual recursive group", testStabilizesActualMutualRecursion
    "Whole-function ownership infers a deep call chain callee-first", testInfersDeepAcyclicBoundariesCalleeFirst
    "Whole-function ownership rejects calls without contracts", testRejectsMissingCallOwnership
    "Whole-function ownership rejects mismatched call contracts", testRejectsMismatchedCallOwnership
]
