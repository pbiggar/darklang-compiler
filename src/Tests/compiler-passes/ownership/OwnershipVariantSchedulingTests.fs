// OwnershipVariantSchedulingTests.fs - Fixed-point ownership specialization scheduling laws.

module OwnershipVariantSchedulingTests

open OwnedIR

type private Leaf = Fresh of HIR.Value

let private fid = TestIds.functionIdForName
let private value id : HIR.Value = { Id = HIR.ValueId id; Type = AST.TList AST.TInt64 }
let private parameter (value: HIR.Value) : HIR.Parameter = {
    Name = sprintf "value%i" (match value.Id with HIR.ValueId id -> id)
    Binding = AST.bindingId (match value.Id with HIR.ValueId id -> id)
    Value = value
}
let private signature parameters result : FunctionSignature<HIR.ValueId> = {
    Parameters = parameters
    Result = result
}
let private block parameters operations result : Block<Leaf, HIR.ValueId> = {
    Body = { Parameters = List.map parameter parameters; Operations = operations; Result = result }
}
let private definition name ownership body : Function<Leaf, HIR.ValueId> = {
    Definition = { Id = fid name; Name = name; Body = body }
    Ownership = ownership
}
let private call target argument result : HIR.FunctionCall = {
    Target = fid target
    Arguments = [argument]
    Result = result
}

let private condition : HIR.Operand = {
    Expression = CheckedAST.BoolLiteral true
    Type = AST.TBool
    Inputs = Map.empty
}

let private semantics : Semantics<Leaf, HIR.ValueId> = {
    Leaf = fun (Fresh output) -> { Inputs = []; Outputs = [output.Id] }
    LeafUniqueness = fun (Fresh output) -> {
        RequiredInputs = Set.empty
        UniqueOutputs = Set.singleton output.Id
    }
    CallOwnership = fun _ -> None
    ScalarUses = fun operand -> operand.Inputs |> Map.values |> Seq.map (fun input -> input.Id) |> Set.ofSeq
    ScalarEscapes = fun _ -> Set.empty
    BlockArgument = fun input ->
        if input.Type = AST.TUnit then Unmanaged else Managed input.Id
}

let private contracts : VerifyOwnedHIR.HIRContracts<Leaf> = {
    Leaf = fun (Fresh output) -> {
        Inputs = []
        Operands = []
        Outputs = [{ Value = output; Alias = HIR.FreshManaged }]
        Effects = Set.singleton HIR.MayAllocate
    }
    CallSignature = fun _ -> None
    CallContract = fun invocation ->
        match invocation.Arguments with
        | first :: rest -> Some {
            Inputs = invocation.Arguments
            Operands = []
            Outputs = [{ Value = invocation.Result; Alias = HIR.MayAliasInputs (first, rest) }]
            Effects = Set.singleton HIR.MayInvokeUserCode
          }
        | [] -> None
}

let private fixture () =
    let calleeInput = value 0
    let identity =
        definition "identity"
            (signature [ConsumedParameter calleeInput.Id] (ProducedResult calleeInput.Id))
            (block [calleeInput] [] calleeInput)
    let input, middle, output = value 10, value 11, value 12
    let first, second = call "identity" input middle, call "identity" middle output
    let caller =
        definition "caller"
            (signature [UniqueParameter input.Id] (ProducedResult output.Id))
            (block [input] [Evaluate (HIR.Call first); Evaluate (HIR.Call second)] output)
    [identity; caller], first, second

let private testPropagatesUniquenessToFixedPoint () =
    let definitions, first, second = fixture ()
    ScheduleOwnershipVariants.schedule
        ScheduleOwnershipVariants.defaultLimits
        contracts
        semantics
        Map.empty
        definitions
    |> Result.mapError (sprintf "%A")
    |> Result.bind (fun plan ->
        let sites =
            ScheduleOwnershipVariants.materialization plan
            |> MaterializeOwnershipVariants.rewrites
            |> List.map (fun rewrite -> rewrite.Site.Result)
            |> Set.ofList
        if sites = Set.ofList [first.Result.Id; second.Result.Id]
           && List.length (ScheduleOwnershipVariants.iterations plan) >= 2 then Ok ()
        else Error (sprintf "Expected both calls to specialize across iterations, got %A" sites))

let private testBoundsConvergence () =
    let definitions, _, _ = fixture ()
    let limits = { ScheduleOwnershipVariants.defaultLimits with MaxIterations = 1 }
    match ScheduleOwnershipVariants.schedule limits contracts semantics Map.empty definitions with
    | Error (ScheduleOwnershipVariants.IterationLimitExceeded 1) -> Ok ()
    | actual -> Error (sprintf "Expected the scheduler iteration bound, got %A" actual)

let private testSkipsUnusedWideVariantSearch () =
    let unitInput : HIR.Value = { Id = HIR.ValueId 100; Type = AST.TUnit }
    let managedInputs = [0 .. 8] |> List.map value
    let wide =
        definition
            "unusedWide"
            (signature
                (UnmanagedParameter
                 :: (managedInputs |> List.map (fun input -> ConsumedParameter input.Id)))
                UnmanagedResult)
            (block
                (unitInput :: managedInputs)
                (managedInputs |> List.map (fun input -> Drop input.Id))
                unitInput)
    ScheduleOwnershipVariants.schedule
        ScheduleOwnershipVariants.defaultLimits
        contracts
        semantics
        Map.empty
        [wide]
    |> Result.mapError (sprintf "%A")
    |> Result.bind (fun plan ->
        let materialized = ScheduleOwnershipVariants.materialization plan
        if List.isEmpty (MaterializeOwnershipVariants.groups materialized)
           && List.isEmpty (MaterializeOwnershipVariants.rewrites materialized) then Ok ()
        else Error "Expected an uncalled wide function to produce no ownership variants")

let private testSchedulesRecursiveDemandAtomically () =
    let loopInput, recursiveResult, loopResult = value 20, value 21, value 22
    let recursiveCall = call "loop" loopInput recursiveResult
    let recursiveBranch = block [] [Evaluate (HIR.Call recursiveCall)] recursiveResult
    let baseBranch = block [] [] loopInput
    let loop =
        definition
            "loop"
            (signature [ConsumedParameter loopInput.Id] (ProducedResult loopResult.Id))
            (block
                [loopInput]
                [Evaluate (HIR.Branch (loopResult, condition, recursiveBranch, baseBranch))]
                loopResult)
    let callerInput, callerResult = value 30, value 31
    let externalCall = call "loop" callerInput callerResult
    let caller =
        definition
            "recursiveCaller"
            (signature [UniqueParameter callerInput.Id] (ProducedResult callerResult.Id))
            (block [callerInput] [Evaluate (HIR.Call externalCall)] callerResult)
    ScheduleOwnershipVariants.schedule
        ScheduleOwnershipVariants.defaultLimits
        contracts
        semantics
        Map.empty
        [loop; caller]
    |> Result.mapError (sprintf "%A")
    |> Result.bind (fun plan ->
        let materialized = ScheduleOwnershipVariants.materialization plan
        match MaterializeOwnershipVariants.groups materialized,
              MaterializeOwnershipVariants.rewrites materialized with
        | [group], [rewrite]
            when List.length (AST.NonEmptyList.toList group.Members) = 1
                 && rewrite.Site.Result = externalCall.Result.Id
                 && rewrite.Ownership = {
                     Parameters = [UniqueCallParameter]
                     Result = UniqueProducedCallResult
                 } -> Ok ()
        | actual -> Error (sprintf "Expected one atomic recursive specialization, got %A" actual))

let private testLowersSpecializedCallsAndContracts () =
    let definitions, _, _ = fixture ()
    ScheduleOwnershipVariants.schedule
        ScheduleOwnershipVariants.defaultLimits
        contracts
        semantics
        Map.empty
        definitions
    |> Result.mapError (sprintf "%A")
    |> Result.bind (fun scheduled ->
        let identity : ANF.Function = {
            Id = fid "identity"
            Name = "identity"
            TypedParams = [{ Id = ANF.TempId 0; Type = AST.TList AST.TInt64 }]
            ReturnType = AST.TList AST.TInt64
            ReturnOwnership = ANF.OwnedReturn
            Body = ANF.Return (ANF.Var (ANF.TempId 0))
        }
        let caller : ANF.Function = {
            Id = fid "caller"
            Name = "caller"
            TypedParams = [{ Id = ANF.TempId 10; Type = AST.TList AST.TInt64 }]
            ReturnType = AST.TList AST.TInt64
            ReturnOwnership = ANF.OwnedReturn
            Body =
                ANF.Let (
                    ANF.TempId 11,
                    ANF.Call (fid "identity", [ANF.Var (ANF.TempId 10)]),
                    ANF.Let (
                        ANF.TempId 12,
                        ANF.Call (fid "identity", [ANF.Var (ANF.TempId 11)]),
                        ANF.Return (ANF.Var (ANF.TempId 12))))
        }
        LowerOwnershipVariants.lower
            definitions
            (ScheduleOwnershipVariants.materialization scheduled)
            [identity; caller]
            (ANF.VarGen 100)
            Set.empty
        |> Result.mapError (sprintf "%A")
        |> Result.bind (fun lowered ->
            let cloneContracts = lowered.Contracts |> Map.toList
            let rewrittenTargets =
                lowered.Functions
                |> List.tryFind (fun functionDefinition -> functionDefinition.Id = caller.Id)
                |> Option.map (fun functionDefinition ->
                    let rec targets = function
                        | ANF.Let (_, ANF.Call (target, _), body) -> target :: targets body
                        | ANF.Let (_, _, body) -> targets body
                        | ANF.If (_, yes, no) -> targets yes @ targets no
                        | ANF.Join (_, continuation, entry) -> targets entry @ targets continuation
                        | ANF.Return _ | ANF.Jump _ -> []
                    targets functionDefinition.Body)
                |> Option.defaultValue []
            match cloneContracts, rewrittenTargets with
            | [(clone, boundary)], [first; second]
                when clone = first
                     && first = second
                     && boundary = {
                         Parameters = [UniqueCallParameter]
                         Result = UniqueProducedCallResult
                     }
                     && List.length lowered.Functions = 3 -> Ok ()
            | actual -> Error (sprintf "Expected one explicit ANF clone contract and two routed calls, got %A" actual)))

let tests = [
    "Ownership specialization propagates uniqueness to a fixed point", testPropagatesUniquenessToFixedPoint
    "Ownership specialization has an explicit convergence bound", testBoundsConvergence
    "Ownership specialization skips unused wide functions", testSkipsUnusedWideVariantSearch
    "Ownership specialization schedules recursive demand atomically", testSchedulesRecursiveDemandAtomically
    "Ownership specialization lowers clones calls and contracts into ANF", testLowersSpecializedCallsAndContracts
]
