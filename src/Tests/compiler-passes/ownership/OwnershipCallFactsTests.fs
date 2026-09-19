// OwnershipCallFactsTests.fs - Call-point uniqueness laws and specialization handoff.

module OwnershipCallFactsTests

open OwnedIR

type private Leaf = Fresh of HIR.Value
type private External = HIR.FunctionSignature * CallSignature

let private fid = AST.functionIdForName
let private managed id : HIR.Value = { Id = HIR.ValueId id; Type = AST.TList AST.TInt64 }
let private scalar id : HIR.Value = { Id = HIR.ValueId id; Type = AST.TUnit }
let private unitValue = scalar 99
let private parameter (value: HIR.Value) : HIR.Parameter =
    let (HIR.ValueId id) = value.Id
    { Name = $"v{id}"; Binding = AST.bindingId id; Value = value }
let private block parameters operations result : Block<Leaf, HIR.ValueId> = {
    Body = { Parameters = parameters; Operations = operations; Result = result }
}
let private definition name parameters operations result resultOwnership : Function<Leaf, HIR.ValueId> = {
    Definition = {
        Id = fid name; Name = name
        Body = block (parameters |> List.map (fst >> parameter)) operations result
    }
    Ownership = { Parameters = List.map snd parameters; Result = resultOwnership }
}
let private unitFunction name parameters operations =
    definition name (parameters @ [unitValue, UnmanagedParameter]) operations unitValue UnmanagedResult
let private call target arguments result : HIR.FunctionCall = { Target = fid target; Arguments = arguments; Result = result }
let private invoke call = Evaluate (HIR.Call call)
let private external (call: HIR.FunctionCall) modes result : AST.FunctionId * External =
    call.Target,
    ({ Parameters = List.map (fun (value: HIR.Value) -> value.Type) call.Arguments; Result = call.Result.Type },
     { Parameters = modes; Result = result })

let private ownership registrations aliases : Semantics<Leaf, HIR.ValueId> =
    let argument (value: HIR.Value) =
        match value.Type with
        | AST.TList _ -> Managed (Map.tryFind value.Id aliases |> Option.defaultValue value.Id)
        | _ -> Unmanaged
    let uses (operand: HIR.Operand) =
        operand.Inputs |> Map.values
        |> Seq.choose (fun value -> match argument value with Managed id -> Some id | Unmanaged -> None)
        |> Set.ofSeq
    {
        Leaf = fun (Fresh value) -> { Inputs = []; Outputs = [value.Id] }
        LeafUniqueness = fun (Fresh value) -> { RequiredInputs = Set.empty; UniqueOutputs = Set.singleton value.Id }
        CallOwnership = fun call -> Map.tryFind call.Target registrations |> Option.map snd
        ScalarUses = uses
        ScalarEscapes = uses
        BlockArgument = argument
    }

let private hir registrations : VerifyOwnedHIR.HIRContracts<Leaf> = {
    Leaf = fun (Fresh value) -> {
        Inputs = []; Operands = []; Outputs = [{ Value = value; Alias = HIR.FreshManaged }]
        Effects = Set.singleton HIR.MayAllocate
    }
    CallSignature = fun target -> Map.tryFind target registrations |> Option.map fst
    CallContract = fun call ->
        let alias =
            match call.Result.Type, call.Arguments with
            | AST.TList _, first :: rest -> HIR.MayAliasInputs (first, rest)
            | AST.TList _, [] -> HIR.FreshManaged
            | _ -> HIR.NoManagedAlias
        Some {
            Inputs = call.Arguments; Operands = []
            Outputs = [{ Value = call.Result; Alias = alias }]
            Effects = Set.singleton HIR.MayInvokeUserCode
        }
}
let private analyze registrations aliases definitions =
    let registrations = Map.ofList registrations
    VerifyOwnedHIR.analyzeFunctions (hir registrations) (ownership registrations aliases) definitions
let private expect expected result =
    match result with
    | Error error -> Error (sprintf "Unexpected analysis failure: %A" error)
    | Ok facts ->
        let actual = facts |> List.map (fun facts -> facts.Call.Result.Id, facts.UniqueArguments)
        if actual = expected then Ok () else Error (sprintf "Expected call facts %A, got %A" expected actual)
let private at (call: HIR.FunctionCall) positions = call.Result.Id, Set.ofList positions

let private testParameterModes () =
    let unique, consumed, borrowed = managed 0, managed 1, managed 2
    let invocation = call "inspect" [unitValue; unique; consumed; borrowed] (scalar 10)
    let modes = [UnmanagedCallParameter; BorrowedCallParameter; BorrowedCallParameter; BorrowedCallParameter]
    let registration = external invocation modes UnmanagedCallResult
    let caller = unitFunction "caller"
                     [unique, UniqueParameter unique.Id; consumed, ConsumedParameter consumed.Id; borrowed, BorrowedParameter borrowed.Id]
                     [invoke invocation; Drop unique.Id; Drop consumed.Id]
    analyze [registration] Map.empty [caller]
    |> Result.bind (fun facts ->
        match facts with
        | [fact] when fact.Caller = caller.Definition.Id && fact.Call = invocation && fact.Established = snd (snd registration)
                      && callSiteIdentity fact = { Caller = caller.Definition.Id; Result = invocation.Result.Id } -> Ok facts
        | _ -> Error (VerifyOwnedHIR.OwnershipVerificationFailed InconsistentFunctionParameters))
    |> expect [at invocation [1]]

let private testPreTransferFacts () =
    let input, output = managed 0, managed 1
    let transfer = call "transfer" [input] output
    let inspect = call "inspect" [output] (scalar 10)
    let caller = unitFunction "caller" [input, UniqueParameter input.Id] [invoke transfer; invoke inspect; Drop output.Id]
    analyze [external transfer [ConsumedCallParameter] ProducedCallResult; external inspect [BorrowedCallParameter] UnmanagedCallResult]
        Map.empty [caller]
    |> expect [at transfer [0]; at inspect []]

let private testDupDrop () =
    let input = managed 0
    let before = call "inspect" [input] (scalar 10)
    let during = call "inspect" [input] (scalar 11)
    let after = call "inspect" [input] (scalar 12)
    let caller = unitFunction "caller" [input, UniqueParameter input.Id]
                     [invoke before; Dup input.Id; invoke during; Drop input.Id; invoke after; Drop input.Id]
    analyze [external before [BorrowedCallParameter] UnmanagedCallResult] Map.empty [caller]
    |> expect [at before [0]; at during []; at after [0]]

let private testRepeatedConsumption () =
    let input = managed 0
    let invocation = call "consumeBoth" [input; input] (scalar 10)
    let caller = unitFunction "caller" [input, UniqueParameter input.Id] [Dup input.Id; invoke invocation]
    analyze [external invocation [ConsumedCallParameter; ConsumedCallParameter] UnmanagedCallResult] Map.empty [caller]
    |> expect [at invocation []]

let private testFreshAndUniqueResults () =
    let fresh, produced = managed 0, managed 1
    let inspectFresh = call "inspect" [fresh] (scalar 10)
    let make = call "make" [] produced
    let inspectProduced = call "inspect" [produced] (scalar 11)
    let caller = unitFunction "caller" []
                     [Evaluate (HIR.Leaf (Fresh fresh)); invoke inspectFresh; Drop fresh.Id; invoke make; invoke inspectProduced; Drop produced.Id]
    analyze [external inspectFresh [BorrowedCallParameter] UnmanagedCallResult; external make [] UniqueProducedCallResult]
        Map.empty [caller]
    |> expect [at inspectFresh [0]; at make []; at inspectProduced [0]]

let private testEscapes () =
    let input = managed 0
    let before, after = call "inspect" [input] (scalar 10), call "inspect" [input] (scalar 11)
    let escape : HIR.Operand = {
        Expression = CheckedAST.UnitLiteral; Type = AST.TUnit
        Inputs = Map.ofList [AST.bindingId 0, input]
    }
    let caller = unitFunction "caller" [input, UniqueParameter input.Id]
                     [invoke before; Evaluate (HIR.ScalarBinding (scalar 12, escape)); Dup input.Id; Drop input.Id; invoke after; Drop input.Id]
    analyze [external before [BorrowedCallParameter] UnmanagedCallResult] Map.empty [caller]
    |> expect [at before [0]; at after []]

let private testBorrowedAlias () =
    let input, alias = managed 0, managed 1
    let borrow = call "borrow" [input] alias
    let inspect = call "inspect" [alias] (scalar 10)
    let caller = unitFunction "caller" [input, UniqueParameter input.Id] [invoke borrow; invoke inspect; Drop input.Id]
    analyze [external borrow [BorrowedCallParameter] (BorrowedCallResult 0); external inspect [BorrowedCallParameter] UnmanagedCallResult]
        (Map.ofList [alias.Id, input.Id]) [caller]
    |> expect [at borrow [0]; at inspect [0]]

let private branchFixture bothUnique =
    let yesValue, noValue, joined = managed 0, managed 1, managed 2
    let yesInspect, noInspect, after = call "inspect" [yesValue] (scalar 10), call "inspect" [noValue] (scalar 11), call "inspect" [joined] (scalar 12)
    let make = call "make" [] noValue
    let yes = block [] [Evaluate (HIR.Leaf (Fresh yesValue)); invoke yesInspect] yesValue
    let no = block [] [invoke make; invoke noInspect] noValue
    let condition : HIR.Operand = { Expression = CheckedAST.BoolLiteral true; Type = AST.TBool; Inputs = Map.empty }
    let caller = unitFunction "caller" [] [Evaluate (HIR.Branch (joined, condition, yes, no)); invoke after; Drop joined.Id]
    let registrations = [external yesInspect [BorrowedCallParameter] UnmanagedCallResult;
                         external make [] (if bothUnique then UniqueProducedCallResult else ProducedCallResult)]
    caller, registrations, [at yesInspect [0]; at make []; at noInspect (if bothUnique then [0] else []); at after (if bothUnique then [0] else [])]

let private testBranchJoins bothUnique () =
    let caller, registrations, expected = branchFixture bothUnique
    analyze registrations Map.empty [caller] |> expect expected

let private testBranchStateIsolation () =
    let input = managed 0
    let yesCall, noCall, after = call "inspect" [input] (scalar 10), call "inspect" [input] (scalar 11), call "inspect" [input] (scalar 12)
    let condition : HIR.Operand = { Expression = CheckedAST.BoolLiteral false; Type = AST.TBool; Inputs = Map.empty }
    let yes = block [] [Dup input.Id; invoke yesCall; Drop input.Id] unitValue
    let no = block [] [invoke noCall] unitValue
    let caller = unitFunction "caller" [input, UniqueParameter input.Id]
                     [Evaluate (HIR.Branch (scalar 13, condition, yes, no)); invoke after; Drop input.Id]
    analyze [external yesCall [BorrowedCallParameter] UnmanagedCallResult] Map.empty [caller]
    |> expect [at yesCall []; at noCall [0]; at after [0]]

let private testAliasedArguments () =
    let input, alias = managed 0, managed 1
    let borrow = call "borrow" [input] alias
    let both = call "inspectBoth" [input; alias] (scalar 10)
    let after = call "inspect" [input] (scalar 11)
    let caller = unitFunction "caller" [input, UniqueParameter input.Id] [invoke borrow; invoke both; invoke after; Drop input.Id]
    analyze [external borrow [BorrowedCallParameter] (BorrowedCallResult 0);
             external both [BorrowedCallParameter; BorrowedCallParameter] UnmanagedCallResult;
             external after [BorrowedCallParameter] UnmanagedCallResult]
        (Map.ofList [alias.Id, input.Id]) [caller]
    |> expect [at borrow [0]; at both []; at after [0]]

let private testRejectsUniqueAliasedArgument () =
    let input = managed 0
    let invocation = call "consumeAndBorrow" [input; input] (scalar 10)
    let caller = unitFunction "caller" [input, UniqueParameter input.Id] [invoke invocation]
    match analyze [external invocation [UniqueCallParameter; BorrowedCallParameter] UnmanagedCallResult] Map.empty [caller] with
    | Error (VerifyOwnedHIR.OwnershipVerificationFailed (NonUniqueUse id)) when id = input.Id -> Ok ()
    | actual -> Error (sprintf "Expected an argument also borrowed by the call to reject unique consumption, got %A" actual)

let private testFailureDiscardsFacts () =
    let input = managed 0
    let invocation = call "inspect" [input] (scalar 10)
    let caller = unitFunction "caller" [input, UniqueParameter input.Id] [invoke invocation]
    let registrations = Map.ofList [external invocation [BorrowedCallParameter] UnmanagedCallResult]
    let semantics = ownership registrations Map.empty
    match VerifyOwnership.analyzeFunction semantics caller,
          VerifyOwnership.verifyFunction semantics caller.Ownership caller.Definition.Body,
          VerifyOwnedHIR.analyzeFunctions (hir registrations) semantics [caller] with
    | Error first, Error second, Error (VerifyOwnedHIR.OwnershipVerificationFailed third)
        when first = UndroppedValues (Set.singleton input.Id) && first = second && second = third -> Ok ()
    | actual -> Error (sprintf "Expected no fact result after final ownership validation fails, got %A" actual)

let private testOwnedResultFromBorrow resultMode expectedPositions () =
    let input, output = managed 0, managed 1
    let produce = call "produce" [input] output
    let inspect = call "inspect" [input] (scalar 10)
    let caller = unitFunction "caller" [input, UniqueParameter input.Id]
                     [invoke produce; invoke inspect; Drop output.Id; Drop input.Id]
    analyze [external produce [BorrowedCallParameter] resultMode; external inspect [BorrowedCallParameter] UnmanagedCallResult]
        Map.empty [caller]
    |> expect [at produce [0]; at inspect expectedPositions]

let private testOwnedResultFromDuplicatedInput () =
    let input, output = managed 0, managed 1
    let transfer = call "transfer" [input] output
    let beforeDrop, afterDrop = call "inspect" [input] (scalar 10), call "inspect" [input] (scalar 11)
    let caller = unitFunction "caller" [input, UniqueParameter input.Id]
                     [Dup input.Id; invoke transfer; invoke beforeDrop; Drop output.Id; invoke afterDrop; Drop input.Id]
    analyze [external transfer [ConsumedCallParameter] ProducedCallResult; external beforeDrop [BorrowedCallParameter] UnmanagedCallResult]
        Map.empty [caller]
    |> expect [at transfer []; at beforeDrop []; at afterDrop []]

let private testRejectsUniqueUseAfterAliasingResult () =
    let input, output = managed 0, managed 1
    let produce = call "produce" [input] output
    let consume = call "consumeUnique" [input] (scalar 10)
    let caller = unitFunction "caller" [input, UniqueParameter input.Id] [invoke produce; invoke consume; Drop output.Id]
    let registrations = Map.ofList [external produce [BorrowedCallParameter] ProducedCallResult;
                                    external consume [UniqueCallParameter] UnmanagedCallResult]
    let semantics = ownership registrations Map.empty
    match VerifyOwnership.analyzeFunction semantics caller,
          VerifyOwnership.verifyFunction semantics caller.Ownership caller.Definition.Body with
    | Error (NonUniqueUse first), Error (NonUniqueUse second) when first = input.Id && first = second -> Ok ()
    | actual -> Error (sprintf "Expected both verification and analysis to reject an aliased unique input, got %A" actual)

let private testFunctionScopedSites () =
    let input = managed 0
    let invocation = call "inspect" [input] (scalar 10)
    let first = unitFunction "first" [input, UniqueParameter input.Id] [invoke invocation; Drop input.Id]
    let second = unitFunction "second" [input, ConsumedParameter input.Id] [invoke invocation; Drop input.Id]
    match analyze [external invocation [BorrowedCallParameter] UnmanagedCallResult] Map.empty [first; second] with
    | Ok [firstFacts; secondFacts] when firstFacts.UniqueArguments = Set.singleton 0 && Set.isEmpty secondFacts.UniqueArguments
                                     && callSiteIdentity firstFacts <> callSiteIdentity secondFacts -> Ok ()
    | actual -> Error (sprintf "Expected function-local values to retain distinct call-site identities, got %A" actual)

let private testRecursiveContracts self () =
    let input, output = managed 0, managed 1
    let firstCall = call (if self then "first" else "second") [input] output
    let secondCall = call "first" [input] output
    let recursive name invocation =
        definition name [input, UniqueParameter input.Id] [invoke invocation] output (UniqueProducedResult output.Id)
    let definitions =
        if self then [recursive "first" firstCall]
        else [recursive "first" firstCall; recursive "second" secondCall]
    match analyze [] Map.empty definitions with
    | Ok facts when List.length facts = List.length definitions
                    && List.forall (fun fact -> fact.UniqueArguments = Set.singleton 0
                                               && fact.Established = { Parameters = [UniqueCallParameter]; Result = UniqueProducedCallResult }) facts -> Ok ()
    | actual -> Error (sprintf "Expected recursive call facts to use the group's derived boundaries, got %A" actual)

let private testConflictingRegistration () =
    let input = managed 0
    let identity = definition "identity" [input, ConsumedParameter input.Id] [] input (ProducedResult input.Id)
    let invocation = call "identity" [input] input
    match analyze [external invocation [BorrowedCallParameter] ProducedCallResult] Map.empty [identity] with
    | Error (VerifyOwnedHIR.OwnershipVerificationFailed (InconsistentRegisteredCallOwnership target))
        when target = identity.Definition.Id -> Ok ()
    | actual -> Error (sprintf "Expected conflicting registered ownership to prevent analysis, got %A" actual)

let private testTypedFailure () =
    let input = managed 0
    let invocation = call "inspect" [input] (scalar 10)
    let caller = unitFunction "caller" [input, UniqueParameter input.Id] [invoke invocation; Drop input.Id]
    let registrations = Map.ofList [external invocation [BorrowedCallParameter] UnmanagedCallResult]
    let contracts = { hir registrations with CallSignature = fun _ -> Some { Parameters = [AST.TInt64]; Result = AST.TUnit } }
    let semantics = ownership registrations Map.empty
    match VerifyOwnedHIR.analyzeFunctions contracts semantics [caller], VerifyOwnedHIR.verifyFunctions contracts semantics [caller] with
    | Error (VerifyOwnedHIR.HIRVerificationFailed first), Error (VerifyOwnedHIR.HIRVerificationFailed second) when first = second -> Ok ()
    | actual -> Error (sprintf "Expected the same typed-HIR error before collecting facts, got %A" actual)

let private testSpecializationHandoff () =
    let input, output = managed 0, managed 1
    let identity = definition "identity" [input, ConsumedParameter input.Id] [] input (ProducedResult input.Id)
    let invocation = call "identity" [input] output
    let caller = definition "caller" [input, UniqueParameter input.Id] [invoke invocation] output (ProducedResult output.Id)
    let definitions = [identity; caller]
    let semantics, contracts = ownership Map.empty Map.empty, hir Map.empty
    let report result = result |> Result.mapError (sprintf "%A")
    VerifyOwnedHIR.analyzeFunctions contracts semantics definitions |> report
    |> Result.bind (fun facts ->
        match facts with
        | [fact] ->
            InferOwnedFunctionGroups.infer semantics [identity] |> report
            |> Result.bind (SelectOwnershipVariants.create >> report)
            |> Result.bind (fun catalog ->
                SelectOwnershipVariants.select catalog
                    { Target = identity.Definition.Name; Established = fact.Established; UniqueArguments = fact.UniqueArguments } |> report)
            |> Result.bind (fun selection ->
                MaterializeOwnershipVariants.materialize contracts semantics Set.empty definitions
                    [{ Caller = fact.Caller; Call = fact.Call; Selection = selection }] |> report)
            |> Result.bind (fun plan ->
                VerifyOwnedHIR.analyzeFunctions
                    (MaterializeOwnershipVariants.hirContracts plan contracts)
                    (MaterializeOwnershipVariants.ownershipSemantics plan semantics)
                    (MaterializeOwnershipVariants.functions plan) |> report)
            |> Result.bind (fun specializedFacts ->
                match specializedFacts with
                | [specialized] when callSiteIdentity specialized = callSiteIdentity fact
                                     && specialized.UniqueArguments = Set.singleton 0
                                     && specialized.Established = { Parameters = [UniqueCallParameter]; Result = UniqueProducedCallResult } -> Ok ()
                | actual -> Error (sprintf "Expected analyzed facts to establish a verified unique call boundary, got %A" actual))
        | actual -> Error (sprintf "Expected the original caller's single call fact, got %A" actual))

let tests = [
    "Call facts distinguish unmanaged borrowed consumed and unique parameters", testParameterModes
    "Call facts are captured before argument transfer", testPreTransferFacts
    "Call facts suspend and restore uniqueness across dup and drop", testDupDrop
    "Call facts retain multiplicity for repeated consuming arguments", testRepeatedConsumption
    "Call facts recognize fresh values and verified unique results", testFreshAndUniqueResults
    "Call facts permanently revoke escaped provenance", testEscapes
    "Call facts resolve borrowed result aliases", testBorrowedAlias
    "Call facts intersect uniqueness at managed joins", testBranchJoins false
    "Call facts retain uniqueness when both managed arms are unique", testBranchJoins true
    "Call facts keep sibling ownership states independent", testBranchStateIsolation
    "Call facts account for aliases passed to the same call", testAliasedArguments
    "Call verification rejects unique arguments aliased in the same call", testRejectsUniqueAliasedArgument
    "Call analysis returns no partial facts after validation failure", testFailureDiscardsFacts
    "Call facts revoke borrowed provenance for potentially aliasing owned results", testOwnedResultFromBorrow ProducedCallResult []
    "Call facts preserve borrowed provenance for independently unique results", testOwnedResultFromBorrow UniqueProducedCallResult [0]
    "Call facts revoke surviving consumed provenance for potentially aliasing owned results", testOwnedResultFromDuplicatedInput
    "Call verification rejects uniqueness after an owned result may alias", testRejectsUniqueUseAfterAliasingResult
    "Call facts scope identities to their caller", testFunctionScopedSites
    "Call facts derive self-recursive boundaries", testRecursiveContracts true
    "Call facts derive mutually recursive boundaries", testRecursiveContracts false
    "Call analysis rejects conflicting group registrations", testConflictingRegistration
    "Call analysis preserves typed HIR verification failures", testTypedFailure
    "Analyzed facts drive verified selection and materialization", testSpecializationHandoff
]
