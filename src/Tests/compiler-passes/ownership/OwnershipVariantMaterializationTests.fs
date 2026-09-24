// OwnershipVariantMaterializationTests.fs - Verify atomic specialization and call-boundary laws.

module OwnershipVariantMaterializationTests

open OwnedIR
open MaterializeOwnershipVariants

type private Leaf = Fresh of HIR.Value

let private value id : HIR.Value = { Id = HIR.ValueId id; Type = AST.TList AST.TInt64 }
let private functionId name = TestIds.functionIdForName name
let private binding name =
    name |> Seq.fold (fun hash ch -> (hash * 31) + int ch) 17 |> AST.bindingId
let private signature parameters result : FunctionSignature<HIR.ValueId> = { Parameters = parameters; Result = result }
let private block parameters operations result : Block<Leaf, HIR.ValueId> = {
    Body = { Parameters = parameters; Operations = operations; Result = result }
}
let private parameter value : HIR.Parameter = { Name = "input"; Binding = binding "input"; Value = value }
let private definition name ownership body : Function<Leaf, HIR.ValueId> = {
    Definition = { Id = functionId name; Name = name; Body = body }
    Ownership = ownership
}
let private call target argument result : HIR.FunctionCall = {
    Target = functionId target
    Arguments = [argument]
    Result = result
}
let private ordinary : CallSignature = { Parameters = [ConsumedCallParameter]; Result = ProducedCallResult }
let private unique : CallSignature = { Parameters = [UniqueCallParameter]; Result = UniqueProducedCallResult }

let private semantics : Semantics<Leaf, HIR.ValueId> = {
    Leaf = fun (Fresh output) -> { Inputs = []; Outputs = [output.Id] }
    LeafUniqueness = fun (Fresh output) -> { RequiredInputs = Set.empty; UniqueOutputs = Set.singleton output.Id }
    CallOwnership = fun _ -> None
    ScalarUses = fun operand -> operand.Inputs |> Map.values |> Seq.map (fun value -> value.Id) |> Set.ofSeq
    ScalarEscapes = fun _ -> Set.empty
    BlockArgument = fun value -> Managed value.Id
}

let private hir names : VerifyOwnedHIR.HIRContracts<Leaf> = {
    Leaf = fun (Fresh output) -> {
        Inputs = []; Operands = []
        Outputs = [{ Value = output; Alias = HIR.FreshManaged }]
        Effects = Set.singleton HIR.MayAllocate
    }
    CallSignature = fun _ -> None
    CallContract = fun call ->
        match call.Arguments with
        | first :: rest when Set.contains call.Target names -> Some {
            Inputs = call.Arguments; Operands = []
            Outputs = [{ Value = call.Result; Alias = HIR.MayAliasInputs (first, rest) }]
            Effects = Set.singleton HIR.MayInvokeUserCode
          }
        | _ -> None
}

let private contracts definitions = definitions |> List.map (fun definition -> definition.Definition.Id) |> Set.ofList |> hir
let private report result = result |> Result.mapError (sprintf "%A")

let private select definitions target uniqueArguments =
    InferOwnedFunctionGroups.infer semantics definitions |> report
    |> Result.bind (SelectOwnershipVariants.create >> report)
    |> Result.bind (fun catalog ->
        SelectOwnershipVariants.select catalog { Target = target; Established = ordinary; UniqueArguments = uniqueArguments }
        |> report)

let private request caller call selection : Request<HIR.ValueId> = {
    Caller = functionId caller
    Call = call
    Selection = selection
}
let private run definitions requests = materialize (contracts definitions) semantics Map.empty definitions requests

let private identity () =
    let input = value 0
    definition "identity" (signature [ConsumedParameter input.Id] (ProducedResult input.Id))
        (block [parameter input] [] input)

let private caller name target baseId uniqueInput =
    let input, output = value baseId, value (baseId + 1)
    let boundary = if uniqueInput then UniqueParameter input.Id else ConsumedParameter input.Id
    let invocation = call target input output
    definition name (signature [boundary] (ProducedResult output.Id))
        (block [parameter input] [Evaluate (HIR.Call invocation)] output), invocation

let private singleFixture () =
    let callee = identity ()
    let first, firstCall = caller "first" "identity" 10 true
    let second, secondCall = caller "second" "identity" 20 true
    callee, [callee; first; second], firstCall, secondCall

let private testDeduplicatesAndVerifies () =
    let callee, definitions, firstCall, secondCall = singleFixture ()
    select [callee] "identity" (Set.singleton 0)
    |> Result.bind (fun chosen ->
        run definitions [request "first" firstCall chosen; request "second" secondCall chosen] |> report
        |> Result.bind (fun plan ->
            let cloneCount = groups plan |> List.sumBy (fun group -> AST.NonEmptyList.toList group.Members |> List.length)
            let rewritten = rewrites plan
            match rewritten with
            | [first; second] when cloneCount = 1
                                  && first.Specialized.Target = second.Specialized.Target
                                  && first.Ownership = unique && second.Ownership = unique ->
                VerifyOwnedHIR.verifyFunctions (hirContracts plan (contracts definitions))
                    (ownershipSemantics plan semantics) (functions plan) |> report
            | _ -> Error $"Expected one verified clone shared by both call sites, got {cloneCount} clones"))

let private testPreservesEstablishedCalls () =
    let callee, definitions, firstCall, secondCall = singleFixture ()
    select [callee] "identity" (Set.singleton 0)
    |> Result.bind (fun chosen ->
        run definitions [request "first" firstCall chosen; request "second" secondCall (SelectOwnershipVariants.EstablishedBoundary ordinary)] |> report
        |> Result.bind (fun plan ->
            let secondBefore = definitions |> List.tryFind (fun definition -> definition.Definition.Name = "second")
            let secondAfter = functions plan |> List.tryFind (fun definition -> definition.Definition.Name = "second")
            if secondBefore = secondAfter && List.length (rewrites plan) = 1 then Ok ()
            else Error "Established call changed while a neighboring call was specialized"))

let private testEmptyPlanPreservesProgram () =
    let _, definitions, firstCall, _ = singleFixture ()
    run definitions [request "first" firstCall (SelectOwnershipVariants.EstablishedBoundary ordinary)] |> report
    |> Result.bind (fun plan ->
        if functions plan = definitions && List.isEmpty (groups plan) && List.isEmpty (rewrites plan) then Ok ()
        else Error "Established-only materialization changed the program")

let private testRejectsUnprovenCallUniqueness () =
    let callee = identity ()
    let caller, invocation = caller "shared" "identity" 10 false
    select [callee] "identity" (Set.singleton 0)
    |> Result.bind (fun chosen ->
        match run [callee; caller] [request "shared" invocation chosen] with
        | Error (InvalidMaterializedProgram (VerifyOwnedHIR.OwnershipVerificationFailed (NonUniqueUse id)))
            when id = (value 10).Id -> Ok ()
        | actual -> Error (sprintf "Expected the caller's actual ownership state to reject unique consumption, got %A" actual))

let private recursiveFixture self =
    let condition : HIR.Operand = { Expression = CheckedAST.BoolLiteral true; Type = AST.TBool; Inputs = Map.empty }
    let recursive name target baseId =
        let input, recursiveResult, result = value baseId, value (baseId + 1), value (baseId + 2)
        let invocation = call target input recursiveResult
        let body = block [parameter input]
                       [Evaluate (HIR.Branch (result, condition,
                           block [] [Evaluate (HIR.Call invocation)] recursiveResult,
                           block [] [] input))] result
        definition name (signature [ConsumedParameter input.Id] (ProducedResult result.Id)) body, invocation
    let first, internalCall = recursive "first" (if self then "first" else "second") 0
    let second, _ = recursive "second" "first" 10
    let targets = if self then [first] else [first; second]
    let entry, invocation = caller "entry" "first" 20 true
    targets, targets @ [entry], internalCall, invocation

let private recursivePlan self reverse =
    let targets, definitions, _, invocation = recursiveFixture self
    let targets = if reverse then List.rev targets else targets
    select targets "first" (Set.singleton 0)
    |> Result.bind (fun chosen -> run definitions [request "entry" invocation chosen] |> report)

let private testRecursiveAtomicity self () =
    recursivePlan self false
    |> Result.bind (fun plan ->
        match groups plan with
        | [group] ->
            let members = AST.NonEmptyList.toList group.Members
            let symbols =
                members
                |> List.map (fun memberDefinition -> memberDefinition.Original, memberDefinition.Function.Definition.Id)
                |> Map.ofList
            let contract = ownershipSemantics plan semantics
            let check memberDefinition =
                let definition = memberDefinition.Function
                let expectedTarget =
                    if self || memberDefinition.Original = functionId "second" then
                        functionId "first"
                    else
                        functionId "second"
                match definition.Definition.Body.Body.Operations with
                | [Evaluate (HIR.Branch (_, _, yes, _))] ->
                    match yes.Body.Operations with
                    | [Evaluate (HIR.Call invocation)] ->
                        Map.tryFind expectedTarget symbols = Some invocation.Target
                        && contract.CallOwnership invocation = Some unique
                    | _ -> false
                | _ -> false
            if List.length members = (if self then 1 else 2) && List.forall check members then Ok ()
            else Error "Recursive calls and unique contracts did not move together as one complete group"
        | _ -> Error "Expected one materialized recursive group")

let private testDeterministicRecursiveSymbols () =
    match recursivePlan false false, recursivePlan false true with
    | Ok first, Ok second when groups first = groups second && rewrites first = rewrites second -> Ok ()
    | actual -> Error (sprintf "Expected identical recursive materialization independent of discovery order, got %A" actual)

let private testMissingRecursiveMember () =
    let targets, definitions, _, invocation = recursiveFixture false
    select targets "first" (Set.singleton 0)
    |> Result.bind (fun chosen ->
        let incomplete = definitions |> List.filter (fun definition -> definition.Definition.Name <> "second")
        match run incomplete [request "entry" invocation chosen] with
        | Error (MissingGroupMember "second") -> Ok ()
        | actual -> Error (sprintf "Expected a missing recursive member error, got %A" actual))

let private testRejectsPartialRecursiveSelection () =
    let targets, definitions, internalCall, _ = recursiveFixture false
    select targets "second" (Set.singleton 0)
    |> Result.bind (fun chosen ->
        match run definitions [request "first" internalCall chosen] with
        | Error (MixedRecursiveCandidate site) when site.Caller = functionId "first" -> Ok ()
        | actual -> Error (sprintf "Expected a per-edge recursive selection to be rejected, got %A" actual))

let private testRejectsChangedGroup () =
    let targets, definitions, _, invocation = recursiveFixture false
    select targets "first" (Set.singleton 0)
    |> Result.bind (fun chosen ->
        let changed = definitions |> List.map (fun definition ->
            if definition.Definition.Name = "second" then
                let input = value 10
                { definition with Definition = { definition.Definition with Body = block [parameter input] [] input } }
            else definition)
        match run changed [request "entry" invocation chosen] with
        | Error (GroupMembershipMismatch "first") -> Ok ()
        | actual -> Error (sprintf "Expected stale SCC membership to be rejected, got %A" actual))

let private testRejectsChangedBoundary () =
    let callee, definitions, firstCall, _ = singleFixture ()
    select [callee] "identity" (Set.singleton 0)
    |> Result.bind (fun chosen ->
        let changed = definitions |> List.map (fun definition ->
            if definition.Definition.Name = "identity" then
                { definition with Ownership = signature [BorrowedParameter (value 0).Id] (BorrowedResult (value 0).Id) }
            else definition)
        match run changed [request "first" firstCall chosen] with
        | Error (BoundaryMismatch "identity") -> Ok ()
        | actual -> Error (sprintf "Expected a changed transfer boundary to be rejected, got %A" actual))

let private testRejectsCallSiteErrors () =
    let callee, definitions, firstCall, _ = singleFixture ()
    select [callee] "identity" (Set.singleton 0)
    |> Result.bind (fun chosen ->
        let valid = request "first" firstCall chosen
        let missing = { valid with Caller = functionId "missing" }
        let stale = { valid with Call = { firstCall with Arguments = [] } }
        match run definitions [missing], run definitions [stale], run definitions [valid; valid] with
        | Error (MissingCallSite _), Error (StaleCallSite _), Error (DuplicateCallSite _) -> Ok ()
        | actual -> Error (sprintf "Expected missing, stale and duplicate call sites to fail, got %A" actual))

let private testRejectsCollisions () =
    let callee, definitions, firstCall, _ = singleFixture ()
    select [callee] "identity" (Set.singleton 0)
    |> Result.bind (fun chosen ->
        let requests = [request "first" firstCall chosen]
        run definitions requests |> report
        |> Result.bind (fun plan ->
            match rewrites plan with
            | [rewrite] ->
                let symbol = rewrite.Specialized.Target
                let symbolName =
                    functions plan
                    |> List.find (fun definition -> definition.Definition.Id = symbol)
                    |> fun definition -> definition.Definition.Name
                let shadow = {
                    callee with
                        Definition = { callee.Definition with Id = symbol; Name = symbolName }
                }
                let reserved =
                    materialize
                        (contracts definitions)
                        semantics
                        (Map.ofList [symbol, symbolName])
                        definitions
                        requests
                let declared = run (definitions @ [shadow]) requests
                match reserved, declared with
                | Error (SymbolCollision first), Error (SymbolCollision second)
                    when first = symbolName && second = symbolName -> Ok ()
                | actual -> Error (sprintf "Expected reserved and existing definition collisions to fail, got %A" actual)
            | _ -> Error "Expected one rewrite"))

let private testRetainsIndependentContracts () =
    let callee, definitions, firstCall, _ = singleFixture ()
    select [callee] "identity" (Set.singleton 0)
    |> Result.bind (fun chosen ->
        let source = contracts definitions
        run definitions [request "first" firstCall chosen] |> report
        |> Result.bind (fun plan ->
            match rewrites plan with
            | [rewrite] ->
                if (hirContracts plan source).CallContract rewrite.Specialized = source.CallContract rewrite.Original
                   && (ownershipSemantics plan semantics).CallOwnership rewrite.Specialized = Some unique then Ok ()
                else Error "Specialization lost the independent effect/alias contract or ownership registration"
            | _ -> Error "Expected one rewrite"))

let private testMultipleRecursiveCandidates () =
    let targets, definitions, _, invocation = recursiveFixture false
    let other, otherCall = caller "other" "second" 30 false
    let definitions = definitions @ [other]
    select targets "first" (Set.singleton 0)
    |> Result.bind (fun strong ->
        select targets "second" Set.empty
        |> Result.bind (fun weak ->
            let requests = [request "entry" invocation strong; request "other" otherCall weak]
            match run definitions requests, run (List.rev definitions) (List.rev requests) with
            | Ok first, Ok second ->
                let complete = groups first |> List.forall (fun group -> List.length (AST.NonEmptyList.toList group.Members) = 2)
                if List.length (groups first) = 2 && complete
                   && groups first = groups second && rewrites first = rewrites second then Ok ()
                else Error "Expected two complete, deterministic recursive variants rather than mixed member boundaries"
            | actual -> Error (sprintf "Expected both recursive candidates to verify independently, got %A" actual)))

let private testNestedExternalCall () =
    let callee = identity ()
    let input, yesResult, noResult, result = value 20, value 21, value 22, value 23
    let yesCall, noCall = call "identity" input yesResult, call "identity" input noResult
    let condition : HIR.Operand = { Expression = CheckedAST.BoolLiteral false; Type = AST.TBool; Inputs = Map.empty }
    let no = block [] [Evaluate (HIR.Call noCall)] noResult
    let branch = definition "branch" (signature [UniqueParameter input.Id] (ProducedResult result.Id))
                     (block [parameter input]
                         [Evaluate (HIR.Branch (result, condition, block [] [Evaluate (HIR.Call yesCall)] yesResult, no))] result)
    select [callee] "identity" (Set.singleton 0)
    |> Result.bind (fun chosen ->
        run [callee; branch] [request "branch" yesCall chosen] |> report
        |> Result.bind (fun plan ->
            match functions plan |> List.tryFind (fun definition -> definition.Definition.Name = "branch"), rewrites plan with
            | Some rewritten, [rewrite] ->
                match rewritten.Definition.Body.Body.Operations with
                | [Evaluate (HIR.Branch (_, actualCondition, yes, actualNo))]
                    when actualCondition = condition && actualNo = no
                         && yes.Body.Operations = [Evaluate (HIR.Call rewrite.Specialized)] -> Ok ()
                | _ -> Error "Nested call rewrite changed a condition, the other branch, or its ownership transfer"
            | _ -> Error "Expected exactly one nested call rewrite"))

let private testRejectsStaleBodyProof () =
    let callee, definitions, firstCall, _ = singleFixture ()
    select [callee] "identity" (Set.singleton 0)
    |> Result.bind (fun chosen ->
        let changed = definitions |> List.map (fun definition ->
            if definition.Definition.Name = "identity" then
                // Balanced local units do not restore provenance after an
                // escaping scalar use. The old uniqueness proof is now stale.
                let input = value 0
                let ignored : HIR.Value = { Id = HIR.ValueId 2; Type = AST.TInt64 }
                let escape : HIR.Operand = {
                    Expression = CheckedAST.Int64Literal 0L; Type = AST.TInt64
                    Inputs = Map.ofList [binding "input", input]
                }
                let body =
                    block
                        [parameter input]
                        [Evaluate (HIR.ScalarBinding (ignored, escape)); Drop ignored.Id]
                        input
                { definition with Definition = { definition.Definition with Body = body } }
            else definition)
        let escaping = { semantics with ScalarEscapes = semantics.ScalarUses }
        match materialize (contracts changed) escaping Map.empty changed [request "first" firstCall chosen] with
        | Error (InvalidMaterializedProgram (VerifyOwnedHIR.OwnershipVerificationFailed (NonUniqueUse id)))
            when id = (value 0).Id -> Ok ()
        | actual -> Error (sprintf "Expected changed body provenance to invalidate the old candidate proof, got %A" actual))

let private testRejectsRegisteredCollisions () =
    let callee, definitions, firstCall, _ = singleFixture ()
    select [callee] "identity" (Set.singleton 0)
    |> Result.bind (fun chosen ->
        let requests = [request "first" firstCall chosen]
        run definitions requests |> report
        |> Result.bind (fun plan ->
            match rewrites plan with
            | [rewrite] ->
                let symbol = rewrite.Specialized.Target
                let symbolName =
                    functions plan
                    |> List.find (fun definition -> definition.Definition.Id = symbol)
                    |> fun definition -> definition.Definition.Name
                let typed = {
                    contracts definitions with
                        CallSignature = fun name ->
                            if name = symbol then Some { Parameters = [AST.TList AST.TInt64]; Result = AST.TList AST.TInt64 }
                            else None
                }
                let owned = { semantics with CallOwnership = fun call -> if call.Target = symbol then Some unique else None }
                match materialize typed semantics Map.empty definitions requests,
                      materialize (contracts definitions) owned Map.empty definitions requests with
                | Error (SymbolCollision first), Error (SymbolCollision second)
                    when first = symbolName && second = symbolName -> Ok ()
                | actual -> Error (sprintf "Expected registered symbol collisions to be rejected, got %A" actual)
            | _ -> Error "Expected one rewrite"))

let private testRejectsMismatchedRequests () =
    let callee, definitions, firstCall, _ = singleFixture ()
    let other = {
        callee with
            Definition = { callee.Definition with Id = functionId "other"; Name = "other" }
    }
    select [other] "other" (Set.singleton 0)
    |> Result.bind (fun chosen ->
        match run (definitions @ [other]) [request "first" firstCall chosen],
              run definitions [request "first" firstCall (SelectOwnershipVariants.EstablishedBoundary unique)],
              run (callee :: definitions) [] with
        | Error (BoundaryMismatch "identity"), Error (BoundaryMismatch "identity"),
          Error (GroupingFailed (OwnedFunctionGroups.DuplicateFunctionName id))
            when id = functionId "identity" -> Ok ()
        | actual -> Error (sprintf "Expected wrong target, established boundary and duplicate definition errors, got %A" actual))

let tests = [
    "Materialization deduplicates candidates and verifies all rewritten callers", testDeduplicatesAndVerifies
    "Materialization preserves established calls", testPreservesEstablishedCalls
    "Materialization leaves established-only programs unchanged", testEmptyPlanPreservesProgram
    "Materialization rejects false caller uniqueness facts", testRejectsUnprovenCallUniqueness
    "Materialization transfers self-recursive contracts atomically", testRecursiveAtomicity true
    "Materialization transfers mutual-recursive contracts atomically", testRecursiveAtomicity false
    "Materialization uses deterministic recursive symbols", testDeterministicRecursiveSymbols
    "Materialization rejects missing recursive members", testMissingRecursiveMember
    "Materialization rejects mixed recursive candidates", testRejectsPartialRecursiveSelection
    "Materialization rejects changed SCC membership", testRejectsChangedGroup
    "Materialization rejects changed ownership boundaries", testRejectsChangedBoundary
    "Materialization rejects invalid call-site requests", testRejectsCallSiteErrors
    "Materialization rejects symbol collisions", testRejectsCollisions
    "Materialization preserves independent call contracts", testRetainsIndependentContracts
    "Materialization separates complete recursive candidates deterministically", testMultipleRecursiveCandidates
    "Materialization rewrites nested calls while preserving branch ownership", testNestedExternalCall
    "Materialization rechecks stale candidate body proofs", testRejectsStaleBodyProof
    "Materialization rejects registered symbol collisions", testRejectsRegisteredCollisions
    "Materialization rejects mismatched request boundaries", testRejectsMismatchedRequests
]
