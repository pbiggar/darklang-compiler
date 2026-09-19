// ElaborateFunctionOwnership.fs - Infer conservative boundaries and place whole-function ownership steps.

module ElaborateFunctionOwnership

open HIR
open OwnedIR

type Dialect<'leaf, 'block> = {
    Body: 'block -> HIR.Block<HIR.Operation<'leaf, 'block>>
    LeafOwnership: 'leaf -> Contract<HIR.ValueId>
    LeafUniqueness: 'leaf -> UniquenessContract<HIR.ValueId>
    IsManaged: HIR.Value -> bool
    ExternalCallOwnership: HIR.FunctionCall -> CallSignature option
}

type ElaborationError =
    | UnknownCallOwnership of target: AST.FunctionId
    | InconsistentCallParameters of target: AST.FunctionId
    | InvalidFunctionBoundary of target: AST.FunctionId * OwnedIR.VerificationError<HIR.ValueId>

type Analysis<'leaf> = private {
    Functions: OwnedIR.Function<'leaf, HIR.ValueId> list
    Semantics: Semantics<'leaf, HIR.ValueId>
}

let functions analysis = analysis.Functions
let semantics analysis = analysis.Semantics

let private managedId dialect value =
    if dialect.IsManaged value then Some value.Id else None

let private unmanagedOr
    (mode: HIR.ValueId -> OwnedIR.ParameterOwnership<HIR.ValueId>)
    dialect
    value
    : OwnedIR.ParameterOwnership<HIR.ValueId> =
    match managedId dialect value with
    | Some id -> mode id
    | None -> UnmanagedParameter

let private initialBoundary dialect (definition: HIR.Function<'block>) : OwnedIR.FunctionSignature<HIR.ValueId> =
    let block = dialect.Body definition.Body
    {
        Parameters =
            block.Parameters
            |> List.map (fun parameter -> unmanagedOr ConsumedParameter dialect parameter.Value)
        Result =
            match managedId dialect block.Result with
            | Some id -> ProducedResult id
            | None -> UnmanagedResult
    }

let private callRegistry
    externalOwnership
    (boundaries: Map<AST.FunctionId, OwnedIR.FunctionSignature<HIR.ValueId>>)
    : Result<HIR.FunctionCall -> OwnedIR.CallSignature option, ElaborationError> =
    let internalCalls =
        boundaries
        |> Map.toList
        |> List.fold (fun result (target, boundary) ->
            result
            |> Result.bind (fun registry ->
                VerifyOwnership.callSignatureOfFunction boundary
                |> Result.mapError (fun error -> InvalidFunctionBoundary (target, error))
                |> Result.map (fun signature -> Map.add target signature registry))) (Ok Map.empty)
    internalCalls
    |> Result.map (fun registry call ->
        match Map.tryFind call.Target registry with
        | Some signature -> Some signature
        | None -> externalOwnership call)

let private callInputs dialect signature (call: HIR.FunctionCall) =
    if List.length signature.Parameters <> List.length call.Arguments then
        Error (InconsistentCallParameters call.Target)
    else
        List.zip signature.Parameters call.Arguments
        |> List.fold (fun result (mode, value) ->
            result
            |> Result.bind (fun (borrowed, consumed) ->
                match mode, managedId dialect value with
                | UnmanagedCallParameter, None -> Ok (borrowed, consumed)
                | BorrowedCallParameter, Some id -> Ok (Set.add id borrowed, consumed)
                | (ConsumedCallParameter | UniqueCallParameter), Some id -> Ok (borrowed, id :: consumed)
                | _ -> Error (InconsistentCallParameters call.Target))) (Ok (Set.empty, []))

let private inferBoundary
    (dialect: Dialect<'leaf, 'block>)
    (ownership: HIR.FunctionCall -> OwnedIR.CallSignature option)
    (definition: HIR.Function<'block>)
    : Result<OwnedIR.FunctionSignature<HIR.ValueId>, ElaborationError> =
    let rec demandBlock demanded block =
        let body = dialect.Body block
        List.foldBack (fun operation result ->
            result
            |> Result.bind (fun demanded ->
                match operation with
                | HIR.Leaf leaf ->
                    let contract = dialect.LeafOwnership leaf
                    let definitions = contract.Outputs |> Set.ofList
                    let consumed =
                        contract.Inputs
                        |> List.choose (function Consumed id -> Some id | Borrowed _ -> None)
                        |> Set.ofList
                    Ok (Set.union consumed (Set.difference demanded definitions))
                | HIR.ScalarBinding (output, _) ->
                    let definitions = managedId dialect output |> Option.toList |> Set.ofList
                    Ok (Set.difference demanded definitions)
                | HIR.Call call ->
                    match ownership call with
                    | None -> Error (UnknownCallOwnership call.Target)
                    | Some signature ->
                        callInputs dialect signature call
                        |> Result.map (fun (_, consumed) ->
                            let definitions =
                                match signature.Result, managedId dialect call.Result with
                                | (ProducedCallResult | UniqueProducedCallResult), Some id -> Set.singleton id
                                | _ -> Set.empty
                            Set.union (Set.ofList consumed) (Set.difference demanded definitions))
                | HIR.Branch (output, _, yes, no) ->
                    let continuation =
                        match managedId dialect output with
                        | Some id -> Set.remove id demanded
                        | None -> demanded
                    let branchDemand branch =
                        let body = dialect.Body branch
                        match managedId dialect body.Result with
                        | Some id -> Set.add id continuation
                        | None -> continuation
                    demandBlock (branchDemand yes) yes
                    |> Result.bind (fun yesDemand ->
                        demandBlock (branchDemand no) no
                        |> Result.map (Set.union yesDemand)))
            ) body.Operations (Ok demanded)
    let body = dialect.Body definition.Body
    let resultDemand = managedId dialect body.Result |> Option.toList |> Set.ofList
    demandBlock resultDemand definition.Body
    |> Result.map (fun demanded ->
        {
            Parameters =
                body.Parameters
                |> List.map (fun parameter ->
                    match managedId dialect parameter.Value with
                    | None -> UnmanagedParameter
                    | Some id when Set.contains id demanded -> ConsumedParameter id
                    | Some id -> BorrowedParameter id)
            Result =
                match managedId dialect body.Result with
                | Some id -> ProducedResult id
                | None -> UnmanagedResult
        })

let private convergeBoundaries
    (dialect: Dialect<'leaf, 'block>)
    (definitions: HIR.Function<'block> list) =
    let initial =
        definitions
        |> List.map (fun definition -> definition.Id, initialBoundary dialect definition)
        |> Map.ofList
    let rec loop seen boundaries =
        if Set.contains boundaries seen then
            Crash.crash "Whole-function ownership boundary inference did not converge"
        else
            callRegistry dialect.ExternalCallOwnership boundaries
            |> Result.bind (fun ownership ->
                definitions
                |> List.fold (fun result definition ->
                    result
                    |> Result.bind (fun inferred ->
                        inferBoundary dialect ownership definition
                        |> Result.map (fun boundary -> Map.add definition.Id boundary inferred))) (Ok Map.empty)
                |> Result.bind (fun next ->
                    if next = boundaries then Ok (next, ownership)
                    else loop (Set.add boundaries seen) next))
    loop Set.empty initial

let private collectDefinitions
    (dialect: Dialect<'leaf, 'block>)
    (ownership: HIR.FunctionCall -> OwnedIR.CallSignature option)
    (definition: HIR.Function<'block>) =
    let rec block acc source =
        let body = dialect.Body source
        body.Operations
        |> List.fold (fun result operation ->
            result
            |> Result.bind (fun acc ->
                let addManaged value =
                    managedId dialect value
                    |> Option.map (fun id -> Set.add id acc)
                    |> Option.defaultValue acc
                match operation with
                | HIR.Leaf leaf ->
                    dialect.LeafOwnership leaf
                    |> fun contract -> Ok (Set.union acc (Set.ofList contract.Outputs))
                | HIR.ScalarBinding (output, _) -> Ok (addManaged output)
                | HIR.Call call ->
                    match ownership call with
                    | None -> Error (UnknownCallOwnership call.Target)
                    | Some signature ->
                        match signature.Result with
                        | ProducedCallResult | UniqueProducedCallResult -> Ok (addManaged call.Result)
                        | UnmanagedCallResult | BorrowedCallResult _ -> Ok acc
                | HIR.Branch (output, _, yes, no) ->
                    block (addManaged output) yes |> Result.bind (fun acc -> block acc no))) (Ok acc)
    block Set.empty definition.Body

let private elaborateFunction
    (dialect: Dialect<'leaf, 'block>)
    (ownership: HIR.FunctionCall -> OwnedIR.CallSignature option)
    (boundary: OwnedIR.FunctionSignature<HIR.ValueId>)
    (definition: HIR.Function<'block>) =
    let consumedParameters =
        boundary.Parameters
        |> List.choose (function ConsumedParameter id | UniqueParameter id -> Some id | _ -> None)
        |> Set.ofList
    collectDefinitions dialect ownership definition
    |> Result.bind (fun definitions ->
      let owned = Set.union consumedParameters definitions
      let reverseIds ids = ids |> Set.toList |> List.sortDescending
      let drops ids = ids |> Set.filter (fun id -> Set.contains id owned) |> reverseIds |> List.map Drop
      let dups consumed liveAfter =
          consumed
          |> List.countBy id
          |> List.collect (fun (id, count) ->
              let required = count + (if Set.contains id liveAfter then 1 else 0)
              List.replicate (max 0 (required - 1)) (Dup id))
      let rec elaborateBlock liveAfter source =
        let body = dialect.Body source
        List.foldBack (fun operation result ->
            result
            |> Result.bind (fun (tail, live) ->
                match operation with
                | HIR.Branch (output, condition, yes, no) ->
                    let continuation =
                        match managedId dialect output with
                        | Some id -> Set.remove id live
                        | None -> live
                    let branchLive branch =
                        let body = dialect.Body branch
                        match managedId dialect body.Result with
                        | Some id -> Set.add id continuation
                        | None -> continuation
                    elaborateBlock (branchLive yes) yes
                    |> Result.bind (fun (ownedYes, yesBefore) ->
                        elaborateBlock (branchLive no) no
                        |> Result.map (fun (ownedNo, noBefore) ->
                            let conditionUses =
                                condition.Inputs
                                |> Map.values
                                |> Seq.choose (managedId dialect)
                                |> Set.ofSeq
                            let before = Set.union conditionUses (Set.union yesBefore noBefore)
                            let edge required (block: OwnedIR.Block<'leaf, HIR.ValueId>) =
                                let cleanup = Set.difference before required |> drops
                                let preserveResult =
                                    managedId dialect block.Body.Result
                                    |> Option.filter (fun id -> Set.contains id continuation)
                                    |> Option.map Dup
                                    |> Option.toList
                                {
                                    block with
                                        Body = {
                                            block.Body with
                                                Operations = cleanup @ block.Body.Operations @ preserveResult
                                        }
                                }
                            let branch =
                                HIR.Branch (output, condition, edge yesBefore ownedYes, edge noBefore ownedNo)
                            let unusedOutput =
                                managedId dialect output
                                |> Option.filter (fun id -> not (Set.contains id live))
                                |> Option.toList
                                |> Set.ofList
                            Evaluate branch :: (drops unusedOutput @ tail), before))
                | _ ->
                    let operationOwnership =
                        match operation with
                        | HIR.Leaf leaf ->
                            let contract = dialect.LeafOwnership leaf
                            let borrowed =
                                contract.Inputs
                                |> List.choose (function Borrowed id -> Some id | Consumed _ -> None)
                                |> Set.ofList
                            let consumed =
                                contract.Inputs
                                |> List.choose (function Consumed id -> Some id | Borrowed _ -> None)
                            Ok (borrowed, consumed, Set.ofList contract.Outputs)
                        | HIR.ScalarBinding (output, operand) ->
                            let borrowed =
                                operand.Inputs |> Map.values |> Seq.choose (managedId dialect) |> Set.ofSeq
                            let definitions = managedId dialect output |> Option.toList |> Set.ofList
                            Ok (borrowed, [], definitions)
                        | HIR.Call call ->
                            match ownership call with
                            | None -> Error (UnknownCallOwnership call.Target)
                            | Some signature ->
                                match callInputs dialect signature call with
                                | Error error -> Error error
                                | Ok (borrowed, consumed) ->
                                    let definitions =
                                        match signature.Result, managedId dialect call.Result with
                                        | (ProducedCallResult | UniqueProducedCallResult), Some id -> Set.singleton id
                                        | _ -> Set.empty
                                    Ok (borrowed, consumed, definitions)
                        | HIR.Branch _ -> Crash.crash "Whole-function ownership: branch handled separately"
                    operationOwnership
                    |> Result.map (fun (borrowed, consumed, definitions) ->
                        let uses = Set.union borrowed (Set.ofList consumed)
                        let before = Set.union uses (Set.difference live definitions)
                        let consumedSet = Set.ofList consumed
                        let lastBorrowed =
                            Set.difference uses live |> Set.filter (fun id -> not (Set.contains id consumedSet))
                        let unusedDefinitions = Set.difference definitions live
                        let steps =
                            dups consumed live
                            @ [Evaluate (match operation with
                                         | HIR.Leaf leaf -> HIR.Leaf leaf
                                         | HIR.ScalarBinding (output, operand) -> HIR.ScalarBinding (output, operand)
                                         | HIR.Call call -> HIR.Call call
                                         | HIR.Branch _ -> Crash.crash "Whole-function ownership: branch handled separately")]
                            @ drops (Set.union lastBorrowed unusedDefinitions)
                        steps @ tail, before))
            ) body.Operations (Ok ([], liveAfter))
        |> Result.map (fun (operations, before) ->
            { Body = { Parameters = body.Parameters; Operations = operations; Result = body.Result } }, before)
      let sourceBody = dialect.Body definition.Body
      let liveResult = managedId dialect sourceBody.Result |> Option.toList |> Set.ofList
      elaborateBlock liveResult definition.Body
      |> Result.map (fun (body, liveBefore) ->
          let unusedParameters = Set.difference consumedParameters liveBefore |> drops
          let body = { body with Body = { body.Body with Operations = unusedParameters @ body.Body.Operations } }
          {
              Definition = { Id = definition.Id; Name = definition.Name; Body = body }
              Ownership = boundary
          }))

let elaborateFunctions
    (dialect: Dialect<'leaf, 'block>)
    (definitions: HIR.Function<'block> list)
    : Result<Analysis<'leaf>, ElaborationError> =
    convergeBoundaries dialect definitions
    |> Result.bind (fun (boundaries, ownership) ->
        definitions
        |> List.fold (fun result definition ->
            result
            |> Result.bind (fun functions ->
                match Map.tryFind definition.Id boundaries with
                | None -> Crash.crash "Whole-function ownership boundary disappeared during elaboration"
                | Some boundary ->
                    elaborateFunction dialect ownership boundary definition
                    |> Result.map (fun owned -> owned :: functions))) (Ok [])
        |> Result.map (fun functions ->
            let ownershipSemantics : Semantics<'leaf, HIR.ValueId> = {
                Leaf = dialect.LeafOwnership
                LeafUniqueness = dialect.LeafUniqueness
                CallOwnership = ownership
                ScalarUses = fun operand ->
                    operand.Inputs |> Map.values |> Seq.choose (managedId dialect) |> Set.ofSeq
                ScalarEscapes = fun operand ->
                    operand.Inputs |> Map.values |> Seq.choose (managedId dialect) |> Set.ofSeq
                BlockArgument = fun value ->
                    match managedId dialect value with
                    | Some id -> Managed id
                    | None -> Unmanaged
            }
            { Functions = List.rev functions; Semantics = ownershipSemantics }))
