// InferOwnershipUniqueness.fs - Derive verifier-proven uniqueness boundary variants.

module InferOwnershipUniqueness

open OwnedIR

/// Ownership transfer modes are established before this pass. Inference may
/// strengthen consumed parameters and produced results with exclusivity, but
/// it never changes whether ownership crosses the boundary.
type Candidates<'id> =
    private
    | Candidates of head: FunctionSignature<'id> * tail: FunctionSignature<'id> list

type InferenceError<'id when 'id: comparison> =
    | VariantLimitExceeded of refinableModes: int * maximumVariants: int
    | RecursiveFunctionRequiresGroupInference of functionId: AST.FunctionId
    | NoVerifiedBoundary of VerificationError<'id>
    | NoVerifiedFunctionGroup of VerificationError<'id>

let maximumVariants = 256

let toList (Candidates (head, tail)) = head :: tail

let private parameterVariants = function
    | ConsumedParameter id -> [ConsumedParameter id; UniqueParameter id]
    | ownership -> [ownership]

let private resultVariants = function
    | ProducedResult id -> [ProducedResult id; UniqueProducedResult id]
    | ownership -> [ownership]

let internal refinableModeCount (signature: FunctionSignature<'id>) =
    let parameters =
        signature.Parameters
        |> List.sumBy (function ConsumedParameter _ -> 1 | _ -> 0)
    match signature.Result with
    | ProducedResult _ -> parameters + 1
    | _ -> parameters

let internal withinVariantLimit refinableModes =
    let rec doubleWithinLimit remaining variants =
        if remaining = 0 then true
        elif variants > maximumVariants / 2 then false
        else doubleWithinLimit (remaining - 1) (variants * 2)
    doubleWithinLimit refinableModes 1

let rec private parameterSignatures = function
    | [] -> Seq.singleton []
    | ownership :: rest ->
        seq {
            for variant in parameterVariants ownership do
                for remaining in parameterSignatures rest do
                    yield variant :: remaining
        }

let internal signatureSequence
    (signature: FunctionSignature<'id>)
    : FunctionSignature<'id> seq =
    seq {
        for parameterModes in parameterSignatures signature.Parameters do
            for resultMode in resultVariants signature.Result do
                yield ({ Parameters = parameterModes; Result = resultMode }: FunctionSignature<'id>)
    }

let internal signatures (signature: FunctionSignature<'id>) : FunctionSignature<'id> list =
    signatureSequence signature |> Seq.toList

let rec private combinations size values =
    seq {
        match size, values with
        | 0, _ -> yield []
        | _, [] -> ()
        | size, head :: tail when size > 0 ->
            for rest in combinations (size - 1) tail do
                yield head :: rest
            yield! combinations size tail
        | _ -> ()
    }

/// Produce only refinements that a concrete call can satisfy. Candidates are
/// ordered by the number and then positions of required unique arguments, so
/// verification may stop at the first successful boundary.
let internal demandedSignatures
    (uniqueArguments: Set<int>)
    (signature: FunctionSignature<'id>)
    : FunctionSignature<'id> seq =
    match signature.Result with
    | ProducedResult result ->
        let available =
            signature.Parameters
            |> List.indexed
            |> List.choose (fun (index, ownership) ->
                match ownership with
                | ConsumedParameter _ when Set.contains index uniqueArguments -> Some index
                | UnmanagedParameter
                | BorrowedParameter _
                | ConsumedParameter _
                | UniqueParameter _ -> None)
        seq {
            for size in 0 .. List.length available do
                for selected in combinations size available do
                    let selected = Set.ofList selected
                    let parameters =
                        signature.Parameters
                        |> List.mapi (fun index ownership ->
                            match ownership with
                            | ConsumedParameter id when Set.contains index selected -> UniqueParameter id
                            | _ -> ownership)
                    yield ({
                        Parameters = parameters
                        Result = UniqueProducedResult result
                    } : FunctionSignature<'id>)
        }
    | UnmanagedResult
    | BorrowedResult _
    | UniqueProducedResult _ -> Seq.empty

let private parameterStrength = function
    | UnmanagedParameter | BorrowedParameter _ -> 0
    | ConsumedParameter _ -> 1
    | UniqueParameter _ -> 2

let private resultStrength = function
    | UnmanagedResult | BorrowedResult _ -> 0
    | ProducedResult _ -> 1
    | UniqueProducedResult _ -> 2

let internal boundaryRelation
    (first: FunctionSignature<'id>)
    (second: FunctionSignature<'id>) =
    let rec compareParameters noStronger strictlyBetter first second =
        match first, second with
        | [], [] -> noStronger, strictlyBetter
        | first :: firstRest, second :: secondRest ->
            compareParameters
                (noStronger && parameterStrength first <= parameterStrength second)
                (strictlyBetter || parameterStrength first < parameterStrength second)
                firstRest
                secondRest
        | _ -> Crash.crash "Uniqueness variants changed function parameter arity"
    let parametersNoStronger, strictlyWeakerParameters =
        compareParameters true false first.Parameters second.Parameters
    let resultNoWeaker = resultStrength first.Result >= resultStrength second.Result
    let strictlyBetter =
        strictlyWeakerParameters
        || resultStrength first.Result > resultStrength second.Result
    parametersNoStronger && resultNoWeaker, strictlyBetter

/// A boundary dominates another when it requires no stronger parameter modes
/// and promises no weaker result mode, with at least one strict improvement.
let private dominates first second =
    match boundaryRelation first second with
    | true, true -> true
    | _ -> false

let rec private callsTarget target (block: Block<'leaf, 'id>) =
    block.Body.Operations
    |> List.exists (function
        | Evaluate (HIR.Call call) -> call.Target = target
        | Evaluate (HIR.Branch (_, _, ifTrue, ifFalse)) ->
            callsTarget target ifTrue || callsTarget target ifFalse
        | Evaluate (HIR.Leaf _ | HIR.ScalarBinding _)
        | Dup _
        | Drop _ -> false)

/// Enumerate the nondominated uniqueness refinements accepted by the
/// ownership verifier. Returning every tradeoff keeps specialization policy
/// separate from proof: for example, a weaker input requirement and a stronger
/// result guarantee can both remain useful boundaries. Typed HIR, primitive
/// contracts, and non-recursive call ownership remain independent prerequisites;
/// recursive boundaries use `InferRecursiveOwnership.infer` so calls are
/// checked against the same group-wide candidate.
let infer
    (semantics: Semantics<'leaf, 'id>)
    (functionDefinition: Function<'leaf, 'id>)
    : Result<Candidates<'id>, InferenceError<'id>> =
    let signature = functionDefinition.Ownership
    let root = functionDefinition.Definition.Body
    let refinableModes = refinableModeCount signature
    if callsTarget functionDefinition.Definition.Id root then
        Error (RecursiveFunctionRequiresGroupInference functionDefinition.Definition.Id)
    elif not (withinVariantLimit refinableModes) then
        Error (VariantLimitExceeded (refinableModes, maximumVariants))
    else
        let verified, firstFailure =
            signatures signature
            |> List.fold (fun (verified, firstFailure) candidate ->
                match VerifyOwnership.verifyFunction semantics candidate root with
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
        | head :: tail, _ -> Ok (Candidates (head, tail))
        | [], Some error -> Error (NoVerifiedBoundary error)
        | [], None -> Crash.crash "Ownership uniqueness inference generated no boundary candidates"

/// Verify the best ownership refinement usable by one concrete call. Search is
/// lazy and bounded: inability to prove an optimization retains the established
/// boundary instead of rejecting an otherwise valid program.
let inferDemand
    (semantics: Semantics<'leaf, 'id>)
    (uniqueArguments: Set<int>)
    (functionDefinition: Function<'leaf, 'id>)
    : Result<FunctionSignature<'id> option, InferenceError<'id>> =
    let root = functionDefinition.Definition.Body
    if callsTarget functionDefinition.Definition.Id root then
        Error (RecursiveFunctionRequiresGroupInference functionDefinition.Definition.Id)
    else
        functionDefinition.Ownership
        |> demandedSignatures uniqueArguments
        |> Seq.truncate maximumVariants
        |> Seq.tryPick (fun candidate ->
            match VerifyOwnership.verifyFunction semantics candidate root with
            | Ok () -> Some candidate
            | Error _ -> None)
        |> Ok
