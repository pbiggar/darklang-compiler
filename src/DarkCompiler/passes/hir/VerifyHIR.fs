// VerifyHIR.fs - Verify normalized HIR value identities and structured control-flow edges.

module VerifyHIR

open HIR

type VerificationError =
    | UnknownValue of ValueId
    | DuplicateDefinition of ValueId
    | InconsistentValueType of ValueId
    | BindingTypeMismatch of result: ValueId
    | InvalidBranchCondition of AST.Type
    | InconsistentBranchResult of result: ValueId
    | InvalidAliasSource of result: ValueId * source: ValueId
    | IncompatibleAliasTypes of result: ValueId * source: ValueId
    | DuplicateAliasSource of result: ValueId * source: ValueId
    | UnaccountedOpaqueEffects
    | UnknownCallTarget of target: string
    | MissingCallContract of target: string
    | InvalidCallArgumentCount of target: string
    | InvalidCallArgumentType of target: string * parameterIndex: int
    | InvalidCallResultType of target: string
    | InconsistentCallContract of target: string

type Dialect<'leaf, 'block> = {
    Body: 'block -> Block<Operation<'leaf, 'block>>
    Leaf: 'leaf -> PrimitiveContract
    CallSignature: string -> FunctionSignature option
    CallContract: FunctionCall -> PrimitiveContract option
}

let verify (dialect: Dialect<'leaf, 'block>) (root: 'block) =
    let require (visible: Map<ValueId, AST.Type>) (value: Value) =
        match Map.tryFind value.Id visible with
        | None -> Error (UnknownValue value.Id)
        | Some typ when typ = value.Type -> Ok ()
        | Some _ -> Error (InconsistentValueType value.Id)
    let requireMany visible (values: Value list) =
        values
        |> List.fold (fun result value -> result |> Result.bind (fun () -> require visible value)) (Ok ())
    let operand visible (value: Operand) = value.Inputs |> Map.values |> Seq.toList |> requireMany visible
    let define declared visible (value: Value) =
        if Map.containsKey value.Id declared then Error (DuplicateDefinition value.Id)
        else Ok (Map.add value.Id value.Type declared, Map.add value.Id value.Type visible)
    let defineMany declared visible (values: Value list) =
        values
        |> List.fold (fun result value -> result |> Result.bind (fun (declared, visible) -> define declared visible value)) (Ok (declared, visible))
    let aliases (contract: PrimitiveContract) =
        let inputs = contract.Inputs |> List.map (fun value -> value.Id, value) |> Map.ofList
        let validate output source =
            match Map.tryFind source.Id inputs with
            | None -> Error (InvalidAliasSource (output.Value.Id, source.Id))
            | Some input when input.Type <> source.Type || output.Value.Type <> source.Type ->
                Error (IncompatibleAliasTypes (output.Value.Id, source.Id))
            | Some _ -> Ok ()
        let validateMany output first rest =
            let sources = first :: rest
            match sources |> List.countBy (fun value -> value.Id) |> List.tryFind (fun (_, count) -> count > 1) with
            | Some (source, _) -> Error (DuplicateAliasSource (output.Value.Id, source))
            | None ->
                sources
                |> List.fold (fun result source -> result |> Result.bind (fun () -> validate output source)) (Ok ())
        contract.Outputs
        |> List.fold (fun result output ->
            result |> Result.bind (fun () ->
                match output.Alias with
                | NoManagedAlias | FreshManaged -> Ok ()
                | MayReuseInput source -> validate output source
                | MayAliasInputs (first, rest) -> validateMany output first rest)) (Ok ())
    let effects (contract: PrimitiveContract) =
        if List.isEmpty contract.Operands || Set.contains MayEvaluateOpaqueSource contract.Effects then Ok ()
        else Error UnaccountedOpaqueEffects
    let primitive declared visible (contract: PrimitiveContract) =
        requireMany visible contract.Inputs |> Result.bind (fun () ->
            contract.Operands
            |> List.fold (fun result value -> result |> Result.bind (fun () -> operand visible value)) (Ok ())
            |> Result.bind (fun () -> effects contract)
            |> Result.bind (fun () -> aliases contract)
            |> Result.bind (fun () -> defineMany declared visible (contract.Outputs |> List.map (fun output -> output.Value))))
    let call declared visible (call: FunctionCall) =
        match dialect.CallSignature call.Target with
        | None -> Error (UnknownCallTarget call.Target)
        | Some signature when List.length signature.Parameters <> List.length call.Arguments ->
            Error (InvalidCallArgumentCount call.Target)
        | Some signature ->
            let invalidArgument =
                List.zip signature.Parameters call.Arguments
                |> List.mapi (fun index (expected, actual) -> index, expected, actual.Type)
                |> List.tryFind (fun (_, expected, actual) -> expected <> actual)
            match invalidArgument with
            | Some (index, _, _) -> Error (InvalidCallArgumentType (call.Target, index))
            | None when signature.Result <> call.Result.Type -> Error (InvalidCallResultType call.Target)
            | None ->
                match dialect.CallContract call with
                | None -> Error (MissingCallContract call.Target)
                | Some contract when contract.Inputs <> call.Arguments
                                     || not (List.isEmpty contract.Operands)
                                     || (contract.Outputs |> List.map (fun output -> output.Value)) <> [call.Result] ->
                    Error (InconsistentCallContract call.Target)
                | Some contract -> primitive declared visible contract
    let rec operations declared visible = function
        | [] -> Ok (declared, visible)
        | operation :: rest ->
            let next =
                match operation with
                | Leaf leaf ->
                    let contract: PrimitiveContract = dialect.Leaf leaf
                    primitive declared visible contract
                | ScalarBinding (result, value) ->
                    if result.Type <> value.Type then Error (BindingTypeMismatch result.Id)
                    else operand visible value |> Result.bind (fun () -> define declared visible result)
                | Call functionCall -> call declared visible functionCall
                | Branch (result, condition, ifTrue, ifFalse) ->
                    if condition.Type <> AST.TBool then Error (InvalidBranchCondition condition.Type)
                    else
                        operand visible condition |> Result.bind (fun () ->
                            block declared visible ifTrue |> Result.bind (fun (afterTrue, (trueResult: Value)) ->
                                block afterTrue visible ifFalse |> Result.bind (fun (afterFalse, (falseResult: Value)) ->
                                    if trueResult.Type <> result.Type || falseResult.Type <> result.Type then
                                        Error (InconsistentBranchResult result.Id)
                                    else define afterFalse visible result)))
            next |> Result.bind (fun (declared, visible) -> operations declared visible rest)
    and block declared visible block =
        let body: Block<Operation<'leaf, 'block>> = dialect.Body block
        defineMany declared visible (body.Parameters |> Map.values |> Seq.toList)
        |> Result.bind (fun (declared, visible) ->
            operations declared visible body.Operations |> Result.bind (fun (declared, visible) ->
                require visible body.Result |> Result.map (fun () -> declared, body.Result)))
    block Map.empty Map.empty root |> Result.map ignore
