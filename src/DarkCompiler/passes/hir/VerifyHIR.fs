// VerifyHIR.fs - Verify normalized HIR value identities and structured control-flow edges.

module VerifyHIR

open HIR

type VerificationError =
    | UnknownValue of ValueId
    | DuplicateDefinition of ValueId
    | DuplicateParameterName of string
    | DuplicateFunctionName of AST.FunctionId
    | InconsistentValueType of ValueId
    | BindingTypeMismatch of result: ValueId
    | InvalidBranchCondition of AST.SemanticType
    | InconsistentBranchResult of result: ValueId
    | InvalidAliasSource of result: ValueId * source: ValueId
    | IncompatibleAliasTypes of result: ValueId * source: ValueId
    | DuplicateAliasSource of result: ValueId * source: ValueId
    | UnaccountedOpaqueEffects
    | UnknownCallTarget of target: AST.FunctionId
    | MissingCallContract of target: AST.FunctionId
    | InvalidCallArgumentCount of target: AST.FunctionId
    | InvalidCallArgumentType of target: AST.FunctionId * parameterIndex: int
    | InvalidCallResultType of target: AST.FunctionId
    | InconsistentCallContract of target: AST.FunctionId
    | InconsistentRegisteredFunctionSignature of target: AST.FunctionId

type Dialect<'leaf, 'block> = {
    Body: 'block -> Block<Operation<'leaf, 'block>>
    Leaf: 'leaf -> PrimitiveContract
    CallSignature: AST.FunctionId -> FunctionSignature option
    CallContract: FunctionCall -> PrimitiveContract option
}

let verify (dialect: Dialect<'leaf, 'block>) (root: 'block) =
    let require (visible: Map<ValueId, AST.SemanticType>) (value: Value) =
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
    let parameters (values: Parameter list) =
        match values |> List.countBy (fun parameter -> parameter.Name) |> List.tryFind (fun (_, count) -> count > 1) with
        | Some (name, _) -> Error (DuplicateParameterName name)
        | None -> Ok (values |> List.map (fun parameter -> parameter.Value))
    let aliases (contract: PrimitiveContract) =
        let inputs : Map<ValueId, Value> =
            contract.Inputs |> List.map (fun value -> value.Id, value) |> Map.ofList
        let validate (output: OutputContract) (source: Value) =
            match Map.tryFind source.Id inputs with
            | None -> Error (InvalidAliasSource (output.Value.Id, source.Id))
            | Some input when input.Type <> source.Type || output.Value.Type <> source.Type ->
                Error (IncompatibleAliasTypes (output.Value.Id, source.Id))
            | Some _ -> Ok ()
        let validateMany (output: OutputContract) (first: Value) (rest: Value list) =
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
                | NoManagedAlias | UnknownManagedAlias | FreshManaged -> Ok ()
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
        parameters body.Parameters
        |> Result.bind (fun values ->
            defineMany declared visible values
            |> Result.bind (fun (declared, visible) ->
                operations declared visible body.Operations |> Result.bind (fun (declared, visible) ->
                    require visible body.Result |> Result.map (fun () -> declared, body.Result))))
    block Map.empty Map.empty root |> Result.map ignore

let functionSignature (dialect: Dialect<'leaf, 'block>) (definition: Function<'block>) : FunctionSignature =
    let body = dialect.Body definition.Body
    {
        Parameters = body.Parameters |> List.map (fun parameter -> parameter.Value.Type)
        Result = body.Result.Type
    }

let verifyFunction (dialect: Dialect<'leaf, 'block>) (definition: Function<'block>) =
    verify dialect definition.Body

/// Verify a mutually visible function group. Internal typed signatures are
/// derived from definitions; independently registered signatures for the same
/// names must agree. Primitive call contracts remain a separate dialect input.
let verifyFunctions (dialect: Dialect<'leaf, 'block>) (definitions: Function<'block> list) =
    match definitions |> List.countBy (fun definition -> definition.Id) |> List.tryFind (fun (_, count) -> count > 1) with
    | Some (name, _) -> Error (DuplicateFunctionName name)
    | None ->
        let signatures =
            definitions
            |> List.map (fun definition -> definition.Id, functionSignature dialect definition)
            |> Map.ofList
        let inconsistentRegistration =
            definitions
            |> List.tryPick (fun definition ->
                match dialect.CallSignature definition.Id with
                | Some registered when registered <> functionSignature dialect definition ->
                    Some (InconsistentRegisteredFunctionSignature definition.Id)
                | _ -> None)
        match inconsistentRegistration with
        | Some error -> Error error
        | None ->
            let programDialect = {
                dialect with
                    CallSignature = fun target ->
                        match Map.tryFind target signatures with
                        | Some signature -> Some signature
                        | None -> dialect.CallSignature target
            }
            definitions
            |> List.fold (fun result definition ->
                result |> Result.bind (fun () -> verifyFunction programDialect definition)) (Ok ())
