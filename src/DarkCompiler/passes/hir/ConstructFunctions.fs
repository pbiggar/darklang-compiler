// ConstructFunctions.fs - Normalize checked functions into structured semantic HIR.

module ConstructHIRFunctions

open HIR

/// Checked-source HIR initially keeps non-structural evaluation opaque. Later
/// normalization passes may replace those scalar bindings with contracted
/// primitives and resolved calls without changing function or block edges.
type Block = private Block of HIR.Block<HIR.Operation<HIR.PrimitiveContract, Block>>

type ConstructionError =
    | CannotInferExpression of functionName: string * message: string
    | InconsistentCallSignature of functionName: string * target: string

type CallContracts = {
    ExternalSignature: string -> HIR.FunctionSignature option
    Contract: string -> (HIR.FunctionCall -> HIR.PrimitiveContract) option
}

type private State = {
    Values: Map<string, HIR.Value>
    Operations: HIR.Operation<HIR.PrimitiveContract, Block> list
    NextId: int
}

let body (Block block) = block

let verificationDialect (calls: CallContracts) : VerifyHIR.Dialect<HIR.PrimitiveContract, Block> = {
    Body = body
    Leaf = id
    CallSignature = calls.ExternalSignature
    CallContract = fun call ->
        calls.Contract call.Target
        |> Option.map (fun contract -> contract call)
}

let private signatureOfCheckedFunction (definition: CheckedAST.FunctionDef) : HIR.FunctionSignature = {
    Parameters =
        definition.Params
        |> AST.NonEmptyList.toList
        |> List.map snd
    Result = definition.ReturnType
}

let private constructWithSignatures
    (infer: Map<string, AST.Type> -> CheckedAST.Expr -> Result<AST.Type, string>)
    (dependencies: CheckedAST.Expr -> Set<string>)
    (calls: CallContracts)
    (callSignature: string -> HIR.FunctionSignature option)
    (definition: CheckedAST.FunctionDef)
    : Result<HIR.Function<Block>, ConstructionError> =
    let parameterValues, nextId =
        definition.Params
        |> AST.NonEmptyList.toList
        |> List.mapFold (fun nextId (name, typ) ->
            let parameter = {
                Name = name
                Value = { Id = HIR.ValueId nextId; Type = typ }
            }
            parameter, nextId + 1) 0
    let values =
        parameterValues
        |> List.map (fun parameter -> parameter.Name, parameter.Value)
        |> Map.ofList
    let types state = state.Values |> Map.map (fun _ value -> value.Type)
    let inferExpression state expression =
        infer (types state) expression
        |> Result.mapError (fun message -> CannotInferExpression (definition.Name, message))
    let fresh typ state =
        let value = { Id = HIR.ValueId state.NextId; Type = typ }
        value, { state with NextId = state.NextId + 1 }
    let operand state expression typ =
        let inputs =
            dependencies expression
            |> Set.toList
            |> List.choose (fun name ->
                Map.tryFind name state.Values
                |> Option.map (fun value -> name, value))
            |> Map.ofList
        { Expression = expression; Type = typ; Inputs = inputs }
    let opaque state expression typ =
        let result, next = fresh typ state
        let operation = HIR.ScalarBinding (result, operand state expression typ)
        Ok (result, { next with Operations = operation :: state.Operations })
    let finishNested initialState finalState result =
        Block {
            Parameters = []
            Operations = List.rev finalState.Operations
            Result = result
        },
        { initialState with NextId = finalState.NextId }
    let rec normalize state expected expression =
        match expression with
        | CheckedAST.Var name ->
            match Map.tryFind name state.Values with
            | Some value when value.Type = expected -> Ok (value, state)
            | _ -> opaque state expression expected
        | CheckedAST.Let (CheckedAST.LPVariable name, value, continuation) ->
            inferExpression state value
            |> Result.bind (fun valueType ->
                normalize state valueType value
                |> Result.bind (fun (boundValue, afterValue) ->
                    normalize
                        { afterValue with Values = Map.add name boundValue afterValue.Values }
                        expected
                        continuation))
        | CheckedAST.Let ((CheckedAST.LPUnit | CheckedAST.LPWildcard), value, continuation) ->
            inferExpression state value
            |> Result.bind (fun valueType ->
                normalize state valueType value
                |> Result.bind (fun (_, afterValue) -> normalize afterValue expected continuation))
        | CheckedAST.Sequence (first, continuation) ->
            normalize state AST.TUnit first
            |> Result.bind (fun (_, afterFirst) -> normalize afterFirst expected continuation)
        | CheckedAST.If (condition, ifTrue, ifFalse) ->
            let condition = operand state condition AST.TBool
            let branchState = { state with Operations = [] }
            normalize branchState expected ifTrue
            |> Result.bind (fun (trueResult, afterTrue) ->
                let trueBlock, nextAfterTrue = finishNested state afterTrue trueResult
                let falseState = { branchState with NextId = nextAfterTrue.NextId }
                normalize falseState expected ifFalse
                |> Result.map (fun (falseResult, afterFalse) ->
                    let falseBlock, nextAfterFalse = finishNested state afterFalse falseResult
                    let result, next = fresh expected nextAfterFalse
                    let branch = HIR.Branch (result, condition, trueBlock, falseBlock)
                    result, { next with Operations = branch :: state.Operations }))
        | CheckedAST.Call (target, arguments) ->
            match callSignature target, calls.Contract target with
            | Some signature, Some _ ->
                let arguments = AST.NonEmptyList.toList arguments
                if List.length arguments <> List.length signature.Parameters
                   || signature.Result <> expected then
                    Error (InconsistentCallSignature (definition.Name, target))
                else
                    normalizeArguments state target signature.Parameters arguments
                    |> Result.map (fun (arguments, afterArguments) ->
                        let result, next = fresh signature.Result afterArguments
                        let operation = HIR.Call {
                            Target = target
                            Arguments = arguments
                            Result = result
                        }
                        result, { next with Operations = operation :: afterArguments.Operations })
            | _ -> opaque state expression expected
        | _ -> opaque state expression expected
    and normalizeArguments state target parameterTypes arguments =
        match parameterTypes, arguments with
        | [], [] -> Ok ([], state)
        | parameterType :: parameterTypes, argument :: arguments ->
            inferExpression state argument
            |> Result.bind (fun argumentType ->
                if argumentType <> parameterType then
                    Error (InconsistentCallSignature (definition.Name, target))
                else
                    normalize state parameterType argument
                    |> Result.bind (fun (value, afterArgument) ->
                        let next = { afterArgument with Values = state.Values }
                        normalizeArguments next target parameterTypes arguments
                        |> Result.map (fun (values, finalState) -> value :: values, finalState)))
        | _ -> Error (InconsistentCallSignature (definition.Name, target))
    let initial = {
        Values = values
        Operations = []
        NextId = nextId
    }
    normalize initial definition.ReturnType definition.Body
    |> Result.map (fun (result, finalState) ->
        {
            Name = definition.Name
            Body = Block {
                Parameters = parameterValues
                Operations = List.rev finalState.Operations
                Result = result
            }
        })

let constructFunction infer dependencies calls (definition: CheckedAST.FunctionDef) =
    let internalSignature target =
        if target = definition.Name then Some (signatureOfCheckedFunction definition)
        else calls.ExternalSignature target
    constructWithSignatures infer dependencies calls internalSignature definition

let constructFunctions infer dependencies calls (definitions: CheckedAST.FunctionDef list) =
    let internalSignatures =
        definitions
        |> List.map (fun definition -> definition.Name, signatureOfCheckedFunction definition)
        |> Map.ofList
    let callSignature target =
        match Map.tryFind target internalSignatures with
        | Some signature -> Some signature
        | None -> calls.ExternalSignature target
    definitions
    |> List.fold (fun result definition ->
        result
        |> Result.bind (fun functions ->
            constructWithSignatures infer dependencies calls callSignature definition
            |> Result.map (fun functionDefinition -> functionDefinition :: functions))) (Ok [])
    |> Result.map List.rev
