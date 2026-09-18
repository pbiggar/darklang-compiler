// ConstructFunctions.fs - Normalize checked functions into structured semantic HIR.

module ConstructHIRFunctions

open HIR

/// Checked-source HIR initially keeps non-structural evaluation opaque. Later
/// normalization passes may replace those scalar bindings with contracted
/// primitives and resolved calls without changing function or block edges.
type Block = private Block of HIR.Block<HIR.Operation<HIR.PrimitiveContract, Block>>

type ConstructionError =
    | CannotInferExpression of functionName: string * message: string

type private State = {
    Values: Map<string, HIR.Value>
    Operations: HIR.Operation<HIR.PrimitiveContract, Block> list
    NextId: int
}

let body (Block block) = block

let verificationDialect : VerifyHIR.Dialect<HIR.PrimitiveContract, Block> = {
    Body = body
    Leaf = id
    CallSignature = fun _ -> None
    CallContract = fun _ -> None
}

let constructFunction
    (infer: Map<string, AST.Type> -> CheckedAST.Expr -> Result<AST.Type, string>)
    (dependencies: CheckedAST.Expr -> Set<string>)
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
        | _ -> opaque state expression expected
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

let constructFunctions infer dependencies definitions =
    definitions
    |> List.fold (fun result definition ->
        result
        |> Result.bind (fun functions ->
            constructFunction infer dependencies definition
            |> Result.map (fun functionDefinition -> functionDefinition :: functions))) (Ok [])
    |> Result.map List.rev
