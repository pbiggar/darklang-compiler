// ConstructFunctions.fs - Normalize checked functions into structured semantic HIR.

module ConstructHIRFunctions

open HIR

/// Source scalar primitives retain the operation identity needed by later
/// lowering while exposing their semantic contract independently.
type ScalarLiteral =
    | UnitLiteral
    | Int8Literal of sbyte
    | Int16Literal of int16
    | Int32Literal of int32
    | Int64Literal of int64
    | UInt8Literal of byte
    | UInt16Literal of uint16
    | UInt32Literal of uint32
    | UInt64Literal of uint64
    | BoolLiteral of bool
    | FloatLiteral of float

type Primitive =
    | Literal of result: HIR.Value * value: ScalarLiteral
    | Unary of result: HIR.Value * op: AST.UnaryOp * operand: HIR.Value
    | Binary of result: HIR.Value * op: AST.BinOp * left: HIR.Value * right: HIR.Value

type Block = private Block of HIR.Block<HIR.Operation<Primitive, Block>>

type ConstructionError =
    | CannotInferExpression of functionName: string * message: string
    | InconsistentCallSignature of functionName: string * target: string

type CallContracts = {
    ExternalSignature: string -> HIR.FunctionSignature option
    Contract: string -> (HIR.FunctionCall -> HIR.PrimitiveContract) option
}

type private State = {
    Values: Map<string, HIR.Value>
    Operations: HIR.Operation<Primitive, Block> list
    NextId: int
}

let body (Block block) = block

let primitiveContract primitive : HIR.PrimitiveContract =
    let inputs, output, effects =
        match primitive with
        | Literal (result, _) -> [], result, Set.empty
        | Unary (result, _, operand) -> [operand], result, Set.empty
        | Binary (result, (AST.Div | AST.Mod), left, right)
            when left.Type <> AST.TFloat64 ->
            [left; right], result, Set.singleton HIR.MayFail
        | Binary (result, _, left, right) -> [left; right], result, Set.empty
    {
        Inputs = inputs
        Operands = []
        Outputs = [{ Value = output; Alias = HIR.NoManagedAlias }]
        Effects = effects
    }

let verificationDialect (calls: CallContracts) : VerifyHIR.Dialect<Primitive, Block> = {
    Body = body
    Leaf = primitiveContract
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
    let emitPrimitive state typ build =
        let result, next = fresh typ state
        let operation = HIR.Leaf (build result)
        result, { next with Operations = operation :: state.Operations }
    let literal expected expression =
        match expression, expected with
        | CheckedAST.UnitLiteral, AST.TUnit -> Some UnitLiteral
        | CheckedAST.Int8Literal value, AST.TInt8 -> Some (Int8Literal value)
        | CheckedAST.Int16Literal value, AST.TInt16 -> Some (Int16Literal value)
        | CheckedAST.Int32Literal value, AST.TInt32 -> Some (Int32Literal value)
        | CheckedAST.Int64Literal value, AST.TInt64 -> Some (Int64Literal value)
        | CheckedAST.UInt8Literal value, AST.TUInt8 -> Some (UInt8Literal value)
        | CheckedAST.UInt16Literal value, AST.TUInt16 -> Some (UInt16Literal value)
        | CheckedAST.UInt32Literal value, AST.TUInt32 -> Some (UInt32Literal value)
        | CheckedAST.UInt64Literal value, AST.TUInt64 -> Some (UInt64Literal value)
        | CheckedAST.BoolLiteral value, AST.TBool -> Some (BoolLiteral value)
        | CheckedAST.FloatLiteral value, AST.TFloat64 -> Some (FloatLiteral value)
        | _ -> None
    let nativeNumericType = function
        | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
        | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
        | AST.TFloat64 -> true
        | _ -> false
    let nativeIntegerType typ = nativeNumericType typ && typ <> AST.TFloat64
    let nativeImmediateType typ =
        nativeNumericType typ || typ = AST.TBool || typ = AST.TUnit
    let supportsUnary op operandType expected =
        operandType = expected
        && match op with
           | AST.Neg -> nativeNumericType operandType
           | AST.Not -> operandType = AST.TBool
           | AST.BitNot ->
               match operandType with
               | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
               | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 -> true
               | _ -> false
    let supportsBinary op leftType rightType expected =
        leftType = rightType
        && match op with
           | AST.Add | AST.Sub | AST.Mul | AST.Div ->
               expected = leftType && nativeNumericType leftType
           | AST.Mod -> expected = leftType && nativeIntegerType leftType
           | AST.Shl | AST.Shr | AST.BitAnd | AST.BitOr | AST.BitXor ->
               expected = leftType && nativeIntegerType leftType
           | AST.Eq | AST.Neq ->
               expected = AST.TBool && nativeImmediateType leftType
           | AST.Lt | AST.Gt | AST.Lte | AST.Gte ->
               expected = AST.TBool && nativeNumericType leftType
           | AST.And | AST.Or ->
               expected = AST.TBool && leftType = AST.TBool
           | AST.Pow | AST.StringConcat -> false
    let finishNested initialState finalState result =
        Block {
            Parameters = []
            Operations = List.rev finalState.Operations
            Result = result
        },
        { initialState with NextId = finalState.NextId }
    let rec normalize state expected expression =
        match literal expected expression with
        | Some value -> Ok (emitPrimitive state expected (fun result -> Literal (result, value)))
        | None -> normalizeNonLiteral state expected expression
    and normalizeNonLiteral state expected expression =
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
        | CheckedAST.UnaryOp (op, operandExpression) ->
            inferExpression state operandExpression
            |> Result.bind (fun operandType ->
                if supportsUnary op operandType expected then
                    normalize state operandType operandExpression
                    |> Result.map (fun (operandValue, afterOperand) ->
                        let afterOperand = { afterOperand with Values = state.Values }
                        emitPrimitive afterOperand expected (fun result ->
                            Unary (result, op, operandValue)))
                else
                    opaque state expression expected)
        | CheckedAST.BinOp (op, leftExpression, rightExpression) ->
            inferExpression state leftExpression
            |> Result.bind (fun leftType ->
                inferExpression state rightExpression
                |> Result.bind (fun rightType ->
                    if supportsBinary op leftType rightType expected then
                        normalize state leftType leftExpression
                        |> Result.bind (fun (leftValue, afterLeft) ->
                            let afterLeft = { afterLeft with Values = state.Values }
                            normalize afterLeft rightType rightExpression
                            |> Result.map (fun (rightValue, afterRight) ->
                                let afterRight = { afterRight with Values = state.Values }
                                emitPrimitive afterRight expected (fun result ->
                                    Binary (result, op, leftValue, rightValue))))
                    else
                        opaque state expression expected))
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
