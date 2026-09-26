// ANF_Intrinsics.fs - Give named fixed-width arithmetic and operators one ANF operation.

module ANF_Intrinsics

open ANF
open ANFExpressionOptimization
open TypeRegistries

type ArithmeticIntrinsic = {
    OperandType: AST.SemanticType
    Operation: BinOp
}

let private nativeIntegerTypes = [
    "Int8", AST.TInt8
    "Int16", AST.TInt16
    "Int32", AST.TInt32
    "Int64", AST.TInt64
    "UInt8", AST.TUInt8
    "UInt16", AST.TUInt16
    "UInt32", AST.TUInt32
    "UInt64", AST.TUInt64
]

let private arithmeticOperations = [
    "add", Add
    "subtract", Sub
    "multiply", Mul
    "divide", Div
]

let private intrinsicFunctions
    (functionIds: FunctionIdRegistry)
    (functions: FunctionRegistry)
    : Map<AST.FunctionId, ArithmeticIntrinsic> =
    nativeIntegerTypes
    |> List.collect (fun (typeName, operandType) ->
        arithmeticOperations
        |> List.choose (fun (operationName, operation) ->
            let name = $"Darklang.Stdlib.{typeName}.{operationName}"
            Map.tryFind name functionIds
            |> Option.map (fun id ->
                match Map.tryFind id functions with
                | Some (_, AST.TFunction ([left; right], result))
                    when left = operandType && right = operandType && result = operandType ->
                    id, { OperandType = operandType; Operation = operation }
                | _ ->
                    Crash.crash $"Arithmetic intrinsic {name} has an unexpected signature")))
    |> Map.ofList

let private canonicalizeCExpr intrinsics cexpr =
    match cexpr with
    | Call (target, [left; right])
    | BorrowedCall (target, [left; right])
    | TailCall (target, [left; right]) ->
        match Map.tryFind target intrinsics with
        | Some intrinsic -> Prim (intrinsic.Operation, left, right)
        | None -> cexpr
    | _ -> cexpr

let rec private canonicalizeExpr intrinsics expr =
    match expr with
    | Let (result, operation, continuation) ->
        Let (result, canonicalizeCExpr intrinsics operation, canonicalizeExpr intrinsics continuation)
    | Join (parameter, continuation, entry) ->
        Join (parameter, canonicalizeExpr intrinsics continuation, canonicalizeExpr intrinsics entry)
    | If (condition, whenTrue, whenFalse) ->
        If (condition, canonicalizeExpr intrinsics whenTrue, canonicalizeExpr intrinsics whenFalse)
    | Return _ | Jump _ -> expr

/// A function reference keeps its resolved ID. Its compiled definition is the
/// callable adapter below; direct calls and the adapter both use the same Prim.
let canonicalizeProgram
    (functionIds: FunctionIdRegistry)
    (functions: FunctionRegistry)
    (program: Program)
    : Program =
    let intrinsics = intrinsicFunctions functionIds functions
    if Map.isEmpty intrinsics then program
    else
        let (Program (definitions, main)) = program
        let initialVarGen =
            if definitions |> List.exists (fun definition -> Map.containsKey definition.Id intrinsics) then
                freshVarGenForProgram program
            else
                initialVarGen
        let definitions, _ =
            definitions
            |> List.mapFold (fun varGen definition ->
                match Map.tryFind definition.Id intrinsics with
                | None ->
                    { definition with Body = canonicalizeExpr intrinsics definition.Body }, varGen
                | Some intrinsic ->
                    match definition.TypedParams with
                    | [left; right]
                        when left.Type = intrinsic.OperandType
                             && right.Type = intrinsic.OperandType
                             && definition.ReturnType = intrinsic.OperandType ->
                        let result, next = freshVar varGen
                        { definition with
                            Body =
                                Let (
                                    result,
                                    Prim (intrinsic.Operation, Var left.Id, Var right.Id),
                                    Return (Var result)) }, next
                    | _ ->
                        Crash.crash $"Arithmetic intrinsic adapter {definition.Name} has an unexpected ANF signature") initialVarGen
        Program (definitions, canonicalizeExpr intrinsics main)
