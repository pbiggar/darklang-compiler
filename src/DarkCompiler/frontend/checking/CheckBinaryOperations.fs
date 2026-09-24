// CheckBinaryOperations.fs - Check BinOp expressions while preserving source diagnostics and order.

module CheckBinaryOperations

open AST
open CheckingDiagnostics
open CheckingTypes
open ComparisonPlanning
open TypeUnification
open CheckExpressionSupport

let internal check (checkExpr: ExpressionChecker) (indexedSumTypeReg: IndexedSumTypeRegistry) (env: TypeEnv) (typeReg: IndexedTypeRegistry) (variantLookup: VariantLookup) (genericFuncReg: GenericFuncRegistry) (warningSettings: WarningSettings) (moduleRegistry: ModuleRegistry) (aliasReg: AliasRegistry) (expectedType: SemanticType option) (op: BinOp) (left: Expr) (right: Expr) : Result<SemanticType * Expr, TypeError> =
    match op with
    // Arithmetic operators: T -> T -> T (where T is int or float)
    | Add | Sub | Mul | Div | Mod ->
        let opName =
            match op with
            | Add -> "+"
            | Sub -> "-"
            | Mul -> "*"
            | Div -> "/"
            | Mod -> "%"
            | _ -> Crash.crash $"Non-arithmetic operator reached arithmetic type-checking path: {op}"

        let tryAsNumericType (typ: SemanticType) : SemanticType option =
            match resolveType aliasReg typ with
            | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
            | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
            | TFloat64 as numeric ->
                Some numeric
            | _ ->
                None

        match tryExtractKnownTestRuntimeErrorMessage Map.empty left with
        | Some msg ->
            Error (GenericError $"Uncaught exception: {msg}")
        | None ->
            // Check left operand to determine numeric type
            checkExpr left env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
            |> Result.bind (fun (leftType, left') ->
                match tryAsNumericType leftType with
                | Some leftNumericType ->
                    // Runtime Dark operators inspect both operand values before
                    // deciding whether their numeric representations agree.
                    checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                    |> Result.bind (fun (rightType, right') ->
                        if rightType <> leftNumericType then
                            let errorMessage =
                                $"Cannot perform numeric operation on {typeToString leftType} and {typeToString rightType}"
                            let evaluatedOperands = "__dark_numeric_operands"
                            Ok (
                                leftNumericType,
                                Let (
                                    LPVariable evaluatedOperands,
                                    TupleLiteral [left'; right'],
                                    RuntimeError errorMessage
                                )
                            )
                        else
                            match expectedType with
                            | Some expected when expected <> leftNumericType ->
                                Error (TypeMismatch (expected, leftNumericType, $"result of {opName}"))
                            | _ -> Ok (leftNumericType, BinOp (op, left', right')))
                | None ->
                    match leftType with
                    | TVar _ | TInferenceVar _ ->
                        let rightExpectedType =
                            match expectedType with
                            | Some expected ->
                                match tryAsNumericType expected with
                                | Some numericExpected -> Some numericExpected
                                | None -> None
                            | None ->
                                None
                        checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg rightExpectedType
                        |> Result.bind (fun (rightType, right') ->
                            let inferredNumericType =
                                match rightExpectedType with
                                | Some numericExpected ->
                                    Some numericExpected
                                | None ->
                                    tryAsNumericType rightType
                            match inferredNumericType with
                            | Some numericType ->
                                match expectedType with
                                | Some expected when not (typesCompatible expected numericType) ->
                                    Error (TypeMismatch (expected, numericType, $"result of {opName}"))
                                | _ ->
                                    Ok (numericType, BinOp (op, left', right'))
                            | None ->
                                Error (InvalidOperation (opName, [leftType])))
                    | other ->
                        Error (InvalidOperation (opName, [other])))

    // Comparison operators: T -> T -> bool
    // Eq and Neq: work on any type (structural equality for complex types)
    // Lt, Gt, Lte, Gte: only work on numeric types
    | Eq | Neq | Lt | Gt | Lte | Gte ->
        let opName =
            match op with
            | Eq -> "=="
            | Neq -> "!="
            | Lt -> "<"
            | Gt -> ">"
            | Lte -> "<="
            | Gte -> ">="
            | _ -> Crash.crash $"Non-comparison operator reached comparison type-checking path: {op}"

        let lambdaLiteralFastPath : Result<SemanticType * Expr, TypeError> option =
            let comparisonResult =
                checkExpr left env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                |> Result.bind (fun (leftType, left') ->
                    // Check without context first so rejected comparisons keep
                    // the right operand's actual type in their diagnostic.
                    let rightWithoutContext =
                        checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                    let rightResult =
                        match rightWithoutContext with
                        | Ok (rightType, _) when containsTVar rightType ->
                            checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some leftType)
                        | Ok _ -> rightWithoutContext
                        | Error _ ->
                            checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some leftType)
                    rightResult
                    |> Result.bind (fun (rightType, right') ->
                        classifyComparison
                            aliasReg
                            typeReg
                            variantLookup
                            indexedSumTypeReg
                            op
                            leftType
                            rightType
                        |> Result.bind (fun plan ->
                            let comparisonExpr =
                                match plan with
                                | EqualityComparison comparableType ->
                                    let equality =
                                        buildEqExprForType aliasReg variantLookup comparableType left' right'
                                    if op = Neq then UnaryOp (Not, equality) else equality
                                | OrderingComparison numericType ->
                                    buildOrderingExprForType op numericType left' right'
                            match expectedType with
                            | Some TBool | None -> Ok (TBool, comparisonExpr)
                            | Some other -> Error (TypeMismatch (other, TBool, $"result of {opName}")))))
            Some comparisonResult

        match lambdaLiteralFastPath with
        | Some result ->
            result
        | None ->
            // Check left operand to determine type
            checkExpr left env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
            |> Result.bind (fun (leftType, left') ->
                match op with
                | Eq | Neq ->
                // Equality works on any type - both operands must be same type
                    let rightWithExpected =
                        checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some leftType)
                    let rightResult =
                        match resolveType aliasReg leftType, rightWithExpected with
                        | TSum _, Error (TypeMismatch _) ->
                            // A distinct nominal enum is a valid equality
                            // operand. Retry without forcing the left type;
                            // same-type generic calls keep the contextual
                            // inference from the successful first check.
                            checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                        | _ -> rightWithExpected

                    rightResult
                    |> Result.bind (fun (rightType, right') ->
                        let distinctNominalResult =
                            match resolveType aliasReg leftType, resolveType aliasReg rightType with
                            | TSum (leftName, _), TSum (rightName, _) when leftName <> rightName ->
                                let evaluatedOperands = "__dark_nominal_equality_operands"
                                let result = BoolLiteral (op = Neq)
                                Some (
                                    Ok (
                                        TBool,
                                        Let (
                                            LPVariable evaluatedOperands,
                                            TupleLiteral [left'; right'],
                                            result
                                        )
                                    )
                                )
                            | _ -> None

                        match distinctNominalResult with
                        | Some result -> result
                        | None ->
                        // In generic contexts, one side can still contain type variables
                        // while the other side has become concrete.
                        match reconcileTypes (Some aliasReg) leftType rightType with
                        | None ->
                            Error (TypeMismatch (leftType, rightType, $"right operand of {opName}"))
                        | Some comparableType ->
                            let tryNamedPartialState (candidate: Expr) : (string * (SemanticType * Expr) list) option =
                                match candidate with
                                | Lambda (parameters, returnAnnotation, body) ->
                                    let parameterList = NonEmptyList.toList parameters
                                    let generatedPartial =
                                        parameterList
                                        |> List.forall (fun parameter ->
                                            match parameter.Pattern with
                                            | LPVariable name -> name.StartsWith "__partial_"
                                            | _ -> false)

                                    let callDetails =
                                        match body with
                                        | Apply (Var name, typeArgs, args) -> Some (name, typeArgs, NonEmptyList.toList args)
                                        | _ -> None

                                    match generatedPartial, callDetails with
                                    | true, Some (name, typeArgs, callArgs) ->
                                        let remainingCount = List.length parameterList
                                        let appliedCount = List.length callArgs - remainingCount
                                        let trailingArgs =
                                            if appliedCount >= 0 then List.skip appliedCount callArgs else []
                                        let trailingAreParameters =
                                            appliedCount > 0
                                            && List.length trailingArgs = remainingCount
                                            && List.forall2
                                                (fun arg parameter ->
                                                    match parameter.Pattern with
                                                    | LPVariable parameterName -> arg = Var parameterName
                                                    | _ -> false)
                                                trailingArgs
                                                parameterList

                                        let concreteFunctionType =
                                            match Map.tryFind name env with
                                            | Some functionType when List.isEmpty typeArgs -> Some functionType
                                            | Some functionType ->
                                                match Map.tryFind name genericFuncReg.Functions with
                                                | Some typeParams when List.length typeParams = List.length typeArgs ->
                                                    let subst = List.zip typeParams typeArgs |> Map.ofList
                                                    Some (applySubst subst functionType)
                                                | _ -> None
                                            | None -> None

                                        match trailingAreParameters, concreteFunctionType with
                                        | true, Some (TFunction (parameterTypes, _)) when appliedCount <= List.length parameterTypes ->
                                            let identity =
                                                if List.isEmpty typeArgs then name
                                                else
                                                    let typeArgText = typeArgs |> List.map typeToString |> String.concat ", "
                                                    $"{name}<{typeArgText}>"
                                            Some (
                                                identity,
                                                List.zip
                                                    (List.take appliedCount parameterTypes)
                                                    (List.take appliedCount callArgs)
                                            )
                                        | _ -> None
                                    | _ -> None
                                | _ -> None

                            let buildNamedPartialEquality
                                (leftIdentity: string, leftState: (SemanticType * Expr) list)
                                (rightIdentity: string, rightState: (SemanticType * Expr) list)
                                : Expr =
                                let leftBindings =
                                    leftState
                                    |> List.mapi (fun index (typ, value) -> ($"__dark_partial_left_{index}", typ, value))
                                let rightBindings =
                                    rightState
                                    |> List.mapi (fun index (typ, value) -> ($"__dark_partial_right_{index}", typ, value))
                                let stateComparisons =
                                    if leftIdentity = rightIdentity && List.length leftBindings = List.length rightBindings then
                                        List.map2
                                            (fun (leftName, typ, _) (rightName, _, _) ->
                                                buildEqExprForType
                                                    aliasReg
                                                    variantLookup
                                                    typ
                                                    (Var leftName)
                                                    (Var rightName))
                                            leftBindings
                                            rightBindings
                                    else
                                        [BoolLiteral false]
                                let comparison = chainAndExpr stateComparisons
                                (leftBindings @ rightBindings)
                                |> List.foldBack
                                    (fun (name, _, value) body -> Let (LPVariable name, value, body))
                                    <| comparison

                            let eqExpr =
                                match resolveType aliasReg comparableType, tryNamedPartialState left', tryNamedPartialState right' with
                                | TFunction _, Some leftPartial, Some rightPartial ->
                                    buildNamedPartialEquality leftPartial rightPartial
                                | _ ->
                                    buildEqExprForType aliasReg variantLookup comparableType left' right'
                            let comparisonExpr =
                                if op = Neq then
                                    UnaryOp (Not, eqExpr)
                                else
                                    eqExpr
                            match expectedType with
                            | Some TBool | None -> Ok (TBool, comparisonExpr)
                            | Some other -> Error (TypeMismatch (other, TBool, $"result of {opName}")))
                | Lt | Gt | Lte | Gte ->
                    // Ordering only works on numeric types.
                    // Allow unresolved type variables here so guards like
                    // `match Error 5 with | Ok x when x > 2 -> ...` can infer x as Int64.
                    checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some leftType)
                    |> Result.bind (fun (rightType, right') ->
                        match reconcileTypes (Some aliasReg) leftType rightType with
                        | None ->
                            Error (TypeMismatch (leftType, rightType, $"right operand of {opName}"))
                        | Some comparableType ->
                            match comparableType with
                            | TInt8 | TInt16 | TInt32 | TInt64
                            | TInt
                            | TUInt8 | TUInt16 | TUInt32 | TUInt64
                            | TFloat64 ->
                                match expectedType with
                                | Some TBool | None -> Ok (TBool, BinOp (op, left', right'))
                                | Some other -> Error (TypeMismatch (other, TBool, $"result of {opName}"))
                            | other ->
                                Error (InvalidOperation (opName, [other])))
                | _ ->
                    Error (GenericError $"Unexpected comparison operator: {opName}"))

    // Boolean operators: bool -> bool -> bool
    | And | Or ->
        let opName = if op = And then "&&" else "||"

        let checkBooleanOperand (operand: Expr) : Result<Expr, TypeError> =
            let checkedOperandResult =
                match op with
                | And ->
                    checkExpr operand env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some TBool)
                | Or ->
                    checkExpr operand env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                | _ ->
                    checkExpr operand env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
            match checkedOperandResult with
            | Ok (operandType, operand') when operandType = TBool ->
                Ok operand'
            | Ok _ ->
                Error (GenericError $"{opName} only supports Booleans")
            | Error (TypeMismatch (expected, _, _)) when op = And && expected = TBool ->
                Error (GenericError $"{opName} only supports Booleans")
            | Error err ->
                Error err

        match (op, tryExtractKnownTestRuntimeErrorMessage Map.empty left) with
        | And, Some msg ->
            Error (GenericError msg)
        | _ ->
            checkBooleanOperand left
            |> Result.bind (fun left' ->
                let rightIsKnownRuntimeError = isKnownTestRuntimeErrorExpr Map.empty right
                let shortCircuitResult =
                    match (op, left', rightIsKnownRuntimeError) with
                    | (And, BoolLiteral false, true) -> Some false
                    | (Or, BoolLiteral true, true) -> Some true
                    | _ -> None
                match shortCircuitResult with
                | Some result ->
                    match expectedType with
                    | Some TBool | None -> Ok (TBool, BoolLiteral result)
                    | Some other -> Error (TypeMismatch (other, TBool, $"result of {opName}"))
                | None ->
                    match (op, tryExtractKnownTestRuntimeErrorMessage Map.empty right) with
                    | And, Some msg ->
                        Error (GenericError msg)
                    | _ ->
                        checkBooleanOperand right
                        |> Result.bind (fun right' ->
                            match expectedType with
                            | Some TBool | None -> Ok (TBool, BinOp (op, left', right'))
                            | Some other -> Error (TypeMismatch (other, TBool, $"result of {opName}"))))

    // Exponentiation is defined by the canonical numeric modules. The
    // 128-bit modules intentionally have no power operation.
    | Pow ->
        let supportsPower (typ: SemanticType) =
            match typ with
            | TInt | TInt8 | TInt16 | TInt32 | TInt64
            | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TFloat64 -> true
            | _ -> false

        checkExpr left env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
        |> Result.bind (fun (leftType, left') ->
            if not (supportsPower leftType) then
                Error
                    (GenericError
                        $"Cannot perform numeric operation on {typeToString leftType} and {typeToString leftType}")
            else
                checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some leftType)
                |> Result.bind (fun (rightType, right') ->
                    if rightType <> leftType then
                        Error (TypeMismatch (leftType, rightType, "right operand of ^"))
                    else
                        match expectedType with
                        | Some expected when expected <> leftType ->
                            Error (TypeMismatch (expected, leftType, "result of ^"))
                        | _ -> Ok (leftType, BinOp (Pow, left', right'))))

    // Internal bitwise operators: Int -> Int -> Int (same integer type).
    | Shl | Shr | BitAnd | BitOr | BitXor ->
        let opName =
            match op with
            | Shl -> "<<"
            | Shr -> ">>"
            | BitAnd -> "&"
            | BitOr -> "|"
            | BitXor -> "^"
            | _ -> Crash.crash $"Non-bitwise operator reached bitwise type-checking path: {op}"

        let isIntegerType (typ: SemanticType) =
            match typ with
            | TInt8 | TInt16 | TInt32 | TInt64 | TInt
            | TUInt8 | TUInt16 | TUInt32 | TUInt64 -> true
            | _ -> false

        checkExpr left env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
        |> Result.bind (fun (leftType, left') ->
            if not (isIntegerType leftType) then
                Error (InvalidOperation (opName, [leftType]))
            else
                checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some leftType)
                |> Result.bind (fun (rightType, right') ->
                    if rightType <> leftType then
                        Error (TypeMismatch (leftType, rightType, $"right operand of {opName}"))
                    else
                        match expectedType with
                        | Some expected when expected <> leftType ->
                            Error (TypeMismatch (expected, leftType, $"result of {opName}"))
                        | _ -> Ok (leftType, BinOp (op, left', right'))))

    // String concatenation: string -> string -> string
    | StringConcat ->
        match tryExtractKnownTestRuntimeErrorMessage Map.empty left with
        | Some msg ->
            Error (GenericError $"Uncaught exception: {msg}")
        | None ->
            checkExpr left env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some TString)
            |> Result.bind (fun (leftType, left') ->
                let isStringLike t = t = TString || t = TChar
                if not (isStringLike leftType) then
                    Error (InvalidOperation ("++", [leftType]))
                else
                    match tryExtractKnownTestRuntimeErrorMessage Map.empty right with
                    | Some msg ->
                        Error (GenericError $"Uncaught exception: {msg}")
                    | None ->
                        checkExpr right env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some TString)
                        |> Result.bind (fun (rightType, right') ->
                            if not (isStringLike rightType) then
                                Error (TypeMismatch (TString, rightType, "right operand of ++"))
                            else
                                match expectedType with
                                | Some TString | None -> Ok (TString, BinOp (op, left', right'))
                                | Some other -> Error (TypeMismatch (other, TString, "result of ++"))))
