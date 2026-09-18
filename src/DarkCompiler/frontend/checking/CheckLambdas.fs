// CheckLambdas.fs - Check Lambda expressions while preserving source diagnostics and order.

module CheckLambdas

open AST
open CheckingDiagnostics
open CheckingTypes
open CheckedFreeVariables
open TypeUnification
open CheckExpressionSupport

let internal check (checkExpr: ExpressionChecker) (env: TypeEnv) (typeReg: IndexedTypeRegistry) (variantLookup: VariantLookup) (genericFuncReg: GenericFuncRegistry) (warningSettings: WarningSettings) (moduleRegistry: ModuleRegistry) (aliasReg: AliasRegistry) (expectedType: Type option) (parameters: NonEmptyList<LambdaParameter>) (returnAnnotation: Type option) (body: Expr) : Result<Type * Expr, TypeError> =
    let parametersList = NonEmptyList.toList parameters
    let parameterNames =
        parametersList
        |> List.collect (fun parameter -> letPatternBindings parameter.Pattern)
        |> Set.ofList

    let concreteTypeOf candidate =
        let candidateType =
            match candidate with
            | UnitLiteral -> Some TUnit
            | Int64Literal _ -> Some TInt64
            | Int128Literal _ -> Some TInt128
            | BigIntLiteral _ -> Some TInt
            | Int8Literal _ -> Some TInt8
            | Int16Literal _ -> Some TInt16
            | Int32Literal _ -> Some TInt32
            | UInt8Literal _ -> Some TUInt8
            | UInt16Literal _ -> Some TUInt16
            | UInt32Literal _ -> Some TUInt32
            | UInt64Literal _ -> Some TUInt64
            | UInt128Literal _ -> Some TUInt128
            | FloatLiteral _ -> Some TFloat64
            | BoolLiteral _ -> Some TBool
            | StringLiteral _ -> Some TString
            | CharLiteral _ -> Some TChar
            | Var name -> Map.tryFind name env
            | Call (name, _) ->
                match Map.tryFind name env with
                | Some (TFunction (_, returnType)) -> Some returnType
                | _ ->
                    Stdlib.tryGetFunction moduleRegistry name
                    |> Option.map (fun (fn, _) -> fn.ReturnType)
            | _ -> None
        candidateType |> Option.filter (containsTVar >> not)

    let addConstraint name typ constraints =
        if Set.contains name parameterNames && not (containsTVar typ) then
            match Map.tryFind name constraints with
            | Some existing ->
                match reconcileTypes (Some aliasReg) existing typ with
                | Some reconciled -> Map.add name reconciled constraints
                | None -> constraints
            | None -> Map.add name typ constraints
        else
            constraints

    let rec collectConstraints expected candidate constraints =
        let collect expectedType inner current =
            collectConstraints expectedType inner current
        let collectChildren children current =
            children |> List.fold (fun state child -> collect None child state) current
        match candidate with
        | Var name ->
            expected |> Option.map (fun typ -> addConstraint name typ constraints) |> Option.defaultValue constraints
        | BinOp (op, left, right) ->
            match op with
            | Add | Sub | Mul | Div | Mod | Pow | Shl | Shr | BitAnd | BitOr | BitXor ->
                let operandType =
                    expected
                    |> Option.filter (containsTVar >> not)
                    |> Option.orElseWith (fun () -> concreteTypeOf left)
                    |> Option.orElseWith (fun () -> concreteTypeOf right)
                constraints |> collect operandType left |> collect operandType right
            | Lt | Gt | Lte | Gte | Eq | Neq ->
                let operandType = concreteTypeOf left |> Option.orElseWith (fun () -> concreteTypeOf right)
                constraints |> collect operandType left |> collect operandType right
            | StringConcat -> constraints |> collect (Some TString) left |> collect (Some TString) right
            | And | Or -> constraints |> collect (Some TBool) left |> collect (Some TBool) right
        | UnaryOp (Not, inner) -> collect (Some TBool) inner constraints
        | UnaryOp (_, inner) -> collect expected inner constraints
        | Call (name, arguments) ->
            let argumentList = NonEmptyList.toList arguments
            match Map.tryFind name env with
            | Some (TFunction (parameterTypes, _)) when List.length parameterTypes = List.length argumentList ->
                List.zip parameterTypes argumentList
                |> List.fold (fun state (parameterType, argument) -> collect (Some parameterType) argument state) constraints
            | _ -> collectChildren argumentList constraints
        | Apply (func, arguments)
        | IndirectApply (func, arguments) ->
            let argumentList = NonEmptyList.toList arguments
            let constraints = collect None func constraints
            match func with
            | Var name ->
                match Map.tryFind name env with
                | Some (TFunction (parameterTypes, _)) when List.length argumentList <= List.length parameterTypes ->
                    List.zip (List.take (List.length argumentList) parameterTypes) argumentList
                    |> List.fold (fun state (parameterType, argument) -> collect (Some parameterType) argument state) constraints
                | _ -> collectChildren argumentList constraints
            | _ -> collectChildren argumentList constraints
        | Let (pattern, value, continuation) ->
            let afterValue = collect None value constraints
            if letPatternBindings pattern |> List.exists (fun name -> Set.contains name parameterNames) then afterValue
            else collect expected continuation afterValue
        | RecursiveLet (_, value, continuation) ->
            constraints |> collect None value |> collect expected continuation
        | Lambda (nestedParameters, _, nestedBody) ->
            let shadows =
                nestedParameters
                |> NonEmptyList.toList
                |> List.collect (fun parameter -> letPatternBindings parameter.Pattern)
                |> List.exists (fun name -> Set.contains name parameterNames)
            if shadows then
                constraints
            else
                let nestedReturnExpectation =
                    match expected with
                    | Some (TFunction (_, returnType)) -> Some returnType
                    | _ -> None
                collect nestedReturnExpectation nestedBody constraints
        | BoundaryRender (_, value) | TupleAccess (value, _) | RecordAccess (value, _) ->
            collect None value constraints
        | Sequence (first, next) -> constraints |> collect None first |> collect expected next
        | If (condition, thenBranch, elseBranch) ->
            constraints
            |> collect (Some TBool) condition
            |> collect expected thenBranch
            |> collect expected elseBranch
        | TypeApp (_, _, arguments) -> collectChildren (NonEmptyList.toList arguments) constraints
        | TupleLiteral elements | ListLiteral elements -> collectChildren elements constraints
        | DictLiteral (_, _, entries) ->
            collectChildren (entries |> List.collect (fun (key, value) -> [key; value])) constraints
        | RecordLiteral (_, fields) -> collectChildren (fields |> List.map snd) constraints
        | RecordUpdate (record, fields) -> collectChildren (record :: (fields |> List.map snd)) constraints
        | Constructor (_, _, fields) -> collectChildren fields constraints
        | Match (scrutinee, cases) ->
            let afterScrutinee = collect None scrutinee constraints
            cases
            |> List.fold (fun state case ->
                state
                |> fun current -> case.Guard |> Option.map (fun guard -> collect (Some TBool) guard current) |> Option.defaultValue current
                |> collect expected case.Body) afterScrutinee
        | Closure (_, captures) -> collectChildren captures constraints
        | InterpolatedString parts ->
            parts
            |> List.choose (function StringExpr inner -> Some inner | StringText _ -> None)
            |> fun children -> collectChildren children constraints
        | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _
        | Int8Literal _ | Int16Literal _ | Int32Literal _ | UInt8Literal _
        | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
        | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _
        | FuncRef _ | RuntimeError _ -> constraints

    let bodyConstraints = collectConstraints returnAnnotation body Map.empty

    let rec refinePatternType pattern typ =
        match pattern, typ with
        | LPVariable name, _ -> Map.tryFind name bodyConstraints |> Option.defaultValue typ
        | LPTuple (first, second, rest), TTuple elementTypes ->
            let patterns = first :: second :: rest
            if List.length patterns = List.length elementTypes then
                List.zip patterns elementTypes
                |> List.map (fun (innerPattern, innerType) -> refinePatternType innerPattern innerType)
                |> TTuple
            else typ
        | _ -> typ

    let typeCheckLambdaWithParams
        (resolvedParams: (LambdaParameter * Type) list)
        (bodyExpectedType: Type option)
        : Result<Type * Expr, TypeError> =
        let bindingResults =
            resolvedParams
            |> List.map (fun (parameter, typ) -> bindLetPatternTypes parameter.Pattern typ)
        if bindingResults |> List.exists Option.isNone then
            Error (GenericError "Lambda parameter pattern is incompatible with its inferred type")
        else
            let bindings = bindingResults |> List.choose id |> List.concat
            let paramEnv =
                bindings
                |> List.fold (fun current (name, typ) -> Map.add name typ current) env
            let effectiveBodyExpectedType =
                returnAnnotation |> Option.orElse bodyExpectedType
            checkExpr body paramEnv typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg effectiveBodyExpectedType
            |> Result.map (fun (bodyType, body') ->
                let paramTypes = resolvedParams |> List.map snd
                let typedParams =
                    resolvedParams
                    |> List.map (fun (parameter, typ) -> { parameter with InferredType = Some typ })
                    |> NonEmptyList.fromList
                (TFunction (paramTypes, bodyType), Lambda (typedParams, returnAnnotation, body')))

    let initialParams =
        parametersList
        |> List.mapi (fun index parameter ->
            let initialType =
                parameter.InferredType
                |> Option.orElse parameter.SourceAnnotation
                |> Option.defaultValue (inferredLetPatternType $"lambda_{index}" parameter.Pattern)
            (parameter, refinePatternType parameter.Pattern initialType))

    match expectedType with
    | Some (TFunction (expectedParams, expectedRet)) ->
        if List.length expectedParams < List.length parametersList then
            // Interpreter lambdas may expose a prefix of their binders as
            // the callable expected by a higher-order argument. Preserve
            // the remaining binders as a returned lambda so applying the
            // outer closure once has the same curried behavior.
            let (outerParameters, remainingParameters) =
                List.splitAt (List.length expectedParams) parametersList
            match NonEmptyList.tryFromList outerParameters, NonEmptyList.tryFromList remainingParameters with
            | Some outer, Some remaining ->
                let nested = Lambda (remaining, returnAnnotation, body)
                checkExpr
                    (Lambda (outer, None, nested))
                    env
                    typeReg
                    variantLookup
                    genericFuncReg
                    warningSettings
                    moduleRegistry
                    aliasReg
                    expectedType
            | _ -> Error (GenericError "Lambda currying requires non-empty binder groups")
        elif List.length expectedParams <> List.length parametersList then
            Error (GenericError $"Expected {List.length parametersList} arguments, got {List.length expectedParams}")
        else
            let rec reconcileParamTypes
                (remaining: ((LambdaParameter * Type) * Type) list)
                (acc: (LambdaParameter * Type) list)
                : Result<(LambdaParameter * Type) list, TypeError> =
                match remaining with
                | [] -> Ok (List.rev acc)
                | ((parameter, declaredParamType), expectedParamType) :: rest ->
                    match reconcileTypes (Some aliasReg) declaredParamType expectedParamType with
                    | Some reconciledParamType ->
                        reconcileParamTypes rest ((parameter, reconciledParamType) :: acc)
                    | None ->
                        Error (TypeMismatch (expectedParamType, declaredParamType, "lambda parameter type"))

            reconcileParamTypes (List.zip initialParams expectedParams) []
            |> Result.bind (fun reconciledParams ->
                let bodyExpectedType =
                    if containsTVar expectedRet then
                        None
                    else
                        Some expectedRet
                typeCheckLambdaWithParams reconciledParams bodyExpectedType
                |> Result.bind (fun (funcType, lambdaExpr) ->
                    match funcType with
                    | TFunction (paramTypes, bodyType) ->
                        match reconcileTypes (Some aliasReg) expectedRet bodyType with
                        | None ->
                            Error (TypeMismatch (expectedRet, bodyType, "lambda return type"))
                        | Some reconciledRetType ->
                            let concreteReturnType =
                                if bodyType = TRuntimeError && containsTVar expectedRet then
                                    // Bottom has no runtime payload representation. Unit is
                                    // the canonical monomorphic witness when the result is
                                    // otherwise unconstrained.
                                    TUnit
                                else
                                    reconciledRetType
                            Ok (TFunction (paramTypes, concreteReturnType), lambdaExpr)
                    | _ ->
                        Error (GenericError "Internal error: lambda did not type-check to a function")))
    | Some other ->
        typeCheckLambdaWithParams initialParams None
        |> Result.bind (fun (funcType, lambdaExpr) ->
            match reconcileTypes (Some aliasReg) other funcType with
            | Some reconciledType -> Ok (reconciledType, lambdaExpr)
            | None -> Error (TypeMismatch (other, funcType, "lambda")))
    | None ->
        typeCheckLambdaWithParams initialParams None
