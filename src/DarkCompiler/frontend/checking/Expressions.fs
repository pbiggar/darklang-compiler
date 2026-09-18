// Expressions.fs - Dispatch expression checking and propagate contextual expectations.

module CheckExpressions

open CheckExpressionSupport

open AST
open CheckingDiagnostics
open CheckingTypes
open ComparisonPlanning
open CheckedFreeVariables
open TypeUnification

/// Check expression type top-down, potentially transforming the expression.
/// Parameters:
///   - expr: Expression to type-check
///   - env: Type environment (variable name -> type mappings)
///   - typeReg: Type registry (record type name -> field definitions)
///   - variantLookup: Maps variant names to (type name, tag index)
///   - genericFuncReg: Registry of generic functions (function name -> type params)
///   - expectedType: Optional expected type from context (for checking)
/// Returns: Result<Type * Expr, TypeError>
///   - Type: The type of the expression
///   - Expr: The (possibly transformed) expression
let rec internal checkExprWithParamNamesAndSumTypeNames
    (funcParamNameReg: Map<string, string list>)
    (sumTypeNames: Set<string>)
    (indexedSumTypeReg: IndexedSumTypeRegistry)
    (expr: Expr)
    (env: TypeEnv)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (genericFuncReg: GenericFuncRegistry)
    (warningSettings: WarningSettings)
    (moduleRegistry: ModuleRegistry)
    (aliasReg: AliasRegistry)
    (expectedType: Type option)
    : Result<Type * Expr, TypeError> =
    let checkExpr
        (innerExpr: Expr)
        (innerEnv: TypeEnv)
        (innerTypeReg: IndexedTypeRegistry)
        (innerVariantLookup: VariantLookup)
        (innerGenericFuncReg: GenericFuncRegistry)
        (innerWarningSettings: WarningSettings)
        (innerModuleRegistry: ModuleRegistry)
        (innerAliasReg: AliasRegistry)
        (innerExpectedType: Type option)
        : Result<Type * Expr, TypeError> =
        checkExprWithParamNamesAndSumTypeNames
            funcParamNameReg
            sumTypeNames
            indexedSumTypeReg
            innerExpr
            innerEnv
            innerTypeReg
            innerVariantLookup
            innerGenericFuncReg
            innerWarningSettings
            innerModuleRegistry
            innerAliasReg
            innerExpectedType

    let expectedType =
        expectedType
        |> Option.map (canonicalizeBareSumTypeRefsWithNames sumTypeNames)

    let rec tryFindCallArguments (targetName: string) (candidate: Expr) : Expr list option =
        let tryChildren children = children |> List.tryPick (tryFindCallArguments targetName)
        let letPatternShadows pattern =
            AST.letPatternBindings pattern |> List.contains targetName
        let matchPatternShadows pattern =
            AST.validateBinders (MatchBinderPattern pattern)
            |> Result.map (List.contains targetName)
            |> Result.defaultValue true

        match candidate with
        | Call (name, args) when name = targetName -> Some (NonEmptyList.toList args)
        | Apply (Var name, args) when name = targetName -> Some (NonEmptyList.toList args)
        | Let (pattern, value, body) ->
            match tryFindCallArguments targetName value with
            | Some args -> Some args
            | None when letPatternShadows pattern -> None
            | None -> tryFindCallArguments targetName body
        | RecursiveLet (recursion, value, body) ->
            match tryFindCallArguments targetName value with
            | Some args -> Some args
            | None when recursiveBindingName recursion = targetName -> None
            | None -> tryFindCallArguments targetName body
        | Lambda (parameters, returnAnnotation, body) ->
            let shadowed =
                parameters
                |> NonEmptyList.toList
                |> List.collect (fun parameter -> AST.letPatternBindings parameter.Pattern)
                |> List.contains targetName
            if shadowed then None else tryFindCallArguments targetName body
        | Match (scrutinee, cases) ->
            match tryFindCallArguments targetName scrutinee with
            | Some args -> Some args
            | None ->
                cases
                |> List.tryPick (fun case ->
                    let shadows =
                        case.Patterns
                        |> NonEmptyList.toList
                        |> List.exists matchPatternShadows
                    let guardResult =
                        if shadows then None
                        else case.Guard |> Option.bind (tryFindCallArguments targetName)
                    guardResult
                    |> Option.orElseWith (fun () ->
                        if shadows then None
                        else tryFindCallArguments targetName case.Body))
        | BoundaryRender (_, value)
        | UnaryOp (_, value)
        | TupleAccess (value, _)
        | RecordAccess (value, _) -> tryFindCallArguments targetName value
        | BinOp (_, left, right)
        | Sequence (left, right) -> tryChildren [left; right]
        | If (condition, thenBranch, elseBranch) ->
            tryChildren [condition; thenBranch; elseBranch]
        | Call (_, args)
        | TypeApp (_, _, args) -> args |> NonEmptyList.toList |> tryChildren
        | TupleLiteral elements
        | ListLiteral elements -> tryChildren elements
        | DictLiteral (_, entries) -> entries |> List.map snd |> tryChildren
        | RecordLiteral (_, fields) -> fields |> List.map snd |> tryChildren
        | RecordUpdate (record, fields) -> record :: (fields |> List.map snd) |> tryChildren
        | Constructor (_, _, payload) -> payload |> Option.bind (tryFindCallArguments targetName)
        | Apply (func, args)
        | IndirectApply (func, args) -> func :: NonEmptyList.toList args |> tryChildren
        | Closure (_, captures) -> tryChildren captures
        | InterpolatedString parts ->
            parts
            |> List.choose (function StringExpr inner -> Some inner | StringText _ -> None)
            |> tryChildren
        | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _
        | Int8Literal _ | Int16Literal _ | Int32Literal _ | UInt8Literal _
        | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
        | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _
        | Var _ | FuncRef _ | RuntimeError _ -> None

    let inferFunctionExpectationFromArguments
        (parameterCount: int)
        (arguments: Expr list)
        : Type option =
        if List.length arguments <> parameterCount then
            None
        else
            arguments
            |> ResultList.traverse (fun argument ->
                checkExpr argument env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                |> Result.map fst)
            |> Result.toOption
            |> Option.map (fun argumentTypes ->
                TFunction (argumentTypes, TVar "binding_return"))

    let rec tryFindFunctionValueExpectation
        (targetName: string)
        (candidate: Expr)
        : Type option =
        let tryChildren children =
            children |> List.tryPick (tryFindFunctionValueExpectation targetName)
        let fromCall (functionName: string) (arguments: Expr list) =
            match Map.tryFind functionName env with
            | Some (TFunction (parameterTypes, _)) when List.length parameterTypes = List.length arguments ->
                let parameterArguments = List.zip parameterTypes arguments
                match
                    parameterArguments
                    |> List.tryPick (fun (parameterType, argument) ->
                        match argument with
                        | Var name when name = targetName -> Some parameterType
                        | _ -> None)
                with
                | Some targetParameterType ->
                    let siblingBindings =
                        parameterArguments
                        |> ResultList.traverse (fun (parameterType, argument) ->
                            match argument with
                            | Var name when name = targetName -> Ok []
                            | _ ->
                                checkExpr argument env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                                |> Result.bind (fun (argumentType, _) ->
                                    match matchTypes parameterType argumentType with
                                    | Ok bindings -> Ok bindings
                                    | Error _ -> Ok []))
                        |> Result.map List.concat
                        |> Result.bind (consolidateBindings >> Result.mapError GenericError)
                        |> Result.toOption
                        |> Option.defaultValue Map.empty
                    Some (applySubst siblingBindings targetParameterType)
                | None ->
                    parameterArguments
                    |> List.tryPick (fun (_, argument) -> tryFindFunctionValueExpectation targetName argument)
            | _ -> tryChildren arguments

        match candidate with
        | Call (functionName, arguments) -> fromCall functionName (NonEmptyList.toList arguments)
        | TypeApp (functionName, _, arguments) -> fromCall functionName (NonEmptyList.toList arguments)
        | Let (pattern, value, body) ->
            tryFindFunctionValueExpectation targetName value
            |> Option.orElseWith (fun () ->
                if letPatternBindings pattern |> List.contains targetName then None
                else tryFindFunctionValueExpectation targetName body)
        | RecursiveLet (recursion, value, body) ->
            tryFindFunctionValueExpectation targetName value
            |> Option.orElseWith (fun () ->
                if recursiveBindingName recursion = targetName then None
                else tryFindFunctionValueExpectation targetName body)
        | Lambda (parameters, _, body) ->
            let shadows =
                parameters
                |> NonEmptyList.toList
                |> List.collect (fun parameter -> letPatternBindings parameter.Pattern)
                |> List.contains targetName
            if shadows then None else tryFindFunctionValueExpectation targetName body
        | BoundaryRender (_, value) | UnaryOp (_, value)
        | TupleAccess (value, _) | RecordAccess (value, _) ->
            tryFindFunctionValueExpectation targetName value
        | BinOp (_, left, right) | Sequence (left, right) -> tryChildren [left; right]
        | If (condition, thenBranch, elseBranch) -> tryChildren [condition; thenBranch; elseBranch]
        | TupleLiteral elements | ListLiteral elements -> tryChildren elements
        | DictLiteral (_, entries) -> entries |> List.map snd |> tryChildren
        | RecordLiteral (_, fields) -> fields |> List.map snd |> tryChildren
        | RecordUpdate (record, fields) -> record :: (fields |> List.map snd) |> tryChildren
        | Constructor (_, _, payload) -> payload |> Option.bind (tryFindFunctionValueExpectation targetName)
        | Match (scrutinee, cases) ->
            scrutinee
            :: (cases |> List.collect (fun case -> Option.toList case.Guard @ [case.Body]))
            |> tryChildren
        | Apply (func, arguments)
        | IndirectApply (func, arguments) -> func :: NonEmptyList.toList arguments |> tryChildren
        | Closure (_, captures) -> tryChildren captures
        | InterpolatedString parts ->
            parts
            |> List.choose (function StringExpr inner -> Some inner | StringText _ -> None)
            |> tryChildren
        | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _
        | Int8Literal _ | Int16Literal _ | Int32Literal _ | UInt8Literal _
        | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
        | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _
        | Var _ | FuncRef _ | RuntimeError _ -> None

    match expr with
    | BoundaryRender (renderer, value) ->
        checkExpr value env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
        |> Result.map (fun (_, value') -> (TString, BoundaryRender (renderer, value')))
    | RuntimeError message -> Ok (TRuntimeError, RuntimeError message)
    | UnitLiteral ->
        // Unit literal is always TUnit
        match expectedType with
        | Some expected when not (typesCompatible expected TUnit) ->
            Error (TypeMismatch (expected, TUnit, "unit literal"))
        | _ -> Ok (TUnit, expr)

    | Int64Literal _ ->
        match expectedType with
        | Some TInt64 | None -> Ok (TInt64, expr)
        | Some other ->
            // Handle type variables (e.g., when expected is TVar "t")
            match reconcileTypes (Some aliasReg) other TInt64 with
            | Some TInt64 -> Ok (TInt64, expr)
            | _ -> Error (TypeMismatch (other, TInt64, "integer literal"))

    | Int128Literal _ ->
        match expectedType with
        | Some TInt128 | None -> Ok (TInt128, expr)
        | Some other ->
            match reconcileTypes (Some aliasReg) other TInt128 with
            | Some TInt128 -> Ok (TInt128, expr)
            | _ -> Error (TypeMismatch (other, TInt128, "integer literal"))

    | BigIntLiteral _ ->
        match expectedType with
        | Some TInt | None -> Ok (TInt, expr)
        | Some other ->
            // A type variable or an alias of Int, as for the sized literals.
            match reconcileTypes (Some aliasReg) other TInt with
            | Some TInt -> Ok (TInt, expr)
            | _ -> Error (TypeMismatch (other, TInt, "Int literal"))

    | Int8Literal _ ->
        match expectedType with
        | Some expected when not (typesCompatible expected TInt8) ->
            Error (TypeMismatch (expected, TInt8, "Int8 literal"))
        | _ -> Ok (TInt8, expr)

    | Int16Literal _ ->
        match expectedType with
        | Some expected when not (typesCompatible expected TInt16) ->
            Error (TypeMismatch (expected, TInt16, "Int16 literal"))
        | _ -> Ok (TInt16, expr)

    | Int32Literal _ ->
        match expectedType with
        | Some expected when not (typesCompatible expected TInt32) ->
            Error (TypeMismatch (expected, TInt32, "Int32 literal"))
        | _ -> Ok (TInt32, expr)

    | UInt8Literal _ ->
        match expectedType with
        | Some expected when not (typesCompatible expected TUInt8) ->
            Error (TypeMismatch (expected, TUInt8, "UInt8 literal"))
        | _ -> Ok (TUInt8, expr)

    | UInt16Literal _ ->
        match expectedType with
        | Some expected when not (typesCompatible expected TUInt16) ->
            Error (TypeMismatch (expected, TUInt16, "UInt16 literal"))
        | _ -> Ok (TUInt16, expr)

    | UInt32Literal _ ->
        match expectedType with
        | Some expected when not (typesCompatible expected TUInt32) ->
            Error (TypeMismatch (expected, TUInt32, "UInt32 literal"))
        | _ -> Ok (TUInt32, expr)

    | UInt64Literal _ ->
        match expectedType with
        | Some expected when not (typesCompatible expected TUInt64) ->
            Error (TypeMismatch (expected, TUInt64, "UInt64 literal"))
        | _ -> Ok (TUInt64, expr)

    | UInt128Literal _ ->
        match expectedType with
        | Some expected when not (typesCompatible expected TUInt128) ->
            Error (TypeMismatch (expected, TUInt128, "UInt128 literal"))
        | _ -> Ok (TUInt128, expr)

    | BoolLiteral _ ->
        // Boolean literals are always TBool
        match expectedType with
        | Some expected when not (typesCompatible expected TBool) ->
            Error (TypeMismatch (expected, TBool, "boolean literal"))
        | _ -> Ok (TBool, expr)

    | StringLiteral _ ->
        // String literals are always TString
        match expectedType with
        | Some expected when not (typesCompatibleWithAliases aliasReg expected TString) ->
            Error (TypeMismatch (expected, TString, "string literal"))
        | _ -> Ok (TString, expr)

    | CharLiteral _ ->
        // Char literals are always TChar (single Extended Grapheme Cluster)
        match expectedType with
        | Some expected when not (typesCompatible expected TChar) ->
            Error (TypeMismatch (expected, TChar, "char literal"))
        | _ -> Ok (TChar, expr)

    | InterpolatedString parts ->
        // Interpolated strings are always TString
        // Check that all expression parts are strings
        let rec checkParts (parts: StringPart list) (checkedParts: StringPart list) : Result<StringPart list, TypeError> =
            match parts with
            | [] -> Ok (List.rev checkedParts)
            | StringText s :: rest ->
                checkParts rest (StringText s :: checkedParts)
            | StringExpr e :: rest ->
                let checkedPartResult =
                    checkExpr e env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some TString)
                let normalizedPartResult =
                    match checkedPartResult with
                    | Error (UndefinedVariable name) ->
                        Error (UndefinedCallTarget name)
                    | Error (TypeMismatch (expected, _, _)) when expected = TString ->
                        checkExpr e env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                        |> Result.mapError (fun innerErr ->
                            match innerErr with
                            | UndefinedVariable name -> UndefinedCallTarget name
                            | _ -> innerErr)
                        |> Result.bind (fun (actualType, checkedExpr) ->
                            Error (GenericError (interpolationTypeMismatchMessage checkedExpr actualType)))
                    | _ ->
                        checkedPartResult
                normalizedPartResult
                |> Result.bind (fun (partType, checkedExpr) ->
                    if partType = TString || partType = TChar then
                        checkParts rest (StringExpr checkedExpr :: checkedParts)
                    else
                        Error (GenericError (interpolationTypeMismatchMessage checkedExpr partType)))
        match checkParts parts [] with
        | Ok checkedParts ->
            match expectedType with
            | Some TString | None -> Ok (TString, InterpolatedString checkedParts)
            | Some other -> Error (TypeMismatch (other, TString, "interpolated string"))
        | Error err -> Error err

    | FloatLiteral _ ->
        // Float literals are always TFloat64
        match expectedType with
        | Some expected when not (typesCompatible expected TFloat64) ->
            Error (TypeMismatch (expected, TFloat64, "float literal"))
        | _ -> Ok (TFloat64, expr)

    | BinOp (op, left, right) ->
        CheckBinaryOperations.check checkExpr indexedSumTypeReg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg expectedType op left right

    | UnaryOp (op, inner) ->
        match op with
        | Neg ->
            // Negation works on integer and float numeric types
            checkExpr inner env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
            |> Result.bind (fun (innerType, inner') ->
                match innerType with
                | TInt8 | TInt16 | TInt32 | TInt64 | TInt
                | TUInt8 | TUInt16 | TUInt32 | TUInt64
                | TFloat64 ->
                    match expectedType with
                    | Some expected when expected <> innerType ->
                        Error (TypeMismatch (expected, innerType, "result of negation"))
                    | _ -> Ok (innerType, UnaryOp (op, inner'))
                | other ->
                    Error (InvalidOperation ("-", [other])))

        | Not ->
            // Boolean not works on booleans and returns booleans
            checkExpr inner env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some TBool)
            |> Result.bind (fun (innerType, inner') ->
                if innerType <> TBool then
                    Error (TypeMismatch (TBool, innerType, "operand of !"))
                else
                    match expectedType with
                    | Some TBool | None -> Ok (TBool, UnaryOp (op, inner'))
                    | Some other -> Error (TypeMismatch (other, TBool, "result of !")))

        | BitNot ->
            // Bitwise NOT works on integer types and preserves the operand type
            let isIntegerType (typ: Type) =
                match typ with
                | TInt8 | TInt16 | TInt32 | TInt64 | TInt
                | TUInt8 | TUInt16 | TUInt32 | TUInt64 -> true
                | _ -> false

            checkExpr inner env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
            |> Result.bind (fun (innerType, inner') ->
                if not (isIntegerType innerType) then
                    Error (InvalidOperation ("~~~", [innerType]))
                else
                    match expectedType with
                    | Some expected when expected <> innerType ->
                        Error (TypeMismatch (expected, innerType, "result of ~~~"))
                    | _ -> Ok (innerType, UnaryOp (op, inner')))

    | RecursiveLet (recursion, value, body) ->
        let name = recursiveBindingName recursion
        let availability = recursiveBindingAvailability recursion |> Option.defaultValue SelfRecursiveMember
        let continuationExpectation =
            match value with
            | Lambda (parameters, _, _) ->
                tryFindFunctionValueExpectation name body
                |> Option.orElseWith (fun () ->
                    tryFindCallArguments name body
                    |> Option.bind (inferFunctionExpectationFromArguments (parameters |> NonEmptyList.length)))
            | _ -> None
        let provisionalType =
            match value with
            | Lambda (parameters, returnAnnotation, _) ->
                let expectedParameters, expectedReturn =
                    match continuationExpectation with
                    | Some (TFunction (parameterTypes, returnType)) -> (parameterTypes, Some returnType)
                    | _ -> ([], None)
                let parameterTypes =
                    parameters
                    |> NonEmptyList.toList
                    |> List.mapi (fun index parameter ->
                        parameter.InferredType
                        |> Option.orElse parameter.SourceAnnotation
                        |> Option.orElseWith (fun () -> List.tryItem index expectedParameters)
                        |> Option.defaultValue (TVar $"recursiveParameter{index}"))
                TFunction (
                    parameterTypes,
                    returnAnnotation
                    |> Option.orElse expectedReturn
                    |> Option.defaultValue (TVar "recursiveReturn")
                )
            | _ ->
                Crash.crash "RecursiveLet must contain a lambda value"
        let valueEnvironment =
            match availability with
            | SelfRecursiveMember | MutualRecursiveMember -> Map.add name provisionalType env
            | OrdinaryBinding | CompletedGroupMember | ImportedGroupMember -> env
        checkExpr value valueEnvironment typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some provisionalType)
        |> Result.bind (fun (valueType, value') ->
            let valueType =
                match valueType, value' with
                | TFunction (_, returnType), Lambda (parameters, _, _) ->
                    let inferredParameters =
                        parameters
                        |> NonEmptyList.toList
                        |> List.map (fun parameter ->
                            parameter.InferredType
                            |> Option.orElse parameter.SourceAnnotation
                            |> Option.defaultValue (TVar "underdeterminedRecursiveParameter"))
                    TFunction (inferredParameters, returnType)
                | _ -> valueType
            let typedRecursion =
                match recursion with
                | ResolvedRecursiveBinding resolved ->
                    TypedRecursiveBinding { Resolved = resolved; MonomorphicType = valueType }
                | TypedRecursiveBinding typed ->
                    TypedRecursiveBinding { typed with MonomorphicType = valueType }
                | RecursiveBindingCandidate _ | ParsedRecursiveBinding _ ->
                    Crash.crash "Recursive binding reached type checking before name resolution"
            let bodyEnvironment = Map.add name valueType env
            checkExpr body bodyEnvironment typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg expectedType
            |> Result.map (fun (bodyType, body') ->
                (bodyType, RecursiveLet (typedRecursion, value', body'))))

    | Let (pattern, value, body) ->
        // Checking a long sequence through recursive Result.bind calls retains
        // one host stack frame per binding. Large generated programs combine
        // that depth with their top-level functions, so walk consecutive lets
        // iteratively and rebuild their typed form after checking the tail.
        let rebuildLets checkedLets typedBody =
            checkedLets
            |> List.fold (fun currentBody (checkedPattern, checkedValue) ->
                Let (checkedPattern, checkedValue, currentBody)) typedBody

        let rec checkLetChain currentEnv checkedLets currentPattern currentValue currentBody =
            // The RHS is checked in the incoming environment. Only a completely
            // validated and type-compatible pattern extends the continuation.
            let valueExpectedType =
                match currentPattern, currentValue with
                | LPVariable name, Lambda (parameters, _, _) ->
                    tryFindFunctionValueExpectation name currentBody
                    |> Option.orElseWith (fun () ->
                        tryFindCallArguments name currentBody
                        |> Option.bind (inferFunctionExpectationFromArguments (parameters |> NonEmptyList.toList |> List.length)))
                | _, ListLiteral [] -> Some (TList (TVar emptyListElementVar))
                | _ -> None

            match
                checkExpr
                    currentValue
                    currentEnv
                    typeReg
                    variantLookup
                    genericFuncReg
                    warningSettings
                    moduleRegistry
                    aliasReg
                    valueExpectedType
            with
            | Error error -> Error error
            | Ok (valueType, value') ->
                let valueType = canonicalizeBareSumTypeRefsWithNames sumTypeNames valueType
                match validateBinders (LetBinderPatterns [currentPattern]) with
                | Error message -> Error (GenericError message)
                | Ok _ ->
                    match bindLetPatternTypes currentPattern valueType with
                    | None ->
                        let renderedValue =
                            tryFormatLiteralValue value'
                            |> Option.defaultValue $"<{typeToString valueType}>"
                        let message =
                            $"Could not deconstruct value {renderedValue} into pattern {formatLetDeconstructionPattern currentPattern}"
                        let runtimeError = Let (currentPattern, value', RuntimeError message)
                        Ok (TRuntimeError, rebuildLets checkedLets runtimeError)
                    | Some bindings ->
                        let nextEnv =
                            bindings
                            |> List.fold (fun current (name, typ) -> Map.add name typ current) currentEnv
                        let bodyForChecking =
                            match currentPattern, value' with
                            | LPVariable name, (FloatLiteral _ | Int64Literal _) ->
                                substituteInterpolationLiteral name value' currentBody
                            | _ -> currentBody
                        let nextCheckedLets = (currentPattern, value') :: checkedLets
                        match bodyForChecking with
                        | Let (nextPattern, nextValue, nextBody) ->
                            checkLetChain nextEnv nextCheckedLets nextPattern nextValue nextBody
                        | _ ->
                            checkExpr
                                bodyForChecking
                                nextEnv
                                typeReg
                                variantLookup
                                genericFuncReg
                                warningSettings
                                moduleRegistry
                                aliasReg
                                expectedType
                            |> Result.map (fun (bodyType, body') ->
                                (bodyType, rebuildLets nextCheckedLets body'))

        checkLetChain env [] pattern value body

    | Var name ->
        if isBuiltinTestNanName name then
            let builtinExpr = Var "Builtin.testNan"
            match expectedType with
            | Some expected ->
                match reconcileTypes (Some aliasReg) expected TFloat64 with
                | Some reconciledType -> Ok (reconciledType, builtinExpr)
                | None -> Error (TypeMismatch (expected, TFloat64, $"variable {name}"))
            | None -> Ok (TFloat64, builtinExpr)
        else if isBuiltinTestInfinityName name then
            let builtinExpr = Var "Builtin.testInfinity"
            match expectedType with
            | Some expected ->
                match reconcileTypes (Some aliasReg) expected TFloat64 with
                | Some reconciledType -> Ok (reconciledType, builtinExpr)
                | None -> Error (TypeMismatch (expected, TFloat64, $"variable {name}"))
            | None -> Ok (TFloat64, builtinExpr)
        else if isBuiltinBlobEmptyName name then
            match expectedType with
            | Some expected ->
                match reconcileTypes (Some aliasReg) expected TBlob with
                | Some reconciledType -> Ok (reconciledType, Var "Builtin.blobEmpty")
                | None -> Error (TypeMismatch (expected, TBlob, $"variable {name}"))
            | None -> Ok (TBlob, Var "Builtin.blobEmpty")
        else
            // Variable reference: look up in environment
            match tryLookupResolved name env with
            | Some (varType, resolvedName) ->
                let varType = canonicalizeBareSumTypeRefsWithNames sumTypeNames varType

                match expectedType with
                | Some expected ->
                    match reconcileTypes (Some aliasReg) expected varType with
                    | Some reconciledType -> Ok (reconciledType, Var resolvedName)
                    | None -> Error (TypeMismatch (expected, varType, $"variable {name}"))
                | None -> Ok (varType, Var resolvedName)
            | None ->
                // Check if it's a module function (e.g., Stdlib.Int64.add)
                match Stdlib.tryGetFunction moduleRegistry name with
                | Some (moduleFunc, resolvedName) ->
                    let funcType = Stdlib.getFunctionType moduleFunc
                    match expectedType with
                    | Some expected ->
                        match reconcileTypes (Some aliasReg) expected funcType with
                        | Some reconciledType -> Ok (reconciledType, Var resolvedName)
                        | None -> Error (TypeMismatch (expected, funcType, $"variable {name}"))
                    | None -> Ok (funcType, Var resolvedName)
                | None -> Error (UndefinedVariable name)

    | If (cond, thenBranch, elseBranch) ->
        // If expression: condition must be bool, branches must have same type
        checkExpr cond env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
        |> Result.bind (fun (condType, cond') ->
            let normalizedConditionResult : Result<Expr, TypeError> =
                if condType = TBool then
                    Ok cond'
                else
                    let conditionIsKnownFailure =
                        isKnownUnwrapFailureExpr Map.empty cond
                        || isKnownUnwrapFailureExpr Map.empty cond'
                        || isKnownTestRuntimeErrorExpr Map.empty cond
                        || isKnownTestRuntimeErrorExpr Map.empty cond'

                    if conditionIsKnownFailure then
                        checkExpr cond env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some TBool)
                        |> Result.bind (fun (resolvedCondType, resolvedCondExpr) ->
                            if resolvedCondType = TBool then
                                Ok resolvedCondExpr
                            else
                                Error (GenericError (ifConditionTypeMismatchMessage resolvedCondExpr resolvedCondType)))
                    else
                        Error (GenericError (ifConditionTypeMismatchMessage cond' condType))

            normalizedConditionResult
            |> Result.bind (fun normalizedCond ->
                checkExpr thenBranch env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg expectedType
                |> Result.bind (fun (thenType, then') ->
                    let elseExpectedType =
                        // If an outer context already provides an expected type, keep using it.
                        // Otherwise, use the then-branch type to type-check the else-branch.
                        // This lets bottom-like runtime-failing expressions (e.g. unwrap None)
                        // inhabit the enclosing branch type.
                        match expectedType with
                        | Some outerExpected -> Some outerExpected
                        | None -> Some thenType

                    let elseResult =
                        match checkExpr elseBranch env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg elseExpectedType with
                        | Ok checkedElse ->
                            Ok checkedElse
                        | Error originalErr ->
                            // When no outer expected type exists, we type-check else with then-type context.
                            // If that fails due to contextual mismatch, re-check else unconstrained so the
                            // final diagnostic can report branch-vs-branch mismatch, not literal mismatch.
                            match expectedType, originalErr with
                            | None, TypeMismatch (expectedElse, _, _) when expectedElse = thenType ->
                                checkExpr elseBranch env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                            | _ ->
                                Error originalErr

                    elseResult
                    |> Result.bind (fun (elseType, else') ->
                        let reconciledBranchType =
                            match reconcileTypes (Some aliasReg) thenType elseType with
                            | Some reconciledType ->
                                Some reconciledType
                            | None ->
                                let thenIsKnownFailure =
                                    isKnownUnwrapFailureExpr Map.empty thenBranch
                                    || isKnownUnwrapFailureExpr Map.empty then'

                                let elseIsKnownFailure =
                                    isKnownUnwrapFailureExpr Map.empty elseBranch
                                    || isKnownUnwrapFailureExpr Map.empty else'

                                if thenIsKnownFailure && not elseIsKnownFailure then
                                    Some elseType
                                elif elseIsKnownFailure && not thenIsKnownFailure then
                                    Some thenType
                                else
                                    None

                        match reconciledBranchType with
                        | None ->
                            Error (IfBranchTypeMismatch (thenType, elseType))
                        | Some reconciledType ->
                            match expectedType with
                            | Some expected ->
                                match reconcileTypes (Some aliasReg) expected reconciledType with
                                | Some reconciledExpected -> Ok (reconciledExpected, If (normalizedCond, then', else'))
                                | None -> Error (TypeMismatch (expected, reconciledType, "if expression"))
                            | _ -> Ok (reconciledType, If (normalizedCond, then', else'))))))

    | Sequence (first, next) ->
        // The interpreter checks this at the statement boundary. The compiler
        // enforces the same Unit contract statically and uses only the final
        // expression to determine the sequence's result type.
        checkExpr first env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some TUnit)
        |> Result.bind (fun (_, first') ->
            checkExpr next env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg expectedType
            |> Result.map (fun (nextType, next') ->
                (nextType, Sequence (first', next'))))

    | Call (funcName, args) ->
        CheckCalls.check checkExpr funcParamNameReg indexedSumTypeReg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg expectedType funcName args

    | TypeApp (funcName, [targetType], { Head = leftExpr; Tail = [rightExpr] })
        when funcName = internalTypeAppMarkerName EqHelperDispatch ->
        // Specialized programs can be checked again by the E2E preamble
        // planner. Keep this compiler-internal plan well typed without
        // exposing its marker as a source-level function.
        checkExpr leftExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some targetType)
        |> Result.bind (fun (_, leftExpr') ->
            checkExpr rightExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some targetType)
            |> Result.bind (fun (_, rightExpr') ->
                match expectedType with
                | Some expected when not (typesCompatible expected TBool) ->
                    Error (TypeMismatch (expected, TBool, "comparison result"))
                | _ ->
                    Ok (
                        TBool,
                        makeInternalTypeApp
                            (EqHelperDispatchTypeApp (targetType, leftExpr', rightExpr'))
                    )))

    | TypeApp (funcName, typeArgs, args) ->
        // Generic function call with explicit type arguments: func<Type1, Type2>(args)
        // 1. Look up the canonical function identity
        let args = NonEmptyList.toList args
        match tryLookupResolved funcName env with
        | Some (TFunction (paramTypes, returnType), resolvedFuncName) ->
            // 2. Look up type parameters
            match tryLookupResolved resolvedFuncName genericFuncReg.Functions with
            | Some (typeParams, _) ->
                let expectedTypeArgCount = List.length typeParams
                let actualTypeArgCount = List.length typeArgs
                if expectedTypeArgCount <> actualTypeArgCount then
                    Error (
                        GenericError (
                            formatTypeArgumentArityError funcName expectedTypeArgCount actualTypeArgCount
                        )
                    )
                else
                // 3. Build substitution from type params to type args
                    let typeArgs =
                        typeArgs
                        |> List.map (canonicalizeBareSumTypeRefsWithNames sumTypeNames)

                    validateCanonicalSortableCall aliasReg typeReg indexedSumTypeReg resolvedFuncName typeArgs
                    |> Result.bind (fun () ->
                        match resolvedFuncName, typeArgs with
                        | ("Stdlib.Json.serialize" | "Stdlib.Json.parse"), [targetType] ->
                            validateJsonTargetType
                                aliasReg
                                typeReg
                                variantLookup
                                indexedSumTypeReg
                                targetType
                        | _ -> Ok ())
                    |> Result.bind (fun () -> buildSubstitution typeParams typeArgs |> Result.mapError GenericError)
                    |> Result.bind (fun subst ->
                        // 4. Apply substitution to get concrete types
                        let concreteParamTypes =
                            paramTypes
                            |> List.map (applySubst subst)
                            |> List.map (canonicalizeBareSumTypeRefsWithNames sumTypeNames)
                        let concreteReturnType =
                            returnType
                            |> applySubst subst
                            |> canonicalizeBareSumTypeRefsWithNames sumTypeNames

                        // 5. Check argument count - allow partial application
                        let numParams = List.length concreteParamTypes
                        let args = normalizeNullaryCallArgs numParams args
                        let numArgs = List.length args
                        if numArgs > numParams then
                            Error (GenericError (formatValueArgumentArityError funcName numParams numArgs))
                        else if numArgs < numParams then
                            // Partial application with explicit type args
                            let providedParamTypes = List.take numArgs concreteParamTypes
                            let remainingParamTypes = List.skip numArgs concreteParamTypes

                            // Type-check the provided arguments
                            let rec checkProvidedArgs remaining paramTys paramIndex accArgs =
                                match remaining, paramTys with
                                | [], [] -> Ok (List.rev accArgs)
                                | arg :: restArgs, paramT :: restParams ->
                                    let paramName =
                                        paramNameForLegacyError funcParamNameReg resolvedFuncName paramIndex

                                    checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                                    |> Result.mapError (fun err ->
                                        match err with
                                        | TypeMismatch (_, actualType, _) when not (isRuntimeErrorType actualType) ->
                                            GenericError
                                                (formatLegacyParamTypeError
                                                    funcName
                                                    paramIndex
                                                    paramName
                                                    paramT
                                                    actualType
                                                    arg)
                                        | _ ->
                                            err)
                                    |> Result.bind (fun (argType, arg') ->
                                        // Use typesCompatible to allow type variables to unify with concrete types
                                        if typesCompatibleWithAliases aliasReg paramT argType then
                                            checkProvidedArgs restArgs restParams (paramIndex + 1) (arg' :: accArgs)
                                        else
                                            Error (
                                                GenericError
                                                    (formatLegacyParamTypeError
                                                        funcName
                                                        paramIndex
                                                        paramName
                                                        paramT
                                                        argType
                                                        arg)
                                            ))
                                | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                            checkProvidedArgs args providedParamTypes 1 []
                            |> Result.bind (fun args' ->
                                // Create unique parameter names for the remaining parameters
                                let remainingParams = makePartialParams resolvedFuncName remainingParamTypes

                                // Create the lambda body: TypeApp call with all args (using resolved name)
                                let allArgs = args' @ (remainingParams |> List.map (fun (name, _) -> Var name))
                                let lambdaBody = TypeApp (resolvedFuncName, typeArgs, toCallArgs allArgs)

                                // Create the lambda
                                let lambdaExpr = Lambda (toLambdaParams remainingParams, None, lambdaBody)

                                // The resulting type is a function from remaining params to return type
                                let partialType = TFunction (remainingParamTypes, concreteReturnType)

                                match expectedType with
                                | Some expected when not (typesCompatible expected partialType) ->
                                    Error (TypeMismatch (expected, partialType, $"partial application of {funcName}"))
                                | _ -> Ok (partialType, lambdaExpr))
                        else
                            // 6. Type check each argument and collect transformed args
                            let rec checkArgsWithTypes remaining paramTys paramIndex accArgs =
                                match remaining, paramTys with
                                | [], [] -> Ok (List.rev accArgs)
                                | arg :: restArgs, paramT :: restParams ->
                                    let paramName =
                                        paramNameForLegacyError funcParamNameReg resolvedFuncName paramIndex

                                    checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                                    |> Result.mapError (fun err ->
                                        match err with
                                        | TypeMismatch (_, actualType, _) when not (isRuntimeErrorType actualType) ->
                                            GenericError
                                                (formatLegacyParamTypeError
                                                    funcName
                                                    paramIndex
                                                    paramName
                                                    paramT
                                                    actualType
                                                    arg)
                                        | _ ->
                                            err)
                                    |> Result.bind (fun (argType, arg') ->
                                        // Use typesCompatible to allow type variables to unify with concrete types
                                        if typesCompatibleWithAliases aliasReg paramT argType then
                                            checkArgsWithTypes restArgs restParams (paramIndex + 1) (arg' :: accArgs)
                                        else
                                            Error (
                                                GenericError
                                                    (formatLegacyParamTypeError
                                                        funcName
                                                        paramIndex
                                                        paramName
                                                        paramT
                                                        argType
                                                        arg)
                                            ))
                                | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                            checkArgsWithTypes args concreteParamTypes 1 []
                            |> Result.bind (fun args' ->
                                // 7. Return the concrete return type (using resolved name)
                                // Use typesCompatible to allow type variables to unify with concrete types
                                match expectedType with
                                | Some expected when not (typesCompatibleWithAliases aliasReg expected concreteReturnType) ->
                                    Error (TypeMismatch (expected, concreteReturnType, $"result of call to {funcName}"))
                                | _ ->
                                    Ok (
                                        concreteReturnType,
                                        TypeApp (resolvedFuncName, typeArgs, toCallArgs args')
                                    )))
            | None ->
                Error (GenericError $"Function {funcName} is not generic, use regular call syntax")
        | Some (other, _) ->
            Error (GenericError $"{funcName} is not a function (has type {typeToString other})")
        | None ->
            // Check if it's a generic module function (e.g., __raw_get<v>)
            match Stdlib.tryGetFunction moduleRegistry funcName with
            | Some (moduleFunc, resolvedFuncName) when not (List.isEmpty moduleFunc.TypeParams) ->
                let typeParams = moduleFunc.TypeParams
                let paramTypes = moduleFunc.ParamTypes
                let returnType = moduleFunc.ReturnType
                let expectedTypeArgCount = List.length typeParams
                let actualTypeArgCount = List.length typeArgs
                if expectedTypeArgCount <> actualTypeArgCount then
                    Error (
                        GenericError (
                            formatTypeArgumentArityError funcName expectedTypeArgCount actualTypeArgCount
                        )
                    )
                else
                // Build substitution from type params to type args
                    let typeArgs =
                        typeArgs
                        |> List.map (canonicalizeBareSumTypeRefsWithNames sumTypeNames)

                    validateCanonicalSortableCall aliasReg typeReg indexedSumTypeReg resolvedFuncName typeArgs
                    |> Result.bind (fun () -> buildSubstitution typeParams typeArgs |> Result.mapError GenericError)
                    |> Result.bind (fun subst ->
                        // Apply substitution to get concrete types
                        let concreteParamTypes =
                            paramTypes
                            |> List.map (applySubst subst)
                            |> List.map (canonicalizeBareSumTypeRefsWithNames sumTypeNames)
                        let concreteReturnType =
                            returnType
                            |> applySubst subst
                            |> canonicalizeBareSumTypeRefsWithNames sumTypeNames

                        // Check argument count - allow partial application
                        let numParams = List.length concreteParamTypes
                        let args = normalizeNullaryCallArgs numParams args
                        let numArgs = List.length args
                        if numArgs > numParams then
                            Error (GenericError (formatValueArgumentArityError funcName numParams numArgs))
                        else if numArgs < numParams then
                            // Partial application with explicit type args
                            let providedParamTypes = List.take numArgs concreteParamTypes
                            let remainingParamTypes = List.skip numArgs concreteParamTypes

                            // Type-check the provided arguments
                            let rec checkProvidedArgs remaining paramTys paramIndex accArgs =
                                match remaining, paramTys with
                                | [], [] -> Ok (List.rev accArgs)
                                | arg :: restArgs, paramT :: restParams ->
                                    let paramName =
                                        paramNameForLegacyError funcParamNameReg resolvedFuncName paramIndex

                                    checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                                    |> Result.mapError (fun err ->
                                        match err with
                                        | TypeMismatch (_, actualType, _) when not (isRuntimeErrorType actualType) ->
                                            GenericError
                                                (formatLegacyParamTypeError
                                                    funcName
                                                    paramIndex
                                                    paramName
                                                    paramT
                                                    actualType
                                                    arg)
                                        | _ ->
                                            err)
                                    |> Result.bind (fun (argType, arg') ->
                                        // Use typesCompatible to allow type variables to unify with concrete types
                                        if typesCompatible paramT argType then
                                            checkProvidedArgs restArgs restParams (paramIndex + 1) (arg' :: accArgs)
                                        else
                                            Error (
                                                GenericError
                                                    (formatLegacyParamTypeError
                                                        funcName
                                                        paramIndex
                                                        paramName
                                                        paramT
                                                        argType
                                                        arg)
                                            ))
                                | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                            checkProvidedArgs args providedParamTypes 1 []
                            |> Result.bind (fun args' ->
                                // Create unique parameter names for the remaining parameters
                                let remainingParams = makePartialParams resolvedFuncName remainingParamTypes

                                // Create the lambda body: TypeApp call with all args (using resolved name)
                                let allArgs = args' @ (remainingParams |> List.map (fun (name, _) -> Var name))
                                let lambdaBody = TypeApp (resolvedFuncName, typeArgs, toCallArgs allArgs)

                                // Create the lambda
                                let lambdaExpr = Lambda (toLambdaParams remainingParams, None, lambdaBody)

                                // The resulting type is a function from remaining params to return type
                                let partialType = TFunction (remainingParamTypes, concreteReturnType)

                                match expectedType with
                                | Some expected when not (typesCompatible expected partialType) ->
                                    Error (TypeMismatch (expected, partialType, $"partial application of {funcName}"))
                                | _ -> Ok (partialType, lambdaExpr))
                        else
                            // Type check each argument and collect transformed args
                            let rec checkArgsWithTypes remaining paramTys paramIndex accArgs =
                                match remaining, paramTys with
                                | [], [] -> Ok (List.rev accArgs)
                                | arg :: restArgs, paramT :: restParams ->
                                    let paramName =
                                        paramNameForLegacyError funcParamNameReg resolvedFuncName paramIndex

                                    checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                                    |> Result.mapError (fun err ->
                                        match err with
                                        | TypeMismatch (_, actualType, _) when not (isRuntimeErrorType actualType) ->
                                            GenericError
                                                (formatLegacyParamTypeError
                                                    funcName
                                                    paramIndex
                                                    paramName
                                                    paramT
                                                    actualType
                                                    arg)
                                        | _ ->
                                            err)
                                    |> Result.bind (fun (argType, arg') ->
                                        // Use typesCompatible to allow type variables to unify with concrete types
                                        if typesCompatible paramT argType then
                                            checkArgsWithTypes restArgs restParams (paramIndex + 1) (arg' :: accArgs)
                                        else
                                            Error (
                                                GenericError
                                                    (formatLegacyParamTypeError
                                                        funcName
                                                        paramIndex
                                                        paramName
                                                        paramT
                                                        argType
                                                        arg)
                                            ))
                                | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                            checkArgsWithTypes args concreteParamTypes 1 []
                            |> Result.bind (fun args' ->
                                // Use typesCompatible to allow type variables to unify with concrete types
                                match expectedType with
                                | Some expected when not (typesCompatible expected concreteReturnType) ->
                                    Error (TypeMismatch (expected, concreteReturnType, $"result of call to {funcName}"))
                                | _ ->
                                    Ok (
                                        concreteReturnType,
                                        TypeApp (resolvedFuncName, typeArgs, toCallArgs args')
                                    )))
            | Some (_, _) ->
                Error (GenericError $"Function {funcName} is not generic, use regular call syntax")
            | None ->
                Error (UndefinedCallTarget funcName)

    | TupleLiteral elements ->
        // Type-check each element and build tuple type
        let expectedElemTypes =
            match expectedType with
            | Some expected ->
                match resolveType aliasReg expected with
                | TTuple elemTypes when List.length elemTypes = List.length elements ->
                    elemTypes |> List.map Some
                | _ -> List.replicate (List.length elements) None
            | None -> List.replicate (List.length elements) None

        let rec checkElements elems expectedElems accTypes accExprs =
            match elems, expectedElems with
            | [], [] -> Ok (List.rev accTypes, List.rev accExprs)
            | e :: rest, expectedElem :: expectedRest ->
                checkExpr e env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg expectedElem
                |> Result.bind (fun (elemType, e') ->
                    checkElements rest expectedRest (elemType :: accTypes) (e' :: accExprs))
            | _ -> Ok (List.rev accTypes, List.rev accExprs)
        checkElements elements expectedElemTypes [] []
        |> Result.bind (fun (elemTypes, elements') ->
            // Tuple elements that are known runtime failures should make the whole
            // tuple expression runtime-fail (bottom-like behavior), preserving the
            // left-to-right first failure.
            let firstRuntimeErrorElem =
                elements'
                |> List.tryFind (isKnownTestRuntimeErrorExpr Map.empty)

            match firstRuntimeErrorElem with
            | Some runtimeErrExpr ->
                let outputType =
                    match expectedType with
                    | Some expected -> expected
                    | None -> TRuntimeError

                let runtimeErrCall =
                    match runtimeErrExpr with
                    | Call (funcName, { Head = argExpr; Tail = [] }) when isBuiltinTestRuntimeErrorName funcName ->
                        Call ("Builtin.testRuntimeError", NonEmptyList.singleton argExpr)
                    | _ ->
                        match tryExtractKnownTestRuntimeErrorMessage Map.empty runtimeErrExpr with
                        | Some msg -> Call ("Builtin.testRuntimeError", NonEmptyList.singleton (StringLiteral msg))
                        | None ->
                            Call (
                                "Builtin.testRuntimeError",
                                NonEmptyList.singleton (StringLiteral "<runtime error>")
                            )

                Ok (outputType, runtimeErrCall)
            | None ->
                let tupleType = TTuple elemTypes
                match expectedType with
                | Some expected ->
                    // Resolve type aliases first, then check compatibility for type variables
                    // This allows Pair<Int64> to match (Int64, Int64) when Pair<a> = (a, a)
                    // and (a, b) to match (Int64, Int64) when using generic functions
                    let resolvedExpected = resolveType aliasReg expected
                    if typesCompatible resolvedExpected tupleType then
                        Ok (tupleType, TupleLiteral elements')
                    else
                        Error (TypeMismatch (expected, tupleType, "tuple literal"))
                | None -> Ok (tupleType, TupleLiteral elements'))

    | TupleAccess (tupleExpr, index) ->
        // Check the tuple expression
        checkExpr tupleExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
        |> Result.bind (fun (tupleType, tupleExpr') ->
            match tupleType with
            | TTuple elemTypes ->
                if index < 0 || index >= List.length elemTypes then
                    Error (GenericError $"Tuple index {index} out of bounds (tuple has {List.length elemTypes} elements)")
                else
                    let elemType = List.item index elemTypes
                    match expectedType with
                    | Some expected when expected <> elemType ->
                        Error (TypeMismatch (expected, elemType, $"tuple access .{index}"))
                    | _ -> Ok (elemType, TupleAccess (tupleExpr', index))
            | other ->
                Error (GenericError $"Cannot access .{index} on non-tuple type {typeToString other}"))

    | RecordLiteral (reference, fields) ->
        CheckRecordLiterals.check checkExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg expectedType reference fields

    | RecordUpdate (recordExpr, updates) ->
        // Check the record expression to get its type
        checkExpr recordExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
        |> Result.bind (fun (recordType, recordExpr') ->
            match resolveAliasTargetType aliasReg recordType with
            | TRecord (typeName, typeArgs) ->
                match Map.tryFind typeName typeReg with
                | None ->
                    Error (GenericError $"Unknown record type: {typeName}")
                | Some recordInfo ->
                    let normalizedUpdates =
                        updates
                        |> List.map (fun (name, value) ->
                            (if name = "___" then "" else name), value)

                    match normalizedUpdates |> List.tryFind (fst >> (=) "") with
                    | Some _ -> Error (GenericError "Empty key in record update")
                    | None ->
                    match buildRecordFieldSubstitutionFromParams recordInfo.TypeParams typeArgs with
                    | Error message -> Error (GenericError message)
                    | Ok subst ->
                        // Update expressions remain in source order. Duplicate names
                        // are intentionally accepted and the lowering uses the last value.
                        let unknownFields =
                            normalizedUpdates
                            |> List.filter (fun (fname, _) -> not (Map.containsKey fname recordInfo.FieldTypes))
                            |> List.map fst

                        if not (List.isEmpty unknownFields) then
                            let unknownStr = String.concat ", " unknownFields
                            Error (GenericError $"Unknown fields in record update: {unknownStr}")
                        else
                            let rec checkUpdates remaining accUpdates =
                                match remaining with
                                | [] -> Ok (List.rev accUpdates)
                                | (fname, updateExpr) :: rest ->
                                    match Map.tryFind fname recordInfo.FieldTypes with
                                    | Some fieldTypePattern ->
                                        let expectedFieldType = applyTypeArguments subst fieldTypePattern
                                        checkExpr updateExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some expectedFieldType)
                                        |> Result.bind (fun (actualType, updateExpr') ->
                                            if typesCompatibleWithAliases aliasReg expectedFieldType actualType then
                                                checkUpdates rest ((fname, updateExpr') :: accUpdates)
                                            else
                                                Error (TypeMismatch (expectedFieldType, actualType, $"field {fname} in record update")))
                                    | None ->
                                        Crash.crash $"Validated record update field '{fname}' disappeared"

                            checkUpdates normalizedUpdates []
                            |> Result.map (fun updates' -> (TRecord (typeName, typeArgs), RecordUpdate (recordExpr', updates')))
            | other ->
                Error (GenericError $"Cannot use record update syntax on non-record type {typeToString other}"))

    | RecordAccess (recordExpr, fieldName) ->
        let fieldName = if fieldName = "___" then "" else fieldName
        if fieldName = "" then
            Error (GenericError "Field name is empty")
        else
        // Check the record expression
        checkExpr recordExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
        |> Result.bind (fun (recordType, recordExpr') ->
            match resolveAliasTargetType aliasReg recordType with
            | TRecord (typeName, typeArgs) ->
                match Map.tryFind typeName typeReg with
                | None ->
                    Error (GenericError $"Unknown record type: {typeName}")
                | Some recordInfo ->
                    match Map.tryFind fieldName recordInfo.FieldTypes with
                    | None ->
                        Error (GenericError $"Tried to access field '{fieldName}' but record type {typeName} has no such field")
                    | Some fieldTypePattern ->
                        match buildRecordFieldSubstitutionFromParams recordInfo.TypeParams typeArgs with
                        | Error msg ->
                            Error (GenericError msg)
                        | Ok subst ->
                            let fieldType = applyTypeArguments subst fieldTypePattern
                            match expectedType with
                            | Some expected when not (typesCompatibleWithAliases aliasReg expected fieldType) ->
                                Error (TypeMismatch (expected, fieldType, $"field access .{fieldName}"))
                            | _ -> Ok (fieldType, RecordAccess (recordExpr', fieldName))
            | other ->
                Error (GenericError $"Attempting to access field '{fieldName}' on non-record type {typeToString other}"))

    | Constructor (constructorReference, variantName, payload) ->
        // Look up the variant to find its type and expected payload
        let expectedVariant =
            match constructorReference, expectedType with
            | UnresolvedConstructor None, Some (TSum (typeName, _)) ->
                tryFindVariant
                    (resolvedConstructorReference (resolveTypeName aliasReg typeName))
                    variantName
                    variantLookup
            | _ -> None
        if constructorReference = UnresolvedConstructor None
           && unqualifiedVariantOwnerCount variantName variantLookup > 1
           && Option.isNone expectedVariant then
            let identities =
                variantLookup
                |> Map.toList
                |> List.choose (fun (lookupName, (typeName, _, _, _)) ->
                    if lookupName = $"{typeName}.{variantName}" then
                        Some (NameResolution.ConstructorSymbol (typeName, variantName))
                    else None)
                |> List.distinct
                |> List.sortBy NameResolution.symbolIdentityToString
            match NameResolution.tryQualifiedName variantName with
            | Some originalName ->
                Error (
                    ResolutionFailure (
                        NameResolution.AmbiguousReference (
                            originalName,
                            NameResolution.ResolutionContext.Constructor,
                            identities)))
            | None -> Error (GenericError $"Ambiguous constructor: {variantName}")
        else
        let resolvedVariant =
            match expectedVariant with
            | Some found -> Some found
            | None ->
                match tryFindVariant constructorReference variantName variantLookup with
                | Some found -> Some found
                | None when not genericFuncReg.RequireExplicitTypeArgsForBareCalls ->
                    Map.tryFind variantName variantLookup
                | None -> None
        match resolvedVariant with
        | None ->
            Error (GenericError $"Unknown constructor: {variantName}")
        | Some (typeName, typeParams, _tag, expectedPayload) ->
            let resolvedReference = resolvedConstructorReference typeName
            let resolvedExpr payload = Constructor (resolvedReference, variantName, payload)
            let payloadArityError =
                match expectedPayload, payload with
                | Some (TEnumFields expectedFields), Some (TupleLiteral actualFields)
                    when List.length expectedFields <> List.length actualFields ->
                    Some (
                        GenericError
                            $"Expected {List.length expectedFields} fields in {typeName}.`{variantName}`, but got {List.length actualFields}"
                    )
                | Some (TEnumFields expectedFields), Some actualPayload
                    when (match actualPayload with | TupleLiteral _ -> false | _ -> true)
                         && List.length expectedFields <> 1 ->
                    Some (
                        GenericError
                            $"Expected {List.length expectedFields} fields in {typeName}.`{variantName}`, but got 1"
                    )
                | _ -> None

            let normalizedExpectedPayload =
                expectedPayload
                |> Option.map (function
                    | TEnumFields fieldTypes -> TTuple fieldTypes
                    | payloadType -> payloadType)

            (match payloadArityError with
             | Some error -> Error error
             | None -> Ok (normalizedExpectedPayload, payload))
            |> Result.bind (fun (expectedPayload, payload) ->
            match expectedPayload, payload with
            | None, None ->
                // Variant without payload, no payload provided - OK
                if List.isEmpty typeParams then
                    // Non-generic type - simple case
                    let sumType = TSum (typeName, [])
                    match expectedType with
                    | Some expected when expected <> sumType ->
                        Error (TypeMismatch (expected, sumType, $"constructor {variantName}"))
                    | _ -> Ok (sumType, resolvedExpr None)
                else
                    // Generic type with nullary constructor (e.g., None in Option<t>)
                    // Try to get type arguments from expectedType
                    match expectedType with
                    | Some (TSum (expectedName, args)) when expectedName = typeName && List.length args = List.length typeParams ->
                        // Use type args from expected type
                        let sumType = TSum (typeName, args)
                        Ok (sumType, resolvedExpr None)
                    | Some expected ->
                        // Expected type doesn't match - error
                        let sumTypeWithVars = TSum (typeName, typeParams |> List.map TVar)
                        Error (TypeMismatch (expected, sumTypeWithVars, $"constructor {variantName}"))
                    | None ->
                        // No expected type - return type with unresolved type variables
                        // This allows type inference to resolve them later from context
                        let sumType = TSum (typeName, typeParams |> List.map TVar)
                        Ok (sumType, resolvedExpr None)
            | None, Some _ ->
                // Variant doesn't take payload but one was provided
                Error (GenericError $"Constructor {variantName} does not take a payload")
            | Some _, None ->
                // Variant requires payload but none provided
                Error (GenericError $"Constructor {variantName} requires a payload")
            | Some payloadType, Some payloadExpr ->
                // Variant with payload - check payload type
                // For generic types, infer type variables from the payload
                let payloadType = canonicalizeBareSumTypeRefsWithNames sumTypeNames payloadType

                if List.isEmpty typeParams then
                    // Non-generic type - check payload has exact type
                    checkExpr payloadExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some payloadType)
                    |> Result.bind (fun (actualPayloadType, payloadExpr') ->
                        let actualPayloadType =
                            canonicalizeBareSumTypeRefsWithNames sumTypeNames actualPayloadType

                        // Use typesCompatible to allow type variables to match concrete types
                        if not (typesCompatible payloadType actualPayloadType) then
                            Error (TypeMismatch (payloadType, actualPayloadType, $"payload of {variantName}"))
                        else
                            let sumType = TSum (typeName, [])
                            match expectedType with
                            | Some expected ->
                                // Use reconcileTypes to allow type variables to unify with concrete types
                                match reconcileTypes (Some aliasReg) expected sumType with
                                | None -> Error (TypeMismatch (expected, sumType, $"constructor {variantName}"))
                                | Some reconciledType -> Ok (reconciledType, resolvedExpr (Some payloadExpr'))
                            | None -> Ok (sumType, resolvedExpr (Some payloadExpr')))
                else
                    // Generic type - infer type variables from payload
                    // First, check the payload expression without expected type
                    checkExpr payloadExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                    |> Result.bind (fun (actualPayloadType, payloadExpr') ->
                        let actualPayloadType =
                            canonicalizeBareSumTypeRefsWithNames sumTypeNames actualPayloadType

                        // Try to unify payloadType (may contain TVar) with actualPayloadType
                        match unifyTypes payloadType actualPayloadType with
                        | Error msg ->
                            Error (GenericError $"Type mismatch in {variantName} payload: {msg}")
                        | Ok subst ->
                            // Apply substitution to verify all type vars are resolved
                            let concretePayloadType =
                                payloadType
                                |> applySubst subst
                                |> canonicalizeBareSumTypeRefsWithNames sumTypeNames

                            // Use typesCompatible to allow type variables to match concrete types
                            if not (typesCompatible concretePayloadType actualPayloadType) then
                                Error (TypeMismatch (concretePayloadType, actualPayloadType, $"payload of {variantName}"))
                            else
                                // Build concrete type arguments from substitution
                                // For unresolved type vars, try to get them from expectedType
                                let expectedArgs =
                                    match expectedType with
                                    | Some (TSum (expectedName, args)) when expectedName = typeName && List.length args = List.length typeParams ->
                                        Some args
                                    | _ -> None
                                let typeArgs = typeParams |> List.mapi (fun i p ->
                                    match Map.tryFind p subst with
                                    | Some t -> t
                                    | None ->
                                        // Try to get from expected type args
                                        match expectedArgs with
                                        | Some args -> List.item i args
                                        | None -> TVar p)
                                let sumType = TSum (typeName, typeArgs)
                                match expectedType with
                                | Some expected ->
                                    // Use reconcileTypes to allow type variables to unify with concrete types
                                    match reconcileTypes (Some aliasReg) expected sumType with
                                    | None -> Error (TypeMismatch (expected, sumType, $"constructor {variantName}"))
                                    | Some reconciledType -> Ok (reconciledType, resolvedExpr (Some payloadExpr'))
                                | None -> Ok (sumType, resolvedExpr (Some payloadExpr'))))

    | Match (scrutinee, cases) ->
        CheckMatches.check checkExpr sumTypeNames indexedSumTypeReg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg expectedType scrutinee cases

    | DictLiteral (_, entries) ->
        let duplicateKey =
            entries
            |> List.fold (fun (seen, duplicate) (key, _) ->
                match duplicate with
                | Some _ -> (seen, duplicate)
                | None when Set.contains key seen -> (seen, Some key)
                | None -> (Set.add key seen, None)) (Set.empty, None)
            |> snd

        match duplicateKey with
        | Some key ->
            Error (GenericError $"Cannot add two dictionary entries with the same key `{key}`")
        | None ->
            let expectedValueType =
                match expectedType with
                | Some (TDict (TString, valueType)) -> Some valueType
                | _ -> None

            let finish valueType checkedEntries =
                let dictType = TDict (TString, valueType)
                match expectedType with
                | Some expected ->
                    match reconcileTypes (Some aliasReg) expected dictType with
                    | Some reconciled -> Ok (reconciled, DictLiteral (valueType, checkedEntries))
                    | None -> Error (TypeMismatch (expected, dictType, "Dict literal"))
                | None -> Ok (dictType, DictLiteral (valueType, checkedEntries))

            match entries with
            | [] ->
                finish (Option.defaultValue (TVar "dictValue") expectedValueType) []
            | (firstKey, firstValue) :: rest ->
                checkExpr firstValue env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg expectedValueType
                |> Result.bind (fun (valueType, checkedFirst) ->
                    let rec checkRemaining remaining acc =
                        match remaining with
                        | [] -> Ok (List.rev acc)
                        | (key, value) :: tail ->
                            checkExpr value env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                            |> Result.bind (fun (actualType, checkedValue) ->
                                match reconcileTypes (Some aliasReg) valueType actualType with
                                | Some _ -> checkRemaining tail ((key, checkedValue) :: acc)
                                | None ->
                                    Error (GenericError $"dict values must have one type: key `{key}` has {typeToString actualType}, expected {typeToString valueType}"))
                    checkRemaining rest [(firstKey, checkedFirst)]
                    |> Result.bind (finish valueType))

    | ListLiteral elements ->
        // Type-check elements and infer element type from first element
        match elements with
        | [] ->
            // Empty list: use expected list type or keep a type variable
            match expectedType |> Option.map (resolveType aliasReg) with
            | Some (TList elemType) -> Ok (TList elemType, ListLiteral [])
            // A bare type variable (a generic parameter not yet bound, as the seed
            // of a fold) takes the list; the element stays open for the other
            // arguments to fix. `Stdlib.List.fold xs [] (fun acc x -> [x])` was a
            // mismatch reported as the enclosing function's return value.
            | Some (TVar _) | None -> Ok (TList (TVar emptyListElementVar), ListLiteral [])
            | Some other -> Error (TypeMismatch (other, TList (TVar emptyListElementVar), "empty list"))
        | first :: rest ->
            // Use expected list element type for the first element when available, so
            // lambda/list literals in expected contexts reconcile type variables consistently.
            let firstExpectedType =
                match expectedType |> Option.map (resolveType aliasReg) with
                // A bare type variable must be inferred from the element. Passing it into
                // expression checking would hide concrete requirements such as arithmetic.
                | Some (TList (TVar _)) -> None
                // Structured generic types still carry useful constraints. In particular,
                // checking (String, a) contextually preserves the precise error at a bad key.
                | Some (TList expectedElemType) -> Some expectedElemType
                | _ -> None
            checkExpr first env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg firstExpectedType
            |> Result.bind (fun (elemType, first') ->
                // Check remaining elements match the inferred type
                let rec checkRest remaining acc =
                    match remaining with
                    | [] -> Ok (List.rev acc)
                    | e :: rs ->
                        checkExpr e env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some elemType)
                        |> Result.bind (fun (eType, e') ->
                            if eType = elemType then checkRest rs (e' :: acc)
                            else Error (TypeMismatch (elemType, eType, "list element")))
                checkRest rest [first']
                |> Result.bind (fun elements' ->
                    let listType = TList elemType
                    match expectedType with
                    | Some expected ->
                        // Use reconcileTypes to allow type variables to unify and resolve type aliases
                        match reconcileTypes (Some aliasReg) expected listType with
                        | Some reconciledType -> Ok (reconciledType, ListLiteral elements')
                        | None -> Error (TypeMismatch (expected, listType, "list literal"))
                    | None -> Ok (listType, ListLiteral elements')))

    | Lambda (parameters, returnAnnotation, body) ->
        CheckLambdas.check checkExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg expectedType parameters returnAnnotation body

    | Apply (func, args) ->
        let argsList = NonEmptyList.toList args
        let functionExpectedType =
            match func with
            | Lambda (parameters, _, _) ->
                inferFunctionExpectationFromArguments
                    (parameters |> NonEmptyList.toList |> List.length)
                    argsList
            | _ -> None
        // Type-check the function expression
        checkExpr func env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg functionExpectedType
        |> Result.bind (fun (funcType, func') ->
            match funcType with
            | TFunction (paramTypes, returnType) ->
                let numParams = List.length paramTypes
                let argsList = normalizeNullaryCallArgs numParams argsList
                let numArgs = List.length argsList
                // Check argument count - allow partial application
                if numArgs > numParams then
                    Error (GenericError $"Expected {numParams} arguments, got {numArgs}")
                else if numArgs < numParams then
                    // Partial application of lambda/function value
                    let providedParamTypes = List.take numArgs paramTypes
                    let remainingParamTypes = List.skip numArgs paramTypes

                    // Type-check the provided arguments
                    let rec checkProvidedArgs (argExprs: Expr list) (paramTys: Type list) (checkedArgs: Expr list) : Result<Expr list, TypeError> =
                        match argExprs, paramTys with
                        | [], [] -> Ok (List.rev checkedArgs)
                        | arg :: restArgs, paramTy :: restParams ->
                            checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramTy)
                            |> Result.bind (fun (argType, arg') ->
                                if typesEqual aliasReg argType paramTy then
                                    checkProvidedArgs restArgs restParams (arg' :: checkedArgs)
                                else
                                    Error (TypeMismatch (paramTy, argType, "function argument")))
                        | _ -> Error (GenericError "Argument count mismatch")

                    checkProvidedArgs argsList providedParamTypes []
                    |> Result.bind (fun args' ->
                        // Create fresh parameter names for the remaining parameters
                        // Use "lambda" as identifier since we're applying a function value, not a named function
                        let remainingParams = makePartialParams "lambda" remainingParamTypes

                        // Create the lambda body: apply the original function with all args
                        let allArgs = args' @ (remainingParams |> List.map (fun (name, _) -> Var name))
                        let lambdaBody = Apply (func', toCallArgs allArgs)

                        // Create the lambda: fun p0 p1 ... -> func(providedArgs, p0, p1, ...)
                        let lambdaExpr = Lambda (toLambdaParams remainingParams, None, lambdaBody)

                        // The resulting type is a function from remaining params to return type
                        let partialType = TFunction (remainingParamTypes, returnType)

                        match expectedType with
                        | Some expected when not (typesEqual aliasReg expected partialType) ->
                            Error (TypeMismatch (expected, partialType, "partial application"))
                        | _ -> Ok (partialType, lambdaExpr))
                else
                    // Check each argument against expected param type
                    let rec checkArgs (argExprs: Expr list) (paramTys: Type list) (checkedArgs: Expr list) : Result<Expr list, TypeError> =
                        match argExprs, paramTys with
                        | [], [] -> Ok (List.rev checkedArgs)
                        | arg :: restArgs, paramTy :: restParams ->
                            checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramTy)
                            |> Result.bind (fun (argType, arg') ->
                                if typesEqual aliasReg argType paramTy then
                                    checkArgs restArgs restParams (arg' :: checkedArgs)
                                else
                                    Error (TypeMismatch (paramTy, argType, "function argument")))
                        | _ -> Error (GenericError "Argument count mismatch")
                    checkArgs argsList paramTypes []
                    |> Result.bind (fun args' ->
                        match expectedType with
                        | Some expected when not (typesEqual aliasReg expected returnType) ->
                            Error (TypeMismatch (expected, returnType, "function application result"))
                        | _ -> Ok (returnType, Apply (func', toCallArgs args')))
            | _ ->
                Error (GenericError $"Cannot apply non-function type: {typeToString funcType}"))

    | IndirectApply _ ->
        Crash.crash "IndirectApply is compiler-generated after expression type checking"

    | FuncRef funcName ->
        // Function reference: look up function signature
        match Map.tryFind funcName env with
        | Some funcType ->
            match expectedType with
            | Some expected when expected <> funcType ->
                Error (TypeMismatch (expected, funcType, $"function reference {funcName}"))
            | _ -> Ok (funcType, expr)
        | None ->
            Error (UndefinedVariable funcName)

    | Closure (funcName, captures) ->
        // Closure: function with captured values
        // The closure has the same type as the underlying function (minus closure param)
        // For now, just check the captures and return function type
        let checkCapture (cap: Expr) : Result<Expr, TypeError> =
            checkExpr cap env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
            |> Result.map snd
        let rec checkCaptures (caps: Expr list) (acc: Expr list) : Result<Expr list, TypeError> =
            match caps with
            | [] -> Ok (List.rev acc)
            | cap :: rest ->
                checkCapture cap |> Result.bind (fun cap' -> checkCaptures rest (cap' :: acc))
        checkCaptures captures []
        |> Result.bind (fun captures' ->
            // Look up closure function type
            match Map.tryFind funcName env with
            | Some (TFunction (_ :: restParams, returnType)) ->
                // The closure type is the function type without the closure param
                let closureType = TFunction (restParams, returnType)
                match expectedType with
                | Some expected when expected <> closureType ->
                    Error (TypeMismatch (expected, closureType, $"closure {funcName}"))
                | _ -> Ok (closureType, Closure (funcName, captures'))
            | Some funcType -> Ok (funcType, Closure (funcName, captures'))
            | None -> Error (UndefinedVariable funcName))
