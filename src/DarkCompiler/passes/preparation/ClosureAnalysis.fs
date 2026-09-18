// ClosureAnalysis.fs - Track closure environments, free variables, and inferred capture types.

module ClosureAnalysis

open MemoryModel
open ANF
open LoweringPrimitives
open TypeRegistries
open SpecializationIdentity
open TypeSubstitution

type LiftState = {
    Counter: int
    LiftedFunctions: CheckedAST.FunctionDef list
    ComparisonFuncs: Map<string * AST.Type list, string>
    ComparableFunctionParams: Set<AST.Type list>
    TypeEnv: Map<string, AST.Type>  // Variable name -> Type (for tracking types of captured variables)
    FuncParams: Map<string, (string * AST.Type) list>  // Function name -> params (for inferring function value types)
    FuncReturnTypes: Map<string, AST.Type>  // Function name -> Return type (for inferring call result types)
    GenericFuncDefs: Map<string, string list * AST.Type>  // Function name -> (TypeParams, ReturnType) for TypeApp substitution
    TypeReg: TypeRegistry
    VariantLookup: VariantLookup
    RecursiveSelf: (AST.BindingId * AST.Type * AST.TypedRecursiveMember) option
}

let private liftedNameExists (state: LiftState) (name: string) : bool =
    Map.containsKey name state.FuncParams
    || (state.LiftedFunctions |> List.exists (fun f -> f.Name = name))

let rec private findNextLiftedNameCounter
    (state: LiftState)
    (prefix: string)
    (counter: int)
    : int =
    let candidate = $"{prefix}{counter}"
    if liftedNameExists state candidate then
        findNextLiftedNameCounter state prefix (counter + 1)
    else
        counter

let internal freshLiftedName (state: LiftState) (prefix: string) : string * LiftState =
    let nextCounter = findNextLiftedNameCounter state prefix state.Counter
    let name = $"{prefix}{nextCounter}"
    (name, { state with Counter = nextCounter + 1 })

let rec internal matchPatternBindingTypes
    (typeReg: TypeRegistry)
    (variantLookup: VariantLookup)
    (pattern: AST.Pattern)
    (scrutineeType: AST.Type)
    : Map<string, AST.Type> =
    let merge left right = Map.fold (fun current name typ -> Map.add name typ current) left right
    match pattern with
    | AST.POr alternatives ->
        matchPatternBindingTypes typeReg variantLookup (AST.NonEmptyList.head alternatives) scrutineeType
    | AST.PVar name -> Map.ofList [(name, scrutineeType)]
    | AST.PWildcard | AST.PUnit | AST.PInt64 _ | AST.PBigInt _ | AST.PInt128Literal _
    | AST.PInt8Literal _ | AST.PInt16Literal _ | AST.PInt32Literal _
    | AST.PUInt8Literal _ | AST.PUInt16Literal _ | AST.PUInt32Literal _
    | AST.PUInt64Literal _ | AST.PUInt128Literal _ | AST.PBool _
    | AST.PString _ | AST.PChar _ | AST.PFloat _ -> Map.empty
    | AST.PTuple patterns ->
        match scrutineeType with
        | AST.TTuple elementTypes when List.length patterns = List.length elementTypes ->
            List.zip patterns elementTypes
            |> List.fold (fun current (innerPattern, elementType) ->
                merge current (matchPatternBindingTypes typeReg variantLookup innerPattern elementType)) Map.empty
        | _ -> Map.empty
    | AST.PConstructor (variantName, payloadPattern) ->
        match Map.tryFind variantName variantLookup, payloadPattern with
        | Some (typeName, typeParameters, _, Some payloadType), Some innerPattern ->
            let substitution =
                match scrutineeType with
                | AST.TSum (scrutineeTypeName, typeArguments)
                    when scrutineeTypeName = typeName
                         && List.length typeParameters = List.length typeArguments ->
                    List.zip typeParameters typeArguments |> Map.ofList
                | _ -> Map.empty
            matchPatternBindingTypes
                typeReg
                variantLookup
                innerPattern
                (match applySubstToType substitution payloadType with
                 | AST.TEnumFields fieldTypes -> AST.TTuple fieldTypes
                 | other -> other)
        | _ -> Map.empty
    | AST.PList patterns ->
        match scrutineeType with
        | AST.TList elementType ->
            patterns
            |> List.fold (fun current innerPattern ->
                merge current (matchPatternBindingTypes typeReg variantLookup innerPattern elementType)) Map.empty
        | _ -> Map.empty
    | AST.PListCons (headPatterns, tailPattern) ->
        match scrutineeType with
        | AST.TList elementType ->
            let headBindings =
                headPatterns
                |> List.fold (fun current innerPattern ->
                    merge current (matchPatternBindingTypes typeReg variantLookup innerPattern elementType)) Map.empty
            merge headBindings (matchPatternBindingTypes typeReg variantLookup tailPattern scrutineeType)
        | _ -> Map.empty

let internal lambdaNeedsComparison
    (parameters: AST.NonEmptyList<CheckedAST.LambdaParameter>)
    (state: LiftState)
    : bool =
    parameters
    |> AST.NonEmptyList.toList
    |> List.map lambdaParameterType
    |> fun paramTypes -> Set.contains paramTypes state.ComparableFunctionParams

/// Collect free variables in an expression (variables not bound by let or lambda parameters)
let rec freeVars (expr: CheckedAST.Expr) (bound: Set<string>) : Set<string> =
    match expr with
    | CheckedAST.BoundaryRender (_, value) -> freeVars value bound
    | CheckedAST.UnitLiteral | CheckedAST.Int64Literal _ | CheckedAST.Int128Literal _ | CheckedAST.BigIntLiteral _ | CheckedAST.Int8Literal _ | CheckedAST.Int16Literal _ | CheckedAST.Int32Literal _
    | CheckedAST.UInt8Literal _ | CheckedAST.UInt16Literal _ | CheckedAST.UInt32Literal _ | CheckedAST.UInt64Literal _ | CheckedAST.UInt128Literal _
    | CheckedAST.BoolLiteral _ | CheckedAST.StringLiteral _ | CheckedAST.CharLiteral _ | CheckedAST.FloatLiteral _ | CheckedAST.RuntimeError _ -> Set.empty
    | CheckedAST.Var name -> if Set.contains name bound then Set.empty else Set.singleton name
    | CheckedAST.BinOp (_, left, right) -> Set.union (freeVars left bound) (freeVars right bound)
    | CheckedAST.UnaryOp (_, inner) -> freeVars inner bound
    | CheckedAST.Let (pattern, value, body) ->
        let valueVars = freeVars value bound
        let bodyVars =
            freeVars body (Set.union bound (CheckedAST.letPatternBindings pattern |> Set.ofList))
        Set.union valueVars bodyVars
    | CheckedAST.RecursiveLet (recursion, value, body) ->
        let recursiveBound = Set.add (CheckedAST.recursiveBindingName recursion) bound
        Set.union (freeVars value recursiveBound) (freeVars body recursiveBound)
    | CheckedAST.If (cond, thenBr, elseBr) ->
        Set.union (freeVars cond bound) (Set.union (freeVars thenBr bound) (freeVars elseBr bound))
    | CheckedAST.Sequence (first, next) ->
        Set.union (freeVars first bound) (freeVars next bound)
    | CheckedAST.Call (funcName, args) ->
        // Check if funcName is a local variable (not in bound) - if so, it's a free variable
        // Top-level function names will be filtered out later since they won't be in TypeEnv
        let funcFree = if Set.contains funcName bound then Set.empty else Set.singleton funcName
        let argsFree =
            args
            |> exprArgsToList
            |> List.map (fun a -> freeVars a bound)
            |> List.fold Set.union Set.empty
        Set.union funcFree argsFree
    | CheckedAST.TypeApp (_, _, args) ->
        args |> exprArgsToList |> List.map (fun a -> freeVars a bound) |> List.fold Set.union Set.empty
    | CheckedAST.TupleLiteral elems | CheckedAST.ListLiteral elems ->
        elems |> List.map (fun e -> freeVars e bound) |> List.fold Set.union Set.empty
    | CheckedAST.TupleAccess (tuple, _) -> freeVars tuple bound
    | CheckedAST.DictLiteral (_, _, entries) ->
        entries
        |> List.collect (fun (key, value) -> [freeVars key bound; freeVars value bound])
        |> List.fold Set.union Set.empty
    | CheckedAST.RecordLiteral (_, fields) ->
        fields |> List.map (fun (_, e) -> freeVars e bound) |> List.fold Set.union Set.empty
    | CheckedAST.RecordUpdate (record, updates) ->
        let recordVars = freeVars record bound
        let updateVars = updates |> List.map (fun (_, e) -> freeVars e bound) |> List.fold Set.union Set.empty
        Set.union recordVars updateVars
    | CheckedAST.RecordAccess (record, _) -> freeVars record bound
    | CheckedAST.Constructor (_, _, payload) ->
        payload |> Option.map (fun e -> freeVars e bound) |> Option.defaultValue Set.empty
    | CheckedAST.Match (scrutinee, cases) ->
        let scrutineeVars = freeVars scrutinee bound
        let caseVars =
            cases
            |> List.map (fun mc ->
                let caseNames =
                    mc.Patterns
                    |> AST.NonEmptyList.toList
                    |> List.collect (fun pattern ->
                        AST.validateBinders (AST.MatchBinderPattern pattern)
                        |> Result.defaultValue [])
                    |> Set.ofList
                let caseBound = Set.union bound caseNames
                let guardVars =
                    mc.Guard
                    |> Option.map (fun guard -> freeVars guard caseBound)
                    |> Option.defaultValue Set.empty
                Set.union guardVars (freeVars mc.Body caseBound))
            |> List.fold Set.union Set.empty
        Set.union scrutineeVars caseVars
    | CheckedAST.Lambda (parameters, returnAnnotation, body) ->
        let paramNames =
            parameters
            |> AST.NonEmptyList.toList
            |> List.collect (fun parameter -> CheckedAST.letPatternBindings parameter.Pattern)
            |> Set.ofList
        freeVars body (Set.union bound paramNames)
    | CheckedAST.Apply (func, args)
    | CheckedAST.IndirectApply (func, args) ->
        let funcVars = freeVars func bound
        let argVars = args |> exprArgsToList |> List.map (fun a -> freeVars a bound) |> List.fold Set.union Set.empty
        Set.union funcVars argVars
    | CheckedAST.FuncRef _ -> Set.empty
    | CheckedAST.Closure (_, captures) ->
        // Closure captures may contain free variables
        captures |> List.map (fun c -> freeVars c bound) |> List.fold Set.union Set.empty
    | CheckedAST.InterpolatedString parts ->
        parts |> List.choose (fun part ->
            match part with
            | CheckedAST.StringText _ -> None
            | CheckedAST.StringExpr e -> Some (freeVars e bound))
        |> List.fold Set.union Set.empty

/// Simple type inference for lambda lifting - infers types of simple expressions
/// This allows let-bound variables to be captured in nested lambdas
let rec simpleInferType
    (expr: CheckedAST.Expr)
    (typeEnv: Map<string, AST.Type>)
    (funcParams: Map<string, (string * AST.Type) list>)
    (funcReturnTypes: Map<string, AST.Type>)
    (genericFuncDefs: Map<string, string list * AST.Type>)
    (typeReg: TypeRegistry)
    (variantLookup: VariantLookup)
    : AST.Type option =
    let isIntType (typ: AST.Type) : bool =
        match typ with
        | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
        | AST.TInt128 | AST.TInt
        | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
        | AST.TUInt128 -> true
        | _ -> false

    let isNumericType (typ: AST.Type) : bool =
        isIntType typ || typ = AST.TFloat64

    let mergeBindings (bindings: Map<string, AST.Type>) (extra: Map<string, AST.Type>) : Map<string, AST.Type> =
        Map.fold (fun acc name typ -> Map.add name typ acc) bindings extra

    let rec extractPatternBindings (pattern: AST.Pattern) (scrutType: AST.Type) : Map<string, AST.Type> =
        match pattern with
        | AST.POr alternatives ->
            extractPatternBindings (AST.NonEmptyList.head alternatives) scrutType
        | AST.PVar name -> Map.ofList [(name, scrutType)]
        | AST.PWildcard -> Map.empty
        | AST.PInt64 _ | AST.PBigInt _ | AST.PInt128Literal _
        | AST.PInt8Literal _
        | AST.PInt16Literal _
        | AST.PInt32Literal _
        | AST.PUInt8Literal _
        | AST.PUInt16Literal _
        | AST.PUInt32Literal _
        | AST.PUInt64Literal _ | AST.PUInt128Literal _
        | AST.PUnit
        | AST.PBool _
        | AST.PString _
        | AST.PChar _
        | AST.PFloat _ -> Map.empty
        | AST.PTuple innerPats ->
            match scrutType with
            | AST.TTuple elemTypes when List.length elemTypes = List.length innerPats ->
                List.zip innerPats elemTypes
                |> List.fold (fun acc (pat, typ) -> mergeBindings acc (extractPatternBindings pat typ)) Map.empty
            | _ -> Map.empty
        | AST.PConstructor (variantName, payloadPat) ->
            match Map.tryFind variantName variantLookup, payloadPat with
            | Some (typeName, typeParams, _, Some payloadType), Some pat ->
                let subst =
                    match scrutType with
                    | AST.TSum (scrutTypeName, typeArgs)
                        when scrutTypeName = typeName
                             && List.length typeParams = List.length typeArgs ->
                        List.zip typeParams typeArgs |> Map.ofList
                    | _ -> Map.empty
                extractPatternBindings
                    pat
                    (match applySubstToType subst payloadType with
                     | AST.TEnumFields fieldTypes -> AST.TTuple fieldTypes
                     | other -> other)
            | _ -> Map.empty
        | AST.PList innerPats ->
            match scrutType with
            | AST.TList elemType ->
                innerPats
                |> List.fold (fun acc pat -> mergeBindings acc (extractPatternBindings pat elemType)) Map.empty
            | _ -> Map.empty
        | AST.PListCons (headPats, tailPat) ->
            match scrutType with
            | AST.TList elemType ->
                let headBindings =
                    headPats
                    |> List.fold (fun acc pat -> mergeBindings acc (extractPatternBindings pat elemType)) Map.empty
                mergeBindings headBindings (extractPatternBindings tailPat scrutType)
            | _ -> Map.empty

    match expr with
    | CheckedAST.Int64Literal _ -> Some AST.TInt64
    | CheckedAST.Int128Literal _ -> Some AST.TInt128
    | CheckedAST.BigIntLiteral _ -> Some AST.TInt
    | CheckedAST.Int8Literal _ -> Some AST.TInt8
    | CheckedAST.Int16Literal _ -> Some AST.TInt16
    | CheckedAST.Int32Literal _ -> Some AST.TInt32
    | CheckedAST.UInt8Literal _ -> Some AST.TUInt8
    | CheckedAST.UInt16Literal _ -> Some AST.TUInt16
    | CheckedAST.UInt32Literal _ -> Some AST.TUInt32
    | CheckedAST.UInt64Literal _ -> Some AST.TUInt64
    | CheckedAST.UInt128Literal _ -> Some AST.TUInt128
    | CheckedAST.BoolLiteral _ -> Some AST.TBool
    | CheckedAST.StringLiteral _ -> Some AST.TString
    | CheckedAST.InterpolatedString _ -> Some AST.TString
    | CheckedAST.CharLiteral _ -> Some AST.TChar
    | CheckedAST.FloatLiteral _ -> Some AST.TFloat64
    | CheckedAST.UnitLiteral -> Some AST.TUnit
    | CheckedAST.Var name ->
        match Map.tryFind name typeEnv with
        | Some typ -> Some typ
        | None ->
            match Map.tryFind name funcParams, Map.tryFind name funcReturnTypes with
            | Some parameters, Some returnType ->
                Some (AST.TFunction (parameters |> List.map snd, returnType))
            | _ -> None
    | CheckedAST.FuncRef name ->
        match Map.tryFind name funcParams, Map.tryFind name funcReturnTypes with
        | Some parameters, Some returnType ->
            Some (AST.TFunction (parameters |> List.map snd, returnType))
        | _ -> None
    | CheckedAST.Let (pattern, value, body) ->
        let valueType = simpleInferType value typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup
        let typeEnv' =
            match valueType with
            | Some typ ->
                letPatternBindingTypes pattern typ
                |> List.fold (fun current (name, bindingType) -> Map.add name bindingType current) typeEnv
            | None -> typeEnv
        simpleInferType body typeEnv' funcParams funcReturnTypes genericFuncDefs typeReg variantLookup
    | CheckedAST.RecursiveLet (recursion, value, body) ->
        let valueType = Some recursion.MonomorphicType
        let typeEnv' =
            valueType
            |> Option.map (fun typ -> Map.add (CheckedAST.recursiveBindingName recursion) typ typeEnv)
            |> Option.defaultValue typeEnv
        simpleInferType body typeEnv' funcParams funcReturnTypes genericFuncDefs typeReg variantLookup
    | CheckedAST.TupleLiteral elements ->
        // Recursively infer types of tuple elements
        let elemTypes = elements |> List.map (fun e -> simpleInferType e typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup)
        let rec collectTypes remaining acc =
            match remaining with
            | [] -> Some (List.rev acc)
            | Some typ :: rest -> collectTypes rest (typ :: acc)
            | None :: _ -> None

        match collectTypes elemTypes [] with
        | Some types -> Some (AST.TTuple types)
        | None -> None
    | CheckedAST.TupleAccess (tupleExpr, index) ->
        match simpleInferType tupleExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup with
        | Some (AST.TTuple elemTypes) when index >= 0 && index < List.length elemTypes ->
            Some (List.item index elemTypes)
        | _ -> None
    | CheckedAST.DictLiteral (keyType, valueType, _) ->
        Some (AST.TDict (keyType, valueType))
    | CheckedAST.RecordLiteral (reference, fields) ->
            let typeName = reference.TypeName
            match Map.tryFind typeName typeReg with
            | None ->
                Some (AST.TRecord (typeName, []))
            | Some recordInfo ->
                let expectedFields =
                    recordInfo.Fields
                    |> List.map (fun (fieldName, fieldType) ->
                        (fieldName, canonicalizeBareSumTypeRefs variantLookup fieldType))

                let fieldMap = Map.ofList fields
                let typeParams = recordInfo.TypeParams
                let rec inferBindings remaining acc =
                    match remaining with
                    | [] -> Some acc
                    | (fieldName, expectedFieldType) :: rest ->
                        match Map.tryFind fieldName fieldMap with
                        | None -> inferBindings rest acc
                        | Some fieldExpr ->
                            match simpleInferType fieldExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup with
                            | None -> inferBindings rest acc
                            | Some actualFieldType ->
                                let actualFieldType =
                                    canonicalizeBareSumTypeRefs variantLookup actualFieldType
                                match matchTypePattern expectedFieldType actualFieldType with
                                | Ok newBindings -> inferBindings rest (acc @ newBindings)
                                | Error _ -> inferBindings rest acc

                match inferBindings expectedFields [] with
                | None ->
                    Some (AST.TRecord (typeName, []))
                | Some bindings ->
                    match consolidateTypeBindings bindings with
                    | Error _ ->
                        Some (AST.TRecord (typeName, []))
                    | Ok subst ->
                        let typeArgs =
                            if List.isEmpty reference.TypeArgs then
                                typeParams
                                |> List.map (fun name -> Map.tryFind name subst |> Option.defaultValue (AST.TVar name))
                            else reference.TypeArgs
                        Some (AST.TRecord (typeName, typeArgs))
    | CheckedAST.RecordAccess (recordExpr, fieldName) ->
        match simpleInferType recordExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup with
        | Some (AST.TRecord (typeName, typeArgs)) ->
            match Map.tryFind typeName typeReg with
            | Some recordInfo ->
                recordInfo.Fields
                |> List.tryFind (fun (name, _) -> name = fieldName)
                |> Option.map (fun (_, fieldTypePattern) ->
                    match buildDeclaredRecordFieldSubst recordInfo typeArgs with
                    | Some subst -> applySubstToType subst fieldTypePattern
                    | None -> fieldTypePattern)
            | None -> None
        | _ -> None
    | CheckedAST.Constructor (constructorReference, variantName, payload) ->
        // Sum type constructor has the sum type; infer generic args from payload when possible.
        match tryFindVariant constructorReference variantName variantLookup with
        | Some (sumTypeName, typeParams, _, payloadPattern) ->
            let defaultTypeArgs = typeParams |> List.map AST.TVar
            match payloadPattern, payload with
            | Some expectedPayloadType, Some payloadExpr ->
                match simpleInferType payloadExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup with
                | Some actualPayloadType ->
                    match matchTypePattern expectedPayloadType actualPayloadType with
                    | Ok bindings ->
                        match consolidateTypeBindings bindings with
                        | Ok subst ->
                            let typeArgs =
                                typeParams
                                |> List.map (fun typeParam ->
                                    Map.tryFind typeParam subst |> Option.defaultValue (AST.TVar typeParam))
                            Some (AST.TSum (sumTypeName, typeArgs))
                        | Error _ ->
                            Some (AST.TSum (sumTypeName, defaultTypeArgs))
                    | Error _ ->
                        Some (AST.TSum (sumTypeName, defaultTypeArgs))
                | None ->
                    Some (AST.TSum (sumTypeName, defaultTypeArgs))
            | _ ->
                Some (AST.TSum (sumTypeName, defaultTypeArgs))
        | None ->
            Some (AST.TSum (constructorReference.TypeName, []))
    | CheckedAST.BinOp (op, left, right) ->
        let leftType = simpleInferType left typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup
        let rightType = simpleInferType right typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup
        match op with
        | AST.Add | AST.Sub | AST.Mul | AST.Div | AST.Mod | AST.Pow ->
            match leftType, rightType with
            | Some lt, Some rt when lt = rt && isNumericType lt -> Some lt
            | Some (AST.TVar _), Some rt when isNumericType rt -> Some rt
            | Some lt, Some (AST.TVar _) when isNumericType lt -> Some lt
            | _ -> None
        | AST.Shl | AST.Shr | AST.BitAnd | AST.BitOr | AST.BitXor ->
            match leftType, rightType with
            | Some lt, Some rt when lt = rt && isIntType lt -> Some lt
            | _ -> None
        | AST.Eq | AST.Neq | AST.Lt | AST.Gt | AST.Lte | AST.Gte | AST.And | AST.Or -> Some AST.TBool
        | AST.StringConcat -> Some AST.TString
    | CheckedAST.Call (funcName, args) ->
        // Look up the function's return type, checking local bindings first
        if isRuntimeFailureName funcName then
            Some AST.TRuntimeError
        else
            match Map.tryFind funcName typeEnv with
            | Some (AST.TFunction (paramTypes, returnType)) ->
                let argCount = args |> exprArgsToList |> List.length
                let paramCount = List.length paramTypes
                if argCount = paramCount then
                    Some returnType
                elif argCount < paramCount then
                    Some (AST.TFunction (paramTypes |> List.skip argCount, returnType))
                else
                    None
            | Some (AST.TVar funcTypeVar) ->
                // Higher-order generic values can remain unresolved in public source.
                // Keep lambda lifting moving by modeling a symbolic return type.
                Some (AST.TVar $"__call_result_{funcTypeVar}")
            | _ ->
                Map.tryFind funcName funcReturnTypes
    | CheckedAST.TypeApp (funcName, typeArgs, _) ->
        // Look up the generic function's definition and apply type substitution
        match Map.tryFind funcName genericFuncDefs with
        | Some (typeParams, returnType) when List.length typeParams = List.length typeArgs ->
            // Build substitution from type params to type args
            let subst = List.zip typeParams typeArgs |> Map.ofList
            Some (applySubstToType subst returnType)
        | _ ->
            // Fall back to funcReturnTypes for non-generic or arity mismatch
            Map.tryFind funcName funcReturnTypes
    | CheckedAST.If (_, thenExpr, elseExpr) ->
        let rec reconcileBranchTypes (left: AST.Type) (right: AST.Type) : AST.Type option =
            if left = right then Some left
            else
                match left, right with
                | AST.TVar _, concrete
                | concrete, AST.TVar _ -> Some concrete
                | AST.TRuntimeError, concrete
                | concrete, AST.TRuntimeError -> Some concrete
                | AST.TSum (leftName, leftArgs), AST.TSum (rightName, rightArgs)
                    when leftName = rightName && List.length leftArgs = List.length rightArgs ->
                    List.zip leftArgs rightArgs
                    |> List.fold (fun reconciled (leftArg, rightArg) ->
                        reconciled
                        |> Option.bind (fun args ->
                            reconcileBranchTypes leftArg rightArg
                            |> Option.map (fun arg -> arg :: args))) (Some [])
                    |> Option.map (List.rev >> fun args -> AST.TSum (leftName, args))
                | _ -> None

        match simpleInferType thenExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup,
              simpleInferType elseExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup with
        | Some thenType, Some elseType when thenType = elseType -> Some thenType
        | Some (AST.TSum (thenName, thenArgs)), Some (AST.TSum (elseName, [])) when thenName = elseName ->
            Some (AST.TSum (thenName, thenArgs))
        | Some (AST.TSum (thenName, [])), Some (AST.TSum (elseName, elseArgs)) when thenName = elseName ->
            Some (AST.TSum (elseName, elseArgs))
        | Some AST.TRuntimeError, Some elseType -> Some elseType
        | Some thenType, Some AST.TRuntimeError -> Some thenType
        | Some thenType, Some elseType -> reconcileBranchTypes thenType elseType
        | _ -> None
    | CheckedAST.Sequence (_, next) ->
        simpleInferType next typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup
    | CheckedAST.Match (scrutinee, cases) ->
        let scrutineeType = simpleInferType scrutinee typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup
        let caseTypes =
            cases
            |> List.map (fun mc ->
                let patterns = AST.NonEmptyList.toList mc.Patterns
                let caseEnv =
                    match scrutineeType with
                    | Some scrutType ->
                        patterns
                        |> List.map (fun pat -> extractPatternBindings pat scrutType)
                        |> List.fold mergeBindings typeEnv
                    | None -> typeEnv
                simpleInferType mc.Body caseEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup)
        if List.forall Option.isSome caseTypes then
            let types = caseTypes |> List.choose id
            match types with
            | first :: rest when rest |> List.forall (fun t -> t = first) -> Some first
            | _ -> None
        else
            None
    | CheckedAST.Lambda (parameters, returnAnnotation, body) ->
        let paramTypes =
            parameters |> AST.NonEmptyList.toList |> List.map lambdaParameterType
        let lambdaParamTypes =
            parameters
            |> AST.NonEmptyList.toList
            |> List.collect lambdaParameterBindings
            |> Map.ofList
        let typeEnv' = Map.fold (fun acc k v -> Map.add k v acc) typeEnv lambdaParamTypes
        match simpleInferType body typeEnv' funcParams funcReturnTypes genericFuncDefs typeReg variantLookup with
        | Some returnType -> Some (AST.TFunction (paramTypes, returnType))
        | None -> None
    | CheckedAST.Apply (funcExpr, args) ->
        match simpleInferType funcExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup with
        | Some (AST.TFunction (paramTypes, returnType)) ->
            let argCount = args |> exprArgsToList |> List.length
            let paramCount = List.length paramTypes
            if argCount = paramCount then
                Some returnType
            elif argCount < paramCount then
                Some (AST.TFunction (paramTypes |> List.skip argCount, returnType))
            else
                None
        | _ -> None
    | CheckedAST.IndirectApply _ -> Some AST.TBool
    | _ -> None  // Complex expressions require full type inference

let inferLambdaReturnType (body: CheckedAST.Expr) (state: LiftState) : Result<AST.Type, string> =
    match simpleInferType body state.TypeEnv state.FuncParams state.FuncReturnTypes state.GenericFuncDefs state.TypeReg state.VariantLookup with
    | Some AST.TRuntimeError -> Ok AST.TUnit
    | Some returnType -> Ok returnType
    | None -> Error "Lambda lifting could not infer return type for lambda body"
