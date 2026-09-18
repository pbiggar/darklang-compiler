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
    LiftedFunctions: AST.FunctionDef list
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
    (parameters: AST.NonEmptyList<AST.LambdaParameter>)
    (state: LiftState)
    : bool =
    parameters
    |> AST.NonEmptyList.toList
    |> List.map lambdaParameterType
    |> fun paramTypes -> Set.contains paramTypes state.ComparableFunctionParams

/// Collect free variables in an expression (variables not bound by let or lambda parameters)
let rec freeVars (expr: AST.Expr) (bound: Set<string>) : Set<string> =
    match expr with
    | AST.BoundaryRender (_, value) -> freeVars value bound
    | AST.UnitLiteral | AST.Int64Literal _ | AST.Int128Literal _ | AST.BigIntLiteral _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _ | AST.UInt128Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _ | AST.RuntimeError _ -> Set.empty
    | AST.Var name -> if Set.contains name bound then Set.empty else Set.singleton name
    | AST.BinOp (_, left, right) -> Set.union (freeVars left bound) (freeVars right bound)
    | AST.UnaryOp (_, inner) -> freeVars inner bound
    | AST.Let (pattern, value, body) ->
        let valueVars = freeVars value bound
        let bodyVars =
            freeVars body (Set.union bound (AST.letPatternBindings pattern |> Set.ofList))
        Set.union valueVars bodyVars
    | AST.RecursiveLet (recursion, value, body) ->
        let recursiveBound = Set.add (AST.recursiveBindingName recursion) bound
        Set.union (freeVars value recursiveBound) (freeVars body recursiveBound)
    | AST.If (cond, thenBr, elseBr) ->
        Set.union (freeVars cond bound) (Set.union (freeVars thenBr bound) (freeVars elseBr bound))
    | AST.Sequence (first, next) ->
        Set.union (freeVars first bound) (freeVars next bound)
    | AST.Call (funcName, args) ->
        // Check if funcName is a local variable (not in bound) - if so, it's a free variable
        // Top-level function names will be filtered out later since they won't be in TypeEnv
        let funcFree = if Set.contains funcName bound then Set.empty else Set.singleton funcName
        let argsFree =
            args
            |> exprArgsToList
            |> List.map (fun a -> freeVars a bound)
            |> List.fold Set.union Set.empty
        Set.union funcFree argsFree
    | AST.TypeApp (_, _, args) ->
        args |> exprArgsToList |> List.map (fun a -> freeVars a bound) |> List.fold Set.union Set.empty
    | AST.TupleLiteral elems | AST.ListLiteral elems ->
        elems |> List.map (fun e -> freeVars e bound) |> List.fold Set.union Set.empty
    | AST.TupleAccess (tuple, _) -> freeVars tuple bound
    | AST.DictLiteral (_, entries) ->
        entries |> List.map (fun (_, e) -> freeVars e bound) |> List.fold Set.union Set.empty
    | AST.RecordLiteral (_, fields) ->
        fields |> List.map (fun (_, e) -> freeVars e bound) |> List.fold Set.union Set.empty
    | AST.RecordUpdate (record, updates) ->
        let recordVars = freeVars record bound
        let updateVars = updates |> List.map (fun (_, e) -> freeVars e bound) |> List.fold Set.union Set.empty
        Set.union recordVars updateVars
    | AST.RecordAccess (record, _) -> freeVars record bound
    | AST.Constructor (_, _, payload) ->
        payload |> Option.map (fun e -> freeVars e bound) |> Option.defaultValue Set.empty
    | AST.Match (scrutinee, cases) ->
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
    | AST.Lambda (parameters, returnAnnotation, body) ->
        let paramNames =
            parameters
            |> AST.NonEmptyList.toList
            |> List.collect (fun parameter -> AST.letPatternBindings parameter.Pattern)
            |> Set.ofList
        freeVars body (Set.union bound paramNames)
    | AST.Apply (func, args)
    | AST.IndirectApply (func, args) ->
        let funcVars = freeVars func bound
        let argVars = args |> exprArgsToList |> List.map (fun a -> freeVars a bound) |> List.fold Set.union Set.empty
        Set.union funcVars argVars
    | AST.FuncRef _ -> Set.empty
    | AST.Closure (_, captures) ->
        // Closure captures may contain free variables
        captures |> List.map (fun c -> freeVars c bound) |> List.fold Set.union Set.empty
    | AST.InterpolatedString parts ->
        parts |> List.choose (fun part ->
            match part with
            | AST.StringText _ -> None
            | AST.StringExpr e -> Some (freeVars e bound))
        |> List.fold Set.union Set.empty

/// Simple type inference for lambda lifting - infers types of simple expressions
/// This allows let-bound variables to be captured in nested lambdas
/// Two types the checker already proved compatible, where either may still
/// carry what a literal leaves open (`[]` is a List<t>, `None` an Option<t>):
/// the concrete side wins at every position; two variables keep the first; a
/// real shape mismatch is None.
let rec reconcileBranchTypes (left: AST.Type) (right: AST.Type) : AST.Type option =
    let reconcileAll (lefts: AST.Type list) (rights: AST.Type list) : AST.Type list option =
        if List.length lefts <> List.length rights then None
        else
            List.zip lefts rights
            |> List.fold (fun reconciled (l, r) ->
                reconciled |> Option.bind (fun acc -> reconcileBranchTypes l r |> Option.map (fun t -> t :: acc))) (Some [])
            |> Option.map List.rev
    if left = right then Some left
    else
        match left, right with
        | AST.TVar _, concrete
        | concrete, AST.TVar _ -> Some concrete
        | AST.TRuntimeError, concrete
        | concrete, AST.TRuntimeError -> Some concrete
        | AST.TSum (leftName, leftArgs), AST.TSum (rightName, rightArgs) when leftName = rightName ->
            reconcileAll leftArgs rightArgs |> Option.map (fun args -> AST.TSum (leftName, args))
        | AST.TRecord (leftName, leftArgs), AST.TRecord (rightName, rightArgs) when leftName = rightName ->
            reconcileAll leftArgs rightArgs |> Option.map (fun args -> AST.TRecord (leftName, args))
        | AST.TList l, AST.TList r -> reconcileBranchTypes l r |> Option.map AST.TList
        | AST.TTuple ls, AST.TTuple rs -> reconcileAll ls rs |> Option.map AST.TTuple
        | AST.TDict (lk, lv), AST.TDict (rk, rv) ->
            reconcileBranchTypes lk rk |> Option.bind (fun k -> reconcileBranchTypes lv rv |> Option.map (fun v -> AST.TDict (k, v)))
        | AST.TFunction (largs, lret), AST.TFunction (rargs, rret) ->
            reconcileAll largs rargs |> Option.bind (fun args -> reconcileBranchTypes lret rret |> Option.map (fun ret -> AST.TFunction (args, ret)))
        | _ -> None

let rec simpleInferType
    (expr: AST.Expr)
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
    | AST.Int64Literal _ -> Some AST.TInt64
    | AST.Int128Literal _ -> Some AST.TInt128
    | AST.BigIntLiteral _ -> Some AST.TInt
    | AST.Int8Literal _ -> Some AST.TInt8
    | AST.Int16Literal _ -> Some AST.TInt16
    | AST.Int32Literal _ -> Some AST.TInt32
    | AST.UInt8Literal _ -> Some AST.TUInt8
    | AST.UInt16Literal _ -> Some AST.TUInt16
    | AST.UInt32Literal _ -> Some AST.TUInt32
    | AST.UInt64Literal _ -> Some AST.TUInt64
    | AST.UInt128Literal _ -> Some AST.TUInt128
    | AST.BoolLiteral _ -> Some AST.TBool
    | AST.StringLiteral _ -> Some AST.TString
    | AST.InterpolatedString _ -> Some AST.TString
    | AST.CharLiteral _ -> Some AST.TChar
    | AST.FloatLiteral _ -> Some AST.TFloat64
    | AST.UnitLiteral -> Some AST.TUnit
    | AST.Var name ->
        match Map.tryFind name typeEnv with
        | Some typ -> Some typ
        | None ->
            match Map.tryFind name funcParams, Map.tryFind name funcReturnTypes with
            | Some parameters, Some returnType ->
                Some (AST.TFunction (parameters |> List.map snd, returnType))
            | _ -> None
    | AST.FuncRef name ->
        match Map.tryFind name funcParams, Map.tryFind name funcReturnTypes with
        | Some parameters, Some returnType ->
            Some (AST.TFunction (parameters |> List.map snd, returnType))
        | _ -> None
    | AST.Let (pattern, value, body) ->
        let valueType = simpleInferType value typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup
        let typeEnv' =
            match valueType with
            | Some typ ->
                letPatternBindingTypes pattern typ
                |> List.fold (fun current (name, bindingType) -> Map.add name bindingType current) typeEnv
            | None -> typeEnv
        simpleInferType body typeEnv' funcParams funcReturnTypes genericFuncDefs typeReg variantLookup
    | AST.RecursiveLet (recursion, value, body) ->
        let valueType =
            match recursion with
            | AST.TypedRecursiveBinding typed -> Some typed.MonomorphicType
            | _ -> simpleInferType value typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup
        let typeEnv' =
            valueType
            |> Option.map (fun typ -> Map.add (AST.recursiveBindingName recursion) typ typeEnv)
            |> Option.defaultValue typeEnv
        simpleInferType body typeEnv' funcParams funcReturnTypes genericFuncDefs typeReg variantLookup
    | AST.TupleLiteral elements ->
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
    | AST.TupleAccess (tupleExpr, index) ->
        match simpleInferType tupleExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup with
        | Some (AST.TTuple elemTypes) when index >= 0 && index < List.length elemTypes ->
            Some (List.item index elemTypes)
        | _ -> None
    | AST.ListLiteral [] ->
        // Open at the element: reconciled against the other arm or branch.
        Some (AST.TList (AST.TVar "__empty_list_elem"))
    | AST.ListLiteral (first :: _) ->
        simpleInferType first typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup
        |> Option.map AST.TList
    | AST.DictLiteral (valueType, _) ->
        Some (AST.TDict (AST.TString, valueType))
    | AST.RecordLiteral (reference, fields) ->
            let typeName = reference.ResolvedTypeName
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
    | AST.RecordAccess (recordExpr, fieldName) ->
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
    | AST.Constructor (constructorReference, variantName, payload) ->
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
            AST.constructorReferenceTypeName constructorReference
            |> Option.map (fun typeName -> AST.TSum (typeName, []))
    | AST.BinOp (op, left, right) ->
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
    | AST.UnaryOp (op, operand) ->
        // `a != b` parses as Not (a == b), so a lambda ending in it is common.
        match op with
        | AST.Not -> Some AST.TBool
        | AST.Neg | AST.BitNot ->
            simpleInferType operand typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup
    | AST.Call (funcName, args) ->
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
    | AST.TypeApp (funcName, typeArgs, _) ->
        // Look up the generic function's definition and apply type substitution
        match Map.tryFind funcName genericFuncDefs with
        | Some (typeParams, returnType) when List.length typeParams = List.length typeArgs ->
            // Build substitution from type params to type args
            let subst = List.zip typeParams typeArgs |> Map.ofList
            Some (applySubstToType subst returnType)
        | _ ->
            // Fall back to funcReturnTypes for non-generic or arity mismatch
            Map.tryFind funcName funcReturnTypes
    | AST.If (_, thenExpr, elseExpr) ->
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
    | AST.Sequence (_, next) ->
        simpleInferType next typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup
    | AST.Match (scrutinee, cases) ->
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
            // Arms agree up to what a literal leaves open: `[]` is a List<t> next
            // to a List<Int64> arm, `None` an Option<t> next to a Some.
            let types = caseTypes |> List.choose id
            match types with
            | first :: rest ->
                rest |> List.fold (fun merged t -> merged |> Option.bind (fun m -> reconcileBranchTypes m t)) (Some first)
            | [] -> None
        else
            None
    | AST.Lambda (parameters, returnAnnotation, body) ->
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
    | AST.Apply (funcExpr, args) ->
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
    | AST.IndirectApply _ -> Some AST.TBool
    | _ -> None  // Complex expressions require full type inference

let inferLambdaReturnType (body: AST.Expr) (state: LiftState) : Result<AST.Type, string> =
    match simpleInferType body state.TypeEnv state.FuncParams state.FuncReturnTypes state.GenericFuncDefs state.TypeReg state.VariantLookup with
    | Some AST.TRuntimeError -> Ok AST.TUnit
    | Some returnType -> Ok returnType
    | None -> Error "Lambda lifting could not infer return type for lambda body"
