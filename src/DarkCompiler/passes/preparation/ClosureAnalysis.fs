// ClosureAnalysis.fs - Track closure environments, free variables, and inferred capture types.

module ClosureAnalysis

open MemoryModel
open ANF
open LoweringPrimitives
open TypeRegistries
open SpecializationIdentity
open TypeSubstitution

type LiftState = {
    Symbols: CheckedAST.Symbols
    Counter: int
    LiftedFunctions: CheckedAST.FunctionDef list
    ComparisonFuncs: Map<AST.FunctionId * AST.SemanticType list, string>
    ComparableFunctionParams: Set<AST.SemanticType list>
    TypeEnv: Map<AST.BindingId, AST.SemanticType>
    FuncParams: Map<AST.FunctionId, AST.SemanticType list>
    FuncReturnTypes: Map<AST.FunctionId, AST.SemanticType>
    GenericFuncDefs: Map<AST.FunctionId, string list * AST.SemanticType>
    TypeReg: TypeRegistry
    VariantLookup: VariantLookup
    RecursiveSelf: (AST.BindingId * AST.BindingId * AST.SemanticType * AST.TypedRecursiveMember) option
}

let private liftedNameExists (state: LiftState) (name: string) : bool =
    (CheckedAST.tryFindFunctionId name state.Symbols
     |> Option.exists (fun id -> Map.containsKey id state.FuncParams))
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
    (typeNames: TypeNameRegistry)
    (pattern: CheckedAST.Pattern)
    (scrutineeType: AST.SemanticType)
    : Map<AST.BindingId, AST.SemanticType> =
    let merge left right = Map.fold (fun current name typ -> Map.add name typ current) left right
    match pattern with
    | CheckedAST.POr alternatives ->
        matchPatternBindingTypes typeReg variantLookup typeNames (AST.NonEmptyList.head alternatives) scrutineeType
    | CheckedAST.PVariable id -> Map.ofList [(id, scrutineeType)]
    | CheckedAST.PWildcard | CheckedAST.PUnit | CheckedAST.PInt64 _ | CheckedAST.PBigInt _
    | CheckedAST.PInt128Literal _ | CheckedAST.PInt8Literal _ | CheckedAST.PInt16Literal _
    | CheckedAST.PInt32Literal _ | CheckedAST.PUInt8Literal _ | CheckedAST.PUInt16Literal _
    | CheckedAST.PUInt32Literal _ | CheckedAST.PUInt64Literal _ | CheckedAST.PUInt128Literal _
    | CheckedAST.PBool _ | CheckedAST.PString _ | CheckedAST.PChar _ | CheckedAST.PFloat _ -> Map.empty
    | CheckedAST.PTuple patterns ->
        match scrutineeType with
        | AST.TTuple elementTypes when List.length patterns = List.length elementTypes ->
            List.zip patterns elementTypes
            |> List.fold (fun current (innerPattern, elementType) ->
                merge current (matchPatternBindingTypes typeReg variantLookup typeNames innerPattern elementType)) Map.empty
        | _ -> Map.empty
    | CheckedAST.PConstructor (constructorId, fieldPatterns) ->
        match tryFindVariantForTypeById constructorId scrutineeType typeNames variantLookup with
        | Some (typeName, typeParameters, _, fieldTypes)
            when List.length fieldPatterns = List.length fieldTypes ->
            let substitution =
                match scrutineeType with
                | AST.TSum (scrutineeTypeName, typeArguments)
                    when scrutineeTypeName = typeName
                         && List.length typeParameters = List.length typeArguments ->
                    List.zip typeParameters typeArguments |> Map.ofList
                | _ -> Map.empty
            List.zip fieldPatterns fieldTypes
            |> List.fold (fun current (fieldPattern, fieldType) ->
                merge current
                    (matchPatternBindingTypes
                        typeReg
                        variantLookup
                        typeNames
                        fieldPattern
                        (applySubstToType substitution fieldType))) Map.empty
        | _ -> Map.empty
    | CheckedAST.PList patterns ->
        match scrutineeType with
        | AST.TList elementType ->
            patterns
            |> List.fold (fun current innerPattern ->
                merge current (matchPatternBindingTypes typeReg variantLookup typeNames innerPattern elementType)) Map.empty
        | _ -> Map.empty
    | CheckedAST.PListCons (headPatterns, tailPattern) ->
        match scrutineeType with
        | AST.TList elementType ->
            let headBindings =
                headPatterns
                |> List.fold (fun current innerPattern ->
                    merge current (matchPatternBindingTypes typeReg variantLookup typeNames innerPattern elementType)) Map.empty
            merge headBindings (matchPatternBindingTypes typeReg variantLookup typeNames tailPattern scrutineeType)
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
let rec freeVars (expr: CheckedAST.Expr) (bound: Set<AST.BindingId>) : Set<AST.BindingId> =
    match expr with
    | CheckedAST.BoundaryRender (_, value) -> freeVars value bound
    | CheckedAST.UnitLiteral | CheckedAST.Int64Literal _ | CheckedAST.Int128Literal _ | CheckedAST.BigIntLiteral _ | CheckedAST.Int8Literal _ | CheckedAST.Int16Literal _ | CheckedAST.Int32Literal _
    | CheckedAST.UInt8Literal _ | CheckedAST.UInt16Literal _ | CheckedAST.UInt32Literal _ | CheckedAST.UInt64Literal _ | CheckedAST.UInt128Literal _
    | CheckedAST.BoolLiteral _ | CheckedAST.StringLiteral _ | CheckedAST.BlobLiteral _ | CheckedAST.CharLiteral _ | CheckedAST.FloatLiteral _ | CheckedAST.RuntimeError _ -> Set.empty
    | CheckedAST.Local id -> if Set.contains id bound then Set.empty else Set.singleton id
    | CheckedAST.BinOp (_, left, right) -> Set.union (freeVars left bound) (freeVars right bound)
    | CheckedAST.UnaryOp (_, inner) -> freeVars inner bound
    | CheckedAST.Let (pattern, value, body) ->
        let valueVars = freeVars value bound
        let bodyVars =
            freeVars body (Set.union bound (CheckedAST.letPatternBindings pattern |> Set.ofList))
        Set.union valueVars bodyVars
    | CheckedAST.RecursiveLet (recursion, value, body) ->
        let recursiveBound = Set.add (CheckedAST.recursiveBindingId recursion) bound
        Set.union (freeVars value recursiveBound) (freeVars body recursiveBound)
    | CheckedAST.If (cond, thenBr, elseBr) ->
        Set.union (freeVars cond bound) (Set.union (freeVars thenBr bound) (freeVars elseBr bound))
    | CheckedAST.Sequence (first, next) ->
        Set.union (freeVars first bound) (freeVars next bound)
    | CheckedAST.Call (_, args) ->
        args |> exprArgsToList |> List.map (fun a -> freeVars a bound) |> List.fold Set.union Set.empty
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
    | CheckedAST.Constructor (_, fields) ->
        fields |> List.map (fun e -> freeVars e bound) |> List.fold Set.union Set.empty
    | CheckedAST.Match (scrutinee, cases) ->
        let scrutineeVars = freeVars scrutinee bound
        let caseVars =
            cases
            |> List.map (fun mc ->
                let caseNames =
                    mc.Patterns
                    |> AST.NonEmptyList.toList
                    |> List.collect CheckedAST.patternBindings
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
/// Two types the checker already proved compatible, where either may still
/// carry what a literal leaves open (`[]` is a List<t>, `None` an Option<t>):
/// the concrete side wins at every position; two variables keep the first; a
/// real shape mismatch is None.
let rec reconcileBranchTypes (left: AST.SemanticType) (right: AST.SemanticType) : AST.SemanticType option =
    let reconcileAll (lefts: AST.SemanticType list) (rights: AST.SemanticType list) : AST.SemanticType list option =
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
        | AST.TNever, concrete
        | concrete, AST.TNever -> Some concrete
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
    (expr: CheckedAST.Expr)
    (typeEnv: Map<AST.BindingId, AST.SemanticType>)
    (funcParams: Map<AST.FunctionId, AST.SemanticType list>)
    (funcReturnTypes: Map<AST.FunctionId, AST.SemanticType>)
    (genericFuncDefs: Map<AST.FunctionId, string list * AST.SemanticType>)
    (typeReg: TypeRegistry)
    (variantLookup: VariantLookup)
    (typeNames: TypeNameRegistry)
    : AST.SemanticType option =
    let fieldIndex id =
        tryFindFieldIndex id typeNames
        |> Option.defaultWith (fun () -> Crash.crash "Checked field identity is absent from layout metadata")
    let constructorTag id =
        tryFindConstructorTag id typeNames
        |> Option.defaultWith (fun () -> Crash.crash "Checked constructor identity is absent from layout metadata")
    let isIntType (typ: AST.SemanticType) : bool =
        match typ with
        | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
        | AST.TInt128 | AST.TInt
        | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
        | AST.TUInt128 -> true
        | _ -> false

    let isNumericType (typ: AST.SemanticType) : bool =
        isIntType typ || typ = AST.TFloat64

    let mergeBindings
        (bindings: Map<AST.BindingId, AST.SemanticType>)
        (extra: Map<AST.BindingId, AST.SemanticType>)
        : Map<AST.BindingId, AST.SemanticType> =
        Map.fold (fun acc name typ -> Map.add name typ acc) bindings extra

    let rec extractPatternBindings
        (pattern: CheckedAST.Pattern)
        (scrutType: AST.SemanticType)
        : Map<AST.BindingId, AST.SemanticType> =
        match pattern with
        | CheckedAST.POr alternatives ->
            extractPatternBindings (AST.NonEmptyList.head alternatives) scrutType
        | CheckedAST.PVariable id -> Map.ofList [(id, scrutType)]
        | CheckedAST.PWildcard -> Map.empty
        | CheckedAST.PInt64 _ | CheckedAST.PBigInt _ | CheckedAST.PInt128Literal _
        | CheckedAST.PInt8Literal _ | CheckedAST.PInt16Literal _ | CheckedAST.PInt32Literal _
        | CheckedAST.PUInt8Literal _ | CheckedAST.PUInt16Literal _ | CheckedAST.PUInt32Literal _
        | CheckedAST.PUInt64Literal _ | CheckedAST.PUInt128Literal _ | CheckedAST.PUnit
        | CheckedAST.PBool _ | CheckedAST.PString _ | CheckedAST.PChar _
        | CheckedAST.PFloat _ -> Map.empty
        | CheckedAST.PTuple innerPats ->
            match scrutType with
            | AST.TTuple elemTypes when List.length elemTypes = List.length innerPats ->
                List.zip innerPats elemTypes
                |> List.fold (fun acc (pat, typ) -> mergeBindings acc (extractPatternBindings pat typ)) Map.empty
            | _ -> Map.empty
        | CheckedAST.PConstructor (constructorId, fieldPatterns) ->
            match tryFindVariantForTypeById constructorId scrutType typeNames variantLookup with
            | Some (typeName, typeParams, _, fieldTypes)
                when List.length fieldPatterns = List.length fieldTypes ->
                let subst =
                    match scrutType with
                    | AST.TSum (scrutTypeName, typeArgs)
                        when scrutTypeName = typeName
                             && List.length typeParams = List.length typeArgs ->
                        List.zip typeParams typeArgs |> Map.ofList
                    | _ -> Map.empty
                List.zip fieldPatterns fieldTypes
                |> List.fold (fun current (fieldPattern, fieldType) ->
                    mergeBindings current
                        (extractPatternBindings fieldPattern (applySubstToType subst fieldType))) Map.empty
            | _ -> Map.empty
        | CheckedAST.PList innerPats ->
            match scrutType with
            | AST.TList elemType ->
                innerPats
                |> List.fold (fun acc pat -> mergeBindings acc (extractPatternBindings pat elemType)) Map.empty
            | _ -> Map.empty
        | CheckedAST.PListCons (headPats, tailPat) ->
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
    | CheckedAST.BlobLiteral _ -> Some AST.TBlob
    | CheckedAST.InterpolatedString _ -> Some AST.TString
    | CheckedAST.CharLiteral _ -> Some AST.TChar
    | CheckedAST.FloatLiteral _ -> Some AST.TFloat64
    | CheckedAST.UnitLiteral -> Some AST.TUnit
    | CheckedAST.Local id -> Map.tryFind id typeEnv
    | CheckedAST.FuncRef name ->
        match Map.tryFind name funcParams, Map.tryFind name funcReturnTypes with
        | Some parameters, Some returnType ->
            Some (AST.TFunction (parameters, returnType))
        | _ -> None
    | CheckedAST.Let (pattern, value, body) ->
        let valueType = simpleInferType value typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup typeNames
        let typeEnv' =
            match valueType with
            | Some typ ->
                letPatternBindingTypes pattern typ
                |> List.fold (fun current (name, bindingType) -> Map.add name bindingType current) typeEnv
            | None -> typeEnv
        simpleInferType body typeEnv' funcParams funcReturnTypes genericFuncDefs typeReg variantLookup typeNames
    | CheckedAST.RecursiveLet (recursion, value, body) ->
        let valueType = Some recursion.MonomorphicType
        let typeEnv' =
            valueType
            |> Option.map (fun typ -> Map.add (CheckedAST.recursiveBindingId recursion) typ typeEnv)
            |> Option.defaultValue typeEnv
        simpleInferType body typeEnv' funcParams funcReturnTypes genericFuncDefs typeReg variantLookup typeNames
    | CheckedAST.TupleLiteral elements ->
        // Recursively infer types of tuple elements
        let elemTypes = elements |> List.map (fun e -> simpleInferType e typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup typeNames)
        let rec collectTypes remaining acc =
            match remaining with
            | [] -> Some (List.rev acc)
            | Some typ :: rest -> collectTypes rest (typ :: acc)
            | None :: _ -> None

        match collectTypes elemTypes [] with
        | Some types -> Some (AST.TTuple types)
        | None -> None
    | CheckedAST.TupleAccess (tupleExpr, index) ->
        match simpleInferType tupleExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup typeNames with
        | Some (AST.TTuple elemTypes) when index >= 0 && index < List.length elemTypes ->
            Some (List.item index elemTypes)
        | _ -> None
    | CheckedAST.ListLiteral [] ->
        // Open at the element: reconciled against the other arm or branch.
        Some (AST.TList (AST.TVar "__empty_list_elem"))
    | CheckedAST.ListLiteral (first :: _) ->
        simpleInferType first typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup typeNames
        |> Option.map AST.TList
    | CheckedAST.DictLiteral (keyType, valueType, _) ->
        Some (AST.TDict (keyType, valueType))
    | CheckedAST.RecordLiteral (reference, fields) ->
        match tryFindRecordTypeNameById reference.TypeId typeNames with
        | None -> None
        | Some typeName ->
            match Map.tryFind typeName typeReg with
            | None ->
                Some (AST.TRecord (typeName, []))
            | Some recordInfo ->
                let expectedFields =
                    recordInfo.Fields
                    |> List.map (fun (fieldName, fieldType) ->
                        (fieldName, canonicalizeBareSumTypeRefs variantLookup fieldType))

                let fieldMap =
                    fields
                    |> List.map (fun (field, value) -> fieldIndex field, value)
                    |> Map.ofList
                let typeParams = recordInfo.TypeParams
                let rec inferBindings remaining acc =
                    match remaining with
                    | [] -> Some acc
                    | (fieldIndex, expectedFieldType) :: rest ->
                        match Map.tryFind fieldIndex fieldMap with
                        | None -> inferBindings rest acc
                        | Some fieldExpr ->
                            match simpleInferType fieldExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup typeNames with
                            | None -> inferBindings rest acc
                            | Some actualFieldType ->
                                let actualFieldType =
                                    canonicalizeBareSumTypeRefs variantLookup actualFieldType
                                match matchTypePattern expectedFieldType actualFieldType with
                                | Ok newBindings -> inferBindings rest (acc @ newBindings)
                                | Error _ -> inferBindings rest acc

                match inferBindings (expectedFields |> List.map snd |> List.indexed) [] with
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
        match simpleInferType recordExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup typeNames with
        | Some (AST.TRecord (typeName, typeArgs)) ->
            match Map.tryFind typeName typeReg with
            | Some recordInfo ->
                recordInfo.Fields
                |> List.tryItem (fieldIndex fieldName)
                |> Option.map (fun (_, fieldTypePattern) ->
                    match buildDeclaredRecordFieldSubst recordInfo typeArgs with
                    | Some subst -> applySubstToType subst fieldTypePattern
                    | None -> fieldTypePattern)
            | None -> None
        | _ -> None
    | CheckedAST.Constructor (constructorReference, fields) ->
        // Sum type constructor has the sum type; infer generic args from fields when possible.
        match
            tryFindSumTypeNameById constructorReference.TypeId typeNames
            |> Option.bind (fun typeName ->
                tryFindVariantByTag
                    typeName
                    (constructorTag constructorReference.ConstructorId)
                    variantLookup)
        with
        | Some (sumTypeName, typeParams, _, fieldPatterns) ->
            let defaultTypeArgs = typeParams |> List.map AST.TVar
            if List.length fieldPatterns <> List.length fields then
                Some (AST.TSum (sumTypeName, defaultTypeArgs))
            else
                List.zip fieldPatterns fields
                |> List.map (fun (fieldPattern, fieldExpr) ->
                    simpleInferType fieldExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup typeNames
                    |> Option.bind (fun actualFieldType ->
                        match matchTypePattern fieldPattern actualFieldType with
                        | Ok bindings -> Some bindings
                        | Error _ -> None))
                |> fun inferred ->
                    if inferred |> List.exists Option.isNone then
                        Some (AST.TSum (sumTypeName, defaultTypeArgs))
                    else
                        inferred
                        |> List.choose id
                        |> List.concat
                        |> consolidateTypeBindings
                        |> function
                            | Ok subst ->
                                typeParams
                                |> List.map (fun typeParam ->
                                    Map.tryFind typeParam subst |> Option.defaultValue (AST.TVar typeParam))
                                |> fun typeArgs -> Some (AST.TSum (sumTypeName, typeArgs))
                            | Error _ -> Some (AST.TSum (sumTypeName, defaultTypeArgs))
        | None ->
            tryFindSumTypeNameById constructorReference.TypeId typeNames
            |> Option.map (fun typeName -> AST.TSum (typeName, []))
    | CheckedAST.BinOp (op, left, right) ->
        let leftType = simpleInferType left typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup typeNames
        let rightType = simpleInferType right typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup typeNames
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
    | CheckedAST.UnaryOp (op, operand) ->
        // `a != b` parses as Not (a == b), so a lambda ending in it is common.
        match op with
        | AST.Not -> Some AST.TBool
        | AST.Neg | AST.BitNot ->
            simpleInferType operand typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup typeNames
    | CheckedAST.Call (funcName, args) ->
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
        match simpleInferType thenExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup typeNames,
              simpleInferType elseExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup typeNames with
        | Some thenType, Some elseType when thenType = elseType -> Some thenType
        | Some (AST.TSum (thenName, thenArgs)), Some (AST.TSum (elseName, [])) when thenName = elseName ->
            Some (AST.TSum (thenName, thenArgs))
        | Some (AST.TSum (thenName, [])), Some (AST.TSum (elseName, elseArgs)) when thenName = elseName ->
            Some (AST.TSum (elseName, elseArgs))
        | Some AST.TNever, Some elseType -> Some elseType
        | Some thenType, Some AST.TNever -> Some thenType
        | Some thenType, Some elseType -> reconcileBranchTypes thenType elseType
        | _ -> None
    | CheckedAST.Sequence (_, next) ->
        simpleInferType next typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup typeNames
    | CheckedAST.Match (scrutinee, cases) ->
        let scrutineeType = simpleInferType scrutinee typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup typeNames
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
                simpleInferType mc.Body caseEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup typeNames)
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
    | CheckedAST.Lambda (parameters, returnAnnotation, body) ->
        let paramTypes =
            parameters |> AST.NonEmptyList.toList |> List.map lambdaParameterType
        let lambdaParamTypes =
            parameters
            |> AST.NonEmptyList.toList
            |> List.collect lambdaParameterBindings
            |> Map.ofList
        let typeEnv' = Map.fold (fun acc k v -> Map.add k v acc) typeEnv lambdaParamTypes
        match simpleInferType body typeEnv' funcParams funcReturnTypes genericFuncDefs typeReg variantLookup typeNames with
        | Some returnType -> Some (AST.TFunction (paramTypes, returnType))
        | None -> None
    | CheckedAST.Apply (funcExpr, args) ->
        match simpleInferType funcExpr typeEnv funcParams funcReturnTypes genericFuncDefs typeReg variantLookup typeNames with
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

let inferLambdaReturnType (body: CheckedAST.Expr) (state: LiftState) : Result<AST.SemanticType, string> =
    match simpleInferType body state.TypeEnv state.FuncParams state.FuncReturnTypes state.GenericFuncDefs state.TypeReg state.VariantLookup (typeNamesFromSymbols state.Symbols) with
    | Some AST.TNever -> Ok AST.TUnit
    | Some returnType -> Ok returnType
    | None ->
        let target =
            match body with
            | CheckedAST.Call (id, _) ->
                CheckedAST.functionName id state.Symbols
                |> Option.map (fun name -> $" (call target: {name})")
                |> Option.defaultValue ""
            | _ -> ""
        Error $"Lambda lifting could not infer return type for lambda body{target}: {body}"
