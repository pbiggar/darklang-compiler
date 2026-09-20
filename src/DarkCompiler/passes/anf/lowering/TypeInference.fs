// TypeInference.fs - Recover checked expression types for representation-directed ANF lowering.

module LoweringTypeInference

open MemoryModel
open ANF
open LoweringPrimitives
open TypeRegistries
open SpecializationIdentity
open TypeSubstitution
open ClosureAnalysis
open LiftExpressions
open LiftFunctions

let rec inferTypeCore (sumTypeNames: Set<string>) (expr: CheckedAST.Expr) (typeEnv: Map<AST.BindingId, AST.Type>) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (funcReg: FunctionRegistry) (functionNames: FunctionNameRegistry) (moduleRegistry: AST.ModuleRegistry) : Result<AST.Type, string> =
    match expr with
    | CheckedAST.BoundaryRender _ -> Ok AST.TString
    | CheckedAST.RuntimeError _ -> Ok AST.TRuntimeError
    | CheckedAST.UnitLiteral -> Ok AST.TUnit
    | CheckedAST.Int64Literal _ -> Ok AST.TInt64
    | CheckedAST.Int128Literal _ -> Ok AST.TInt128
    | CheckedAST.BigIntLiteral _ -> Ok AST.TInt
    | CheckedAST.Int8Literal _ -> Ok AST.TInt8
    | CheckedAST.Int16Literal _ -> Ok AST.TInt16
    | CheckedAST.Int32Literal _ -> Ok AST.TInt32
    | CheckedAST.UInt8Literal _ -> Ok AST.TUInt8
    | CheckedAST.UInt16Literal _ -> Ok AST.TUInt16
    | CheckedAST.UInt32Literal _ -> Ok AST.TUInt32
    | CheckedAST.UInt64Literal _ -> Ok AST.TUInt64
    | CheckedAST.UInt128Literal _ -> Ok AST.TUInt128
    | CheckedAST.BoolLiteral _ -> Ok AST.TBool
    | CheckedAST.StringLiteral _ -> Ok AST.TString
    | CheckedAST.CharLiteral _ -> Ok AST.TChar
    | CheckedAST.FloatLiteral _ -> Ok AST.TFloat64
    | CheckedAST.Local id ->
        match Map.tryFind id typeEnv with
        | Some typ -> Ok typ
        | None -> Error "Cannot infer type: undefined local binding identity"
    | CheckedAST.NamedValue name ->
        if isBuiltinTestNanName name || isBuiltinTestInfinityName name then
            Ok AST.TFloat64
        else if isBuiltinBlobEmptyName name then
            Ok AST.TBlob
        else
            // Check if it's a module function (e.g., Stdlib.Int64.add)
            match Stdlib.tryGetFunction moduleRegistry name with
            | Some (moduleFunc, _) -> Ok (Stdlib.getFunctionType moduleFunc)
            | None -> Error $"Cannot infer type: undefined named value '{name}'"
    | CheckedAST.DictLiteral (keyType, valueType, _) ->
        Ok (AST.TDict (keyType, valueType))
    | CheckedAST.RecordLiteral (reference, fields) ->
        match tryFindRecordTypeNameById reference.TypeId typeReg with
        | None -> Error "Unknown semantic record type"
        | Some typeName ->
            match Map.tryFind typeName typeReg with
            | None ->
                Error $"Unknown record type: {typeName}"
            | Some recordInfo ->
                let expectedFields =
                    recordInfo.Fields
                    |> List.map (fun (fieldName, fieldType) ->
                        (fieldName, canonicalizeBareSumTypeRefs variantLookup fieldType))

                let fieldMap =
                    fields
                    |> List.map (fun (field, value) -> AST.fieldIndex field, value)
                    |> Map.ofList
                let typeParams = recordInfo.TypeParams

                let rec inferBindings
                    (remainingFields: (int * AST.Type) list)
                    (accBindings: (string * AST.Type) list)
                    : Result<(string * AST.Type) list, string> =
                    match remainingFields with
                    | [] -> Ok accBindings
                    | (fieldIndex, expectedFieldType) :: rest ->
                        match Map.tryFind fieldIndex fieldMap with
                        | None ->
                            // Type checker should have enforced completeness already.
                            inferBindings rest accBindings
                        | Some fieldExpr ->
                            inferTypeCore sumTypeNames fieldExpr typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
                            |> Result.bind (fun actualFieldType ->
                                let actualFieldType =
                                    canonicalizeBareSumTypeRefs variantLookup actualFieldType
                                matchTypePattern expectedFieldType actualFieldType
                                |> Result.bind (fun newBindings ->
                                    inferBindings rest (accBindings @ newBindings)))

                inferBindings (expectedFields |> List.map snd |> List.indexed) []
                |> Result.bind consolidateTypeBindings
                |> Result.map (fun subst ->
                    let typeArgs =
                        if List.isEmpty reference.TypeArgs then
                            typeParams
                            |> List.map (fun typeParam ->
                                Map.tryFind typeParam subst |> Option.defaultValue (AST.TVar typeParam))
                        else reference.TypeArgs
                    AST.TRecord (typeName, typeArgs))
    | CheckedAST.RecordUpdate (recordExpr, _) ->
        // Record update returns the same type as the record being updated
        inferTypeCore sumTypeNames recordExpr typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
    | CheckedAST.RecordAccess (recordExpr, fieldName) ->
        inferTypeCore sumTypeNames recordExpr typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
        |> Result.bind (fun recordType ->
            match recordType with
            | AST.TRecord (typeName, typeArgs) ->
                match Map.tryFind typeName typeReg with
                | Some recordInfo ->
                    match List.tryItem (AST.fieldIndex fieldName) recordInfo.Fields with
                    | Some (_, fieldTypePattern) ->
                        let fieldType =
                            match buildDeclaredRecordFieldSubst recordInfo typeArgs with
                            | Some subst -> applySubstToType subst fieldTypePattern
                            | None -> fieldTypePattern
                        Ok fieldType
                    | None -> Error $"Record type {typeName} has no field at the resolved slot"
                | None -> Error $"Unknown record type: {typeName}"
            | _ -> Error $"Cannot access field on non-record type")
    | CheckedAST.TupleLiteral elems ->
        elems
        |> List.map (fun e -> inferTypeCore sumTypeNames e typeEnv typeReg variantLookup funcReg functionNames moduleRegistry)
        |> List.fold (fun acc r ->
            match acc, r with
            | Ok types, Ok t -> Ok (types @ [t])
            | Error e, _ -> Error e
            | _, Error e -> Error e) (Ok [])
        |> Result.map AST.TTuple
    | CheckedAST.TupleAccess (tupleExpr, index) ->
        inferTypeCore sumTypeNames tupleExpr typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
        |> Result.bind (fun tupleType ->
            match tupleType with
            | AST.TTuple elemTypes when index >= 0 && index < List.length elemTypes ->
                Ok (List.item index elemTypes)
            | AST.TTuple _ -> Error $"Tuple index {index} out of bounds"
            | _ -> Error "Cannot access index on non-tuple type")
    | CheckedAST.Constructor (constructorReference, fields) ->
        match tryFindSumTypeNameById constructorReference.TypeId variantLookup with
        | None -> Error "Unknown semantic constructor type"
        | Some constructorTypeName ->
            match tryFindVariantByTag constructorTypeName (AST.constructorTag constructorReference.ConstructorId) variantLookup with
            | None ->
                Error $"Unknown constructor tag: {AST.constructorTag constructorReference.ConstructorId}"
            | Some (typeName, typeParams, _, fieldPatterns) ->
                let defaultTypeArgs = typeParams |> List.map AST.TVar
                if List.length fieldPatterns <> List.length fields then
                    Ok (AST.TSum (typeName, defaultTypeArgs))
                else
                    List.zip fieldPatterns fields
                    |> List.fold (fun result (fieldPattern, fieldExpr) ->
                        result
                        |> Result.bind (fun bindings ->
                            inferTypeCore sumTypeNames fieldExpr typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
                            |> Result.map (fun actualFieldType ->
                                match matchTypePattern fieldPattern actualFieldType with
                                | Ok fieldBindings -> bindings @ fieldBindings
                                | Error _ -> bindings))) (Ok [])
                    |> Result.map (fun bindings ->
                        match consolidateTypeBindings bindings with
                        | Error _ -> AST.TSum (typeName, defaultTypeArgs)
                        | Ok subst ->
                            typeParams
                            |> List.map (fun typeParam ->
                                Map.tryFind typeParam subst |> Option.defaultValue (AST.TVar typeParam))
                            |> fun typeArgs -> AST.TSum (typeName, typeArgs))
    | CheckedAST.ListLiteral elements ->
        match elements with
        | [] -> Ok (AST.TList (AST.TVar "t"))  // Preserve unknown element type for empty lists
        | first :: _ ->
            inferTypeCore sumTypeNames first typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
            |> Result.map (fun elemType -> AST.TList elemType)
    | CheckedAST.Let (pattern, value, body) ->
        inferTypeCore sumTypeNames value typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
        |> Result.bind (fun valueType ->
            let typeEnv' =
                letPatternBindingTypes pattern valueType
                |> List.fold (fun current (name, bindingType) -> Map.add name bindingType current) typeEnv
            inferTypeCore sumTypeNames body typeEnv' typeReg variantLookup funcReg functionNames moduleRegistry)
    | CheckedAST.RecursiveLet (recursion, value, body) ->
        let valueTypeResult = Ok recursion.MonomorphicType
        valueTypeResult
        |> Result.bind (fun valueType ->
            inferTypeCore sumTypeNames
                body
                (Map.add (CheckedAST.recursiveBindingId recursion) valueType typeEnv)
                typeReg
                variantLookup
                funcReg
                functionNames
                moduleRegistry)
    | CheckedAST.If (_, thenExpr, elseExpr) ->
        let inferBranchType (branchExpr: CheckedAST.Expr) : Result<AST.Type, string> =
            inferTypeCore sumTypeNames branchExpr typeEnv typeReg variantLookup funcReg functionNames moduleRegistry

        let resolveBranchType (preferred: AST.Type) (other: AST.Type) : Result<AST.Type, string> =
            match matchTypePattern preferred other with
            | Error _ -> Error "Branch type mismatch"
            | Ok bindings ->
                match consolidateTypeBindings bindings with
                | Error e -> Error e
                | Ok subst -> Ok (applySubstToType subst preferred)

        inferBranchType thenExpr
        |> Result.bind (fun thenType ->
            inferBranchType elseExpr
            |> Result.bind (fun elseType ->
                if thenType = elseType then
                    Ok thenType
                elif thenType = AST.TRuntimeError then
                    Ok elseType
                elif elseType = AST.TRuntimeError then
                    Ok thenType
                else
                    match resolveBranchType thenType elseType, resolveBranchType elseType thenType with
                    | Ok resolvedThen, Ok resolvedElse ->
                        if containsTypeVar resolvedThen && not (containsTypeVar resolvedElse) then
                            Ok resolvedElse
                        elif containsTypeVar resolvedElse && not (containsTypeVar resolvedThen) then
                            Ok resolvedThen
                        else
                            Ok resolvedThen
                    | Ok resolvedThen, Error _ -> Ok resolvedThen
                    | Error _, Ok resolvedElse -> Ok resolvedElse
                    | Error _, Error _ ->
                        Error
                            $"If branches have incompatible types: then={typeToString thenType}, else={typeToString elseType}"))
    | CheckedAST.Sequence (_, next) ->
        inferTypeCore sumTypeNames next typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
    | CheckedAST.BinOp (op, left, right) ->
        let ensureSameType () =
            inferTypeCore sumTypeNames left typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
            |> Result.bind (fun leftType ->
                inferTypeCore sumTypeNames right typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
                |> Result.bind (fun rightType ->
                    if leftType = AST.TRuntimeError then
                        Ok rightType
                    elif rightType = AST.TRuntimeError then
                        Ok leftType
                    elif leftType = rightType then
                        Ok leftType
                    else
                        matchTypePattern leftType rightType
                        |> Result.bind consolidateTypeBindings
                        |> Result.map (fun subst -> applySubstToType subst leftType)
                        |> Result.mapError (fun _ ->
                            $"Binary operator operands must match: left={leftType}, right={rightType}")))
        match op with
        | AST.Add | AST.Sub | AST.Mul | AST.Div | AST.Mod | AST.Pow ->
            ensureSameType ()
            |> Result.bind (fun operandType ->
                match operandType with
                | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
                | AST.TInt128 | AST.TInt
                | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TUInt128
                | AST.TFloat64 -> Ok operandType
                | _ -> Error $"Arithmetic operator requires numeric operands, got {operandType}")
        | AST.Shl | AST.Shr | AST.BitAnd | AST.BitOr | AST.BitXor ->
            ensureSameType ()
            |> Result.bind (fun operandType ->
                match operandType with
                | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
                | AST.TInt
                | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 -> Ok operandType
                | _ -> Error $"Bitwise operator requires integer operands, got {operandType}")
        | AST.Eq | AST.Neq -> Ok AST.TBool
        | AST.Lt | AST.Gt | AST.Lte | AST.Gte ->
            ensureSameType ()
            |> Result.bind (fun operandType ->
                match operandType with
                | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
                | AST.TInt
                | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
                | AST.TFloat64 -> Ok AST.TBool
                | _ -> Error $"Comparison operator requires numeric operands, got {operandType}")
        | AST.And | AST.Or -> Ok AST.TBool
        | AST.StringConcat -> Ok AST.TString
    | CheckedAST.UnaryOp (op, inner) ->
        inferTypeCore sumTypeNames inner typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
        |> Result.bind (fun innerType ->
            match op with
            | AST.Neg ->
                match innerType with
                | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64
                | AST.TInt
                | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
                | AST.TFloat64 -> Ok innerType
                | _ -> Error $"Negation requires numeric operand, got {innerType}"
            | AST.Not ->
                match innerType with
                | AST.TBool -> Ok AST.TBool
                | _ -> Error $"Logical not requires Bool operand, got {innerType}"
            | AST.BitNot ->
                match innerType with
                | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TInt -> Ok innerType
                | _ -> Error $"Bitwise not requires integer operand, got {innerType}")
    | CheckedAST.Match (scrutinee, cases) ->
        // Infer from first case body, but first extend environment with pattern variables
        // Infer scrutinee type to help with pattern variable typing
        let scrutineeTypeResult = inferTypeCore sumTypeNames scrutinee typeEnv typeReg variantLookup funcReg functionNames moduleRegistry

        let rec substituteType (subst: Map<string, AST.Type>) (typ: AST.Type) : AST.Type =
            match typ with
            | AST.TVar name -> Map.tryFind name subst |> Option.defaultValue typ
            | AST.TTuple elems -> AST.TTuple (List.map (substituteType subst) elems)
            | AST.TRecord (name, args) -> AST.TRecord (name, List.map (substituteType subst) args)
            | AST.TList elem -> AST.TList (substituteType subst elem)
            | AST.TDict (k, v) -> AST.TDict (substituteType subst k, substituteType subst v)
            | AST.TSum (name, args) -> AST.TSum (name, List.map (substituteType subst) args)
            | AST.TFunction (args, ret) -> AST.TFunction (List.map (substituteType subst) args, substituteType subst ret)
            | _ -> typ

        // Helper to extract pattern variable names and infer their types
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
                let tupleElemTypesOpt =
                    match scrutType with
                    | AST.TTuple elemTypes when List.length elemTypes = List.length innerPats ->
                        Some elemTypes
                    | AST.TVar tupleTypeVar ->
                        // Preserve unresolved tuple element types rather than dropping bindings
                        // or defaulting to a concrete numeric type.
                        innerPats
                        |> List.mapi (fun idx _ -> AST.TVar $"__tuple_elem_{tupleTypeVar}_{idx}")
                        |> Some
                    | AST.TRuntimeError ->
                        innerPats
                        |> List.mapi (fun idx _ -> AST.TVar $"__tuple_elem_runtime_error_{idx}")
                        |> Some
                    | _ ->
                        None

                match tupleElemTypesOpt with
                | Some elemTypes when List.length elemTypes = List.length innerPats ->
                    List.zip innerPats elemTypes
                    |> List.fold (fun acc (pat, typ) -> Map.fold (fun m k v -> Map.add k v m) acc (extractPatternBindings pat typ)) Map.empty
                | _ ->
                    // Non-matching tuple patterns must not introduce bindings with fabricated types.
                    // Type checking treats these as non-matching alternatives.
                    Map.empty
            | AST.PConstructor (variantName, fieldPatterns) ->
                match Map.tryFind variantName variantLookup with
                | Some (_, typeParams, _, fieldTypes)
                    when List.length fieldPatterns = List.length fieldTypes ->
                    let subst =
                        match scrutType with
                        | AST.TSum (_, typeArgs) when List.length typeParams = List.length typeArgs ->
                            List.zip typeParams typeArgs |> Map.ofList
                        | _ -> Map.empty
                    List.zip fieldPatterns fieldTypes
                    |> List.fold (fun acc (fieldPattern, fieldType) ->
                        Map.fold (fun current name typ -> Map.add name typ current)
                            acc
                            (extractPatternBindings fieldPattern (substituteType subst fieldType))) Map.empty
                | Some _ -> Map.empty
                | None -> Crash.crash $"Unknown constructor '{variantName}' in pattern"
            | AST.PResolvedConstructor (declaringType, _, tag, fieldPatterns) ->
                match tryFindVariantByTag declaringType tag variantLookup with
                | Some (_, typeParams, _, fieldTypes)
                    when List.length fieldPatterns = List.length fieldTypes ->
                    let subst =
                        match scrutType with
                        | AST.TSum (_, typeArgs) when List.length typeParams = List.length typeArgs ->
                            List.zip typeParams typeArgs |> Map.ofList
                        | _ -> Map.empty
                    List.zip fieldPatterns fieldTypes
                    |> List.fold (fun acc (fieldPattern, fieldType) ->
                        Map.fold
                            (fun current name typ -> Map.add name typ current)
                            acc
                            (extractPatternBindings fieldPattern (substituteType subst fieldType))) Map.empty
                | Some _ -> Map.empty
                | None -> Crash.crash $"Unknown resolved constructor tag '{tag}' for '{declaringType}'"
            | AST.PList innerPats ->
                let elemTypeOpt =
                    match scrutType with
                    | AST.TList t -> Some t
                    | AST.TVar _
                    | AST.TRuntimeError -> Some (AST.TVar "__list_elem_unknown")
                    | _ -> None
                match elemTypeOpt with
                | None ->
                    // Grouped alternatives may include impossible list branches (for example `0 | [_]`).
                    // Treat those as contributing no bindings rather than crashing.
                    Map.empty
                | Some elemType ->
                    innerPats
                    |> List.fold (fun acc pat -> Map.fold (fun m k v -> Map.add k v m) acc (extractPatternBindings pat elemType)) Map.empty
            | AST.PListCons (headPats, tailPat) ->
                let elemTypeOpt =
                    match scrutType with
                    | AST.TList t -> Some t
                    | AST.TVar _
                    | AST.TRuntimeError -> Some (AST.TVar "__list_elem_unknown")
                    | _ -> None
                match elemTypeOpt with
                | None ->
                    // Impossible list-cons alternatives must not fabricate bindings.
                    Map.empty
                | Some elemType ->
                    let headBindings =
                        headPats
                        |> List.fold (fun acc pat -> Map.fold (fun m k v -> Map.add k v m) acc (extractPatternBindings pat elemType)) Map.empty
                    let tailBindings = extractPatternBindings tailPat scrutType
                    Map.fold (fun m k v -> Map.add k v m) headBindings tailBindings

        let resolveCaseType (preferred: AST.Type) (other: AST.Type) : Result<AST.Type, string> =
            match matchTypePattern preferred other with
            | Error _ -> Error "Match case type mismatch"
            | Ok bindings ->
                match consolidateTypeBindings bindings with
                | Error e -> Error e
                | Ok subst -> Ok (applySubstToType subst preferred)

        let mergeCaseTypes (accType: AST.Type) (nextType: AST.Type) : Result<AST.Type, string> =
            if accType = nextType then
                Ok accType
            elif accType = AST.TRuntimeError then
                Ok nextType
            elif nextType = AST.TRuntimeError then
                Ok accType
            else
                match resolveCaseType accType nextType, resolveCaseType nextType accType with
                | Ok resolvedAcc, Ok resolvedNext ->
                    if containsTypeVar resolvedAcc && not (containsTypeVar resolvedNext) then
                        Ok resolvedNext
                    elif containsTypeVar resolvedNext && not (containsTypeVar resolvedAcc) then
                        Ok resolvedAcc
                    else
                        Ok resolvedAcc
                | Ok resolvedAcc, Error _ -> Ok resolvedAcc
                | Error _, Ok resolvedNext -> Ok resolvedNext
                | Error _, Error _ ->
                    Error
                        $"Match cases have incompatible types: {typeToString accType} vs {typeToString nextType}"

        let inferCaseType (patternType: AST.Type) (mc: CheckedAST.MatchCase) : Result<AST.Type, string> =
            let patBindings =
                mc.Patterns
                |> AST.NonEmptyList.toList
                |> List.fold (fun acc pat ->
                    matchPatternBindingTypes typeReg variantLookup pat patternType
                    |> Map.fold (fun current id typ -> Map.add id typ current) acc) Map.empty
            let typeEnv' = Map.fold (fun m k v -> Map.add k v m) typeEnv patBindings
            inferTypeCore sumTypeNames mc.Body typeEnv' typeReg variantLookup funcReg functionNames moduleRegistry

        let patternType =
            match scrutineeTypeResult with
            | Ok t -> t
            | Error msg -> Crash.crash $"Pattern match: Could not determine scrutinee type: {msg}"

        match cases with
        | [] -> Error "Empty match expression"
        | firstCase :: restCases ->
            inferCaseType patternType firstCase
            |> Result.bind (fun firstCaseType ->
                restCases
                |> List.fold
                    (fun accResult mc ->
                        accResult
                        |> Result.bind (fun accType ->
                            inferCaseType patternType mc
                            |> Result.bind (fun nextType -> mergeCaseTypes accType nextType)))
                    (Ok firstCaseType))
    | CheckedAST.Call (funcName, args) ->
        let argList = exprArgsToList args
        let displayName =
            Map.tryFind funcName funcReg
            |> Option.map fst
            |> Option.orElseWith (fun () -> Map.tryFind funcName functionNames)
            |> Option.orElseWith (fun () ->
                moduleRegistry
                |> Map.keys
                |> Seq.tryFind (fun name -> AST.functionIdForName name = funcName))
        if funcName = AST.functionIdForName "Builtin.unwrap" then
            match argList with
            | [argExpr] ->
                inferTypeCore sumTypeNames argExpr typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
                |> Result.bind (fun argType ->
                    match argType with
                    | AST.TSum ("Darklang.Stdlib.Option.Option", [valueType]) -> Ok valueType
                    | AST.TSum ("Darklang.Stdlib.Result.Result", [okType; _]) -> Ok okType
                    | AST.TSum ("Darklang.Stdlib.Option.Option", []) ->
                        match argExpr with
                        | CheckedAST.Constructor (reference, [payloadExpr])
                            when constructorReferenceMatches
                                "Darklang.Stdlib.Option.Option"
                                "Some"
                                reference
                                variantLookup ->
                            inferTypeCore sumTypeNames payloadExpr typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
                        | _ ->
                            // Type args may be unavailable in ANF inferType.
                            // Use Unit to avoid leaking unresolved type variables into later passes.
                            Ok AST.TUnit
                    | AST.TSum ("Darklang.Stdlib.Result.Result", []) ->
                        match argExpr with
                        | CheckedAST.Constructor (reference, [payloadExpr])
                            when constructorReferenceMatches
                                "Darklang.Stdlib.Result.Result"
                                "Ok"
                                reference
                                variantLookup ->
                            inferTypeCore sumTypeNames payloadExpr typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
                        | _ ->
                            // Type args may be unavailable in ANF inferType.
                            // Use Unit to avoid leaking unresolved type variables into later passes.
                            Ok AST.TUnit
                    | _ ->
                        Error $"Internal error: Builtin.unwrap expects Option/Result argument, got {typeToString argType}")
            | _ ->
                Error $"Internal error: Builtin.unwrap expects 1 argument, got {List.length argList}"
        elif funcName = AST.functionIdForName "Builtin.testRuntimeError"
             || funcName = AST.functionIdForName "Builtin.crash" then
            match argList with
            // Runtime errors are bottom-like: branch and match inference select
            // the type of the reachable value-producing alternatives.
            | [_] -> Ok AST.TRuntimeError
            | _ ->
                Error $"Internal error: runtime failure function expects 1 argument, got {List.length argList}"
        else
            // Look up function return type from the function registry
            match Map.tryFind funcName funcReg with
            | Some (_, AST.TFunction (_, returnType)) -> Ok returnType
            | Some (name, _) -> Error $"Expected function type for {name} in funcReg"
            | None ->
                // Check if it's a module function (e.g., Stdlib.File.exists)
                match displayName |> Option.bind (Stdlib.tryGetFunction moduleRegistry) with
                | Some (moduleFunc, _) -> Ok moduleFunc.ReturnType
                | None ->
                    // Check if it's a monomorphized intrinsic (e.g., __raw_get_i64)
                    // These are raw memory operations that work with 8-byte values
                    let functionId = funcName
                    let missingFunctionIdentity = Option.isNone displayName
                    let funcName =
                        displayName
                        |> Option.defaultValue
                            $"__missing_function_identity_{AST.functionIdValue funcName}"
                    if missingFunctionIdentity then
                        Error $"Function identity {AST.functionIdValue functionId} is absent from lowering registries"
                    elif funcName.StartsWith("__raw_get_") then
                        // Preserve the monomorphized return type; defaulting to Int64 can
                        // incorrectly mark pattern-match branches as impossible.
                        let suffix = funcName.Substring("__raw_get_".Length)
                        tryParseMangledTypeWithSumTypeNames sumTypeNames suffix
                    elif funcName.StartsWith("__raw_take_") then
                        let suffix = funcName.Substring("__raw_take_".Length)
                        tryParseMangledTypeWithSumTypeNames sumTypeNames suffix
                    elif funcName.StartsWith("__stream_to_rawptr_") then
                        Ok AST.TRawPtr
                    elif funcName.StartsWith("__rawptr_to_stream_") then
                        let suffix = funcName.Substring("__rawptr_to_stream_".Length)
                        tryParseMangledTypeWithSumTypeNames sumTypeNames suffix |> Result.map AST.TStream
                    elif funcName.StartsWith("__raw_slot_init_") then
                        // __raw_slot_init<T> returns Unit
                        Ok AST.TUnit
                    // Key intrinsics for Dict - monomorphized versions
                    elif funcName.StartsWith("__hash_") then
                        // __hash<k> returns Int64 (hash value)
                        Ok AST.TInt64
                    elif funcName.StartsWith("__key_eq_") then
                        // __key_eq<k> returns Bool (equality check)
                        Ok AST.TBool
                    // Dict intrinsics - monomorphized versions
                    elif funcName.StartsWith("__empty_dict_") then
                        // __empty_dict<k, v> returns Dict<k, v> - but at ANF level it's Int64 (null ptr)
                        Ok AST.TInt64
                    elif funcName.StartsWith("__dict_is_null_") then
                        // __dict_is_null<k, v> returns Bool
                        Ok AST.TBool
                    elif funcName.StartsWith("__dict_get_tag_") then
                        // __dict_get_tag<k, v> returns Int64 (tag bits)
                        Ok AST.TInt64
                    elif funcName.StartsWith("__dict_to_rawptr_") then
                        // __dict_to_rawptr<k, v> returns RawPtr
                        Ok AST.TRawPtr
                    elif funcName.StartsWith("__rawptr_to_dict_") then
                        // __rawptr_to_dict<k, v> returns Dict<k, v>
                        let suffix = funcName.Substring("__rawptr_to_dict_".Length)
                        tryParseMangledTypeWithSumTypeNames sumTypeNames $"dict_{suffix}"
                    // List intrinsics - monomorphized versions for the skew list.
                    elif funcName.StartsWith("__list_is_null_") then
                        // __list_is_null<a> returns Bool
                        Ok AST.TBool
                    elif funcName.StartsWith("__list_get_tag_") then
                        // __list_get_tag<a> returns Int64 (tag bits)
                        Ok AST.TInt64
                    elif funcName.StartsWith("__list_to_rawptr_") then
                        // __list_to_rawptr<a> returns RawPtr
                        Ok AST.TRawPtr
                    elif funcName.StartsWith("__rawptr_to_list_") then
                        // __rawptr_to_list<a> returns List<a> - parse element type from mangled name
                        let suffix = funcName.Substring("__rawptr_to_list_".Length)
                        tryParseMangledTypeWithSumTypeNames sumTypeNames suffix
                        |> Result.map AST.TList
                    elif funcName.StartsWith("__list_empty_") then
                        // Preserve the semantic list type for match/type inference.
                        let suffix = funcName.Substring("__list_empty_".Length)
                        tryParseMangledTypeWithSumTypeNames sumTypeNames suffix
                        |> Result.map AST.TList
                    else
                        Error $"Unknown function: '{funcName}'"
    | CheckedAST.TypeApp (_funcName, _typeArgs, _args) ->
        // Generic function call - not yet implemented
        Error "Generic function calls not yet implemented"
    | CheckedAST.Lambda (parameters, returnAnnotation, body) ->
        // Lambda has function type (paramTypes) -> returnType
        let parameterList = parameters |> AST.NonEmptyList.toList
        let paramTypes = parameterList |> List.map lambdaParameterType
        let typeEnv' =
            parameterList
            |> List.collect lambdaParameterBindings
            |> List.fold (fun env (name, ty) -> Map.add name ty env) typeEnv
        inferTypeCore sumTypeNames body typeEnv' typeReg variantLookup funcReg functionNames moduleRegistry
        |> Result.map (fun returnType -> AST.TFunction (paramTypes, returnType))
    | CheckedAST.Apply (func, _args) ->
        // Apply result is the return type of the function
        inferTypeCore sumTypeNames func typeEnv typeReg variantLookup funcReg functionNames moduleRegistry
        |> Result.bind (fun funcType ->
            match funcType with
            | AST.TFunction (_, returnType) -> Ok returnType
            | _ -> Error "Apply requires a function type")
    | CheckedAST.IndirectApply _ -> Ok AST.TBool
    | CheckedAST.FuncRef name ->
        // Function reference has the function's type
        match Map.tryFind name funcReg with
        | Some (_, returnType) -> Ok returnType
        | None -> Error "Cannot infer type: undefined function identity"
    | CheckedAST.Closure (funcName, _) ->
        // Closure has function type (without the closure param)
        match Map.tryFind funcName funcReg with
        | Some (_, AST.TFunction (_ :: restParams, returnType)) ->
            Ok (AST.TFunction (restParams, returnType))
        | Some (_, funcType) -> Ok funcType
        | None -> Error "Cannot infer type: undefined closure function identity"
    | CheckedAST.InterpolatedString _ ->
        // Interpolated strings are always String type
        Ok AST.TString

/// Build the final direct-payload skew-list forest for a list literal.
/// Element expressions have already been evaluated into atoms in source order.
