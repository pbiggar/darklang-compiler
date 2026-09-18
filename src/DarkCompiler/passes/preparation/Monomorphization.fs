// Monomorphization.fs - Solve reachable generic instances and replace type applications.

module Monomorphization

open MemoryModel
open ANF
open LoweringPrimitives
open SpecializationIdentity
open TypeSubstitution

let private hasPredefinedKeyIntrinsic (typ: AST.Type) : bool =
    match typ with
    | AST.TInt64 | AST.TBool | AST.TString | AST.TBlob -> true
    | _ -> false

let collectTypeApps (expr: CheckedAST.Expr) : Set<SpecKey> =
    let rec visit (specs: Set<SpecKey>) (current: CheckedAST.Expr) : Set<SpecKey> =
        match current with
        | CheckedAST.BoundaryRender (_, value)
        | CheckedAST.UnaryOp (_, value)
        | CheckedAST.TupleAccess (value, _)
        | CheckedAST.RecordAccess (value, _)
        | CheckedAST.Lambda (_, _, value) ->
            visit specs value
        | CheckedAST.UnitLiteral | CheckedAST.Int64Literal _ | CheckedAST.Int128Literal _ | CheckedAST.BigIntLiteral _
        | CheckedAST.Int8Literal _ | CheckedAST.Int16Literal _ | CheckedAST.Int32Literal _
        | CheckedAST.UInt8Literal _ | CheckedAST.UInt16Literal _ | CheckedAST.UInt32Literal _
        | CheckedAST.UInt64Literal _ | CheckedAST.UInt128Literal _ | CheckedAST.BoolLiteral _
        | CheckedAST.StringLiteral _ | CheckedAST.CharLiteral _ | CheckedAST.FloatLiteral _
        | CheckedAST.Local _ | CheckedAST.NamedValue _ | CheckedAST.FuncRef _ | CheckedAST.Closure _ | CheckedAST.RuntimeError _ ->
            specs
        | CheckedAST.BinOp (_, left, right)
        | CheckedAST.Let (_, left, right)
        | CheckedAST.RecursiveLet (_, left, right)
        | CheckedAST.Sequence (left, right) ->
            visit (visit specs left) right
        | CheckedAST.If (condition, thenBranch, elseBranch) ->
            visit (visit (visit specs condition) thenBranch) elseBranch
        | CheckedAST.Call (_, args) ->
            visitMany specs (exprArgsToList args)
        | CheckedAST.TypeApp (funcName, typeArgs, args) ->
            let argSpecs = visitMany specs (exprArgsToList args)
            let hasTypeVars = List.exists containsTypeVar typeArgs
            if funcName = eqHelperDispatchMarker || funcName = "__compare" then
                argSpecs
            elif isGenericKeyIntrinsicName funcName then
                match typeArgs with
                | [keyType] when not hasTypeVars && hasPredefinedKeyIntrinsic keyType ->
                    Set.add (funcName, typeArgs) argSpecs
                | _ -> argSpecs
            elif (funcName = "Darklang.Stdlib.Dict.fromList" || funcName = "Dict.fromList")
                 && exprArgsToList args = [CheckedAST.ListLiteral []]
                 && not hasTypeVars then
                // Optimization: avoid building a Dict from an empty list when types are concrete.
                Set.add ("Darklang.Stdlib.Dict.empty", typeArgs) argSpecs
            else
                Set.add (funcName, typeArgs) argSpecs
        | CheckedAST.TupleLiteral elements
        | CheckedAST.ListLiteral elements ->
            visitMany specs elements
        | CheckedAST.DictLiteral (keyType, valueType, entries) ->
            let entrySpecs =
                entries |> List.fold (fun acc (key, value) -> visit (visit acc key) value) specs
            if List.isEmpty entries then entrySpecs
            else
                Set.add
                    ("Darklang.Stdlib.Dict.__setOverwriting", [keyType; valueType])
                    entrySpecs
        | CheckedAST.RecordLiteral (_, fields) ->
            fields |> List.fold (fun acc (_, value) -> visit acc value) specs
        | CheckedAST.RecordUpdate (record, updates) ->
            updates
            |> List.fold (fun acc (_, value) -> visit acc value) (visit specs record)
        | CheckedAST.Constructor (_, _, fields) ->
            fields |> List.fold visit specs
        | CheckedAST.Match (scrutinee, cases) ->
            cases
            |> List.fold (fun acc case ->
                let acc = case.Guard |> Option.map (visit acc) |> Option.defaultValue acc
                visit acc case.Body) (visit specs scrutinee)
        | CheckedAST.Apply (func, args)
        | CheckedAST.IndirectApply (func, args) ->
            visitMany (visit specs func) (exprArgsToList args)
        | CheckedAST.InterpolatedString parts ->
            parts
            |> List.fold (fun acc part ->
                match part with
                | CheckedAST.StringText _ -> acc
                | CheckedAST.StringExpr value -> visit acc value) specs

    and visitMany specs expressions =
        expressions |> List.fold visit specs

    visit Set.empty expr

/// Collect TypeApps from a function definition
let collectTypeAppsFromFunc (funcDef: CheckedAST.FunctionDef) : Set<SpecKey> =
    collectTypeApps funcDef.Body

/// Collect canonical call targets without interpreting their names. The AOT
/// catalog bridge uses this typed traversal to materialize only lookup
/// primitives that are reachable from the current compilation.
let rec collectCalledFunctions (expr: CheckedAST.Expr) : Set<string> =
    let combine expressions =
        expressions
        |> List.map collectCalledFunctions
        |> List.fold Set.union Set.empty

    match expr with
    | CheckedAST.UnitLiteral | CheckedAST.Int64Literal _ | CheckedAST.Int128Literal _ | CheckedAST.BigIntLiteral _
    | CheckedAST.Int8Literal _ | CheckedAST.Int16Literal _ | CheckedAST.Int32Literal _
    | CheckedAST.UInt8Literal _ | CheckedAST.UInt16Literal _ | CheckedAST.UInt32Literal _
    | CheckedAST.UInt64Literal _ | CheckedAST.UInt128Literal _ | CheckedAST.BoolLiteral _
    | CheckedAST.StringLiteral _ | CheckedAST.CharLiteral _ | CheckedAST.FloatLiteral _
    | CheckedAST.Local _ | CheckedAST.NamedValue _ | CheckedAST.FuncRef _ | CheckedAST.RuntimeError _ -> Set.empty
    | CheckedAST.BoundaryRender (_, value)
    | CheckedAST.UnaryOp (_, value)
    | CheckedAST.TupleAccess (value, _)
    | CheckedAST.RecordAccess (value, _) -> collectCalledFunctions value
    | CheckedAST.BinOp (_, left, right)
    | CheckedAST.Sequence (left, right) -> combine [left; right]
    | CheckedAST.Let (_, value, body) -> combine [value; body]
    | CheckedAST.RecursiveLet (_, value, body) -> combine [value; body]
    | CheckedAST.If (condition, thenBranch, elseBranch) ->
        combine [condition; thenBranch; elseBranch]
    | CheckedAST.Call (name, args)
    | CheckedAST.TypeApp (name, _, args) ->
        Set.add name (combine (exprArgsToList args))
    | CheckedAST.TupleLiteral elements
    | CheckedAST.ListLiteral elements -> combine elements
    | CheckedAST.DictLiteral (_, _, entries) ->
        entries |> List.collect (fun (key, value) -> [key; value]) |> combine
    | CheckedAST.RecordLiteral (_, fields) -> fields |> List.map snd |> combine
    | CheckedAST.RecordUpdate (record, fields) ->
        combine (record :: (fields |> List.map snd))
    | CheckedAST.Constructor (_, _, fields) ->
        fields |> List.map collectCalledFunctions |> List.fold Set.union Set.empty
    | CheckedAST.Match (scrutinee, cases) ->
        let caseCalls =
            cases
            |> List.collect (fun case ->
                case.Body :: (case.Guard |> Option.toList))
            |> combine
        Set.union (collectCalledFunctions scrutinee) caseCalls
    | CheckedAST.Lambda (_, _, body) -> collectCalledFunctions body
    | CheckedAST.Apply (func, args)
    | CheckedAST.IndirectApply (func, args) ->
        combine (func :: exprArgsToList args)
    | CheckedAST.Closure (name, captures) -> Set.add name (combine captures)
    | CheckedAST.InterpolatedString parts ->
        parts
        |> List.choose (function CheckedAST.StringExpr value -> Some value | CheckedAST.StringText _ -> None)
        |> combine

/// Specialize only the requested generic specs, returning new functions and a registry
let specializeFromSpecs (genericFuncDefs: GenericFuncDefs) (initialSpecs: Set<SpecKey>) : SpecializationResult =
    let rec iterate
        (pendingSpecs: Set<SpecKey>)
        (processedSpecs: Set<SpecKey>)
        (accFuncs: GenericFunctionArtifact list)
        (specRegistry: SpecRegistry)
        (externalSpecs: Set<SpecKey>)
        : SpecializationResult =
        let newSpecs = Set.difference pendingSpecs processedSpecs
        if Set.isEmpty newSpecs then
            { SpecializedFuncs = accFuncs
              SpecRegistry = specRegistry
              ExternalSpecs = externalSpecs }
        else
            let (newFuncs, newPendingSpecs, newRegistry, newExternal) =
                newSpecs
                |> Set.toList
                |> List.fold
                    (fun (funcs, pending, registry, external) (funcName, typeArgs) ->
                        match Map.tryFind funcName genericFuncDefs with
                        | Some artifact ->
                            let specialized = specializeFunction artifact.Function typeArgs
                            let specializedArtifact = { artifact with Function = specialized }
                            let registry' = Map.add (funcName, typeArgs) specialized.Name registry
                            let bodySpecs = collectTypeAppsFromFunc specialized
                            (specializedArtifact :: funcs, Set.union pending bodySpecs, registry', external)
                        | None ->
                            (funcs, pending, registry, Set.add (funcName, typeArgs) external))
                    ([], Set.empty, specRegistry, externalSpecs)

            iterate
                newPendingSpecs
                (Set.union processedSpecs newSpecs)
                (newFuncs @ accFuncs)
                newRegistry
                newExternal

    iterate initialSpecs Set.empty [] Map.empty Set.empty

/// Replace TypeApp with Call using specialized name in an expression
let rec replaceTypeApps (expr: CheckedAST.Expr) : CheckedAST.Expr =
    match expr with
    | CheckedAST.UnitLiteral | CheckedAST.Int64Literal _ | CheckedAST.Int128Literal _ | CheckedAST.BigIntLiteral _ | CheckedAST.Int8Literal _ | CheckedAST.Int16Literal _ | CheckedAST.Int32Literal _
    | CheckedAST.UInt8Literal _ | CheckedAST.UInt16Literal _ | CheckedAST.UInt32Literal _ | CheckedAST.UInt64Literal _ | CheckedAST.UInt128Literal _
    | CheckedAST.BoolLiteral _ | CheckedAST.StringLiteral _ | CheckedAST.CharLiteral _ | CheckedAST.FloatLiteral _
    | CheckedAST.Local _ | CheckedAST.NamedValue _ | CheckedAST.FuncRef _ | CheckedAST.Closure _ | CheckedAST.RuntimeError _ ->
        expr
    | CheckedAST.BoundaryRender (renderer, value) ->
        CheckedAST.BoundaryRender (renderer, replaceTypeApps value)
    | CheckedAST.BinOp (op, left, right) ->
        CheckedAST.BinOp (op, replaceTypeApps left, replaceTypeApps right)
    | CheckedAST.UnaryOp (op, inner) ->
        CheckedAST.UnaryOp (op, replaceTypeApps inner)
    | CheckedAST.Let (pattern, value, body) ->
        CheckedAST.Let (pattern, replaceTypeApps value, replaceTypeApps body)
    | CheckedAST.RecursiveLet (recursion, value, body) ->
        CheckedAST.RecursiveLet (recursion, replaceTypeApps value, replaceTypeApps body)
    | CheckedAST.If (cond, thenBranch, elseBranch) ->
        CheckedAST.If (replaceTypeApps cond, replaceTypeApps thenBranch, replaceTypeApps elseBranch)
    | CheckedAST.Sequence (first, next) ->
        CheckedAST.Sequence (replaceTypeApps first, replaceTypeApps next)
    | CheckedAST.Call (funcName, args) ->
        CheckedAST.Call (funcName, AST.NonEmptyList.map replaceTypeApps args)
    | CheckedAST.TypeApp (funcName, typeArgs, args) ->
        // Replace with a regular Call to the specialized name
        let hasTypeVars = List.exists containsTypeVar typeArgs
        if funcName = eqHelperDispatchMarker then
            match typeArgs with
            | [targetType] when not hasTypeVars ->
                materializeComparisonPlan
                    targetType
                    (args |> exprArgsToList |> List.map replaceTypeApps)
                |> replaceTypeApps
            | _ ->
                match args |> exprArgsToList |> List.map replaceTypeApps with
                | [leftExpr; rightExpr] -> CheckedAST.BinOp (AST.Eq, leftExpr, rightExpr)
                | _ -> Crash.crash "Comparison plan expected exactly two operands"
        elif funcName = "__compare" then
            let replacedArgs = args |> exprArgsToList |> List.map replaceTypeApps
            match typeArgs, replacedArgs with
            | [targetType], [leftExpr; rightExpr] when not hasTypeVars ->
                CheckedAST.Call (
                    ComparisonPlanning.compareHelperName targetType,
                    exprArgsFromList [leftExpr; rightExpr]
                )
            | _, evaluatedArgs ->
                wrapWithIgnoredArgEvaluations
                    evaluatedArgs
                    (CheckedAST.RuntimeError "Canonical comparison remained polymorphic after monomorphization")
        elif (funcName = "Darklang.Stdlib.Dict.fromList" || funcName = "Dict.fromList")
           && exprArgsToList args = [CheckedAST.ListLiteral []]
           && not hasTypeVars then
            // Optimization: avoid building a Dict from an empty list when types are concrete.
            let specializedName = specName "Darklang.Stdlib.Dict.empty" typeArgs
            CheckedAST.Call (specializedName, exprArgsFromList [])
        elif isGenericKeyIntrinsicName funcName && hasTypeVars then
            let replacedArgs = args |> exprArgsToList |> List.map replaceTypeApps
            wrapWithIgnoredArgEvaluations replacedArgs (unresolvedKeyIntrinsicTypeArgErrorExpr funcName)
        elif isGenericKeyIntrinsicName funcName then
            let replacedArgs = args |> exprArgsToList |> List.map replaceTypeApps
            match funcName, typeArgs with
            | "__hash", [keyType] when hasPredefinedKeyIntrinsic keyType ->
                CheckedAST.Call (specName funcName typeArgs, exprArgsFromList replacedArgs)
            | "__hash", [_] ->
                wrapWithIgnoredArgEvaluations replacedArgs (CheckedAST.Int64Literal 0L)
            | "__key_eq", [keyType] when hasPredefinedKeyIntrinsic keyType ->
                CheckedAST.Call (specName funcName typeArgs, exprArgsFromList replacedArgs)
            | "__key_eq", [keyType] ->
                materializeComparisonPlan keyType replacedArgs |> replaceTypeApps
            | _ -> Crash.crash $"Invalid generic key intrinsic application: {funcName}"
        else
            let specializedName = specName funcName typeArgs
            CheckedAST.Call (specializedName, AST.NonEmptyList.map replaceTypeApps args)
    | CheckedAST.TupleLiteral elements ->
        CheckedAST.TupleLiteral (List.map replaceTypeApps elements)
    | CheckedAST.TupleAccess (tuple, index) ->
        CheckedAST.TupleAccess (replaceTypeApps tuple, index)
    | CheckedAST.DictLiteral (keyType, valueType, entries) ->
        match entries with
        | [] -> expr
        | _ ->
            let empty = CheckedAST.DictLiteral (keyType, valueType, [])
            entries
            |> List.fold (fun dictExpr (key, value) ->
                CheckedAST.TypeApp (
                    "Darklang.Stdlib.Dict.__setOverwriting",
                    [keyType; valueType],
                    AST.NonEmptyList.fromList [dictExpr; key; value]
                )) empty
            |> replaceTypeApps
    | CheckedAST.RecordLiteral (typeName, fields) ->
        CheckedAST.RecordLiteral (typeName, List.map (fun (n, e) -> (n, replaceTypeApps e)) fields)
    | CheckedAST.RecordUpdate (record, updates) ->
        CheckedAST.RecordUpdate (replaceTypeApps record, List.map (fun (n, e) -> (n, replaceTypeApps e)) updates)
    | CheckedAST.RecordAccess (record, fieldName) ->
        CheckedAST.RecordAccess (replaceTypeApps record, fieldName)
    | CheckedAST.Constructor (typeName, variantName, fields) ->
        CheckedAST.Constructor (typeName, variantName, List.map replaceTypeApps fields)
    | CheckedAST.Match (scrutinee, cases) ->
        CheckedAST.Match (replaceTypeApps scrutinee,
                   cases |> List.map (fun mc -> { mc with Guard = mc.Guard |> Option.map replaceTypeApps; Body = replaceTypeApps mc.Body }))
    | CheckedAST.ListLiteral elements ->
        CheckedAST.ListLiteral (List.map replaceTypeApps elements)
    | CheckedAST.Lambda (parameters, returnAnnotation, body) ->
        CheckedAST.Lambda (parameters, returnAnnotation, replaceTypeApps body)
    | CheckedAST.Apply (func, args) ->
        CheckedAST.Apply (replaceTypeApps func, AST.NonEmptyList.map replaceTypeApps args)
    | CheckedAST.IndirectApply (func, args) ->
        CheckedAST.IndirectApply (replaceTypeApps func, AST.NonEmptyList.map replaceTypeApps args)
    | CheckedAST.InterpolatedString parts ->
        let replacePart part =
            match part with
            | CheckedAST.StringText s -> CheckedAST.StringText s
            | CheckedAST.StringExpr e -> CheckedAST.StringExpr (replaceTypeApps e)
        CheckedAST.InterpolatedString (List.map replacePart parts)

let private isIntrinsicTypeAppName (funcName: string) : bool =
    match funcName with
    | "__raw_get"
    | "__raw_take"
    | "__raw_slot_init"
    | "__stream_to_rawptr"
    | "__rawptr_to_stream"
    | "__empty_dict"
    | "__dict_is_null"
    | "__dict_get_tag"
    | "__dict_to_rawptr"
    | "__rawptr_to_dict"
    | "__list_empty"
    | "__list_is_null"
    | "__list_get_tag"
    | "__list_to_rawptr"
    | "__rawptr_to_list" -> true
    | "Builtin.pmEvaluateValue" -> true
    | _ -> false

let private missingSpecMessage (funcName: string) (typeArgs: AST.Type list) : string =
    let typeArgText =
        typeArgs
        |> List.map typeToMangledName
        |> String.concat ", "
    $"Missing specialization for {funcName}<{typeArgText}>"

/// Replace TypeApp with Call using a precomputed specialization registry
let replaceTypeAppsWithRegistry (specRegistry: SpecRegistry) (expr: CheckedAST.Expr) : Result<CheckedAST.Expr, string> =
    let rec mapResult (f: 'a -> Result<'b, string>) (items: 'a list) : Result<'b list, string> =
        match items with
        | [] -> Ok []
        | x :: xs ->
            f x
            |> Result.bind (fun x' ->
                mapResult f xs
                |> Result.map (fun xs' -> x' :: xs'))

    let rec replace (expr': CheckedAST.Expr) : Result<CheckedAST.Expr, string> =
        match expr' with
        | CheckedAST.UnitLiteral
        | CheckedAST.Int64Literal _ | CheckedAST.Int128Literal _ | CheckedAST.BigIntLiteral _
        | CheckedAST.Int8Literal _
        | CheckedAST.Int16Literal _
        | CheckedAST.Int32Literal _
        | CheckedAST.UInt8Literal _
        | CheckedAST.UInt16Literal _
        | CheckedAST.UInt32Literal _
        | CheckedAST.UInt64Literal _ | CheckedAST.UInt128Literal _
        | CheckedAST.BoolLiteral _
        | CheckedAST.StringLiteral _
        | CheckedAST.CharLiteral _
        | CheckedAST.FloatLiteral _
        | CheckedAST.Local _ | CheckedAST.NamedValue _
        | CheckedAST.FuncRef _
        | CheckedAST.Closure _
        | CheckedAST.RuntimeError _ -> Ok expr'
        | CheckedAST.BoundaryRender (renderer, value) ->
            replace value |> Result.map (fun value' -> CheckedAST.BoundaryRender (renderer, value'))
        | CheckedAST.BinOp (op, left, right) ->
            replace left
            |> Result.bind (fun left' ->
                replace right
                |> Result.map (fun right' -> CheckedAST.BinOp (op, left', right')))
        | CheckedAST.UnaryOp (op, inner) ->
            replace inner |> Result.map (fun inner' -> CheckedAST.UnaryOp (op, inner'))
        | CheckedAST.Let (pattern, value, body) ->
            replace value
            |> Result.bind (fun value' ->
                replace body |> Result.map (fun body' -> CheckedAST.Let (pattern, value', body')))
        | CheckedAST.RecursiveLet (recursion, value, body) ->
            replace value
            |> Result.bind (fun value' ->
                replace body |> Result.map (fun body' -> CheckedAST.RecursiveLet (recursion, value', body')))
        | CheckedAST.If (cond, thenBranch, elseBranch) ->
            replace cond
            |> Result.bind (fun cond' ->
                replace thenBranch
                |> Result.bind (fun thenBranch' ->
                    replace elseBranch
                    |> Result.map (fun elseBranch' -> CheckedAST.If (cond', thenBranch', elseBranch'))))
        | CheckedAST.Sequence (first, next) ->
            replace first
            |> Result.bind (fun first' ->
                replace next
                |> Result.map (fun next' -> CheckedAST.Sequence (first', next')))
        | CheckedAST.Call (funcName, args) ->
            mapResult replace (exprArgsToList args)
            |> Result.map (fun args' -> CheckedAST.Call (funcName, exprArgsFromList args'))
        | CheckedAST.TypeApp (funcName, typeArgs, args) ->
            let hasTypeVars = List.exists containsTypeVar typeArgs
            let emptyDictSpec = (funcName = "Darklang.Stdlib.Dict.fromList" || funcName = "Dict.fromList")
                                && exprArgsToList args = [CheckedAST.ListLiteral []]
                                && not hasTypeVars
            let unresolvedKeyIntrinsicSpec = isGenericKeyIntrinsicName funcName && hasTypeVars
            let resolvedNameResult =
                if funcName = eqHelperDispatchMarker then
                    match typeArgs with
                    | [targetType] when not hasTypeVars -> Ok eqHelperDispatchMarker
                    | _ -> Error "Comparison helper remained polymorphic after monomorphization"
                elif funcName = "__compare" then
                    match typeArgs with
                    | [targetType] when not hasTypeVars -> Ok (ComparisonPlanning.compareHelperName targetType)
                    | _ -> Error "Canonical comparison remained polymorphic after monomorphization"
                elif isGenericKeyIntrinsicName funcName then
                    Ok (specName funcName typeArgs)
                elif isIntrinsicTypeAppName funcName then
                    Ok (specName funcName typeArgs)
                elif emptyDictSpec then
                    let key = ("Darklang.Stdlib.Dict.empty", typeArgs)
                    match Map.tryFind key specRegistry with
                    | Some name -> Ok name
                    | None -> Error (missingSpecMessage "Darklang.Stdlib.Dict.empty" typeArgs)
                else
                    match Map.tryFind (funcName, typeArgs) specRegistry with
                    | Some name -> Ok name
                    | None -> Error (missingSpecMessage funcName typeArgs)

            if unresolvedKeyIntrinsicSpec then
                mapResult replace (exprArgsToList args)
                |> Result.map (fun args' ->
                    wrapWithIgnoredArgEvaluations args' (unresolvedKeyIntrinsicTypeArgErrorExpr funcName))
            elif isGenericKeyIntrinsicName funcName then
                mapResult replace (exprArgsToList args)
                |> Result.bind (fun args' ->
                    match funcName, typeArgs with
                    | "__hash", [keyType] when hasPredefinedKeyIntrinsic keyType ->
                        Ok (CheckedAST.Call (specName funcName typeArgs, exprArgsFromList args'))
                    | "__hash", [_] ->
                        Ok (wrapWithIgnoredArgEvaluations args' (CheckedAST.Int64Literal 0L))
                    | "__key_eq", [keyType] when hasPredefinedKeyIntrinsic keyType ->
                        Ok (CheckedAST.Call (specName funcName typeArgs, exprArgsFromList args'))
                    | "__key_eq", [keyType] ->
                        replace (materializeComparisonPlan keyType args')
                    | _ -> Error $"Invalid generic key intrinsic application: {funcName}")
            elif (funcName = eqHelperDispatchMarker || funcName = "__compare") && hasTypeVars then
                // The original generic template remains in the combined
                // preamble alongside its callable concrete specializations.
                // Concrete copies have already received substituted plans;
                // lower only this unreachable template to a compilable form.
                mapResult replace (exprArgsToList args)
                |> Result.map (fun args' ->
                    if funcName = eqHelperDispatchMarker then
                        match args' with
                        | [leftExpr; rightExpr] -> CheckedAST.BinOp (AST.Eq, leftExpr, rightExpr)
                        | _ -> Crash.crash "Comparison plan expected exactly two operands"
                    else
                        wrapWithIgnoredArgEvaluations
                            args'
                            (CheckedAST.RuntimeError "Canonical comparison remained polymorphic after monomorphization"))
            else
                if funcName = eqHelperDispatchMarker && not hasTypeVars then
                    mapResult replace (exprArgsToList args)
                    |> Result.bind (fun args' ->
                        replace (materializeComparisonPlan (List.head typeArgs) args'))
                else
                    resolvedNameResult
                    |> Result.bind (fun resolvedName ->
                        if emptyDictSpec then
                            Ok (CheckedAST.Call (resolvedName, exprArgsFromList []))
                        else
                            mapResult replace (exprArgsToList args)
                            |> Result.map (fun args' -> CheckedAST.Call (resolvedName, exprArgsFromList args')))
        | CheckedAST.TupleLiteral elements ->
            mapResult replace elements
            |> Result.map CheckedAST.TupleLiteral
        | CheckedAST.TupleAccess (tuple, index) ->
            replace tuple |> Result.map (fun tuple' -> CheckedAST.TupleAccess (tuple', index))
        | CheckedAST.DictLiteral (keyType, valueType, entries) ->
            match entries with
            | [] -> Ok expr'
            | _ ->
                let lowered =
                    entries
                    |> List.fold (fun dictExpr (key, value) ->
                        CheckedAST.TypeApp (
                            "Darklang.Stdlib.Dict.__setOverwriting",
                            [keyType; valueType],
                            AST.NonEmptyList.fromList [dictExpr; key; value]
                        )) (CheckedAST.DictLiteral (keyType, valueType, []))
                replace lowered
        | CheckedAST.RecordLiteral (typeName, fields) ->
            fields
            |> mapResult (fun (name, value) ->
                replace value |> Result.map (fun value' -> (name, value')))
            |> Result.map (fun fields' -> CheckedAST.RecordLiteral (typeName, fields'))
        | CheckedAST.RecordUpdate (record, updates) ->
            replace record
            |> Result.bind (fun record' ->
                updates
                |> mapResult (fun (name, value) ->
                    replace value |> Result.map (fun value' -> (name, value')))
                |> Result.map (fun updates' -> CheckedAST.RecordUpdate (record', updates')))
        | CheckedAST.RecordAccess (record, fieldName) ->
            replace record |> Result.map (fun record' -> CheckedAST.RecordAccess (record', fieldName))
        | CheckedAST.Constructor (typeName, variantName, fields) ->
            fields
            |> mapResult replace
            |> Result.map (fun fields' -> CheckedAST.Constructor (typeName, variantName, fields'))
        | CheckedAST.Match (scrutinee, cases) ->
            replace scrutinee
            |> Result.bind (fun scrutinee' ->
                cases
                |> mapResult (fun mc ->
                    let guardResult =
                        match mc.Guard with
                        | None -> Ok None
                        | Some guardExpr -> replace guardExpr |> Result.map Some
                    guardResult
                    |> Result.bind (fun guard' ->
                        replace mc.Body
                        |> Result.map (fun body' -> { mc with Guard = guard'; Body = body' })))
                |> Result.map (fun cases' -> CheckedAST.Match (scrutinee', cases')))
        | CheckedAST.ListLiteral elements ->
            mapResult replace elements |> Result.map CheckedAST.ListLiteral
        | CheckedAST.Lambda (parameters, returnAnnotation, body) ->
            replace body |> Result.map (fun body' -> CheckedAST.Lambda (parameters, returnAnnotation, body'))
        | CheckedAST.Apply (func, args) ->
            replace func
            |> Result.bind (fun func' ->
                mapResult replace (exprArgsToList args)
                |> Result.map (fun args' -> CheckedAST.Apply (func', exprArgsFromList args')))
        | CheckedAST.IndirectApply (func, args) ->
            replace func
            |> Result.bind (fun func' ->
                mapResult replace (exprArgsToList args)
                |> Result.map (fun args' -> CheckedAST.IndirectApply (func', exprArgsFromList args')))
        | CheckedAST.InterpolatedString parts ->
            parts
            |> mapResult (function
                | CheckedAST.StringText s -> Ok (CheckedAST.StringText s)
                | CheckedAST.StringExpr e -> replace e |> Result.map CheckedAST.StringExpr)
            |> Result.map CheckedAST.InterpolatedString

    replace expr

/// Replace TypeApp with Call in a function definition
let replaceTypeAppsInFunc (funcDef: CheckedAST.FunctionDef) : CheckedAST.FunctionDef =
    { funcDef with Body = replaceTypeApps funcDef.Body }

/// Replace TypeApp with Call in a function definition using a registry
let replaceTypeAppsInFuncWithRegistry (specRegistry: SpecRegistry) (funcDef: CheckedAST.FunctionDef) : Result<CheckedAST.FunctionDef, string> =
    replaceTypeAppsWithRegistry specRegistry funcDef.Body
    |> Result.map (fun body' -> { funcDef with Body = body' })

let private materializeFunctionComparisons (program: CheckedAST.Program) : CheckedAST.Program =
    let rec rewriteList symbols expressions =
        expressions
        |> List.mapFold (fun symbols expression ->
            let expression, symbols = rewrite symbols expression
            (expression, symbols)) symbols
    and rewrite symbols expression =
        let rewriteArgs symbols args =
            let values, symbols = rewriteList symbols (exprArgsToList args)
            (exprArgsFromList values, symbols)
        match expression with
        | CheckedAST.BoundaryRender (renderer, value) ->
            let value, symbols = rewrite symbols value
            (CheckedAST.BoundaryRender (renderer, value), symbols)
        | CheckedAST.BinOp (op, left, right) ->
            let left, symbols = rewrite symbols left
            let right, symbols = rewrite symbols right
            (CheckedAST.BinOp (op, left, right), symbols)
        | CheckedAST.UnaryOp (op, value) ->
            let value, symbols = rewrite symbols value
            (CheckedAST.UnaryOp (op, value), symbols)
        | CheckedAST.Let (pattern, value, body) ->
            let value, symbols = rewrite symbols value
            let body, symbols = rewrite symbols body
            (CheckedAST.Let (pattern, value, body), symbols)
        | CheckedAST.RecursiveLet (recursion, value, body) ->
            let value, symbols = rewrite symbols value
            let body, symbols = rewrite symbols body
            (CheckedAST.RecursiveLet (recursion, value, body), symbols)
        | CheckedAST.If (condition, thenBranch, elseBranch) ->
            let condition, symbols = rewrite symbols condition
            let thenBranch, symbols = rewrite symbols thenBranch
            let elseBranch, symbols = rewrite symbols elseBranch
            (CheckedAST.If (condition, thenBranch, elseBranch), symbols)
        | CheckedAST.Sequence (first, next) ->
            let first, symbols = rewrite symbols first
            let next, symbols = rewrite symbols next
            (CheckedAST.Sequence (first, next), symbols)
        | CheckedAST.Call (name, args) ->
            let args, symbols = rewriteArgs symbols args
            (CheckedAST.Call (name, args), symbols)
        | CheckedAST.TypeApp (name, types, args) ->
            let args, symbols = rewriteArgs symbols args
            let functionComparisonType =
                match name, types with
                | marker, [AST.TFunction _ as targetType] when marker = eqHelperDispatchMarker -> Some targetType
                | "__key_eq", [AST.TFunction _ as targetType] -> Some targetType
                | _ -> None
            match functionComparisonType with
            | None -> (CheckedAST.TypeApp (name, types, args), symbols)
            | Some _ ->
                let leftId, symbols = CheckedAST.allocateBinding "__comparison_left" symbols
                let rightId, symbols = CheckedAST.allocateBinding "__comparison_right" symbols
                (materializeFunctionComparisonPlan leftId rightId (exprArgsToList args), symbols)
        | CheckedAST.TupleLiteral values ->
            let values, symbols = rewriteList symbols values
            (CheckedAST.TupleLiteral values, symbols)
        | CheckedAST.TupleAccess (tuple, index) ->
            let tuple, symbols = rewrite symbols tuple
            (CheckedAST.TupleAccess (tuple, index), symbols)
        | CheckedAST.DictLiteral (keyType, valueType, entries) ->
            let entries, symbols =
                entries
                |> List.mapFold (fun symbols (key, value) ->
                    let key, symbols = rewrite symbols key
                    let value, symbols = rewrite symbols value
                    ((key, value), symbols)) symbols
            (CheckedAST.DictLiteral (keyType, valueType, entries), symbols)
        | CheckedAST.RecordLiteral (reference, fields) ->
            let fields, symbols =
                fields
                |> List.mapFold (fun symbols (name, value) ->
                    let value, symbols = rewrite symbols value
                    ((name, value), symbols)) symbols
            (CheckedAST.RecordLiteral (reference, fields), symbols)
        | CheckedAST.RecordUpdate (record, fields) ->
            let record, symbols = rewrite symbols record
            let fields, symbols =
                fields
                |> List.mapFold (fun symbols (name, value) ->
                    let value, symbols = rewrite symbols value
                    ((name, value), symbols)) symbols
            (CheckedAST.RecordUpdate (record, fields), symbols)
        | CheckedAST.RecordAccess (record, field) ->
            let record, symbols = rewrite symbols record
            (CheckedAST.RecordAccess (record, field), symbols)
        | CheckedAST.Constructor (reference, name, fields) ->
            let fields, symbols = fields |> List.mapFold rewrite symbols
            (CheckedAST.Constructor (reference, name, fields), symbols)
        | CheckedAST.Match (scrutinee, cases) ->
            let scrutinee, symbols = rewrite symbols scrutinee
            let cases, symbols =
                cases
                |> List.mapFold (fun symbols case ->
                    let guard, symbols =
                        match case.Guard with
                        | None -> (None, symbols)
                        | Some guard ->
                            let guard, symbols = rewrite symbols guard
                            (Some guard, symbols)
                    let body, symbols = rewrite symbols case.Body
                    ({ case with Guard = guard; Body = body }, symbols)) symbols
            (CheckedAST.Match (scrutinee, cases), symbols)
        | CheckedAST.ListLiteral values ->
            let values, symbols = rewriteList symbols values
            (CheckedAST.ListLiteral values, symbols)
        | CheckedAST.Lambda (parameters, annotation, body) ->
            let body, symbols = rewrite symbols body
            (CheckedAST.Lambda (parameters, annotation, body), symbols)
        | CheckedAST.Apply (func, args) ->
            let func, symbols = rewrite symbols func
            let args, symbols = rewriteArgs symbols args
            (CheckedAST.Apply (func, args), symbols)
        | CheckedAST.IndirectApply (func, args) ->
            let func, symbols = rewrite symbols func
            let args, symbols = rewriteArgs symbols args
            (CheckedAST.IndirectApply (func, args), symbols)
        | CheckedAST.Closure (name, captures) ->
            let captures, symbols = rewriteList symbols captures
            (CheckedAST.Closure (name, captures), symbols)
        | CheckedAST.InterpolatedString parts ->
            let parts, symbols =
                parts
                |> List.mapFold (fun symbols part ->
                    match part with
                    | CheckedAST.StringText _ -> (part, symbols)
                    | CheckedAST.StringExpr value ->
                        let value, symbols = rewrite symbols value
                        (CheckedAST.StringExpr value, symbols)) symbols
            (CheckedAST.InterpolatedString parts, symbols)
        | _ -> (expression, symbols)
    let symbols = CheckedAST.programSymbols program
    let topLevels, symbols =
        CheckedAST.programTopLevels program
        |> List.mapFold (fun symbols topLevel ->
            match topLevel with
            | CheckedAST.FunctionDef functionDef ->
                let body, symbols = rewrite symbols functionDef.Body
                (CheckedAST.FunctionDef { functionDef with Body = body }, symbols)
            | CheckedAST.ValueDef valueDef ->
                let body, symbols = rewrite symbols valueDef.Body
                (CheckedAST.ValueDef { valueDef with Body = body }, symbols)
            | CheckedAST.Expression expression ->
                let expression, symbols = rewrite symbols expression
                (CheckedAST.Expression expression, symbols)
            | CheckedAST.TypeDef _ -> (topLevel, symbols)) symbols
    CheckedAST.Program (symbols, topLevels)

/// Replace TypeApp with Call across a program using a registry (drops generic defs)
let replaceTypeAppsInProgramWithRegistry (specRegistry: SpecRegistry) (program: CheckedAST.Program) : Result<CheckedAST.Program, string> =
    let program = materializeFunctionComparisons program
    let symbols = CheckedAST.programSymbols program
    let topLevels = CheckedAST.programTopLevels program
    let rec loop (remaining: CheckedAST.TopLevel list) (acc: CheckedAST.TopLevel list) : Result<CheckedAST.Program, string> =
        match remaining with
        | [] -> Ok (CheckedAST.Program (symbols, List.rev acc))
        | tl :: rest ->
            match tl with
            | CheckedAST.FunctionDef f when not (List.isEmpty f.TypeParams) ->
                loop rest acc
            | CheckedAST.FunctionDef f ->
                replaceTypeAppsInFuncWithRegistry specRegistry f
                |> Result.bind (fun f' -> loop rest (CheckedAST.FunctionDef f' :: acc))
            | CheckedAST.Expression e ->
                replaceTypeAppsWithRegistry specRegistry e
                |> Result.bind (fun e' -> loop rest (CheckedAST.Expression e' :: acc))
            | CheckedAST.ValueDef valueDef ->
                replaceTypeAppsWithRegistry specRegistry valueDef.Body
                |> Result.bind (fun body ->
                    let valueDef' = { valueDef with Body = body }
                    loop rest (CheckedAST.ValueDef valueDef' :: acc))
            | CheckedAST.TypeDef td ->
                loop rest (CheckedAST.TypeDef td :: acc)

    loop topLevels []

let private collectInitialMonomorphizationSpecs (program: CheckedAST.Program) : Set<SpecKey> =
    let topLevels = CheckedAST.programTopLevels program
    topLevels
    |> List.map (function
        | CheckedAST.FunctionDef f when List.isEmpty f.TypeParams -> collectTypeAppsFromFunc f
        | CheckedAST.ValueDef valueDef -> collectTypeApps (CheckedAST.valueDefBody valueDef)
        | CheckedAST.Expression e -> collectTypeApps e
        | _ -> Set.empty)
    |> List.fold Set.union Set.empty

let private registryWithExternalSpecs (specialization: SpecializationResult) : SpecRegistry =
    specialization.ExternalSpecs
    |> Set.fold
        (fun registry (funcName, typeArgs) ->
            Map.add (funcName, typeArgs) (specName funcName typeArgs) registry)
        specialization.SpecRegistry

let internal monomorphizeWithGenericFuncDefs (genericFuncDefs: GenericFuncDefs) (program: CheckedAST.Program) : CheckedAST.Program =
    let initialSpecs = collectInitialMonomorphizationSpecs program
    let specialization = specializeFromSpecs genericFuncDefs initialSpecs
    let symbols = CheckedAST.programSymbols program
    let topLevels = CheckedAST.programTopLevels program
    let symbols, specializedFunctions =
        importSpecializedFunctions symbols specialization.SpecializedFuncs
    let specializedTopLevels = specializedFunctions |> List.map CheckedAST.FunctionDef
    let programWithSpecializations = CheckedAST.Program (symbols, specializedTopLevels @ topLevels)
    match replaceTypeAppsInProgramWithRegistry (registryWithExternalSpecs specialization) programWithSpecializations with
    | Ok monomorphized -> monomorphized
    | Error err -> Crash.crash $"monomorphizeWithGenericFuncDefs: {err}"

/// Check if a program needs lambda lowering (lambda inlining + lifting)
/// based on lambdas, closures, or function values.
let programNeedsLambdaLowering (knownFuncNames: Set<string>) (program: CheckedAST.Program) : bool =
    let rec exprNeedsLambdaLowering (bound: Set<AST.BindingId>) (expr: CheckedAST.Expr) : bool =
        match expr with
        | CheckedAST.Lambda _ | CheckedAST.Apply _ | CheckedAST.IndirectApply _ | CheckedAST.FuncRef _ | CheckedAST.Closure _ ->
            true
        | CheckedAST.NamedValue name -> Set.contains name knownFuncNames
        | CheckedAST.Local _ -> false
        | CheckedAST.BoundaryRender (_, value) ->
            exprNeedsLambdaLowering bound value
        | CheckedAST.Let (pattern, value, body) ->
            exprNeedsLambdaLowering bound value
            || exprNeedsLambdaLowering
                (Set.union bound (CheckedAST.letPatternBindings pattern |> Set.ofList))
                body
        | CheckedAST.RecursiveLet (recursion, value, body) ->
            let recursiveBound = Set.add (CheckedAST.recursiveBindingId recursion) bound
            exprNeedsLambdaLowering recursiveBound value
            || exprNeedsLambdaLowering recursiveBound body
        | CheckedAST.If (cond, thenBranch, elseBranch) ->
            exprNeedsLambdaLowering bound cond
            || exprNeedsLambdaLowering bound thenBranch
            || exprNeedsLambdaLowering bound elseBranch
        | CheckedAST.Sequence (first, next) ->
            exprNeedsLambdaLowering bound first || exprNeedsLambdaLowering bound next
        | CheckedAST.BinOp (_, left, right) ->
            exprNeedsLambdaLowering bound left
            || exprNeedsLambdaLowering bound right
        | CheckedAST.UnaryOp (_, inner) ->
            exprNeedsLambdaLowering bound inner
        | CheckedAST.Call (_, args)
        | CheckedAST.TypeApp (_, _, args) ->
            args |> exprArgsToList |> List.exists (exprNeedsLambdaLowering bound)
        | CheckedAST.TupleLiteral elems
        | CheckedAST.ListLiteral elems ->
            elems |> List.exists (exprNeedsLambdaLowering bound)
        | CheckedAST.TupleAccess (tuple, _) ->
            exprNeedsLambdaLowering bound tuple
        | CheckedAST.RecordLiteral (_, fields) ->
            fields |> List.exists (fun (_, e) -> exprNeedsLambdaLowering bound e)
        | CheckedAST.RecordUpdate (record, updates) ->
            exprNeedsLambdaLowering bound record
            || (updates |> List.exists (fun (_, e) -> exprNeedsLambdaLowering bound e))
        | CheckedAST.RecordAccess (record, _) ->
            exprNeedsLambdaLowering bound record
        | CheckedAST.Constructor (_, _, fields) ->
            fields |> List.exists (exprNeedsLambdaLowering bound)
        | CheckedAST.Match (scrutinee, cases) ->
            exprNeedsLambdaLowering bound scrutinee
            || (cases |> List.exists (fun (mc: CheckedAST.MatchCase) ->
                (mc.Guard |> Option.map (exprNeedsLambdaLowering bound) |> Option.defaultValue false)
                || exprNeedsLambdaLowering bound mc.Body))
        | CheckedAST.InterpolatedString parts ->
            parts |> List.exists (fun part ->
                match part with
                | CheckedAST.StringText _ -> false
                | CheckedAST.StringExpr e -> exprNeedsLambdaLowering bound e)
        | _ ->
            false

    let topLevels = CheckedAST.programTopLevels program
    let rec loop (remaining: CheckedAST.TopLevel list) : bool =
        match remaining with
        | [] -> false
        | tl :: rest ->
            match tl with
            | CheckedAST.FunctionDef f ->
                let paramNames = f.Params |> paramsToList |> List.map fst |> Set.ofList
                if exprNeedsLambdaLowering paramNames f.Body then true else loop rest
            | CheckedAST.Expression e ->
                if exprNeedsLambdaLowering Set.empty e then true else loop rest
            | CheckedAST.ValueDef valueDef ->
                if exprNeedsLambdaLowering Set.empty (CheckedAST.valueDefBody valueDef) then true else loop rest
            | CheckedAST.TypeDef _ ->
                loop rest

    loop topLevels

// =============================================================================
// Lambda Inlining
// =============================================================================
// For first-class function support, we inline lambdas at their call sites.
// This transforms:
//   let f = fun x -> x + 1 in f(5)
// Into:
//   let f = fun x -> x + 1 in (fun x -> x + 1)(5)
// Which is then handled by immediate application desugaring.

/// Environment mapping variable names to their lambda definitions
