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

let collectTypeApps (expr: AST.Expr) : Set<SpecKey> =
    let rec visit (specs: Set<SpecKey>) (current: AST.Expr) : Set<SpecKey> =
        match current with
        | AST.BoundaryRender (_, value)
        | AST.UnaryOp (_, value)
        | AST.TupleAccess (value, _)
        | AST.RecordAccess (value, _)
        | AST.Lambda (_, _, value) ->
            visit specs value
        | AST.UnitLiteral | AST.Int64Literal _ | AST.Int128Literal _ | AST.BigIntLiteral _
        | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
        | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _
        | AST.UInt64Literal _ | AST.UInt128Literal _ | AST.BoolLiteral _
        | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _
        | AST.Var _ | AST.FuncRef _ | AST.Closure _ | AST.RuntimeError _ ->
            specs
        | AST.BinOp (_, left, right)
        | AST.Let (_, left, right)
        | AST.RecursiveLet (_, left, right)
        | AST.Sequence (left, right) ->
            visit (visit specs left) right
        | AST.If (condition, thenBranch, elseBranch) ->
            visit (visit (visit specs condition) thenBranch) elseBranch
        | AST.Call (_, args) ->
            visitMany specs (exprArgsToList args)
        | AST.TypeApp (funcName, typeArgs, args) ->
            let argSpecs = visitMany specs (exprArgsToList args)
            let hasTypeVars = List.exists containsTypeVar typeArgs
            if funcName = eqHelperDispatchMarker || funcName = "__compare" then
                argSpecs
            elif isGenericKeyIntrinsicName funcName then
                match typeArgs with
                | [keyType] when not hasTypeVars && hasPredefinedKeyIntrinsic keyType ->
                    Set.add (funcName, typeArgs) argSpecs
                | _ -> argSpecs
            elif (funcName = "Stdlib.Dict.fromList" || funcName = "Dict.fromList")
                 && exprArgsToList args = [AST.ListLiteral []]
                 && not hasTypeVars then
                // Optimization: avoid building a Dict from an empty list when types are concrete.
                Set.add ("Stdlib.Dict.empty", typeArgs) argSpecs
            else
                Set.add (funcName, typeArgs) argSpecs
        | AST.TupleLiteral elements
        | AST.ListLiteral elements ->
            visitMany specs elements
        | AST.DictLiteral (keyType, valueType, entries) ->
            let entrySpecs =
                entries |> List.fold (fun acc (key, value) -> visit (visit acc key) value) specs
            if List.isEmpty entries then entrySpecs
            else
                Set.add
                    ("Stdlib.Dict.__setOverwriting", [keyType; valueType])
                    entrySpecs
        | AST.RecordLiteral (_, fields) ->
            fields |> List.fold (fun acc (_, value) -> visit acc value) specs
        | AST.RecordUpdate (record, updates) ->
            updates
            |> List.fold (fun acc (_, value) -> visit acc value) (visit specs record)
        | AST.Constructor (_, _, payload) ->
            payload |> Option.map (visit specs) |> Option.defaultValue specs
        | AST.Match (scrutinee, cases) ->
            cases
            |> List.fold (fun acc case ->
                let acc = case.Guard |> Option.map (visit acc) |> Option.defaultValue acc
                visit acc case.Body) (visit specs scrutinee)
        | AST.Apply (func, args)
        | AST.IndirectApply (func, args) ->
            visitMany (visit specs func) (exprArgsToList args)
        | AST.InterpolatedString parts ->
            parts
            |> List.fold (fun acc part ->
                match part with
                | AST.StringText _ -> acc
                | AST.StringExpr value -> visit acc value) specs

    and visitMany specs expressions =
        expressions |> List.fold visit specs

    visit Set.empty expr

/// Collect TypeApps from a function definition
let collectTypeAppsFromFunc (funcDef: AST.FunctionDef) : Set<SpecKey> =
    collectTypeApps funcDef.Body

/// Collect canonical call targets without interpreting their names. The AOT
/// catalog bridge uses this typed traversal to materialize only lookup
/// primitives that are reachable from the current compilation.
let rec collectCalledFunctions (expr: AST.Expr) : Set<string> =
    let combine expressions =
        expressions
        |> List.map collectCalledFunctions
        |> List.fold Set.union Set.empty

    match expr with
    | AST.UnitLiteral | AST.Int64Literal _ | AST.Int128Literal _ | AST.BigIntLiteral _
    | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _
    | AST.UInt64Literal _ | AST.UInt128Literal _ | AST.BoolLiteral _
    | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _
    | AST.Var _ | AST.FuncRef _ | AST.RuntimeError _ -> Set.empty
    | AST.BoundaryRender (_, value)
    | AST.UnaryOp (_, value)
    | AST.TupleAccess (value, _)
    | AST.RecordAccess (value, _) -> collectCalledFunctions value
    | AST.BinOp (_, left, right)
    | AST.Sequence (left, right) -> combine [left; right]
    | AST.Let (_, value, body) -> combine [value; body]
    | AST.RecursiveLet (_, value, body) -> combine [value; body]
    | AST.If (condition, thenBranch, elseBranch) ->
        combine [condition; thenBranch; elseBranch]
    | AST.Call (name, args)
    | AST.TypeApp (name, _, args) ->
        Set.add name (combine (exprArgsToList args))
    | AST.TupleLiteral elements
    | AST.ListLiteral elements -> combine elements
    | AST.DictLiteral (_, _, entries) -> entries |> List.collect (fun (key, value) -> [key; value]) |> combine
    | AST.RecordLiteral (_, fields) -> fields |> List.map snd |> combine
    | AST.RecordUpdate (record, fields) ->
        combine (record :: (fields |> List.map snd))
    | AST.Constructor (_, _, payload) ->
        payload |> Option.map collectCalledFunctions |> Option.defaultValue Set.empty
    | AST.Match (scrutinee, cases) ->
        let caseCalls =
            cases
            |> List.collect (fun case ->
                case.Body :: (case.Guard |> Option.toList))
            |> combine
        Set.union (collectCalledFunctions scrutinee) caseCalls
    | AST.Lambda (_, _, body) -> collectCalledFunctions body
    | AST.Apply (func, args)
    | AST.IndirectApply (func, args) ->
        combine (func :: exprArgsToList args)
    | AST.Closure (name, captures) -> Set.add name (combine captures)
    | AST.InterpolatedString parts ->
        parts
        |> List.choose (function AST.StringExpr value -> Some value | AST.StringText _ -> None)
        |> combine

/// Specialize only the requested generic specs, returning new functions and a registry
let specializeFromSpecs (genericFuncDefs: GenericFuncDefs) (initialSpecs: Set<SpecKey>) : SpecializationResult =
    let rec iterate
        (pendingSpecs: Set<SpecKey>)
        (processedSpecs: Set<SpecKey>)
        (accFuncs: AST.FunctionDef list)
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
                        | Some funcDef ->
                            let specialized = specializeFunction funcDef typeArgs
                            let registry' = Map.add (funcName, typeArgs) specialized.Name registry
                            let bodySpecs = collectTypeAppsFromFunc specialized
                            (specialized :: funcs, Set.union pending bodySpecs, registry', external)
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
let rec replaceTypeApps (expr: AST.Expr) : AST.Expr =
    match expr with
    | AST.UnitLiteral | AST.Int64Literal _ | AST.Int128Literal _ | AST.BigIntLiteral _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _ | AST.UInt128Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _ | AST.Var _ | AST.FuncRef _ | AST.Closure _ | AST.RuntimeError _ ->
        expr
    | AST.BoundaryRender (renderer, value) ->
        AST.BoundaryRender (renderer, replaceTypeApps value)
    | AST.BinOp (op, left, right) ->
        AST.BinOp (op, replaceTypeApps left, replaceTypeApps right)
    | AST.UnaryOp (op, inner) ->
        AST.UnaryOp (op, replaceTypeApps inner)
    | AST.Let (pattern, value, body) ->
        AST.Let (pattern, replaceTypeApps value, replaceTypeApps body)
    | AST.RecursiveLet (recursion, value, body) ->
        AST.RecursiveLet (recursion, replaceTypeApps value, replaceTypeApps body)
    | AST.If (cond, thenBranch, elseBranch) ->
        AST.If (replaceTypeApps cond, replaceTypeApps thenBranch, replaceTypeApps elseBranch)
    | AST.Sequence (first, next) ->
        AST.Sequence (replaceTypeApps first, replaceTypeApps next)
    | AST.Call (funcName, args) ->
        AST.Call (funcName, AST.NonEmptyList.map replaceTypeApps args)
    | AST.TypeApp (funcName, typeArgs, args) ->
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
                | [leftExpr; rightExpr] -> AST.BinOp (AST.Eq, leftExpr, rightExpr)
                | _ -> Crash.crash "Comparison plan expected exactly two operands"
        elif funcName = "__compare" then
            let replacedArgs = args |> exprArgsToList |> List.map replaceTypeApps
            match typeArgs, replacedArgs with
            | [targetType], [leftExpr; rightExpr] when not hasTypeVars ->
                AST.Call (
                    ComparisonPlanning.compareHelperName targetType,
                    exprArgsFromList [leftExpr; rightExpr]
                )
            | _, evaluatedArgs ->
                wrapWithIgnoredArgEvaluations
                    evaluatedArgs
                    (AST.RuntimeError "Canonical comparison remained polymorphic after monomorphization")
        elif (funcName = "Stdlib.Dict.fromList" || funcName = "Dict.fromList")
           && exprArgsToList args = [AST.ListLiteral []]
           && not hasTypeVars then
            // Optimization: avoid building a Dict from an empty list when types are concrete.
            let specializedName = specName "Stdlib.Dict.empty" typeArgs
            AST.Call (specializedName, exprArgsFromList [])
        elif isGenericKeyIntrinsicName funcName && hasTypeVars then
            let replacedArgs = args |> exprArgsToList |> List.map replaceTypeApps
            wrapWithIgnoredArgEvaluations replacedArgs (unresolvedKeyIntrinsicTypeArgErrorExpr funcName)
        elif isGenericKeyIntrinsicName funcName then
            let replacedArgs = args |> exprArgsToList |> List.map replaceTypeApps
            match funcName, typeArgs with
            | "__hash", [keyType] when hasPredefinedKeyIntrinsic keyType ->
                AST.Call (specName funcName typeArgs, exprArgsFromList replacedArgs)
            | "__hash", [_] ->
                wrapWithIgnoredArgEvaluations replacedArgs (AST.Int64Literal 0L)
            | "__key_eq", [keyType] when hasPredefinedKeyIntrinsic keyType ->
                AST.Call (specName funcName typeArgs, exprArgsFromList replacedArgs)
            | "__key_eq", [keyType] ->
                materializeComparisonPlan keyType replacedArgs |> replaceTypeApps
            | _ -> Crash.crash $"Invalid generic key intrinsic application: {funcName}"
        else
            let specializedName = specName funcName typeArgs
            AST.Call (specializedName, AST.NonEmptyList.map replaceTypeApps args)
    | AST.TupleLiteral elements ->
        AST.TupleLiteral (List.map replaceTypeApps elements)
    | AST.TupleAccess (tuple, index) ->
        AST.TupleAccess (replaceTypeApps tuple, index)
    | AST.DictLiteral (keyType, valueType, entries) ->
        match entries with
        | [] -> expr
        | _ ->
            let empty = AST.DictLiteral (keyType, valueType, [])
            entries
            |> List.fold (fun dictExpr (key, value) ->
                AST.TypeApp (
                    "Stdlib.Dict.__setOverwriting",
                    [keyType; valueType],
                    AST.NonEmptyList.fromList [dictExpr; key; value]
                )) empty
            |> replaceTypeApps
    | AST.RecordLiteral (typeName, fields) ->
        AST.RecordLiteral (typeName, List.map (fun (n, e) -> (n, replaceTypeApps e)) fields)
    | AST.RecordUpdate (record, updates) ->
        AST.RecordUpdate (replaceTypeApps record, List.map (fun (n, e) -> (n, replaceTypeApps e)) updates)
    | AST.RecordAccess (record, fieldName) ->
        AST.RecordAccess (replaceTypeApps record, fieldName)
    | AST.Constructor (typeName, variantName, payload) ->
        AST.Constructor (typeName, variantName, Option.map replaceTypeApps payload)
    | AST.Match (scrutinee, cases) ->
        AST.Match (replaceTypeApps scrutinee,
                   cases |> List.map (fun mc -> { mc with Guard = mc.Guard |> Option.map replaceTypeApps; Body = replaceTypeApps mc.Body }))
    | AST.ListLiteral elements ->
        AST.ListLiteral (List.map replaceTypeApps elements)
    | AST.Lambda (parameters, returnAnnotation, body) ->
        AST.Lambda (parameters, returnAnnotation, replaceTypeApps body)
    | AST.Apply (func, args) ->
        AST.Apply (replaceTypeApps func, AST.NonEmptyList.map replaceTypeApps args)
    | AST.IndirectApply (func, args) ->
        AST.IndirectApply (replaceTypeApps func, AST.NonEmptyList.map replaceTypeApps args)
    | AST.InterpolatedString parts ->
        let replacePart part =
            match part with
            | AST.StringText s -> AST.StringText s
            | AST.StringExpr e -> AST.StringExpr (replaceTypeApps e)
        AST.InterpolatedString (List.map replacePart parts)

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
let replaceTypeAppsWithRegistry (specRegistry: SpecRegistry) (expr: AST.Expr) : Result<AST.Expr, string> =
    let rec mapResult (f: 'a -> Result<'b, string>) (items: 'a list) : Result<'b list, string> =
        match items with
        | [] -> Ok []
        | x :: xs ->
            f x
            |> Result.bind (fun x' ->
                mapResult f xs
                |> Result.map (fun xs' -> x' :: xs'))

    let rec replace (expr': AST.Expr) : Result<AST.Expr, string> =
        match expr' with
        | AST.UnitLiteral
        | AST.Int64Literal _ | AST.Int128Literal _ | AST.BigIntLiteral _
        | AST.Int8Literal _
        | AST.Int16Literal _
        | AST.Int32Literal _
        | AST.UInt8Literal _
        | AST.UInt16Literal _
        | AST.UInt32Literal _
        | AST.UInt64Literal _ | AST.UInt128Literal _
        | AST.BoolLiteral _
        | AST.StringLiteral _
        | AST.CharLiteral _
        | AST.FloatLiteral _
        | AST.Var _
        | AST.FuncRef _
        | AST.Closure _
        | AST.RuntimeError _ -> Ok expr'
        | AST.BoundaryRender (renderer, value) ->
            replace value |> Result.map (fun value' -> AST.BoundaryRender (renderer, value'))
        | AST.BinOp (op, left, right) ->
            replace left
            |> Result.bind (fun left' ->
                replace right
                |> Result.map (fun right' -> AST.BinOp (op, left', right')))
        | AST.UnaryOp (op, inner) ->
            replace inner |> Result.map (fun inner' -> AST.UnaryOp (op, inner'))
        | AST.Let (pattern, value, body) ->
            replace value
            |> Result.bind (fun value' ->
                replace body |> Result.map (fun body' -> AST.Let (pattern, value', body')))
        | AST.RecursiveLet (recursion, value, body) ->
            replace value
            |> Result.bind (fun value' ->
                replace body |> Result.map (fun body' -> AST.RecursiveLet (recursion, value', body')))
        | AST.If (cond, thenBranch, elseBranch) ->
            replace cond
            |> Result.bind (fun cond' ->
                replace thenBranch
                |> Result.bind (fun thenBranch' ->
                    replace elseBranch
                    |> Result.map (fun elseBranch' -> AST.If (cond', thenBranch', elseBranch'))))
        | AST.Sequence (first, next) ->
            replace first
            |> Result.bind (fun first' ->
                replace next
                |> Result.map (fun next' -> AST.Sequence (first', next')))
        | AST.Call (funcName, args) ->
            mapResult replace (exprArgsToList args)
            |> Result.map (fun args' -> AST.Call (funcName, exprArgsFromList args'))
        | AST.TypeApp (funcName, typeArgs, args) ->
            let hasTypeVars = List.exists containsTypeVar typeArgs
            let emptyDictSpec = (funcName = "Stdlib.Dict.fromList" || funcName = "Dict.fromList")
                                && exprArgsToList args = [AST.ListLiteral []]
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
                    let key = ("Stdlib.Dict.empty", typeArgs)
                    match Map.tryFind key specRegistry with
                    | Some name -> Ok name
                    | None -> Error (missingSpecMessage "Stdlib.Dict.empty" typeArgs)
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
                        Ok (AST.Call (specName funcName typeArgs, exprArgsFromList args'))
                    | "__hash", [_] ->
                        Ok (wrapWithIgnoredArgEvaluations args' (AST.Int64Literal 0L))
                    | "__key_eq", [keyType] when hasPredefinedKeyIntrinsic keyType ->
                        Ok (AST.Call (specName funcName typeArgs, exprArgsFromList args'))
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
                        | [leftExpr; rightExpr] -> AST.BinOp (AST.Eq, leftExpr, rightExpr)
                        | _ -> Crash.crash "Comparison plan expected exactly two operands"
                    else
                        wrapWithIgnoredArgEvaluations
                            args'
                            (AST.RuntimeError "Canonical comparison remained polymorphic after monomorphization"))
            else
                if funcName = eqHelperDispatchMarker && not hasTypeVars then
                    mapResult replace (exprArgsToList args)
                    |> Result.bind (fun args' ->
                        replace (materializeComparisonPlan (List.head typeArgs) args'))
                else
                    resolvedNameResult
                    |> Result.bind (fun resolvedName ->
                        if emptyDictSpec then
                            Ok (AST.Call (resolvedName, exprArgsFromList []))
                        else
                            mapResult replace (exprArgsToList args)
                            |> Result.map (fun args' -> AST.Call (resolvedName, exprArgsFromList args')))
        | AST.TupleLiteral elements ->
            mapResult replace elements
            |> Result.map AST.TupleLiteral
        | AST.TupleAccess (tuple, index) ->
            replace tuple |> Result.map (fun tuple' -> AST.TupleAccess (tuple', index))
        | AST.DictLiteral (keyType, valueType, entries) ->
            match entries with
            | [] -> Ok expr'
            | _ ->
                let lowered =
                    entries
                    |> List.fold (fun dictExpr (key, value) ->
                        AST.TypeApp (
                            "Stdlib.Dict.__setOverwriting",
                            [keyType; valueType],
                            AST.NonEmptyList.fromList [dictExpr; key; value]
                        )) (AST.DictLiteral (keyType, valueType, []))
                replace lowered
        | AST.RecordLiteral (typeName, fields) ->
            fields
            |> mapResult (fun (name, value) ->
                replace value |> Result.map (fun value' -> (name, value')))
            |> Result.map (fun fields' -> AST.RecordLiteral (typeName, fields'))
        | AST.RecordUpdate (record, updates) ->
            replace record
            |> Result.bind (fun record' ->
                updates
                |> mapResult (fun (name, value) ->
                    replace value |> Result.map (fun value' -> (name, value')))
                |> Result.map (fun updates' -> AST.RecordUpdate (record', updates')))
        | AST.RecordAccess (record, fieldName) ->
            replace record |> Result.map (fun record' -> AST.RecordAccess (record', fieldName))
        | AST.Constructor (typeName, variantName, payload) ->
            match payload with
            | None -> Ok (AST.Constructor (typeName, variantName, None))
            | Some payloadExpr ->
                replace payloadExpr
                |> Result.map (fun payload' -> AST.Constructor (typeName, variantName, Some payload'))
        | AST.Match (scrutinee, cases) ->
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
                |> Result.map (fun cases' -> AST.Match (scrutinee', cases')))
        | AST.ListLiteral elements ->
            mapResult replace elements |> Result.map AST.ListLiteral
        | AST.Lambda (parameters, returnAnnotation, body) ->
            replace body |> Result.map (fun body' -> AST.Lambda (parameters, returnAnnotation, body'))
        | AST.Apply (func, args) ->
            replace func
            |> Result.bind (fun func' ->
                mapResult replace (exprArgsToList args)
                |> Result.map (fun args' -> AST.Apply (func', exprArgsFromList args')))
        | AST.IndirectApply (func, args) ->
            replace func
            |> Result.bind (fun func' ->
                mapResult replace (exprArgsToList args)
                |> Result.map (fun args' -> AST.IndirectApply (func', exprArgsFromList args')))
        | AST.InterpolatedString parts ->
            parts
            |> mapResult (function
                | AST.StringText s -> Ok (AST.StringText s)
                | AST.StringExpr e -> replace e |> Result.map AST.StringExpr)
            |> Result.map AST.InterpolatedString

    replace expr

/// Replace TypeApp with Call in a function definition
let replaceTypeAppsInFunc (funcDef: AST.FunctionDef) : AST.FunctionDef =
    { funcDef with Body = replaceTypeApps funcDef.Body }

/// Replace TypeApp with Call in a function definition using a registry
let replaceTypeAppsInFuncWithRegistry (specRegistry: SpecRegistry) (funcDef: AST.FunctionDef) : Result<AST.FunctionDef, string> =
    replaceTypeAppsWithRegistry specRegistry funcDef.Body
    |> Result.map (fun body' -> { funcDef with Body = body' })

/// Replace TypeApp with Call across a program using a registry (drops generic defs)
let replaceTypeAppsInProgramWithRegistry (specRegistry: SpecRegistry) (program: AST.Program) : Result<AST.Program, string> =
    let (AST.Program topLevels) = program
    let rec loop (remaining: AST.TopLevel list) (acc: AST.TopLevel list) : Result<AST.Program, string> =
        match remaining with
        | [] -> Ok (AST.Program (List.rev acc))
        | tl :: rest ->
            match tl with
            | AST.FunctionDef f when not (List.isEmpty f.TypeParams) ->
                loop rest acc
            | AST.FunctionDef f ->
                replaceTypeAppsInFuncWithRegistry specRegistry f
                |> Result.bind (fun f' -> loop rest (AST.FunctionDef f' :: acc))
            | AST.Expression e ->
                replaceTypeAppsWithRegistry specRegistry e
                |> Result.bind (fun e' -> loop rest (AST.Expression e' :: acc))
            | AST.ValueDef valueDef ->
                replaceTypeAppsWithRegistry specRegistry (AST.valueDefBody valueDef)
                |> Result.bind (fun body ->
                    let valueDef' =
                        match valueDef with
                        | AST.UncheckedValueDef (name, _) -> AST.UncheckedValueDef (name, body)
                        | AST.CheckedValueDef (name, typ, _) -> AST.CheckedValueDef (name, typ, body)
                    loop rest (AST.ValueDef valueDef' :: acc))
            | AST.TypeDef td ->
                loop rest (AST.TypeDef td :: acc)

    loop topLevels []

let private collectInitialMonomorphizationSpecs (program: AST.Program) : Set<SpecKey> =
    let (AST.Program topLevels) = program
    topLevels
    |> List.map (function
        | AST.FunctionDef f when List.isEmpty f.TypeParams -> collectTypeAppsFromFunc f
        | AST.ValueDef valueDef -> collectTypeApps (AST.valueDefBody valueDef)
        | AST.Expression e -> collectTypeApps e
        | _ -> Set.empty)
    |> List.fold Set.union Set.empty

let private registryWithExternalSpecs (specialization: SpecializationResult) : SpecRegistry =
    specialization.ExternalSpecs
    |> Set.fold
        (fun registry (funcName, typeArgs) ->
            Map.add (funcName, typeArgs) (specName funcName typeArgs) registry)
        specialization.SpecRegistry

let internal monomorphizeWithGenericFuncDefs (genericFuncDefs: GenericFuncDefs) (program: AST.Program) : AST.Program =
    let initialSpecs = collectInitialMonomorphizationSpecs program
    let specialization = specializeFromSpecs genericFuncDefs initialSpecs
    let (AST.Program topLevels) = program
    let specializedTopLevels = specialization.SpecializedFuncs |> List.map AST.FunctionDef
    let programWithSpecializations = AST.Program (specializedTopLevels @ topLevels)
    match replaceTypeAppsInProgramWithRegistry (registryWithExternalSpecs specialization) programWithSpecializations with
    | Ok monomorphized -> monomorphized
    | Error err -> Crash.crash $"monomorphizeWithGenericFuncDefs: {err}"

/// Check if a program needs lambda lowering (lambda inlining + lifting)
/// based on lambdas, closures, or function values.
let programNeedsLambdaLowering (knownFuncNames: Set<string>) (program: AST.Program) : bool =
    let rec exprNeedsLambdaLowering (bound: Set<string>) (expr: AST.Expr) : bool =
        match expr with
        | AST.Lambda _ | AST.Apply _ | AST.IndirectApply _ | AST.FuncRef _ | AST.Closure _ ->
            true
        | AST.Var name ->
            Set.contains name knownFuncNames && not (Set.contains name bound)
        | AST.BoundaryRender (_, value) ->
            exprNeedsLambdaLowering bound value
        | AST.Let (pattern, value, body) ->
            exprNeedsLambdaLowering bound value
            || exprNeedsLambdaLowering
                (Set.union bound (AST.letPatternBindings pattern |> Set.ofList))
                body
        | AST.RecursiveLet (recursion, value, body) ->
            let recursiveBound = Set.add (AST.recursiveBindingName recursion) bound
            exprNeedsLambdaLowering recursiveBound value
            || exprNeedsLambdaLowering recursiveBound body
        | AST.If (cond, thenBranch, elseBranch) ->
            exprNeedsLambdaLowering bound cond
            || exprNeedsLambdaLowering bound thenBranch
            || exprNeedsLambdaLowering bound elseBranch
        | AST.Sequence (first, next) ->
            exprNeedsLambdaLowering bound first || exprNeedsLambdaLowering bound next
        | AST.BinOp (_, left, right) ->
            exprNeedsLambdaLowering bound left
            || exprNeedsLambdaLowering bound right
        | AST.UnaryOp (_, inner) ->
            exprNeedsLambdaLowering bound inner
        | AST.Call (_, args)
        | AST.TypeApp (_, _, args) ->
            args |> exprArgsToList |> List.exists (exprNeedsLambdaLowering bound)
        | AST.TupleLiteral elems
        | AST.ListLiteral elems ->
            elems |> List.exists (exprNeedsLambdaLowering bound)
        | AST.TupleAccess (tuple, _) ->
            exprNeedsLambdaLowering bound tuple
        | AST.RecordLiteral (_, fields) ->
            fields |> List.exists (fun (_, e) -> exprNeedsLambdaLowering bound e)
        | AST.RecordUpdate (record, updates) ->
            exprNeedsLambdaLowering bound record
            || (updates |> List.exists (fun (_, e) -> exprNeedsLambdaLowering bound e))
        | AST.RecordAccess (record, _) ->
            exprNeedsLambdaLowering bound record
        | AST.Constructor (_, _, payload) ->
            payload |> Option.exists (exprNeedsLambdaLowering bound)
        | AST.Match (scrutinee, cases) ->
            exprNeedsLambdaLowering bound scrutinee
            || (cases |> List.exists (fun (mc: AST.MatchCase) ->
                (mc.Guard |> Option.map (exprNeedsLambdaLowering bound) |> Option.defaultValue false)
                || exprNeedsLambdaLowering bound mc.Body))
        | AST.InterpolatedString parts ->
            parts |> List.exists (fun part ->
                match part with
                | AST.StringText _ -> false
                | AST.StringExpr e -> exprNeedsLambdaLowering bound e)
        | _ ->
            false

    let (AST.Program topLevels) = program
    let rec loop (remaining: AST.TopLevel list) : bool =
        match remaining with
        | [] -> false
        | tl :: rest ->
            match tl with
            | AST.FunctionDef f ->
                let paramNames = f.Params |> paramsToList |> List.map fst |> Set.ofList
                if exprNeedsLambdaLowering paramNames f.Body then true else loop rest
            | AST.Expression e ->
                if exprNeedsLambdaLowering Set.empty e then true else loop rest
            | AST.ValueDef valueDef ->
                if exprNeedsLambdaLowering Set.empty (AST.valueDefBody valueDef) then true else loop rest
            | AST.TypeDef _ ->
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
