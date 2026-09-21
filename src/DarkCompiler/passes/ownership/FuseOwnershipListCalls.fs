// FuseOwnershipListCalls.fs - Inline selected List<Int64> boundaries before region extraction.

module FuseOwnershipListCalls

open OwnedIR

type Result = {
    Functions: CheckedAST.FunctionDef list
    FusedSites: Set<CallSiteIdentity>
}

let rec private mapExpr rewrite state expr =
    let mapList values state =
        values |> List.mapFold (fun current value -> mapExpr rewrite current value) state
    let mapNonEmpty values state =
        let mapped, next = mapList (AST.NonEmptyList.toList values) state
        AST.NonEmptyList.fromList mapped, next
    let mapPair first second state =
        let first, afterFirst = mapExpr rewrite state first
        let second, next = mapExpr rewrite afterFirst second
        first, second, next
    let mapped, next =
        match expr with
        | CheckedAST.UnitLiteral | CheckedAST.Int64Literal _ | CheckedAST.Int128Literal _
        | CheckedAST.BigIntLiteral _ | CheckedAST.Int8Literal _ | CheckedAST.Int16Literal _
        | CheckedAST.Int32Literal _ | CheckedAST.UInt8Literal _ | CheckedAST.UInt16Literal _
        | CheckedAST.UInt32Literal _ | CheckedAST.UInt64Literal _ | CheckedAST.UInt128Literal _
        | CheckedAST.BoolLiteral _ | CheckedAST.StringLiteral _ | CheckedAST.CharLiteral _
        | CheckedAST.FloatLiteral _ | CheckedAST.BlobLiteral _ | CheckedAST.Local _
        | CheckedAST.FuncRef _ | CheckedAST.RuntimeError _ -> expr, state
        | CheckedAST.InterpolatedString parts ->
            let parts, next =
                parts
                |> List.mapFold (fun current part ->
                    match part with
                    | CheckedAST.StringText _ -> part, current
                    | CheckedAST.StringExpr value ->
                        let value, following = mapExpr rewrite current value
                        CheckedAST.StringExpr value, following) state
            CheckedAST.InterpolatedString parts, next
        | CheckedAST.BinOp (op, left, right) ->
            let left, right, next = mapPair left right state
            CheckedAST.BinOp (op, left, right), next
        | CheckedAST.UnaryOp (op, value) ->
            let value, next = mapExpr rewrite state value
            CheckedAST.UnaryOp (op, value), next
        | CheckedAST.Let (pattern, value, body) ->
            let value, body, next = mapPair value body state
            CheckedAST.Let (pattern, value, body), next
        | CheckedAST.RecursiveLet (recursion, value, body) ->
            let value, body, next = mapPair value body state
            CheckedAST.RecursiveLet (recursion, value, body), next
        | CheckedAST.If (condition, yes, no) ->
            let condition, afterCondition = mapExpr rewrite state condition
            let yes, no, next = mapPair yes no afterCondition
            CheckedAST.If (condition, yes, no), next
        | CheckedAST.Sequence (first, second) ->
            let first, second, next = mapPair first second state
            CheckedAST.Sequence (first, second), next
        | CheckedAST.Call (target, arguments) ->
            let arguments, next = mapNonEmpty arguments state
            CheckedAST.Call (target, arguments), next
        | CheckedAST.TypeApp (target, types, arguments) ->
            let arguments, next = mapNonEmpty arguments state
            CheckedAST.TypeApp (target, types, arguments), next
        | CheckedAST.TupleLiteral values ->
            let values, next = mapList values state
            CheckedAST.TupleLiteral values, next
        | CheckedAST.TupleAccess (value, index) ->
            let value, next = mapExpr rewrite state value
            CheckedAST.TupleAccess (value, index), next
        | CheckedAST.DictLiteral (keyType, valueType, entries) ->
            let entries, next =
                entries
                |> List.mapFold (fun current (key, value) ->
                    let key, value, following = mapPair key value current
                    (key, value), following) state
            CheckedAST.DictLiteral (keyType, valueType, entries), next
        | CheckedAST.RecordLiteral (reference, fields) ->
            let fields, next =
                fields
                |> List.mapFold (fun current (field, value) ->
                    let value, following = mapExpr rewrite current value
                    (field, value), following) state
            CheckedAST.RecordLiteral (reference, fields), next
        | CheckedAST.RecordUpdate (record, fields) ->
            let record, afterRecord = mapExpr rewrite state record
            let fields, next =
                fields
                |> List.mapFold (fun current (field, value) ->
                    let value, following = mapExpr rewrite current value
                    (field, value), following) afterRecord
            CheckedAST.RecordUpdate (record, fields), next
        | CheckedAST.RecordAccess (record, field) ->
            let record, next = mapExpr rewrite state record
            CheckedAST.RecordAccess (record, field), next
        | CheckedAST.Constructor (reference, fields) ->
            let fields, next = mapList fields state
            CheckedAST.Constructor (reference, fields), next
        | CheckedAST.Match (value, cases) ->
            let value, afterValue = mapExpr rewrite state value
            let cases, next =
                cases
                |> List.mapFold (fun current case ->
                    let guard, afterGuard =
                        match case.Guard with
                        | None -> None, current
                        | Some guard ->
                            let guard, following = mapExpr rewrite current guard
                            Some guard, following
                    let body, following = mapExpr rewrite afterGuard case.Body
                    { case with Guard = guard; Body = body }, following) afterValue
            CheckedAST.Match (value, cases), next
        | CheckedAST.ListLiteral values ->
            let values, next = mapList values state
            CheckedAST.ListLiteral values, next
        | CheckedAST.Lambda (parameters, annotation, body) ->
            let body, next = mapExpr rewrite state body
            CheckedAST.Lambda (parameters, annotation, body), next
        | CheckedAST.Apply (functionValue, arguments)
        | CheckedAST.IndirectApply (functionValue, arguments) ->
            let functionValue, afterFunction = mapExpr rewrite state functionValue
            let arguments, next = mapNonEmpty arguments afterFunction
            match expr with
            | CheckedAST.Apply _ -> CheckedAST.Apply (functionValue, arguments), next
            | _ -> CheckedAST.IndirectApply (functionValue, arguments), next
        | CheckedAST.Closure (target, captures) ->
            let captures, next = mapList captures state
            CheckedAST.Closure (target, captures), next
        | CheckedAST.BoundaryRender (renderer, value) ->
            let value, next = mapExpr rewrite state value
            CheckedAST.BoundaryRender (renderer, value), next
    rewrite next mapped

let private substitute parameters body =
    mapExpr
        (fun () expression ->
            match expression with
            | CheckedAST.Local binding ->
                Map.tryFind binding parameters |> Option.defaultValue expression, ()
            | _ -> expression, ())
        ()
        body
    |> fst

type private State = {
    Selected: Map<AST.FunctionId, CallSiteIdentity list>
    Fused: Set<CallSiteIdentity>
    FusedValues: CheckedAST.Expr list
}

let rec private bindValue pattern value body =
    match value with
    | CheckedAST.Let (innerPattern, innerValue, continuation) ->
        CheckedAST.Let (innerPattern, innerValue, bindValue pattern continuation body)
    | CheckedAST.Sequence (first, continuation) ->
        CheckedAST.Sequence (first, bindValue pattern continuation body)
    | result -> CheckedAST.Let (pattern, result, body)

let rec private removeFirst value = function
    | [] -> []
    | head :: tail when head = value -> tail
    | head :: tail -> head :: removeFirst value tail

let private isSafeArgument = function
    | CheckedAST.Local _ | CheckedAST.UnitLiteral | CheckedAST.Int64Literal _
    | CheckedAST.Int8Literal _ | CheckedAST.Int16Literal _ | CheckedAST.Int32Literal _
    | CheckedAST.UInt8Literal _ | CheckedAST.UInt16Literal _ | CheckedAST.UInt32Literal _
    | CheckedAST.UInt64Literal _ | CheckedAST.BoolLiteral _ | CheckedAST.FloatLiteral _
    | CheckedAST.FuncRef _ -> true
    | _ -> false

let private isSupportedTransformTarget functionNames target =
    match Map.tryFind target functionNames with
    | Some "Darklang.Stdlib.List.map_i64_i64"
    | Some "Darklang.Stdlib.List.reverse_i64" -> true
    | _ -> false

let private isTransformBody functionNames body =
    let _, targets =
        mapExpr
            (fun targets expression ->
                match expression with
                | CheckedAST.Call (target, _) -> expression, Set.add target targets
                | _ -> expression, targets)
            Set.empty
            body
    not (Set.isEmpty targets)
    && Set.forall (isSupportedTransformTarget functionNames) targets

let private eligible
    functionNames
    (callee: CheckedAST.FunctionDef)
    (ownership: CallSignature) =
    let rec hasConsumedListParameter parameters modes =
        match parameters, modes with
        | (_, AST.TList AST.TInt64) :: _, (ConsumedCallParameter | UniqueCallParameter) :: _ -> true
        | _ :: parameters, _ :: modes -> hasConsumedListParameter parameters modes
        | _, _ -> false
    callee.ReturnType = AST.TList AST.TInt64
    && isTransformBody functionNames callee.Body
    && ownership.Result = UniqueProducedCallResult
    && hasConsumedListParameter
        (callee.Params |> AST.NonEmptyList.toList)
        ownership.Parameters

let private ownershipBoundary boundaryId argument =
    CheckedAST.Call (
        boundaryId,
        AST.NonEmptyList.fromList [argument])

let private substitutions boundaryId parameters modes arguments =
    let rec build result parameters modes arguments =
        match parameters, modes, arguments with
        | (binding, typ) :: parameters, mode :: modes, argument :: arguments ->
            let replacement =
                if typ = AST.TList AST.TInt64 && mode = ConsumedCallParameter then
                    ownershipBoundary boundaryId argument
                else argument
            build (Map.add binding replacement result) parameters modes arguments
        | [], [], [] -> Some result
        | _ -> None
    build Map.empty parameters modes arguments

/// Fuse only compiler-selected unique list calls, and only when substitution
/// cannot duplicate evaluation. Unsupported calls keep their scheduled ANF
/// specialization and the ordinary persistent-list representation.
let fuse
    functionNames
    (plan: MaterializeOwnershipVariants.Plan<'leaf, 'id>)
    (functions: CheckedAST.FunctionDef list)
    : Result =
    let boundaryId =
        functionNames
        |> Map.toSeq
        |> Seq.tryPick (fun (id, name) ->
            if name = "Darklang.Stdlib.List.__arrayOwnershipBoundary_i64" then Some id
            else None)
    let checkedById = functions |> List.map (fun definition -> definition.Id, definition) |> Map.ofList
    let rewrites =
        MaterializeOwnershipVariants.rewrites plan
        |> List.map (fun rewrite -> rewrite.Site, rewrite.Ownership)
        |> Map.ofList
    let selectedByCaller =
        MaterializeOwnershipVariants.rewrites plan
        |> List.groupBy (fun rewrite -> rewrite.Site.Caller)
        |> List.map (fun (caller, callerRewrites) ->
            caller,
            (callerRewrites
             |> List.groupBy (fun rewrite -> rewrite.Original.Target)
             |> List.map (fun (target, targetRewrites) ->
                 target,
                 (targetRewrites |> List.sortBy (fun rewrite -> rewrite.Site) |> List.map (fun rewrite -> rewrite.Site)))
             |> Map.ofList))
        |> Map.ofList
    let fuseFunction (definition: CheckedAST.FunctionDef) =
        let initial = {
            Selected = Map.tryFind definition.Id selectedByCaller |> Option.defaultValue Map.empty
            Fused = Set.empty
            FusedValues = []
        }
        let rewrite state expression =
            match expression with
            | CheckedAST.Let (pattern, value, body)
                when List.contains value state.FusedValues ->
                bindValue pattern value body,
                { state with FusedValues = removeFirst value state.FusedValues }
            | CheckedAST.Call (target, arguments) ->
                match Map.tryFind target state.Selected with
                | Some (site :: rest) ->
                    let selected =
                        if List.isEmpty rest then Map.remove target state.Selected
                        else Map.add target rest state.Selected
                    let next = { state with Selected = selected }
                    match Map.tryFind site rewrites, Map.tryFind target checkedById with
                    | Some ownership, Some callee
                        when eligible functionNames callee ownership
                             && (AST.NonEmptyList.toList arguments |> List.forall isSafeArgument) ->
                        let boundaryId =
                            boundaryId
                            |> Option.defaultWith (fun () ->
                                Crash.crash
                                    "Ownership boundary function is absent from semantic function metadata")
                        match
                            substitutions
                                boundaryId
                                (callee.Params |> AST.NonEmptyList.toList)
                                ownership.Parameters
                                (AST.NonEmptyList.toList arguments)
                        with
                        | Some replacements ->
                            let fused = substitute replacements callee.Body
                            fused,
                            {
                                next with
                                    Fused = Set.add site next.Fused
                                    FusedValues = fused :: next.FusedValues
                            }
                        | None -> expression, next
                    | _ -> expression, next
                | Some [] | None -> expression, state
            | _ -> expression, state
        let body, final = mapExpr rewrite initial definition.Body
        { definition with Body = body }, final.Fused
    let rewritten, fused =
        functions
        |> List.map fuseFunction
        |> List.fold (fun (definitions, allFused) (definition, fused) ->
            definition :: definitions, Set.union allFused fused) ([], Set.empty)
    { Functions = List.rev rewritten; FusedSites = fused }
