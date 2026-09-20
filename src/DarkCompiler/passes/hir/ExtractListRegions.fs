// ExtractListRegions.fs - Recognize closed list computations and prove scalar scope eligibility.

module ExtractListRegions

open HIR

open ListRegion

type private ScalarLifetime = EnclosingLifetime | JoinEntryLifetime

type private ExtractionName =
    | SourceBinding of AST.BindingId
    | RegionResult

type private Extraction = {
    Lists: Map<ExtractionName, HIR.Value>
    Values: Map<ExtractionName, HIR.Value>
    Operations: HIR.Operation<Operation<Transform * ReuseSelection>, FunctionalBlock> list
    RuntimeInputs: Set<ListId>
    NextId: int
    Lifetime: ScalarLifetime
}

let private listCall = function
    | CheckedAST.Call (name, args) -> Some (name, AST.NonEmptyList.toList args)
    | _ -> None

/// Prove scope destruction separately from evaluation effects. Calls use an
/// explicit contract; nominal payloads and unknown closures remain unproven.
let private inertExpression infer callIsInert =
    let inertType = DestructionAnalysis.hasInertDestruction
    let rec check types expr =
        let typedInert () =
            match infer types expr with
            | Ok typ -> inertType typ
            | Error _ -> false
        let recur = check types
        match expr with
        | CheckedAST.FuncRef _ -> true
        | CheckedAST.Closure (_, captures) -> List.forall recur captures
        | CheckedAST.Let (CheckedAST.LPVariable name, value, body) ->
            // Reject unsupported syntax before inference: declaration overlays
            // need not contain the pattern/layout metadata of their base context.
            if not (recur value) then false
            else
                match infer types value with
                | Ok typ when inertType typ -> check (Map.add name typ types) body
                | _ -> false
        | CheckedAST.Let ((CheckedAST.LPUnit | CheckedAST.LPWildcard), value, body)
        | CheckedAST.Sequence (value, body) -> recur value && recur body
        | CheckedAST.If (condition, yes, no) -> recur condition && recur yes && recur no
        | CheckedAST.BinOp (_, left, right) -> recur left && recur right
        | CheckedAST.UnaryOp (_, value) | CheckedAST.TupleAccess (value, _) -> recur value
        | CheckedAST.Call (name, args) ->
            callIsInert name && (AST.NonEmptyList.toList args |> List.forall recur) && typedInert ()
        | CheckedAST.TupleLiteral values | CheckedAST.ListLiteral values -> List.forall recur values
        | CheckedAST.Local _ | CheckedAST.NamedValue _ -> typedInert ()
        | CheckedAST.UnitLiteral | CheckedAST.Int64Literal _ | CheckedAST.Int128Literal _
        | CheckedAST.Int8Literal _ | CheckedAST.Int16Literal _ | CheckedAST.Int32Literal _
        | CheckedAST.UInt8Literal _ | CheckedAST.UInt16Literal _ | CheckedAST.UInt32Literal _
        | CheckedAST.UInt64Literal _ | CheckedAST.UInt128Literal _ | CheckedAST.BigIntLiteral _
        | CheckedAST.BoolLiteral _ | CheckedAST.StringLiteral _ | CheckedAST.CharLiteral _
        | CheckedAST.FloatLiteral _ | CheckedAST.RuntimeError _ -> true
        | _ -> false
    check

/// Retain dependencies with each local proof so registry composition can revoke
/// transitive proofs when a definition changes. Recursive components need no
/// unrolling: the consumer rejects the backwards closure of unproven callees.
let scopeContracts infer (functions: CheckedAST.FunctionDef list) =
    let rec calls expr =
        let many expressions = expressions |> List.map calls |> Set.unionMany
        match expr with
        | CheckedAST.Call (name, args) -> Set.add name (many (AST.NonEmptyList.toList args))
        | CheckedAST.Closure (_, captures) | CheckedAST.TupleLiteral captures | CheckedAST.ListLiteral captures -> many captures
        | CheckedAST.Let (_, value, body) | CheckedAST.Sequence (value, body)
        | CheckedAST.BinOp (_, value, body) -> many [value; body]
        | CheckedAST.If (condition, yes, no) -> many [condition; yes; no]
        | CheckedAST.UnaryOp (_, value) | CheckedAST.TupleAccess (value, _) -> calls value
        | _ -> Set.empty
    functions |> List.map (fun func ->
        let parameters = AST.NonEmptyList.toList func.Params
        let types = Map.ofList parameters
        let localInert =
            DestructionAnalysis.hasInertDestruction func.ReturnType
            && List.forall (snd >> DestructionAnalysis.hasInertDestruction) parameters
            && inertExpression infer (fun _ -> true) types func.Body
        func.Id,
        ({ LocalDestruction = if localInert then DestructionAnalysis.InertScope else DestructionAnalysis.UnprovenScope
           Calls = calls func.Body }: DestructionAnalysis.FunctionScopeContract))
    |> Map.ofList

/// A failed recognition is semantic absence, not a compiler failure. The
/// original checked expression then uses the supported persistent List path.
let tryExtract
    (inertScopes: Set<AST.FunctionId>)
    (parameterTypes: Map<AST.BindingId, AST.Type>)
    (infer: Map<AST.BindingId, AST.Type> -> CheckedAST.Expr -> Result<AST.Type, string>)
    (freeVariables: CheckedAST.Expr -> Set<AST.BindingId>)
    (expression: CheckedAST.Expr)
    : FunctionalRegion option =
    let inertExpression = inertExpression infer (fun name -> Set.contains name inertScopes)
    let types state =
        state.Values
        |> Map.toList
        |> List.choose (fun (name, value) ->
            match name with
            | SourceBinding id -> Some (id, value.Type)
            | RegionResult -> None)
        |> Map.ofList
    let normalizedOperand state expr typ =
        let inputs =
            freeVariables expr
            |> Set.toList
            |> List.choose (fun name ->
                Map.tryFind (SourceBinding name) state.Values
                |> Option.map (fun value -> name, value))
            |> Map.ofList
        { Expression = expr; Type = typ; Inputs = inputs }

    let operand state accepts expr : Scalar option =
        let referencesList =
            freeVariables expr |> Set.exists (fun name -> Map.containsKey (SourceBinding name) state.Lists)
        let destructionIsInert =
            match state.Lifetime with
            | EnclosingLifetime -> true
            | JoinEntryLifetime -> inertExpression (types state) expr
        if referencesList || not destructionIsInert then None
        else
            match infer (types state) expr with
            | Ok typ when accepts typ -> Some (normalizedOperand state expr typ)
            | _ -> None

    let scalar state expr = operand state immediate expr

    let callback state expected expr : Scalar option =
        let scopeIsInert =
            match state.Lifetime, expr with
            | EnclosingLifetime, _ -> true
            | JoinEntryLifetime, CheckedAST.FuncRef name
            | JoinEntryLifetime, CheckedAST.Closure (name, _) -> Set.contains name inertScopes
            | JoinEntryLifetime, _ -> false
        // A closure may not hide a region alias or an effectful destructor.
        let capturesAreImmediate =
            match expr with
            | CheckedAST.Closure (_, captures) ->
                captures |> List.forall (function
                    | CheckedAST.FuncRef _ -> true // Static code addresses, including closure comparators.
                    | capture -> Option.isSome (scalar state capture))
            | CheckedAST.FuncRef _ -> true
            | _ -> false
        if not capturesAreImmediate || not scopeIsInert then None
        else
            match infer (types state) expr with
            | Ok typ when typ = expected -> Some (normalizedOperand state expr typ)
            | _ -> None

    let fresh typ state =
        { Id = HIR.ValueId state.NextId; Type = typ }, { state with NextId = state.NextId + 1 }

    let addList state operation =
        let value, next = fresh (AST.TList AST.TInt64) state
        value, { next with Operations = operation value :: state.Operations }

    let rec list state expr =
        match expr with
        | CheckedAST.Local name ->
            Map.tryFind (SourceBinding name) state.Lists |> Option.map (fun id -> id, state)
        | CheckedAST.ListLiteral elements when List.length elements <= maxCapacity ->
            let values = elements |> List.map (fun value -> scalar state value |> Option.filter (fun typed -> typed.Type = AST.TInt64))
            if values |> List.forall Option.isSome then
                Some (addList state (fun output -> Leaf (Construct (output, Literal (List.choose id values)))))
            else None
        | _ ->
            match listCall expr with
            | Some (id, [input])
                when id = AST.functionIdForName "Darklang.Stdlib.List.__arrayOwnershipBoundary_i64" ->
                list state input
                |> Option.map (fun (source, next) ->
                    source,
                    { next with RuntimeInputs = Set.add source.Id next.RuntimeInputs })
            | Some (id, [count; value]) when id = AST.functionIdForName "Darklang.Stdlib.List.repeatUnsafe_i64" ->
                match operand state ((=) AST.TInt) count, operand state ((=) AST.TInt64) value with
                | Some count, Some value -> Some (addList state (fun output -> Leaf (Construct (output, Repeat (count, value)))))
                | _ -> None
            | Some (id, [input; fn]) when id = AST.functionIdForName "Darklang.Stdlib.List.map_i64_i64" ->
                list state input
                |> Option.bind (fun (source, next) ->
                    callback state (AST.TFunction ([AST.TInt64], AST.TInt64)) fn
                    |> Option.map (fun fn ->
                        let reuse =
                            if Set.contains source.Id next.RuntimeInputs then RuntimeReuse
                            else StaticReuse
                        let next = { next with RuntimeInputs = Set.remove source.Id next.RuntimeInputs }
                        addList next (fun id -> Leaf (Transform (id, source, (Map fn, reuse))))))
            | Some (id, [input]) when id = AST.functionIdForName "Darklang.Stdlib.List.reverse_i64" ->
                list state input
                |> Option.map (fun (source, next) ->
                    let reuse =
                        if Set.contains source.Id next.RuntimeInputs then RuntimeReuse
                        else StaticReuse
                    let next = { next with RuntimeInputs = Set.remove source.Id next.RuntimeInputs }
                    addList next (fun id -> Leaf (Transform (id, source, (Reverse, reuse)))))
            | _ -> None

    let rec bindScalar state name expr =
        match expr with
        | CheckedAST.If (condition, yes, no) ->
            operand state ((=) AST.TBool) condition
            |> Option.bind (fun condition ->
                region name { state with Operations = []; Lifetime = JoinEntryLifetime } yes
                |> Option.bind (fun (FunctionalBlock yes as yesBlock, afterYes) ->
                    region name { state with Operations = []; NextId = afterYes; Lifetime = JoinEntryLifetime } no
                    |> Option.bind (fun (FunctionalBlock no as noBlock, afterNo) ->
                        if yes.Result.Type <> no.Result.Type then None
                        else
                            let result, next = fresh yes.Result.Type { state with NextId = afterNo }
                            Some { next with
                                      Values = Map.add name result state.Values
                                      Lists = Map.remove name state.Lists
                                      Operations = Branch (result, condition, yesBlock, noBlock) :: state.Operations })))
        | _ -> bindSimpleScalar state name expr

    and bindSimpleScalar state name expr =
        match listCall expr with
        | Some (id, [input; initial; fn]) when id = AST.functionIdForName "Darklang.Stdlib.List.fold_i64_i64" ->
            list state input
            |> Option.bind (fun (source, next) ->
                match scalar state initial, callback state (AST.TFunction ([AST.TInt64; AST.TInt64], AST.TInt64)) fn with
                | Some initial, Some fn when initial.Type = AST.TInt64 ->
                    let result, afterResult = fresh AST.TInt64 next
                    Some { afterResult with
                             Values = Map.add name result next.Values
                             Lists = Map.remove name next.Lists
                             Operations = Leaf (Fold (result, source, initial, fn)) :: next.Operations }
                | _ -> None)
        | _ ->
            scalar state expr
            |> Option.map (fun value ->
                let result, next = fresh value.Type state
                { next with Values = Map.add name result state.Values
                            Lists = Map.remove name state.Lists
                            Operations = ScalarBinding (result, value) :: state.Operations })

    and region finalName state expr =
        match expr with
        | CheckedAST.Let (CheckedAST.LPVariable name, value, body) ->
            let sourceName = SourceBinding name
            match list state value with
            | Some (id, next) ->
                region finalName { next with Lists = Map.add sourceName id next.Lists
                                             Values = Map.add sourceName id next.Values } body
            | None -> bindScalar state sourceName value |> Option.bind (fun next -> region finalName next body)
        | _ ->
            bindScalar state finalName expr
            |> Option.bind (fun next ->
                Map.tryFind finalName next.Values
                |> Option.map (fun result ->
                    FunctionalBlock { Parameters = []
                                      Operations = List.rev next.Operations
                                      Result = result }, next.NextId))

    let isListOperation value =
        match listCall value with
        | Some (id, _)
            when id = AST.functionIdForName "Darklang.Stdlib.List.map_i64_i64"
                 || id = AST.functionIdForName "Darklang.Stdlib.List.reverse_i64"
                 || id = AST.functionIdForName "Darklang.Stdlib.List.repeatUnsafe_i64"
                 || id = AST.functionIdForName "Darklang.Stdlib.List.fold_i64_i64" -> true
        | _ -> false
    let candidate =
        match expression with
        | CheckedAST.Let (_, CheckedAST.ListLiteral _, _) -> true
        | CheckedAST.Let (_, value, _) -> isListOperation value
        | _ -> isListOperation expression
    if candidate then
        let finalName = RegionResult
        let parameterNames = freeVariables expression |> Set.intersect (parameterTypes |> Map.keys |> Set.ofSeq)
        let parameters, nextId =
            parameterNames
            |> Set.toList
            |> List.mapFold (fun nextId name ->
                let typ = Map.find name parameterTypes
                (name, { Id = HIR.ValueId nextId; Type = typ }), nextId + 1) 0
            |> fun (values, nextId) -> Map.ofList values, nextId
        let extractionParameters =
            parameters |> Map.toList |> List.map (fun (name, value) -> SourceBinding name, value) |> Map.ofList
        region finalName {
            Lists = Map.empty
            Values = extractionParameters
            Operations = []
            RuntimeInputs = Set.empty
            NextId = nextId
            Lifetime = EnclosingLifetime
        } expression
        |> Option.bind (fun (FunctionalBlock block, _) ->
            let rec containsListOperation (FunctionalBlock block) =
                block.Operations
                |> List.exists (function
                    | Leaf _ -> true
                    | Branch (_, _, ifTrue, ifFalse) -> containsListOperation ifTrue || containsListOperation ifFalse
                    | Call _ -> false
                    | ScalarBinding _ -> false)
            let blockParameters =
                parameters
                |> Map.toList
                |> List.map (fun (binding, value) ->
                    ({ Name = string binding; Binding = binding; Value = value }: HIR.Parameter))
            let root = FunctionalBlock { block with Parameters = blockParameters }
            if not (containsListOperation root) then None
            else Some (FunctionalRegion root))
    else None
